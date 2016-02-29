; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_cal_delvalue
;
; PURPOSE:
;+
;   Add values to the EDI calibration file.
;
; :Categories:
;    MMS, EDI, L2, Ambient
;
; :See Also:
;   mms_edi_amb_cal_addvalue.pro
;   mms_edi_amb_cal_apply.pro
;   mms_edi_amb_cal_find.pro
;   mms_edi_amb_cal_read.pro
;   mms_edi_amb_cal_update.pro
;   mms_edi_amb_cal_write.pro
;
; :History:
;    Modification History::
;       2016/02/25  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Remove values from the EDI calibration files.
;
; :Params:
;       FILE:               in, required, type=string
;                           Calibration file from which values are removed.
;       TT2000:             in, required, type=lon64arr
;                           Time tags of the values to remove from the cal file.
;
; :Keywords:
;       ABSCAL_ONLY:        in, optional, type=string
;                           If set, only relative calibration parameters are deleted.
;       CLOBBER:            in, optional, type=boolean, default=0
;                           If set, then the output cal file will clober any existing
;                               file by the same name.
;       RELCAL_ONLY:        in, optional, type=boolean, default=0
;                           If set, only absolute calibration parameters are deleted.
;       SET_FILL:           in, optional, type=boolean, default=0
;                           If set, values will be replaced by their fill values instead
;                               of being deleted.
;       VERSION:            in, optional, type=string
;                           The version number of the output file, as 'X.Y.Z', where
;                               X, Y, and Z are integers. If not given, 1 will be added
;                               to the Y-version number of `FILE`, and Z will be reset
;                               to zero.
;
; :Returns:
;       CAL_FILE:           Name of the file created. It will be the same as `FILE`, but
;                               with its Y-version number incremented by 1.
;-
function mms_edi_amb_cal_delvalue, file, tt2000, $
ABSCAL_ONLY, $
RELCAL_ONLY, $
CLOBBER=clobber, $
CAL_PATH_ROOT=cal_path_root, $
SET_FILL=set_fill, $
VERSION=version
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(ocal) then obj_destroy, ocal
		MrPrintF, 'LogErr'
		return, ''
	endif
	
	;Check if the file exists
	if ~file_test(file, /READ, /WRITE, /REGULAR) then $
		message, 'FILE must exist and be read and writeable.'

	;Cal path. Can be in two places:
	;   - The environment variable CAL_PATH_ROOT
	;   - The system variable !EDI_INIT (unh_edi_amb_init.pro)
	if n_elements(cal_path_root) eq 0 then begin
		cal_path_root = getenv('CAL_PATH_ROOT')
		if cal_path_root eq '' then begin
			defsysv, '!edi_init', EXIST=tf_exist
			if tf_exist $
				then cal_path_root = !edi_init.cal_path_root $
				else message, 'Cannot determine CAL_PATH_ROOT.'
		endif
	endif
	
	;Check keywords
	tf_set_fill    = keyword_set(set_fill)
	tf_relcal_only = keyword_set(relcal_only)
	tf_abscal_only = keyword_set(abscal_only)
	tf_get_version = n_elements(version) eq 0
	if tf_relcal_only + tf_abscal_only gt 1 then message, 'RELCAL_ONLY and ABSCAL_ONLy are mutually exclusive.'
	
	;Simplify logic
	tf_relcal = tf_relcal_only || (~tf_relcal_only && ~tf_abscal_only)
	tf_abscal = tf_abscal_only || (~tf_relcal_only && ~tf_abscal_only)
	
	;Number of points to throw
	nDelete = n_elements(tt2000)
	
;-----------------------------------------------------
; Output File Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Dissect file name
	mms_dissect_filename, file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level, TSTART=tstart, $
	                            OPTDESC=optdesc, VX=vx, VY=vy, VZ=vz
	
	;Output version
	;   - Increment the Y-version number by 1
	if tf_get_version then begin
		vy_out   = string(fix(vy) + 1, FORMAT='(i0)')
		vz_out   = '0'
	endif else begin
		v  = strsplit(version, '.', /EXTRACT)
		vx = v[0]
		vy_out = v[1]
		vz_out = v[1]
	endelse
	
	;Build the output file name
	cal_file = mms_forge_filename(sc, instr, mode, level, tstart, $
	                              strjoin([vx, vy_out, vz_out], '.'), $
	                              OPTDESC   = optdesc)
	
	;Create the file
	cal_file = filepath(cal_file, ROOT_DIR=cal_path)
	ocal_old = MrCDF_File(file)
	ocal_new = MrCDF_File(cal_file, /CREATE, CLOBBER=clobber)
	
;-----------------------------------------------------
; Copy Global Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the names of all global attributes in the old file
	gAttrNames = ocal_old -> GetAttrNames(COUNT=nGAttrs)
	
	;Copy them to the new file
	for i = 0, nGAttrs-1 do ocal_old -> CopyGlobalAttrTo, gAttrNames[i], ocal_new
	
;-----------------------------------------------------
; Copy Variable Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the names of all global attributes in the old file
	vAttrNames = ocal_old -> GetAttrNames(/VARIABLE_SCOPE, COUNT=nVAttrs)
	
	;Copy them to the new file
	for i = 0, nVAttrs-1 do ocal_old -> CopyVarAttrTo, vAttrNames[i], ocal_new

;-----------------------------------------------------
; Copy Support Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Variable name prefix/suffix
	prefix = strjoin([sc, instr], '_') + '_'
	suffix = '_' + strjoin([mode, level], '_')
	
	;Variable Names
	epoch_vname        = 'Epoch'
	epoch_rel_vname    = 'epoch_rel'
	epoch_abs_vname    = 'epoch_abs'
	phi_vname          = prefix + 'phi'
	theta_vname        = prefix + 'theta'
	rel_optics_vname   = prefix + 'optics_rel'  + suffix
	relcal_gdu1_vname  = prefix + 'relcal_gdu1' + suffix
	relcal_gdu2_vname  = prefix + 'relcal_gdu2' + suffix
	abs_optics_vname   = prefix + 'optics_abs'  + suffix
	abscal_gdu1_vname  = prefix + 'abscal_gdu1' + suffix
	abscal_gdu2_vname  = prefix + 'abscal_gdu2' + suffix
	theta_labels_vname = prefix + 'theta_labels'
	phi_labels_vname   = prefix + 'phi_labels'

	;Copy support data
	ocal_old -> CopyVariableTo, Epoch_vname,        ocal_new
	ocal_old -> CopyVariableTo, phi_vname,          ocal_new
	ocal_old -> CopyVariableTo, theta_vname,        ocal_new
	ocal_old -> CopyVariableTo, theta_labels_vname, ocal_new
	ocal_old -> CopyVariableTo, phi_labels_vname,   ocal_new

;-----------------------------------------------------
; Remove Relative Calibration \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_relcal then begin
	
		;Read the old values
		epoch_rel   = ocal_old -> Read(epoch_rel_vname,   FILLVALUE=epoch_fill)
		rel_optics  = ocal_old -> Read(rel_optics_vname,  FILLVALUE=optics_fill)
		relcal_gdu1 = ocal_old -> Read(relcal_gdu1_vname, FILLVALUE=gdu1_fill)
		relcal_gdu2 = ocal_old -> Read(relcal_gdu2_vname, FILLVALUE=gdu2_fill)

		;Which to delete
		tf_toss = MrIsMember(tt2000, epoch_rel, iToss, COUNT=nTossRel, $
		                     COMPLEMENT=iKeep, NCOMPLEMENT=nKeep)

		;Remove values
		if nTossRel ne nDelete then begin
			MrPrintF, 'LogWarn', nDelete-nTossRel, FORMAT='(%"%i entries not deleted. No entry found.")'
		endif
		
		;Filter out unwanted data
		if nTossRel gt 0 then begin
			if tf_set_fill then begin
				epoch_rel[iToss]   = epoch_fill
				rel_optics[iToss]  = optics_fill
				relcal_gdu1[iToss] = gdu1_fill
				relcal_gdu2[iToss] = gdu2_fill
		
			endif else begin
				epoch_rel   = epoch_rel[ikeep]
				rel_optics  = rel_optics[ikeep]
				relcal_gdu1 = relcal_gdu1[ikeep]
				relcal_gdu2 = relcal_gdu2[ikeep]
			endelse
		endif
		
		;Write data
		ocal_new -> WriteVar, /CREATE, epoch_rel_vname,   epoch_rel,   CDF_TYPE='CDF_TIME_TT2000'
		ocal_new -> WriteVar, /CREATE, rel_optics_vname,  rel_optics,  COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal_new -> WriteVar, /CREATE, relcal_gdu1_vname, relcal_gdu1, COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal_new -> WriteVar, /CREATE, relcal_gdu2_vname, relcal_gdu2, COMPRESSION='GZIP', GZIP_LEVEL=6
		
	
	;COPY RELATIVE CALIBRATIONS
	endif else begin
		ocal_old -> CopyVariableTo, epoch_rel_vname,   ocal_new
		ocal_old -> CopyVariableTo, rel_optics_vname,  ocal_new
		ocal_old -> CopyVariableTo, relcal_gdu1_vname, ocal_new
		ocal_old -> CopyVariableTo, relcal_gdu2_vname, ocal_new
	endelse

;-----------------------------------------------------
; Remove Absolute Calibration \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;ADD RELATIVE CALIBRATIONS
	if MrStruct_HasTag(cal_data, 'TT2000_ABS') then begin

		;Read the old values
		epoch_abs   = ocal_old -> Read(epoch_abs_vname,   FILLVALUE=epoch_fill)
		abs_optics  = ocal_old -> Read(abs_optics_vname,  FILLVALUE=optics_fill)
		abscal_gdu1 = ocal_old -> Read(abscal_gdu1_vname, FILLVALUE=gdu1_fill)
		abscal_gdu2 = ocal_old -> Read(abscal_gdu2_vname, FILLVALUE=gdu2_fill)

		;Which to delete
		tf_toss = MrIsMember(tt2000, epoch_abs, iToss, COUNT=nTossAbs, $
		                     COMPLEMENT=iKeep, NCOMPLEMENT=nKeep)

		;Remove values
		if nTossAbs ne nDelete then begin
			MrPrintF, 'LogWarn', nDelete-nToss_abs, FORMAT='(%"%i entries not deleted. No entry found.")'
		endif
		
		;Filter out unwanted data
		if nTossAbs gt 0 then begin
			if tf_set_fill then begin
				epoch_abs[iToss]   = epoch_fill
				abs_optics[iToss]  = optics_fill
				abscal_gdu1[iToss] = gdu1_fill
				abscal_gdu2[iToss] = gdu2_fill
		
			endif else begin
				epoch_abs   = epoch_abs[ikeep]
				abs_optics  = abs_optics[ikeep]
				abscal_gdu1 = abscal_gdu1[ikeep]
				abscal_gdu2 = abscal_gdu2[ikeep]
			endelse
		endif
		
		;Write data
		ocal_new -> WriteVar, /CREATE, epoch_abs_vname,   epoch_abs,   CDF_TYPE='CDF_TIME_TT2000'
		ocal_new -> WriteVar, /CREATE, abs_optics_vname,  abs_optics,  COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal_new -> WriteVar, /CREATE, abscal_gdu1_vname, abscal_gdu1, COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal_new -> WriteVar, /CREATE, abscal_gdu2_vname, abscal_gdu2, COMPRESSION='GZIP', GZIP_LEVEL=6
	
	;COPY RELATIVE CALIBRATIONS
	endif else begin
		nExtend_abs = 0
		ocal_old -> CopyVariableTo, epoch_abs_vname,   ocal_new
		ocal_old -> CopyVariableTo, abs_optics_vname,  ocal_new
		ocal_old -> CopyVariableTo, abscal_gdu1_vname, ocal_new
		ocal_old -> CopyVariableTo, abscal_gdu2_vname, ocal_new
	endelse

;-----------------------------------------------------
; Update Global Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Add an entry to the "MODS" global attribute
	new_mod = string(FORMAT='(%"v%s.%s.%s - Removed %i relative and %i absolute calibrations")', $
	                 vx, vy_out, vz_out, nTossRel, nTossAbs)
	ocal_new -> WriteGlobalAttr, 'MODS', temporary(new_mod)
	
	;Change the "LOGICAL_FILE_ID
	;   - Use GENTRYNUM to overwrite old value [it is like an array index]
	;   - The default is to append values
	ocal_new -> WriteGlobalAttr, 'Logical_file_id', cgRootName(cal_file), GENTRYNUM=0
	
	;Change the generation date
	caldat, systime(/JULIAN, /UTC), month, day, year
	ocal_new -> WriteGlobalAttr, 'Generation_date', string(year, month, day, FORMAT='(i04, i02, i02)'), GENTRYNUM=0
	
	;Change the data verion
	ocal_new -> WriteGlobalAttr, 'Data_version', strjoin([vx, vy_out, vz_out], '.'), GENTRYNUM=0

;-----------------------------------------------------
; Clean Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Close files
	obj_destroy, ocal_old
	obj_destroy, ocal_new

	return, cal_file
end