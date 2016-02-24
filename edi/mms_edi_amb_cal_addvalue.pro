; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_cal_addvalue
;
; PURPOSE:
;+
;   Add values to the EDI calibration file.
;
; :Categories:
;    MMS, EDI, L2, Ambient
;
; :Params:
;       FILE:               in, required, type=string
;                           Calibration file to which to add values.
;       CAL_DATA:           in, required, type=struct
;                           EDI ambient data structure with the fields below. The
;                             4 relative and/or 4 absolute calibration fields must be
;                             present::
;                               TT2000_REL   -  TT2000 time stamps for relative calibration data
;                               TT2000_ABS   -  TT2000 time stamps for absolute calibration data
;                               REL_OPTICS   -  Optics state at the time relative calibrations were performed
;                               RELCAL_GDU1  -  Relative calibration factors for GDU1
;                               RELCAL_GDU2  -  Relative calibration factors for GDU2
;                               ABS_OPTICS   -  Optics state at the time absolute calibrations were performed
;                               ABSCAL_GDU1  -  Absolute calibration factors for GDU1
;                               ABSCAL_GDU2  -  Absolute calibration factors for GDU2
;
; :Returns:
;       CAL_FILE:           Name of the file created. It will be the same as `FILE`, but
;                               with its Y-version number incremented by 1.
;
; :See Also:
;   mms_edi_amb_cal_find.pro
;   mms_edi_amb_cal_read.pro
;   mms_edi_amb_cal_write.pro
;   mms_edi_amb_cal_apply.pro
;
; :History:
;    Modification History::
;       2016/01/08  -   Written by Matthew Argall
;       2016/02/23  -   Added optics state during relative and absolute calibrations. - MRA
;-
function mms_edi_amb_cal_addvalue, file, cal_data, $
CAL_PATH_ROOT=cal_path_root
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
	;   - The system variable !EDI_AMB_INIT (unh_edi_amb_init.pro)
	if n_elements(cal_path_root) eq 0 then begin
		cal_path_root = getenv('CAL_PATH_ROOT')
		if cal_path_root eq '' then begin
			defsysv, '!edi_amb_init', EXIST=tf_exist
			if tf_exist $
				then cal_path_root = !edi_amb_init.cal_path_root $
				else message, 'Cannot determine CAL_PATH_ROOT.'
		endif
	endif
	
;-----------------------------------------------------
; Output File Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Dissect file name
	mms_dissect_filename, file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level, TSTART=tstart, $
	                            OPTDESC=optdesc, VX=vx, VY=vy, VZ=vz
	
	;Create the output file name
	vy_out   = string(fix(vy) + 1, FORMAT='(i0)')
	vz_out   = '0'
	cal_file = mms_forge_filename(sc, instr, mode, level, tstart, $
	                              strjoin([vx, vy_out, vz_out], '.'), $
	                              OPTDESC   = optdesc)
	
	;Check if files with this z-verison already exist
	cal_path  = filepath('', ROOT_DIR=cal_path_root, SUBDIRECTORY=[sc, instr])
	zversion = mms_latest_zversion(cal_path, cal_file)
	if zversion ne 0 then message, 'Cal file already exists with version ' + vx + '.' + vy_out + '.' + vz_out
	
	;Create the file
	cal_file = filepath(cal_file, ROOT_DIR=cal_path)
	ocal_old = MrCDF_File(file)
	ocal_new = MrCDF_File(cal_file, /CREATE)
	
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
; Add Relative Calibration \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if MrStruct_HasTag(cal_data, 'TT2000_REL') then begin
		;Extract the old data
		epoch_rel   = ocal_old -> Read(epoch_rel_vname)
		rel_optics  = ocal_old -> Read(rel_optics_vname)
		relcal_gdu1 = ocal_old -> Read(relcal_gdu1_vname)
		relcal_gdu2 = ocal_old -> Read(relcal_gdu2_vname)
		
		;Number of points we currently have
		nPts = n_elements(epoch_rel)

		;Append data
		;   - [DIM3, DIM2, DIM1, RECS]
		epoch_rel   = [[epoch_rel], [cal_data.tt2000_rel]]
		rel_optics  = [[rel_optics], [cal_data.rel_optics]]
		relcal_gdu1 = [[[relcal_gdu1]], [[cal_data.relcal_gdu1]]]
		relcal_gdu2 = [[[relcal_gdu2]], [[cal_data.relcal_gdu2]]]
		
		;Sort data
		isort       = sort(epoch_rel)
		iuniq       = uniq(epoch_rel, isort)
		epoch_rel   = epoch_rel[iuniq]
		rel_optics  = rel_optics[iuniq]
		relcal_gdu1 = relcal_gdu1[*,*,iuniq]
		relcal_gdu2 = relcal_gdu2[*,*,iuniq]
	
		;Number of records to extend
		nExtend_rel = n_elements(epoch_rel) - nPts
		
		;Copy variable definitions
		;   - Records need to be allocated before data is written.
		;   - Copy variable definition with extended records first to
		;     have all records adjacent in the file
		;   - Then write the data
		ocal_old -> CopyVarDefTo, epoch_rel_vname,   ocal_new, EXTEND_RECS=nExtend_rel
		ocal_old -> CopyVarDefTo, rel_optics_vname,  ocal_new, EXTEND_RECS=nExtend_rel
		ocal_old -> CopyVarDefTo, relcal_gdu1_vname, ocal_new, EXTEND_RECS=nExtend_rel
		ocal_old -> CopyVarDefTo, relcal_gdu2_vname, ocal_new, EXTEND_RECS=nExtend_rel
		
		;Write variable data
		ocal_new -> WriteVar, epoch_rel_vname,   temporary(epoch_rel)
		ocal_new -> WriteVar, rel_optics_vname,  temporary(rel_optics)
		ocal_new -> WriteVar, relcal_gdu1_vname, temporary(relcal_gdu1)
		ocal_new -> WriteVar, relcal_gdu2_vname, temporary(relcal_gdu2)
	
	;COPY RELATIVE CALIBRATIONS
	endif else begin
		nExtend_rel = 0
		ocal_old -> CopyVariableTo, epoch_rel_vname,   ocal_new
		ocal_old -> CopyVariableTo, rel_optics,        ocal_new
		ocal_old -> CopyVariableTo, relcal_gdu1_vname, ocal_new
		ocal_old -> CopyVariableTo, relcal_gdu2_vname, ocal_new
	endelse

;-----------------------------------------------------
; Add Absolute Calibration \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;ADD RELATIVE CALIBRATIONS
	if MrStruct_HasTag(cal_data, 'TT2000_ABS') then begin
		;Extract the old data
		epoch_abs   = ocal_old -> Read(epoch_abs_vname)
		abs_optics  = ocal_old -> Read(abs_optics_vname)
		abscal_gdu1 = ocal_old -> Read(abscal_gdu1_vname)
		abscal_gdu2 = ocal_old -> Read(abscal_gdu2_vname)
		
		;Number of points we currently have
		nPts = n_elements(epoch_abs)
		
		;Append data
		;   - [DIM3, DIM2, DIM1, RECS]
		epoch_abs   = [[epoch_abs], [cal_data.tt2000_abs]]
		abs_optics  = [[[abs_optics]], [[cal_data.abs_optics]]]
		abscal_gdu1 = [[[abscal_gdu1]], [[cal_data.abscal_gdu1]]]
		abscal_gdu2 = [[[abscal_gdu2]], [[cal_data.abscal_gdu2]]]
		
		;Sort data
		isort       = sort(epoch_abs)
		iuniq       = uniq(epoch_abs, isort)
		epoch_abs   = epoch_abs[iuniq]
		abs_optics  = abs_optics[iuniq]
		abscal_gdu1 = abscal_gdu1[*,*,iuniq]
		abscal_gdu2 = abscal_gdu2[*,*,iuniq]
	
		;Number of records to extend
		nExtend_abs = n_elements(epoch_abs) - nPts
		
		;Copy variable definitions
		;   - Records need to be allocated before data is written.
		;   - Copy variable definition with extended records first to
		;     have all records adjacent in the file
		;   - Then write the data
		ocal_old -> CopyVarDefTo, epoch_abs_vname,   ocal_new, EXTEND_RECS=nExtend_abs
		ocal_old -> CopyVarDefTo, abs_optics_vname,  ocal_new, EXTEND_RECS=nExtend_abs
		ocal_old -> CopyVarDefTo, abscal_gdu1_vname, ocal_new, EXTEND_RECS=nExtend_abs
		ocal_old -> CopyVarDefTo, abscal_gdu2_vname, ocal_new, EXTEND_RECS=nExtend_abs
		
		;Write variable data
		ocal_new -> WriteVar, epoch_abs_vname,   temporary(epoch_abs)
		ocal_new -> WriteVar, abs_optics_vname,  temporary(abs_optics)
		ocal_new -> WriteVar, abscal_gdu1_vname, temporary(abscal_gdu1)
		ocal_new -> WriteVar, abscal_gdu2_vname, temporary(abscal_gdu2)
	
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
	new_mod = string(FORMAT='(%"v%s.%s.%s - Added %i relative and %i absolute calibrations")', $
	                 vx, vy_out, vz_out, nExtend_rel, nExtend_abs)
	ocal_new -> WriteGlobalAttr, 'MODS', temporary(new_mod)
	
	;Change the "LOGICAL_FILE_ID
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