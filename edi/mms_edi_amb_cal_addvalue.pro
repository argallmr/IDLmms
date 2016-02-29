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
; :See Also:
;   mms_edi_amb_cal_apply.pro
;   mms_edi_amb_cal_delvalue.pro
;   mms_edi_amb_cal_find.pro
;   mms_edi_amb_cal_read.pro
;   mms_edi_amb_cal_update.pro
;   mms_edi_amb_cal_write.pro
;
; :History:
;    Modification History::
;       2016/01/08  -   Written by Matthew Argall
;       2016/02/23  -   Added optics state during relative and absolute calibrations. - MRA
;       2016/02/25  -   Replace individual values, added VERSION and CLOBBER keywords. - MRA
;-
;*****************************************************************************************
;+
;   Helper function to retrieve input value.
;
; :Params:
;       CAL_DATA:       in, required, type=structarr
;                       Structure array with one set of calibration parameters
;                           per structure element.
;       TAG:            in, required, type=string
;                       Structure array with one set of calibration parameters
;                           per structure element.
;       FILLVALUE:      in, required, type=depends
;                       Fill value to return if `TAG` does not exist in `CAL_DATA`.
;
; :Returns:
;       VALUE:          The value of `TAG` in `CAL_DATA`.
;-
function amb_cal_getvalue, cal_data, tag, fillval
	compile_opt idl2, hidden
	on_error, 2
	
	;Is the tag present in the structure?
	tags = tag_names(cal_data)
	itag = where(tags eq strupcase(tag), ntags)
	
	;Check if the data structure has the desired tag
	if ntags eq 0 $
		then value = replicate(fillval, n_elements(cal_data)) $
		else value = cal_data.(itag)
	
	return, value
end


;+
;   Add values to the EDI calibration file.
;
; :Params:
;       FILE:               in, required, type=string
;                           Calibration file to which to add values.
;       CAL_DATA:           in, required, type=structarr
;                           EDI ambient data structure with any of the fields below.
;                             Time tags must be given. A new entry will be appended
;                             in the appropriate location. If a duplicate entry is given,
;                             variables present in the structure will overwrite variables
;                             in the cal file. Other cal file variables remain unchanged.
;                               TT2000_REL   -  TT2000 time stamps for relative calibration data
;                               TT2000_ABS   -  TT2000 time stamps for absolute calibration data
;                               REL_OPTICS   -  Optics state at the time relative calibrations were performed
;                               RELCAL_GDU1  -  Relative calibration factors for GDU1
;                               RELCAL_GDU2  -  Relative calibration factors for GDU2
;                               ABS_OPTICS   -  Optics state at the time absolute calibrations were performed
;                               ABSCAL_GDU1  -  Absolute calibration factors for GDU1
;                               ABSCAL_GDU2  -  Absolute calibration factors for GDU2
;
; :Keywords:
;       VERSION:            in, optional, type=string
;                           The version number of the output file, as 'X.Y.Z', where
;                               X, Y, and Z are integers. If not given, 1 will be added
;                               to the Y-version number of `FILE`, and Z will be reset
;                               to zero.
;       CLOBBER:            in, optional, type=boolean, default=0
;                           If set, then the output cal file will clober any existing
;                               file by the same name.
;
; :Returns:
;       CAL_FILE:           Name of the file created. It will be the same as `FILE`, but
;                               with its Y-version number incremented by 1.
;-
function mms_edi_amb_cal_addvalue, file, cal_data, $
CAL_PATH_ROOT=cal_path_root, $
CLOBBER=clobber, $
VERSION=version
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(ocal_old) then obj_destroy, ocal_old
		if obj_valid(ocal_new) then obj_destroy, ocal_new
		MrPrintF, 'LogErr'
		return, ''
	endif
	
	;Check if the file exists
	if ~file_test(file, /READ, /WRITE, /REGULAR) then $
		message, 'FILE must exist and be read and writeable.'

	;Cal path. Can be in two places:
	;   - The environment variable CAL_PATH_ROOT
	;   - The system variable !EDI_INIT (unh_edi_init.pro)
	if n_elements(cal_path_root) eq 0 then begin
		cal_path_root = getenv('CAL_PATH_ROOT')
		if cal_path_root eq '' then begin
			defsysv, '!edi_init', EXIST=tf_exist
			if tf_exist $
				then cal_path_root = !edi_init.cal_path_root $
				else message, 'Cannot determine CAL_PATH_ROOT.'
		endif
	endif
	
	;Auto-determine version number
	tf_get_version = n_elements(version) eq 0
	
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
	cal_file = filepath(cal_file, ROOT_DIR=cal_path_root, SUBDIRECTORY=[sc, 'edi'])
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
	;Get the names of all variable attributes in the old file
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
	optics_rel_vname   = prefix + 'optics_rel'  + suffix
	relcal_gdu1_vname  = prefix + 'rel_gdu1' + suffix
	relcal_gdu2_vname  = prefix + 'rel_gdu2' + suffix
	optics_abs_vname   = prefix + 'optics_abs'  + suffix
	abscal_gdu1_vname  = prefix + 'abs_gdu1' + suffix
	abscal_gdu2_vname  = prefix + 'abs_gdu2' + suffix
	theta_labels_vname = prefix + 'theta_labels'
	phi_labels_vname   = prefix + 'phi_labels'

	;Copy support data
	ocal_old -> CopyVariableTo, Epoch_vname,        ocal_new
	ocal_old -> CopyVariableTo, phi_vname,          ocal_new
	ocal_old -> CopyVariableTo, theta_vname,        ocal_new

;-----------------------------------------------------
; Add Relative Calibration \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if MrStruct_HasTag(cal_data, 'TT2000_REL') then begin
		;Number of new values
		nRel = n_elements(cal_data)
		
		;Read the old values
		epoch_rel   = ocal_old -> Read(epoch_rel_vname,   NRECS=nEpoch,   FILLVALUE=epoch_fill)
		optics_rel  = ocal_old -> Read(optics_rel_vname,  NRECS=nOptics,  FILLVALUE=optics_fill)
		relcal_gdu1 = ocal_old -> Read(relcal_gdu1_vname, NRECS=nRelGDU1, FILLVALUE=gdu1_fill)
		relcal_gdu2 = ocal_old -> Read(relcal_gdu2_vname, NRECS=nRelGDU2, FILLVALUE=gdu2_fill)

		;Are there duplicate points?
		!Null = MrIsMember( epoch_rel, cal_data.tt2000_rel, iDup, COUNT=nDupRel, $
		                    A_INDICES=iReplace, COMPLEMENT=iNew, NCOMPLEMENT=nNewRel )
		
		;Replace duplicates
		;   - By requiring CAL_DATA to be an array of structures, we ensure that
		;     all variables contain the same number of records. Think of the
		;     structure array element as records.
		if nDupRel gt 0 then begin
			;REL_OPTICS
			if MrStruct_HasTag(cal_data[0], 'OPTICS_REL') $
				then optics_rel[iReplace]  = cal_data[iDup].optics_rel
				
			;RELCAL_GDU2
			if MrStruct_HasTag(cal_data[0], 'RELCAL_GDU1') $
				then relcal_gdu1[iReplace] = cal_data[iDup].relcal_gdu1
			
			;RELCAL_GDU2
			if MrStruct_HasTag(cal_data[0], 'RELCAL_GDU2') $
				then relcal_gdu2[iReplace] = cal_data[iDup].relcal_gdu2
		endif
		
		;Add new values
		if nNewRel gt 0 then begin
			;Append and sort time
			epoch_rel = nEpoch eq 0 ? cal_data[iNew].tt2000_rel : [epoch_rel, cal_data[iNew].tt2000_rel]
			isort     = sort(epoch_rel)
		
			;Get values 
			;   - If the tag does not exist, the fill value will be returned
			new_optics_rel  = amb_cal_getvalue(cal_data[iNew], 'OPTICS_REL',  optics_fill)
			new_relcal_gdu1 = amb_cal_getvalue(cal_data[iNew], 'RELCAL_GDU1', gdu1_fill)
			new_relcal_gdu2 = amb_cal_getvalue(cal_data[iNew], 'RELCAL_GDU2', gdu2_fill)
			
			;Append
			optics_rel  = nOptics  eq 0 ? new_optics_rel  : [ [optics_rel],  [new_optics_rel] ]
			relcal_gdu1 = nRelGDU1 eq 0 ? new_relcal_gdu1 : [ [[relcal_gdu1]], [[new_relcal_gdu1]] ]
			relcal_gdu2 = nRelGDU2 eq 0 ? new_relcal_gdu2 : [ [[relcal_gdu2]], [[new_relcal_gdu2]] ]
			
			;Sort
			optics_rel  = optics_rel[isort]
			relcal_gdu1 = relcal_gdu1[*,*,isort]
			relcal_gdu2 = relcal_gdu2[*,*,isort]
		endif

		;Write data
		ocal_new -> WriteVar, /CREATE, epoch_rel_vname,   epoch_rel,   CDF_TYPE='CDF_TIME_TT2000'
		ocal_new -> WriteVar, /CREATE, optics_rel_vname,  optics_rel,  COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal_new -> WriteVar, /CREATE, relcal_gdu1_vname, relcal_gdu1, COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal_new -> WriteVar, /CREATE, relcal_gdu2_vname, relcal_gdu2, COMPRESSION='GZIP', GZIP_LEVEL=6
		
		;Variables
		relcal_vars = [epoch_rel_vname, optics_rel_vname, relcal_gdu1_vname, relcal_gdu2_vname]
		nRelVars    = n_elements(relcal_vars)

		;Copy variable attributes
		for i = 0, nRelVars-1 do begin
			;Get variable attribute names
			vAttrNames = ocal_old -> GetVarAttrNames(relcal_vars[i], COUNT=nVarAttrs)
			
			;Copy them to the new file
			for j = 0, nVarAttrs-1 do begin
				ocal_old -> CopyVarAttrTo, vAttrNames[j], ocal_new, $
				                           VARNAME = relcal_vars[i]
			endfor
		endfor
	
	;COPY RELATIVE CALIBRATIONS
	endif else begin
		nDupRel = 0
		nNewRel = 0
		ocal_old -> CopyVariableTo, epoch_rel_vname,   ocal_new
		ocal_old -> CopyVariableTo, optics_rel_vname,  ocal_new
		ocal_old -> CopyVariableTo, relcal_gdu1_vname, ocal_new
		ocal_old -> CopyVariableTo, relcal_gdu2_vname, ocal_new
	endelse

;-----------------------------------------------------
; Add Absolute Calibration \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;ADD RELATIVE CALIBRATIONS
	if MrStruct_HasTag(cal_data, 'TT2000_ABS') then begin
		;Number of new values
		nAbs = n_elements(cal_data)

		;Read the old values
		epoch_abs   = ocal_old -> Read(epoch_abs_vname,   NRECS=nEpoch,   FILLVALUE=epoch_fill)
		optics_abs  = ocal_old -> Read(optics_abs_vname,  NRECS=nOptics,  FILLVALUE=optics_fill)
		abscal_gdu1 = ocal_old -> Read(abscal_gdu1_vname, NRECS=nAbsGDU1, FILLVALUE=gdu1_fill)
		abscal_gdu2 = ocal_old -> Read(abscal_gdu2_vname, NRECS=nAbsGDU2, FILLVALUE=gdu2_fill)

		;Are there duplicate points?
		!Null = MrIsMember( epoch_abs, cal_data.tt2000_abs, iDup, COUNT=nDupAbs, $
		                    A_INDICES=iReplace, COMPLEMENT=iNew, NCOMPLEMENT=nNewAbs )
		
		;Replace duplicates
		;   - By requiring CAL_DATA to be an array of structures, we ensure that
		;     all variables contain the same number of records. Think of the
		;     structure array element as records.
		if nDupAbs gt 0 then begin
			;abs_OPTICS
			if MrStruct_HasTag(cal_data[0], 'OPTICS_ABS') $
				then optics_abs[iReplace]  = cal_data[iDup].optics_abs
				
			;absCAL_GDU2
			if MrStruct_HasTag(cal_data[0], 'ABSCAL_GDU1') $
				then abscal_gdu1[iReplace] = cal_data[iDup].abscal_gdu1
			
			;absCAL_GDU2
			if MrStruct_HasTag(cal_data[0], 'ABSCAL_GDU2') $
				then abscal_gdu2[iReplace] = cal_data[iDup].abscal_gdu2
		endif
		
		;Add new values
		if nNewAbs gt 0 then begin
			;Append and sort time
			epoch_abs = nEpoch eq 0 ? cal_data[iNew].tt2000_abs : [epoch_abs, cal_data[iNew].tt2000_abs]
			isort     = sort(epoch_abs)
		
			;Get values 
			;   - If the tag does not exist, the fill value will be returned
			new_optics_abs  = amb_cal_getvalue(cal_data[iNew], 'OPTICS_ABS',  optics_fill)
			new_abscal_gdu1 = amb_cal_getvalue(cal_data[iNew], 'ABSCAL_GDU1', gdu1_fill)
			new_abscal_gdu2 = amb_cal_getvalue(cal_data[iNew], 'ABSCAL_GDU2', gdu2_fill)

			;Append
			optics_abs  = nOptics  eq 0 ? new_optics_abs  : [ [ optics_abs  ], [ new_optics_abs  ] ]
			abscal_gdu1 = nAbsGDU1 eq 0 ? new_abscal_gdu1 : [ [ abscal_gdu1 ], [ new_abscal_gdu1 ] ]
			abscal_gdu2 = nAbsGDU2 eq 0 ? new_abscal_gdu2 : [ [ abscal_gdu2 ], [ new_abscal_gdu2 ] ]
			
			;Sort
			optics_abs  = optics_abs[isort]
			abscal_gdu1 = abscal_gdu1[isort]
			abscal_gdu2 = abscal_gdu2[isort]
		endif
		
		;Write data
		ocal_new -> WriteVar, /CREATE, epoch_abs_vname,   epoch_abs,   CDF_TYPE='CDF_TIME_TT2000'
		ocal_new -> WriteVar, /CREATE, optics_abs_vname,  optics_abs,  COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal_new -> WriteVar, /CREATE, abscal_gdu1_vname, abscal_gdu1, COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal_new -> WriteVar, /CREATE, abscal_gdu2_vname, abscal_gdu2, COMPRESSION='GZIP', GZIP_LEVEL=6
		
		;Variables
		abscal_vars = [epoch_abs_vname, optics_abs_vname, abscal_gdu1_vname, abscal_gdu2_vname]
		nAbsVars    = n_elements(abscal_vars)

		;Copy variable attributes
		for i = 0, nAbsVars-1 do begin
			;Get variable attribute names
			vAttrNames = ocal_old -> GetVarAttrNames(abscal_vars[i], COUNT=nVarAttrs)
			
			;Copy them to the new file
			for j = 0, nVarAttrs-1 do begin
				ocal_old -> CopyVarAttrTo, vAttrNames[j], ocal_new, $
				                           VARNAME = abscal_vars[i]
			endfor
		endfor
	
	;COPY RELATIVE CALIBRATIONS
	endif else begin
		nDupAbs = 0
		nNewAbs = 0
		ocal_old -> CopyVariableTo, epoch_abs_vname,   ocal_new
		ocal_old -> CopyVariableTo, optics_abs_vname,  ocal_new
		ocal_old -> CopyVariableTo, abscal_gdu1_vname, ocal_new
		ocal_old -> CopyVariableTo, abscal_gdu2_vname, ocal_new
	endelse

;-----------------------------------------------------
; Copy Metadata \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Copy metadata
	ocal_old -> CopyVariableTo, theta_labels_vname, ocal_new
	ocal_old -> CopyVariableTo, phi_labels_vname,   ocal_new

;-----------------------------------------------------
; Update Global Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Add an entry to the "MODS" global attribute
	new_mod = string(FORMAT='(%"v%s.%s.%s - Added (changed) %i (%i) relative and %i (%i) absolute calibrations")', $
	                 vx, vy_out, vz_out, nDupRel, nNewRel, nDupAbs, nNewAbs)
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