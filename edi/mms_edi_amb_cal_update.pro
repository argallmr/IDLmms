; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_cal_update
;
; PURPOSE:
;+
;   Update the EDI calibration file. The steps are
;       1) Create a new, empty edi ambient calibration
;          file using mms_edi_amb_cal_write.
;       2) Find the latest existing calibration file and copy all
;          data into the file just created.
;
;   This is useful if the structure of the calibration file changes (i.e. the
;   metadata or variables). It is assumed that mms_edi_amb_cal_write will have
;   the version number updated. This file does not do anything with version
;   numbers. 
;
; :Categories:
;    MMS, EDI, L2, Ambient
;
; :Params:
;       SC:                 in, required, type=string
;                           Spacecraft for which the cal file is to be updated.
;
; :Returns:
;       NEW_FILE:           Name of the new calibration file.
;
; :See Also:
;   mms_edi_amb_cal_addvalue.pro
;   mms_edi_amb_cal_apply.pro
;   mms_edi_amb_cal_delvalue.pro
;   mms_edi_amb_cal_find.pro
;   mms_edi_amb_cal_read.pro
;   mms_edi_amb_cal_write.pro
;
; :History:
;    Modification History::
;       2016/02/25  -   Written by Matthew Argall
;-
function mms_edi_amb_cal_update, sc, $
CAL_PATH_ROOT=cal_path_root
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(old_cal) then obj_destroy, old_cal
		if obj_valid(new_cal) then obj_destroy, new_cal
		if file_test(new_file, /REGULAR) then file_delete, new_file
		MrPrintF, 'LogErr'
		return, ''
	endif

;-----------------------------------------------------
; Most Recent Cal File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find the most recent version of the cal file
	old_file = mms_edi_amb_cal_find(sc, CAL_PATH_ROOT=cal_path_root)
	old_cal = MrCDF_File(old_file)

;-----------------------------------------------------
; Create New Cal File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Write a new, empty cal file
	new_file = mms_edi_amb_cal_write(sc)
	
	;Parse the file name for its z-version
	mms_dissect_filename, new_file, VERSION=version, VZ=vz
	if vz ne 0 $
		then message, 'Z-Version of new file is not 0. Increase X-version or delete existing files.'

	;Open file so we can transfer variable data.
	new_cal  = MrCDF_File(new_file, /MODIFY)
	
;-----------------------------------------------------
; Copy Variable Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	prefix = strjoin([sc, 'edi'], '_') + '_'
	suffix = '_cal_l2'
	
	;Get the names of all global attributes in the old file
	varNames = old_cal -> GetVarNames(COUNT=nVars)
	
	;Copy them to the new file
	for i = 0, nVars-1 do begin
		
		;Translate names between versions
		;   - VERSION DEPENDENT!!
		name = varNames[i]
		case 1 of
			name eq prefix + 'relcal_gdu1' + suffix: name = prefix + 'rel_gdu1' + suffix
			name eq prefix + 'relcal_gdu2' + suffix: name = prefix + 'rel_gdu2' + suffix
			name eq prefix + 'abscal_gdu1' + suffix: name = prefix + 'abs_gdu1' + suffix
			name eq prefix + 'abscal_gdu2' + suffix: name = prefix + 'abs_gdu2' + suffix
			else: ;do nothing
		endcase

		;Get variable data
		data = old_cal -> Read(varNames[i], NRECS=nRecs)

		;Write variable data
		if nRecs gt 0 then new_cal -> WriteVar, name, data
	endfor

;-----------------------------------------------------
; Clean Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Close files
	obj_destroy, old_cal
	obj_destroy, new_cal

	return, new_file
end