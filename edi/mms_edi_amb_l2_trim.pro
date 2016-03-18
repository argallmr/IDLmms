; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_l2_write
;
; PURPOSE:
;+
;   Trim fat L2 files to skinny L2 files. There are several things that should be
;   changed manually. Use with caution.
;
; :Categories:
;    MMS, EDI, L2, Ambient
;
; :Returns:
;       AMB_FILE:           Name of the file created.
;
; :Author:
;    Matthew Argall::
;        University of New Hampshire
;        Morse Hall Room 348
;        8 College Road
;        Durham, NH 03824
;        matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/03/02  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Reduce the size of a single edi_[srvy|brst]_l2_amb data file by changing trajectory
;   delta values to scalars. Also, update metadata quantities.
;
; :Params:
;       FILE:       in, required, type=string
;                   Name of the file to be altered.
;
; :Keywords:
;       STATUS:     out, optional, type=byte
;                   Status flag. Successful result returns 0.
;       DELETE:     in, optional, type=boolean, default=0
;                   If set `FILE` will be deleted when completed.
;-
function mms_edi_amb_l2_trim1, file, $
DELETE=delete, $
STATUS=status
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Close and delete the file, if it was created
		if obj_valid(cdf_src)  then obj_destroy, cdf_src
		if obj_valid(cdf_dest) then obj_destroy, cdf_dest
		if n_elements(fout) gt 0 && file_test(fout, /REGULAR) && file_test(file, /REGULAR) $
			 then file_delete, fout
		
		;Report error
		if n_elements(status) eq 0 || status eq 0 then status = 100
		MrPrintF, 'LogErr'
		
		;Return
		return, ''
	endif
	
	;Assume success
	status = 0

	;Constants for output file
	vout = '2.0.2'

;---------------------------------------------------------------------
; Form Output File Name //////////////////////////////////////////////
;---------------------------------------------------------------------
	;Dissect the input file name
	mms_dissect_filename, file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level, $
	                            OPTDESC=optdesc, TSTART=tstart, VERSION=version, $
	                            DIRECTORY=dir
	
	;Compose output file name
	fout = mms_forge_filename(sc, instr, mode, level, tstart, vout, $
	                          OPTDESC=optdesc, DIRECTORY=dir)

;---------------------------------------------------------------------
; Open Files /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Source and destination files
	cdf_src  = MrCDF_File(file)
	cdf_dest = MrCDF_File(fout, /CREATE, /CLOBBER)

;---------------------------------------------------------------------
; Copy Global Attributes /////////////////////////////////////////////
;---------------------------------------------------------------------
	;Get the names and number of global attributes
	gAttrNames = cdf_src -> GetAttrNames(COUNT=nGAttrs)
	
	;Copy each to the destination file
	foreach gAtt, gAttrNames do cdf_src -> CopyGlobalAttrTo, gAtt, cdf_dest

;---------------------------------------------------------------------
; Copy Variable Attributes ///////////////////////////////////////////
;---------------------------------------------------------------------
	;Get the names and number of global attributes
	vAttrNames = cdf_src -> GetAttrNames(COUNT=nGAttrs, /VARIABLE_SCOPE)
	
	;Copy each to the destination file
	foreach vAtt, vAttrNames do cdf_src -> CopyVarAttrTo, vAtt, cdf_dest
	
	;Add new variable attributeS
	cdf_dest -> CreateAttr, 'DELTA_MINUS', /VARIABLE_SCOPE
	cdf_dest -> CreateAttr, 'DELTA_PLUS',  /VARIABLE_SCOPE
	cdf_dest -> CreateAttr, 'VAR_NOTES',   /VARIABLE_SCOPE

;---------------------------------------------------------------------
; Copy Variables To //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Get all variable names
	varNames = cdf_src -> GetVarNames()
	
	;Weed out all of the traj_*_delta_(plus/minus) variables
	tf_bad   = stregex(varNames, 'mms[1-4]_edi_traj[1-4]_(gse|gsm)_(0|180)_delta_(minus|plus)_(brst|srvy)_l2', /BOOLEAN)
	tf_bad   = tf_bad or stregex(varNames, 'mms[1-4]_edi_traj_delta_labl', /BOOLEAN)
	iGood    = where(~tf_bad, nGood)
	theNames = varNames[iGood]

	;Copy the good variables
	foreach var, theNames do cdf_src -> CopyVariableTo, var, cdf_dest

;---------------------------------------------------------------------
; Time Attributes ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;
	; Add DELTA_PLUS/MINUS attributes to time
	;

	;Get the time variables
	tf_time = stregex(varNames, 'epoch_(0|180)', /BOOLEAN)
	itime   = where(tf_time, nTime)
	tNames  = varNames[itime]
	
	;Delta
	if mode eq 'brst' $
		then delta = ( 1.0D / 1024D) / 4.0D $
		else delta = (16.0D / 1024D) / 4.0D
	
	;Set DELTA_PLUS/MINUS
	foreach var, tNames do begin
		cdf_dest -> WriteVarAttr, var, 'DELTA_MINUS', long64(delta * 1d9)
		cdf_dest -> WriteVarAttr, var, 'DELTA_PLUS',  long64(delta * 1d9)
	endforeach

;---------------------------------------------------------------------
; Flux Attributes ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;
	; Add SI_Conversion to FLUX variables
	;
	tf_flux  = stregex(varNames, 'mms[1-4]_edi_flux[1-4]_(0|180)_(brst|srvy)_l2', /BOOLEAN)
	iflux    = where(tf_flux, nFlux)
	theNames = varNames[iFlux]
	
	;Add SI_Conversion
	foreach var, theNames do cdf_dest -> WriteVarAttr, var, 'SI_CONVERSION', '1e4>m^-2 s^-1'

;---------------------------------------------------------------------
; Trajectory Variable Attributes /////////////////////////////////////
;---------------------------------------------------------------------
	;Variable names for which we want to edit attributes
	tf_edit  = stregex(varNames, 'mms[1-4]_edi_traj[1-4]_(gse|gsm)_(0|180)_(brst|srvy)_l2', /BOOLEAN)
	iedit    = where(tf_edit, nEdit)
	theNames = varNames[iedit]
	
	;New deltas
	delta_theta = 7.0   ;90.0 / 129.0 / 2.0   ;Acceptance cone
	if mode eq 'brst' $
		then delta_phi = 11.25 / 2.0 $
		else delta_phi = 11.25
	
	;Edit attributes
	foreach var, theNames do begin
		;Delete DELTA_PLUS/MINUS_VAR
		cdf_dest -> DelVarAttr, 'DELTA_MINUS_VAR', var
		cdf_dest -> DelVarAttr, 'DELTA_PLUS_VAR',  var

		;Add/edit values
		cdf_dest -> WriteVarAttr, var, 'DELTA_MINUS', [delta_theta, delta_phi]
		cdf_dest -> WriteVarAttr, var, 'DELTA_PLUS',  [delta_theta, delta_phi]
		cdf_dest -> WriteVarAttr, var, 'VAR_NOTES',   'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                              '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                              'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                              "of the instrument. Errors reflect the instrument's acceptance cone. For more " + $
		                                              'details about errors, contact the EDI instrument team.'
	endforeach

;---------------------------------------------------------------------
; Update Global Attribute Information ////////////////////////////////
;---------------------------------------------------------------------

	;Add an entry to the "MODS" global attribute
	new_mod = string(vout, FORMAT='(%"v%s - Reduced file size with scalar errors.")')
	cdf_dest -> WriteGlobalAttr, 'MODS', temporary(new_mod)
	
	;Change the "LOGICAL_FILE_ID
	;   - Use GENTRYNUM to overwrite old value [it is like an array index]
	;   - The default is to append values
	cdf_dest -> WriteGlobalAttr, 'Logical_file_id', cgRootName(fout), GENTRYNUM=0
	
	;Change the generation date
	caldat, systime(/JULIAN, /UTC), month, day, year
	cdf_dest -> WriteGlobalAttr, 'Generation_date', string(year, month, day, FORMAT='(i04, i02, i02)'), GENTRYNUM=0
	
	;Change the data verion
	cdf_dest -> WriteGlobalAttr, 'Data_version', vout, GENTRYNUM=0
	
	;Change 
	cdf_dest -> WriteGlobalAttr, 'PI_name', 'Roy Torbert', GENTRYNUM=0

;-----------------------------------------------------
; Clean Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Close files
	obj_destroy, cdf_src
	obj_destroy, cdf_dest
	
	;Delete the old file
	if keyword_set(delete) then file_delete, file

	return, fout
end


;+
;   Reduce the size of edi_[srvy|brst]_l2_amb data files by changing trajectory delta
;   values to scalars. Also, update metadata quantities.
;-
pro mms_edi_amb_l2_trim
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		void = MrStdLog('stderr')
		return
	endif

	;Get the current date and time
	caldat, systime(1, /JULIAN, /UTC), month, day, year, hour, minute, second
	date = string(year, month, day,     FORMAT='(i04,i02,i02)')
	time = string(hour, minute, second, FORMAT='(i02,i02,i02)')

	;Create the log file
	logfile = filepath('mms_amb_l2_reduce' + '_' + date + '_' + time + '.log', ROOT_DIR='/nfs/edi/logs/batch_logs')
	oLog    = MrStdLog(logfile)
	
	;Print header
	oLog -> AddText, ''
	oLog -> AddText, 'Reducing file size of Amb L2 files'
	oLog -> AddText, ''
	oLog -> AddText, ''
	
	;Find files
	files = file_search('/nfs/edi/temp', 'mms*_edi_brst_l2_amb_*_v1.6.0.cdf', COUNT=nFiles)
	
	;Loop over each file
	foreach file, files do begin
		;Trim the file
		fout = mms_edi_amb_l2_trim1(file, STATUS=status, /DELETE)
		
		;Report to log
		MrPrintF, 'LogText', status, fout, FORMAT='(2x, i3, 4x, a0)'
	endforeach

	;Close the log file
	void = MrStdLog('stderr')
end
