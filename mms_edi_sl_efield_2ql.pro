; docformat = 'rst'
;
; NAME:
;       mms_sl2ql_cdf
;
; PURPOSE:
;+
;   Convert an EDI E-field slow-look data file and convert it to a quick-look
;   data file by copying a subset of the variables variables.
;
; :Categories:
;   MMS
;
; :Params:
;       SL_FILE:        in, required, type=string
;                       Name of the "slow-look" data file to be turned into a
;                           "quick-look" file.
;       OUT_DIR:        in, optional, type=string, default=File_DirName(`SL_FILE`)
;
; :Returns:
;       QL_FILE:        Name of the quick-look file that is created.
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/09/10  -   Written by Matthew Argall
;-
function mms_edi_sl_efield_2ql, sl_file, out_dir
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Close the slow-look file
		if obj_valid(oSL) then obj_destroy, oSL
		
		;Close and delete the quick-look file
		if obj_valid(oQL) then begin
			file_delete, ql_file
			obj_destroy, oQL
		endif
		
		;Error
		void = cgErrorMSG(/quiet)
		return, ''
	endif

;-------------------------------------------------------
; Check Input File /////////////////////////////////////
;-------------------------------------------------------
	
	;Can only take one file
	if n_elements(sl_file) ne 1 then message, 'FILENAME must be a scalar string.'

	;Dissect the file name
	mms_dissect_filename, sl_file, SC        = sc, $
	                               INSTR     = instr, $
	                               MODE      = mode, $
	                               LEVEL     = level, $
	                               OPTDESC   = optdesc, $
	                               TSTART    = tstart, $
	                               VERSION   = version, $
	                               DIRECTORY = directory
	if n_elements(out_dir) eq 0 then out_dir = directory
	
	;Must be an "sl" (slow-look) file
	if instr   ne 'edi'    then message, 'SL_FILE must be an EDI file.'
	if level   ne 'ql'     then message, 'SL_FILE must be an "sl", or slow-look, file.'
	if optdesc ne 'efield' then message, 'SL_FILE must be an EDI E-field files.'

;-------------------------------------------------------
; Create Output File ///////////////////////////////////
;-------------------------------------------------------
	
	;Create a new file name
	ql_file = mms_construct_filename(sc, instr, mode, 'ql', $
	                                 OPTDESC   = optdesc, $
	                                 TSTART    = tstart, $
	                                 VERSION   = version, $
	                                 DIRECTORY = out_dir)
	
	;Open both files
	oSL = MrCDF_File(sl_file)
	oQL = MrCDF_File(ql_file, /CREATE, /CLOBBER)

;-------------------------------------------------------
; Copy Global Attributes ///////////////////////////////
;-------------------------------------------------------
	
	;Get all of the global attributes from SL and copy them to QL
	gAttrNames = oSL -> GetAttrNames(COUNT=count)
	for i = 0, count - 1 $
		do oSL -> CopyGlobalAttrTo, gAttrNames[i], oQL

;-------------------------------------------------------
; Copy Variable Attributes /////////////////////////////
;-------------------------------------------------------
	
	;Names of the variables we want to copy:
	vars      = [ 'Epoch', $
	              'Epoch_delta_plus', $
	              mms_construct_varname(sc, instr, 'E',        'dmpa'), $
	              mms_construct_varname(sc, instr, 'v_ExB',    'dmpa'), $
	              mms_construct_varname(sc, instr, 'E_bc',     'dmpa'), $
	              mms_construct_varname(sc, instr, 'v_ExB_bc', 'dmpa'), $
	              mms_construct_varname(sc, instr, 'quality_bc'      )  $
	            ]

	;Copy them to the quicklook file
	for i = 0, n_elements(vars) - 1 $
		do oSL -> CopyVariableTo, vars[i], oQL

;-------------------------------------------------------
; Finish ///////////////////////////////////////////////
;-------------------------------------------------------
	
	;Close the files
	obj_destroy, oSL
	obj_destroy, oQL
	
	return, ql_file
end