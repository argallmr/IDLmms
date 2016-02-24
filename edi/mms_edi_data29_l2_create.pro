; docformat = 'rst'
;
; NAME:
;    mms_edi_data29_l2_create
;
; PURPOSE:
;+
;   Process EDI L1A EFIELD data, selecting data29 electron counts.
;
; :Categories:
;    MMS, EDI
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
;       2016/02/18  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Process EDI L1A EFIELD data, filtering for qualty 0 background counts.
;
; :Params:
;       FILES:      in, required, type=string/strarr
;                   EDI E-Field files to be read and processed.
;       TSTART:     in, optional, types=string
;                   An ISO-8601 string indicating the start time of the interval to process.
;       TEND:       in, optional, types=string
;                   An ISO-8601 string indicating the end time of the interval to process.
;
; :Keywords:
;       OUTDIR:     in, optional, type=string, default='/nfs/edi/amb/'
;                   Directory in which to save data.
;       STATUS:     out, required, type=byte
;                   An error code. Values are:::
;                       OK      = 0
;                       Warning = 1-99
;                       Error   = 100-255
;                           105      -  Trapped error
;
; :Returns:
;       EDI_OUT:    Structure of processed data. Fields are::
;                       TT2000_GD12 - TT2000 time tags for the Gun1-Detector2 pair
;                       TT2000_GD21 - TT2000 time tags for the Gun2-Detector1 pair
;                       ENERGY_GD12 - Energy state for GD12
;                       ENERGY_GD21 - Energy state for GD21
;                       Q0_GD12     - Quality 0 counts for GD12
;                       Q0_GD21     - Quality 0 counts for GD21
;-
function mms_edi_data29_l2_create, files, tstart, tend, $
STATUS=status
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;TODO: Give error codes to specific errors.
		status = 105

		MrPrintF, 'LogErr'
		return, !Null
	endif
	
;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Total number of files given
	nEDI = n_elements(files)
	
	;Check if files exist and are readable
	if nEDI eq 0 then message, 'No EDI files given'
	if min(file_test(files, /READ, /REGULAR)) eq 0 $
		then message, 'EDI files must exist and be readable.'
	
	;Burst mode flag
	tf_brst = stregex(files[0], 'brst', /BOOLEAN)

;-----------------------------------------------------
; Collect DATA29 Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Read Data
	;   - Automatically combines slow and fast survey data
	;   - Will check sc, instr, mode, level, optdesc
	data29 = mms_edi_data29_l1a_read(files, tstart, tend)

	;
	; TODO: Calculate trajectory vector of electrons
	;

	;Done
	return, data29
end