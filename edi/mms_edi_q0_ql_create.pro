; docformat = 'rst'
;
; NAME:
;    mms_edi_q0_l2_create
;
; PURPOSE:
;+
;   Process EDI L1A EFIELD data, filtering for qualty 0 background counts.
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
;       2016/02/17  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Process EDI L1A EFIELD data, filtering for qualty 0 background counts.
;
; :Params:
;       SC:         in, required, type=string/strarr
;                   Either the spacecraft identifier ('mms1', 'mms2', 'mms3', 'mms4')
;                       of the spacecraft for which to process data or the EDI data
;                       file(s) to be processed. If files, they may be 'fast' and/or 'slow'
;                       mode data files.
;       MODE:       in, required, type=string/strarr
;                   Either the mode ('srvy', 'brst') of data to process or FGM
;                       data file names used to calculate pitch angle if 'brst' files
;                       are given for `SC`. 
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
;                       OPTICS      - Optics state during operation
;                       ENERGY_GD12 - Energy state for GD12
;                       ENERGY_GD21 - Energy state for GD21
;                       Q0_GD12     - Quality 0 counts for GD12
;                       Q0_GD21     - Quality 0 counts for GD21
;-
function mms_edi_q0_ql_create, files, tstart, tend, $
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
; Collect Q0 Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Read Data
	;   - Automatically combines slow and fast survey data
	;   - Will check sc, instr, mode, level, optdesc
	q0_data = mms_edi_q0_l1a_read(files, tstart, tend)

;-----------------------------------------------------
; Output Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	q0_out = { tt2000_gdu1:    q0_data.tt2000_gd21, $
	           tt2000_gdu2:    q0_data.tt2000_gd12, $
	           tt2000_timetag: q0_data.tt2000_timetag, $
	           optics:         q0_data.optics, $
	           energy_gdu1:    q0_data.energy_gd21, $
	           energy_gdu2:    q0_data.energy_gd21, $
	           counts_gdu1:    q0_data.word15_gd21, $
	           counts_gdu2:    q0_data.word15_gd12 $
	         }
	
	;Done
	return, q0_out
end