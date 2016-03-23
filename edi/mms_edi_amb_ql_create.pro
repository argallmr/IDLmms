; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_create
;
; PURPOSE:
;+
;   Process EDI AMB L1A data, sorting counts by pitch angle instead of GDU and,
;   for burst data, calculate the pitch angle of each anode.
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
;       2015/10/27  -   Written by Matthew Argall
;       2015/11/04  -   Calculate pitch angle for ambient data. - MRA
;       2016/01/29  -   Split the QL and L2 processes into separate programs. Removed
;                           helper functions to separate files. - MRA
;       2016/02/01  -   Split the QL and L2 processes into separate programs. Removed
;                           helper functions to separate files. - MRA
;-
;*****************************************************************************************
;+
;   Process EDI AMB L1A data, sorting counts by pitch angle instead of GDU and,
;   for burst data, calculate the pitch angle of each anode.
;
;   Calling Sequences:
;       fname = mms_edi_ql_amb_create( fast_file )
;       fname = mms_edi_ql_amb_create( slow_file )
;       fname = mms_edi_ql_amb_create( brst_file )
;       fname = mms_edi_ql_amb_create( ..., tstart, tend )
;       fname = mms_edi_ql_amb_create( fast_file, slow_file )
;       fname = mms_edi_ql_amb_create( ..., tstart, thend )
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
;                           100      -  Unexpected trapped error
;
; :Returns:
;       EDI_OUT:    Structure of processed data. Fields are::
;                       TT2000_0    - TT2000 time tags for 0-pitch angle sorted data
;                       TT2000_180  - TT2000 time tags for 180-pitch angle sorted data
;                       TT2000_TT   - TT2000 time tags for packet-resolution data
;                       ENERGY_GDU1 - Electron energy for GDU1
;                       ENERGY_GDU2 - Electron energy for GDU2
;                       PACK_MODE   - Packing mode
;                       COUNTS1_0   - Counts1 data sorted by 0-degree pitch mode
;                       COUNTS1_180 - Counts1 data sorted by 180-degree pitch mode
;                       COUNTS2_0   - Counts2 data sorted by 0-degree pitch mode (brst only)
;                       COUNTS2_180 - Counts2 data sorted by 180-degree pitch mode (brst only)
;                       COUNTS3_0   - Counts3 data sorted by 0-degree pitch mode (brst only)
;                       COUNTS3_180 - Counts3 data sorted by 180-degree pitch mode (brst only)
;                       COUNTS4_0   - Counts4 data sorted by 0-degree pitch mode (brst only)
;                       COUNTS4_180 - Counts4 data sorted by 180-degree pitch mode (brst only)
;                       PA1_0       - Pitch angle associated with COUNTS1_0 (L2 only)
;                       PA1_180     - Pitch angle associated with COUNTS1_180 (L2 only)
;                       PA2_0       - Pitch angle associated with COUNTS2_0 (L2 only)
;                       PA2_180     - Pitch angle associated with COUNTS2_180 (L2 only)
;                       PA3_0       - Pitch angle associated with COUNTS3_0 (L2 only)
;                       PA3_180     - Pitch angle associated with COUNTS3_180 (L2 only)
;                       PA4_0       - Pitch angle associated with COUNTS4_0 (L2 only)
;                       PA4_180     - Pitch angle associated with COUNTS4_180 (L2 only)
;-
function mms_edi_amb_ql_create, amb_files, tstart, tend, $
CAL_FILE=cal_file, $
STATUS=status
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;TODO: Give error codes to specific errors.
		if n_elements(status) eq 0 || status eq 0 then status = 100

		MrPrintF, 'LogErr'
		return, !Null
	endif
	
;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Total number of files given
	nEDI = n_elements(amb_files)
	
	;Check if files exist and are readable
	if nEDI eq 0 then message, 'No EDI files given'
	if min(file_test(amb_files, /READ, /REGULAR)) eq 0 $
		then message, 'EDI files must exist and be readable.'
	
	;Check the calibration file
	tf_cal = 1
	if n_elements(cal_file) eq 0 || cal_file eq '' then begin
		MrPrintF, 'LogWarn', 'No calibration file given. Skipping cal process.'
		tf_cal = 0
	endif else if ~file_test(cal_file, /READ, /REGULAR) then begin
		MrPrintF, 'LogErr', 'Calibration file is not readable: "' + cal_file + '".'
		tf_cal = 0
	endif
	
	;Burst mode flag
	tf_brst =stregex(amb_files[0], 'brst', /BOOLEAN)

;-----------------------------------------------------
; Read EDI Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Read Data
	;   - Automatically combines slow and fast survey data
	;   - Will check sc, instr, mode, level, optdesc
	edi = mms_edi_amb_l1a_read(amb_files, tstart, tend, /EXPAND_ANGLES, STATUS=status)
	if status ge 100 then message, 'Error reading file.'
	
	;Number of elements.
	ncts = n_elements(edi.epoch_gdu1)
	ntt  = n_elements(edi.epoch_timetag)
	
	;Search for fill values and when the energy is 0
	ibad = where(edi.energy_gdu1 ne 500 and edi.energy_gdu1 ne 1000, nbad)
	if nbad gt 0 then MrPrintF, 'LogWarn', nbad, nbad/float(ntt)*100.0, FORMAT='(%"energy_gdu1 has %i (%0.2f\%) bad values")'
	ibad = where(edi.energy_gdu2 ne 500 and edi.energy_gdu2 ne 1000, nbad)
	if nbad gt 0 then MrPrintF, 'LogWarn', nbad, nbad/float(ntt)*100.0, FORMAT='(%"energy_gdu2 has %i (%0.2f\%) bad values")'

;-----------------------------------------------------
; Apply Calibrations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Read all of the calibration data, then prune
	; down within cal_apply
	;
	cals     = mms_edi_amb_cal_read(cal_file)
	cal_cnts = mms_edi_amb_calibrate(edi, temporary(cals), BRST=tf_brst)

	;Remove uncalibrated data
	if tf_brst then begin
		edi = MrStruct_RemoveTags(edi, ['COUNTS1_GDU1', 'COUNTS1_GDU2', $
		                                'COUNTS2_GDU1', 'COUNTS2_GDU2', $
		                                'COUNTS3_GDU1', 'COUNTS3_GDU2', $
		                                'COUNTS4_GDU1', 'COUNTS4_GDU2'])
	endif else begin
		edi = MrStruct_RemoveTags(edi, ['COUNTS1_GDU1', 'COUNTS1_GDU2'])
	endelse

	;Append calibrated data
	edi = create_struct(edi, temporary(cal_cnts))

;-----------------------------------------------------
; Sort by 0 and 180 Pitch Angle \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_brst $
		then counts_0_180 = mms_edi_amb_brst_sort_cnts(edi) $
		else counts_0_180 = mms_edi_amb_srvy_sort_cnts(edi)

;-----------------------------------------------------
; Output Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create the output structure
	edi_out = { tt2000_tt:   reform(edi.epoch_timetag), $
	            optics:      reform(edi.optics), $
	            energy_gdu1: reform(edi.energy_gdu1), $
	            energy_gdu2: reform(edi.energy_gdu2) $
	          }
	edi = !Null

	;Burst mode counts
	edi_out = create_struct(edi_out, temporary(counts_0_180))

	;Return
	if n_elements(status) eq 0 then status = 0
	return, edi_out
end