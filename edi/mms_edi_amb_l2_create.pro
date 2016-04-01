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
;       2016/01/29  -   Split the QL and L2 processes into separate programs. - MRA
;       2016/03/30  -   Added the ABSCAL keyword. - MRA
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
;       AMB_FILE:       in, required, type=string
;                       The EDI L1A ambient mode file to be turned into L2 data.
;       CAL_FILE:       in, required, type=string
;                       The ambient mode calibration file containing relative and absolute
;                           calibration parameters.
;       DSS_FILE:       in, required, type=string
;                       File name of the digital sun sensor HK101 sunpulse file. Used to
;                           despin data.
;       DEFATT_FILE:    in, required, type=string
;                       File name of the FDOA definitive attitude file. Used to transform
;                           trajectory vectors into GSE and GSM.
;       TSTART:         in, optional, types=string
;                       An ISO-8601 string indicating the start time of the interval to process.
;       TEND:           in, optional, types=string
;                       An ISO-8601 string indicating the end time of the interval to process.
;
; :Keywords:
;       ABSCAL:     in, optional, type=boolean, default=1
;                   If set to zero, absolute calibrations will not be applied to the data.
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
function mms_edi_amb_l2_create, amb_files, cal_file, dss_file, defatt_file, tstart, tend, $
ABSCAL=abscal, $
STATUS=status
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;TODO: Give error codes to specific errors.
		if n_elements(status) eq 0 || status eq 0 $
			then status = 100
		
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Everything starts out ok
	status = 0
	
;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Total number of files given
	nEDI = n_elements(amb_files)
	nCal = n_elements(cal_file)
	nFGM = n_elements(fgm_files)
	
	;Check if files exist and are readable
	if nEDI eq 0 then message, 'No EDI files given'
	if nCal eq 0 then message, 'No EDI calibration file given.'
	if min(file_test(amb_files, /READ, /REGULAR)) eq 0 $
		then message, 'EDI files must exist and be readable.'
	
	;Burst mode flag
	tf_brst   = stregex(amb_files[0], 'brst', /BOOLEAN)
	tf_abscal = n_elements(abscal) eq 0 ? 1 : keyword_set(abscal)

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Read Data
	;   - Automatically combines slow and fast survey data
	;   - Will check sc, instr, mode, level, optdesc
	;   - Expand AZIMUTH and POLAR angles to COUNTS time resolution
	edi = mms_edi_amb_l1a_read(amb_files, tstart, tend, /EXPAND_ANGLES, STATUS=status)
	if status ge 100 then message, 'Error reading Amb L1A data.'

	;Read calibration file
	cals = mms_edi_amb_cal_read(cal_file)

;-----------------------------------------------------
; Apply Calibrations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Read Calibration File
	cal_cnts = mms_edi_amb_calibrate(edi, cals, ABSCAL=tf_abscal, BRST=tf_brst)

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
; Calculate Pitch Angles \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Compute trajectories
	;   - Requires: azimuth, polar, epoch, pitch, pack
	traj = mms_edi_amb_l2_trajectories(edi, dss_file, defatt_file)

;-----------------------------------------------------
; Output Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create the output structure
	edi_out = { tt2000_timetag: reform(edi.epoch_timetag), $
	            optics:         reform(edi.optics), $
	            energy_gdu1:    reform(edi.energy_gdu1), $
	            energy_gdu2:    reform(edi.energy_gdu2) $
	          }
	edi = !Null

	;Burst mode counts
	edi_out = create_struct(edi_out, temporary(counts_0_180), temporary(traj))

	return, edi_out
end