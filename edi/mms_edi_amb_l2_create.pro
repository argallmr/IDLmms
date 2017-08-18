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
; Operations Bitmask \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Create an operations bitmask
	bitmask = mms_edi_amb_ops_bitmask(edi)

	;Update the EDI structure
;	edi = MrStruct_RemoveTags(edi, ['PITCH_MODE', 'PACK_MODE', $
;	                                'PERP_ONESIDE', 'PERP_BIDIR'])

;-----------------------------------------------------
; Apply Calibrations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Annode assocated with each channel
	anodes = mms_edi_amb_anodes(edi.azimuth, bitmask, edi.pitch_gdu1, edi.pitch_gdu2, BRST=tf_brst)

	;Read Calibration File
	cal_cnts = mms_edi_amb_calibrate( edi, cals, temporary(anodes), bitmask, $
	                                  ABSCAL = tf_abscal )

	;Remove uncalibrated data
	edi = MrStruct_RemoveTags(edi, ['COUNTS_GDU1', 'COUNTS_GDU2'])

	;Append calibrated data
	edi = create_struct(edi, temporary(cal_cnts))

;-----------------------------------------------------
; Particle Trajectories \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Azimuth look direction associated with each channel
	phi = mms_edi_amb_anodes_phi(edi.azimuth, bitmask, edi.pitch_gdu1, edi.pitch_gdu2, BRST=tf_brst)

	;Incident trajectories
	traj = mms_edi_amb_traj( temporary(phi), reform(edi.polar) )

	;Incident trajectories
	;   - GDU1 and GDU2 have identical time tags
	traj = mms_edi_amb_traj_rotate( edi.epoch_gdu1, temporary(traj), dss_file, defatt_file )

	;Combine data
	edi = create_struct(edi, temporary(traj))

;-----------------------------------------------------
; Apply Flip Bit \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	iFlip = where( edi.flip_flag, nFlip )
	if nFlip GT 0 then begin
		iAlt = where( MrBitGet( bitmask[iFlip], 3 ), nAlt )
		if nAlt gt 0 then begin
			fillval = -1e31
			iFlip   = iFlip[iAlt]
			edi.counts_gdu1[iFlip,*]      = fillval
			edi.counts_gdu2[iFlip,*]      = fillval
			edi.delta_gdu1[iFlip,*]       = fillval
			edi.delta_gdu2[iFlip,*]       = fillval
			edi.traj_bcs_gdu1[*,iFlip,*]  = fillval
			edi.traj_dbcs_gdu1[*,iFlip,*] = fillval
			edi.traj_gse_gdu1[*,iFlip,*]  = fillval
			edi.traj_gsm_gdu1[*,iFlip,*]  = fillval
			edi.traj_bcs_gdu2[*,iFlip,*]  = fillval
			edi.traj_dbcs_gdu2[*,iFlip,*] = fillval
			edi.traj_gse_gdu2[*,iFlip,*]  = fillval
			edi.traj_gsm_gdu2[*,iFlip,*]  = fillval
		endif
	endif

;-----------------------------------------------------
; Sort Results by Mode and Pitch Angle \\\\\\\\\\\\\\\
;-----------------------------------------------------
	results = mms_edi_amb_sort( temporary(edi), temporary(bitmask) )

	return, results
end