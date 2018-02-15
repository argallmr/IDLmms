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
;       2018/02/13  -   Handle cases when one or both guns are not operational. - MRA
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
;                       ENERGY_GD12 - Energy state for GD12
;                       ENERGY_GD21 - Energy state for GD21
;                       Q0_GD12     - Quality 0 counts for GD12
;                       Q0_GD21     - Quality 0 counts for GD21
;-
function mms_edi_q0_l2_create, edi_files, dss_file, defatt_file, tstart, tend, $
STATUS=status
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;TODO: Give error codes to specific errors.
		if n_elements(status) eq 0 || status eq 0 then status = 105

		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Everything starts out ok
	status = 0
	
;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Total number of files given
	nEDI = n_elements(edi_files)
	
	;Check if files exist and are readable
	if nEDI eq 0 then message, 'No EDI files given'
	if min(file_test(edi_files, /READ, /REGULAR)) eq 0 $
		then message, 'EDI files must exist and be readable.'
	
	;Burst mode flag
	tf_brst = stregex(edi_files[0], 'brst', /BOOLEAN)

;-----------------------------------------------------
; Collect Q0 Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Read Data
	;   - Automatically combines slow and fast survey data
	;   - Will check sc, instr, mode, level, optdesc
	q0_data = mms_edi_q0_l1a_read(edi_files, tstart, tend, STATUS=status)
	if status ge 100 then message, 'Error reading Q0 data.'

;-----------------------------------------------------
; Angular Bins \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Degrees to radians
	deg2rad = !dpi / 180.0D
	
	;Delta look direction (degrees)
	dphi   = 360.0 / 32.0
	dtheta = 360.0 / 512.0
	
	;Possible look angles (degrees)
	phi   = findgen(32)  * dphi
	theta = findgen(129) * dtheta

;-----------------------------------------------------
; Electron Trajectories \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Read sun pulse times
	dss = mms_dss_read_sunpulse(dss_file)

	;Read attitude data
	defatt = mms_fdoa_read_defatt(defatt_file)
	
	;Output structure
	q0_out = { n_gdu1:         q0_data.count_gd21, $
	           n_gdu2:         q0_data.count_gd12, $
	           tt2000_timetag: q0_data.tt2000_timetag, $
	           optics:         q0_data.optics }
	
	;Loop over GDU
	for i = 0, 1 do begin

	;-----------------------------------------------------
	; Determine Look Directions \\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;
		; For a given GDU1 gun firing direction (phi1,theta1) the GDU2 detector 
		; uses sensor pads n and n+1, where
		;    n = [15 - round(phi1/11.25) + 32] modulo 32
		; and phi1 is in degrees
		;
		; The polar angles of the gun in one GDU and of the detector in the other GDU
		; are the same (assuming no  aberration term has been applied to the detector
		; look direction).
		;
	
		;
		; The variable PHI contains the lower bound of each anode. Since two
		; anodes are in use, and n is the lower of the two, we calculate
		; the look direction using the lower-bound of anode n+1 -- the center
		; point.
		;
	
	
		;
		; The error in theta is negligible.
		;
		; The error in phi is +/-15 degrees.
		;
	
	
		;Map firing angle to look direction (radians)
		;   - lo is lower bound of n
		;   - hi is lower bound of n+2
		
		
		;DETECTOR 1
		if i eq 0 then begin
			;Skip if there is no data
			N = q0_data.count_gd21
			if N eq 0 then continue
			
			;Data
			edi_sys = 'EDI2'
			tag     = '_gdu1'
			time    = q0_data.tt2000_gd21
			
			;Transfer to output structure
			q0_out = create_struct( q0_out, $
			                        'energy' + tag, q0_data.energy_gd21, $
			                        'counts' + tag, q0_data.word15_gd21 )
			
			;Azimuth
			phi    = q0_data.azimuth_gd21 * deg2rad
			phi_lo = ( ( (q0_data.azimuth_gd21 - dphi) + 360.0 ) mod 360.0 ) * deg2rad
			phi_hi = ( (q0_data.azimuth_gd21 + dphi) mod 360.0 ) * deg2rad
		
			;Polar
			theta = q0_data.polar_gd21 * deg2rad
		
		;DETECTOR 2
		endif else begin
			;Skip if there is no data
			N = q0_data.count_gd12
			if N eq 0 then continue
			
			;Data
			edi_sys = 'EDI1'
			tag     = '_gdu2'
			time    = q0_data.tt2000_gd12
			
			;Transfer to output structure
			q0_out = create_struct( q0_out, $
			                        'energy' + tag, q0_data.energy_gd12, $
			                        'counts' + tag, q0_data.word15_gd12 )
			
			;Azimuth
			phi    = q0_data.azimuth_gd12 * deg2rad
			phi_lo = ( ( (q0_data.azimuth_gd12 - dphi) + 360.0 ) mod 360.0 ) * deg2rad
			phi_hi = ( (q0_data.azimuth_gd12 + dphi) mod 360.0 ) * deg2rad
		
			;Polar
			theta = q0_data.polar_gd12 * deg2rad
		endelse

	;-----------------------------------------------------
	; Electron Trajectories \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Allocate memory
		traj    = fltarr(3, N)
		traj_lo = fltarr(3, N)
		traj_hi = fltarr(3, N)
		
		;Vector
		traj[0,*] = sin(theta) * cos(phi)
		traj[1,*] = sin(theta) * sin(phi)
		traj[2,*] = cos(theta)

		;LO GDU1
		traj_lo[0,*] = sin(theta) * cos(phi_lo)
		traj_lo[1,*] = sin(theta) * sin(phi_lo)
		traj_lo[2,*] = cos(theta)
	
		;HI GDU1
		traj_hi[0,*] = sin(theta) * cos(phi_hi)
		traj_hi[1,*] = sin(theta) * sin(phi_hi)
		traj_hi[2,*] = cos(theta)

	;-----------------------------------------------------
	; EDI --> BCS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Rotate first to BCS (same orientation as OCS)
		edi_to_bcs = mms_instr_xxyz2ocs(edi_sys)
	
		;DET1
		traj_bcs    = MrVector_Rotate(edi_to_bcs, traj)
		traj_bcs_lo = MrVector_Rotate(edi_to_bcs, traj_lo)
		traj_bcs_hi = MrVector_Rotate(edi_to_bcs, traj_hi)

	;-----------------------------------------------------
	; Despin \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		bcs2dbcs     = mms_dss_xdespin(dss, time)
		traj_dbcs    = MrVector_Rotate(bcs2dbcs, traj_bcs)
		traj_dbcs_lo = MrVector_Rotate(bcs2dbcs, traj_bcs_lo)
		traj_dbcs_hi = MrVector_Rotate(bcs2dbcs, traj_bcs_hi)

	;-----------------------------------------------------
	; Rotate to GSE and GSM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;DBCS -> GSE
		traj_gse    = mms_rot_despun2gse(defatt, time, traj_dbcs,    TYPE='Z')
		traj_gse_lo = mms_rot_despun2gse(defatt, time, traj_dbcs_lo, TYPE='Z')
		traj_gse_hi = mms_rot_despun2gse(defatt, time, traj_dbcs_hi, TYPE='Z')
		
		;GSE -> GSM
		traj_gsm    = mms_rot_gse2gsm(time, traj_gse)
		traj_gsm_lo = mms_rot_gse2gsm(time, traj_gse_lo)
		traj_gsm_hi = mms_rot_gse2gsm(time, traj_gse_hi)

	;-----------------------------------------------------
	; Spherical Coordinates \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;BCS
		traj_bcs    = cv_coord(FROM_RECT=traj_bcs,    /TO_SPHERE, /DEGREES)
		traj_bcs_lo = cv_coord(FROM_RECT=traj_bcs_lo, /TO_SPHERE, /DEGREES)
		traj_bcs_hi = cv_coord(FROM_RECT=traj_bcs_hi, /TO_SPHERE, /DEGREES)
	
		;DBCS
		traj_dbcs    = cv_coord(FROM_RECT=traj_dbcs,    /TO_SPHERE, /DEGREES)
		traj_dbcs_lo = cv_coord(FROM_RECT=traj_dbcs_lo, /TO_SPHERE, /DEGREES)
		traj_dbcs_hi = cv_coord(FROM_RECT=traj_dbcs_hi, /TO_SPHERE, /DEGREES)
		
		;GSE
		traj_gse    = cv_coord(FROM_RECT=traj_gse,    /TO_SPHERE, /DEGREES)
		traj_gse_lo = cv_coord(FROM_RECT=traj_gse_lo, /TO_SPHERE, /DEGREES)
		traj_gse_hi = cv_coord(FROM_RECT=traj_gse_hi, /TO_SPHERE, /DEGREES)
	
		;GSM
		traj_gsm    = cv_coord(FROM_RECT=traj_gsm,    /TO_SPHERE, /DEGREES)
		traj_gsm_lo = cv_coord(FROM_RECT=traj_gsm_lo, /TO_SPHERE, /DEGREES)
		traj_gsm_hi = cv_coord(FROM_RECT=traj_gsm_hi, /TO_SPHERE, /DEGREES)

	;-----------------------------------------------------
	; Elevation to Polar Angle \\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;BCS
		traj_bcs[1,*]    = abs(traj_bcs[1,*] - 90.0)
		traj_bcs_lo[1,*] = abs(traj_bcs_lo[1,*] - 90.0)
		traj_bcs_hi[1,*] = abs(traj_bcs_hi[1,*] - 90.0)
	
		;DBCS
		traj_dbcs[1,*]    = abs(traj_dbcs[1,*] - 90.0)
		traj_dbcs_lo[1,*] = abs(traj_dbcs_lo[1,*] - 90.0)
		traj_dbcs_hi[1,*] = abs(traj_dbcs_hi[1,*] - 90.0)
	
		;GSE
		traj_gse[1,*]    = abs(traj_gse[1,*] - 90.0)
		traj_gse_lo[1,*] = abs(traj_gse_lo[1,*] - 90.0)
		traj_gse_hi[1,*] = abs(traj_gse_hi[1,*] - 90.0)
	
		;GSM
		traj_gsm[1,*]    = abs(traj_gsm[1,*] - 90.0)
		traj_gsm_lo[1,*] = abs(traj_gsm_lo[1,*] - 90.0)
		traj_gsm_hi[1,*] = abs(traj_gsm_hi[1,*] - 90.0)

	;-----------------------------------------------------
	; Deltas \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		traj_bcs_lo  = traj_bcs - traj_bcs_lo
		traj_bcs_hi  = traj_bcs_hi - traj_bcs
		traj_dbcs_lo = traj_dbcs - traj_dbcs_lo
		traj_dbcs_hi = traj_dbcs_hi - traj_dbcs
		traj_gse_lo  = traj_gse - traj_gse_lo
		traj_gse_hi  = traj_gse_hi - traj_gse
		traj_gsm_lo  = traj_gsm - traj_gsm_lo
		traj_gsm_hi  = traj_gsm_hi - traj_gsm

	;-----------------------------------------------------
	; Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		q0_out = create_struct( q0_out, $
		                        'tt2000' + tag,              temporary(time), $
		                        'traj'   + tag + '_bcs',     float((temporary(traj_bcs))[0:1,*]), $
		                        'traj'   + tag + '_bcs_lo',  float((temporary(traj_bcs_lo))[0:1,*]), $
		                        'traj'   + tag + '_bcs_hi',  float((temporary(traj_bcs_hi))[0:1,*]), $
		                        'traj'   + tag + '_dbcs',    (temporary(traj_dbcs))[0:1,*], $
		                        'traj'   + tag + '_dbcs_lo', (temporary(traj_dbcs_lo))[0:1,*], $
		                        'traj'   + tag + '_dbcs_hi', (temporary(traj_dbcs_hi))[0:1,*], $
		                        'traj'   + tag + '_gse',     (temporary(traj_gse))[0:1,*], $
		                        'traj'   + tag + '_gse_lo',  (temporary(traj_gse_lo))[0:1,*], $
		                        'traj'   + tag + '_gse_hi',  (temporary(traj_gse_hi))[0:1,*], $
		                        'traj'   + tag + '_gsm',     (temporary(traj_gsm))[0:1,*], $
		                        'traj'   + tag + '_gsm_lo',  (temporary(traj_gsm_lo))[0:1,*], $
		                        'traj'   + tag + '_gsm_hi',  (temporary(traj_gsm_hi))[0:1,*] )
	endfor

;-----------------------------------------------------
; Finished! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	return, q0_out

;-----------------------------------------------------
; Look Directions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; For a given GDU1 gun firing direction (phi1,theta1) the GDU2 detector 
	; uses sensor pads n and n+1, where
	;    n = [15 - round(phi1/11.25) + 32] modulo 32
	; and phi1 is in degrees
	;
	; The polar angles of the gun in one GDU and of the detector in the other GDU
	; are the same (assuming no  aberration term has been applied to the detector
	; look direction).
	;
	
	;
	; The variable PHI contains the lower bound of each anode. Since two
	; anodes are in use, and n is the lower of the two, we calculate
	; the look direction using the lower-bound of anode n+1 -- the center
	; point.
	;
	
	
	;
	; The error in theta is negligible.
	;
	; The error in phi is +/-15 degrees.
	;
	
	
	;Map firing angle to look direction (radians)
	;   - lo is lower bound of n
	;   - hi is lower bound of n+2
	if q0_data.count_gd12 gt 0 then begin
		;Azimuthal look direction
		phi_gd12    = q0_data.azimuth_gd12 * deg2rad
		phi_gd12_lo = ( ( (q0_data.azimuth_gd12 - dphi) + 360.0 ) mod 360.0 ) * deg2rad
		phi_gd12_hi = ( (q0_data.azimuth_gd12 + dphi) mod 360.0 ) * deg2rad
		
		;Polar look direction
		theta_gd12 = q0_data.polar_gd12 * deg2rad
	endif
	
	if q0_data.count_gd21 gt 0 then begin
		;Azimuthal look direction
		phi_gd21    = q0_data.azimuth_gd21 * deg2rad
		phi_gd21_lo = ( ( (q0_data.azimuth_gd21 - dphi) + 360.0 ) mod 360.0 ) * deg2rad
		phi_gd21_hi = ( (q0_data.azimuth_gd21 + dphi) mod 360.0 ) * deg2rad
	
		;Polar look direction
		theta_gd21 = q0_data.polar_gd21 * deg2rad
	endif

;-----------------------------------------------------
; Electron Trajectories \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;GD21
	if q0_data.count_gd21 gt 0 then begin
		;Allocate memory
		traj_gd21    = fltarr(3, q0_data.count_gd21)
		traj_gd21_lo = fltarr(3, q0_data.count_gd21)
		traj_gd21_hi = fltarr(3, q0_data.count_gd21)
		
		;Vector
		traj_gd21[0,*] = sin(theta_gd21) * cos(phi_gd21)
		traj_gd21[1,*] = sin(theta_gd21) * sin(phi_gd21)
		traj_gd21[2,*] = cos(theta_gd21)

		;LO GDU1
		traj_gd21_lo[0,*] = sin(theta_gd21) * cos(phi_gd21_lo)
		traj_gd21_lo[1,*] = sin(theta_gd21) * sin(phi_gd21_lo)
		traj_gd21_lo[2,*] = cos(theta_gd21)
	
		;HI GDU1
		traj_gd21_hi[0,*] = sin(theta_gd21) * cos(phi_gd21_hi)
		traj_gd21_hi[1,*] = sin(theta_gd21) * sin(phi_gd21_hi)
		traj_gd21_hi[2,*] = cos(theta_gd21)
	endif
	
	;GD12
	if q0_data.count_gd12 gt 0 then begin
		;Allocate memory
		traj_gd12    = fltarr(3, q0_data.count_gd12)
		traj_gd12_lo = fltarr(3, q0_data.count_gd12)
		traj_gd12_hi = fltarr(3, q0_data.count_gd12)
		
		;Vector
		traj_gd12[0,*] = sin(theta_gd12) * cos(phi_gd12)
		traj_gd12[1,*] = sin(theta_gd12) * sin(phi_gd12)
		traj_gd12[2,*] = cos(theta_gd12)
	
		;LO GDU2
		traj_gd12_lo[0,*] = sin(theta_gd12) * cos(phi_gd12_lo)
		traj_gd12_lo[1,*] = sin(theta_gd12) * sin(phi_gd12_lo)
		traj_gd12_lo[2,*] = cos(theta_gd12)
	
		;HI GDU2
		traj_gd12_hi[0,*] = sin(theta_gd12) * cos(phi_gd12_hi)
		traj_gd12_hi[1,*] = sin(theta_gd12) * sin(phi_gd12_hi)
		traj_gd12_hi[2,*] = cos(theta_gd12)
	endif

;-----------------------------------------------------
; EDI --> BCS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;GD21
	if q0_data.count_gd21 gt 0 then begin
		;Rotate first to BCS (same orientation as OCS)
		gd21_to_bcs = mms_instr_xxyz2ocs('EDI2')
	
		;DET1
		traj_gd21_bcs    = MrVector_Rotate(gd21_to_bcs, traj_gd21)
		traj_gd21_bcs_lo = MrVector_Rotate(gd21_to_bcs, traj_gd21_lo)
		traj_gd21_bcs_hi = MrVector_Rotate(gd21_to_bcs, traj_gd21_hi)
	endif
	
	;GD12
	if q0_data.count_gd12 gt 0 then begin
		;Rotate first to BCS (same orientation as OCS)
		gd12_to_bcs = mms_instr_xxyz2ocs('EDI1')
	
		;DET2
		traj_gd12_bcs    = MrVector_Rotate(gd12_to_bcs, traj_gd12)
		traj_gd12_bcs_lo = MrVector_Rotate(gd12_to_bcs, traj_gd12_lo)
		traj_gd12_bcs_hi = MrVector_Rotate(gd12_to_bcs, traj_gd12_hi)
	endif

;-----------------------------------------------------
; Despin \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Read sun pulse times
	dss = mms_dss_read_sunpulse(dss_file)

	;Despin det1
	if q0_data.counts_gd21 gt 0 then begin
		bcs2dbcs          = mms_dss_xdespin(dss, q0_data.tt2000_gd21)
		traj_gd21_dbcs    = MrVector_Rotate(bcs2dbcs, traj_gd21_bcs)
		traj_gd21_dbcs_lo = MrVector_Rotate(bcs2dbcs, traj_gd21_bcs_lo)
		traj_gd21_dbcs_hi = MrVector_Rotate(bcs2dbcs, traj_gd21_bcs_hi)
	endif
	
	;Despin det2
	if q0_data.counts_gd12 gt 0 then begin
		bcs2dbcs          = mms_dss_xdespin(dss, q0_data.tt2000_gd12)
		traj_gd12_dbcs    = MrVector_Rotate(bcs2dbcs, traj_gd12_bcs)
		traj_gd12_dbcs_lo = MrVector_Rotate(bcs2dbcs, traj_gd12_bcs_lo)
		traj_gd12_dbcs_hi = MrVector_Rotate(bcs2dbcs, traj_gd12_bcs_hi)
	endif
	
	;Free memory
	dss      = !Null
	bcs2dbcs = !Null

;-----------------------------------------------------
; Rotate to GSE and GSM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Read attitude data
	defatt = mms_fdoa_read_defatt(defatt_file)
	
	;
	; DBCS --> GSE
	;
	
	;det1
	traj_gd21_gse    = mms_rot_despun2gse(defatt, q0_data.tt2000_gd21, traj_gd21_dbcs,    TYPE='Z')
	traj_gd21_gse_lo = mms_rot_despun2gse(defatt, q0_data.tt2000_gd21, traj_gd21_dbcs_lo, TYPE='Z')
	traj_gd21_gse_hi = mms_rot_despun2gse(defatt, q0_data.tt2000_gd21, traj_gd21_dbcs_hi, TYPE='Z')
	
	;gd12
	traj_gd12_gse    = mms_rot_despun2gse(defatt, q0_data.tt2000_gd12, traj_gd12_dbcs,    TYPE='Z')
	traj_gd12_gse_lo = mms_rot_despun2gse(defatt, q0_data.tt2000_gd12, traj_gd12_dbcs_lo, TYPE='Z')
	traj_gd12_gse_hi = mms_rot_despun2gse(defatt, q0_data.tt2000_gd12, traj_gd12_dbcs_hi, TYPE='Z')
	defatt           = !Null

	;
	; GSE --> GSM
	;
	
	;gd21
	traj_gd21_gsm    = mms_rot_gse2gsm(q0_data.tt2000_gd21, traj_gd21_gse)
	traj_gd21_gsm_lo = mms_rot_gse2gsm(q0_data.tt2000_gd21, traj_gd21_gse_lo)
	traj_gd21_gsm_hi = mms_rot_gse2gsm(q0_data.tt2000_gd21, traj_gd21_gse_hi)
	
	;gd21
	traj_gd12_gsm    = mms_rot_gse2gsm(q0_data.tt2000_gd12, traj_gd12_gse)
	traj_gd12_gsm_lo = mms_rot_gse2gsm(q0_data.tt2000_gd12, traj_gd12_gse_lo)
	traj_gd12_gsm_hi = mms_rot_gse2gsm(q0_data.tt2000_gd12, traj_gd12_gse_hi)

;-----------------------------------------------------
; Spherical Coordinates \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Reduce data by one dimensions by converting to spherical coordinates
	;
	
	;gd21 BCS
	traj_gd21_bcs    = cv_coord(FROM_RECT=traj_gd21_bcs,    /TO_SPHERE, /DEGREES)
	traj_gd21_bcs_lo = cv_coord(FROM_RECT=traj_gd21_bcs_lo, /TO_SPHERE, /DEGREES)
	traj_gd21_bcs_hi = cv_coord(FROM_RECT=traj_gd21_bcs_hi, /TO_SPHERE, /DEGREES)
	
	;gd12 BCS
	traj_gd12_bcs    = cv_coord(FROM_RECT=traj_gd12_bcs,    /TO_SPHERE, /DEGREES)
	traj_gd12_bcs_lo = cv_coord(FROM_RECT=traj_gd12_bcs_lo, /TO_SPHERE, /DEGREES)
	traj_gd12_bcs_hi = cv_coord(FROM_RECT=traj_gd12_bcs_hi, /TO_SPHERE, /DEGREES)
	
	;gd21 DBCS
	traj_gd21_dbcs    = cv_coord(FROM_RECT=traj_gd21_dbcs,    /TO_SPHERE, /DEGREES)
	traj_gd21_dbcs_lo = cv_coord(FROM_RECT=traj_gd21_dbcs_lo, /TO_SPHERE, /DEGREES)
	traj_gd21_dbcs_hi = cv_coord(FROM_RECT=traj_gd21_dbcs_hi, /TO_SPHERE, /DEGREES)
	
	;gd12 DBCS
	traj_gd12_dbcs    = cv_coord(FROM_RECT=traj_gd12_dbcs,    /TO_SPHERE, /DEGREES)
	traj_gd12_dbcs_lo = cv_coord(FROM_RECT=traj_gd12_dbcs_lo, /TO_SPHERE, /DEGREES)
	traj_gd12_dbcs_hi = cv_coord(FROM_RECT=traj_gd12_dbcs_hi, /TO_SPHERE, /DEGREES)

	;gd21 GSE
	traj_gd21_gse    = cv_coord(FROM_RECT=traj_gd21_gse,    /TO_SPHERE, /DEGREES)
	traj_gd21_gse_lo = cv_coord(FROM_RECT=traj_gd21_gse_lo, /TO_SPHERE, /DEGREES)
	traj_gd21_gse_hi = cv_coord(FROM_RECT=traj_gd21_gse_hi, /TO_SPHERE, /DEGREES)
	
	;gd12 GSE
	traj_gd12_gse    = cv_coord(FROM_RECT=traj_gd12_gse,    /TO_SPHERE, /DEGREES)
	traj_gd12_gse_lo = cv_coord(FROM_RECT=traj_gd12_gse_lo, /TO_SPHERE, /DEGREES)
	traj_gd12_gse_hi = cv_coord(FROM_RECT=traj_gd12_gse_hi, /TO_SPHERE, /DEGREES)
	
	;gd21 GSM
	traj_gd21_gsm    = cv_coord(FROM_RECT=traj_gd21_gsm,    /TO_SPHERE, /DEGREES)
	traj_gd21_gsm_lo = cv_coord(FROM_RECT=traj_gd21_gsm_lo, /TO_SPHERE, /DEGREES)
	traj_gd21_gsm_hi = cv_coord(FROM_RECT=traj_gd21_gsm_hi, /TO_SPHERE, /DEGREES)
	
	;gd12 GSM
	traj_gd12_gsm    = cv_coord(FROM_RECT=traj_gd12_gsm,    /TO_SPHERE, /DEGREES)
	traj_gd12_gsm_lo = cv_coord(FROM_RECT=traj_gd12_gsm_lo, /TO_SPHERE, /DEGREES)
	traj_gd12_gsm_hi = cv_coord(FROM_RECT=traj_gd12_gsm_hi, /TO_SPHERE, /DEGREES)

	;
	;Convert from elevation to polar angle
	;
	
	;gd21 BCS
	traj_gd21_bcs[1,*]    = abs(traj_gd21_bcs[1,*] - 90.0)
	traj_gd21_bcs_lo[1,*] = abs(traj_gd21_bcs_lo[1,*] - 90.0)
	traj_gd21_bcs_hi[1,*] = abs(traj_gd21_bcs_hi[1,*] - 90.0)

	;gd12 BCS
	traj_gd12_bcs[1,*]    = abs(traj_gd12_bcs[1,*] - 90.0)
	traj_gd12_bcs_lo[1,*] = abs(traj_gd12_bcs_lo[1,*] - 90.0)
	traj_gd12_bcs_hi[1,*] = abs(traj_gd12_bcs_hi[1,*] - 90.0)
	
	;gd21 DBCS
	traj_gd21_dbcs[1,*]    = abs(traj_gd21_dbcs[1,*] - 90.0)
	traj_gd21_dbcs_lo[1,*] = abs(traj_gd21_dbcs_lo[1,*] - 90.0)
	traj_gd21_dbcs_hi[1,*] = abs(traj_gd21_dbcs_hi[1,*] - 90.0)

	;gd12 DBCS
	traj_gd12_dbcs[1,*]    = abs(traj_gd12_dbcs[1,*] - 90.0)
	traj_gd12_dbcs_lo[1,*] = abs(traj_gd12_dbcs_lo[1,*] - 90.0)
	traj_gd12_dbcs_hi[1,*] = abs(traj_gd12_dbcs_hi[1,*] - 90.0)
	
	;gd21 GSE
	traj_gd21_gse[1,*]    = abs(traj_gd21_gse[1,*] - 90.0)
	traj_gd21_gse_lo[1,*] = abs(traj_gd21_gse_lo[1,*] - 90.0)
	traj_gd21_gse_hi[1,*] = abs(traj_gd21_gse_hi[1,*] - 90.0)

	;gd12 GSE
	traj_gd12_gse[1,*]    = abs(traj_gd12_gse[1,*] - 90.0)
	traj_gd12_gse_lo[1,*] = abs(traj_gd12_gse_lo[1,*] - 90.0)
	traj_gd12_gse_hi[1,*] = abs(traj_gd12_gse_hi[1,*] - 90.0)
	
	;gd21 gsm
	traj_gd21_gsm[1,*]    = abs(traj_gd21_gsm[1,*] - 90.0)
	traj_gd21_gsm_lo[1,*] = abs(traj_gd21_gsm_lo[1,*] - 90.0)
	traj_gd21_gsm_hi[1,*] = abs(traj_gd21_gsm_hi[1,*] - 90.0)
	
	;gd12 gsm
	traj_gd12_gsm[1,*]    = abs(traj_gd12_gsm[1,*] - 90.0)
	traj_gd12_gsm_lo[1,*] = abs(traj_gd12_gsm_lo[1,*] - 90.0)
	traj_gd12_gsm_hi[1,*] = abs(traj_gd12_gsm_hi[1,*] - 90.0)

;-----------------------------------------------------
; Deltas \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;GD12
	traj_gd12_bcs_lo  = traj_gd12_bcs - traj_gd12_bcs_lo
	traj_gd12_bcs_hi  = traj_gd12_bcs_hi - traj_gd12_bcs
	traj_gd12_dbcs_lo = traj_gd12_dbcs - traj_gd12_dbcs_lo
	traj_gd12_dbcs_hi = traj_gd12_dbcs_hi - traj_gd12_dbcs
	traj_gd12_gse_lo  = traj_gd12_gse - traj_gd12_gse_lo
	traj_gd12_gse_hi  = traj_gd12_gse_hi - traj_gd12_gse
	traj_gd12_gsm_lo  = traj_gd12_gsm - traj_gd12_gsm_lo
	traj_gd12_gsm_hi  = traj_gd12_gsm_hi - traj_gd12_gsm
	
	;GD12
	traj_gd21_bcs_lo  = traj_gd21_bcs - traj_gd21_bcs_lo
	traj_gd21_bcs_hi  = traj_gd21_bcs_hi - traj_gd21_bcs
	traj_gd21_dbcs_lo = traj_gd21_dbcs - traj_gd21_dbcs_lo
	traj_gd21_dbcs_hi = traj_gd21_dbcs_hi - traj_gd21_dbcs
	traj_gd21_gse_lo  = traj_gd21_gse - traj_gd21_gse_lo
	traj_gd21_gse_hi  = traj_gd21_gse_hi - traj_gd21_gse
	traj_gd21_gsm_lo  = traj_gd21_gsm - traj_gd21_gsm_lo
	traj_gd21_gsm_hi  = traj_gd21_gsm_hi - traj_gd21_gsm

;-----------------------------------------------------
; Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Collect the data into a structure
	;   - Detector quantities: GD12 -> GDU2 and GD21 -> GDU1
	q0_out = { tt2000_gdu1:       q0_data.tt2000_gd21, $
	           tt2000_gdu2:       q0_data.tt2000_gd12, $
	           tt2000_timetag:    q0_data.tt2000_timetag, $
	           optics:            q0_data.optics, $
	           energy_gdu1:       q0_data.energy_gd21, $
	           energy_gdu2:       q0_data.energy_gd12, $
	           counts_gdu1:       q0_data.word15_gd21, $
	           counts_gdu2:       q0_data.word15_gd12, $
	           traj_gdu1_bcs:     float( traj_gd21_bcs[0:1,*] ), $
	           traj_gdu1_bcs_lo:  float( traj_gd21_bcs_lo[0:1,*] ), $
	           traj_gdu1_bcs_hi:  float( traj_gd21_bcs_hi[0:1,*] ), $
	           traj_gdu2_bcs:     float( traj_gd12_bcs[0:1,*] ), $
	           traj_gdu2_bcs_lo:  float( traj_gd12_bcs_lo[0:1,*] ), $
	           traj_gdu2_bcs_hi:  float( traj_gd12_bcs_hi[0:1,*] ), $
	           traj_gdu1_dbcs:    traj_gd21_dbcs[0:1,*], $
	           traj_gdu1_dbcs_lo: traj_gd21_dbcs_lo[0:1,*], $
	           traj_gdu1_dbcs_hi: traj_gd21_dbcs_hi[0:1,*], $
	           traj_gdu2_dbcs:    traj_gd12_dbcs[0:1,*], $
	           traj_gdu2_dbcs_lo: traj_gd12_dbcs_lo[0:1,*], $
	           traj_gdu2_dbcs_hi: traj_gd12_dbcs_hi[0:1,*], $
	           traj_gdu1_gse:     traj_gd21_gse[0:1,*], $
	           traj_gdu1_gse_lo:  traj_gd21_gse_lo[0:1,*], $
	           traj_gdu1_gse_hi:  traj_gd21_gse_hi[0:1,*], $
	           traj_gdu2_gse:     traj_gd12_gse[0:1,*], $
	           traj_gdu2_gse_lo:  traj_gd12_gse_lo[0:1,*], $
	           traj_gdu2_gse_hi:  traj_gd12_gse_hi[0:1,*], $
	           traj_gdu1_gsm:     traj_gd21_gsm[0:1,*], $
	           traj_gdu1_gsm_lo:  traj_gd21_gsm_lo[0:1,*], $
	           traj_gdu1_gsm_hi:  traj_gd21_gsm_hi[0:1,*], $
	           traj_gdu2_gsm:     traj_gd12_gsm[0:1,*], $
	           traj_gdu2_gsm_lo:  traj_gd12_gsm_lo[0:1,*], $
	           traj_gdu2_gsm_hi:  traj_gd12_gsm_hi[0:1,*] $
	         }

	;Done
	return, q0_out
end