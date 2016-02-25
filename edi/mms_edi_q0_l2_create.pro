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
		status = 105

		MrPrintF, 'LogErr'
		return, !Null
	endif
	
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
	q0_data = mms_edi_q0_l1a_read(edi_files, tstart, tend)

;-----------------------------------------------------
; Angular Bins \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Degrees to radians
	deg2rad = !dpi / 360.0D
	
	;Delta look direction (degrees)
	dphi   = 360.0 / 32.0
	dtheta = 360.0 / 512.0
	
	;Possible look angles (degrees)
	phi   = findgen(32)  * dphi
	theta = findgen(129) * dtheta

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
	phi_det1    = phi[ (16 - round(q0_data.azimuth_gd21/dphi) + 32) mod 32 ] * deg2rad
	phi_det2    = phi[ (16 - round(q0_data.azimuth_gd12/dphi) + 32) mod 32 ] * deg2rad
	phi_det1_lo = phi[ (15 - round(q0_data.azimuth_gd21/dphi) + 32) mod 32 ] * deg2rad
	phi_det2_lo = phi[ (15 - round(q0_data.azimuth_gd12/dphi) + 32) mod 32 ] * deg2rad
	phi_det1_hi = phi[ (17 - round(q0_data.azimuth_gd21/dphi) + 32) mod 32 ] * deg2rad
	phi_det2_hi = phi[ (17 - round(q0_data.azimuth_gd12/dphi) + 32) mod 32 ] * deg2rad
	
	;Polar look direction (radians)
	theta_det1 = q0_data.polar_gd21 * deg2rad
	theta_det2 = q0_data.polar_gd12 * deg2rad

;-----------------------------------------------------
; Error in Electron Trajectories \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Allocate memory
	traj_det1    = fltarr(3, q0_data.count_gd21)
	traj_det1_lo = fltarr(3, q0_data.count_gd21)
	traj_det1_hi = fltarr(3, q0_data.count_gd21)
	traj_det2    = fltarr(3, q0_data.count_gd12)
	traj_det2_lo = fltarr(3, q0_data.count_gd12)
	traj_det2_hi = fltarr(3, q0_data.count_gd12)
	
	;GDU1
	traj_det1[0,*] = -sin(theta_det1) * cos(phi_det1)
	traj_det1[1,*] = -sin(theta_det1) * sin(phi_det1)
	traj_det1[2,*] = -sin(theta_det1)
	
	;LO GDU1
	traj_det1_lo[0,*] = -sin(theta_det1) * cos(phi_det1_lo)
	traj_det1_lo[1,*] = -sin(theta_det1) * sin(phi_det1_lo)
	traj_det1_lo[2,*] = -sin(theta_det1)
	
	;HI GDU1
	traj_det1_hi[0,*] = -sin(theta_det1) * cos(phi_det1_hi)
	traj_det1_hi[1,*] = -sin(theta_det1) * sin(phi_det1_hi)
	traj_det1_hi[2,*] = -sin(theta_det1)
	
	;GDU2
	traj_det2[0,*] = -sin(theta_det2) * cos(phi_det2)
	traj_det2[1,*] = -sin(theta_det2) * sin(phi_det2)
	traj_det2[2,*] = -sin(theta_det2)
	
	;LO GDU2
	traj_det2_lo[0,*] = -sin(theta_det2) * cos(phi_det2_lo)
	traj_det2_lo[1,*] = -sin(theta_det2) * sin(phi_det2_lo)
	traj_det2_lo[2,*] = -sin(theta_det2)
	
	;HI GDU2
	traj_det2_hi[0,*] = -sin(theta_det2) * cos(phi_det2_hi)
	traj_det2_hi[1,*] = -sin(theta_det2) * sin(phi_det2_hi)
	traj_det2_hi[2,*] = -sin(theta_det2)

;-----------------------------------------------------
; EDI --> BCS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Rotate first to BCS (same orientation as OCS)
	edi1_to_bcs = mms_instr_xxyz2ocs('EDI1')
	edi2_to_bcs = mms_instr_xxyz2ocs('EDI2')
	
	;DET1
	traj_det1_bcs    = MrVector_Rotate(edi1_to_bcs, traj_det1)
	traj_det1_bcs_lo = MrVector_Rotate(edi1_to_bcs, traj_det1_lo)
	traj_det1_bcs_hi = MrVector_Rotate(edi1_to_bcs, traj_det1_hi)
	
	;DET2
	traj_det2_bcs    = MrVector_Rotate(edi2_to_bcs, traj_det2)
	traj_det2_bcs_lo = MrVector_Rotate(edi2_to_bcs, traj_det2_lo)
	traj_det2_bcs_hi = MrVector_Rotate(edi2_to_bcs, traj_det2_hi)

;-----------------------------------------------------
; Despin \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Read sun pulse times
	dss = mms_dss_read_sunpulse(dss_file)

	;Despin det1
	bcs2dbcs          = mms_dss_xdespin(dss, q0_data.tt2000_gd21, /SPINUP)
	traj_det1_dbcs    = MrVector_Rotate(bcs2dbcs, temporary(traj_det1_bcs))
	traj_det1_dbcs_lo = MrVector_Rotate(bcs2dbcs, temporary(traj_det1_bcs_lo))
	traj_det1_dbcs_hi = MrVector_Rotate(bcs2dbcs, temporary(traj_det1_bcs_hi))
	
	;Despin det2
	bcs2dbcs          = mms_dss_xdespin(temporary(dss), q0_data.tt2000_gd12, /SPINUP)
	traj_det2_dbcs    = MrVector_Rotate(bcs2dbcs, temporary(traj_det2_bcs))
	traj_det2_dbcs_lo = MrVector_Rotate(bcs2dbcs, temporary(traj_det2_bcs_lo))
	traj_det2_dbcs_hi = MrVector_Rotate(bcs2dbcs, temporary(traj_det2_bcs_hi))
	bcs2dbcs          = !Null

;-----------------------------------------------------
; Rotate to GSE and GSM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Read attitude data
	defatt = mms_fdoa_read_defatt(defatt_file)
	
	;
	;DBCS -> GSE
	;
	
	;det1
	traj_det1_gse    = mms_rot_despun2gse(defatt, q0_data.tt2000_gd21, temporary(traj_det1_dbcs))
	traj_det1_gse_lo = mms_rot_despun2gse(defatt, q0_data.tt2000_gd21, temporary(traj_det1_dbcs_lo))
	traj_det1_gse_hi = mms_rot_despun2gse(defatt, q0_data.tt2000_gd21, temporary(traj_det1_dbcs_hi))
	
	;det2
	traj_det2_gse    = mms_rot_despun2gse(defatt, q0_data.tt2000_gd12, temporary(traj_det2_dbcs))
	traj_det2_gse_lo = mms_rot_despun2gse(defatt, q0_data.tt2000_gd12, temporary(traj_det2_dbcs_lo))
	traj_det2_gse_hi = mms_rot_despun2gse(defatt, q0_data.tt2000_gd12, temporary(traj_det2_dbcs_hi))
	defatt           = !Null
	
	;
	;GSE -> GSM
	;
	
	;det1
	traj_det1_gsm    = mms_rot_gse2gsm(q0_data.tt2000_gd21, traj_det1_gse)
	traj_det1_gsm_lo = mms_rot_gse2gsm(q0_data.tt2000_gd21, traj_det1_gse_lo)
	traj_det1_gsm_hi = mms_rot_gse2gsm(q0_data.tt2000_gd21, traj_det1_gse_hi)
	
	;det1
	traj_det2_gsm    = mms_rot_gse2gsm(q0_data.tt2000_gd12, traj_det2_gse)
	traj_det2_gsm_lo = mms_rot_gse2gsm(q0_data.tt2000_gd12, traj_det2_gse_lo)
	traj_det2_gsm_hi = mms_rot_gse2gsm(q0_data.tt2000_gd12, traj_det2_gse_hi)

;-----------------------------------------------------
; Spherical Coordinates \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Reduce data by one dimensions by converting to spherical coordinates
	;
	
	;DET1 GSE
	traj_det1_gse    = cv_coord(FROM_RECT=traj_det1_gse,    /TO_SPHERE, /DEGREES)
	traj_det1_gse_lo = cv_coord(FROM_RECT=traj_det1_gse_lo, /TO_SPHERE, /DEGREES)
	traj_det1_gse_hi = cv_coord(FROM_RECT=traj_det1_gse_hi, /TO_SPHERE, /DEGREES)
	
	;DET1 GSM
	traj_det2_gse    = cv_coord(FROM_RECT=traj_det2_gse,    /TO_SPHERE, /DEGREES)
	traj_det2_gse_lo = cv_coord(FROM_RECT=traj_det2_gse_lo, /TO_SPHERE, /DEGREES)
	traj_det2_gse_hi = cv_coord(FROM_RECT=traj_det2_gse_hi, /TO_SPHERE, /DEGREES)
	
	;DET1 GSE
	traj_det1_gsm    = cv_coord(FROM_RECT=traj_det1_gsm,    /TO_SPHERE, /DEGREES)
	traj_det1_gsm_lo = cv_coord(FROM_RECT=traj_det1_gsm_lo, /TO_SPHERE, /DEGREES)
	traj_det1_gsm_hi = cv_coord(FROM_RECT=traj_det1_gsm_hi, /TO_SPHERE, /DEGREES)
	
	;DET2 GSM
	traj_det2_gsm    = cv_coord(FROM_RECT=traj_det2_gsm,    /TO_SPHERE, /DEGREES)
	traj_det2_gsm_lo = cv_coord(FROM_RECT=traj_det2_gsm_lo, /TO_SPHERE, /DEGREES)
	traj_det2_gsm_hi = cv_coord(FROM_RECT=traj_det2_gsm_hi, /TO_SPHERE, /DEGREES)
	
	;
	;Convert from elevation to polar angle
	;
	
	;DET1 GSE
	traj_det1_gse[1,*]    = abs(traj_det1_gse[1,*] - 90.0)
	traj_det1_gse_lo[1,*] = abs(traj_det1_gse_lo[1,*] - 90.0)
	traj_det1_gse_hi[1,*] = abs(traj_det1_gse_hi[1,*] - 90.0)
	
	;DET1 gsm
	traj_det1_gsm[1,*]    = abs(traj_det1_gsm[1,*] - 90.0)
	traj_det1_gsm_lo[1,*] = abs(traj_det1_gsm_lo[1,*] - 90.0)
	traj_det1_gsm_hi[1,*] = abs(traj_det1_gsm_hi[1,*] - 90.0)
	
	;DET2 GSE
	traj_det2_gse[1,*]    = abs(traj_det2_gse[1,*] - 90.0)
	traj_det2_gse_lo[1,*] = abs(traj_det2_gse_lo[1,*] - 90.0)
	traj_det2_gse_hi[1,*] = abs(traj_det2_gse_hi[1,*] - 90.0)
	
	;DET2 gsm
	traj_det2_gsm[1,*]    = abs(traj_det2_gsm[1,*] - 90.0)
	traj_det2_gsm_lo[1,*] = abs(traj_det2_gsm_lo[1,*] - 90.0)
	traj_det2_gsm_hi[1,*] = abs(traj_det2_gsm_hi[1,*] - 90.0)

;-----------------------------------------------------
; Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Collect the data into a structure
	;   - Detector quantities: GD12 -> GDU2 and GD21 -> GDU1
	q0_out = { tt2000_gdu1:      q0_data.tt2000_gd21, $
	           tt2000_gdu2:      q0_data.tt2000_gd12, $
	           tt2000_timetag:   q0_data.tt2000_timetag, $
	           optics:           q0_data.optics, $
	           energy_gdu1:      q0_data.energy_gd21, $
	           energy_gdu2:      q0_data.energy_gd12, $
	           counts_gdu1:      q0_data.word15_gd21, $
	           counts_gdu2:      q0_data.word15_gd12, $
	           traj_gdu1_gse:    traj_det1_gse[0:1,*], $
	           traj_gdu1_gse_lo: traj_det1_gse_lo[0:1,*], $
	           traj_gdu1_gse_hi: traj_det1_gse_hi[0:1,*], $
	           traj_gdu1_gsm:    traj_det1_gsm[0:1,*], $
	           traj_gdu1_gsm_lo: traj_det1_gsm_lo[0:1,*], $
	           traj_gdu1_gsm_hi: traj_det1_gsm_hi[0:1,*], $
	           traj_gdu2_gse:    traj_det2_gse[0:1,*], $
	           traj_gdu2_gse_lo: traj_det2_gse_lo[0:1,*], $
	           traj_gdu2_gse_hi: traj_det2_gse_hi[0:1,*], $
	           traj_gdu2_gsm:    traj_det2_gsm[0:1,*], $
	           traj_gdu2_gsm_lo: traj_det2_gsm_lo[0:1,*], $
	           traj_gdu2_gsm_hi: traj_det2_gsm_hi[0:1,*] $
	         }

	;Done
	return, q0_out
end