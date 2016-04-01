; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_trajectories
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
;       2016/02/27  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Calculate trajectory vectors for electrons incident on EDI GDUs.
;
; :Params:
;       AZIMUTH:        in, required, type=fltarr
;                       Azimuthal look directions of GDU1 (degrees).
;       POLAR:          in, required, type=fltarr
;                       Polar look directions of GDU1 (degrees).
;       PACK_MODE:      in, required, type=integer
;                       Telemetry packing mode of the data.
;
; :Returns:
;       TRAJ_DATA:      A data structure with the following tags:
;                           TRAJ_0_GSE       -  Trajectory vector for 0-degree pitch angle electrons in GSE coordinates.
;                           TRAJ_0_GSE_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_GSE_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_0_GSM       -  Trajectory vector for 0-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_0_GSM_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_GSM_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE     -  Trajectory vector for 180-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_180_GSE_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE_HI  -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE     -  Trajectory vector for 180-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_180_GSE_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE_HI  -  Upper-bound for trajectory vector for electrons.
function brst_traj_compute, azimuth, polar, pack_mode
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Annode Operations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	; PACK_MODE 1
	;   channel    anode#      azimuth of look direction
	;   ---------------------------------------------------
	;   counts1      p-2       (p-1.5) * 11.25 deg
	;   counts2      p-1       (p-0.5) * 11.25 deg         } B points between
	;   counts3      p         (p+0.5) * 11.25 deg         } these two anodes
	;   counts4      p+1       (p+1.5) * 11.25 deg
	;
	; PACK_MODE 2
	;   channel    anode#      azimuth of look direction
	;   ---------------------------------------------------
	;   counts1      p         (p+0.5) * 11.25 deg
	;   counts2      p-1       (p-0.5) * 11.25 deg
	;   counts3      p-2       (p-1.5) * 11.25 deg
	;   counts4      p-3       (p-2.5) * 11.25 deg
	;
	
	;
	;GDU2 (Opposite to GDU1)
	; Anodes on GDU2 are in reverse order compared to anodes from GDU1
	; e.g., for packing mode 1, if B lies between anode 2 and 3
	;
	;                 GDU1                             GDU2
	;      channel  anode#   azimuth        channel  anode#    azimuth
	;      ----------------------------     ------------------------------
	;      counts1  p-2  [ 0.00, 11.25]     counts4  p+1  [168.75, 180.00]
	;      counts2  p-1  [11.25, 22.50]     counts3  p    [157.50, 168.75]
	;      counts3  p    [22.50, 33.75]     counts2  p-1  [146.25, 157.50]
	;      counts4  p+1  [33.75, 45.00]     counts1  p-2  [135.00, 146.25]
	;
	; This means that we have to interchange hi & lo and also count
	; down while GDU1 counts up. Doing so, the pitch angle data will
	; be ordered as
	;
	;        pa[*,0:3] = GDU1 counts1-4
	;        pa[*,4:7] = GDU2 counts1-4
	;
	
	;Polar and azimuth angle resolution
	dtheta = 360.0 / 512.0
	dphi   = 360.0 / 32.0

	;Lower-bound of polar and azimuth look directions
	theta = findgen(129) * dtheta
	phi   = findgen(32)  * dphi

	;Offset of each pad in anode units
	case pack_mode of
		1: offset = [-2, -1,  0,  1]
		2: offset = [ 0, -1, -2, -3]
		else: message, 'Invalid packing mode (' + string(pack_mode, FORMAT='(i0)', /PRINT) + ').'
	endcase
	npads = n_elements(offset)
	
;-----------------------------------------------------
; Azimuth of Edges & Center of Anodes \\\\\\\\\\\\\\\\
;-----------------------------------------------------

	npts = n_elements(azimuth)
	
	;Angle of the center and lower- and upper-edges of each pad
	pa    = fltarr(npts, npads)
	pa_lo = fltarr(npts, npads)
	pa_hi = fltarr(npts, npads)
	for i = 0, npads-1 do begin
		;Azimuth of upper and lower edges
		pa_lo[0,i] = azimuth +  offset[i]        * dphi
		pa[0,i]    = azimuth + (offset[i] + 0.5) * dphi
		pa_hi[0,i] = azimuth + (offset[i] + 1.0) * dphi

		;Force to range [0,360)
		if offset[i] lt 0 then begin
			pa[*,i]    += (pa[*,i]    lt 0) * 360.0
			pa_lo[*,i] += (pa_lo[*,i] lt 0) * 360.0
			pa_hi[*,i] += (pa_hi[*,i] lt 0) * 360.0
		endif else begin
			pa[0,i]     =  pa[*,i]    mod 360.0
			pa_lo[0,i]  =  pa_lo[*,i] mod 360.0
			pa_hi[0,i]  =  pa_hi[*,i] mod 360.0
		endelse
	endfor

;-----------------------------------------------------
; GDU1 Trajectories \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert to radians
	deg2rad  = !dpi / 180.0D
	pa      *= deg2rad
	pa_lo   *= deg2rad
	pa_hi   *= deg2rad
	azimuth *= deg2rad
	polar    = rebin(polar*deg2rad, npts, npads)
	
	;Allocate memory
	traj_gdu1    = fltarr(npts, npads, 3)
	traj_gdu1_lo = fltarr(npts, npads, 3)
	traj_gdu1_hi = fltarr(npts, npads, 3)

	;Center
	traj_gdu1[0,0,0] = -sin(polar) * cos(pa)
	traj_gdu1[0,0,1] = -sin(polar) * sin(pa)
	traj_gdu1[0,0,2] = -cos(polar)

	;Lower
	traj_gdu1_lo[0,0,0] = -sin(polar) * cos(pa_lo)
	traj_gdu1_lo[0,0,1] = -sin(polar) * sin(pa_lo)
	traj_gdu1_lo[0,0,2] = -cos(polar)

	;Upper
	traj_gdu1_hi[0,0,0] = -sin(polar) * cos(pa_hi)
	traj_gdu1_hi[0,0,1] = -sin(polar) * sin(pa_hi)
	traj_gdu1_hi[0,0,2] = -cos(polar)

	;Transpose to put components first
	traj_gdu1    = transpose( temporary(traj_gdu1),    [2,0,1])
	traj_gdu1_lo = transpose( temporary(traj_gdu1_lo), [2,0,1])
	traj_gdu1_hi = transpose( temporary(traj_gdu1_hi), [2,0,1])
	
;-----------------------------------------------------
; GDU2 Trajectories \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;
	;GDU2 (Opposite to GDU1)
	; Anodes on GDU2 are in reverse order compared to anodes from GDU1
	; e.g., for packing mode 1, if B lies between anode 2 and 3
	;
	;                 GDU1                             GDU2
	;      channel  anode#   azimuth        channel  anode#    azimuth
	;      ----------------------------     ------------------------------
	;      counts1  p-2  [ 0.00, 11.25]     counts4  p+1  [168.75, 180.00]
	;      counts2  p-1  [11.25, 22.50]     counts3  p    [157.50, 168.75]
	;      counts3  p    [22.50, 33.75]     counts2  p-1  [146.25, 157.50]
	;      counts4  p+1  [33.75, 45.00]     counts1  p-2  [135.00, 146.25]
	;
	; This means that we have to interchange hi & lo and also count
	; down while GDU1 counts up. Doing so, the pitch angle data will
	; be ordered as
	;
	
	;Allocate memory
	traj_gdu2    = fltarr(3, npts, npads)
	traj_gdu2_lo = fltarr(3, npts, npads)
	traj_gdu2_hi = fltarr(3, npts, npads)
	
	;Create the trajectory vectors for GDU2
	for i = 0, npads-1 do begin
		ipad                   = npads - 1 - i
		traj_gdu2[0,0,ipad]    = -traj_gdu1[*,*,i]
		traj_gdu2_lo[0,0,ipad] = -traj_gdu1_hi[*,*,i]
		traj_gdu2_hi[0,0,ipad] = -traj_gdu1_lo[*,*,i]
	endfor
	
;-----------------------------------------------------
; Return Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	traj_data = { traj_gdu1:    temporary(traj_gdu1), $
	              traj_gdu1_lo: temporary(traj_gdu1_lo), $
	              traj_gdu1_hi: temporary(traj_gdu1_hi), $
	              traj_gdu2:    temporary(traj_gdu2), $
	              traj_gdu2_lo: temporary(traj_gdu2_lo), $
	              traj_gdu2_hi: temporary(traj_gdu2_hi) $
	            }
	
	return, traj_data
end


;+
;   Despin and rotate electron trajectory vectors into GSE and GSM coordinates.
;
; :Params:
;       TRAJ:           in, required, type=structure
;                       Structure with the following fields:
;                           TRAJ_0       -  Trajectory vector for 0-degree pitch angle electrons into GDU1 in EDI coordinates.
;                           TRAJ_0_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180     -  Trajectory vector for 180-degree pitch angle electrons in EDI coordinates.
;                           TRAJ_180_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_HI  -  Upper-bound for trajectory vector for electrons.
;       DSS_FILE:       in, required, type=string
;                       File name of the digital sunsensor file from which sunpulse
;                           information is read.
;       DEFATT_FILE:    in, required, type=string
;                       Name of the definitive attitude data file from which RA and DEC
;                           are read.
;
; :Returns:
;       OUT:             A data structure with the following tags:
;                           TRAJ_0_GSE       -  Trajectory vector for 0-degree pitch angle electrons in GSE coordinates.
;                           TRAJ_0_GSE_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_GSE_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_0_GSM       -  Trajectory vector for 0-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_0_GSM_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_GSM_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE     -  Trajectory vector for 180-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_180_GSE_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE_HI  -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE     -  Trajectory vector for 180-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_180_GSE_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE_HI  -  Upper-bound for trajectory vector for electrons.
;-
function brst_traj_rotate, traj, dss_file, defatt_file
	compile_opt idl2
	on_error, 2
	
	dims_0   = size(traj.traj_0,   /DIMENSIONS)
	dims_180 = size(traj.traj_180, /DIMENSIONS)
	dims_0   = [2, dims_0[1:-1]]
	dims_180 = [2, dims_180[1:-1]]
	
	;Read data
	dss    = mms_dss_read_sunpulse(dss_file)
	defatt = mms_fdoa_read_defatt(defatt_file)
	
	;EDI --> BCS
	edi1_to_bcs = mms_instr_xxyz2ocs('EDI1')
	edi2_to_bcs = mms_instr_xxyz2ocs('EDI2')
	
	;Despin matrices
	bcs2dbcs_0   = mms_dss_xdespin(dss, traj.t_0)
	bcs2dbcs_180 = mms_dss_xdespin(dss, traj.t_180)

	;Output structure
	;   - Spherical coordinates [azimuth, polar]
	out = { traj_0_gse:       fltarr(dims_0), $
	        traj_0_gse_lo:    fltarr(dims_0), $
	        traj_0_gse_hi:    fltarr(dims_0), $
	        traj_0_gsm:       fltarr(dims_0), $
	        traj_0_gsm_lo:    fltarr(dims_0), $
	        traj_0_gsm_hi:    fltarr(dims_0), $
	        traj_180_gse:     fltarr(dims_180), $
	        traj_180_gse_lo:  fltarr(dims_180), $
	        traj_180_gse_hi:  fltarr(dims_180), $
	        traj_180_gsm:     fltarr(dims_180), $
	        traj_180_gsm_lo:  fltarr(dims_180), $
	        traj_180_gsm_hi:  fltarr(dims_180) $
	      }

	;Step through each
	for i = 0, 3 do begin
	
	;-----------------------------------------------------
	; EDI --> BCS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;0-PA
		traj_0_bcs    = MrVector_Rotate(edi1_to_bcs, traj.traj_0[*,*,i])
		traj_0_bcs_lo = MrVector_Rotate(edi1_to_bcs, traj.traj_0_lo[*,*,i])
		traj_0_bcs_hi = MrVector_Rotate(edi1_to_bcs, traj.traj_0_hi[*,*,i])
		
		;180-PA
		traj_180_bcs    = MrVector_Rotate(edi1_to_bcs, traj.traj_180[*,*,i])
		traj_180_bcs_lo = MrVector_Rotate(edi1_to_bcs, traj.traj_180_lo[*,*,i])
		traj_180_bcs_hi = MrVector_Rotate(edi1_to_bcs, traj.traj_180_hi[*,*,i])

	;-----------------------------------------------------
	; BCS --> DBCS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;0-PA
		traj_0_dbcs    = MrVector_Rotate(bcs2dbcs_0, temporary(traj_0_bcs) )
		traj_0_dbcs_lo = MrVector_Rotate(bcs2dbcs_0, temporary(traj_0_bcs_lo) )
		traj_0_dbcs_hi = MrVector_Rotate(bcs2dbcs_0, temporary(traj_0_bcs_hi) )
		
		;180-PA
		traj_180_dbcs    = MrVector_Rotate(bcs2dbcs_180, temporary(traj_180_bcs) )
		traj_180_dbcs_lo = MrVector_Rotate(bcs2dbcs_180, temporary(traj_180_bcs_lo) )
		traj_180_dbcs_hi = MrVector_Rotate(bcs2dbcs_180, temporary(traj_180_bcs_hi) )

	;-----------------------------------------------------
	; DBCS --> GSE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;0-PA
		traj_0_gse    = mms_rot_despun2gse(defatt, traj.t_0, temporary(traj_0_dbcs),    TYPE='Z')
		traj_0_gse_lo = mms_rot_despun2gse(defatt, traj.t_0, temporary(traj_0_dbcs_lo), TYPE='Z')
		traj_0_gse_hi = mms_rot_despun2gse(defatt, traj.t_0, temporary(traj_0_dbcs_hi), TYPE='Z')
		
		;180-PA
		traj_180_gse    = mms_rot_despun2gse(defatt, traj.t_180, temporary(traj_180_dbcs),    TYPE='Z')
		traj_180_gse_lo = mms_rot_despun2gse(defatt, traj.t_180, temporary(traj_180_dbcs_lo), TYPE='Z')
		traj_180_gse_hi = mms_rot_despun2gse(defatt, traj.t_180, temporary(traj_180_dbcs_hi), TYPE='Z')

	;-----------------------------------------------------
	; GSE --> GSM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;0-PA
		traj_0_gsm    = mms_rot_gse2gsm(traj.t_0, traj_0_gse)
		traj_0_gsm_lo = mms_rot_gse2gsm(traj.t_0, traj_0_gse_lo)
		traj_0_gsm_hi = mms_rot_gse2gsm(traj.t_0, traj_0_gse_hi)
		
		;180-PA
		traj_180_gsm    = mms_rot_gse2gsm(traj.t_180, traj_180_gse)
		traj_180_gsm_lo = mms_rot_gse2gsm(traj.t_180, traj_180_gse_lo)
		traj_180_gsm_hi = mms_rot_gse2gsm(traj.t_180, traj_180_gse_hi)

	;-----------------------------------------------------
	; CARTESIAN --> SPHERICAL \\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;
		; TODO: Renormalize unit vectors in case of rounding errors?
		;
		
		;0-PA
		traj_0_gse     = cv_coord(FROM_RECT=traj_0_gse,     /TO_SPHERE, /DEGREES)
		traj_0_gse_lo  = cv_coord(FROM_RECT=traj_0_gse_lo,  /TO_SPHERE, /DEGREES)
		traj_0_gse_hi  = cv_coord(FROM_RECT=traj_0_gse_hi,  /TO_SPHERE, /DEGREES)
		traj_0_gsm     = cv_coord(FROM_RECT=traj_0_gsm,     /TO_SPHERE, /DEGREES)
		traj_0_gsm_lo  = cv_coord(FROM_RECT=traj_0_gsm_lo,  /TO_SPHERE, /DEGREES)
		traj_0_gsm_hi  = cv_coord(FROM_RECT=traj_0_gsm_hi,  /TO_SPHERE, /DEGREES)
		
		;180-PA
		traj_180_gse     = cv_coord(FROM_RECT=traj_180_gse,     /TO_SPHERE, /DEGREES)
		traj_180_gse_lo  = cv_coord(FROM_RECT=traj_180_gse_lo,  /TO_SPHERE, /DEGREES)
		traj_180_gse_hi  = cv_coord(FROM_RECT=traj_180_gse_hi,  /TO_SPHERE, /DEGREES)
		traj_180_gsm     = cv_coord(FROM_RECT=traj_180_gsm,     /TO_SPHERE, /DEGREES)
		traj_180_gsm_lo  = cv_coord(FROM_RECT=traj_180_gsm_lo,  /TO_SPHERE, /DEGREES)
		traj_180_gsm_hi  = cv_coord(FROM_RECT=traj_180_gsm_hi,  /TO_SPHERE, /DEGREES)
		
		;
		; Change from elevation to polar angle
		;
		
		;0-PA
		traj_0_gse[1,*]     = abs(traj_0_gse[1,*]     - 90.0)
		traj_0_gse_lo[1,*]  = abs(traj_0_gse_lo[1,*]  - 90.0)
		traj_0_gse_hi[1,*]  = abs(traj_0_gse_hi[1,*]  - 90.0)
		traj_0_gsm[1,*]     = abs(traj_0_gsm[1,*]     - 90.0)
		traj_0_gsm_lo[1,*]  = abs(traj_0_gsm_lo[1,*]  - 90.0)
		traj_0_gsm_hi[1,*]  = abs(traj_0_gsm_hi[1,*]  - 90.0)
		
		;180-PA
		traj_180_gse[1,*]     = abs(traj_180_gse[1,*]     - 90.0)
		traj_180_gse_lo[1,*]  = abs(traj_180_gse_lo[1,*]  - 90.0)
		traj_180_gse_hi[1,*]  = abs(traj_180_gse_hi[1,*]  - 90.0)
		traj_180_gsm[1,*]     = abs(traj_180_gsm[1,*]     - 90.0)
		traj_180_gsm_lo[1,*]  = abs(traj_180_gsm_lo[1,*]  - 90.0)
		traj_180_gsm_hi[1,*]  = abs(traj_180_gsm_hi[1,*]  - 90.0)

	;-----------------------------------------------------
	; Deltas \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;0-PA
		traj_0_gse_lo = traj_0_gse - traj_0_gse_lo
		traj_0_gse_hi = traj_0_gse_hi - traj_0_gse
		traj_0_gsm_lo = traj_0_gsm - traj_0_gsm_lo
		traj_0_gsm_hi = traj_0_gsm_hi - traj_0_gsm
	
		;180-PA
		traj_180_gse_lo = traj_180_gse - traj_180_gse_lo
		traj_180_gse_hi = traj_180_gse_hi - traj_180_gse
		traj_180_gsm_lo = traj_180_gsm - traj_180_gsm_lo
		traj_180_gsm_hi = traj_180_gsm_hi - traj_180_gsm

	;-----------------------------------------------------
	; Store in Output Structure \\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;0-PA
		out.traj_0_gse[*,*,i]     = traj_0_gse[0:1,*]
		out.traj_0_gse_lo[*,*,i]  = traj_0_gse_lo[0:1,*]
		out.traj_0_gse_hi[*,*,i]  = traj_0_gse_hi[0:1,*]
		out.traj_0_gsm[*,*,i]     = traj_0_gsm[0:1,*]
		out.traj_0_gsm_lo[*,*,i]  = traj_0_gsm_lo[0:1,*]
		out.traj_0_gsm_hi[*,*,i]  = traj_0_gsm_hi[0:1,*]
		
		;180-PA
		out.traj_180_gse[*,*,i]     = traj_180_gse[0:1,*]
		out.traj_180_gse_lo[*,*,i]  = traj_180_gse_lo[0:1,*]
		out.traj_180_gse_hi[*,*,i]  = traj_180_gse_hi[0:1,*]
		out.traj_180_gsm[*,*,i]     = traj_180_gsm[0:1,*]
		out.traj_180_gsm_lo[*,*,i]  = traj_180_gsm_lo[0:1,*]
		out.traj_180_gsm_hi[*,*,i]  = traj_180_gsm_hi[0:1,*]
	endfor
	
;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	return, out
end


;+
;   Sort trajectories by their reported pitch angle.
;
; :Params:
;       T_GDU1:         in, required, type=long64arr (cdf_time_tt2000)
;                       Time tags for GDU1 data.
;       T_GDU2:         in, required, type=long64arr (cdf_time_tt2000)
;                       Time tags for GDU2 data.
;       PITCH_GDU1:     in, required, type=long64arr (cdf_time_tt2000)
;                       Pitch angle flag computed on board the spacecraft for GDU1.
;       PITCH_GDU2:     in, required, type=long64arr (cdf_time_tt2000)
;                       Pitch angle flag computed on board the spacecraft for GDU2.
;       DATA:           in, required, type=long64arr (cdf_time_tt2000)
;                       A structure of trajectory vectors with the following fields:
;                           TRAJ_GDU1     -  Trajectory vector for electrons into GDU1 in EDI coordinates.
;                           TRAJ_GDU1_LO  -  Lower-bound for trajectory vector for electrons into GDU1.
;                           TRAJ_GDU1_HI  -  Upper-bound for trajectory vector for electrons into GDU1.
;                           TRAJ_GDU2     -  Trajectory vector for electrons into GDU2 in EDI coordinates.
;                           TRAJ_GDU2_LO  -  Lower-bound for trajectory vector for electrons into GDU2.
;                           TRAJ_GDU2_HI  -  Upper-bound for trajectory vector for electrons into GDU2.
;
; :Returns:
;       OUT:             A data structure with the following tags:
;                           TRAJ_0       -  Trajectory vector for 0-degree pitch angle electrons in EDI coordinates.
;                           TRAJ_0_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180     -  Trajectory vector for 180-degree pitch angle electrons in EDI coordinates.
;                           TRAJ_180_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_HI  -  Upper-bound for trajectory vector for electrons.
;-
function brst_traj_sort, t_gdu1, t_gdu2, pitch_gdu1, pitch_gdu2, data
	compile_opt idl2
	on_error, 2
	
;-----------------------------------------------------
; Sort By Pitch Angle 0 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Find 0s
	i0_gdu1 = where(pitch_gdu1 eq 0, n0_gdu1)
	i0_gdu2 = where(pitch_gdu2 eq 0, n0_gdu2)

	;Both detectors have data
	if n0_gdu1 gt 0 && n0_gdu2 gt 0 then begin
		t_0       = [ t_gdu1[i0_gdu1], t_gdu2[i0_gdu2] ]
		traj_0    = [ [ data.traj_gdu1[*,i0_gdu1,*]    ], [ data.traj_gdu2[*,i0_gdu2,*] ] ]
		traj_0_lo = [ [ data.traj_gdu1_lo[*,i0_gdu1,*] ], [ data.traj_gdu2[*,i0_gdu2,*] ] ]
		traj_0_hi = [ [ data.traj_gdu1_hi[*,i0_gdu1,*] ], [ data.traj_gdu2[*,i0_gdu2,*] ] ]

		;Sort times
		isort     = sort(t_0)
		t_0       = t_0[isort]
		traj_0    = traj_0[*,isort,*]
		traj_0_lo = traj_0_lo[*,isort,*]
		traj_0_hi = traj_0_hi[*,isort,*]
		
		;Mark GDU
		;   - GDU1 and GDU2 are complements of one another
		;   - When GDU1 is in PA0 mode, GDU2 will be in PA180 mode.
		;   - Therefore, we can use the original indices
		;   - It is the same as storing 1B then 2B and soring with ISORT
		gdu_0 = bytarr(n0_gdu1 + n0_gdu2)
		gdu_0[i0_gdu1] = 1B
		gdu_0[i0_gdu2] = 2B
	
	;GDU1 has data
	endif else if n0_gdu1 gt 0 then begin
		t_0       = t_gdu1[i0_gdu1]
		traj_0    = data.traj_gdu1[*,i0_gdu1,*]
		traj_0_lo = data.traj_gdu1_lo[*,i0_gdu1,*]
		traj_0_hi = data.traj_gdu1_hi[*,i0_gdu1,*]
		gdu_0     = replicate(1B, n0_gdu1)
	
	;GDU2 has data
	endif else if n0_gdu2 gt 0 then begin
		t_0       = t_gdu2[i0_gdu2]
		traj_0    = data.traj_gdu2[*,i0_gdu2,*]
		traj_0_lo = data.traj_gdu2_lo[*,i0_gdu2,*]
		traj_0_hi = data.traj_gdu2_hi[*,i0_gdu2,*]
		gdu_0     = replicate(2B, n0_gdu2)
	endif else begin
		MrPrintF, 'LogText', 'No 0 degree pitch angle data.'
		t_0      = 0LL
		counts_0 = -1S
	endelse
	
;-----------------------------------------------------
; Sort By Pitch Angle 180 \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Find 18180s
	i180_gdu1 = where(pitch_gdu1 eq 180, n180_gdu1)
	i180_gdu2 = where(pitch_gdu2 eq 180, n180_gdu2)

	;Both detectors have data
	if n180_gdu1 gt 0 && n180_gdu2 gt 0 then begin
		t_180       = [ t_gdu1[i180_gdu1], t_gdu2[i180_gdu2] ]
		traj_180    = [ [ data.traj_gdu1[*,i180_gdu1,*]    ], [ data.traj_gdu2[*,i180_gdu2,*] ] ]
		traj_180_lo = [ [ data.traj_gdu1_lo[*,i180_gdu1,*] ], [ data.traj_gdu2[*,i180_gdu2,*] ] ]
		traj_180_hi = [ [ data.traj_gdu1_hi[*,i180_gdu1,*] ], [ data.traj_gdu2[*,i180_gdu2,*] ] ]

		;Sort times
		isort       = sort(t_180)
		t_180       = t_180[isort]
		traj_180    = traj_180[*,isort,*]
		traj_180_lo = traj_180_lo[*,isort,*]
		traj_180_hi = traj_180_hi[*,isort,*]
		
		;Mark GDU
		;   - GDU1 and GDU2 are complements of one another
		;   - When GDU1 is in PA0 mode, GDU2 will be in PA180 mode.
		;   - Therefore, we can use the original indices
		;   - It is the same as storing 1B then 2B and soring with ISORT
		gdu_180 = bytarr(n180_gdu1 + n180_gdu2)
		gdu_180[i180_gdu1] = 1B
		gdu_180[i180_gdu2] = 2B
	
	;GDU1 has data
	endif else if n180_gdu1 gt 0 then begin
		t_180       = t_gdu1[i180_gdu1]
		traj_180    = data.traj_gdu1[*,i180_gdu1,*]
		traj_180_lo = data.traj_gdu1_lo[*,i180_gdu1,*]
		traj_180_hi = data.traj_gdu1_hi[*,i180_gdu1,*]
		gdu_180     = replicate(1B, n180_gdu1)
	
	;GDU2 has data
	endif else if n180_gdu2 gt 0 then begin
		t_180       = t_gdu2[i180_gdu2]
		traj_180    = data.traj_gdu2[*,i180_gdu2,*]
		traj_180_lo = data.traj_gdu2_lo[*,i180_gdu2,*]
		traj_180_hi = data.traj_gdu2_hi[*,i180_gdu2,*]
		gdu_180     = replicate(2B, n180_gdu2)
	endif else begin
		MrPrintF, 'LogText', 'No 180 degree pitch angle data.'
		t_180      = 180LL
		counts_180 = -1S
	endelse
	
;-----------------------------------------------------
; Out \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	out = { t_0:         temporary(t_0), $
	        t_180:       temporary(t_180), $
	        gdu_0:       temporary(gdu_0), $
	        gdu_180:     temporary(gdu_180), $
	        traj_0:      temporary(traj_0), $
	        traj_0_lo:   temporary(traj_0_lo), $
	        traj_0_hi:   temporary(traj_0_hi), $
	        traj_180:    temporary(traj_180), $
	        traj_180_lo: temporary(traj_180_lo), $
	        traj_180_hi: temporary(traj_180_hi) $
	      }

	return, out
end


;+
;   Calculate trajectory vectors for electrons incident on EDI GDUs.
;
; :Params:
;       AZIMUTH:        in, required, type=fltarr
;                       Azimuthal look directions of GDU1 (degrees).
;       POLAR:          in, required, type=fltarr
;                       Polar look directions of GDU1 (degrees).
;       PACK_MODE:      in, required, type=integer
;                       Telemetry packing mode of the data.
;
; :Returns:
;       TRAJ_DATA:      A data structure with the following tags:
;                           TRAJ_0_GSE       -  Trajectory vector for 0-degree pitch angle electrons in GSE coordinates.
;                           TRAJ_0_GSE_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_GSE_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_0_GSM       -  Trajectory vector for 0-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_0_GSM_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_GSM_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE     -  Trajectory vector for 180-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_180_GSE_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE_HI  -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE     -  Trajectory vector for 180-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_180_GSE_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE_HI  -  Upper-bound for trajectory vector for electrons.
function srvy_traj_compute, azimuth, polar, pack_mode
	compile_opt idl2
	on_error, 2
	

;-----------------------------------------------------
; Annode Operations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	; PACK_MODE 1
	;   channel    anode#      azimuth of look direction
	;   ---------------------------------------------------
	;   counts1      p-2       (p-1.5) * 11.25 deg
	;   counts2      p-1       (p-0.5) * 11.25 deg         } B points between
	;   counts3      p         (p+0.5) * 11.25 deg         } these two anodes
	;   counts4      p+1       (p+1.5) * 11.25 deg
	;
	; PACK_MODE 2
	;   channel    anode#      azimuth of look direction
	;   ---------------------------------------------------
	;   counts1      p         (p+0.5) * 11.25 deg
	;   counts2      p-1       (p-0.5) * 11.25 deg
	;   counts3      p-2       (p-1.5) * 11.25 deg
	;   counts4      p-3       (p-2.5) * 11.25 deg
	;
	
	;
	;GDU2 (Opposite to GDU1)
	; Anodes on GDU2 are in reverse order compared to anodes from GDU1
	; e.g., for packing mode 1, if B lies between anode 2 and 3
	;
	;                 GDU1                             GDU2
	;      channel  anode#   azimuth        channel  anode#    azimuth
	;      ----------------------------     ------------------------------
	;      counts1  p-2  [ 0.00, 11.25]     counts4  p+1  [168.75, 180.00]
	;      counts2  p-1  [11.25, 22.50]     counts3  p    [157.50, 168.75]
	;      counts3  p    [22.50, 33.75]     counts2  p-1  [146.25, 157.50]
	;      counts4  p+1  [33.75, 45.00]     counts1  p-2  [135.00, 146.25]
	;
	; This means that we have to interchange hi & lo and also count
	; down while GDU1 counts up. Doing so, the pitch angle data will
	; be ordered as
	;
	;        pa[*,0:3] = GDU1 counts1-4
	;        pa[*,4:7] = GDU2 counts1-4
	;
	
	;Polar and azimuth angle resolution
	dtheta = 360.0 / 512.0
	dphi   = 360.0 / 32.0

	;Lower-bound of polar and azimuth look directions
	theta = findgen(129) * dtheta
	phi   = findgen(32)  * dphi
	
	;Offset of each pad in anode units
	;   [low, center, high]
	case pack_mode of
		0: offset = [-1.0, 0.0, 1.0]  ;Identical to PACK_MODE 1
		1: offset = [-1.0, 0.0, 1.0]
		2: offset = [ 0.0, 0.5, 1.0]
		else: message, 'Invalid packing mode (' + string(pack_mode, FORMAT='(i0)', /PRINT) + ').'
	endcase
	npads = n_elements(offset)
	
;-----------------------------------------------------
; Azimuth of Edges & Center of Anodes \\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Angle of the lower edge of each pad
	pa_lo = azimuth + offset[0] * 11.25
	pa    = azimuth + offset[1] * 11.25
	pa_hi = azimuth + offset[2] * 11.25
	
;-----------------------------------------------------
; GDU1 Trajectories \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of points
	npts = n_elements(azimuth)

	;Convert to radians
	deg2rad  = !dpi / 180.0D
	pa      *= deg2rad
	pa_lo   *= deg2rad
	pa_hi   *= deg2rad
	azimuth *= deg2rad
	polar   *= deg2rad
	
	;Allocate memory
	traj_gdu1    = fltarr(npts, 3)
	traj_gdu1_lo = fltarr(npts, 3)
	traj_gdu1_hi = fltarr(npts, 3)

	;Center
	traj_gdu1[0,0] = -sin(polar) * cos(pa)
	traj_gdu1[0,1] = -sin(polar) * sin(pa)
	traj_gdu1[0,2] = -cos(polar)

	;Lower
	traj_gdu1_lo[0,0] = -sin(polar) * cos(pa_lo)
	traj_gdu1_lo[0,1] = -sin(polar) * sin(pa_lo)
	traj_gdu1_lo[0,2] = -cos(polar)

	;Upper
	traj_gdu1_hi[0,0] = -sin(polar) * cos(pa_hi)
	traj_gdu1_hi[0,1] = -sin(polar) * sin(pa_hi)
	traj_gdu1_hi[0,2] = -cos(polar)

	;Transpose to put components first
	traj_gdu1    = transpose( temporary(traj_gdu1)    )
	traj_gdu1_lo = transpose( temporary(traj_gdu1_lo) )
	traj_gdu1_hi = transpose( temporary(traj_gdu1_hi) )
	
;-----------------------------------------------------
; GDU2 Trajectories \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;
	;GDU2 (Opposite to GDU1)
	; Anodes on GDU2 are in reverse order compared to anodes from GDU1
	; e.g., for packing mode 1, if B lies between anode 2 and 3
	;
	;                 GDU1                             GDU2
	;      channel  anode#   azimuth        channel  anode#    azimuth
	;      ----------------------------     ------------------------------
	;      counts1  p-2  [ 0.00, 11.25]     counts4  p+1  [168.75, 180.00]
	;      counts2  p-1  [11.25, 22.50]     counts3  p    [157.50, 168.75]
	;      counts3  p    [22.50, 33.75]     counts2  p-1  [146.25, 157.50]
	;      counts4  p+1  [33.75, 45.00]     counts1  p-2  [135.00, 146.25]
	;
	; This means that we have to interchange hi & lo and also count
	; down while GDU1 counts up. Doing so, the pitch angle data will
	; be ordered as
	;
	
	;Create the trajectory vectors for GDU2
	traj_gdu2    = -traj_gdu1
	traj_gdu2_lo = -traj_gdu1_hi
	traj_gdu2_hi = -traj_gdu1_lo
	
;-----------------------------------------------------
; Return Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	traj_data = { traj_gdu1:    temporary(traj_gdu1), $
	              traj_gdu1_lo: temporary(traj_gdu1_lo), $
	              traj_gdu1_hi: temporary(traj_gdu1_hi), $
	              traj_gdu2:    temporary(traj_gdu2), $
	              traj_gdu2_lo: temporary(traj_gdu2_lo), $
	              traj_gdu2_hi: temporary(traj_gdu2_hi) $
	            }
	
	return, traj_data
end


;+
;   Despin and rotate electron trajectory vectors into GSE and GSM coordinates.
;
; :Params:
;       TRAJ:           in, required, type=structure
;                       Structure with the following fields:
;                           TRAJ_0       -  Trajectory vector for 0-degree pitch angle electrons into GDU1 in EDI coordinates.
;                           TRAJ_0_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180     -  Trajectory vector for 180-degree pitch angle electrons in EDI coordinates.
;                           TRAJ_180_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_HI  -  Upper-bound for trajectory vector for electrons.
;       DSS_FILE:       in, required, type=string
;                       File name of the digital sunsensor file from which sunpulse
;                           information is read.
;       DEFATT_FILE:    in, required, type=string
;                       Name of the definitive attitude data file from which RA and DEC
;                           are read.
;
; :Returns:
;       OUT:             A data structure with the following tags:
;                           TRAJ_0_GSE       -  Trajectory vector for 0-degree pitch angle electrons in GSE coordinates.
;                           TRAJ_0_GSE_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_GSE_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_0_GSM       -  Trajectory vector for 0-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_0_GSM_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_GSM_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE     -  Trajectory vector for 180-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_180_GSE_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE_HI  -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE     -  Trajectory vector for 180-degree pitch angle electrons in GSM coordinates.
;                           TRAJ_180_GSE_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_GSE_HI  -  Upper-bound for trajectory vector for electrons.
;-
function srvy_traj_rotate, traj, dss_file, defatt_file
	compile_opt idl2
	on_error, 2
	
	dims_0   = size(traj.traj_0,   /DIMENSIONS)
	dims_180 = size(traj.traj_180, /DIMENSIONS)
	dims_0   = [2, dims_0[1:-1]]
	dims_180 = [2, dims_180[1:-1]]
	
	;Read data
	dss    = mms_dss_read_sunpulse(dss_file)
	defatt = mms_fdoa_read_defatt(defatt_file)
	
	;EDI --> BCS
	edi1_to_bcs = mms_instr_xxyz2ocs('EDI1')
	edi2_to_bcs = mms_instr_xxyz2ocs('EDI2')
	
	;Despin matrices
	bcs2dbcs_0   = mms_dss_xdespin(dss, traj.t_0)
	bcs2dbcs_180 = mms_dss_xdespin(dss, traj.t_180)

	;Output structure
	;   - Spherical coordinates [azimuth, polar]
	out = { traj_0_gse:       fltarr(dims_0), $
	        traj_0_gse_lo:    fltarr(dims_0), $
	        traj_0_gse_hi:    fltarr(dims_0), $
	        traj_0_gsm:       fltarr(dims_0), $
	        traj_0_gsm_lo:    fltarr(dims_0), $
	        traj_0_gsm_hi:    fltarr(dims_0), $
	        traj_180_gse:     fltarr(dims_180), $
	        traj_180_gse_lo:  fltarr(dims_180), $
	        traj_180_gse_hi:  fltarr(dims_180), $
	        traj_180_gsm:     fltarr(dims_180), $
	        traj_180_gsm_lo:  fltarr(dims_180), $
	        traj_180_gsm_hi:  fltarr(dims_180) $
	      }

;-----------------------------------------------------
; EDI --> BCS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;0-PA
	traj_0_bcs    = MrVector_Rotate(edi1_to_bcs, traj.traj_0)
	traj_0_bcs_lo = MrVector_Rotate(edi1_to_bcs, traj.traj_0_lo)
	traj_0_bcs_hi = MrVector_Rotate(edi1_to_bcs, traj.traj_0_hi)
	
	;180-PA
	traj_180_bcs    = MrVector_Rotate(edi1_to_bcs, traj.traj_180)
	traj_180_bcs_lo = MrVector_Rotate(edi1_to_bcs, traj.traj_180_lo)
	traj_180_bcs_hi = MrVector_Rotate(edi1_to_bcs, traj.traj_180_hi)

;-----------------------------------------------------
; BCS --> DBCS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;0-PA
	traj_0_dbcs    = MrVector_Rotate(bcs2dbcs_0, temporary(traj_0_bcs) )
	traj_0_dbcs_lo = MrVector_Rotate(bcs2dbcs_0, temporary(traj_0_bcs_lo) )
	traj_0_dbcs_hi = MrVector_Rotate(bcs2dbcs_0, temporary(traj_0_bcs_hi) )
	
	;180-PA
	traj_180_dbcs    = MrVector_Rotate(bcs2dbcs_180, temporary(traj_180_bcs) )
	traj_180_dbcs_lo = MrVector_Rotate(bcs2dbcs_180, temporary(traj_180_bcs_lo) )
	traj_180_dbcs_hi = MrVector_Rotate(bcs2dbcs_180, temporary(traj_180_bcs_hi) )

;-----------------------------------------------------
; DBCS --> GSE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;0-PA
	traj_0_gse    = mms_rot_despun2gse(defatt, traj.t_0, temporary(traj_0_dbcs),    TYPE='Z')
	traj_0_gse_lo = mms_rot_despun2gse(defatt, traj.t_0, temporary(traj_0_dbcs_lo), TYPE='Z')
	traj_0_gse_hi = mms_rot_despun2gse(defatt, traj.t_0, temporary(traj_0_dbcs_hi), TYPE='Z')
	
	;180-PA
	traj_180_gse    = mms_rot_despun2gse(defatt, traj.t_180, temporary(traj_180_dbcs),    TYPE='Z')
	traj_180_gse_lo = mms_rot_despun2gse(defatt, traj.t_180, temporary(traj_180_dbcs_lo), TYPE='Z')
	traj_180_gse_hi = mms_rot_despun2gse(defatt, traj.t_180, temporary(traj_180_dbcs_hi), TYPE='Z')

;-----------------------------------------------------
; GSE --> GSM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;0-PA
	traj_0_gsm    = mms_rot_gse2gsm(traj.t_0, traj_0_gse)
	traj_0_gsm_lo = mms_rot_gse2gsm(traj.t_0, traj_0_gse_lo)
	traj_0_gsm_hi = mms_rot_gse2gsm(traj.t_0, traj_0_gse_hi)
	
	;180-PA
	traj_180_gsm    = mms_rot_gse2gsm(traj.t_180, traj_180_gse)
	traj_180_gsm_lo = mms_rot_gse2gsm(traj.t_180, traj_180_gse_lo)
	traj_180_gsm_hi = mms_rot_gse2gsm(traj.t_180, traj_180_gse_hi)

;-----------------------------------------------------
; CARTESIAN --> SPHERICAL \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; TODO: Renormalize unit vectors in case of rounding errors?
	;
	
	;0-PA
	traj_0_gse     = cv_coord(FROM_RECT=traj_0_gse,     /TO_SPHERE, /DEGREES)
	traj_0_gse_lo  = cv_coord(FROM_RECT=traj_0_gse_lo,  /TO_SPHERE, /DEGREES)
	traj_0_gse_hi  = cv_coord(FROM_RECT=traj_0_gse_hi,  /TO_SPHERE, /DEGREES)
	traj_0_gsm     = cv_coord(FROM_RECT=traj_0_gsm,     /TO_SPHERE, /DEGREES)
	traj_0_gsm_lo  = cv_coord(FROM_RECT=traj_0_gsm_lo,  /TO_SPHERE, /DEGREES)
	traj_0_gsm_hi  = cv_coord(FROM_RECT=traj_0_gsm_hi,  /TO_SPHERE, /DEGREES)
	
	;180-PA
	traj_180_gse     = cv_coord(FROM_RECT=traj_180_gse,     /TO_SPHERE, /DEGREES)
	traj_180_gse_lo  = cv_coord(FROM_RECT=traj_180_gse_lo,  /TO_SPHERE, /DEGREES)
	traj_180_gse_hi  = cv_coord(FROM_RECT=traj_180_gse_hi,  /TO_SPHERE, /DEGREES)
	traj_180_gsm     = cv_coord(FROM_RECT=traj_180_gsm,     /TO_SPHERE, /DEGREES)
	traj_180_gsm_lo  = cv_coord(FROM_RECT=traj_180_gsm_lo,  /TO_SPHERE, /DEGREES)
	traj_180_gsm_hi  = cv_coord(FROM_RECT=traj_180_gsm_hi,  /TO_SPHERE, /DEGREES)
	
	;
	; Change from elevation to polar angle
	;
	
	;0-PA
	traj_0_gse[1,*]     = abs(traj_0_gse[1,*]     - 90.0)
	traj_0_gse_lo[1,*]  = abs(traj_0_gse_lo[1,*]  - 90.0)
	traj_0_gse_hi[1,*]  = abs(traj_0_gse_hi[1,*]  - 90.0)
	traj_0_gsm[1,*]     = abs(traj_0_gsm[1,*]     - 90.0)
	traj_0_gsm_lo[1,*]  = abs(traj_0_gsm_lo[1,*]  - 90.0)
	traj_0_gsm_hi[1,*]  = abs(traj_0_gsm_hi[1,*]  - 90.0)
	
	;180-PA
	traj_180_gse[1,*]     = abs(traj_180_gse[1,*]     - 90.0)
	traj_180_gse_lo[1,*]  = abs(traj_180_gse_lo[1,*]  - 90.0)
	traj_180_gse_hi[1,*]  = abs(traj_180_gse_hi[1,*]  - 90.0)
	traj_180_gsm[1,*]     = abs(traj_180_gsm[1,*]     - 90.0)
	traj_180_gsm_lo[1,*]  = abs(traj_180_gsm_lo[1,*]  - 90.0)
	traj_180_gsm_hi[1,*]  = abs(traj_180_gsm_hi[1,*]  - 90.0)

;-----------------------------------------------------
; Deltas \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;0-PA
	traj_0_gse_lo = traj_0_gse - traj_0_gse_lo
	traj_0_gse_hi = traj_0_gse_hi - traj_0_gse
	traj_0_gsm_lo = traj_0_gsm - traj_0_gsm_lo
	traj_0_gsm_hi = traj_0_gsm_hi - traj_0_gsm
	
	;180-PA
	traj_180_gse_lo = traj_180_gse - traj_180_gse_lo
	traj_180_gse_hi = traj_180_gse_hi - traj_180_gse
	traj_180_gsm_lo = traj_180_gsm - traj_180_gsm_lo
	traj_180_gsm_hi = traj_180_gsm_hi - traj_180_gsm

;-----------------------------------------------------
; Store in Output Structure \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;0-PA
	out.traj_0_gse     = traj_0_gse[0:1,*]
	out.traj_0_gse_lo  = traj_0_gse_lo[0:1,*]
	out.traj_0_gse_hi  = traj_0_gse_hi[0:1,*]
	out.traj_0_gsm     = traj_0_gsm[0:1,*]
	out.traj_0_gsm_lo  = traj_0_gsm_lo[0:1,*]
	out.traj_0_gsm_hi  = traj_0_gsm_hi[0:1,*]
	
	;180-PA
	out.traj_180_gse     = traj_180_gse[0:1,*]
	out.traj_180_gse_lo  = traj_180_gse_lo[0:1,*]
	out.traj_180_gse_hi  = traj_180_gse_hi[0:1,*]
	out.traj_180_gsm     = traj_180_gsm[0:1,*]
	out.traj_180_gsm_lo  = traj_180_gsm_lo[0:1,*]
	out.traj_180_gsm_hi  = traj_180_gsm_hi[0:1,*]
	
;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	return, out
end


;+
;   Sort trajectories by their reported pitch angle.
;
; :Params:
;       T_GDU1:         in, required, type=long64arr (cdf_time_tt2000)
;                       Time tags for GDU1 data.
;       T_GDU2:         in, required, type=long64arr (cdf_time_tt2000)
;                       Time tags for GDU2 data.
;       PITCH_GDU1:     in, required, type=long64arr (cdf_time_tt2000)
;                       Pitch angle flag computed on board the spacecraft for GDU1.
;       PITCH_GDU2:     in, required, type=long64arr (cdf_time_tt2000)
;                       Pitch angle flag computed on board the spacecraft for GDU2.
;       DATA:           in, required, type=long64arr (cdf_time_tt2000)
;                       A structure of trajectory vectors with the following fields:
;                           TRAJ_GDU1     -  Trajectory vector for electrons into GDU1 in EDI coordinates.
;                           TRAJ_GDU1_LO  -  Lower-bound for trajectory vector for electrons into GDU1.
;                           TRAJ_GDU1_HI  -  Upper-bound for trajectory vector for electrons into GDU1.
;                           TRAJ_GDU2     -  Trajectory vector for electrons into GDU2 in EDI coordinates.
;                           TRAJ_GDU2_LO  -  Lower-bound for trajectory vector for electrons into GDU2.
;                           TRAJ_GDU2_HI  -  Upper-bound for trajectory vector for electrons into GDU2.
;
; :Returns:
;       OUT:             A data structure with the following tags:
;                           TRAJ_0       -  Trajectory vector for 0-degree pitch angle electrons in EDI coordinates.
;                           TRAJ_0_LO    -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_0_HI    -  Upper-bound for trajectory vector for electrons.
;                           TRAJ_180     -  Trajectory vector for 180-degree pitch angle electrons in EDI coordinates.
;                           TRAJ_180_LO  -  Lower-bound for trajectory vector for electrons.
;                           TRAJ_180_HI  -  Upper-bound for trajectory vector for electrons.
;-
function srvy_traj_sort, t_gdu1, t_gdu2, pitch_gdu1, pitch_gdu2, data
	compile_opt idl2
	on_error, 2
	
;-----------------------------------------------------
; Sort By Pitch Angle 0 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Find 0s
	i0_gdu1 = where(pitch_gdu1 eq 0, n0_gdu1)
	i0_gdu2 = where(pitch_gdu2 eq 0, n0_gdu2)

	;Both detectors have data
	if n0_gdu1 gt 0 && n0_gdu2 gt 0 then begin
		t_0       = [ t_gdu1[i0_gdu1], t_gdu2[i0_gdu2] ]
		traj_0    = [ [ data.traj_gdu1[*,i0_gdu1]    ], [ data.traj_gdu2[*,i0_gdu2] ] ]
		traj_0_lo = [ [ data.traj_gdu1_lo[*,i0_gdu1] ], [ data.traj_gdu2[*,i0_gdu2] ] ]
		traj_0_hi = [ [ data.traj_gdu1_hi[*,i0_gdu1] ], [ data.traj_gdu2[*,i0_gdu2] ] ]

		;Sort times
		isort     = sort(t_0)
		t_0       = t_0[isort]
		traj_0    = traj_0[*,isort]
		traj_0_lo = traj_0_lo[*,isort]
		traj_0_hi = traj_0_hi[*,isort]
		
		;Mark GDU
		;   - GDU1 and GDU2 are complements of one another
		;   - When GDU1 is in PA0 mode, GDU2 will be in PA180 mode.
		;   - Therefore, we can use the original indices
		;   - It is the same as storing 1B then 2B and soring with ISORT
		gdu_0 = bytarr(n0_gdu1 + n0_gdu2)
		gdu_0[i0_gdu1] = 1B
		gdu_0[i0_gdu2] = 2B
	
	;GDU1 has data
	endif else if n0_gdu1 gt 0 then begin
		t_0       = t_gdu1[i0_gdu1]
		traj_0    = data.traj_gdu1[*,i0_gdu1]
		traj_0_lo = data.traj_gdu1_lo[*,i0_gdu1]
		traj_0_hi = data.traj_gdu1_hi[*,i0_gdu1]
		gdu_0     = replicate(1B, n0_gdu1)
	
	;GDU2 has data
	endif else if n0_gdu2 gt 0 then begin
		t_0       = t_gdu2[i0_gdu2]
		traj_0    = data.traj_gdu2[*,i0_gdu2]
		traj_0_lo = data.traj_gdu2_lo[*,i0_gdu2]
		traj_0_hi = data.traj_gdu2_hi[*,i0_gdu2]
		gdu_0     = replicate(2B, n0_gdu2)
	endif else begin
		MrPrintF, 'LogText', 'No 0 degree pitch angle data.'
		t_0      = 0LL
		counts_0 = -1S
	endelse
	
;-----------------------------------------------------
; Sort By Pitch Angle 180 \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Find 18180s
	i180_gdu1 = where(pitch_gdu1 eq 180, n180_gdu1)
	i180_gdu2 = where(pitch_gdu2 eq 180, n180_gdu2)

	;Both detectors have data
	if n180_gdu1 gt 0 && n180_gdu2 gt 0 then begin
		t_180       = [ t_gdu1[i180_gdu1], t_gdu2[i180_gdu2] ]
		traj_180    = [ [ data.traj_gdu1[*,i180_gdu1]    ], [ data.traj_gdu2[*,i180_gdu2] ] ]
		traj_180_lo = [ [ data.traj_gdu1_lo[*,i180_gdu1] ], [ data.traj_gdu2[*,i180_gdu2] ] ]
		traj_180_hi = [ [ data.traj_gdu1_hi[*,i180_gdu1] ], [ data.traj_gdu2[*,i180_gdu2] ] ]

		;Sort times
		isort       = sort(t_180)
		t_180       = t_180[isort]
		traj_180    = traj_180[*,isort]
		traj_180_lo = traj_180_lo[*,isort]
		traj_180_hi = traj_180_hi[*,isort]
		
		;Mark GDU
		;   - GDU1 and GDU2 are complements of one another
		;   - When GDU1 is in PA0 mode, GDU2 will be in PA180 mode.
		;   - Therefore, we can use the original indices
		;   - It is the same as storing 1B then 2B and soring with ISORT
		gdu_180 = bytarr(n180_gdu1 + n180_gdu2)
		gdu_180[i180_gdu1] = 1B
		gdu_180[i180_gdu2] = 2B
	
	;GDU1 has data
	endif else if n180_gdu1 gt 0 then begin
		t_180       = t_gdu1[i180_gdu1]
		traj_180    = data.traj_gdu1[*,i180_gdu1]
		traj_180_lo = data.traj_gdu1_lo[*,i180_gdu1]
		traj_180_hi = data.traj_gdu1_hi[*,i180_gdu1]
		gdu_180     = replicate(1B, n180_gdu1)
	
	;GDU2 has data
	endif else if n180_gdu2 gt 0 then begin
		t_180       = t_gdu2[i180_gdu2]
		traj_180    = data.traj_gdu2[*,i180_gdu2]
		traj_180_lo = data.traj_gdu2_lo[*,i180_gdu2]
		traj_180_hi = data.traj_gdu2_hi[*,i180_gdu2]
		gdu_180     = replicate(2B, n180_gdu2)
	endif else begin
		MrPrintF, 'LogText', 'No 180 degree pitch angle data.'
		t_180      = 180LL
		counts_180 = -1S
	endelse
	
;-----------------------------------------------------
; Out \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	out = { t_0:         temporary(t_0), $
	        t_180:       temporary(t_180), $
	        gdu_0:       temporary(gdu_0), $
	        gdu_180:     temporary(gdu_180), $
	        traj_0:      temporary(traj_0), $
	        traj_0_lo:   temporary(traj_0_lo), $
	        traj_0_hi:   temporary(traj_0_hi), $
	        traj_180:    temporary(traj_180), $
	        traj_180_lo: temporary(traj_180_lo), $
	        traj_180_hi: temporary(traj_180_hi) $
	      }

	return, out
end


;+
;
;-
function mms_edi_amb_l2_trajectories, edi, dss_file, defatt_file
	compile_opt idl2
	on_error, 2
	
	;Burst modes?
	tf_brst = MrStruct_HasTag(edi, 'COUNTS4_GDU1')

	;Check packing mode
	if tf_brst then begin
		if ~array_equal(edi.pack_mode, edi.pack_mode[0]) then $
			message, 'Packing mode is changing.'
	
	;SRVY
	endif else begin
		;PACK_MODE 0 and 1 are identical
		if ~array_equal(edi.pack_mode, edi.pack_mode[0]) then begin
			nPacMo     = n_elements(edi.pack_mode)
			tf_pacmo01 = MrIsMember([0B,1B], edi.pack_mode, COUNT=nPacMo01)
			if nPacMo ne nPacMo01 then message, 'Packing mode is changing.'
		endif
	endelse

	;BRST data
	if tf_brst then begin
		;Compute trajectories
		traj = brst_traj_compute(reform(edi.azimuth), reform(edi.polar), edi.pack_mode[0])
	
		;Sort them by pitch mode
		traj = brst_traj_sort(edi.epoch_gdu1, edi.epoch_gdu2, edi.pitch_gdu1, edi.pitch_gdu2, temporary(traj))
	
		;Despin and rotate to Geophysical coordinates
		traj = brst_traj_rotate(temporary(traj), dss_file, defatt_file)
	
	;SRVY data
	endif else begin
		;Compute trajectories
		traj = srvy_traj_compute(reform(edi.azimuth), reform(edi.polar), edi.pack_mode[0])
	
		;Sort them by pitch mode
		traj = srvy_traj_sort(edi.epoch_gdu1, edi.epoch_gdu2, edi.pitch_gdu1, edi.pitch_gdu2, temporary(traj))
	
		;Despin and rotate to Geophysical coordinates
		traj = srvy_traj_rotate(temporary(traj), dss_file, defatt_file)
	endelse
	
	;Return
	return, traj
end

