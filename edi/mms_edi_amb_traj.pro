; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_traj
;
; PURPOSE:
;+
;   Convert EDI look directions to particle incident trajectories.
;
; :Categories:
;    MMS, EDI
;
; :See Also:
;   mms_edi_amb_l1a_read.pro
;   mms_edi_amb_ops_bitmask.pro
;   mms_edi_amb_anodes.pro
;   mms_edi_amb_anodes_phi.pro
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
;       2016/09/14  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Convert EDI instrument look directions to particle incident trajectories.
;
; :Params:
;       PHI:            in, required, type=fltarr
;                       Azimuthal look direction (degrees) for a single channel.
;       THETA:          in, required, type=fltarr
;                       Polar look directions (degrees).
;
; :Keywords:
;       DELTA_MINUS:    in, required, type=NxMx3 fltarr
;                       Lower bound for trajectories. N and M represent the number
;                           of `PHI` and `THETA` coordinates, respectively.
;       DELTA_PLUS:     in, required, type=NxMx3 fltarr
;                       Upper bound for trajectories. N and M represent the number
;                           of `PHI` and `THETA` coordinates, respectively.
;
; :Returns:
;       TRAJ:           in, required, type=NxMx3 fltarr
;                       Particle incident trajector in cartesian coordinates. N and M
;                           represent the number of `PHI` and `THETA` coordinates,
;                           respectively.
;-
function mms_edi_amb_traj_look2traj, phi, theta, $
DELTA_MINUS=traj_lo, $
DELTA_PLUS=traj_hi
	compile_opt idl2
	on_error, 2
	
	;Polar and azimuth angle resolution
	deg2rad = !dpi / 180D
	rad2deg = 180D / !dpi
	dtheta  = 360.0 / 512.0 * deg2rad
	dphi    = 360.0 / 32.0  * deg2rad
	rPhi    = phi   * deg2rad
	rTheta  = theta * deg2rad
	
	;Create a coordinate grid
	;   - POLAR is applied to all channels equally
	;   - AZIMUTH will be reformed as needed
	nPhi   = n_elements(phi)
	nTheta = n_elements(theta)
	if nPhi ne nTheta then message, 'PHI and THETA must have the same number of elements.'

	;Compute the trajectory
	;   - Negate to move from instrument look angle to indident trajectory
	traj      = fltarr(3,nPhi)
	traj[0,*] = -sin(rTheta) * cos(rPhi)
	traj[1,*] = -sin(rTheta) * sin(rPhi)
	traj[2,*] = -cos(rTheta)
	
	;DELTA_MINUS
	if arg_present(delta_minus) then begin
		traj_lo      = fltarr(3,nPhi)
		traj_lo[0,*] = -sin(rTheta - dTheta) * cos(rPhi - dPhi)
		traj_lo[1,*] = -sin(rTheta - dTheta) * sin(rPhi - dPhi)
		traj_lo[2,*] = -cos(rTheta - dTheta)
	endif
	
	;DELTA_PLUS
	if arg_present(delta_plus) then begin
		traj_hi      = fltarr(3,nPhi)
		traj_hi[0,*] = -sin(rTheta + dTheta) * cos(rPhi + dPhi)
		traj_hi[1,*] = -sin(rTheta + dTheta) * sin(rPhi + dPhi)
		traj_hi[2,*] = -cos(rTheta + dTheta)
	endif
	
	;Return
	return, traj
end


;+
;   Using EDI look directions, compute particle incident trajectories.
;
; :Params:
;       PHI:            in, required, type=struct
;                       Azimuthal look direction of each channel (degrees). Tags are:
;                           PHI_GDU1  -  [time, ch] Azimuth angle corresponding to each channel of GDU1
;                           PHI_GDU2  -  [time, ch] Azimuth angle corresponding to each channel of GDU2
;       THETA:          in, required, type=fltarr
;                       Polar look directions (degrees) in instrument coordinates. THETA
;                           in EDI1 coordinates is the same as THETA in EDI2 coordinates.
;
; :Returns:
;       TRAJ:           out, required, type=struct
;                       Particle incident trajectories in cartesian coordinates. Tags are:
;                           TRAJ_GDU1  -  [comp, t, ch] Trajectories corresponding to each channel of GDU1
;                           TRAJ_GDU2  -  [comp, t, ch] Trajectories corresponding to each channel of GDU2
;-
function mms_edi_amb_traj, phi, theta
	compile_opt idl2
	on_error, 2
	
	;Burst data?
	;   - Four channels are active in burst mode (1 in survey)
	dims    = size(phi.phi_gdu1, /DIMENSIONS)
	tf_brst = n_elements(dims) eq 2
	nChannels = tf_brst ? dims[1] : 1
	nPts      = dims[0]

	;Allocate memory
	traj_gdu1 = fltarr(3, nPts, nChannels)
	traj_gdu2 = fltarr(3, nPts, nChannels)

	;Compute trajectories for each component
	for i = 0, nChannels - 1 do begin
		traj_gdu1[0,0,i] = mms_edi_amb_traj_look2traj( phi.phi_gdu1[*,i], theta )
		traj_gdu2[0,0,i] = mms_edi_amb_traj_look2traj( phi.phi_gdu2[*,i], theta )
	endfor
	
	;Gather data
	data = { traj_gdu1: temporary(traj_gdu1), $
	         traj_gdu2: temporary(traj_gdu2) $
	       }
	
	;Return
	return, data
end