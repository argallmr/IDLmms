; docformat = 'rst'
;
; NAME:
;       ep_recalc_aerror
;
;+
;   Calculate error the standard deviation of the angular difference between
;   the angle from gun to target and the actual firing angle.
;
; :Params:
;       TARGET_MAG:     in, required, type=float
;                       Drift step magnitude (meters).
;       TARGET_PHI:     in, optional, type=intarr
;                       Angle (degrees) to target in BPP.
;       RUNORDER:       in, required, type=intarr
;                       Runner order of each beam.
;       XG_BEAM:        in, required, type=fltarr
;                       X-cooridinate (meters) in BPP of the GDU location.
;       YG_BEAM:        in, required, type=fltarr
;                       Y-cooridinate (meters) in BPP of the GDU location.
;       ALPHA_BEAM:     in, required, type=fltarr
;                       Firing angle (degrees) of GDU in BPP.
;
; :Returns:
;       AERROR:         out, required, type=float
;                       Standard deviation of the angular difference between
;                           the gun firing angle and the angle to target (degrees).
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015-10-06  -   Written by Matthew Argall. Adapted from ep_recalc_aerror written
;                           Cluster's bestarg.
;-
function ep_recalc_aerror, target_mag, target_phi, runorder, $
                           xg_beam,    yg_beam,    alpha_beam

	;Vector components of the target
	dx = target_mag * cos(target_phi*!dtor)
	dy = target_mag * sin(target_phi*!dtor)

	;Vector pointing from gun to beam
	dely = runorder*dx - xg_beam
	delx = runorder*dy - yg_beam
	
	;Difference between firing angle and angle to target.
	dalp = atan(dely, delx)
	dalp = abs(dalp - alpha_beam*!radeg)

	; Must check direction (given that we might have away beams)
	dalp = dalp * (dalp lt !pi)   + (2*!pi-dalp) * (dalp ge !pi)
	dalp = dalp * (dalp lt !pi/2) + (!pi-dalp)   * (dalp ge !pi/2)

	;Standard deviation of angular difference between angle to target and firing angle
	aerror = stddev(dalp)*!radeg
	return, aerror
end
