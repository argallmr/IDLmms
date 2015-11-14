; docformat = 'rst'
;
; NAME:
;       ep_method_logic_rmt_sa
;
;+
;   Determine if we are in the time of flight regime or triangulation regime.
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
;       2015-09-14  -   Written by Matthew Argall. Adapted from ep_method_logic_rmt
;                           from Cluster's bestarg.
;       2015-09-14  -   Written by Matthew Argall. Adapted from ep_richmans_tof_sa
;                           from Cluster's bestarg.
;-
;*****************************************************************************************
;+
;   Determine if we are in the time of flight regime or triangulation regime.
;
; :Params:
;       BEAM:           in, required, type=struct
;                       Structure with information from all beams.
;       IDUSE:          in, required, type=intarr
;                       Indices into `BEAM` indicating the beams to use for this analysis.
;                           Must have previously been checked to meet the minimum number
;                           of beams requirement.
;       PHITO:          in, required, type=fltarr
;                       Mean firing angle (radians) toward the virtual source point
;                           for `IDUSE` beams.
;       STDEV:          in, required, type=fltarr
;                       Standard deviation of beams angles (radians) from `PHITO`.
;       MSG_RMT:        out, required, type=string
;                       Status message taken from PP_PSTAT.
;       EDI6_RMT:       out, required, type=6x1 fltarr
;                       Results. Elements are::
;                           0 = drift step magnitude, meters
;                           1 = drift step magnitude error, meters
;                           2 = drift step azimuthal angle, radians
;                           3 = drift step azimuthal angle error, radians
;                           4 = gyrotime, micro-seconds
;                           5 = gyrotime error, micro-seconds
;       OUT_RMT:        out, required, type=intarr
;                       Flag array with the following values:
;                           0 = Used for this round of RMT
;                           2 = Triangulation outliers
;                           3 = noClassC outliers
;                           6 = Fill value (i.e. value not yet assigned)
;       BESTORD_RMT:    out, required, type=intarr
;                       Runner order for the beam set corresponding to `IDUSE`.
;                           Depends a somehow on beam class.
;       CLASSA_TOWARD:  out, required, type=integer
;                       Number of class A beams fired toward the target.
;       NONA_TOWARD:    out, required, type=integer
;                       Number of non-class A beams fired toward the target.
;       CLASSA_AWAY:    out, required, type=integer
;                       Number of class A beams fired away from the target.
;       NONA_AWAY:      out, required, type=integer
;                       Number of non-class A beams fired away from the target.
;
; :Returns:
;       TF_TRI:         Returns 1 of triangulation should be used, 0 if not.
;
; :Common Blocks:
;   ep_envar_con_cb
;       PP_BFILL          - Scalar integer fill value.
;       PP_RFILL          - Fill value used for EDI6_RMT[5]
;       PP_PSTAT          - Array of pre-defined status messages.
;       PP_RMT_DTOF_ERROR - Confidence limit on drift step magnitude to use.
;
function mms_edi_rmt_calc, beams, phito, stdev, $
ORDER=order_lp, $
STATS=stats, $
DTOF_ERROR=dtof_error
	compile_opt idl2
	on_error, 2

;---------------------------------------------------------------------
; Select Data of Interest ////////////////////////////////////////////
;---------------------------------------------------------------------
	nbeams = n_elements(beams)
	if n_elements(dtof_error) eq 0 then dtof_error = 1   ;90% confidence
	
	;Lowest possible order
	order_lp = beams.posord[0]
	
	;Time of flight associated with lowest runner order
;	tof = beams.estof[order_lp]
	tof = beams.tof

	;Find toward and away beams
	ito = where(beams.toaw eq  1, nto)
	iaw = where(beams.toaw eq -1, naw)
	if nto + naw ne nbeams then message, 'No "toward" or "away" assignment for some beams.'
	
	;Make sure we have a single energy
	if max(beams[1:*].energy - beams.energy) ne 0 $
		then message, 'All beams must be the same energy.' $
		else v_beam = beams[0].v

;---------------------------------------------------------------------
; Disambiguous Toward/Away Assignment ////////////////////////////////
;---------------------------------------------------------------------
	; Construct the towards and away times-of-flight RELATIVE TO THE
	; GYROTIME TREND, in the hopes that this detrending of the data will
	; improve our attempts at calculating the true dtof
	;
	; Toward: ToF > Tg
	; Away:   ToF < Tg
	;
	dtof_to = tof[ito] - beams[ito].tGyro ; These values will be mostly positive if towards/away assignment is correct
	dtof_aw = tof[iaw] - beams[iaw].tGyro ; These values will be mostly negative if towards/away assignment is correct

	; If mean(dtof_to) < mean(dtof_aw) then we need to flip the assignment
	if mean(dtof_to) lt mean(dtof_aw) then begin
		;Swap "Toward" and "Away" indices
		temp_to  = ito
		temp_aw  = iaw
		ito      = temporary(temp_aw)
		iaw      = temporary(temp_to)
		
		;Recalculate the difference between Tg and TOF
		dtof_to  = tof[ito] - beams[ito].tGyro
		dtof_aw  = tof[iaw] - beams[iaw].tGyro
		
		;Flip the mean firing angle
		phito = phito + !pi
		
		;Reassign to and away flags
		beams[ito].toaw =  1
		beams[iaw].toaw = -1
	endif

;---------------------------------------------------------------------
; ToF Error //////////////////////////////////////////////////////////
;---------------------------------------------------------------------

	; Assess whether or not the difference between the mean towards
	; and away ToFs is significant.
	tof_errstruct = mms_edi_tof_error(dtof_to, dtof_aw)

	;If there was an error calculating the ToF Error
	if (tof_errstruct.status ne 0) $
		then message, tof_errstruct.msg

	; Select which error on dtof you want to use to define the error on
	; the drift step magnitude
	case dtof_error of
		0:    dtof_error = tof_errstruct.err_t68
		1:    dtof_error = tof_errstruct.err_t90
		2:    dtof_error = tof_errstruct.err_t95
		3:    dtof_error = tof_errstruct.err_nr
		else: message, 'DTOF_ERROR (' + strtrim(dtof_error, 2) + ') should be a value between 0 and 3.'
	endcase

;---------------------------------------------------------------------
; Output Quantities //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Output
	;   - Drift step magnitude
	;   - Drift step magnitude error
	;   - Drift step azimuthal angle
	;   - Drift step azimuthal angle error
	;   - Average gyro-time
	;   - XXX Average gyro-time error (PP_RFILL)
	tof_results            = {edi_tof}
	tof_results.dmag       = tof_errstruct.dtof * 1e-6 * v_beam / 2.0   ; meters
	tof_results.dmag_sigma = dtof_error         * 1e-6 * v_beam / 2.0   ; meters
	tof_results.phi        = atan( sin(phito), cos(phito) )             ; [-pi,pi], rad
	tof_results.phi_sigma  = stdev                                      ; rad
	tof_results.tGyro      = mean(beams.tGyro)                          ; ns

;---------------------------------------------------------------------
; Housekeeping ///////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Statistics -- Number of class (not) A toward/away beams
	idum = where(beams.toaw eq  1 and beams.class eq 'A', nClassA_toward, NCOMPLEMENT=nNonA_toward)
	idum = where(beams.toaw eq -1 and beams.class eq 'A', nClassA_away,    NCOMPLEMENT=nNonA_away)

	;Output stats
	stats = { nClassA_toward: nClassA_toward, $
	          nClassA_away:   nClassA_away,   $
	          nNonA_toward:   nNonA_toward,   $
	          nNonA_away:     nNonA_away      $
	        }

	; Indicate we have used time of flight
	;   - Time-of-Flight analysis is bit 2^1
	;   - Triangulation  analysis is bit 2^0
	beams.tritof_flag += 2 * ((beams.tritof_flag and 2) eq 0)

	;Successful Return
	return, tof_results
end


;+
;   Determine if we are in the time of flight regime or triangulation regime.
;
; :Params:
;       BEAM:           in, required, type=struct
;                       Structure with beam information.
;     MEAN_ANGLE_OUT:   out, required, type=fltarr
;                       Average beam firing angle in BPP
;     TDEV_ANGLE_OUT:   out, required, type=fltarr
;                       Standard deviation of beam angles from `MEAN_ANGLE_OUT`.
;       OUT_RMT:        out, required, type=intarr
;                       Flag array with the following values:
;                           0 = Used for this round of RMT
;                           2 = Triangulation outliers
;                           3 = noClassC outliers
;                           6 = Fill value (i.e. value not yet assigned)
;       BESTORD_OUT:    out, required, type=
;                       Runner order estimate
;       STATUS_OUT:     out, required, type=integer
;                       Index into `PP_PSTAT` common block variable
;       MSG_OUT:        out, required, type=string
;                       Status message associated with `STATUS_OUT`.
;       METHOD_OUT:     out, required, type=string
;                       Method performed. 3 = Rich Man's ToF (RMT)
;       EDI6:           out, required, type=fltarr
;                       Results. Elements are::
;                           0 = drift step magnitude, meters
;                           1 = drift step magnitude error, meters
;                           2 = drift step azimuthal angle, radians
;                           3 = drift step azimuthal angle error, radians
;                           4 = gyrotime, micro-seconds
;                           5 = gyrotime error, micro-seconds
;       AMBIG_180:      out, required, type=
;                       Flag indicating that toward and away should be swapped
;       CLASSA_TOWARD:  out, required, type=integer
;                       Number of class A toward beams.
;       NONA_TOWARD:    out, required, type=integer
;                       Number of class not-A toward beams.
;       CLASSA_AWAY:    out, required, type=integer
;                       Number of class A away beams.
;       NONA_AWAY:      out, required, type=integer
;                       Number of class not-A away beams.
;       BEAM_USED:      out, required, type=intarr
;                       Indicates which beams were used in the present analysis.
;       PERR3:          out, required, type=
;                       Standard deviation of the difference between firing angle
;                           and angle to target (degrees).
;
; :Keywords:
;       CHECKPAR:       in, optional, type=boolean, default=0
;                       Check parallelism of beams.
;
; :Returns:
;       TF_TRI:         Returns 1 of triangulation should be used, 0 if not.
;
; :Common Blocks:
;   ep_envar_con_cb
;       PP_PARA_ANGLIM_RAD    - Maximum angle that defines beams to be parallel to mean direction
;       PP_BFILL              - Scalar integer fill value
;       PP_TOFCLASS_NBEAM_MIN - Minimum number of beams for a single gun that can be used for ToF
;-
function mms_edi_rmt, beams,   phito,     stdev, $
                      bestord, ambig_180, stats, aerror, $
CHECKPAR=checkpar, $
PARA_ANG_LIM=para_ang_lim, $
TOFCLASS_NBEAM_MIN=tofclass_nbeam_min

	;Error handling for TOF analysis
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		
		;Create an output structure
		tof.dmag       = !values.f_nan
		tof.dmag_sigma = !values.f_nan
		tof.phi        = !values.f_nan
		tof.phi_sigma  = !values.f_nan
		tof.tGyro      = !values.f_nan
		
		return, tof
	endif
	
	;Define time-of-flight structure
	tof = { edi_tof,         $
	        dmag:       0.0, $
	        dmag_sigma: 0.0, $
	        phi:        0.0, $
	        phi_sigma:  0.0, $
	        tGyro:      0.0  $
	      }

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Check parallelism of beams
	checkpar = keyword_set(checkpar)
	nbeams   = n_elements(beams)
	if n_elements(para_ang_lim)       eq 0 then para_ang_lim       = 10.0
	if n_elements(tofclass_nbeam_min) eq 0 then tofclass_nbeam_min = 2

;-----------------------------------------------------
; Exit For Triangulation \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; TRIANGULATION if:
	;   1) CHECKPAR = 0 (forced TOF analysis)
	;   2) Beam spread is greater than the minimum angular limit (i.e. not parallel)
	;
	if ( checkpar && (stdev gt para_ang_lim) ) then return, 1

;-----------------------------------------------------
; Beam Check \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Which beams are allowed?
	;   - Assume filter occurs outside.
	if total((beams.max_addr ge 30) and (beams.max_addr le 32)) ne nbeams $
		then message, 'Only MAX_ADDR = {30 | 31 | 32} beams allowed.'
	if total(beams.quality  ge 2)   ne nbeams then message, 'Only QUALITY >= 2 beams allowed.'
	if total(beams.numchips eq 255) ne nbeams then message, 'Only NUMCHIPS = 255 beams allowed.'
	if total((beams.class  eq 'A') or (beams.class eq 'B')) ne nbeams $
		then message, 'Only CLASS "A" or "B" beams allowed.'

	;Pick out toward and away beams
	ito = where(beams.toaw eq  1, nto)
	iaw = where(beams.toaw eq -1, naw)

	;Do we have enough beams to continue?
	if nbeams lt 2*tofclass_nbeam_min then begin
		;In the first case, TRI is not possible either
		;In the second casae, we did not check for paralellism to see how TRI would do.
		if checkpar $
			then message, string(nbeams, FORMAT='(%"RMT/TRI fail. Not enough beams (%i) for RMT. Beams to parallel for TRI.")') $
			else message, string(nbeams, FORMAT='(%"RMT fail (forced): Not enough beams (%i) to start with.")')
			
	;Do we have enough beams in a given Toward/Away class?
	;   - This check is performed inside EP_RICHMANS_TOF_SA as well
	endif else begin
		if (nto lt tofclass_nbeam_min or naw lt tofclass_nbeam_min) then begin
			;In the first case, TRI is not possible either
			;In the second casae, we did not check for paralellism to see how TRI would do.
			if checkpar $
				then message, string(nto, naw, FORMAT='(%"TRI/RMT Failure: Not enough beams in each to (%i) away (%i) class for RMT, too parallel for TRI")') $
				else message, string(nto, naw, FORMAT='(%"RMT Failure: Not enough beams in each to (%i) away (%i) class")')
		endif
	end

;-----------------------------------------------------
; Time of Flight Analysis \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Richman's Time of Flight
	tof = mms_edi_rmt_calc ( beams, phito, stdev, $ 
	                         ORDER = order, $
	                         STATS = stats )

	;If the drift step noise to signal ratio is large, it
	;means we have the 0 and 180 degree beams mixed up. We
	;need to swap them.
	if (tof.dmag_sigma / tof.dmag gt 1.0) $
		then ambig_180 = 1 $
		else ambig_180 = 0

	;Recalculate the standard deviation of difference between firing angle
	;and angle to target.
	aerror = ep_recalc_aerror(tof.dmag,         tof.phi,          order, $
	                          beams.gun_bpp[0], beams.gun_bpp[1], beams.alpha)

	return, tof
end
