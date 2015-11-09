; docformat = 'rst'
;
; NAME:
;       ep_method_logic_rmt_sa
;
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
;-
function ep_method_logic_rmt_sa, beam, mean_angle_out, stdev_angle_out, out_out,     $
                                       bestord_out,    status_out,      msg_out,     $
                                       method_out,     edi6,            ambig_180,   $
                                       classA_towards, nonA_towards,    classA_away, $
                                       nonA_away,      beam_used,       perr3, $
CHECKPAR=checkpar
	common ep_envar_con_cb

	;Check parallelism of beams
	checkpar = keyword_set(checkpar)

	;******************************************************************
	; Perform the to/aw sorting, phito/stdev calculation
	;******************************************************************
	; 1) Which beams do we use for the to/aw sorting and phito/stdev
	;    calculation? THIS MUST BE A SUPERSET OF OR IDENTICAL TO IDUSE_RMT
	;    DEFINED BELOW!
	;    Because for every beam in iduse_rmt, the towards/away assignment
	;    must have been made before ep_richmans_tof_sa.pro is called.
	
	;Calculate the mean firing vector and its standard deviation
	;   - As a side-effect, beams are classified into TOWARD and AWAY categoreis
	iduse           = lindgen(n_elements(beam.maxchan))
	ep_toaw_sort_sa, beam, iduse, phito, stdev
	mean_angle_out  = phito
	stdev_angle_out = stdev

	;******************************************************************
	; Branch to TRI if we're not in Forced RMT (i.e. checkpar=1),
	; and beams fail parallelism test
	;******************************************************************
	if ( checkpar && (stdev gt pp_para_anglim_rad) ) then $
		goto, tri_continue

	;******************************************************************
	; We are HERE because of one of the following:
	; 1) Forced RMT desired, parallelism calculated, but not checked
	; 2) Parallelism test passed
	; Regardless of which one, there will be no branching to TRI below
	; Begin Prep-work for RMT
	; Which beams do we use for Richman's ToF? HAV uses:
	;    1) MaxChan = 7
	;    2) Qual >= 2
	;    3) Short code only
	;    4) Class "A" and "B" only (exclude "C", "D", "G", "H")
	;    5) Include those beams which may have been designated TRI
	;       outliers (don't check 'out' variable)
	;******************************************************************

	;Can we use the Rich Man's Time of Flight (RMT) method? 
	;   - Assume that we can RMT.
	TF_RMT_25 = 1                   ; 0 = Not enough beams to continue
	TF_RMT_26 = 1                   ; 0 = Not enough beams in each to/aw class to continue
	
	;Find beams that match our criteria
	iduse_rmt = where( beam.maxchan    eq 7   and $
	                   beam.qual       ge 2   and $
	                   beam.code_type  eq 0   and $
	                  (beam.class_init eq 'A' or  $
	                   beam.class_init eq 'B'),      nrmt)
	
	;If we did not find any beams, we cannot use RMT
	if nrmt eq 0 then begin
		TF_RMT_25 = 0
	endif else begin
		;The number of beams found has to be greater than our pre-defined threshold
		;   - The total number of beams from any gun has to be >= the threshold number
		;     for a single gun.
		if (nrmt lt 2*pp_tofclass_nbeam_min) then begin
			TF_RMT_25 = 0
		
		;Or, the number of beams from individual guns has to be larger than the single
		;gun threshold.
		endif else begin
			ito = where(beam.toaw_init(iduse_rmt) eq  1, nto)
			iaw = where(beam.toaw_init(iduse_rmt) eq -1, naw)
			if (nto lt pp_tofclass_nbeam_min or naw lt pp_tofclass_nbeam_min) $
				then TF_RMT_26 = 0
		endelse
	endelse

	;RTM not possible: Not enough total beams
	if (NO_RMT_25) then begin
		;STATUS_OUT = 17
		;   - pp_method=7 (TRI/RMT Logic)
		;   - Parallel beams (see earlier check)
		;   - RMT not possible
		;   - Beams too parallel for TRI
		;
		;STATUS_OUT = 25
		;   - pp_method=8 or 9 (Forced RMT)
		;   - Parallelness of beams unchecked
		;   - RMT not possible
		if checkpar  $
			then status_out = 17 $
			else status_out = 25

		;Obtain the pre-defined message
		msg_out = pp_pstat[status_out]
		
		;Do not continue with triangulation
		return, 0
		
	;RMT not possible: Not enough beams in each to/aw class
	endif else if (NO_RMT_26) then begin
		;STATUS_OUT = 14
		;   - pp_method=7 (TRI/RMT Logic)
		;   - Parallel beams (see earlier check)
		;   - RMT not possible
		;   - Beams too parallel for TRI
		;
		;STATUS_OUT = 26
		;   - pp_method=8 or 9 (Forced RMT)
		;   - Parallelness of beams unchecked
		;   - RMT not possible
		if checkpar $
			then status_out = 14 $
			else status_out = 26

		;Obtain the pre-defined status message
		msg_out = pp_pstat[status_out]
		
		;Do not continue with triangulation
		return, 0
	endif

	;================================================================
	; RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT
	method_out = 3
	irmt = ep_richmans_tof_sa ( beam,    iduse_rmt, phito,   stdev, $ 
	                            msg_rmt, edi6_rmt,  out_rmt, bestord_rmt, $
	                            classA_towards, nonA_towards, classA_away, nonA_away )

	;IRMT = 3 if successful
	if (irmt eq 3) then begin
		;Copy to output arrays
		edi6        = edi6_rmt
		out_out     = out_rmt
		bestord_out = bestord_rmt
		
		;If the drift step noise to signal ratio is large, it
		;means we have the 0 and 180 degree beams mixed up. We
		;need to swap them.
		if (edi6_rmt[1]/edi6_rmt[0] gt 1.0) then $
			then ambig_180 = 1 $
			else ambig_180 = 0

		;Array indicating which beams were used for this analysis
		beam_used = make_array(SIZE=size(out_out), VALUE=0)
		ii        = where(out_out eq 0)
		if (ii[0] ne -1) then beam_used[ii] = 1

		;Success = 3
		status_out = 3
		msg_out    = pp_pstat[status_out]

		;Recalculate the standard deviation of difference between firing angle
		;and angle to target.
		ep_recalc_aerror, edi6_rmt[0],   edi6_rmt[2],   beam_used,  bestord_out, $
		                  beam.GDU_locX, beam.GDU_locY, beam.alpha, perr3

	;RMT Error
	endif else begin
		status_out = irmt
		msg_out    = msg_rmt
	endelse

	return, 0                       ; 0 = Do not continue with TRI
	; RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT
	;================================================================

tri_continue:
	return, 1                       ; 1 = Continue with TRI
end
