; docformat = 'rst'
;
; NAME:
;       mms_edi_beam_sort
;
; PURPOSE:
;+
;   Segregate EDI beams into groups fired toward and away from the target.
;
;   NOTE:
;       "Toward" and "away" is ambiguous here. In order to determine if
;       on group is firing toward the target, you would need to examine
;       the 
;
; :Categories:
;   MMS, EDI
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/09/12  -   Written by Matthew Argall. Adapted from Hans Vaith's
;                           ep_hav_sorter.pro
;-
;*****************************************************************************************
;+
;   Sort firing vectors into the toward and away firing directions.
;
; :Params:
;       PHASEARR:       in, required, type=float
;                       Firing angle (degrees) of EDI electron beams projected into the
;                           plane perpendicular to B (BPP), positive clock-wise from
;                           the +x-axis.
;       REFPHASE:       in, required, type=float
;                       Reference firing phase. Ideally, this phase should be half-
;                            way between toward and away beams.
;       AVG:            out, required, type=lonarr
;                       Average phase of beams in either `X2`. If not beams are fir
;       X1:             out, required, type=lonarr
;                       Indices into `PHASEARR` of the beams that were fired toward
;                           the virtual source point.
;       X2:             out, required, type=lonarr
;                       Indices into `PHASEARR` of the beams that were fired away
;                           from the virtual source point.
;
; :Returns:
;       STDEV:          Combined standard deviation of the phase of toward and away
;                           beams from the mean toward and away directions.
;-
function BeamPhaseSort, phasearr, refphase, avg, x1, x2
	compile_opt idl2
	on_error, 2

	; NOTE: Either x1 or x2 could be [-1], but not both.

	;Editable copy of input
	tmpphase = phasearr
	
	;
	; The following two expressions will force PHASEARR to be
	; between -270 and +90 of REFPHASE
	;
	x = where (tmpphase gt refphase+90)
	if x[0] ne -1 then tmpphase[x] = tmpphase[x] - 360
	
	x = where(tmpphase lt refphase-270)
	if x[0] ne -1 then tmpphase[x] = tmpphase[x] + 360
	
	;We will be computing the variance of the firing phase
	;   sum_i{ (x_i - x_avg)^2 }
	sqsum = 0.0
	
	;
	; Split the phases into two groups of 180 degrees
	; centered around 0 and 180 degree phase-angles.
	;    1. -270 <= PHASEARR < -90
	;    2.  -90 <= PHASEARR <= 90
	;
	; Groups 1 and 2 represent
	;    1. Quadrants II & III  (left-half plane)
	;    2. Quadrants  I &  II  (right-half plane)
	;
	; Ideally, Group 1 (2) should be firing toward (away)
	; from the virtual source, or vice versa. REFPHASE
	; should be chosen to make this true.
	;
	
	;GROUP 1
	x2 = where (tmpphase lt refphase-90, cnt2)
	if cnt2 gt 0 then begin
		;Average value of the firing phase
		ph2   = tmpphase[x2]
		avg   = total(ph2)/cnt2
		sqsum = sqsum + total((ph2-avg)^2)
		
		;Force the average to be between 0 and 180 degrees
		if avg lt -180 then avg = avg + 360
		if avg gt +180 then avg = avg - 360
	endif

	;GROUP 2
	;   - Combine with the results from GROUP 1
	x1 = where(tmpphase ge refphase-90, cnt1)
	if cnt1 gt 0 then begin
		;Average value of the firing phase
		ph1   = tmpphase[x1]
		avg   = total(ph1)/cnt1
		sqsum = sqsum + total((ph1-avg)^2)
		
		;Force the average to be between 0 and 180 degrees
		if avg lt -180 then avg = avg + 360
		if avg gt  180 then avg = avg - 360
	endif

	;Standard deviation of all groups
	stdev = sqrt (sqsum / (cnt1+cnt2))

	return, stdev
end


;+
;   Sort firing vectors into the toward and away firing directions.
;
; :Params:
;       PHASEARR:       in, required, type=float
;                       Firing angle (degrees) of EDI electron beams projected into the
;                           plane perpendicular to B (BPP), positive clock-wise from
;                           the +x-axis.
;       IX:             out, required, type=lonarr
;                       Indices into `PHASEARR` of the beams that were fired toward
;                           the virtual source point. Note that there is a 180 degree
;                           ambiguity between "toward" and "away". Either `IX` or `IY`
;                           could be "toward".
;       IY:             out, required, type=lonarr
;                       Indices into `PHASEARR` of the beams that were fired away
;                           from the virtual source point. Note that there is a 180 degree
;                           ambiguity between "toward" and "away". Either `IX` or `IY`
;                           could be "away".
;
; :Keywords:
;       STDEV:          out, optional, type=string, default=''
;                       Combined standard deviation of the toward and away beams
;                           from the mean toward and away directions.
;-
pro ep_hav_sorter, phasearr, ix, iy, $
STDEV = stdev

	;Pick three reference phases in an attempt to
	;split the beams into toward and away groups
	refa =   0.0
	refb = 120.0
	refc = 240.0
	
	;Find the average angle between the two groups
	;   We hope to adjust the reference phase until all
	;   beams are clustered around 0 and 180 degree phase
	;   angles.
	stdeva = BeamPhaseSort(phasearr, refa, avga, x1, y1)
	stdevb = BeamPhaseSort(phasearr, refb, avgb, x2, y2)
	stdevc = BeamPhaseSort(phasearr, refc, avgc, x3, y3)

	;Five more iterations
	for icnt=2,6 do begin
		;Update the reference phase with the average phase,
		;hopefully centering the beams around 0 and 180 with
		;respect to the reference phase angle.
		refa = avga
		refb = avgb
		refc = avgc

		;Compute again
		stdeva = BeamPhaseSort(phasearr, refa, avga, x1, y1)
		stdevb = BeamPhaseSort(phasearr, refb, avgb, x2, y2)
		stdevc = BeamPhaseSort(phasearr, refc, avgc, x3, y3)
	endfor

	;One final time. Choose the maximum phase as the reference.
	ref   = avga > avgb > avgc
	stdev = BeamPhaseSort(phasearr, ref, avg, ix, iy)
	
	;Make do not return anything if no beams were found in a given directin
	;   - This might be true for one group, but not for both, so long as
	;     as PHASEARR has at least one element.
	if ix[0] eq -1 then void = temporary(ix)
	if iy[0] eq -1 then void = temporary(iy)
end
