function BeamPhaseSort, $
	phasearr, refphase, avg, x1, x2

	; NOTE: Either x1 or x2 could be [-1], but not both.

	tmpphase = phasearr
	
	x = where (tmpphase gt refphase+90)
	if x(0) ne -1 then $
		tmpphase(x) = tmpphase(x) - 360
	
	x = where (tmpphase lt refphase-270)
	if x(0) ne -1 then $
		tmpphase(x) = tmpphase(x) + 360
	
	sqsum = 0.0

	x2 = where (tmpphase lt refphase-90, cnt2)
	if cnt2 gt 0 then begin
		ph2   = tmpphase(x2)
		avg   = total(ph2)/cnt2
		sqsum = sqsum + total((ph2-avg)^2)
		if avg lt -180 then $
			avg = avg + 360
		if avg gt +180 then $
			avg = avg - 360
	endif

	x1 = where(tmpphase ge refphase-90, cnt1)
	if cnt1 gt 0 then begin
		ph1   = tmpphase(x1)
		avg   = total(ph1)/cnt1
		sqsum = sqsum + total((ph1-avg)^2)
		if avg lt -180 then $
			avg = avg + 360
		if avg gt  180 then $
			avg = avg - 360
	endif

	stdev = sqrt (sqsum / (cnt1+cnt2))

	return, stdev

end


; ============================================================================
 pro ep_hav_sorter, $
	phasearr, x1, x2, step = step, debug = debug, stdev = stdev
; ============================================================================

	refa = 0.0 & refb = 120.0 & refc = 240.0
	
	stdeva = BeamPhaseSort (phasearr, refa, avga, x1, x2)
	stdevb = BeamPhaseSort (phasearr, refb, avgb, x1, x2)
	stdevc = BeamPhaseSort (phasearr, refc, avgc, x1, x2)

	for icnt=2,6 do begin
		refa = avga & refb = avgb & refc = avgc

		stdeva = BeamPhaseSort (phasearr, refa, avga, x1, x2)
		stdevb = BeamPhaseSort (phasearr, refb, avgb, x1, x2)
		stdevc = BeamPhaseSort (phasearr, refc, avgc, x1, x2)
	endfor

	if avga gt avgb then $
		ref = avga $
	else $
		ref = avgb
	if avgc gt ref then $
		ref = avgc

	stdev = BeamPhaseSort (phasearr, ref, avg, x1, x2)
end
