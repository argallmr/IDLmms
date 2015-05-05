pro ep_calc_runner_penalty, $
	nBeams, runprob, beam_penalty

	common ep_envar_con_cb
	
	; Keep track of beam order statistics
	; At this point in the code, it is assumed that the prob
	; array will contain only valid probabilities (no fills)
	; because every beam that's being used has successfully been
	; assigned probabilities by Hans' RunEst.pro
	
	for im = 0, nBeams-1 do begin
		prob = runprob (*,im)
		if (prob(0) ne pp_rfill) then begin
			mprob = max (prob)
			id = where ((mprob-prob) le (mprob-mprob/exp(1.0)))
			if (id(0) eq -1) then $
				message, 'Ooooops: ep_calc_runner_penalty: (mprob-prob) le (mprob-mprob/exp(1.0)) failed'
			beam_penalty (im) = n_elements (id)
		endif
	endfor
	
	return
end
