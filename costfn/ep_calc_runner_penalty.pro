; docformat = 'rst'
;
; NAME:
;       ep_calc_runner_penalty
;
;+
;   Determine runner order penalty based on number of beams having probability
;   within 1/e of the highest probable beam.
;
; :Params:
;       MM:             in, required, type=long
;                       Number of total beams.
;       RUNPROB:        in, required, type=NxM fltarr
;                       Pobability of each possible runner order for each beam.
;       BEAM_PENALTY:   out, required, type=intarr
;                       Penalty for each beam
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
;       2015-09-14  -   Written by Matthew Argall. Adapted from ep_calc_runner_penalty written
;                           by Hans Vaith for Cluster's bestarg.
;-

pro ep_calc_runner_penalty, mm, runprob, beam_penalty
	common ep_envar_con_cb

	; Keep track of beam order statistics
	; At this point in the code, it is assumed that the prob
	; array will contain only valid probabilities (no fills)
	; because every beam that's being used has successfully been
	; assigned probabilities by Hans' RunEst.pro

	;Step through each beam
	for im = 0, mm-1 do begin

		;Probability associated with each runner order for this beam
		prob = runprob[*,im]

		;If the probability is not a fill value
		if (prob[0] ne pp_rfill) then begin
			;Get the maximum probability
			mprob = max(prob)
			
			;Find beams that are within 1/e of MPROB
			id    = where((mprob-prob) le (mprob-mprob/exp(1.0)))
			if (id[0] eq -1) then message, 'Ooooops. Max probability not low enough.'
			
			;Penalize based on number of beams within 1/e of MPROB.
			beam_penalty[im] = n_elements(id)
		endif
	endfor
end


