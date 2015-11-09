; docformat = 'rst'
;
; NAME:
;       ep_prep_order
;
;+
;   Prep the initial multirunner beam order (1,2,3...) and class ('A'..'H')
;
; :Params:
;       NBEAMS:         in, required, type=long
;                       Total number of beams.
;       BEAM_RUNSTAT:   in, required, type=NxM fltarr
;                       Beam status array with the first dimension ordered as::
;                           RUNSTAT[0,*]                       - Runner order
;                           RUNSTAT[1:MAXORDER,*]              - Flag: 0 if probability > threshold
;                           RUNSTAT[MAXORDER+1:2*MAXORDER,*]   - Estimated single runner ToF
;                           RUNSTAT[2*MAXORDER+1,*]            - Gyrotime
;                           RUNSTAT[2*MAXORDER+2,*]            - Probability
;       STATUS:         in, required, type=struct
;                       Return status flag: 0=fail (not enough "A" beams), 1=pass
;       OUT:            in, required, type=intarr
;                       Flag indicating that::
;                           0 - Use for either TRI or TOF
;                           3 - Should not be used for TRI or TOF
;       CODE_TYPE:      in, required, type=struct
;                       Code type (0=short, 1=long)
;       TCHIP:          in, required, type=struct
;                       Correlator chip period (micro-seconds)
;       TCODE:          in, required, type=struct
;                       Correlator code period (micro-seconds)
;       N_POSORD:       out, required, type=lonarr
;                       Number of possible runner orders to try.
;       POSORD:         out, required, type=NxM lonarr
;                       Runner orders to try. [`MAXORD`, `NBEAMS`]
;       MAXORD:         out, required, type=struct
;                       Maximum of common block variables [pp_maxord, pp_maxord_runest]
;       BEAM_CLASS:     out, required, type=strarr
;                       Class of beam
;                           'A' - ORDER   = 1:   Maximum confidence below threshold
;                                 OUT     = 0:   
;                                 BEAM_PENALTY = 1.0 if order 1
;                                              = 1.5 if order 6
;                           'B' - ORDER   = 0:   Maximum confidence below threshold
;                                 FLAGTOT = 1-3: Between 1 and three runner orders have probability above threshold
;                                 OUT     = 0:   
;                                 BEAM_PENALTY = 1.5 if exactly one legal runner order
;                                              = 2|3 if 2 or 3 legal runner orders
;                           'B' - A second class "B" beams. More than three runner orders
;                                     are probably, but only the 3 most probable are kept.
;                                 ORDER   = 0:   Maximum confidence below threshold
;                                 FLAGTOT = >3:  More than three runner orders have probability above threshold
;                                 OUT     = 0:   
;                                 BEAM_PENALTY = 3
;                           'C' - ORDER   = 0:  Maximum probability OR confidence below threshold
;                                 FLAGTOT = 0:  No probabilities above threshold
;                                 OUT     = 3:  Not used for TRI or TOF (cannot compute rchi2 for TRI)
;                                 BEAM_PENALTY = 0
;                           'D' - Beams not sent to RunEst; they meet one of the following
;                                   criteria (see ep_prep_runest):
;                                     a) Are fill values
;                                     b) Do not meet the minimum quality requirements
;                                     c) Do not have MAXCHAN = 7
;                                 OUT     = 3:  Not used for TRI or TOF (cannot compute rchi2 for TRI)
;                           'G' - ORDER   = 0:  Maximum probability OR confidence below threshold
;                                 FLAGTOT = 0:  Data is in in FGM data gap
;                                 OUT     = 3:  Not used for TRI or TOF (cannot compute rchi2 for TRI)
;                                 BEAM_PENALTY: 0
;                           'H' - Special case of "A" and "B" class beams when long code is in
;                                     use. If a single runner is not one of the possible runner
;                                     orders, the beam is thrown out.
;                                 ORDER   = 0:   Maximum confidence below threshold
;                                 FLAGTOT = 1-3: Between 1 and three runner orders have probability above threshold
;                                 OUT     = 3:   Not used for TRI or TOF
;                                 BEAM_PENALTY = 1.5 if exactly one legal runner order
;                                              = 2|3 if 2 or 3 legal runner orders
;       BEAM_PENALTY:   out, required, type=fltarr
;
; :Common Blocks:
;   ep_envar_con_cb
;       PP_RUNNER_PENALTY_METH - 0: a runner penalty of 1.0 (no penalty) is assigned to everything
;                                2: Penalized based on number of runner orders with prob within 1/e of max probable order
;       PP_NBEAM_MIN           - Threshold for minimum number of class "A" beams
;       PP_MAXORDER_RUNEST     - Maximum runner order runest is allowed to asign to beams
;       PP_MAXORDER            - Maximum runner order used for bestarg
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
;       2015-09-24  -   Written by Matthew Argall. Adapted from ep_prep_order written
;                           Cluster's bestarg.
;-
pro ep_prep_order, $
	nBeams, beam_runstat, status, out, code_type, tchip, tcode, $ ; IN
	n_posord, posord, maxord, beam_class, beam_penalty ; OUT

	common ep_envar_con_cb

	; beam_class(ibeam):
	;    'A' = Definitive order assignment by RunEst
	;          order = RunEst.runorder (non-zero)
	;          iorder = order - 1
	;          RunEst.flag(iorder) = 1
	;          RunEst.prob(iorder) > 1/e^2
	;          RunEst.prob(iorder)^2/Sum(RunEst.prob(other)) > 20
	;          Can be used for TRI and TOF
	;
	;    'B' = Some orders are more probable than others.
	;          Choices of order given by RunEst
	;          Can be used for TRI and TOF
	;
	;    'C' = No order info given by RunEst
	;          COULD be used for TRI or TOF, but not currently (July 2003)
	;
	;    'D' = Beam not ever sent to RunEst (maxchan ne 7, qual lt pp_qual_min,
	;          etc...see ep_prep_runest.pro)
	;          COULD be used for TRI, but not currently (July 2003)
	;          CANNOT be used for TOF (no timing info)!
	;
	;    'F' - Don't ever use this...Reserved for fill...
	;
	;    'G' = No order, and no Tg/Estof information
	;          given by RunEst.  These are 'G'ap beams which
	;          CANNOT BE USED IN TOF because for whatever
	;          reason, a gap in the highres FGM data
	;          prevented RunEst from doing it's job so no
	;          gyrotime, or estimated single runner ToF
	;          could be determined.
	;          COULD be used for TRI, but not currently (July 2003)

	;    'H' = These are discarded beams in the long code that didn't
	;    conform to our expectations;  They are also
	;    assigned an "out" value of 3 (like Class C, D and G) so that they are
	;    not used in any subsequent analysis
	;
	; Method 1: beam_penalty(ibeam):
	;    sum(RunEst Flag) = 0, Class G, Beam not used
	;    sum(RunEst Flag) = 0, Class D, Beam not used
	;    sum(RunEst Flag) = 0, Class C, Beam not used
	;    sum(RunEst Flag) = 6, Class B, beam_penalty(ibeam) = 3
	;    sum(RunEst Flag) = 5, Class B, beam_penalty(ibeam) = 3
	;    sum(RunEst Flag) = 4, Class B, beam_penalty(ibeam) = 3
	;    sum(RunEst Flag) = 3, Class B, beam_penalty(ibeam) = 3
	;    sum(RunEst Flag) = 2, Class B, beam_penalty(ibeam) = 2
	;    sum(RunEst Flag) = 1, Class B, beam_penalty(ibeam) = 1.5
	;    sum(RunEst Flag) = 1, Class A, beam_penalty(ibeam) = 1.0

	; Method 2: beam_penalty(ibeam):  See ep_calc_runner_penalty.pro

	; n_posord(ibeam) = Number of 'pos'sible (probable) 'ord'ers for beam ibeam
	; posord(0:n_posord(ibeam)-1,ibeam) = The 'pos'sible 'ord'ers for beam ibeam

	;==============================================================

	status = 0

	; Who's the biggest:  This will be 1st dimension size of posord
	maxord       = max([pp_maxorder, pp_maxorder_runest])
	n_posord     = lonarr(nBeams)                 ; # of possible orders to try in bestarg.pro
	posord       = lonarr(maxord, nBeams)         ; Which orders to try
	beam_class   = strarr(nBeams)
	beam_penalty = fltarr(nBeams)

	;Extract information
	runorder = reform(beam_runstat[0,*))         ; comes from edi_piso_onechunk_hrbpp:runstat_out->??:runstat1_hrbpp not clear
	runflag  = beam_runstat[1: pp_maxorder_runest, *]
	runtg    = beam_runstat[2*pp_maxorder_runest+1, *]
	runprob  = beam_runstat[2*pp_maxorder_runest+2: 3*pp_maxorder_runest+1, *]

	;Step through each beam
	for ibeam=0,nBeams-1 do begin
		;Runner order assessment -- comes from RunEst
		case runorder[ibeam] of
			;
			; 0 if:
			;   Maximum probability of all runner orders is lower than threshold
			;   Maximum confidence of all runner orders is lower than threshold
			;   Data is in FGM data gap
			;
			0: begin
				;
				;RUNFLAG will be 1 for a particular runner order if the probability
				;of that runner order being true is greater than a threshold limit.
				;FLAGTOT could be anything from 0 to pp_maxorder_runest
				;
				;RUNFLAG
				;   1 Probability of a runner order is greater than threshold.
				;   0 if no runner orders above threshold or point is in FGM data gap.
				; 
				flagtot = total(runflag[*,ibeam])

				;Zero runner orders above threshold, either because:
				;   - No probabilities above threshold
				;   - In FGM data gap
				if (flagtot eq 0) then begin
					
					;CLASS "G"
					;   - Gyroperiod is -1.0 when EDI data in FGM data gaps
					;   - Use for triangulation, but not for time-of-flight
					;
					;   - Later, Decided that 'G' beams shouldn't be used for TRI either
					;     (we need the RunEst probabilities for the rchi2, but they
					;     don't exist for these beams)
					;   - Will not be used for anything
					if runtg[ibeam] eq -1.0 then begin
						;Number of possible orders
						;   - All orders are possible (cannot elminate any)
						;   - NOT pp_maxorder_runest (why??)
						n_posord[ibeam] = pp_maxorder
						posord[0:n_posord[ibeam]-1,ibeam] = lindgen(n_posord[ibeam])+1

						;Define the beam class
						beam_class[ibeam]   = 'G'
						out[ibeam]          = 3  ; Will not be used for anything
						;beam_penalty[ibeam] = ?
					
					;CLASS "C"
					;   - Decided that 'C' beams shouldn't be used for TRI either
					;     (we need the RunEst probabilities for the rchi2, but they
					;     don't exist for these beams)
					endif else begin
						;Number of possible orders
						;   - All orders are possible (cannot elminate any)
						;   - NOT pp_maxorder_runest (why??)
						n_posord[ibeam] = pp_maxorder
						posord[0:n_posord[ibeam]-1,ibeam] = lindgen(n_posord[ibeam])+1

						;Define beam class
						beam_class[ibeam]   = 'C'
						out[ibeam]          = 3
						;beam_penalty(ibeam) = ?
					endelse
				
				;CLASS "B"
				;   - Between 1 and 3 runner orders greater than probability threshold
				endif else if (flagtot ge 1 and flagtot le 3) then begin

					;Pick out the legal runner orders
					n_posord[ibeam] = flagtot
					posord[0: n_posord[ibeam]-1, ibeam] = where(runflag[*, ibeam] eq 1) + 1

					;Classify as class B
					;   - beam_penalty: 1.5 if exactly one runner order
					;                   2-3 if 2 or 3 legal runner orders
					beam_class[ibeam] = 'B'
					if (flagtot eq 1) then $
						then beam_penalty[ibeam] = 1.5 $
						else beam_penalty[ibeam] = float(flagtot)

					;Long code
					;   - If POSORD is NOT 1 (single runner), then throw away beam
					;   - Long code rejected runner orders 2-5 (do not even consider
					;     order >= 6, though it is possible with the long code).
					if (code_type[ibeam] eq 1) then begin
						;We must have a single runner, so check for POSORD==1
						i1 = where(posord[0:n_posord[ibeam]-1,ibeam] eq 1)
						
						;Reject the beam if single runners were not found
						if i1[0] eq -1 then begin
							out[ibeam]        = 3
							beam_class[ibeam] = 'H'
						;Force the beam to have one possible order (the single runner)
						endif else begin
							posord[0:n_posord[ibeam]-1,ibeam = 0 ; Clobber pre-existing assignment
							posord[0,ibeam] = 1 ; Assign as single runner
							n_posord[ibeam] = 1 ; Don't examine other orders
						endelse
					endif
				
				;CLASS "B"
				;   - More than 3 legal runner orders
				;   - Pick three orders with highest probability
				endif else begin
					; Only examine those 3 'legal' orders with the highest probabilities
					n_posord[ibeam] = 3
					id = where(runflag[*,ibeam] eq 1)    ;Find where probability meets threshold
					o  = id + 1                          ;Associated orders
					p  = runprob[id,ibeam]               ;Associated probabilities
					i  = sort(p)                         ;Sort by probability
					p  = reverse(p[i])                   ;Highest probability first
					o  = reverse(o[i])                   ;Sort runner orders accordingly
					o  = o[0:2]                          ;Take 3 most probable orders
					p  = p[0:2]
					i  = sort(o)                         ; Resort -- monotonically increasing required
					posord[0:n_posord[ibeam]-1,ibeam] = o[i]
					beam_class[ibeam]   = 'B'
					beam_penalty[ibeam] = 3.0

					;Special consideration for the long code
					;   - Same process as above
					if (code_type(ibeam) eq 1) then begin
						i1 = where(posord[0:n_posord[ibeam]-1,ibeam] eq 1)
						if (i1[0] eq -1) then begin ; Throw away beam because single-runner not returned as an option
							out[ibeam] = 3 ; Will not be used for anything
							beam_class[ibeam] = 'H'
						endif else begin
							posord[0:n_posord[ibeam]-1,ibeam] = 0 ; Clobber pre-existing assignment
							posord[0,ibeam] = 1 ; Assign as single runner
							n_posord[ibeam] = 1 ; Don't examine other orders
						endelse
					endif
				endelse
			endcase ; case 0

			;Class "D"
			;   - Those beams never sent to Runest (maxchan ne 7, etc...)
			;   - Decided that 'D' beams shouldn't be used for TRI either
			;     (we need the RunEst probabilities for the rchi2, but they
			;     don't exist for these beams)
			pp_bfill: begin
				;Number of possible orders
				;   - All orders are possible (cannot elminate any)
				;   - NOT pp_maxorder_runest (why??)
				n_posord[ibeam]   = pp_maxorder
				posord[0:n_posord[ibeam]-1,ibeam] = lindgen(n_posord[ibeam])+1
				beam_class[ibeam] = 'D'
				out[ibeam]        = 3
				; beam_penalty(ibeam) = ?
			endcase
			
			;CLASS "A"
			;   - Runner order estimate meets probability and confidence thresholds
			else: begin
				;Apparently, there were cases where the runner order was not
				;one of the allowable values.
				if ( (runorder[ibeam] lt 1) or (runorder[ibeam] gt pp_maxorder_runest) ) $
					then message, 'Something wrong with class A beams.'
				
				;Assign class and order
				;   - RUNORDER is the runner order with the highest probability,
				;     so it can be used without considering others.
				n_posord[ibeam]     = 1
				posord[0,ibeam]     = runorder[ibeam]
				beam_class[ibeam]   = 'A'
				beam_penalty[ibeam] = 1.0

				;Special consideration for the long code
				;   - If long code and runorder not equal to 1:
				if (code_type[ibeam] eq 1 and runorder[ibeam] ne 1) then begin
					;Long code rejects runner orders 2-5.
					if (runorder[ibeam] eq 6) then begin
						; Check to see if single and six-fold runners have same apparent TOF
						if (5.0*runtg[ibeam] ge (tcode[ibeam] - 1.5*tchip[ibeam])) and $
						   (5.0*runtg[ibeam] lt (tcode[ibeam] + 1.5*tchip[ibeam])) $
						then begin
							posord[0,ibeam]     = 1   ; Re-assignment as single order
							beam_penalty(ibeam) = 1.5 ; Increased from 1.0 because of ambiguity
						endif ; Otherwise: Do nothing;  keep as 6th-order runner
					endif else begin ; Discard this beam because long code runners that are not single or six-fold are highly improbable
						out[ibeam]        = 3 ; Will not be used for anything
						beam_class[ibeam] = 'H'
					endelse
				endif
			endcase
		endcase
	endfor

	;Runner penalty of 1.0 for all beams (no penalty)
	if (pp_runner_penalty_meth eq 0) then begin
		beam_penalty = fltarr(nBeams) + 1.0
	endif

	;Recalculate runner penalty
	;   - Equal to number of runner orders with probability within 1/e of
	;     the maximum probability.
	if (pp_runner_penalty_meth eq 2) then begin
		ep_calc_runner_penalty, nBeams, runprob, beam_penalty
	endif

	;Check that you still have enough beams!!!
	nbeam = n_elements(where(out le 1))
	if (nbeam lt pp_nbeam_min) then begin
		status = 0
		return
	endif

	status = 1
	return
end
