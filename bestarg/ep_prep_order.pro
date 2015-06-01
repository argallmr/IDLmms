pro ep_prep_order, $
	nBeams, beam_runstat, status, out, code_type, correlatorChipPeriod, correlatorCodePeriod, $ ; IN
	nBeamPossibleOrders, beamPossibleOrders, maxPossibleOrder, beam_class, beam_penalty ; OUT
	; 2015-01-06: edited for readability, not content
	; Prep the initial multirunner beam order (1,2,3...) and class ('A'..'H')

	; Oct. 2009:
	;Added limitations for:  Long Code (code_type=1)
	; These changes came about because of the order=6 assignments in long
	; code during the time period 00:50:00-01:00:00 on 20031001 on Cluster 1

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

	; nBeamPossibleOrders(ibeam) = Number of 'pos'sible (probable) 'ord'ers for beam ibeam
	; beamPossibleOrders(0:nBeamPossibleOrders(ibeam)-1,ibeam) = The 'pos'sible 'ord'ers for beam ibeam

	;==============================================================

	status = 0

	; Who's the biggest:  This will be 1st dimension size of beamPossibleOrders
	maxPossibleOrder           = max ([pp_maxorder, pp_maxorder_runest])
	nBeamPossibleOrders= lonarr(nBeams)                       ; # of possible orders to try in bestarg.pro
	beamPossibleOrders = lonarr(maxPossibleOrder, nBeams)             ; Which orders to try
	beam_class         = strarr(nBeams)
	beam_penalty       = fltarr(nBeams)

	runorder = reform (beam_runstat(0,*))         ; comes from edi_piso_onechunk_hrbpp:runstat_out->??:runstat1_hrbpp not clear
	runflag  = beam_runstat (1: pp_maxorder_runest, *)
	runtg    = beam_runstat (2*pp_maxorder_runest+1, *)
	runprob  = beam_runstat (2*pp_maxorder_runest+2: 3*pp_maxorder_runest+1, *)

	for ibeam=0,nBeams-1 do begin
		case runorder(ibeam) of
			0: begin                 ; HAV order assignment tried but not definitive
				; flagtot could be anything from 0 to pp_maxorder_runest
				flagtot = total(runflag(*,ibeam))

				if (flagtot eq 0) then begin
					if (runtg(ibeam) ne -1.0) then begin ;  no 'G'ap problem
						nBeamPossibleOrders(ibeam) = pp_maxorder ;NOTE:not pp_maxorder_runest
						beamPossibleOrders(0:nBeamPossibleOrders(ibeam)-1,ibeam) = lindgen(nBeamPossibleOrders(ibeam))+1

						beam_class(ibeam) = 'C'
						; Decided that 'C' beams shouldn't be used for TRI either
						; (we need the RunEst probabilities for the rchi2, but they
						; don't exist for these beams)
						out(ibeam) = 3  ; Will not be used for anything

						; beam_penalty(ibeam) = ?
					endif 				else begin
						; Runest suffers from 'G'ap in highres FGM data...This beam class is
						; 'G' which is OK for TRI, but NEVER TO BE USED FOR TOF!!!
						nBeamPossibleOrders(ibeam) = pp_maxorder
						beamPossibleOrders(0:nBeamPossibleOrders(ibeam)-1,ibeam) = lindgen(nBeamPossibleOrders(ibeam))+1

						beam_class(ibeam) = 'G'
						; Decided that 'G' beams shouldn't be used for TRI either
						; (we need the RunEst probabilities for the rchi2, but they
						; don't exist for these beams)
						out (ibeam) = 3  ; Will not be used for anything
						; beam_penalty (ibeam) = ?
					endelse
				endif $
				else $
					if (flagtot ge 1 and flagtot le 3) then begin

						; Allow examination of these 1-3 legal orders
						nBeamPossibleOrders (ibeam) = flagtot
						beamPossibleOrders (0: nBeamPossibleOrders (ibeam)-1, ibeam) = where (runflag (*, ibeam) eq 1) + 1

						beam_class (ibeam) = 'B'
						if (flagtot eq 1) then $
							beam_penalty (ibeam) = 1.5 $
						else $
							beam_penalty (ibeam) = float(flagtot)

						; Special consideration for the long code
						if (code_type(ibeam) eq 1) then begin
							i1 = where(beamPossibleOrders(0:nBeamPossibleOrders(ibeam)-1,ibeam) eq 1)
							if (i1(0) eq -1) then begin ; Throw away beam because single-runner not returned as an option
								out(ibeam) = 3 ; Will not be used for anything
								beam_class(ibeam) = 'H'
							endif $
							else begin
								beamPossibleOrders(0:nBeamPossibleOrders(ibeam)-1,ibeam) = 0 ; Clobber pre-existing assignment
								beamPossibleOrders(0,ibeam) = 1 ; Assign as single runner
								nBeamPossibleOrders(ibeam) = 1 ; Don't examine other orders
							endelse
						endif
					endif $
					else begin
						; Only examine those 3 'legal' orders with the highest probabilities
						nBeamPossibleOrders(ibeam) = 3
						id = where (runflag (*,ibeam) eq 1)
						o = id + 1      ; orders
						p = runprob(id,ibeam) ; probabilities
						i = sort(p)
						o = reverse(o(i))
						p = reverse(p(i))
						o = o(0:2)
						p = p(0:2)
						i = sort(o)     ; Monotonically increasing required
						beamPossibleOrders(0:nBeamPossibleOrders(ibeam)-1,ibeam) = o(i)
						beam_class(ibeam) = 'B'
						beam_penalty(ibeam) = 3.0

						; Special consideration for the long code
						if (code_type(ibeam) eq 1) then begin
							i1 = where(beamPossibleOrders(0:nBeamPossibleOrders(ibeam)-1,ibeam) eq 1)
							if (i1(0) eq -1) then begin ; Throw away beam because single-runner not returned as an option
								out(ibeam) = 3 ; Will not be used for anything
								beam_class(ibeam) = 'H'
							endif $
							else begin
								beamPossibleOrders(0:nBeamPossibleOrders(ibeam)-1,ibeam) = 0 ; Clobber pre-existing assignment
								beamPossibleOrders(0,ibeam) = 1 ; Assign as single runner
								nBeamPossibleOrders(ibeam) = 1 ; Don't examine other orders
							endelse
						endif
					endelse
			end ; case 0

			; Those beams never sent to Runest (maxchan ne 7, etc...)
			pp_bfill: begin
				nBeamPossibleOrders(ibeam) = pp_maxorder
				beamPossibleOrders(0:nBeamPossibleOrders(ibeam)-1,ibeam) = lindgen(nBeamPossibleOrders(ibeam))+1
				beam_class(ibeam) = 'D'
				; Decided that 'D' beams shouldn't be used for TRI either
				; (we need the RunEst probabilities for the rchi2, but they
				; don't exist for these beams)
				out(ibeam) = 3          ; Will not be used for anything
				; beam_penalty(ibeam) = ?
			end

			else: begin              ; HAV order assignment definitive
				if ( (runorder(ibeam) lt 1) or (runorder(ibeam) gt pp_maxorder_runest) ) then $
					message, 'Something wrong'
				nBeamPossibleOrders(ibeam) = 1
				beamPossibleOrders(0,ibeam) = runorder(ibeam)
				beam_class(ibeam) = 'A'
				beam_penalty(ibeam) = 1.0

				; Special consideration for the long code
				; If long code and runorder not equal to 1:
				if (code_type(ibeam) eq 1 and runorder(ibeam) ne 1) then begin
					if (runorder(ibeam) eq 6) then begin
						; Check to see if single and six-fold runners have same apparent TOF
						if (5.*runtg(ibeam) ge (correlatorCodePeriod(ibeam) - 1.5*correlatorChipPeriod(ibeam)) and $
								5.*runtg(ibeam) lt (correlatorCodePeriod(ibeam) + 1.5*correlatorChipPeriod(ibeam))) then begin
							beamPossibleOrders(0,ibeam) = 1 ; Re-assignment as single order
							beam_penalty(ibeam) = 1.5 ; Increased from 1.0 because of ambiguity
						endif ; Otherwise: Do nothing;  keep as 6th-order runner
					endif $
					else begin ; Discard this beam because long code runners that are not single or six-fold are highly improbable
						out(ibeam) = 3 ; Will not be used for anything
						beam_class(ibeam) = 'H'
					endelse
				endif
			end
		endcase
	endfor

	if (pp_runner_penalty_meth eq 0) then begin
		beam_penalty = fltarr(nBeams) + 1.0 ; Reduces to no penalty at all
	endif

	if (pp_runner_penalty_meth eq 2) then begin
		ep_calc_runner_penalty, $
			nBeams, runprob, $ ; IN
			beam_penalty              ; OUT
	endif

	; Check that you still have enough beams!!!
	nbeam = n_elements(where(out le 1))
	if (nbeam lt pp_nbeam_min) then begin ; Fatal, not enough beams after prepping order assignment
		status = 0
		return
	endif

	status = 1
	return
end
