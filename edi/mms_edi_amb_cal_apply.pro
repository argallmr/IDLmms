; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_cal_apply
;
; PURPOSE:
;+
;   Apply dead-time correction as well as relative and absolute calibration
;   factors to EDI ambient data.
;
; :Params:
;       COUNTS:     in, required, type=lonarr
;                   EDI ambient raw counts.
;       PACK_MODE:  in, required, type=byte
;                   Packing mode of the data.
;       RELCAL:     in, required, type=fltarr
;                   Relative calibration parameter.
;       ABSCAL:     in, required, type=float
;                   Absolute calibration parameter.
;
; :Keywords:
;       BRST:       in, optional, type=boolean, default=0
;                   If set, the accumulation time for burst data will be used.
;       DELTA:      out, optional, type=fltarr/lonarr
;                   Uncertainty in the counts measurement.
;
; :Returns:
;       CNTS:       Corrected, calibrated counts.
;-
function mms_edi_amb_cal_apply, counts, pack_mode, relcal, abscal, $
BRST=brst, $
DELTA=err_tot
	compile_opt idl2
	on_error, 2
	
	;Defaults
	abs_err_pct = 0.2  ;20% error on absolute calibration factor
	tf_brst     = keyword_set(brst)
	tf_abscal   = n_elements(abscal) gt 0

	;
	; Looking up specs in EDI design documents, the EDI sensor deadtime is nominally 200ns,
	; although with a fairly generous error (+/- 50 ns)
	;
	; The accumulation times to be used in the deadtime corrections before 
	; applying the relative calibration are:
	;
	;   Burst: 1/1024 sec (0.9765625 ms)
	;   Survey: 16/1024 sec (15.625 ms)
	;
	; The accumulation time for survey counts is 16/1024 seconds (and 1/1024 
	; for burst). For the cal factor adjustment for survey another factor of
	; two comes from the fact that we are summing up two adjacent anodes.
	; [this comes into play when applying the absolute calibration].
	; 
	; These are the right settings for "amb" survey:
	; 
	;    accumulation time = 16./1024
	;    abscal_counts     = relcal_counts * (abscal_factor / 32)
	; 
	; And for "amb-pm2" survey uses a single pad instead of two, so there we 
	; would use:
	; 
	;    accumulation time = 16./1024
	;    abscal_counts     = relcal_counts * (abscal_factor / 16)
	;
	;
	; Dead-time correction formula:
	;   Ct = Cm / ( 1 -  Cm * dt / Ta)
	;   Cm = measured counts
	;   Ct = true counts
	;   dt = dead time (250 ns)
	;   Ta = accumulation time ( 2^(-10) seconds for EDI ambient mode burst telemetry )
	;
	; Dead-time error formula:
	;   dCm = sqrt(Cm)                          (Laplacian counting error)
	;   dCt = Ct * [ 1/Cm + 1/(Ta/dt + Cm) ]    (derivative of Ct with respect to dt)
	;   dCt = Error in true counts
	;   dCm = Error in measured counts
	;
	; Error Propagation:
	;
	;
	; Assuming the fluxes are expressed as
	; 
	;         F = C * R * A
	; 
	; where
	; 
	;         C = deadtime corrected counts
	;         R = relative calibration factor (flat fielding, angle-dependent)
	;         A = absolute calibration factor
	; 
	; Then
	; 
	;         errF = sqrt[ (R*A)^2 * errC^2  +  (R*C)^2 * errA^2 ]
	; 
	; where
	; 
	;         errC = error in deadtime corrected counts (see below)
	;         errA = 0.15 * A  (15 percent error)
	; 
	; Later on, there will be a third term "(C*A)^2 * errR^2" under the square 
	; root. We are skipping that for now.
	; 
	; The formula for the error of the deadtime corrected counts that Roy 
	; derived is:
	; 
	;         errC = sqrt(X) * [ tA/(tA - X*tD) ]^2
	; 
	; where
	;         X = raw counts
	;         tA = accumulation time (1/1024 sec for burst, 16/1024 sec for survey)
	;         tD = deadtime (200ns)
	;
	
;-----------------------------------------------------
; Dead-Time Correction \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Correct for dead-time
	dt = 200.0e-9  ; in seconds
	if tf_brst $
		then ta = 2.0^(-10) $ ; in seconds
		else ta = 2.0^(-6)    ; in seconds
	
	;Dead-Time Correction
	cnts_DT = counts / (1.0 - counts * (dt/ta) )
	
;-----------------------------------------------------
; Relative Calibrations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Apply relative calibrations
	cnts_rel = cnts_DT * relcal
	
;-----------------------------------------------------
; Absolute Calibrations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Absolute calibrations
	if tf_abscal then begin
		;BRST
		if tf_brst then begin
			cnts_abs = cnts_rel * abscal
		
		;SRVY
		endif else begin
			case pack_mode of
				0: cnts_abs = cnts_rel * (abscal / 32.0)
				1: cnts_abs = cnts_rel * (abscal / 32.0)
				2: cnts_abs = cnts_rel * (abscal / 16.0)
				else: message, 'PACK_MODE (' + string(pack_mode, FORMAT='(i0)') + ') not recognized.'
			endcase
		endelse
	
	;Do not apply calibrations
	endif else begin
		cnts_rel  = fix(round(cnts_rel),  TYPE=12)
	endelse
	
;-----------------------------------------------------
; Errors \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; TODO: Incorporate relative calibration error
	;

	;Raw counts error
	err_Raw = sqrt(counts)

	;Error from dead-time correction formula
	;   - Careful of C = R = 0 case. Should be 1.
	iZero = where(counts eq 0, nZero, COMPLEMENT=iGood, NCOMPLEMENT=nGood)
	dC_dR = fltarr(nZero+nGood)
	if nGood gt 0 then dC_dR[iGood] = cnts_DT[igood] * ( 1.0/counts[iGood] + 1.0/(counts[iGood] + ta/dt) )
	if nZero gt 0 then dC_dR[iZero] = 1.0
	
	;Error in dead-time corrected counts
	err_DT = temporary(err_Raw) * temporary(dC_dR)
	
	;ABSCAL Error
	if tf_abscal then begin
		;Independent errors
		err_abs = abs_err_pct * abscal
		
		;Total error
		err_tot = sqrt( (relcal * abscal)^2  * err_DT + $
		                (relcal * cnts_DT)^2 * err_abs )
	
	;RELCAL Error
	endif else begin
		;Total Error
		err_tot  = temporary(err_Raw) * temporary(err_DT)
		
		;Integer count error
		err_tot = fix(round(err_tot), TYPE=12)
	endelse
	
;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	if tf_abscal $
		then return, cnts_abs $
		else return, cnts_rel
end