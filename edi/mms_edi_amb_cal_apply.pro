; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_cal_apply
;
; PURPOSE:
;+
;   Apply dead-time correction and calibration parameters to EDI data.
;
; :Params:
;       COUNTS:     in, required, type=lonarr
;                   EDI ambient raw counts.
;       RELCAL:     in, required, type=129x32xN float
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
function mms_edi_amb_cal_apply, counts, relcal, abscal, $
BRST=brst, $
DELTA=delta
	compile_opt idl2
	on_error, 2
	
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
	;
	; Dead-time correction formula:
	;   Ct = Cm / ( 1 -  Cm * dt / Ta)
	;   Cm = measured counts
	;   Ct = true counts
	;   dt = dead time (250 ns)
	;   Ta = accumulation time ( 2^(-10) seconds for EDI ambient mode burst telemetry )
	;
	; Error:
	;   Laplacian statistics: dCm = sqrt(Cm)
	;   From dead-time correction
	;      dCt = d/dCm (Ct)
	;          = Ct * [ 1/Cm + 1/(Ta/dt - Cm) ]
	;
	;   The uncertainty in the function R with respect to the variable X is:
	;
	;      dR = sqrt[ ( dR/dX DX )^2 ]
	;
	;   where d/dX is the partial derivative and DX is the uncertainty in X. This
	;   means that the total error in counts is
	;
	;      dCm = dCt * dCm
	;          = Ct * sqrt(Cm) * [ 1/Cm + 1/(Ta/dt - Cm) ]
	;
	
	;Correct for dead-time
	dt = 200.0e-9  ; in seconds
	if keyword_set(brst) $
		then ta = 2.0^(-6) $ ; in seconds
		else ta = 2.0^(-10)  ; in seconds
	
	;Dead-Time Correction
	cnts = counts / (1.0 - counts * dt / ta)
	
	;Errors
	delta = cnts * sqrt(counts) * ( 1.0/counts + 1.0/(ta/dt - counts) )
	
	;Apply relative calibrations
	cnts *= relcal
	
	;
	; TODO: apply relative and absolute calibration errors
	;
	
	;Absolute calibrations
	if n_elements(abscal) gt 0 then begin
		cnts *= abscal
	endif else begin
		cnts  = fix(round(cnts),  TYPE=12)
		delta = fix(round(delta), TYPE=12)
	endelse
	
	return, cnts
end