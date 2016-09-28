; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_srvy_calibrate
;
; PURPOSE:
;+
;   Apply calibration parameters to EDI data.
;
; :Params:
;       EDI:        in, required, type=struct
;                   Structure of edi raw counts to be calibrated.
;       CALS:       in, required, type=string
;                   Structure of edi calibration parameters.
;
; :Keywords:
;       ABSCAL:     in, optional, type=boolean, default=0
;                   If set, absolute calibration factors will be applied to the data.
;
; :Returns:
;       EDI_OUT:     A structure with the following tags:
;                        COUNTS_GDU1  -  [time,channel] Calibrated counts data from GDU1
;                        COUNTS_GDU2  -  [time,channel] Calibrated counts data from GDU2
;                        DELTA_GDU1   -  [time,channel] Errors in counts for GDU1
;                        DELTA_GDU2   -  [time,channel] Errors in counts for GDU2
;-
;*****************************************************************************************
;+
;
;-
function mms_edi_amb_calibrate, edi, cals, anodes, bitmask, $
ABSCAL=abscal
	compile_opt idl2
	on_error, 2

	;Check for fill values
	ncts1      = float(n_elements(edi.epoch_gdu1))
	ncts2      = float(n_elements(edi.epoch_gdu2))
	ifill_gdu1 = where(edi.counts_gdu1 eq 65535US, nfill_gdu1)
	ifill_gdu2 = where(edi.counts_gdu2 eq 65535US, nfill_gdu2)
	if nfill_gdu1 gt 0 then MrPrintF, 'LogWarn', nfill_gdu1, nfill_gdu1/ncts1*100.0, FORMAT='(%"counts1_gdu1 has %i (%0.2f\%) fill values.")'
	if nfill_gdu2 gt 0 then MrPrintF, 'LogWarn', nfill_gdu2, nfill_gdu2/ncts2*100.0, FORMAT='(%"counts1_gdu2 has %i (%0.2f\%) fill values.")'

	;Flags
	tf_relcal = 1
	tf_abscal = keyword_set(abscal)
	
	;Angular widths
	dPhi      = 360.0 / 32
	dTheta    = 360.0 / 512
	tf_brst   = MrDim(anodes.n_gdu1, 2) gt 0
	nChannels = tf_brst ? 4 : 1
	type      = tf_abscal ? 4 : 13  ; float | ulong
	fillval   = tf_abscal ? -1e31 : 4294967295UL

;------------------------------------------------------
; Locate Nearest Calibration Set                      |
;------------------------------------------------------
	;Relative
	irel_gdu1 = value_locate(cals.tt2000_rel, edi.epoch_gdu1)
	irel_gdu2 = value_locate(cals.tt2000_rel, edi.epoch_gdu2)
	if min(irel_gdu1) eq -1 then begin
		MrPrintF, 'LogErr', 'No relative calibrations for GDU1'
		tf_relcal = 0
	endif
	if min(irel_gdu2) eq -1 then begin
		MrPrintF, 'LogErr', 'No relative calibrations for GDU2'
		tf_relcal = 0
	endif
	
	;Absolute calibrations first require relative calibrations
	if tf_abscal && ~tf_relcal then message, 'No relative cal data. Cannot apply absolute cals.'
	
	;Absolute
	if tf_abscal then begin
		iabs_gdu1 = value_locate(cals.tt2000_abs, edi.epoch_gdu1)
		iabs_gdu2 = value_locate(cals.tt2000_abs, edi.epoch_gdu2)
		if min(iabs_gdu1) eq -1 then message, 'No absolute calibrations for GDU1'
		if min(iabs_gdu2) eq -1 then message, 'No absolute calibrations for GDU2'
	endif
	
;------------------------------------------------------
; Calibrate                                           |
;------------------------------------------------------
	;Get the absolute calibration factors
	if tf_abscal then begin
		abscal_gdu1 = cals.abscal_gdu1[iabs_gdu1]
		abscal_gdu2 = cals.abscal_gdu2[iabs_gdu2]
	endif
	
	;Polar index
	iTheta = fix( round(edi.polar / dTheta), TYPE=12 )
	
	;One sided
	;   - [0,180] are one-sided when PACK_MODE=2 (bit 2^4)
	;   - 90 is one-sided when PERP_ONESIDE=1 (bit 2^5)
	tf_onesided_gdu1 = ( ((edi.pitch_gdu1 eq 0) or (edi.pitch_gdu1 eq 180)) and MrBitSet(bitmask, 3) ) or $
	                   ( (edi.pitch_gdu1 eq 90) and ((bitmask and 2^5) ne 0) )
	tf_onesided_gdu2 = ( ((edi.pitch_gdu2 eq 0) or (edi.pitch_gdu2 eq 180)) and MrBitSet(bitmask, 5) ) or $
	                   ( (edi.pitch_gdu2 eq 90) and MrBitSet(bitmask, 6) )

	;Allocate memory
	counts_gdu1 = make_array(ncts1, nChannels, TYPE=type)
	counts_gdu2 = make_array(ncts2, nChannels, TYPE=type)
	delta_gdu1  = make_array(ncts1, nChannels, TYPE=type)
	delta_gdu2  = make_array(ncts2, nChannels, TYPE=type)

	;Calibrate each channel
	for i = 0, nChannels - 1 do begin
		;Counts1 GDU1
		counts_gdu1[*,i] = mms_edi_amb_cal_apply( edi.counts_gdu1[*,i], $
		                                          tf_onesided_gdu1, $
		                                          cals.relcal_gdu1[itheta, anodes.n_gdu1[*,i], irel_gdu1], $
		                                          abscal_gdu1, $
		                                          BRST=tf_brst, DELTA=dtemp1 )
		
		;Counts1 GDU2
		counts_gdu2[*,i] = mms_edi_amb_cal_apply( edi.counts_gdu2[*,i], $
		                                          tf_onesided_gdu2, $
		                                          cals.relcal_gdu2[itheta, anodes.n_gdu2[*,i], irel_gdu2], $
		                                          abscal_gdu2, $
		                                          BRST=tf_brst, DELTA=dtemp2 )
		
		;Deltas
		delta_gdu1[*,i] = temporary(dtemp1)
		delta_gdu2[*,i] = temporary(dtemp2)
	endfor

;------------------------------------------------------
; Replace Fillvals                                    |
;------------------------------------------------------
	
	;Counts1 GDU1
	if nfill_gdu1 gt 0 then begin
		counts_gdu1[ifill_gdu1,*] = fillval
		delta_gdu1[ifill_gdu1,*]  = fillval
	endif
	
	;c1 GDU2
	if nfill_gdu2 gt 0 then begin
		counts_gdu2[ifill_gdu2,*] = fillval
		delta_gdu2[ifill_gdu2,*]  = fillval
	endif

;------------------------------------------------------
; Return                                              |
;------------------------------------------------------
	;Create structure
	cal_cnts = { counts_gdu1: temporary(counts_gdu1), $
	             counts_gdu2: temporary(counts_gdu2), $
	             delta_gdu1:  temporary(delta_gdu1), $
	             delta_gdu2:  temporary(delta_gdu2) $
	           }
	return, cal_cnts
end
