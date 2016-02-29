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
;                        COUNTS1_GDU1  -  Calibrated counts1 data from GDU1
;                        COUNTS2_GDU1  -  Calibrated counts2 data from GDU1
;                        COUNTS3_GDU1  -  Calibrated counts3 data from GDU1
;                        COUNTS4_GDU1  -  Calibrated counts4 data from GDU1
;                        COUNTS1_GDU2  -  Calibrated counts1 data from GDU2
;                        COUNTS2_GDU2  -  Calibrated counts2 data from GDU2
;                        COUNTS3_GDU2  -  Calibrated counts3 data from GDU2
;                        COUNTS4_GDU2  -  Calibrated counts4 data from GDU2
;                        DELTA1_GDU1   -  Errors in counts1 for GDU1
;                        DELTA2_GDU1   -  Errors in counts2 for GDU1
;                        DELTA3_GDU1   -  Errors in counts3 for GDU1
;                        DELTA4_GDU1   -  Errors in counts4 for GDU1
;                        DELTA1_GDU2   -  Errors in counts1 for GDU2
;                        DELTA2_GDU2   -  Errors in counts2 for GDU2
;                        DELTA3_GDU2   -  Errors in counts3 for GDU2
;                        DELTA4_GDU2   -  Errors in counts4 for GDU2
;-
;*****************************************************************************************
;+
;
;-
function mms_edi_amb_calibrate, edi, cals, $
BRST=brst, $
ABSCAL=abscal
	compile_opt idl2
	on_error, 2

	;Make sure the packing mode is not changing
	if ~array_equal(edi.pack_mode, edi.pack_mode[0]) then begin
		;BRST
		if MrStruct_HasTag(edi, 'COUNTS4_GDU1') then begin
			if nPacMo ne nPacMo01 then message, 'Packing mode is changing.'
		
		;SRVY
		endif else begin
			;PACK_MODE 0 and 1 are identical
			nPacMo     = n_elements(edi.pack_mode)
			tf_pacmo01 = MrIsMember([0B,1B], edi.pack_mode, COUNT=nPacMo01)
			if nPacMo ne nPacMo01 then message, 'Packing mode is changing.'
		endelse
	endif
	
	;Check for fill values
	ncts1      = float(n_elements(edi.epoch_gdu1))
	ncts2      = float(n_elements(edi.epoch_gdu2))
	ifill_gdu1 = where(edi.counts1_gdu1 eq 65535US, nfill_gdu1)
	ifill_gdu2 = where(edi.counts1_gdu2 eq 65535US, nfill_gdu2)
	if nfill_gdu1 gt 0 then MrPrintF, 'LogWarn', nfill_gdu1, nfill_gdu1/ncts1*100.0, FORMAT='(%"counts1_gdu1 has %i (%0.2f\%) fill values.")'
	if nfill_gdu2 gt 0 then MrPrintF, 'LogWarn', nfill_gdu2, nfill_gdu2/ncts2*100.0, FORMAT='(%"counts1_gdu2 has %i (%0.2f\%) fill values.")'

	;Flags
	tf_brst   = keyword_set(brst)
	tf_relcal = 1
	tf_abscal = keyword_set(abscal)

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
		tf_abscal = 0
	endif
	
;------------------------------------------------------
; Angle Look-Up                                       |
;------------------------------------------------------
	;
	; PACK_MODE 1
	;   channel    anode#      azimuth of look direction
	;   ---------------------------------------------------
	;   counts1      p-2       (p-1.5) * 11.25 deg
	;   counts2      p-1       (p-0.5) * 11.25 deg         } B points between
	;   counts3      p         (p+0.5) * 11.25 deg         } these two anodes
	;   counts4      p+1       (p+1.5) * 11.25 deg
	;
	; PACK_MODE 2
	;   channel    anode#      azimuth of look direction
	;   ---------------------------------------------------
	;   counts1      p         (p+0.5) * 11.25 deg
	;   counts2      p-1       (p-0.5) * 11.25 deg
	;   counts3      p-2       (p-1.5) * 11.25 deg
	;   counts4      p-3       (p-2.5) * 11.25 deg
	;
	
	;Locate the look-angles within the angle tables
	itheta = value_locate(cals.theta, edi.polar)

	;PACK_MODE = 0
	;   - srvy -- identical to PACK_MODE 1
	;   - brst -- should not exist
	if edi.pack_mode[0] eq 0 then begin
		if tf_brst then message, 'Encountered PACK_MODE=0 in brst mode'
		iphi3 = value_locate(cals.phi, edi.azimuth)
	
	;PACK_MODE = 1
	endif else if edi.pack_mode[0] eq 1 then begin
		;Channel for each counts[1-4]
		iphi3 = value_locate(cals.phi, edi.azimuth)
		iphi1 = iphi3 - 2
		iphi2 = iphi3 - 1
		iphi4 = iphi3 + 1
		
		;Wrap around channel value if < 0 or > 32
		iphi1 += (iphi1 lt 0) * 32
		iphi2 += (iphi2 lt 0) * 32
		iphi4 mod= 32
		
	;PACK_MODE = 2
	endif else if edi.pack_mode[0] eq 2 then begin
		iphi1 = value_locate(cals.phi, edi.azimuth)
		iphi2 = iphi1 - 1
		iphi3 = iphi1 - 2
		iphi4 = iphi1 - 3
		
		;Wrap around channel value if < 0 or > 32
		iphi2 += (iphi2 lt 0) * 32
		iphi3 += (iphi3 lt 0) * 32
		iphi4 += (iphi4 lt 0) * 32
	endif else begin
		message, 'PACK_MODE ' + strtrim(edi.pack_mode[0], 2) + ' not recognized.'
	endelse
	
	;Reduce for srvy mode
	if ~tf_brst then begin
		case edi.pack_mode[0] of
			0: iphi1 = temporary(iphi3)
			1: iphi1 = temporary(iphi3)
			2: ;Use iphi1 as calculated above
			else: message, 'PACK_MODE ' + string(edi.pack_mode[0], FORMAT='(i0)', /PRINT) + ' not recognized.'
		endcase
		
		;Junk data in srvy mode
		iphi2 = !Null
		iphi3 = !Null
		iphi4 = !Null
	endif

;------------------------------------------------------
; Calibrate                                           |
;------------------------------------------------------
	;Counts1 GDU1
	c1_gdu1 = mms_edi_amb_cal_apply(edi.counts1_gdu1, cals.relcal_gdu1[itheta, iphi1, irel_gdu1], cals.abscal_gdu1[iabs_gdu1], $
	                                BRST=brst, DELTA=delta1_gdu1)
	
	;Counts1 GDU2
	c1_gdu2 = mms_edi_amb_cal_apply(edi.counts1_gdu2, cals.relcal_gdu2[itheta, iphi1, irel_gdu2], cals.abscal_gdu2[iabs_gdu2], $
	                                BRST=brst, DELTA=delta1_gdu2)
	
	;Burst
	if keyword_set(brst) then begin
		;GDU1 
		c2_gdu1 = mms_edi_amb_cal_apply(edi.counts2_gdu1, cals.relcal_gdu1[itheta, iphi2, irel_gdu1], cals.abscal_gdu1[iabs_gdu1], $
		                                /BRST, DELTA=delta2_gdu1)
		c3_gdu1 = mms_edi_amb_cal_apply(edi.counts3_gdu1, cals.relcal_gdu1[itheta, iphi3, irel_gdu1], cals.abscal_gdu1[iabs_gdu1], $
		                                /BRST, DELTA=delta3_gdu1)
		c4_gdu1 = mms_edi_amb_cal_apply(edi.counts4_gdu1, cals.relcal_gdu1[itheta, iphi4, irel_gdu1], cals.abscal_gdu1[iabs_gdu1], $
		                                /BRST, DELTA=delta4_gdu1)
		
		;GDU2
		c2_gdu2 = mms_edi_amb_cal_apply(edi.counts2_gdu2, cals.relcal_gdu2[itheta, iphi2, irel_gdu2], cals.abscal_gdu2[iabs_gdu2], $
		                                /BRST, DELTA=delta2_gdu2)
		c3_gdu2 = mms_edi_amb_cal_apply(edi.counts3_gdu2, cals.relcal_gdu2[itheta, iphi3, irel_gdu2], cals.abscal_gdu2[iabs_gdu2], $
		                                /BRST, DELTA=delta3_gdu2)
		c4_gdu2 = mms_edi_amb_cal_apply(edi.counts4_gdu2, cals.relcal_gdu2[itheta, iphi4, irel_gdu2], cals.abscal_gdu2[iabs_gdu2], $
		                                /BRST, DELTA=delta4_gdu2)
	endif
	
;------------------------------------------------------
; Replace Fillvals                                    |
;------------------------------------------------------
	
	;Which fill value?
	fillval = tf_abscal ? -1e31 : 65535US
	
	;Counts1 GDU1
	if nfill_gdu1 gt 0 then begin
		c1_gdu1[ifill_gdu1]     = fillval
		delta1_gdu1[ifill_gdu1] = fillval
	endif
	
	;c1 GDU2
	if nfill_gdu2 gt 0 then begin
		c1_gdu2[ifill_gdu2]     = fillval
		delta1_gdu2[ifill_gdu2] = fillval
	endif
	
	;GDU1
	if tf_brst && nfill_gdu1 gt 0 then begin
		;Counts
		c2_gdu1[ifill_gdu1] = fillval
		c3_gdu1[ifill_gdu1] = fillval
		c4_gdu1[ifill_gdu1] = fillval
		
		;Errors
		delta2_gdu1[ifill_gdu1] = fillval
		delta3_gdu1[ifill_gdu1] = fillval
		delta4_gdu1[ifill_gdu1] = fillval
	endif
	
	;GDU2
	if tf_brst && nfill_gdu2 gt 0 then begin
		;Counts
		c2_gdu2[ifill_gdu2] = fillval
		c3_gdu2[ifill_gdu2] = fillval
		c4_gdu2[ifill_gdu2] = fillval
		
		;Errors
		delta2_gdu2[ifill_gdu2] = fillval
		delta3_gdu2[ifill_gdu2] = fillval
		delta4_gdu2[ifill_gdu2] = fillval
	endif

;------------------------------------------------------
; Return                                              |
;------------------------------------------------------
	cal_cnts = { counts1_gdu1: temporary(c1_gdu1), $
	             counts1_gdu2: temporary(c1_gdu2), $
	             delta1_gdu1:  temporary(delta1_gdu1), $
	             delta1_gdu2:  temporary(delta1_gdu2) $
	           }
	
	;Structure of calibrated counts
	if tf_brst then begin
		cal_cnts = create_struct( cal_cnts, $
			'counts2_gdu1', temporary(c2_gdu1), $
			'counts3_gdu1', temporary(c3_gdu1), $
			'counts4_gdu1', temporary(c4_gdu1), $
		
			'counts2_gdu2', temporary(c2_gdu2), $
			'counts3_gdu2', temporary(c3_gdu2), $
			'counts4_gdu2', temporary(c4_gdu2), $
		
			'delta2_gdu1',  temporary(delta2_gdu1), $
			'delta3_gdu1',  temporary(delta3_gdu1), $
			'delta4_gdu1',  temporary(delta4_gdu1), $
		
			'delta2_gdu2',  temporary(delta2_gdu2), $
			'delta3_gdu2',  temporary(delta3_gdu2), $
			'delta4_gdu2',  temporary(delta4_gdu2) )
	endif

	return, cal_cnts
end
