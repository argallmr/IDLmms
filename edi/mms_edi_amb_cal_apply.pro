; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_cal_apply
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
;       BRST:       in, optional, type=boolean, default=0
;                   If set, brst-mode data will be processed. This involves calibrating
;                       channels 2, 3, and 4.
;
; :Returns:
;       EDI_OUT:     A structure with the following tags::
;-
function mms_edi_amb_cal_apply, edi, cals, $
ABSCAL=abscal, $
BRST=brst
	compile_opt idl2
	on_error, 2

	;Make sure the packing mode is not changing
	if max(edi.pack_mode ne edi.pack_mode[0]) $
		then message, 'Packing mode is changing. How do I deal with this?'
	
	;Check for fill values
	ncts1      = float(n_elements(edi.epoch_gdu1))
	ncts2      = float(n_elements(edi.epoch_gdu2))
	ifill_gdu1 = where(edi.counts1_gdu1 eq 65535US, nfill_gdu1)
	ifill_gdu2 = where(edi.counts1_gdu2 eq 65535US, nfill_gdu2)
	if nfill_gdu1 gt 0 then MrPrintF, 'LogWarn', nfill_gdu1, nfill_gdu1/ncts1*100.0, FORMAT='(%"counts1_gdu1 has %i (%0.2f\%) fill values.")'
	if nfill_gdu2 gt 0 then MrPrintF, 'LogWarn', nfill_gdu2, nfill_gdu2/ncts2*100.0, FORMAT='(%"counts1_gdu2 has %i (%0.2f\%) fill values.")'

	;Flags
	tf_relcal = 1
	tf_abscal = keyword_set(abscal)
	tf_brst   = keyword_set(brst)
	
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
	if min(irel_gdu1) eq -1 then begin
		MrPrintF, 'LogErr', 'No relative calibrations for GDU2'
		tf_relcal = 0
	endif
	
	;Absolute calibrations first require relative calibrations
	if tf_abscal && ~tf_relcal then message, 'No relative cal data. Cannot apply absolute cals.'
	
	;Absolute
	if tf_abscal then begin
		iabs_gdu1 = value_locate(cals.tt2000_abs, edi.epoch_gdu1)
		iabs_gdu2 = value_locate(cals.tt2000_abs, edi.epoch_gdu2)
		if min(irel_gdu1) eq -1 then message, 'No absolute calibrations for GDU1'
		if min(irel_gdu1) eq -1 then message, 'No absolute calibrations for GDU2'
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
	;   - Srvy mode -- identical to PACK_MODE 1
	;   - Brst mode -- not used, error thrown below
	if edi.pack_mode[0] eq 0 then begin
		;Channel for each counts[1-4]
		iphi3 = value_locate(cals.phi, edi.azimuth)
		iphi1 = iphi3 - 2
		iphi2 = iphi3 - 1
		iphi4 = iphi3 + 1
		
		;Wrap around channel value if < 0 or > 32
		iphi1 += (iphi1 lt 0) * 32
		iphi2 += (iphi2 lt 0) * 32
		iphi4 mod= 32
	
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
	
	;Brst data
	;   - PACK_MODE=0: Never used
	if tf_brst then begin
		if edi.pack_mode[0] eq 0 then message, 'PACK_MODE=0 found in burst data.'
	
	;Srvy data
	;   - Uses only counts1
	;   - PACK_MODE=0: Identical to PACK_MODE=1
	;   - PACK_MODE=1: Counts1 is an average of channels 2 and 3
	;   - PACK_MODE=2: Counts1 is from channel 1
	endif else  begin
		case edi.pack_mode[0] of
			0: iphi1 = temporary(iphi3)
			1: iphi1 = temporary(iphi3)
			2: ;Do nothing
			else: message, 'PACK_MODE ' + strtrim(edi.pack_mode[0], 2) + ' not recognized.'
		endcase
	endelse

;------------------------------------------------------
; Calibrate Data                                      |
;------------------------------------------------------
	;
	; The deadtime is 200ns (not 250ns), although with a fairly generous error 
	; (200 +/- 50 ns)
	;
	; For slow/fast survey data the accumulation time would be 2^(-6) seconds 
	; (15.625 ms).
	;

	;
	; Dead-time correction formula:
	;   Ct = Cm / ( 1 -  Cm * dt / Ta)
	;   Cm = measured counts
	;   Ct = true counts
	;   dt = dead time (250 ns)
	;   Ta = accumulation time ( 2^(-10) seconds for EDI ambient mode burst telemetry )
	;
	
	;Correct for dead-time
	dt = 250.0e-9  ; in seconds
	ta = 2.0^(-10) ; in seconds
	counts1_gdu1_cal = edi.counts1_gdu1 / (1.0 - edi.counts1_gdu1 * dt / ta)
	counts1_gdu2_cal = edi.counts1_gdu2 / (1.0 - edi.counts1_gdu2 * dt / ta)
	
	;Apply Relative Cals
	if tf_relcal then begin
		counts1_gdu1_cal *= cals.relcal_gdu1[itheta, iphi1, irel_gdu1]
		counts1_gdu2_cal *= cals.relcal_gdu2[itheta, iphi1, irel_gdu2]
	endif
	
	;Apply Absolute Cals
	if tf_abscal then begin
		counts1_gdu1_cal *= cals.relcal_gdu1[itheta, iphi1, iabs_gdu1]
		counts1_gdu2_cal *= cals.relcal_gdu2[itheta, iphi1, iabs_gdu2]
	endif else begin
		counts1_gdu1_cal = fix(round(counts1_gdu1_cal), TYPE=12)
		counts1_gdu2_cal = fix(round(counts1_gdu2_cal), TYPE=12)
	endelse
	
	;Fill values: GDU1
	if nfill_gdu1 gt 0 then begin
		if tf_abscal $
			then counts1_gdu1_cal[ifill_gdu1] = -1e31 $
			else counts1_gdu1_cal[ifill_gdu1] = 65535US
	endif
	
	;Fill values: GDU1
	if nfill_gdu2 gt 0 then begin
		if tf_abscal $
			then counts1_gdu2_cal[ifill_gdu2] = -1e31 $
			else counts1_gdu2_cal[ifill_gdu2] = 65535US
	endif

;------------------------------------------------------
; Burst Mode                                          |
;------------------------------------------------------
	if tf_brst then begin
		;Dead-time correction
		counts2_gdu1_cal = edi.counts2_gdu1 / (1 - edi.counts2_gdu1 * dt / ta)
		counts3_gdu1_cal = edi.counts3_gdu1 / (1 - edi.counts3_gdu1 * dt / ta)
		counts4_gdu1_cal = edi.counts4_gdu1 / (1 - edi.counts4_gdu1 * dt / ta)
		counts2_gdu2_cal = edi.counts2_gdu2 / (1 - edi.counts2_gdu2 * dt / ta)
		counts3_gdu2_cal = edi.counts3_gdu2 / (1 - edi.counts3_gdu2 * dt / ta)
		counts4_gdu2_cal = edi.counts4_gdu2 / (1 - edi.counts4_gdu2 * dt / ta)
	
		;Relative Calibrations
		if tf_relcal then begin
			counts2_gdu1_cal *= cals.relcal_gdu1[itheta, iphi2, irel_gdu1]
			counts3_gdu1_cal *= cals.relcal_gdu1[itheta, iphi3, irel_gdu1]
			counts4_gdu1_cal *= cals.relcal_gdu1[itheta, iphi4, irel_gdu1]
			counts2_gdu2_cal *= cals.relcal_gdu2[itheta, iphi2, irel_gdu2]
			counts3_gdu2_cal *= cals.relcal_gdu2[itheta, iphi3, irel_gdu2]
			counts4_gdu2_cal *= cals.relcal_gdu2[itheta, iphi4, irel_gdu2]
		endif
		
		;Absolute Calibrations
		if tf_abscal then begin
			counts2_gdu1_cal *= cals.relcal_gdu1[itheta, iphi2, iabs_gdu1]
			counts3_gdu1_cal *= cals.relcal_gdu1[itheta, iphi3, iabs_gdu1]
			counts4_gdu1_cal *= cals.relcal_gdu1[itheta, iphi4, iabs_gdu1]
			counts2_gdu2_cal *= cals.relcal_gdu2[itheta, iphi2, iabs_gdu2]
			counts3_gdu2_cal *= cals.relcal_gdu2[itheta, iphi3, iabs_gdu2]
			counts4_gdu2_cal *= cals.relcal_gdu2[itheta, iphi4, iabs_gdu2]
		endif else begin
			counts2_gdu1_cal = fix(round(counts1_gdu1_cal), TYPE=12)
			counts3_gdu1_cal = fix(round(counts1_gdu1_cal), TYPE=12)
			counts4_gdu1_cal = fix(round(counts1_gdu1_cal), TYPE=12)
			counts2_gdu2_cal = fix(round(counts1_gdu2_cal), TYPE=12)
			counts3_gdu2_cal = fix(round(counts1_gdu2_cal), TYPE=12)
			counts4_gdu2_cal = fix(round(counts1_gdu2_cal), TYPE=12)
		endelse
		
		;Fill Values: GDU1
		if nfill_gdu1 gt 0 then begin
			if tf_abscal then begin
				counts2_gdu1_cal[ifill_gdu1] = -1e31
				counts3_gdu1_cal[ifill_gdu1] = -1e31
				counts4_gdu1_cal[ifill_gdu1] = -1e31
			endif else begin
				counts2_gdu1_cal[ifill_gdu1] = 65535US
				counts3_gdu1_cal[ifill_gdu1] = 65535US
				counts4_gdu1_cal[ifill_gdu1] = 65535US
			endelse
		endif
		
		;Fill Values: GDU1
		if nfill_gdu2 gt 0 then begin
			if tf_abscal then begin
				counts2_gdu2_cal[ifill_gdu2] = -1e31
				counts3_gdu2_cal[ifill_gdu2] = -1e31
				counts4_gdu2_cal[ifill_gdu2] = -1e31
			endif else begin
				counts2_gdu2_cal[ifill_gdu2] = 65535US
				counts3_gdu2_cal[ifill_gdu2] = 65535US
				counts4_gdu2_cal[ifill_gdu2] = 65535US
			endelse
		endif
	endif

;------------------------------------------------------
; Return                                              |
;------------------------------------------------------
	;Structure of calibrated counts
	cal_cnts = create_struct( $
		'counts1_gdu1', temporary(counts1_gdu1_cal), $
		'counts1_gdu2', temporary(counts1_gdu2_cal) )
	
	if tf_brst then begin
		cal_cnts = create_struct(cal_cnts, $
			'counts2_gdu1', temporary(counts2_gdu1_cal), $
			'counts3_gdu1', temporary(counts3_gdu1_cal), $
			'counts4_gdu1', temporary(counts4_gdu1_cal), $
			'counts2_gdu2', temporary(counts2_gdu2_cal), $
			'counts3_gdu2', temporary(counts3_gdu2_cal), $
			'counts4_gdu2', temporary(counts4_gdu2_cal) )
	endif

	return, cal_cnts
end
