; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_srvy_sort_cnts
;
; PURPOSE:
;+
;   Sort survey mode data by pitch angle instead of by GDU.
;
;  :Params:
;       EDI:        in, required, type=struct
;                   A structure of EDI L1A data with the following tags:
;                       EPOCH_GDU1  - TT2000 epoch times for GDU1
;                       EPOCH_GDU2  - TT2000 epoch times for GDU2
;                       EPOCH_ANGLE - TT2000 epoch times for PITCH_GDU1 and PITCH_GDU2
;                       PITCH_GDU1  - Pitch angle state of GDU1 (typically 0, 180, or 90)
;                       PITCH_GDU2  - Pitch angle state of GDU2 (typically 0, 180, or 90)
;
; :Returns:
;      EDI_OUT:     A structure with the following tags::
;                       TT2000      - Time tags for counts
;                       COUNTS1_0   - counts1_gdu[12] sorted by pitch angle 0.
;                       COUNTS1_180 - counts1_gdu[12] sorted by pitch angle 180.
;                       GDU_0       - Flag to sort PA 0 counts by GDU.
;                       GDU_180     - Flag to sort PA 180 counts by GDU.
;-
function mms_edi_amb_srvy_sort_cnts, edi
	compile_opt idl2
	on_error, 2

	;In file versions before v0.6.1, PITCH_GDU[12] was not expanded
	if n_elements(edi.pitch_gdu1) ne n_elements(edi.counts1_gdu1) $
		then message, 'PITCH_GDU1 and COUNTS1_GDU1 have incompatible sizes.'
	
	;Search for a bad pitch value
	;   - If COUNTS has fill values, it can still be sorted.
	;   - If PITCH has fill values, COUNTS cannot be sorted.
	;   - TODO: Replace COUNTS value with fill value if a case ever arises
	npitch     = float(n_elements(edi.epoch_angle))
	ifill_gdu1 = where(edi.pitch_gdu1 eq 65535US, nfill_gdu1)
	ifill_gdu2 = where(edi.pitch_gdu2 eq 65535US, nfill_gdu2)
	if nfill_gdu1 gt 0 then message, string(nfill_gdu1, nfill_gdu1/npitch*100.0, FORMAT='(%"pitch_gdu1 has %i (%0.2f\%) fill values.")')
	if nfill_gdu2 gt 0 then message, string(nfill_gdu2, nfill_gdu2/npitch*100.0, FORMAT='(%"pitch_gdu2 has %i (%0.2f\%) fill values.")')
	
;-----------------------------------------------------
; EDI: Sort By Pitch Angle 0 \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find 0 and 180 pitch angles
	i0_gdu1   = where(edi.pitch_gdu1 eq 0, n0_gdu1)
	i0_gdu2   = where(edi.pitch_gdu2 eq 0, n0_gdu2)

	;Select 0 pitch angle
	if n0_gdu1 gt 0 && n0_gdu2 gt 0 then begin
		t_0       = [ edi.epoch_gdu1[i0_gdu1],   edi.epoch_gdu2[i0_gdu2]   ]
		counts1_0 = [ edi.counts1_gdu1[i0_gdu1], edi.counts1_gdu2[i0_gdu2] ]
		delta1_0  = [ edi.delta1_gdu1[i0_gdu1],  edi.delta1_gdu2[i0_gdu2]  ]
	
		;Sort times
		isort    = sort(t_0)
		t_0      = t_0[isort]
		counts1_0 = counts1_0[isort]
		delta1_0  = delta1_0[isort]
	
		;Mark GDU
		gdu_0          = bytarr(n0_gdu1 + n0_gdu2)
		gdu_0[i0_gdu1] = 1B
		gdu_0[i0_gdu2] = 2B

	;Only GDU1 data
	endif else if n0_gdu1 gt 0 then begin
		t_0       = edi.epoch_gdu1[i0_gdu1]
		counts1_0 = edi.counts1_gdu1[i0_gdu1]
		delta1_0  = edi.delta1_gdu1[i0_gdu1]
		gdu_0     = replicate(1B, n0_gdu1)

	;Only GDU2 data
	endif else if n0_gdu2 gt 0 then begin
		t_0      = edi.epoch_gdu2[i0_gdu2]
		counts_0 = edi.counts1_gdu2[i0_gdu2]
		delta1_0 = edi.delta1_gdu2[i0_gdu2]
		gdu_0    = replicate(2B, n0_gdu2)

	;No EDI data
	endif else begin
		MrPrintF, 'LogText', 'No 0 degree pitch angle data.'
		t_0      = 0LL
		counts_0 = -1S
	endelse

;-----------------------------------------------------
; EDI: Sort By Pitch Angle 180 \\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	i180_gdu1 = where(edi.pitch_gdu1 eq 180, n180_gdu1)
	i180_gdu2 = where(edi.pitch_gdu2 eq 180, n180_gdu2)

	;Select 180 pitch angle
	if n180_gdu1 gt 0 && n180_gdu2 gt 0 then begin
		t_180       = [ edi.epoch_gdu1[i180_gdu1],   edi.epoch_gdu2[i180_gdu2]   ]
		counts1_180 = [ edi.counts1_gdu1[i180_gdu1], edi.counts1_gdu2[i180_gdu2] ]
		delta1_180  = [ edi.delta1_gdu1[i180_gdu1],  edi.delta1_gdu2[i180_gdu2]  ]
	
		;Sort times
		isort       = sort(t_180)
		t_180       = t_180[isort]
		counts1_180 = counts1_180[isort]
		delta1_180  = delta1_180[isort]
	
		;Mark GDU
		gdu_180            = bytarr(n180_gdu1 + n180_gdu2)
		gdu_180[i180_gdu1] = 1B
		gdu_180[i180_gdu2] = 2B

	;Only GDU1 data
	endif else if n180_gdu1 gt 0 then begin
		t_180       = edi.epoch_gdu1[i180_gdu1]
		counts1_180 = edi.counts1_gdu1[i180_gdu1]
		delta1_180  = edi.counts1_gdu1[i180_gdu1]
		gdu_180     = replicate(1B, n180_gdu1)

	;Only GDU2 data
	endif else if n180_gdu2 gt 0 then begin
		t_180       = edi.epoch_gdu2[i180_gdu2]
		counts1_180 = edi.counts1_gdu2[i180_gdu2]
		delta1_180  = edi.delta1_gdu2[i180_gdu2]
		gdu_180     = replicate(2B, n180_gdu2)
	
	;No EDI data
	endif else begin
		MrPrintF, 'LogText', 'No 180 degree pitch angle data.'
		t_180 = 0LL
		counts_180 = -1S
	endelse

;-----------------------------------------------------
; Return Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Make sure that TT2000 time are the same
	if ~array_equal(t_0, t_180) then $
		message, 'Sorted TT2000 times are different.'
	
	;Create output structure
	edi_out = { tt2000:      temporary(t_0),    $
	            counts1_0:   temporary(counts1_0),   $
	            counts1_180: temporary(counts1_180), $
	            delta1_0:    temporary(delta1_0),    $
	            delta1_180:  temporary(delta1_180),  $
	            gdu_0:       temporary(gdu_0),       $
	            gdu_180:     temporary(gdu_180)      $
	          }

	return, edi_out
end
