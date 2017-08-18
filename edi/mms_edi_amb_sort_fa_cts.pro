; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_sort_fa_cts
;
; PURPOSE:
;+
;   Sort EDI field-aligned ambient electron counts data.
;
;  :Params:
;       EDI:        in, required, type=struct
;                   A structure of EDI L1A data with the following tags:
;                       EPOCH_GDU1      - TT2000 epoch times for GDU1
;                       EPOCH_GDU2      - TT2000 epoch times for GDU2
;                       PITCH_GDU1      - Pitch angle state of GDU1 (typically 0, 180, or 90)
;                       PITCH_GDU2      - Pitch angle state of GDU2 (typically 0, 180, or 90)
;                       COUNTS_GDU1     - Electron counts or flux measurements for GDU1
;                       COUNTS_GDU2     - Electron counts or flux measurements for GDU2
;                       DELTA_GDU1      - Error in the counts measurements for GDU1
;                       DELTA_GDU2      - Error in the counts measurements for GDU2
;       IDX:        in, required, type=intarr
;                   Indices into each array in `EDI` that select data from the operational
;                       mode being sorted.
;       PA:         in, required, type=integer
;                   Pitch angle being sorted. Options are: {0 | 180}.
;
; :Returns:
;       EDI:        in, required, type=struct
;                   A structure of EDI L1A data with the following tags (substitute `PA` value):
;                       EPOCH_PA      - TT2000 epoch times
;                       COUNTS_PA     - Electron counts or flux measurements
;                       DELTA_PA      - Error in the counts measurements
;                       GDU_PA        - Flag indicating which GDU made the measurement.
;
; :See Also:
;   mms_edi_amb_l1a_read.pro
;   mms_edi_amb_ops_bitmask.pro
;   mms_edi_amb_sort.pro
;   mms_edi_amb_sort_fa_traj.pro
;
; :Author:
;    Matthew Argall::
;        University of New Hampshire
;        Morse Hall Room 348
;        8 College Road
;        Durham, NH 03824
;        matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2016/09/16  -   Written by Matthew Argall
;-
function mms_edi_amb_sort_fa_cts, edi, idx, pa
	compile_opt idl2
	on_error, 2
	
	;Check inputs
	if pa ne 0 && pa ne 180 then message, 'PA must be {0 | 180}.'
	
;-----------------------------------------------------
; Find Pitch Angles \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;PA=0
	iGDU1   = where(edi.pitch_gdu1[idx] eq pa, nGDU1)
	iGDU2   = where(edi.pitch_gdu2[idx] eq pa, nGDU2)
	
	;Overall index
	iGDU1 = idx[iGDU1]
	iGDU2 = idx[iGDU2]
	
;-----------------------------------------------------
; Sort \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if nGDU1 gt 0 && nGDU2 gt 0 then begin
		t         = [ edi.epoch_gdu1[iGDU1],    edi.epoch_gdu2[iGDU2]    ]
		counts    = [ edi.counts_gdu1[iGDU1,*], edi.counts_gdu2[iGDU2,*] ]
		delta     = [ edi.delta_gdu1[iGDU1,*],  edi.delta_gdu2[iGDU2,*]  ]
	
		;Sort times
		isort  = sort(t)
		t      = t[isort]
		counts = counts[isort,*]
		delta  = delta[isort,*]
	
		;Mark GDU
		gdu        = bytarr(nGDU1 + nGDU2)
		gdu[iGDU1] = 1B
		gdu[iGDU2] = 2B

	;Only GDU1 data
	endif else if nGDU1 gt 0 then begin
		t      = edi.epoch_gdu1[iGDU1]
		counts = edi.counts_gdu1[iGDU1,*]
		delta  = edi.delta_gdu1[iGDU1,*]
		gdu    = replicate(1B, nGDU1)

	;Only GDU2 data
	endif else if nGDU2 gt 0 then begin
		t      = edi.epoch_gdu2[iGDU2]
		counts = edi.counts_gdu2[iGDU2,*]
		delta  = edi.delta_gdu2[iGDU2,*]
		gdu    = replicate(2B, nGDU2)

	;No EDI data
	endif else begin
		MrPrintF, 'LogText', 'No ' + strtrim(pa, 2) + ' degree pitch angle data.'
		t      = MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999)
		counts = -1S
		delta  = -1S
		gdu    = -1S
	endelse

	;Gather data
	pa_str = '_' + string(pa, FORMAT='(i0)')
	result = create_struct( 'epoch'  + pa_str, temporary(t), $
	                        'counts' + pa_str, temporary(counts), $
	                        'delta'  + pa_str, temporary(delta), $
	                        'gdu'    + pa_str, temporary(gdu) $
	                      )

	return, result
end