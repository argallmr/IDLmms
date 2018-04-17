; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_sort_perp_cts
;
; PURPOSE:
;+
;   Sort EDI perpendicular ambient electron counts data.
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
;                       EPOCH_90_GDU1      - TT2000 epoch times for GDU1
;                       EPOCH_90_GDU2      - TT2000 epoch times for GDU2
;                       COUNTS_90_GDU1     - Electron counts or flux measurements for GDU1
;                       COUNTS_90_GDU2     - Electron counts or flux measurements for GDU2
;                       DELTA_90_GDU1      - Error in the counts measurements for GDU1
;                       DELTA_90_GDU2      - Error in the counts measurements for GDU2
;
; :See Also:
;   mms_edi_amb_l1a_read.pro
;   mms_edi_amb_ops_bitmask.pro
;   mms_edi_amb_sort.pro
;   mms_edi_amb_sort_perp_traj.pro
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
function mms_edi_amb_sort_perp_cts, edi, idx, pa
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Find Pitch Angles \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;PA=90
	iGDU1 = where(edi.pitch_gdu1[idx] eq 90, nGDU1)
	iGDU2 = where(edi.pitch_gdu2[idx] eq 90, nGDU2)
	
	;Overall index
	iGDU1 = idx[iGDU1]
	iGDU2 = idx[iGDU2]
	
	;
	;Select 90 pitch angle
	;   - GDU1 and GDU2 sample at 90 degree pitch angle simultaneously
	;   - To prevent repeated time tags, keep their data separate
	;
	
	;TT2000 fill value
	t_fill = MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999)
	
	;GDU1
	if nGDU1 gt 0 then begin
		data = create_struct( 'epoch_90_gdu1',  edi.epoch_gdu1[iGDU1,*], $
		                      'counts_90_gdu1', edi.counts_gdu1[iGDU1,*], $
		                      'delta_90_gdu1',  edi.delta_gdu1[iGDU1,*] )
	endif else begin
		MrPrintF, 'LogWarn', 'No PA=90 data for GDU1.'
		data = create_struct( 'epoch_90_gdu1',  t_fill, $
		                      'counts_90_gdu1', -1S, $
		                      'delta_90_gdu1',  -1S )
	endelse
	
	;GDU2
	if nGDU2 gt 0 then begin
		gdu2 = create_struct( 'epoch_90_gdu2',  edi.epoch_gdu2[iGDU2,*], $
		                      'counts_90_gdu2', edi.counts_gdu2[iGDU2,*], $
		                      'delta_90_gdu2',  edi.delta_gdu2[iGDU2,*] )
	endif else begin
		MrPrintF, 'LogWarn', 'No PA=90 data for GDU2.'
		gdu2 = create_struct( 'epoch_90_gdu2',  t_fill, $
		                      'counts_90_gdu2', -1S, $
		                      'delta_90_gdu2',  -1S )
	endelse
	
	;Combine data
	data = create_struct(data, temporary(gdu2))

	return, data
end