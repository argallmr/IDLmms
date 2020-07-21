; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_sort_fa_traj
;
; PURPOSE:
;+
;   Sort incident trajectory vectors of EDI field-aligned ambient electron data. No time
;   tag or GDU information is returned. That information is assumed to come from sorting
;   counts via an identical process (mms_edi_amb_sort_fa_cts.pro).
;
;  :Params:
;       EDI:        in, required, type=struct
;                   A structure of EDI L1A data with the following tags:
;                       EPOCH_GDU1      - TT2000 epoch times for GDU1
;                       EPOCH_GDU2      - TT2000 epoch times for GDU2
;                       PITCH_GDU1      - Pitch angle state of GDU1 (typically 0, 180, or 90)
;                       PITCH_GDU2      - Pitch angle state of GDU2 (typically 0, 180, or 90)
;                       TRAJ_DBCS_GDU1  - Electron incident trajectories in DBCS for GDU1
;                       TRAJ_DBCS_GDU2  - Electron incident trajectories in DBCS for GDU2
;                       TRAJ_GSE_GDU1   - Electron incident trajectories in GSE for GDU1
;                       TRAJ_GSE_GDU2   - Electron incident trajectories in GSE for GDU2
;                       TRAJ_GSM_GDU1   - Electron incident trajectories in GSM for GDU1
;                       TRAJ_GSM_GDU2   - Electron incident trajectories in GSM for GDU2
;       IDX:        in, required, type=intarr
;                   Indices into each array in `EDI` that select data from the operational
;                       mode being sorted.
;       PA:         in, required, type=integer
;                   Pitch angle being sorted. Options are: {0 | 180}.
;
; :Returns:
;       EDI:        in, required, type=struct
;                   A structure of EDI L1A data with the following tags (substitute `PA` value):
;                       TRAJ_DBCS_PA  - Electron incident trajectories in DBCS
;                       TRAJ_GSE_PA   - Electron incident trajectories in GSE
;                       TRAJ_GSM_PA   - Electron incident trajectories in GSM
;
; :See Also:
;   mms_edi_amb_l1a_read.pro
;   mms_edi_amb_ops_bitmask.pro
;   mms_edi_amb_sort.pro
;   mms_edi_amb_sort_fa_cts.pro
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
;       2016/09/18  -   Written by Matthew Argall
;       2020/03/25  -   Fixed typo bug when only GDU1 or GDU2 data is available. - MRA
;-
function mms_edi_amb_sort_fa_traj, edi, idx, pa
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
		t         = [ edi.epoch_gdu1[iGDU1],         edi.epoch_gdu2[iGDU2]         ]
		traj_dbcs = [ [ edi.traj_dbcs_gdu1[*,iGDU1,*] ], [ edi.traj_dbcs_gdu2[*,iGDU2,*] ] ]
		traj_gse  = [ [ edi.traj_gse_gdu1[*,iGDU1,*]  ], [ edi.traj_gse_gdu2[*,iGDU2,*]  ] ]
		traj_gsm  = [ [ edi.traj_gsm_gdu1[*,iGDU1,*]  ], [ edi.traj_gsm_gdu2[*,iGDU2,*]  ] ]
	
		;Sort times
		isort     = sort(t)
		traj_dbcs = traj_dbcs[*,isort,*]
		traj_gse  = traj_gse[*,isort,*]
		traj_gsm  = traj_gsm[*,isort,*]

	;Only GDU1 data
	endif else if nGDU1 gt 0 then begin
		traj_dbcs = edi.traj_dbcs_gdu1[*,iGDU1,*]
		traj_gse  = edi.traj_gse_gdu1[*,iGDU1,*]
		traj_gsm  = edi.traj_gsm_gdu1[*,iGDU1,*]

	;Only GDU2 data
	endif else if nGDU2 gt 0 then begin
		traj_dbcs = edi.traj_dbcs_gdu2[*,iGDU2,*]
		traj_gse  = edi.traj_gse_gdu2[*,iGDU2,*]
		traj_gsm  = edi.traj_gsm_gdu2[*,iGDU2,*]

	;No EDI data
	endif else begin
		MrPrintF, 'LogText', 'No ' + strtrim(pa, 2) + ' degree pitch angle data.'
		traj_dbcs = -1S
		traj_gse  = -1S
		traj_gsm  = -1S
	endelse

	;Gather data
	pa_str = '_' + string(pa, FORMAT='(i0)')
	result = create_struct( 'traj_dbcs' + pa_str, temporary(traj_dbcs), $
	                        'traj_gse'  + pa_str, temporary(traj_gse), $
	                        'traj_gsm'  + pa_str, temporary(traj_gsm) $
	                      )
	return, result
end