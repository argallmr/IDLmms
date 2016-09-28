; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_sort_perp_traj
;
; PURPOSE:
;+
;   Sort incident trajectory vectors of EDI perpendicular ambient electron data. No time
;   tag or GDU information is returned. That information is assumed to come from sorting
;   counts via an identical process (mms_edi_amb_sort_perp_cts.pro).
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
;                       TRAJ_DBCS_90_GDU1  - Electron incident trajectories in DBCS for GDU1
;                       TRAJ_DBCS_90_GDU2  - Electron incident trajectories in DBCS for GDU2
;                       TRAJ_GSE_90_GDU1   - Electron incident trajectories in GSE for GDU1
;                       TRAJ_GSE_90_GDU2   - Electron incident trajectories in GSE for GDU2
;                       TRAJ_GSM_90_GDU1   - Electron incident trajectories in GSM for GDU1
;                       TRAJ_GSM_90_GDU2   - Electron incident trajectories in GSM for GDU2
;
; :See Also:
;   mms_edi_amb_l1a_read.pro
;   mms_edi_amb_ops_bitmask.pro
;   mms_edi_amb_sort.pro
;   mms_edi_amb_sort_perp_cts.pro
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
;-
function mms_edi_amb_sort_perp_traj, edi, idx, pa
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Find Pitch Angles \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;PA=90
	iGDU1 = where(edi.pitch_gdu1 eq 90, nGDU1)
	iGDU2 = where(edi.pitch_gdu2 eq 90, nGDU2)
	
	;Overall index
	iGDU1 = idx[iGDU1]
	iGDU2 = idx[iGDU2]
	
	;
	;Select 90 pitch angle
	;   - GDU1 and GDU2 sample at 90 degree pitch angle simultaneously
	;   - To prevent repeated time tags, keep their data separate
	;
	
	;GDU1
	if nGDU1 gt 0 then begin
		data = create_struct( 'traj_dbcs_90_gdu1', edi.traj_dbcs_gdu1[*,iGDU1,*], $
		                      'traj_gse_90_gdu1',  edi.traj_gse_gdu1[*,iGDU1,*], $
		                      'traj_gsm_90_gdu1',  edi.traj_gsm_gdu1[*,iGDU1,*] )
	endif else begin
		MrPrintF, 'LogWarn', 'No PA=90 data for GDU1.'
	endelse
	
	;GDU2
	if nGDU2 gt 0 then begin
		gdu2 = create_struct( 'traj_dbcs_90_gdu2', edi.traj_dbcs_gdu2[*,iGDU2,*], $
		                      'traj_gse_90_gdu2',  edi.traj_gse_gdu2[*,iGDU2,*], $
		                      'traj_gsm_90_gdu2',  edi.traj_gsm_gdu2[*,iGDU2,*] )
		
		;Combine data
		if n_elements(data) eq 0 $
			then data = temporary(gdu2) $
			else data = create_struct(data, temporary(gdu2))
	endif else begin
		MrPrintF, 'LogWarn', 'No PA=90 data for GDU2.'
	endelse

	;No data
	if nGDU1 eq 0 && nGDU2 eq 0 then data = -1S

	return, data
end