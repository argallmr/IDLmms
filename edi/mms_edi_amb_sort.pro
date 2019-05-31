; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_sort
;
; PURPOSE:
;+
;   Sort EDI data by operational mode and by pitch angle.
;
; :Categories:
;    MMS, EDI
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
;                       TRAJ_DBCS_GDU1  - Electron incident trajectories in DBCS for GDU1
;                       TRAJ_DBCS_GDU2  - Electron incident trajectories in DBCS for GDU2
;                       TRAJ_GSE_GDU1   - Electron incident trajectories in GSE for GDU1
;                       TRAJ_GSE_GDU2   - Electron incident trajectories in GSE for GDU2
;                       TRAJ_GSM_GDU1   - Electron incident trajectories in GSM for GDU1
;                       TRAJ_GSM_GDU2   - Electron incident trajectories in GSM for GDU2
;       BITMASK:    in, required, type=byte
;                   A bitmask use to identify operational modes.
;
; :Returns:
;       TRAJ:       out, required, type=struct
;                   Data sorty by mode. Possible tags are:
;                       AMB          -  Field-aligned, centered
;                       AMB_PM2      -  Field-aligned, one-sided
;                       AMB_ALT_CC   -  Alternating; field-aligned centered; perpendicular centered
;                       AMB_ALT_OC   -  Alternating; field-aligned one-sided; perpendicular centered
;                       AMB_ALT_OOM  -  Alternating; field-aligned one-sided; perpendicular one-sided monodirectional
;                       AMB_ALT_OOB  -  Alternating; field-aligned one-sided; perpendicular one-sided bidirectional
;
; :See Also:
;   mms_edi_amb_l1a_read.pro
;   mms_edi_amb_ops_bitmask.pro
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
;       2016/09/14  -   Written by Matthew Argall
;       2017/10/24  -   Alternating mode bits checking was ambiguous. Reordered from
;                           most to least specific. - MRA
;-
function mms_edi_amb_sort, edi, bitmask
	compile_opt idl2
	on_error, 2
	
;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	dims    = size(edi.counts_gdu1, /DIMENSIONS)
	tf_brst = n_elements(dims) eq 2
	tf_traj = MrStruct_HasTag(edi, 'TRAJ_DBCS_GDU1')
	t_fill  = MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999)

	;In file versions before v0.6.1, PITCH_GDU[12] was not expanded
	if n_elements(edi.pitch_gdu1) ne dims[0] $
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
; Find Unique Modes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	iUniq    = uniq(bitmask, sort(bitmask))
	uniqBits = bitmask[iUniq]
	nBits    = n_elements(uniqBits)
	
	;Log information
	strBits = nBits eq 1 ? string(uniqBits, FORMAT='(i0)') : '[' + strjoin(string(uniqBits, FORMAT='(i0)'), ',') + ']'
	MrPrintF, 'LogText', nBits,              FORMAT= '(%"Number of operational modes found: %i")'
	MrPrintF, 'LogText', temporary(strBits), FORMAT= '(%"  Modes: %s")'
	
;-----------------------------------------------------
; Step Through Each Mode \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	for i = 0, nBits - 1 do begin
		theBit = bitmask[iUniq[i]]
		idx    = where(bitmask eq theBit, nIdx)

	;-----------------------------------------------------
	; Sort Counts \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		data_0   = mms_edi_amb_sort_fa_cts( edi, idx, 0 )
		data_90  = mms_edi_amb_sort_perp_cts( edi, idx )
		data_180 = mms_edi_amb_sort_fa_cts( edi, idx, 180 )

	;-----------------------------------------------------
	; Sort Trajectories \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if tf_traj then begin
			traj_0   = mms_edi_amb_sort_fa_traj( edi, idx, 0 )
			traj_90  = mms_edi_amb_sort_perp_traj( edi, idx )
			traj_180 = mms_edi_amb_sort_fa_traj( edi, idx, 180 )
			
			;Combine with counts data
			if data_0.epoch_0[0]        ne t_fill then data_0   = create_struct(data_0,   temporary(traj_0))
			if data_90.epoch_90_gdu1[0] ne t_fill then data_90  = create_struct(data_90,  temporary(traj_90))
			if data_180.epoch_180[0]    ne t_fill then data_180 = create_struct(data_180, temporary(traj_180))
		endif

	;-----------------------------------------------------
	; Consolidate Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if data_0.epoch_0[0] ne t_fill then begin
			;Assert that that time tags are identical
			if array_equal(data_0.epoch_0, data_180.epoch_180) then begin
				data_0   = MrStruct_RenameTags(data_0,   'EPOCH_0', 'EPOCH_FA')
				data_180 = MrStruct_RemoveTags(data_180, 'EPOCH_180')
			endif else begin
				message, 'Time tags for [0,180] counts are not identical.'
			endelse
		endif
		
		if data_90.epoch_90_gdu1[0] ne t_fill then begin
			;Assert that that time tags are identical
			if array_equal(data_90.epoch_90_gdu1, data_90.epoch_90_gdu2) then begin
				data_90 = MrStruct_RenameTags(data_90, 'EPOCH_90_GDU1', 'EPOCH_PERP')
				data_90 = MrStruct_RemoveTags(data_90, 'EPOCH_90_GDU2')
			endif else begin
				message, 'Time tags for 90-degree counts are not identical.'
			endelse
		endif

	;-----------------------------------------------------
	; Field-Aligned \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if MrBitGet( theBit, 1 ) then begin
			;Combine the data
			data = create_struct(temporary(data_0), temporary(data_180))
			
			;Subset of field-aligned mode
			case 1 of
				MrBitGet(theBit, 4): tag = 'amb'
				MrBitGet(theBit, 5): tag = 'amb_pm2'
				else: message, 'Unknown bit combination (' + strtrim(theBit, 2) + ').'
			endcase
			
			;Select timetag, energy, optics
			i0 = value_locate(edi.epoch_timetag, data.epoch_fa[0])
			i1 = value_locate(edi.epoch_timetag, data.epoch_fa[-1])

	;-----------------------------------------------------
	; Alternating \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		endif else if MrBitGet( theBit, 2 ) then begin
			;Combine the data
			data = create_struct(temporary(data_0), temporary(data_90), temporary(data_180))
			
			;Subset of alternating mode
			case 1 of
				array_equal( MrBitGet(theBit, [5,6,7]), 1 ): tag = 'amb_alt_oob'
				array_equal( MrBitGet(theBit, [5,6]),   1 ): tag = 'amb_alt_oom'
				MrBitGet(theBit, 5): tag = 'amb_alt_oc'
				MrBitGet(theBit, 4): tag = 'amb_alt_cc'
				else: message, 'Unknown bit combination (' + strtrim(theBit, 2) + ').'
			endcase
			
			;Select timetag, energy, optics
			i0 = value_locate(edi.epoch_timetag, min([data.epoch_fa[0],  data.epoch_perp[0]]))
			i1 = value_locate(edi.epoch_timetag, max([data.epoch_fa[-1], data.epoch_perp[-1]]))
			
			;Append dwell time
			data = create_struct( temporary(data), $
			                      'dwell', edi.dwell[i0:i1] )

	;-----------------------------------------------------
	; Perpendicular \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		endif else if MrBitGet( theBit, 3 ) then begin
			;Combine the data
			data = create_struct(temporary(data_90))
			
			;Subset of perpendicular mode
			case 1 of
				array_equal(MrBitGet(theBit, [3,6,7]), 1): tag = 'amb_perp_ob'
				array_equal(MrBitGet(theBit, [3,6]),   1): tag = 'amb_perp_om'
				array_equal(MrBitGet(theBit, [3]),     1): tag = 'amb_perp_c'
				else: message, 'Unknown bit combination (' + strtrim(theBit, 2) + ').'
			endcase
			
			;Select timetag, energy, optics
			i0 = value_locate(edi.epoch_timetag, data.epoch_perp[0])
			i1 = value_locate(edi.epoch_timetag, data.epoch_perp[-1])

	;-----------------------------------------------------
	; ??? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		endif else begin
			message, 'Unknown bit combination (' + strtrim(theBit, 2) + ').'
		endelse

	;-----------------------------------------------------
	; Combine Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Packet-time data
		data = create_struct( temporary(data), $
		                      'flip_flag',     edi.flip_flag[idx], $
		                      'epoch_timetag', edi.epoch_timetag[i0:i1], $
		                      'optics',        edi.optics[i0:i1], $
		                      'energy_gdu1',   edi.energy_gdu1[i0:i1], $
		                      'energy_gdu2',   edi.energy_gdu2[i0:i1] )
		
		;Append results
		result = n_elements(result) eq 0 $
		     ? create_struct(tag, temporary(data)) $
		     : create_struct(tag, temporary(data), result)
	endfor

	return, result
end