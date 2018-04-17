; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_anodes_phi
;
; PURPOSE:
;+
;   Determine the anode number for each channel and GDU.
;
; :Params:
;       PHI:            in, required, type=fltarr
;                       Azimuth angle of reference channel
;       BITMASK:        in, required, type=struct
;                       A bitmask indicate the operational mode.
;       PITCH_GDU1:     in, required, type=bytarr
;                       Pitch angle mode of GDU1
;       PITCH_GDU2:     in, required, type=bytarr
;                       Pitch angle mode of GDU2
;
; :Keywords:
;       BRST:           in, optional, type=boolean, default=0
;                       If set, return 4 channels per GDU. In survey mode, data
;                           from one channel per GDU is returned.
;
; :Returns:
;       AZIMUTH:        A structure with the following tags:
;                           PHI1_GDU1  -  Azimuth angle corresponding to channel 1 on GDU1
;                           PHI2_GDU1  -  Azimuth angle corresponding to channel 2 on GDU1
;                           PHI3_GDU1  -  Azimuth angle corresponding to channel 3 on GDU1
;                           PHI4_GDU1  -  Azimuth angle corresponding to channel 4 on GDU1
;                           PHI1_GDU2  -  Azimuth angle corresponding to channel 1 on GDU2
;                           PHI2_GDU2  -  Azimuth angle corresponding to channel 2 on GDU2
;                           PHI3_GDU2  -  Azimuth angle corresponding to channel 3 on GDU2
;                           PHI4_GDU2  -  Azimuth angle corresponding to channel 4 on GDU2
;
; :See Also:
;   mms_edi_amb_l1a_read.pro
;   mms_edi_amb_ops_bitmask.pro
;   MrStruct_RemoveTags.pro
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
;-
function mms_edi_amb_anodes_phi, phi, bitmask, pitch_gdu1, pitch_gdu2, $
BRST=brst
	compile_opt idl2
	on_error, 2
	
	;Check Inputs
	nPhi    = n_elements(phi)
	tf_brst = keyword_set(brst)
	if n_elements(bitmask)    ne nPhi then message, 'BITMASK has incorrect number of elements.'
	if n_elements(pitch_gdu1) ne nPhi then message, 'PITCH_GDU1 has incorrect number of elements.'
	if n_elements(pitch_gdu2) ne nPhi then message, 'PITCH_GDU2 has incorrect number of elements.'
	
	;Find unique modes
	iUniq = uniq(bitmask, sort(bitmask))
	nUniq = n_elements(iUniq)
	
	;Allocate memory
	;   - Initialize to the CDF fillvalue for byte values
	fillval   = -1e31
	nChannels = tf_brst ? 4 : 1
	gdu1      = make_array(nPhi, nChannels, VALUE=fillval)
	gdu2      = make_array(nPhi, nChannels, VALUE=fillval)
	
	;Number for detector azimuth (0-31)
	;   - Each anode is 11.25 degrees wide
	W = 11.25
	N = fix( round(phi / W), TYPE=1)
	
;-----------------------------------------------------
; Step Through Each Mode \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	for i = 0, nUniq - 1 do begin
		theBit = bitmask[iUniq[i]]
		idx    = where(bitmask eq theBit, nIdx)

	;-----------------------------------------------------
	; AMB-ALT-OOB \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB-ALT-OOB
		;   - pitch    = alternating     = bit 2
		;   - [0,180]  = one-sided       = bit 5
		;   - 90       = one-sided       = bit 6
		;   - 90       = bi-directional  = bit 7
		if array_equal( MrBitGet( theBit, [2,5,6,7] ), 1)  then begin
			ch_fa_gdu1 = indgen(1,nChannels) + 1S
			ch_fa_gdu2 = indgen(1,nChannels) + 1S
			ch_90_gdu1 = indgen(1,nChannels) + 1S
			ch_90_gdu2 = -(indgen(1,nChannels) + 1S)
			
			;BRST
			;   - 0,180, GDU1: (N - 0.5 + ch) * W
			;   - 0,180, GDU2: (14.5 - N + ch) * W
			;   - 90, GDU1: (N - 0.5 + ch) * W
			;   - 90, GDU2: (16.5 - N - ch) * W
			if tf_brst then begin
				offset_fa_gdu1 = -0.5
				offset_fa_gdu2 = 14.5
				offset_90_gdu1 = -0.5
				offset_90_gdu2 = 16.5
			
			;SRVY
			;   - 0,180, GDU1: (N + 0.5) * W
			;   - 0,180, GDU2: (15.5 - N) * W
			;   - 90, GDU1: (N + 0.5) * W
			;   - 90, GDU2: (15.5 - N) * W
			;   - Add/Subtract 1.0 to compensate for subtracting/adding channel 1
			;     (compare with brst)
			endif else begin
				offset_fa_gdu1 =  0.5 - 1.0
				offset_fa_gdu2 = 15.5 - 1.0
				offset_90_gdu1 =  0.5 - 1.0
				offset_90_gdu2 = 15.5 + 1.0
			endelse

	;-----------------------------------------------------
	; AMB-ALT-OOM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB-ALT-OOM
		;   - pitch    = alternating       = bit 2
		;   - [0,180]  = one-sided         = bit 5
		;   - 90       = one-sided         = bit 6
		;   - 90       = moni-directional  = bit -
		endif else if array_equal( MrBitGet( theBit, [2,5,6] ), 1)  then begin
			ch_fa_gdu1 = indgen(1,nChannels) + 1S
			ch_fa_gdu2 = indgen(1,nChannels) + 1S
			ch_90_gdu1 = indgen(1,nChannels) + 1S
			ch_90_gdu2 = indgen(1,nChannels) + 1S
			
			;BRST
			if tf_brst then begin
				offset_fa_gdu1 = -0.5
				offset_fa_gdu2 = 14.5
				offset_90_gdu1 = -0.5
				offset_90_gdu2 = 14.5
			
			;SRVY
			;   - Subtract 1.0 to compensate for adding channel 1 below
			endif else begin
				offset_fa_gdu1 =  0.5 - 1.0
				offset_fa_gdu2 = 15.5 - 1.0
				offset_90_gdu1 =  0.5 - 1.0
				offset_90_gdu2 = 15.5 - 1.0
			endelse

	;-----------------------------------------------------
	; AMB-ALT-OC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB-ALT-OC
		;   - pitch    = alternating = bit 2
		;   - [0,180]  = one-sided   = bit 5
		;   - 90       = centered    = bit -
		endif else if array_equal( MrBitGet( theBit, [2,5] ), 1) then begin
			ch_fa_gdu1 = indgen(1,nChannels) + 1S
			ch_fa_gdu2 = indgen(1,nChannels) + 1S
			ch_90_gdu1 = indgen(1,nChannels) + 1S
			ch_90_gdu2 = indgen(1,nChannels) + 1S
			
			;BRST
			if tf_brst then begin
				offset_fa_gdu1 = -0.5
				offset_fa_gdu2 = 14.5
				offset_90_gdu1 = -2.5
				offset_90_gdu2 = 13.5
			
			;SRVY
			;   - Subtract 1.0 to compensate for adding channel 1 below
			endif else begin
				offset_fa_gdu1 =  0.5 - 1.0
				offset_fa_gdu2 = 15.5 - 1.0
				offset_90_gdu1 =  0.0 - 1.0
				offset_90_gdu2 = 16.0 - 1.0
			endelse

	;-----------------------------------------------------
	; AMB-ALT-CC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB-ALT-CC
		;   - pitch    = alternating = bit 2
		;   - [0,180]  = centered    = bit 4
		;   - 90       = centered    = bit -
		endif else if array_equal( MrBitGet( theBit, [2,4] ), 1) then begin
			ch_fa_gdu1 = indgen(1,nChannels) + 1S
			ch_fa_gdu2 = indgen(1,nChannels) + 1S
			ch_90_gdu1 = indgen(1,nChannels) + 1S
			ch_90_gdu2 = indgen(1,nChannels) + 1S
			
			;BRST
			if tf_brst then begin
				offset_fa_gdu1 = -2.5
				offset_fa_gdu2 = 13.5
				offset_90_gdu1 = -2.5
				offset_90_gdu2 = 13.5
			
			;SRVY
			;   - Subtract 1.0 to compensate for adding channel 1 below
			endif else begin
				offset_fa_gdu1 =  0.0 - 1.0
				offset_fa_gdu2 = 16.0 - 1.0
				offset_90_gdu1 =  0.0 - 1.0
				offset_90_gdu2 = 16.0 - 1.0
			endelse

	;-----------------------------------------------------
	; AMB \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB
		;   - pitch   = field-aligned = bit 1
		;   - [0,180] = centered      = bit 4
		endif else if array_equal( MrBitGet( theBit, [1,4] ), 1) then begin
			ch_fa_gdu1 = indgen(1,nChannels) + 1S
			ch_fa_gdu2 = indgen(1,nChannels) + 1S
			
			;BRST
			if tf_brst then begin
				offset_fa_gdu1 = -2.5
				offset_fa_gdu2 = 13.5
			
			;SRVY
			;   - Subtract 1.0 to compensate for adding channel 1 below
			endif else begin
				offset_fa_gdu1 =  0.0 - 1.0
				offset_fa_gdu2 = 16.0 - 1.0
			endelse

	;-----------------------------------------------------
	; AMB-PM2 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB-PM2
		;   - pitch   = field-aligned = bit 1
		;   - [0,180] = one-sided     = bit 5
		endif else if array_equal( MrBitGet( theBit, [1,5] ), 1) then begin
			ch_fa_gdu1 = indgen(1,nChannels) + 1S
			ch_fa_gdu2 = indgen(1,nChannels) + 1S
			
			;BRST
			if tf_brst then begin
				offset_fa_gdu1 = -0.5
				offset_fa_gdu2 = 14.5
			
			;SRVY
			;   - Subtract 1.0 to compensate for adding channel 1 below
			endif else begin
				offset_fa_gdu1 =  0.5 - 1.0
				offset_fa_gdu2 = 15.5 - 1.0
			endelse

	;-----------------------------------------------------
	; ???? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		endif else begin
			message, 'Unknown operational mode (' + strtrim(theBit, 2) + ').'
		endelse

	;-----------------------------------------------------
	; Field-Aligned Channels \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if n_elements(offset_fa_gdu1 gt 0) then begin
			;Indices of field-aligned anodes
			iGDU1 = where( pitch_gdu1[idx] eq 0 or pitch_gdu1[idx] eq 180, nGDU1)
			iGDU2 = where( pitch_gdu2[idx] eq 0 or pitch_gdu2[idx] eq 180, nGDU2)
			
			;Phi value for each anode
			
			;GDU1
			if nGDU1 gt 0 then gdu1[idx[iGDU1],*] = ( rebin(N[idx[iGDU1]], nGDU1, nChannels) + $
			                                          offset_fa_gdu1                         + $
			                                          rebin(ch_fa_gdu1, nGDU1, nChannels)      $
			                                        ) * W
			;GDU2
			if nGDU2 gt 0 then gdu2[idx[iGDU2],*] = ( offset_fa_gdu2                         - $
			                                          rebin(N[idx[iGDU2]], nGDU2, nChannels) + $
			                                          rebin(ch_fa_gdu2, nGDu2, nChannels)      $
			                                        ) * W
		endif

	;-----------------------------------------------------
	; Perpendicular Channels \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if n_elements(offset_90_gdu1 gt 0) then begin
			;Indices of perpendicular anodes
			iGDU1 = where( pitch_gdu1[idx] eq 90, nGDU1)
			iGDU2 = where( pitch_gdu2[idx] eq 90, nGDU2)
			
			;Phi value for each anode
			
			;GDU1
			if nGDU1 gt 0 then gdu1[idx[iGDU1],*] = ( rebin(N[idx[iGDU1]], nGDU1, nChannels) + $
			                                          offset_90_gdu1                         + $
			                                          rebin(ch_90_gdu1, nGDU1, nChannels)    $
			                                        ) * W
			;GDU2
			if nGDU2 gt 0 then gdu2[idx[iGDU2],*] = ( offset_90_gdu2                         - $
			                                          rebin(N[idx[iGDU2]], nGDU2, nChannels) + $
			                                          rebin(ch_90_gdu2, nGDU2, nChannels)    $
			                                        ) * W
		endif
	endfor

;-----------------------------------------------------
; Force Into Range [0,360) \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;GDU1
	gdu1 = (gdu1 + (gdu1 lt 0)*360.0) mod 360.0
	gdu2 = (gdu2 + (gdu2 lt 0)*360.0) mod 360.0
	
	;Return
	return, {phi_gdu1: temporary(gdu1), phi_gdu2: temporary(gdu2)}
end
