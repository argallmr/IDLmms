; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_srvy_calc_pa
;
; PURPOSE:
;   Calculate the pitch angle of EDI ambient electrons, and sort them accordingly.
;
; :Categories:
;    MMS, EDI, AMB
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
;       2016/01/29  -   Written by Matthew Argall
;-
;*****************************************************************************************;+
;   Sort survey data by pad into 0 and 180 degree counts.
;
; :Params:
;       EDI:        in, required, type=struct
;                   A structure of EDI L1A data with the following tags:
;                       EPOCH_GDU1  - TT2000 epoch times for GDU1
;                       EPOCH_GDU2  - TT2000 epoch times for GDU2
;                       EPOCH_ANGLE - TT2000 epoch times for PITCH_GDU1 and PITCH_GDU2
;                       PITCH_GDU1  - Pitch angle state of GDU1 (typically 0, 180, or 90)
;                       PITCH_GDU2  - Pitch angle state of GDU2 (typically 0, 180, or 90)
;      PA:          in, required, type=8xN float
;                   A 2-column array with the pitch angle corresponding to the center
;                       of each active pad.
;      PA_LO:       in, required, type=8xN float
;                   A 2-column array with the pitch angle corresponding to the lower-edge
;                       of each active pad.
;      PA_HI:       in, required, type=8xN float
;                   A 2-column array with the pitch angle corresponding to the upper-edge
;                       of each active pad.
;
; :Returns:
;      EDI_OUT:     A structure with the following tags::
;                       TT2000_0    - Time tags for 0 degree pitch angle counts
;                       TT2000_180  - Time tags for 180 degree pitch angle counts
;                       PA0         - Center pitch angle of each annode with PA 0
;                       PA0_LO      - Lower pitch angle of each annode with PA 0
;                       PA0_HI      - Upper pitch angle of each annode with PA 0
;                       PA180       - Center pitch angle of each annode with PA 180
;                       PA180_L0    - Lower pitch angle of each annode with PA 180
;                       PA180_HI    - Upper pitch angle of each annode with PA 180
;                       GDU_0       - Flag to sort PA 0 counts by GDU.
;                       GDU_180     - Flag to sort PA 180 counts by GDU.
;-
function mms_edi_amb_srvy_sort_pa, edi, pa, pa_lo, pa_hi
	compile_opt idl2
	on_error, 2
	
	;In file versions before v0.6.1, PITCH_GDU[12] was not expanded
	if n_elements(edi.pitch_gdu1) ne n_elements(edi.counts1_gdu1) then begin
		;Log a warning
		MrPrintF, 'LogWarn', 'PITCH and COUNTS have incompatible sizes.'
		
		;Interpolate pitch angle.
		;   - Same method as AZIMUTH
		iangle     = value_locate(edi.epoch_angle, edi.epoch_gdu1)
		pitch_gdu1 = edi.pitch_gdu1[iangle]
		pitch_gdu2 = edi.pitch_gdu2[iangle]
	endif
	
;-----------------------------------------------------
; PA 0 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	i0_gdu1 = where(edi.pitch_gdu1 eq 0.0, n0_gdu1)
	i0_gdu2 = where(edi.pitch_gdu2 eq 0.0, n0_gdu2)
	
	;GDU1 and GDU2
	;   - See notes in "edi_amb_brst_pa". PA is ordered as
	;         [counts1-4_gdu1, counts_1-4_gdu2]
	;     but in terms of look direction, the following is true
	;         counts1-4_gdu1  <==>  counts4-1_gdu2
	if n0_gdu1 gt 0 && n0_gdu2 gt 0 then begin
		t_0    = [ edi.epoch_gdu1[i0_gdu1], edi.epoch_gdu2[i0_gdu2] ]
		pa0    = [    pa[i0_gdu1,0],         pa[i0_gdu2,1]   ]
		pa0_lo = [ pa_lo[i0_gdu1,0],      pa_lo[i0_gdu2,1]   ]
		pa0_hi = [ pa_hi[i0_gdu1,0],      pa_hi[i0_gdu2,1]   ]
		
		;Sort times
		isort  = sort(t_0)
		t_0    = t_0[isort]
		pa0    = pa0[isort,*]
		pa0_lo = pa0[isort,*]
		pa0_hi = pa0[isort,*]
		
		;Mark GDU
		gdu_0          = bytarr(n0_gdu1 + n0_gdu2)
		gdu_0[i0_gdu1] = 1B
		gdu_0[i0_gdu2] = 2B
	
	;Only GDU1 data
	endif else if n0_gdu1 gt 0 then begin
		t_0    = edi.epoch_gdu1[i0_gdu1]
		pa0    =    pa[i0_gdu1,0]
		pa0_lo = pa_lo[i0_gdu1,0]
		pa0_hi = pa_hi[i0_gdu1,0]
		gdu_0  = replicate(1B, n0_gdu1)
	
	;Only GDU2 data
	endif else if n0_gdu2 gt 0 then begin
		t_0    = edi.epoch_gdu2[i0_gdu2]
		pa0    =    pa[i0_gdu2,1]
		pa0_lo = pa_lo[i0_gdu2,1]
		pa0_hi = pa_hi[i0_gdu2,1]
		gdu_0  = replicate(2B, n0_gdu2)
	endif else begin
		MrPrintF, 'LogText', 'No 0 degree pitch angle data.'
		t_0    = 0LL
		pa0    = -1S
		pa0_lo = -1S
		pa0_hi = -1S
	endelse
	
;-----------------------------------------------------
; PA 180 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	i180_gdu1 = where(edi.pitch_gdu1 eq 180.0, n180_gdu1)
	i180_gdu2 = where(edi.pitch_gdu2 eq 180.0, n180_gdu2)
	
	;GDU1 and GDU2
	;   - See notes in "edi_amb_brst_pa". PA is ordered as
	;         [counts1-4_gdu1, counts_1-4_gdu2]
	;     but in terms of look direction, the following is true
	;         counts1-4_gdu1  <==>  counts4-1_gdu2
	if n180_gdu1 gt 0 && n180_gdu2 gt 0 then begin
		t_180    = [ edi.epoch_gdu1[i180_gdu1], edi.epoch_gdu2[i180_gdu2] ]
		pa180    = [    pa[i180_gdu1,0],         pa[i180_gdu2,1] ]
		pa180_lo = [ pa_lo[i180_gdu1,0],      pa_lo[i180_gdu2,1] ]
		pa180_hi = [ pa_hi[i180_gdu1,0],      pa_hi[i180_gdu2,1] ]
		
		;Sort times
		isort    = sort(t_180)
		t_180    = t_180[isort]
		pa180    = pa180[isort,*]
		pa180_lo = pa180[isort,*]
		pa180_hi = pa180[isort,*]
		
		;Mark GDU
		gdu_180            = bytarr(n180_gdu1 + n180_gdu2)
		gdu_180[i180_gdu1] = 1B
		gdu_180[i180_gdu2] = 2B
	
	;Only GDU1 data
	endif else if n180_gdu1 gt 0 then begin
		t_180    = edi.epoch_gdu1[i180_gdu1]
		pa180    =    pa[i180_gdu1,0]
		pa180_lo = pa_lo[i180_gdu1,0]
		pa180_hi = pa_hi[i180_gdu1,0]
		gdu_180  = replicate(1B, n180_gdu1)
	
	;Only GDU2 data
	endif else if n180_gdu2 gt 0 then begin
		t_180    = edi.epoch_gdu2[i180_gdu2]
		pa180    =    pa[i180_gdu2,1]
		pa180_lo = pa_lo[i180_gdu2,1]
		pa180_hi = pa_hi[i180_gdu2,1]
		gdu_180  = replicate(2B, n180_gdu2)
	endif else begin
		MrPrintF, 'LogText', 'No 0 degree pitch angle data.'
		t_180    = 0LL
		pa180    = -1S
		pa180_lo = -1S
		pa180_hi = -1S
	endelse
	
;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	edi_out = { pa0:        temporary(pa0),      $
	            pa0_lo:     temporary(pa0_lo),   $
	            pa0_hi:     temporary(pa0_hi),   $
	            pa180:      temporary(pa180),    $
	            pa180_lo:   temporary(pa180_lo), $
	            pa180_hi:   temporary(pa180_hi)  $
	          }
	
	return, edi_out
end


;+
;   Determine the pitch angle of each active pad.
;
;   The look direction is defined as
;      lx = sin(polar) * cos(azimuth)
;      ly = sin(polar) * sin(azimuth)
;      lz = cos(polar)
;      lv = [lx, ly, lz]
;
;   while the incident vector of each particle is opposite to the
;   look direction. The pitch angle, then, is the projection angle
;   of the indicent vector onto the magnetic field direction:
;
;      PA = -lv dot b_hat
;
;   AZIMUTH and POLAR identify the anode on which the magnetic field
;   is centered, marked "p" in the table below. The location of the
;   other active pads are given relative to "p" and depend on which
;   packing mode we are in. The look direction of GDU2 anodes is
;   opposite to those of GDU1.
;
;   Once the pitch angle is determined for pad "p" from GDU1 and
;   GDU2 are sorted by PITCH_GDU1 and PITCH_GDU2, arranging counts
;   by pitch angle instead of GDU.
;
;   Active Pads:
;       PACK_MODE 1
;         channel    anode#      azimuth of look direction
;         ---------------------------------------------------
;         counts1      p-2       (p-1.5) * 11.25 deg
;         counts2      p-1       (p-0.5) * 11.25 deg
;         counts3      p         (p+0.5) * 11.25 deg
;         counts4      p+1       (p+1.5) * 11.25 deg
;
;       PACK_MODE 2
;         channel    anode#      azimuth of look direction
;         ---------------------------------------------------
;         counts1      p         (p+0.5) * 11.25 deg
;         counts2      p-1       (p-0.5) * 11.25 deg
;         counts3      p-2       (p-1.5) * 11.25 deg
;         counts4      p-3       (p-2.5) * 11.25 deg
;
;   In survey mode, the following is true
;       PACK_MODE1 - Counts from anode (p-1) and p are summed.
;       PACK_MODE2 - Only anode p is used.
;
;
; :Params:
;       EDI:        in, required, type=struct
;                   A structure of EDI L1A data with the following tags:
;                       EPOCH_GDU1  - TT2000 epoch times for GDU1
;                       EPOCH_GDU2  - TT2000 epoch times for GDU2
;                       EPOCH_ANGLE - TT2000 epoch times for PITCH_GDU1 and PITCH_GDU2
;                       AZIMUTH     - Azimuthal look direction of GDU1.
;                       POLAR       - Polar look direction of GDU2.
;                       PITCH_GDU1  - Pitch angle state of GDU1 (typically 0, 180, or 90)
;                       PITCH_GDU2  - Pitch angle state of GDU2 (typically 0, 180, or 90)
;      FGM:         in, required, type=struct
;                   A structure of FGM L2PRE data with the following tags:
;                       TT2000      - TT2000 time tags for B_BCS
;                       B_BCS       - 3xN magnetic field array in BCS coordinates
;
; :Returns:
;      EDI_OUT:     A structure with the following tags::
;                       TT2000_0    - Time tags for 0 degree pitch angle counts
;                       TT2000_180  - Time tags for 180 degree pitch angle counts
;                       PA0         - Center pitch angle of each annode with PA 0
;                       PA0_LO      - Lower pitch angle of each annode with PA 0
;                       PA0_HI      - Upper pitch angle of each annode with PA 0
;                       PA180       - Center pitch angle of each annode with PA 180
;                       PA180_L0    - Lower pitch angle of each annode with PA 180
;                       PA180_HI    - Upper pitch angle of each annode with PA 180
;                       GDU_0       - Flag to sort PA 0 counts by GDU.
;                       GDU_180     - Flag to sort PA 180 counts by GDU.
;-
function mms_edi_amb_srvy_calc_pa, edi, fgm, $
MODS=mods
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Check Packing Mode \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Check the packing mode
	if total(edi.pack_mode[1:*] - edi.pack_mode) ne 0 $
		then message, 'Packing mode changes. How do I deal with this?' $
		else pack_mode = edi.pack_mode[0]
	
	;Sure only about packing mode 0
	if pack_mode ne 0 then message, 'Unknown packing mode.'

;-----------------------------------------------------
; Filter Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Time range over which we have EDI data
	t0 = edi.epoch_gdu1[0]
	t1 = edi.epoch_gdu2[-1]

	;Prune DFG times
	iselect = where(fgm.tt2000 ge t0 and fgm.tt2000 le t1, nselect)
	if nselect eq 0 then begin
		message, 'No FGM data in time interval.'
	endif else begin
		t_fgm = fgm.tt2000[iselect]
		b_bcs = fgm.b_bcs[0:2,iselect]
	endelse

;-----------------------------------------------------
; EDI Look Directions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Interpolate firing angles to count times
	iangle  = reform(value_locate(edi.epoch_angle, edi.epoch_gdu1))
	azimuth = edi.azimuth[iangle]
	polar   = edi.polar[iangle]
	npts    = n_elements(azimuth)

;-----------------------------------------------------
; Annode Operations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	; PACK_MODE 0
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

	;Offset of each pad in anode units
	;   [low, center, high]
	case pack_mode of
		0: offset = [-1.0, 0.0, 1.0]
		2: offset = [ 0.0, 0.5, 1.0]
		else: message, 'Invalid packing mode (' + string(pack_mode, FORMAT='(i0)') + ').'
	endcase
	npads = n_elements(offset)
	
;-----------------------------------------------------
; Azimuth of Edges & Center of Anodes \\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Angle of the lower edge of each pad
	pa_lo = azimuth + offset[0] * 11.25
	pa    = azimuth + offset[1] * 11.25
	pa_hi = azimuth + offset[2] * 11.25

;-----------------------------------------------------
; Look & Incident Vectors \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert to radians
	deg2rad  = !dpi / 180.0D
	pa      *= deg2rad
	pa_lo   *= deg2rad
	pa_hi   *= deg2rad
	azimuth *= deg2rad
	polar   *= deg2rad

	;Look Direction
	ld    = [ [ cos(pa)    * sin(polar) ], $
	          [ sin(pa)    * sin(polar) ], $
	          [              cos(polar) ] ]
	ld_lo = [ [ cos(pa_lo) * sin(polar) ], $
	          [ sin(pa_lo) * sin(polar) ], $
	          [              cos(polar) ] ]
	ld_hi = [ [ cos(pa_hi) * sin(polar) ], $
	          [ sin(pa_hi) * sin(polar) ], $
	          [              cos(polar) ] ]
	pa    = !Null
	pa_lo = !Null
	pa_hi = !Null
	
	;Incident vectors
	;   Arrange [ncomponents, npts, npads]
	;   Incident particle trajectories are opposite to the look direction
	iv    = -transpose( temporary(ld)    )
	iv_lo = -transpose( temporary(ld_lo) )
	iv_hi = -transpose( temporary(ld_hi) )

;-----------------------------------------------------
; Interpolate B to EDI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;First rotate to GDU1 coordinates
	bcs2edi1 = mms_instr_xxyz2instr('BCS', 'EDI1')
	b_edi1   = MrVector_Rotate(bcs2edi1, temporary(b_bcs))

	;Inteprolate magnetic field to EDI
	b_edi1 = MrInterpol(b_edi1, temporary(t_fgm), edi.epoch_gdu1)

	;Unit vectors of Field-Aligned CS viewed from EDI1
	z_hat = MrVector_Normalize(temporary(b_edi1))
	
;-----------------------------------------------------
; Dot Look-Direction with B \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	pa    = fltarr(npts, 2)
	pa_lo = fltarr(npts, 2)
	pa_hi = fltarr(npts, 2)

	;GDU1
	pa[0,0]    = acos( MrVector_Dot(z_hat, iv    ) )
	pa_lo[0,0] = acos( MrVector_Dot(z_hat, iv_lo ) )
	pa_hi[0,0] = acos( MrVector_Dot(z_hat, iv_hi ) )
	
	;
	;GDU2 (Opposite to GDU1)
	; Anodes on GDU2 are in reverse order compared to anodes from GDU1
	; e.g., for packing mode 1, if B lies between anode 2 and 3
	;
	;                 GDU1                             GDU2
	;      channel  anode#   azimuth        channel  anode#    azimuth
	;      ----------------------------     ------------------------------
	;      counts1  p-2  [ 0.00, 11.25]     counts4  p+1  [168.75, 180.00]
	;      counts2  p-1  [11.25, 22.50]     counts3  p    [157.50, 168.75]
	;      counts3  p    [22.50, 33.75]     counts2  p-1  [146.25, 157.50]
	;      counts4  p+1  [33.75, 45.00]     counts1  p-2  [135.00, 146.25]
	;
	; This means that we have to interchange hi & lo and also count
	; down while GDU1 counts up. Doing so, the pitch angle data will
	; be ordered as
	;
	;        pa[*,0] = GDU1 counts1-4
	;        pa[*,4:7] = GDU2 counts1-4
	;
	pa[0,1]    = acos( MrVector_Dot(z_hat, -iv   ) )
	pa_lo[0,1] = acos( MrVector_Dot(z_hat, -iv_hi) )
	pa_hi[0,1] = acos( MrVector_Dot(z_hat, -iv_lo) )

	;Delete data
	z_hat = !Null
	iv    = !Null
	iv_lo = !Null
	iv_hi = !Null
	
;-----------------------------------------------------
; Convert to Degrees \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert back to degrees
	rad2deg = 180.0D / !dpi
	pa      = float(pa      * rad2deg)
	pa_lo   = float(pa_lo   * rad2deg)
	pa_hi   = float(pa_hi   * rad2deg)
	azimuth = float(azimuth * rad2deg)
	polar   = float(polar   * rad2deg)
	
;-----------------------------------------------------
; Timing Problems? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Check that PITCH_GDU1 and PA agree
	;   - PITCH_GDU1 is at a lower time cadence, so we may have missed
	;     a couple of points in the transition from 0 to 180
	;   - We have to swap GDU1 and GDU2 pitch angles, following the chart
	;     above.
	iswap   = where( abs(pa - edi.pitch_gdu1) gt 90.0, nswap)
	if nswap gt 0 then begin
		MrPrintF, 'LogText', nswap, float(nswap)/npts * 100.0, $
		          FORMAT='(%"Found %i (%0.3f\%) pitch angles that need swapping.")'
	
		;Swap center
		pa[iswap,*] = pa[iswap, 1:0:-1]
		
		;Copy only those elements that will be swapped
		lo_temp = pa_lo[iswap,*]
		hi_temp = pa_hi[iswap,*]
		
		;Swap elements
		pa_lo[iswap,*] = (temporary(hi_temp))[*, 1:0:-1]
		pa_hi[iswap,*] = (temporary(lo_temp))[*, 1:0:-1]
	endif
	
;-----------------------------------------------------
; Compuate Delta \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Right now, PA_LO and PA_HI contain the pitch angle of
	;the lower and upper edges of the anode. Here, we change
	;that to a change in pitch angle from the center of the
	;anode. In other words, a DELTA_MINUS and DELTA_PLUS, as
	;required by the CDF files.
	;
	pa_lo = pa    - pa_lo
	pa_hi = pa_hi - pa
	
;-----------------------------------------------------
; Sort by PA Instead of GDU \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find 0 degree pitch angle
	return, mms_edi_amb_srvy_sort_pa(edi, pa, pa_lo, pa_hi)
end
