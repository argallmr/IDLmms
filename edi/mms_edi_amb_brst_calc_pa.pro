; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_brst_calc_pa
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
;*****************************************************************************************
;+
;   Sort burst data by pad into 0 and 180 degree counts.
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
;                   An 8-column array with the pitch angle corresponding to the center
;                       of each active pad.
;      PA_LO:       in, required, type=8xN float
;                   An 8-column array with the pitch angle corresponding to the lower-edge
;                       of each active pad.
;      PA_HI:       in, required, type=8xN float
;                   An 8-column array with the pitch angle corresponding to the upper-edge
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
function mms_edi_amb_brst_sort_pa, edi, pa, pa_lo, pa_hi
	compile_opt idl2
	on_error, 2

	;Interpolate pitch
	;   - v0.6.0 and earlier
	;   - See mms_edi_amb_l1a_read, /EXPAND_ANGLES
	if n_elements(edi.epoch_angle) ne n_elements(edi.epoch_gdu1) $
		then message, 'EPOCH_ANGLE and EPOCH_GDU1 do not have same number of elements.'
	
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
		pa0    = [    pa[i0_gdu1,0:3],         pa[i0_gdu2,7:4:-1]   ]
		pa0_lo = [ pa_lo[i0_gdu1,0:3],      pa_lo[i0_gdu2,7:4:-1]   ]
		pa0_hi = [ pa_hi[i0_gdu1,0:3],      pa_hi[i0_gdu2,7:4:-1]   ]
		
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
		pa0    =    pa[i0_gdu1,*]
		pa0_lo = pa_lo[i0_gdu1,*]
		pa0_hi = pa_hi[i0_gdu1,*]
		gdu_0  = replicate(1B, n0_gdu1)
	
	;Only GDU2 data
	endif else if n0_gdu2 gt 0 then begin
		t_0    = edi.epoch_gdu2[i0_gdu2]
		pa0    =    pa[i0_gdu2,*]
		pa0_lo = pa_lo[i0_gdu2,*]
		pa0_hi = pa_hi[i0_gdu2,*]
		gdu_0  = replicate(2B, n0_gdu2)
	endif else begin
		MrPrintF, 'LogWarn', 'No 0 degree pitch angle data.'
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
		pa180    = [    pa[i180_gdu1,0:3],         pa[i180_gdu2,7:4:-1]   ]
		pa180_lo = [ pa_lo[i180_gdu1,0:3],      pa_lo[i180_gdu2,7:4:-1]   ]
		pa180_hi = [ pa_hi[i180_gdu1,0:3],      pa_hi[i180_gdu2,7:4:-1]   ]
		
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
		pa180    =    pa[i180_gdu1,*]
		pa180_lo = pa_lo[i180_gdu1,*]
		pa180_hi = pa_hi[i180_gdu1,*]
		gdu_180  = replicate(1B, n180_gdu1)
	
	;Only GDU2 data
	endif else if n180_gdu2 gt 0 then begin
		t_180    = edi.epoch_gdu2[i180_gdu2]
		pa180    =    pa[i180_gdu2,*]
		pa180_lo = pa_lo[i180_gdu2,*]
		pa180_hi = pa_hi[i180_gdu2,*]
		gdu_180  = replicate(2B, n180_gdu2)
	endif else begin
		MrPrintF, 'LogWarn', 'No 0 degree pitch angle data.'
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
function mms_edi_amb_brst_calc_pa_v2, edi, fgm
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Check Packing Mode \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Check the packing mode
	if total(edi.pack_mode[1,*] - edi.pack_mode) ne 0 $
		then message, 'Packing mode changes. How do I deal with this?' $
		else pack_mode = pack_mode[0]

	;Is it really packing mode 1?
	if pack_mode ne 1 then message, 'Unexpected packing mode.'

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
	iangle     = reform(value_locate(edi.epoch_angle, edi.epoch_gdu1))
	azimuth    = edi.azimuth[iangle]
	polar      = edi.polar[iangle]
	npts       = n_elements(azimuth)

;-----------------------------------------------------
; Anode Operations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
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
	
	;Anodes and their phi-angle
	nanodes = 32
	anodes  = indgen(nanodes)
	phi     = anodes * 11.25
	
	;Center anode for GDU1, and its complement on GDU2
	p_gdu1  = value_locate(phi, azimuth)
;	p_gdu2  = 16 - p_gdu1
;	p_gdu2 += (p_gdu2 lt 0) * 32

	;Pitch angles of each pad
	case pack_mode of
		1: offset = [-2, -1,  0,  1]
		2: offset = [ 0, -1, -2, -3]
		else: message, 'Invalid packing mode.'
	endcase
	npads = n_elements(offset)
	
	pad    = fltarr(npts, 2*npads)
	pad_lo = fltarr(npts, 2*npads)
	pad_hi = fltarr(npts, 2*npads)
	for i = 0, npads - 1 do begin
		pad_lo[*,i] = (p_gdu1 + offset[i])       * 11.25
		pad[*,i]    = (p_gdu1 + offset[i] + 0.5) * 11.25
		pad_hi[*,i] = (p_gdu1 + offset[i] + 1.0) * 11.25
	endfor
	for i = 0, npads - 1 do begin
		ipad           = (2*npads-1) - i
		pad_lo[*,ipad] = (16 - p_gdu1 + offset[i])       * 11.25
		pad[*,ipad]    = (16 - p_gdu1 + offset[i] + 0.5) * 11.25
		pad_hi[*,ipad] = (16 - p_gdu1 + offset[i] + 1.0) * 11.25
	endfor
	
	;Adjust
	pad    -= (pad    lt 0.0) * 360.0
	pad_lo -= (pad_lo lt 0.0) * 360.0
	pad_hi -= (pad_hi lt 0.0) * 360.0
	pad    += (pad    gt 0.0) * 360.0
	pad_lo += (pad_lo gt 0.0) * 360.0
	pad_hi += (pad_hi gt 0.0) * 360.0

;-----------------------------------------------------
; Voltage Operations\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Would have to do the same thing for THETA as we 
	; did for PHI.
	;
	
;-----------------------------------------------------
; Look Directions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert to radians
	deg2rad  = !dpi / 180.0D
	pa      *= deg2rad
	pa_lo   *= deg2rad
	pa_hi   *= deg2rad
	azimuth *= deg2rad

	;Vector for each pad
	fv    = [ [[ cos(pa)    * sin(polar) ]], $
	          [[ sin(pa)    * sin(polar) ]], $
	          [[              cos(polar) ]] ]
	fv_lo = [ [[ cos(pa_lo) * sin(polar) ]], $
	          [[ sin(pa_lo) * sin(polar) ]], $
	          [[              cos(polar) ]] ]
	fv_hi = [ [[ cos(pa_hi) * sin(polar) ]], $
	          [[ sin(pa_hi) * sin(polar) ]], $
	          [[              cos(polar) ]] ]
	pa    = !Null
	pa_lo = !Null
	pa_hi = !Null
	
	;Arrange [ncomponents, npts, npads]
	;   Incident particle trajectories are opposite to the look direction
	fv    = -transpose(fv,    [2, 0, 1])
	fv_lo = -transpose(fv_lo, [2, 0, 1])
	fv_hi = -transpose(fv_hi, [2, 0, 1])

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
	;
	; TODO: This was never finished. Copy version 1.
	;

	;Allocate memory for both GDUs
	pa    = fltarr(npts, 2*npads)
	pa_lo = fltarr(npts, 2*npads)
	pa_hi = fltarr(npts, 2*npads)
	
	;GDU1
	for i = 0, npads-1 do begin
		pa[*,i]    = acos( MrVector_Dot(z_hat, fv[*,*,i]) )
		pa_lo[*,i] = acos( MrVector_Dot(z_hat, fv_lo[*,*,i]) )
		pa_hi[*,i] = acos( MrVector_Dot(z_hat, fv_hi[*,*,i]) )
	endfor
	
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
	;        pa[*,0:3] = GDU1 counts1-4
	;        pa[*,4:7] = GDU2 counts1-4
	;
	for i = 0, npads-1 do begin
		ipad           = (2*npads-1) - i
		pa[*,ipads]    = acos( MrVector_Dot(z_hat, -fv[*,*,i]) )
		pa_lo[*,ipads] = acos( MrVector_Dot(z_hat, -fv_hi[*,*,i]) )
		pa_hi[*,ipads] = acos( MrVector_Dot(z_hat, -fv_lo[*,*,i]) )
	endfor
	z_hat = !Null
	fv    = !Null
	fv_lo = !Null
	fv_hi = !Null
	
	;Check that PITCH_GDU1 and PA agree
	;   - PITCH_GDU1 is at a lower time cadence, so we may have missed
	;     a couple of points in the transition from 0 to 180
	;   - We have to swap GDU1 and GDU2 pitch angles, following the chart
	;     above.
	icenter = where(offset eq 0)
	iswap   = where( abs(pa[*,icenter] - edi.pitch_gdu1) gt !pi/4, nswap)
	if nswap gt 0 then begin
		;Swap center
		pa = pa[iswap, 7:0:-1]
		
		;Copy only those elements that will be swapped
		lo_temp = pa_lo[iswap,*]
		hi_temp = pa_hi[iswap,*]
		
		;Swap elements
		pa_lo[iswap,*] = (temporary(hi_temp))[*, 7:0:-1]
		pa_hi[iswap,*] = (temporary(lo_temp))[*, 7:0:-1]
	endif
	
;-----------------------------------------------------
; Convert to Degrees \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert back to degrees
	rad2deg = 180.0D / !dpi
	pa      = float(pa         * rad2deg)
	pa_lo   = float(pa_lo      * rad2deg)
	pa_hi   = float(pa_hi      * rad2deg)
	azimuth = float(azimuth    * rad2deg)
	polar   = float(polar[*,0] * rad2deg)
	
;-----------------------------------------------------
; Sort by PA Instead of GDU \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find 0 degree pitch angle
	return, edi_amb_brst_pa_0_180(edi, pa, pa_lo, pa_hi)
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
function mms_edi_amb_brst_calc_pa, edi, fgm
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Check Packing Mode \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Check the packing mode
	if total(edi.pack_mode[1:*] - edi.pack_mode) ne 0 $
		then message, 'Packing mode changes. How do I deal with this?' $
		else pack_mode = edi.pack_mode[0]
	
	;Only know about pack_mode 1
	if pack_mode ne 1 then message, 'Unknown packing mode (' + string(pack_mode, FORMAT='(i0)') + ').'

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
	iangle     = reform(value_locate(edi.epoch_angle, edi.epoch_gdu1))
	azimuth    = edi.azimuth[iangle]
	polar      = edi.polar[iangle]
	npts       = n_elements(azimuth)

;-----------------------------------------------------
; Annode Operations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
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
	

	;Offset of each pad in anode units
	case pack_mode of
		1: offset = [-2, -1,  0,  1]
		2: offset = [ 0, -1, -2, -3]
		else: message, 'Invalid packing mode.'
	endcase
	npads = n_elements(offset)
	
;-----------------------------------------------------
; Azimuth of Edges & Center of Anodes \\\\\\\\\\\\\\\\
;-----------------------------------------------------
	npts = n_elements(azimuth)
	
	;Angle of the lower edge of each pad
	pa    = fltarr(npts, npads)
	pa_lo = fltarr(npts, npads)
	pa_hi = fltarr(npts, npads)
	for i = 0, npads-1 do begin
		;Azimuth of upper and lower edges
		pa_lo[0,i] = azimuth +  offset[i]        * 11.25
		pa[0,i]    = azimuth + (offset[i] + 0.5) * 11.25
		pa_hi[0,i] = azimuth + (offset[i] + 1.0) * 11.25

		;Force to range [0,360)
		if offset[i] lt 0 then begin
			pa[*,i]    += (pa[*,i]    lt 0) * 360.0
			pa_lo[*,i] += (pa_lo[*,i] lt 0) * 360.0
			pa_hi[*,i] += (pa_hi[*,i] lt 0) * 360.0
		endif else begin
			pa[*,i]     =  pa[*,i]    mod 360.0
			pa_lo[*,i]  =  pa_lo[*,i] mod 360.0
			pa_hi[*,i]  =  pa_hi[*,i] mod 360.0
		endelse
	endfor

;-----------------------------------------------------
; Look & Incident Vectors \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert to radians
	deg2rad  = !dpi / 180.0D
	pa      *= deg2rad
	pa_lo   *= deg2rad
	pa_hi   *= deg2rad
	azimuth *= deg2rad
	polar    = rebin(polar*deg2rad, npts, npads)

	;Look Direction
	ld    = [ [[ cos(pa)    * sin(polar) ]], $
	          [[ sin(pa)    * sin(polar) ]], $
	          [[              cos(polar) ]] ]
	ld_lo = [ [[ cos(pa_lo) * sin(polar) ]], $
	          [[ sin(pa_lo) * sin(polar) ]], $
	          [[              cos(polar) ]] ]
	ld_hi = [ [[ cos(pa_hi) * sin(polar) ]], $
	          [[ sin(pa_hi) * sin(polar) ]], $
	          [[              cos(polar) ]] ]
	pa    = !Null
	pa_lo = !Null
	pa_hi = !Null
	
	;Incident vectors
	;   Arrange [ncomponents, npts, npads]
	;   Incident particle trajectories are opposite to the look direction
	iv    = -transpose( temporary(ld),    [2, 0, 1])
	iv_lo = -transpose( temporary(ld_lo), [2, 0, 1])
	iv_hi = -transpose( temporary(ld_hi), [2, 0, 1])

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
	;Allocate memory for both GDUs
	pa    = fltarr(npts, 2*npads)
	pa_lo = fltarr(npts, 2*npads)
	pa_hi = fltarr(npts, 2*npads)
	
	;GDU1
	for i = 0, npads-1 do begin
		pa[*,i]    = acos( MrVector_Dot(z_hat, iv[*,*,i]) )
		pa_lo[*,i] = acos( MrVector_Dot(z_hat, iv_lo[*,*,i]) )
		pa_hi[*,i] = acos( MrVector_Dot(z_hat, iv_hi[*,*,i]) )
	endfor
	
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
	;        pa[*,0:3] = GDU1 counts1-4
	;        pa[*,4:7] = GDU2 counts1-4
	;
	for i = 0, npads-1 do begin
		ipad          = (2*npads-1) - i
		pa[*,ipad]    = acos( MrVector_Dot(z_hat, -iv[*,*,i]) )
		pa_lo[*,ipad] = acos( MrVector_Dot(z_hat, -iv_hi[*,*,i]) )
		pa_hi[*,ipad] = acos( MrVector_Dot(z_hat, -iv_lo[*,*,i]) )
	endfor
	z_hat = !Null
	iv    = !Null
	iv_lo = !Null
	iv_hi = !Null
	
;-----------------------------------------------------
; Convert to Degrees \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert back to degrees
	rad2deg = 180.0D / !dpi
	pa      = float(pa         * rad2deg)
	pa_lo   = float(pa_lo      * rad2deg)
	pa_hi   = float(pa_hi      * rad2deg)
	azimuth = float(azimuth    * rad2deg)
	polar   = float(polar[*,0] * rad2deg)
	
;-----------------------------------------------------
; Timing Problems? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Check that PITCH_GDU1 and PA agree
	;   - PITCH_GDU1 is at a lower time cadence, so we may have missed
	;     a couple of points in the transition from 0 to 180
	;   - We have to swap GDU1 and GDU2 pitch angles, following the chart
	;     above.
	icenter = where(offset eq 0)
	iswap   = where( abs(pa[*,icenter] - edi.pitch_gdu1) gt 90.0, nswap)
	if nswap gt 0 then begin
		MrPrintF, 'LogText', nswap, float(nswap)/npts * 100.0, $
		          FORMAT='(%"Found %i (%0.3f\%) pitch angles that need swapping.")'
	
		;Swap center
		pa[iswap, *] = pa[iswap, 7:0:-1]
		
		;Copy only those elements that will be swapped
		lo_temp = pa_lo[iswap,*]
		hi_temp = pa_hi[iswap,*]
		
		;Swap elements
		pa_lo[iswap,*] = (temporary(hi_temp))[*, 7:0:-1]
		pa_hi[iswap,*] = (temporary(lo_temp))[*, 7:0:-1]
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
	return, mms_edi_amb_brst_sort_pa(edi, pa, pa_lo, pa_hi)
end