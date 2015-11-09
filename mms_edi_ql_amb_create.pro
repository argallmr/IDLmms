; docformat = 'rst'
;
; NAME:
;    mms_edi_ql_amb_create
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Process EDI AMB L1A data, sorting counts by pitch angle instead of GDU and,
;   for burst data, calculate the pitch angle of each anode.
;
; :Categories:
;    MMS, EDI
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
;       2015/10/27  -   Written by Matthew Argall
;       2015/11/04  -   Calculate pitch angle for ambient data. - MRA
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
function edi_amb_brst_pa_0_180, edi, pa, pa_lo, pa_hi
	compile_opt idl2
	on_error, 2
	
	;Interpolate pitch
	;   - Same method as for AZIMUTH
;	iangle     = value_locate(edi.epoch_angle, edi.epoch_gdu1)
;	pitch_gdu1 = edi.pitch_gdu1[iangle]
;	pitch_gdu2 = edi.pitch_gdu2[iangle]
	
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
function edi_amb_brst_pa_v2, edi, fgm
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
function edi_amb_brst_pa, edi, fgm
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
	return, edi_amb_brst_pa_0_180(edi, pa, pa_lo, pa_hi)
end


;+
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
function edi_amb_srvy_pa_0_180, edi, pa, pa_lo, pa_hi
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
function edi_amb_srvy_pa, edi, fgm, $
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
	return, edi_amb_srvy_pa_0_180(edi, pa, pa_lo, pa_hi)
end


;+
;   Sort burst mode data by pitch angle instead of by GDU.
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
;                       TT2000_0    - Time tags for 0 degree pitch angle counts
;                       TT2000_180  - Time tags for 180 degree pitch angle counts
;                       COUNTS1_0   - counts1_gdu[12] sorted by pitch angle 0.
;                       COUNTS2_0   - counts2_gdu[12] sorted by pitch angle 0.
;                       COUNTS3_0   - counts3_gdu[12] sorted by pitch angle 0.
;                       COUNTS4_0   - counts4_gdu[12] sorted by pitch angle 0.
;                       COUNTS1_180 - counts1_gdu[12] sorted by pitch angle 180.
;                       COUNTS2_180 - counts2_gdu[12] sorted by pitch angle 180.
;                       COUNTS3_180 - counts3_gdu[12] sorted by pitch angle 180.
;                       COUNTS4_180 - counts4_gdu[12] sorted by pitch angle 180.
;                       GDU_0       - Flag to sort PA 0 counts by GDU.
;                       GDU_180     - Flag to sort PA 180 counts by GDU.
;-
function edi_amb_brst_0_180, edi
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
; EDI: Sort By Pitch Angle 0 \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Find 0 and 180 pitch angles
	i0_gdu1   = where(edi.pitch_gdu1 eq 0, n0_gdu1)
	i0_gdu2   = where(edi.pitch_gdu2 eq 0, n0_gdu2)

	;Select 0 pitch angle
	if n0_gdu1 gt 0 && n0_gdu2 gt 0 then begin
		t_0       = [ edi.epoch_gdu1[i0_gdu1],   edi.epoch_gdu2[i0_gdu2] ]
		counts1_0 = [ edi.counts1_gdu1[i0_gdu1], edi.counts1_gdu2[i0_gdu2] ]
		counts2_0 = [ edi.counts2_gdu1[i0_gdu1], edi.counts2_gdu2[i0_gdu2] ]
		counts3_0 = [ edi.counts3_gdu1[i0_gdu1], edi.counts3_gdu2[i0_gdu2] ]
		counts4_0 = [ edi.counts4_gdu1[i0_gdu1], edi.counts4_gdu2[i0_gdu2] ]
	
		;Sort times
		isort     = sort(t_0)
		t_0       = t_0[isort]
		counts1_0 = counts1_0[isort]
		counts2_0 = counts2_0[isort]
		counts3_0 = counts3_0[isort]
		counts4_0 = counts4_0[isort]
	
		;Mark GDU
		gdu_0          = bytarr(n0_gdu1 + n0_gdu2)
		gdu_0[i0_gdu1] = 1B
		gdu_0[i0_gdu2] = 2B

	;Only GDU1 data
	endif else if n0_gdu1 gt 0 then begin
		t_0       = edi.epoch_gdu1[i0_gdu1]
		counts1_0 = edi.counts1_gdu1[i0_gdu1]
		counts2_0 = edi.counts2_gdu1[i0_gdu1]
		counts3_0 = edi.counts3_gdu1[i0_gdu1]
		counts4_0 = edi.counts4_gdu1[i0_gdu1]
		gdu_0     = replicate(1B, n0_gdu1)

	;Only GDU2 data
	endif else if n0_gdu2 gt 0 then begin
		t_0       = edi.epoch_gdu2[i0_gdu2]
		counts1_0 = edi.counts1_gdu2[i0_gdu2]
		counts2_0 = edi.counts2_gdu2[i0_gdu2]
		counts3_0 = edi.counts3_gdu2[i0_gdu2]
		counts4_0 = edi.counts4_gdu2[i0_gdu2]
		gdu_0     = replicate(2B, n0_gdu2)

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
		t_180       = [ edi.epoch_gdu1[i180_gdu1],   edi.epoch_gdu2[i180_gdu2] ]
		counts1_180 = [ edi.counts1_gdu1[i180_gdu1], edi.counts1_gdu2[i180_gdu2] ]
		counts2_180 = [ edi.counts2_gdu1[i180_gdu1], edi.counts2_gdu2[i180_gdu2] ]
		counts3_180 = [ edi.counts3_gdu1[i180_gdu1], edi.counts3_gdu2[i180_gdu2] ]
		counts4_180 = [ edi.counts4_gdu1[i180_gdu1], edi.counts4_gdu2[i180_gdu2] ]
	
		;Sort times
		isort       = sort(t_180)
		t_180       = t_180[isort]
		counts1_180 = counts1_180[isort]
		counts2_180 = counts2_180[isort]
		counts3_180 = counts3_180[isort]
		counts4_180 = counts4_180[isort]
	
		;Mark GDU
		gdu_180            = bytarr(n180_gdu1 + n180_gdu2)
		gdu_180[i180_gdu1] = 1B
		gdu_180[i180_gdu2] = 2B

	;Only GDU1 data
	endif else if n180_gdu1 gt 0 then begin
		t_180       = edi.epoch_gdu1[i180_gdu1]
		counts1_180 = edi.counts1_gdu1[i180_gdu1]
		counts2_180 = edi.counts2_gdu1[i180_gdu1]
		counts3_180 = edi.counts3_gdu1[i180_gdu1]
		counts4_180 = edi.counts4_gdu1[i180_gdu1]
		gdu_180     = replicate(1B, n180_gdu1)

	;Only GDU2 data
	endif else if n180_gdu2 gt 0 then begin
		t_180       = edi.epoch_gdu2[i180_gdu2]
		counts1_180 = edi.counts1_gdu2[i180_gdu2]
		counts2_180 = edi.counts2_gdu2[i180_gdu2]
		counts3_180 = edi.counts3_gdu2[i180_gdu2]
		counts4_180 = edi.counts4_gdu2[i180_gdu2]
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
	
	edi_out = { tt2000_0:    temporary(t_0),         $
	            tt2000_180:  temporary(t_180),       $
	            counts1_0:   temporary(counts1_0),   $
	            counts2_0:   temporary(counts2_0),   $
	            counts3_0:   temporary(counts3_0),   $
	            counts4_0:   temporary(counts4_0),   $
	            counts1_180: temporary(counts1_180), $
	            counts2_180: temporary(counts2_180), $
	            counts3_180: temporary(counts3_180), $
	            counts4_180: temporary(counts4_180), $
	            gdu_0:       temporary(gdu_0),       $
	            gdu_180:     temporary(gdu_180)      $
	          }

	return, edi_out
end


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
;                       TT2000_0    - Time tags for 0 degree pitch angle counts
;                       TT2000_180  - Time tags for 180 degree pitch angle counts
;                       COUNTS1_0   - counts1_gdu[12] sorted by pitch angle 0.
;                       COUNTS1_180 - counts1_gdu[12] sorted by pitch angle 180.
;                       GDU_0       - Flag to sort PA 0 counts by GDU.
;                       GDU_180     - Flag to sort PA 180 counts by GDU.
;-
function edi_amb_srvy_0_180, edi
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
; EDI: Sort By Pitch Angle 0 \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Find 0 and 180 pitch angles
	i0_gdu1   = where(edi.pitch_gdu1 eq 0, n0_gdu1)
	i0_gdu2   = where(edi.pitch_gdu2 eq 0, n0_gdu2)

	;Select 0 pitch angle
	if n0_gdu1 gt 0 && n0_gdu2 gt 0 then begin
		t_0      = [ edi.epoch_gdu1[i0_gdu1],   edi.epoch_gdu2[i0_gdu2] ]
		counts_0 = [ edi.counts1_gdu1[i0_gdu1], edi.counts1_gdu2[i0_gdu2] ]
	
		;Sort times
		isort    = sort(t_0)
		t_0      = t_0[isort]
		counts_0 = counts_0[isort]
	
		;Mark GDU
		gdu_0          = bytarr(n0_gdu1 + n0_gdu2)
		gdu_0[i0_gdu1] = 1B
		gdu_0[i0_gdu2] = 2B

	;Only GDU1 data
	endif else if n0_gdu1 gt 0 then begin
		t_0      = edi.epoch_gdu1[i0_gdu1]
		counts_0 = edi.counts1_gdu1[i0_gdu1]
		gdu_0    = replicate(1B, n0_gdu1)

	;Only GDU2 data
	endif else if n0_gdu2 gt 0 then begin
		t_0      = edi.epoch_gdu2[i0_gdu2]
		counts_0 = edi.counts1_gdu2[i0_gdu2]
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
		t_180      = [ edi.epoch_gdu1[i180_gdu1],   edi.epoch_gdu2[i180_gdu2] ]
		counts_180 = [ edi.counts1_gdu1[i180_gdu1], edi.counts1_gdu2[i180_gdu2] ]
	
		;Sort times
		isort      = sort(t_180)
		t_180      = t_180[isort]
		counts_180 = counts_180[isort]
	
		;Mark GDU
		gdu_180            = bytarr(n180_gdu1 + n180_gdu2)
		gdu_180[i180_gdu1] = 1B
		gdu_180[i180_gdu2] = 2B

	;Only GDU1 data
	endif else if n180_gdu1 gt 0 then begin
		t_180      = edi.epoch_gdu1[i180_gdu1]
		counts_180 = edi.counts1_gdu1[i180_gdu1]
		gdu_180    = replicate(1B, n180_gdu1)

	;Only GDU2 data
	endif else if n180_gdu2 gt 0 then begin
		t_180      = edi.epoch_gdu2[i180_gdu2]
		counts_180 = edi.counts1_gdu2[i180_gdu2]
		gdu_180    = replicate(2B, n180_gdu2)
	
	;No EDI data
	endif else begin
		MrPrintF, 'LogText', 'No 180 degree pitch angle data.'
		t_180 = 0LL
		counts_180 = -1S
	endelse

;-----------------------------------------------------
; Return Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	edi_out = { tt2000_0:    temporary(t_0),        $
	            tt2000_180:  temporary(t_180),      $
	            counts1_0:   temporary(counts_0),   $
	            counts1_180: temporary(counts_180), $
	            gdu_0:       temporary(gdu_0),      $
	            gdu_180:     temporary(gdu_180)     $
	          }

	return, edi_out
end


;+
;   Process EDI AMB L1A data, sorting counts by pitch angle instead of GDU and,
;   for burst data, calculate the pitch angle of each anode.
;
;   Calling Sequences:
;       fname = mms_edi_ql_amb_create( edi_file)
;       fname = mms_edi_ql_amb_create( edi_file, fgm_file)
;       fname = mms_edi_ql_amb_create( sc, mode, tstart, tend)
;
; :Params:
;       SC:         in, required, type=string/strarr
;                   Either the spacecraft identifier ('mms1', 'mms2', 'mms3', 'mms4')
;                       of the spacecraft for which to process data or the EDI data
;                       file(s) to be processed. If files, they may be 'fast' and/or 'slow'
;                       mode data files.
;       MODE:       in, required, type=string/strarr
;                   Either the mode ('srvy', 'brst') of data to process or FGM
;                       data file names used to calculate pitch angle if 'brst' files
;                       are given for `SC`. 
;       TSTART:     in, optional, types=string
;                   An ISO-8601 string indicating the start time of the interval to process.
;       TEND:       in, optional, types=string
;                   An ISO-8601 string indicating the end time of the interval to process.
;
; :Keywords:
;       OUTDIR:     in, optional, type=string, default='/nfs/edi/amb/'
;                   Directory in which to save data.
;
; :Returns:
;       FILENAME:   The name of the file produced.
;-
function mms_edi_ql_amb_create, sc, mode, tstart, tend, $
OUTDIR=outdir, $
CREATE_LOG=create_log
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, ''
	endif

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Defaults
	create_log = keyword_set(create_log)
	if n_elements(outdir) eq 0 then outdir = '/nfs/edi/amb/'

	;Number of parameters with data
	nparams = n_elements(sc)         eq 0 ? 0 : $
	              n_elements(mode)   eq 0 ? 1 : $
	              n_elements(tstart) eq 0 ? 2 : $
	              n_elements(tend)   eq 0 ? 3 : $
	              4
	
	;How was the program called?
	;   1: mms_edi_ql_amb_create, edi_file
	;   2: mms_edi_ql_amb_create, edi_file, fgm_file
	;   4: mms_edi_ql_amb_create, sc, mode, tstart, tend
	case nparams of
		1: begin
			file_edi       = sc
			mms_dissect_filename, file_edi, SC=_sc, MODE=_mode
			if _mode[0] ne 'brst' then begin
				if total((_mode eq 'fast') + (_mode eq 'slow')) ne n_elements(_mode) $
					then message, 'Files must be "brst" or ["slow", "fast"] mode.'
			endif
			tf_pitch_angle = 0B
		endcase
		2: begin
			file_edi       = sc
			file_fgm       = mode
			mms_dissect_filename, file_edi, SC=_sc, MODE=_mode
			if _mode[0] ne 'brst' then begin
				if total((_mode eq 'fast') + (_mode eq 'slow')) ne n_elements(_mode) $
					then message, 'Files must be "brst" or ["slow", "fast"] mode.'
			endif
			tf_pitch_angle = 1B
		endcase
		4: begin
			_sc = sc
			if ~(mode eq 'srvy' || mode eq 'brst') then message, 'MODE must be "brst" or "srvy".'
			_mode          = mode eq 'srvy' ? ['slow', 'fast'] : mode
			tf_pitch_angle = 1B
		endcase
		else:   message, 'Incorrect number of parameters'
	endcase
	tf_brst = _mode[0] eq 'brst'

	;Check inputs
	if tf_brst then begin
		if ~isa(file_edi, /SCALAR, 'STRING') then message, 'Only one EDI file allowed.'
		if ~file_test(file_edi)              then message, 'EDI file does not exist: "' + file_edi + '".'
	endif else begin
		if ~isa(_sc,    /SCALAR, 'STRING') then message, 'SC must be a scalar string.'
		if ~isa(tstart, /SCALAR, 'STRING') then message, 'TSTART must be a scalar string.'
		if ~isa(tend,   /SCALAR, 'STRING') then message, 'TEND must be a scalar string.'
	endelse  

;-----------------------------------------------------
; Sort into 0 and 180 Degree PA \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;EDI QL FAST file
	if n_elements(file_edi) eq 0 then begin
		file_edi = mms_find_file(_sc, 'edi', _mode, 'l1a', $
		                         COUNT     = nedi, $
		                         OPTDESC   = 'amb', $
		                         SEARCHSTR = searchstr, $
		                         TSTART    = tstart, $
		                         TEND      = tend)
		if nedi eq 0 then message, 'EDI files not found: "' + searchstr[0] + '".'
	endif

	;Read Data
	edi = mms_edi_read_l1a_amb(file_edi, tstart, tend)

	;Sort by 0 and 180 pitch angles
	if tf_brst $
		then counts_0_180 = edi_amb_brst_0_180(edi) $
		else counts_0_180 = edi_amb_srvy_0_180(edi)

;-----------------------------------------------------
; Burst Mode \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Search for the fgm file
	nfgm = n_elements(file_fgm)
	if tf_pitch_angle && nfgm eq 0 then begin
		;Try L2PRE first
		fg_mode  = tf_brst ? 'brst' : 'srvy'
		file_fgm = mms_find_file(_sc, 'dfg', fg_mode, 'l2pre', $
		                         COUNT     = nfgm, $
		                         SEARCHSTR = searchstr, $
		                         TSTART    = tstart, $
		                         TEND      = tend)
		
		;Try L1B
		if nfgm eq 0 then begin
			file_fgm = mms_find_file(_sc, 'dfg', fg_mode, 'l1b', $
			                         COUNT     = nfgm, $
			                         SEARCHSTR = searchstr, $
			                         TSTART    = tstart, $
			                         TEND      = tend)
		endif
	endif
	
	;Do we have FGM data?
	if nfgm eq 0 then begin
		MrPrintF, 'LogErr', 'Cannot determine pitch angles. No FGM data.'
		tf_pitch_angle = 0B
	endif else begin
		;Read FGM data
		if tf_brst then begin
			fgm      = mms_fg_read_l1b(file_fgm, tstart, tend)
			pa_0_180 = edi_amb_brst_pa(edi, temporary(fgm))
		endif else begin
			fgm      = mms_fg_read_l2pre(file_fgm, tstart, tend)
			pa_0_180 = edi_amb_srvy_pa(edi, temporary(fgm))
		endelse
	endelse

;-----------------------------------------------------
; Output Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Create the output structure
	edi_out = { tt2000_tt:     reform(edi.epoch_timetag), $
	            energy_gdu1:   reform(edi.energy_gdu1), $
	            energy_gdu2:   reform(edi.energy_gdu2) $
	          }
	edi = !Null

	;Burst mode counts
	edi_out = create_struct(edi_out, temporary(counts_0_180))
	
	;Pitch angle (burst only)
	if tf_pitch_angle then edi_out = create_struct(edi_out, temporary(pa_0_180))

;-----------------------------------------------------
; Write to File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if _mode eq 'brst' then begin
		mms_dissect_filename, file_edi, TSTART=fstart
		MrTimeParser, fstart, '%Y%M%d%H%m%S', '%Y-%M-%dT%H:%m:%S', ffstart
	endif else begin
		ffstart = tstart
	endelse

	;Gather metadata
	meta_data = { sc:        _sc, $
	              instr:     'edi', $
	              mode:      (tf_brst ? 'brst' : 'srvy'), $
	              mods:      (n_elements(mods) eq 0 ? '' : mods), $
	              level:     'l1b', $
	              optdesc:   'amb', $
	              tstart:    ffstart, $
	              parents:   (nfgm eq 0 ? file_edi : [file_edi, file_fgm]), $
	              directory: outdir $
	            }

	;Write the file
	amb_file = mms_edi_ql_amb_write(meta_data, edi_out)
	return, amb_file
end