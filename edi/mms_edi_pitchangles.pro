; docformat = 'rst'
;
; NAME:
;    mms_edi_pitchangles
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
;   Create a quick-look plot of the magnetic and electric fields.
;
; :Categories:
;    MMS, QL
;
; :Params:
;
; :Keywords:
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 348
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/03/15  -   Written by Matthew Argall
;-
function mms_edi_PA_percent, pa_lo, pa_hi, pa_bins
PA_BINS=pa_bins
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		void = cgErrorMSG(/QUIET)
		return, !Null
	endif
	
	;Create the pitch angle bins
	bin_size = mean(pa_bins[1:*] - pa_bins)
	nBins = n_elements(pa_bins)
	
	;Range if each pitch angle bin covered by PA_HI and PA_LO
	;   Assign fill values to that initial range comparisons work below.
	range      = fltarr(nBins, 2) - 1
	range[*,0] =  10
	range[*,1] = -10
	
	;Locate the pitch angles within the bin sectors
	ilo = value_locate(pa_bins[0:nBins-2], pa_lo)
	ihi = value_locate(pa_bins[1:nBins-1], pa_hi)+1

	;What percent of each pitch angle sector is covered?
	for i = 0, n_elements(pa_lo) - 1 do begin
		lo = min( [pa_lo[i], pa_hi[i]], imin, MAX=hi )
		j = imin eq 0 ? ilo[i] : ihi[i] ;Bin number into which lo fell
		k = imin eq 0 ? ihi[i] : ilo[i] ;Bin number into which hi fell
	
		;Pick the minimum and maximum range
		range[j,0] = range[j,0] < lo
		range[k,1] = range[k,1] > hi

		;Are lo and hi in different pitch angle bins?
		if j ne k then begin
			;Set the upper (lower) limit for the lo (hi) bin.
			range[j,1] = pa_bins[j+1]
			range[k,0] = pa_bins[k]
			
			;Completely fill all bins between
			if k - j gt 1 then begin
				range[j+1:k-1,0] = pa_bins[j+1:k-1]
				range[j+1:k-1,1] = pa_bins[j+2:k]
			endif
		endif
	endfor
	
	;Bins that were not filled
	iempty = where(range[*,0] eq 10 and range[*,1] eq -10, nempty)
	if nempty gt 0 then range[iempty,*] = 0

	;Calculate percents
	pct = 100.0 * ( range[*,1] - range[*,0] ) / bin_size

	return, pct
end



;+
;   Create a quick-look plot of the magnetic and electric fields.
;
; :Categories:
;    MMS, QL
;
; :Params:
;       B:              in, required, type=3xN float
;                       Magnetic field data.
;       POLAR_LOOKDIR:  in, optional, type=float, default=
;                       Polar angle (rad) along which the GDU is looking. The default
;                           is to look along direction defined by the first vector
;                           in `B`.
;
; :Keywords:
;       BPAD:           out, optional, type=fltarr
;                       Lower pitch angle (deg) of the pad on which `B` is centered.
;       GDU2:           out, optional, type=fltarr
;                       
;       LOOK_POLAR:     out, optional, type=fltarr
;                       The actual look directions used. This is different from
;                           `POLAR_LOOKDIR` only if it is a scalar and `B` is a vector.
;                           in this case, the look direction is replicated for each
;                           vector in `B`.
;       NPADS:          in, optional, type=fltarr
;                       Number of pads distributed around the 360 degrees of azimuthal
;                           look direction.
;       PA_BINS:        out, optional, type=fltarr
;                       Pitch angle bins used when calculating `PERCENT`.
;       PERCENT:        out, optional, type=fltarr
;                       The percentage of each 10 degree pitch-angle bin that is
;                           covered by the GDU pads for each vector in `B`.
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 348
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/03/15  -   Written by Matthew Argall
;-
function mms_edi_pitchangles, B, polar_lookdir, $
BPAD=bPad, $
GDU2=gdu2, $
LOOK_POLAR=pad_polar, $
NPADS=nPads, $
PA_BINS=pa_bins, $
PERCENT=percent
	compile_opt idl2
;	on_error, 2
	
	sz    = size(B)
	ndims = sz[0]
	dims  = sz[1:ndims]
	nPts  = ndims eq 1 ? 1 : sz[2]
	nLook = n_elements(polar_lookdir)
	if ndims eq 1 && dims[0] ne 3 then message, 'B must be a 3-element vector.'
	if ndims eq 2 && dims[0] ne 3 then message, 'B must be a 3xN array.'
;	if nLook gt 1                 then message, 'POLAR_LOOKDIR must be scalar.'
	
	;Do we want to know about GDU2?
	tf_gdu2 = keyword_set(gdu2)
	
	;Azimuthal angles to edges and center of each pad
	if n_elements(nPads) eq 0 then nPads = 32       ;Number of pads
	pad_lo  =  findgen(nPads)    / nPads * 2*!pi    ;Angle to the edges of each pad
	pad_hi  = (findgen(nPads)+1) / nPads * 2*!pi    ;Angle to the edges of each pad

;-----------------------------------------------------
; Identify the Pad to which B Points \\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Normalize B and find spherical angles
	b_hat   = MrVector_Normalize(B)
	azimuth = atan( b_hat[1,*], b_hat[0,*] )
	polar   = acos(b_hat[2,*]) * !radeg
	
	;Positive angles
	;   * atan(y,x) returns values between -180 and 180
	;   * Shift to 0 - 360
	azimuth += (azimuth lt 0) * 360.0

	;Locate B within the pad layout
	bPad = value_locate(pad_lo, azimuth)
	bPad = pad_lo[bPad]

;-----------------------------------------------------
; Look-Directions of Pads \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Look direction vectors
	;   Sperical coordinates (r, azimuth, polar) with r = 1
	;   Azimuth was determined above with PAD_LOW and PAD_HIGH
	;   Polar angle is determined by B or input
	;
	;   [nPads, nPts, nLook]
	if nLook eq 0 $
		then pad_polar = rebin(reform(polar, 1, npts), nPads, npts) $
		else pad_polar = rebin(reform(polar_lookdir, 1, npts), nPads, npts)

	;Look directions as vectors
	pad_look_lo = [[[sin(pad_polar) * rebin(cos(pad_lo), nPads, nPts)]], $
	               [[sin(pad_polar) * rebin(sin(pad_lo), nPads, nPts)]], $
	               [[cos(pad_polar)              ]]]
	pad_look_hi = [[[sin(pad_polar) * rebin(cos(pad_hi), nPads, nPts)]], $
	               [[sin(pad_polar) * rebin(sin(pad_hi), nPads, nPts)]], $
	               [[cos(pad_polar)              ]]]

	;Pads cannot see into the lower-half plane.
	;   If z-component of look direction is negative, negate it
	iNegLo = where(pad_look_lo[*,*,2] lt 0, nNegLo)
	iNegHi = where(pad_look_hi[*,*,2] lt 0, nNegHi)
	if nNegLo gt 0 then begin
		pad_look_lo = reform(pad_look_lo, nPads*nPts, 3, /OVERWRITE)
		pad_look_lo[iNegLo,*] = -pad_look_lo[iNegLo,*]
		pad_look_lo = reform(pad_look_lo, nPads, nPts, 3, /OVERWRITE)
	endif
	if nNegHi gt 0 then begin
		pad_look_hi = reform(pad_look_hi, nPads*nPts, 3, /OVERWRITE)
		pad_look_hi[iNegHi,*] = -pad_look_lo[iNegHi,*]
		pad_look_hi = reform(pad_look_hi, nPads, nPts, 3, /OVERWRITE)
	endif
	iNegLo = !Null
	iNegHi = !Null
	
	;Transform GDU2 into GDU1 coordinates
	;   - Previously, we made GDU2 look up by forcing z to be positive
	;   - Now, in GDU1 coordinates, it will be looking down.
	if tf_gdu2 then begin
		pad_look_lo[0,0,1:2] = -pad_look_lo[*,*,1:2]
		pad_look_hi[0,0,1:2] = -pad_look_hi[*,*,1:2]
	endif
	
	;Transpose to be [nPads, nComponents, nPts]
	pad_look_lo = transpose(pad_look_lo, [0,2,1])
	pad_look_hi = transpose(pad_look_hi, [0,2,1])
	
;-----------------------------------------------------
; Rotate Incidident Angles into Field-Aligned CS \\\\\
;-----------------------------------------------------
	;Create a transformation into a field-aligned coordinate system
	;   z' = b_hat
	;   x' = y  x z'
	;   y' = z' x x'
	x_hat    = MrVector_Normalize( MrVector_Cross( b_hat, [0,1,0] ) )
	y_hat    = MrVector_Cross( b_hat, x_hat )
	T        = fltarr(3, 3, nPads, nPts)
	T[0,*,*] = temporary(x_hat)
	T[1,*,*] = temporary(y_hat)
	T[2,*,*] = temporary(b_hat)
	
	;Rotate the look direction into the field-aligned coordinate system
	pad_lo_fa = fltarr(nPads,3,nPts)
	pad_hi_fa = fltarr(nPads,3,nPts)
	for i = 0, nPts - 1 do begin
		pad_lo_fa[*,*,i] = T[*,*,i] ## pad_look_lo[*,*,i]
		pad_hi_fa[*,*,i] = T[*,*,i] ## pad_look_hi[*,*,i]
	endfor
	pad_look_lo = !Null
	pad_look_hi = !Null
	
;-----------------------------------------------------
; Compute Pitch Angle \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Pitch angle is the polar angle away from B
	pad_pa_lo = acos( reform( pad_lo_fa[*,2,*] ) )
	pad_pa_hi = acos( reform( pad_hi_fa[*,2,*] ) )

	;Percent each bin is covered
	if arg_present(percent) then begin
		;Create the pitch angle bins.
		if n_elements(bin_min)  eq 0 then bin_min  =   0.0
		if n_elements(bin_max)  eq 0 then bin_max  = 360.0
		if n_elements(bin_size) eq 0 then bin_size =  10.0
		pa_bins = [bin_min:bin_max:bin_size] * !dtor
		nBins   = (bin_max-bin_min) / bin_size + 1

		;Calculate the percent each bin is filled
		percent  = fltarr(nBins, nPts)
		for i = 0, nPts-1 do begin
			percent[*,i] = mms_edi_PA_percent(pad_pa_lo[*,i], pad_pa_hi[*,i], pa_bins)
		end
	endif

	;Combine min and max azimuth edges
	if nPts eq 1 $
		then return, transpose([[pad_pa_lo], [pad_pa_hi]]) $
		else return, transpose([[[pad_pa_lo]], [[pad_pa_hi]]], [2,0,1])
end

;-------------------------------------------------------
; Example Programs: IDL> .r mms_edi_pitchangles \\\\\\\\
;-------------------------------------------------------

;;-----------
;; Example 1 
;;-----------
;;
;; For a given magnetic field vector, determine the
;; possible pitch angles
;;
;
;;Magnetic field vector
;B       = [1.0, 0.0, 1.0]
;lookdir = 70.0 * !dtor
;;if n_elements(lookdir) gt 0 $
;;	then lookdir *= !dtor $
;;	else lookdir = acos( (B/sqrt(total(B^2)))[2] ) * !dtor
;
;;Pitch angles
;pa = mms_edi_pitchangles(B, lookdir, PA_BINS=pa_bins, PERCENT=pct)
;pa      *= !radeg
;pa_bins *= !radeg
;
;print, ''
;print, FORMAT='(%"B    = [%8.4f, %8.4f, %8.4f]")', B
;print, FORMAT='(%"Look = %0.1f (deg)")', lookdir * !radeg
;print, ''
;
;;Print results
;print, FORMAT='(a3, 8x, a2, 8x, a2)', 'PAD', 'LO', 'HI'
;for i = 0, 31 do begin
;	print, FORMAT='(%" %02i   %7.2f   %7.2f   %7.2f")', i+1, pa[0,i], pa[1,i]
;end
;
;;Print results
;print, FORMAT='(a3, 7x, a3, 7x, a3, 6x, a3)', 'PAD', 'MIN', 'MAX', 'PCT'
;for i = 0, n_elements(pct)-2 do begin
;	print, FORMAT='(%" %02i   %7.2f   %7.2f   %6.1f")', i+1, pa_bins[i], pa_bins[i+1], pct[i]
;end
;
;;Create a plot
;title = 'Pitch Angle Coverge!C' + $
;        string(FORMAT='(%"B = [%0.2f, %0.2f, %0.2f]")', B) + ' (nT)!C' + $
;        string(FORMAT='(%"Polar Look Direction = %0.1f (deg)")', lookdir*!radeg)
;bar = barplot(pa_bins, pct, $
;              TITLE  = title, $
;              XTITLE = 'Pitch Angle', $
;              YTITLE = 'Percent Coverage')
;
;
;;Plot pitch angle coverage by each bin
;nPads = n_elements(pa[0,*])
;r0    = replicate(0, 1, nPads)
;r1    = reverse( (findgen(1,nPads) + 1) / 2, 2 )
;img   = bindgen(1,nPads)
;title = 'Pitch Angle Coverage by Pad!C' + $
;        string(FORMAT='(%"B = [%0.2f, %0.2f, %0.2f]")', B) + ' (nT)!C' + $
;        string(FORMAT='(%"Polar Look Direction = %0.1f (deg)")', lookdir*!radeg)
;p1    = MrImage(img, r0, reform(pa[0,*]-90,1,nPads)*!dtor, r1, reform(pa[1,*]-90,1,nPads)*!dtor, $
;                /AXES, $
;                CTINDEX     = 13, $
;                /POLAR, $
;                POL_AXSTYLE = 6, $
;                TITLE       = title, $
;                XRANGE      = [0,16], $
;                YRANGE      = [-!pi/2, !pi/2])
;
;p1 -> Refresh, /DISABLE
;p1 -> SetPalette, NCOLORS=32, CTINDEX=13
;p1.window.oymargin = [4,6]
;p1 -> Refresh
;
;;Kill the plot
;p1  -> Close
;bar -> Close
;
;
;;-----------
;; Example 2 
;;-----------
;;
;; For a range of magnetic field angles, determine the
;; percent coverage of each pitch angle bin.
;;
;polar    = [0.0:180.0:5.0] * !dtor
;azimuth  = 15.0            * !dtor
;lookdir  = [0.0:90.0:5.0]  * !dtor
;nPolar   = n_elements(polar)
;nAzimuth = n_elements(azimuth)
;nLook    = n_elements(lookdir)
;
;;Create the magnetic field
;B = fltarr(3, nPolar, nAzimuth)
;for i = 0, nAzimuth-1 do begin
;	B[0,*,i] = sin(polar)*cos(azimuth[i])
;	B[1,*,i] = sin(polar)*sin(azimuth[i])
;	B[2,*,i] = cos(polar)
;endfor
;B = MrVector_Normalize(B)
;
;;Allocate memory to the output array
;pa  = fltarr(2, 32, nPolar, nLook)
;pct = fltarr(37, nPolar, nLook)
;
;;Compute the percent coverage
;for i = 0, nLook - 1 do begin
;	pa_temp = mms_edi_pitchangles(B, lookdir[i], PERCENT=pct_temp, PA_BINS=pa_bins)
;	pa[*,*,*,i] = temporary(pa_temp)
;	pct[*,*,i]  = temporary(pct_temp)
;endfor
;
;;Create a window
;cgWindow, WOBJECT=oWin
;
;;Create a surface plot
;;   - % Coverage along z and in color
;;   - PA Bin azimuth (0-360) along x
;;   - Polar angle of B along y
;title = 'Pitch Angle Coverage'
;
;;Create positions
;nrows = 4
;ncols = 5
;positions = cgLayout([5,4], XGAP=0, YGAP=0, OYMargin=[10,5], OXMargin=[5,4])
;
;; Color bar.
;cgLoadCT, 4, /Brewer, /Reverse, NColors=10, Bottom=1
;
;;Plot each look direction
;for i = 0, nrows-1 do begin
;	for j = 0, ncols-1 do begin
;		;Get the position of the current plot
;		index = ncols*i + j
;		p     = positions[*,index]
;		if index gt nLook-1 then continue
;		
;		;There is only one possible contour for the first plot
;		if index eq 0 $
;			then nlevels = 1 $
;			else nlevels = 10
;		
;		;Lable the x-axis for the bottom row only
;		if i eq nrows - 1 then begin
;			xtickformat = ''
;			xtitle      = 'PA Bin (deg)'
;		endif else begin
;			xtickformat = '(a1)'
;			xtitle      = ''
;		endelse
;		
;		;Label the y-axis for the left column only
;		if j eq 0 then begin
;			ytitle      = 'B Polar Angle!C(deg)'
;			ytickformat = ''
;		endif else begin
;			ytickformat = '(a1)'
;			ytitle      = ''
;		endelse
;
;		;Plot the contour
;		cgContour, pct[*,*,index], pa_bins*!radeg, polar*!radeg, $
;		           /ADDCMD, $
;		           C_COLORS    = Indgen(10)+1, $
;		           CHARSIZE    = cgDefCharsize()*0.65, $
;		           /Fill, $
;		           /NOERASE, $
;		           NLEVELS     = nlevels, $
;		           POSITION    = p, $
;		           TITLE       = '', $
;		           XTICKFORMAT = xtickformat, $
;		           XTITLE      = xtitle, $
;		           YTICKFORMAT = ytickformat, $
;		           YTITLE      = ytitle
;	endfor
;endfor
;
;;Put a title on the plot
;cgText, 0.5, 0.95, title, /ADDCMD, /NORMAL, ALIGNMENT=0.5
;
;; Calculate spacing for color bars.
;yspace = 4.0*(!D.Y_CH_SIZE)/!D.Y_SIZE
;p = [0.25, p[1]-yspace-0.02, 0.75, p[1]-yspace]
;cgColorbar, /ADDCMD, $
;            BOTTOM   = 1, $
;            CHARSIZE = cgDefCharsize()*0.65, $
;            /DISCRETE, $
;            NCOLORS  = 10, $
;            POSITION = p, $
;            RANGE    = [Min(pct),Max(pct)], $
;            TITLE    = '% Coverage'
;end