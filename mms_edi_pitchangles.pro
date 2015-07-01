; docformat = 'rst'
;
; NAME:
;    mms_sdc_ql_BEfields
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
function mms_edi_PA_percent, pa, pad_grid
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		void = cgErrorMSG(/QUIET)
		return, !Null
	endif
	
	nBins = n_elements(pad_grid) - 1
	range = fltarr(2, nBins) - 1
	epsilon = 1e-5

	;What percent of each pitch angle sector is covered?
	for i = 0, nBins - 1 do begin
		imin = where(pa[0,*] ge pad_grid[0:nBins-1] - epsilon and $
		             pa[0,*] lt pad_grid[1:nBins]   + epsilon, nmin)
		imax = where(pa[2,*] ge pad_grid[0:nBins-1] - epsilon and $
		             pa[2,*] lt pad_grid[1:nBins]   + epsilon, nmax)

		;Set upper and lower range boundries
		if nmin gt 0 then range[0,i] = min(pa[0,imin])
		if nmax gt 0 then range[0,i] = max(pa[2,imax])
	endfor
stop
	;Calculate percents
	pct = ( range[1,*] - range[0,*] ) / (11.5*!radeg)

	return, pct
end



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
function mms_edi_pitchangles, B, $
PERCENT=percent
	compile_opt idl2
	
	sz    = size(B)
	ndims = sz[0]
	dims  = sz[1:ndims]
	nPts  = ndims eq 1 ? 1 : sz[2]
	if ndims eq 1 && dims[0] ne 3 then message, 'B must be a 3-element vector.'
	if ndims eq 2 && dims[0] ne 3 then message, 'B must be a 3xN array.'
	
	;Angle to edges and center of each pad
	nPads      = 32                               ;Number of pads
	pad_width  = 11.25                            ;Angular width of pads
	pad_low    = findgen(nPads)     * pad_width * !dtor    ;Angle to the edges of each pad
	pad_high   = (findgen(nPads)+1) * pad_width * !dtor    ;Angle to the edges of each pad
	pad_center = (pad_high + pad_low) / 2.0 ;Center of each pad
		
	;Unit vector pointing to center of each pad
	pad_low    = [ [cos( pad_low    ) ], [sin( pad_low    ) ] ]
	pad_high   = [ [cos( pad_high   ) ], [sin( pad_high   ) ] ]
	pad_center = [ [cos( pad_center ) ], [sin( pad_center ) ] ]
	
	;Projection of B onto the GDU xy-plane
	B_xy_hat = fltarr(2, nPts)
	iZero    = where(B[0,*] lt 1e-7 and B[1,*] lt 1e-7, nZero, COMPLEMENT=iGood, NCOMPLEMENT=nGood)
	if nZero gt 0 then B_xy_hat[*,iZero] = 0.0
	if nGood gt 0 then begin
		B_mag = sqrt( B[0,iGood]^2 + B[1,iGood]^2 )
		B_xy_hat[0,iGood] = B[0,iGood] / B_mag
		B_xy_hat[1,iGood] = B[1,iGood] / B_mag
	endif

	;Pitch angle bins
	pa        = fltarr(3, nPads, nPts)
	pa[0,*,*] = acos( -1 > B_xy_hat ## pad_low    < 1 )
	pa[1,*,*] = acos( -1 > B_xy_hat ## pad_center < 1 )
	pa[2,*,*] = acos( -1 > B_xy_hat ## pad_high   < 1 )
	
	;Determin the pitch angles between 180 and 360
	B_az       = atan(B_xy_hat[1,*], B_xy_hat[0,*])
	if nPts eq 1 then B_az = B_az[0]
	low_az     = ( atan(pad_low[*,1],    pad_low[*,0])    + 2.0*!pi ) mod (2.0*!pi)
	center_az  = ( atan(pad_center[*,1], pad_center[*,0]) + 2.0*!pi ) mod (2.0*!pi)
	high_az    = ( atan(pad_high[*,1],   pad_high[*,0])   + 2.0*!pi ) mod (2.0*!pi)
	
	pa[0,*,*] = ((low_az    - B_az) + 2*!pi) mod (2*!pi)
	pa[1,*,*] = ((center_az - B_az) + 2*!pi) mod (2*!pi)
	pa[2,*,*] = ((high_az   - B_az) + 2*!pi) mod (2*!pi)
stop
	;Percent each bin is covered
	if arg_present(percent) then begin
		percent = fltarr(nPads, nPts)
		for i = 0, nPts-1 do percent[*,i] = mms_edi_PA_percent(pa[*,*,i], pad_grid)
	endif

	return, pa
end

;-------------------------------------------------------
; Example Programs: IDL> .r mms_edi_pitchangles \\\\\\\\
;-------------------------------------------------------

;-----------
; Example 1 
;-----------
;
; For a given magnetic field vector, determine the
; possible pitch angles
;

;Magnetic field vector
B = [1, 0, 0]

;Pitch angles
pa = mms_edi_pitchangles(B) * !radeg

print, ''
print, FORMAT='(%"B = [%8.4f, %8.4f, %8.4f]")', B
print, ''

;Print results
print, FORMAT='(a3, 6x, a3, 5x, a6, 5x, a4)', 'PAD', 'LOW', 'CENTER', 'HIGH'
for i = 0, 31 do begin
	print, FORMAT='(%" %02i   %7.2f   %7.2f   %7.2f")', i+1, pa[0,i], pa[1,i], pa[2,i]
end


;-----------
; Example 2 
;-----------
;
; For a range of magnetic field angles, determine the
; percent coverage of each pitch angle bin.
;
dPolar   = 5.0
nPolar   = 90.0 / dPolar + 1
polar    = findgen(nPolar) * dPolar * !dtor
dAzimuth = 10.0
nAzimuth = 360.0 / dAzimuth + 1
azimuth  = findgen(nAzimuth) * dAzimuth * !dtor

;Create the magnetic field
B = fltarr(3, nAzimuth, nPolar)
for i = 0, nPolar-1 do begin
	B[0,*,i] = cos(azimuth)*sin(polar[i])
	B[1,*,i] = sin(azimuth)*sin(polar[i])
	B[2,*,i] = cos(polar[i])
endfor

;Allocate memory to the output array
pct = fltarr(32, nAzimuth, nPolar)

;Compute the percent coverage
for i = 0, nPolar - 1 do begin
	!Null      = mms_edi_pitchangles(B[*,*,i], PERCENT=pct_temp)
	pct[*,*,i] = pct_temp
endfor

cd, CURRENT=pwd
fname    = filepath('edi_pa_pct.dat', ROOT_DIR=pwd)
pa       = findgen(32.0) * 11.25
polar   *= !radeg
azimuth *= !radeg

openw, lun, fname, /GET_LUN
printf, lun, 'Polar', 'Azim', pa, FORMAT='(a5, 8x, a4, 3x, 31(f7.3, 3x), f7.3)'
for i = 0, nPolar - 1 do begin
	for j = 0, nAzimuth - 1 do begin
		printf, lun, polar[i], azimuth[j], pct[*,j,i], FORMAT='(33(f7.3, 3x), f7.3)'
	endfor
endfor
free_lun, lun

end