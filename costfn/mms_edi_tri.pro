; docformat = 'rst'
;
; NAME:
;    MrPrintF
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
;   A wrapper for IDL's PrintF procedure that can direct output to standard output,
;   error, or log files.
;
; :Categories:
;    File Utility
;
; :Params:
;       BEAMS:      in, required, type=structarr
;                   An array of beam structures with the following tags::
;                       GUN_BPP  - Position of the gun in BPP
;                       ALPHA    - Firing angle in BPP (degrees)
;                       FV_BPP   - Firing vectors in BPP
;                       VG_BPP   - Gun positions on the virtual spacecraft in BPP
;       WIDTH:      in, required, type=fltarr
;                   Beam widths in BPP.
;
; :Returns:
;       TRI:        Structure of return values. Flags are::
;                       D_BPP    -  3-components of the drift step.
;                       D_DELTA  -  Minimum value of the cost function.
;                       FLAG     -  Success flag. Values are::
;                                       0 - Success
;                                       1 - Error
;                                       2 - Drift step lies on edge of grid.
;                   If FLAG is non-zero, NANs are returned.
;
; :See Also:
;   MrStdErr, MrStdOut, MrStdLog, MrLogFile__Define
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
;       2015/10/29  -   Written by Matthew Argall
;-
function mms_edi_tri, beams, width
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		
		;Make values invalid
		tri.d_bpp   = !values.f_nan
		tri.d_delta = !values.f_nan
		tri.tGyro   = !values.f_nan
		tri.flag    = 1B
		
		return, tri
	endif
	
	;Create output structure
	tri = { edi_tri,            $
	        d_bpp:   fltarr(3), $
	        d_delta: 0.0,       $
	        tGyro:   0.0,       $
	        flag:    0B         $
	      }

;-----------------------------------------------------
; Setup Grid \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;The base is the gun-detector separation
	base = mms_instr_origins_ocs('EDI1_GUN') - mms_instr_origins_ocs('EDI2_DETECTOR')
	base = sqrt(total(base^2))

	;Find maximum triangulation radius
	rmax = mms_edi_tri_rmax(reform(beams.gun_bpp[0,*]), $
	                        reform(beams.gun_bpp[1,*]), $
	                        beams.alpha, base)
	rmax = 125.0
	
	;Create the grid
	rstep   = 0.1
	rmin    = 0.0
	phimin  = 0.0
	phimax  = 2*!pi
	phistep = 0.1*!dtor
	grid    = mmsedi_polargrid(phimin, phimax, phistep, rmin, rmax, rstep)

;-----------------------------------------------------
; Cost Function \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Compute the drift step
	costFn = mmsedi_costfunction( beams.fv_bpp[0], beams.fv_bpp[1], $
	                              beams.vg_bpp[0], beams.vg_bpp[1], width, grid)

;-----------------------------------------------------
; Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;
	; The virtual source point is located at the grid point
	; where the minimum of the cost function occurs. The drift
	; step points from the virtual source point to the detector.
	;
	
	;Index of the grid point where the virtual source point is located.
	delta     = min(costFn, idelta)
	grid_dims = size(grid, /DIMENSIONS)
	i2d       = array_indices(grid_dims, idelta, /DIMENSIONS)

	;Do not accept the answer if it lies on the edige of the grid
	if i2d[0] eq 0 || i2d[0] eq grid_dims[0] || i2d[1] eq 0 || i2d[1] eq grid_dims[1] then begin
		d_bpp        = -[grid[idelta].x, grid[idelta].y]
		tri.d_bpp    = !values.f_nan
		tri.d_delta  = !values.f_nan
		tri.tGyro    = !values.f_nan
		tri.flag     = 2
		MrPrintF, 'LogErr', sqrt(total(d_bpp^2)), i2d, grid_dims, rmax, $
		          FORMAT='(%"|d|=%0.4f lies on edge of grid [%i,%i] of size [%i,%i] with rmax=%0.4f.")'
	
	;Record the drift step
	endif else begin
		tri.d_bpp[0] = -grid[i2d[0],i2d[1]].x
		tri.d_bpp[1] = -grid[i2d[0],i2d[1]].y
		tri.d_delta  =  delta
		tri.tGyro    = mean(beams.tGyro)
		tri.flag     =  0
		MrPrintF, 'LogText', sqrt(total(tri.d_bpp^2)), rmax, FORMAT='(%"|d| = %0.4f and rmax = %0.4f.")'
	endelse
	
	return, tri
end