; docformat = 'rst'
;
; NAME:
;       mms_edi_xxyz2bpp
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;         contributors may  be used to endorse or promote products derived from this     ;
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
;   Using the magnetic field, create a transformation matrix that transforms from
;   the measurement coordinate system into one in which B is along the z-axis. The
;   transformation is built as
;       Z' = B / |B|       (negated if B[2] < 0)
;       X' = Y x Z'
;       Y' = Z' x X'
;
; :Categories:
;   MMS, EDI, Bestarg
;
; :Params:
;       B:              in, required, type=3xN float
;                       Vector magnetic field
;
; :Returns:
;       XYZ2BPP:        Transformation matrix from XYZ to BPP
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/05/03  -   Written by Matthew Argall
;-
function mms_edi_xxyz2bpp, b
	compile_opt idl2
	on_error, 2
	
	;Make sure B is 3-element vector or 3xN
	sz = size(b)
	if (sz[sz[0]+2] ne 3) && (sz[0] ne 2 || sz[1] ne 3) then message, 'B must be 3xN.'
	
	;Normalize b to get the z-axis
	z_bpp = mrvector_normalize(b)
	
	;Make sure Z_BPP points toward +Z
	idown = where(z_bpp[2,*] lt 0, ndown)
	if ndown gt 0 then z_bpp[*,idown] = -z_bpp[*,idown]
	
	;Create X
	x_bpp = mrvector_cross([0, 1, 0], z_bpp)
	x_bpp = mrvector_normalize(x_bpp)
	
	;Create Y
	y_bpp = mrvector_cross(z_bpp, x_bpp)
	
	;Create Ouput matrix
	return, [[x_bpp], [y_bpp], [z_bpp]]
end