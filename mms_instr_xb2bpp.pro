; docformat = 'rst'
;
; NAME:
;       MMS_Instr_xXYZ2Instr
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
;   Use the magnetic field (B) to define a plane perpendicular to B (BPP), then create a
;   rotation matrix that will rotation B's coordinate system (CS) into BPP.
;
; :Categories:
;       MMS
;
; :Example:
;   See the main-level program at the end of this document::
;       IDL> .run mms_instr_xb2bpp
;
; :Params:
;       B_FIELD:            in, required, type=3xN fltarr
;                           3-component magnetic field.
;
; :Returns:
;       XYZ2BPP:            Transformation matrix from `B_FIELD`'s original CS (x,y,z) to
;                               that of BPP (x',y',z')::
;                                   
;                                   Z' = B_hat
;                                   X' = Y  x Z'        Y is [0.0, 1.0, 0.0] in B's CS.
;                                   Y' = Z' x X'
;
;                                   v_xyz     = [[vx], [vy], [vz]]
;                                   A_xyz2bpp = [[ Axx,  Axy,  Axz], $
;                                                [ Ayx,  Ayy,  Ayz], $
;                                                [ Azx,  Azy,  Azz]]
;
;                                   | vx' |   | Axx Axy Axz | | vx |
;                                   | vy' | = | Ayx Ayy Ayz | | vy |
;                                   | vz' |   | Azx Azy Azz | | vz |
;
;                                    [1,3]  =      [3,3]    ## [1,3]
;                                                   ^             ^
;                                   v_bpp = A_xyz2bpp ## v_xyz
;                                   v_bpp = matrix_multiply(v_xyz, A_xyz2bpp)
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/02/26  -   Written by Matthew Argall
;-
function mms_instr_xb2bpp, b_field
	compile_opt idl2
	on_error, 2

	;Size of B
	dims = size(b_field, /DIMENSIONS)
	if ~(dims[0] eq 3 || dims[1] eq 3) then message, 'B_FIELD must be 3xN or Nx3.'

	;Size Y based on input
	if dims[0] eq 3 $
		then y = [ 0.0,   1.0,   0.0 ] $
		else y = [[0.0], [1.0], [0.0]]

	;Transformation matrix from SCS into BPP
	;   - Z' = B_hat
	;   - X' = Y  x Z'
	;   - Y' = Z' x X'
	z_hat   = normalize(b_field)
	if z_hat[2] lt 0 then z_hat = -z_hat
	
	x_hat   = normalize(cross_product(y, z_hat))
	y_hat   = normalize(cross_product(z_hat, x_hat))

	;Form the transformation matrix
	xyz2bpp = [[[x_hat]], [[y_hat]], [[z_hat]]]

	if dims[0] eq 3 $
		then xyz2bpp = transpose(xyz2bpp, [0, 2, 1]) $
		else xyz2bpp = transpose(xyz2bpp, [1, 2, 0])

	return, xyz2bpp
end



;-----------------------------------------------------
; Main-Level Example Program: IDL> mms_instr_xb2bpp \\
;-----------------------------------------------------

;============
; EXAMPLE 1 |
;============
;B-field is 3x1
B       = (randomu(3, 3, 1) - 0.5) * 30.0
B_mag   = sqrt(total(B^2))
xyz2bpp = mms_instr_xb2bpp(B)
B_BPP   = xyz2bpp ## transpose(B)     ;Same as Matrix_Multiply(B, xyz2bpp, /ATRANSPOSE)

;Results
print, 'B is 3x1:'
print, FORMAT='(10x, "B", 15x, "|B|", 15x, "B_BPP")'
print, FORMAT='(%"[%5.2f, %5.2f, %5.2f]    %5.2f     [%5.2f, %5.2f, %5.2f]")', B, B_mag, B_BPP
print, '-----------------------------------'
print, ''

;============
; EXAMPLE 2 |
;============
B       = (randomu(10, 3, 10) - 0.5) * 20.0
B_mag   = sqrt(total(B^2, 1))
Bdims   = size(B, /DIMENSIONS)
B_BPP   = fltarr(bDims)
xyz2bpp = mms_instr_xb2bpp(B)

;Results
print, 'B is 3xN:'
print, FORMAT='(10x, "B", 15x, "|B|", 15x, "B_BPP")'
for i = 0, Bdims[1]-1 do begin
	B_BPP[*,i] = matrix_multiply(B[*,i], xyz2bpp[*,*,i], /ATRANSPOSE)  ;xyz2bpp ## transpose(B_temp)
	print, FORMAT='(%"[%5.2f, %5.2f, %5.2f]    %5.2f     [%5.2f, %5.2f, %5.2f]")', B[*,i], B_mag[i], B_BPP[*,i]
endfor
print, '-----------------------------------'
print, ''

;============
; EXAMPLE 3 |
;============
;B-field is 1x3
B       = (randomu(6, 1, 3) - 0.5) * 25.0
B_mag   = sqrt(total(B^2))
xyz2bpp = mms_instr_xb2bpp(B)
B_BPP   = xyz2bpp ## B            ;Same as Matrix_Multiply(B, xyz2bpp)

;Results
print, 'B is 1x3:'
print, FORMAT='(10x, "B", 15x, "|B|", 15x, "B_BPP")'
print, FORMAT='(%"[%5.2f, %5.2f, %5.2f]    %5.2f     [%5.2f, %5.2f, %5.2f]")', B, B_mag, B_BPP
print, '-----------------------------------'
print, ''

;============
; EXAMPLE 4 |
;============
;B-field is Nx3
B       = (randomu(8, 10, 3) - 0.5) * 15.0
B_mag   = sqrt(total(B^2, 2))
Bdims   = size(B, /DIMENSIONS)
B_BPP   = fltarr(bDims)
xyz2bpp = mms_instr_xb2bpp(B)

;Results
print, 'B is Nx3:'
print, FORMAT='(10x, "B", 15x, "|B|", 15x, "B_BPP")'
for i = 0, Bdims[0]-1 do begin
	B_BPP[i,*] = matrix_multiply(B[i,*], xyz2bpp[*,*,i])  ;xyz2bpp ## transpose(B_temp)
	print, FORMAT='(%"[%5.2f, %5.2f, %5.2f]    %5.2f     [%5.2f, %5.2f, %5.2f]")', B[i,*], B_mag[i], B_BPP[i,*]
endfor
print, '-----------------------------------'
print, ''



end
