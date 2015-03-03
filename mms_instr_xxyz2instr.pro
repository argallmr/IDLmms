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
;   Transform one instrument's coordinate system into the coordinate system of any other
;   instrument.
;
; :Categories:
;       MMS
;
; :Params:
;       INSTR1:             in, required, type=string
;                           Name of the instrument for which the coordinate system is to
;                               be transformed.
;       INSTR2:             in, required, type=string
;                           Name of the instrument native to the destination coordinate
;                               system.
;
; :Returns:
;       XYZ2INSTR:          The from an instrument's coordinate system into OCS. Note that::
;
;                                   v_xyz       = [[vx], [vy], [vz]]
;                                   A_xyz2instr = [[ Axx,  Axy,  Axz], $
;                                                  [ Ayx,  Ayy,  Ayz], $
;                                                  [ Azx,  Azy,  Azz]]
;
;                                   | vx' |   | Axx Axy Axz | | vx |
;                                   | vy' | = | Ayx Ayy Ayz | | vy |
;                                   | vz' |   | Azx Azy Azz | | vz |
;
;                                   v_dss = A_xyz2instr ## v_xyz
;                                   [1,3] =    [3,3]    ## [1,3]
;                                               ^             ^
;
; :References:
;   Magnetospheric Multiscale (MMS) Project Alignment and Coordinate System Document,
;       461-SYS-SPEC-0115, Revision C, Effective Date: July 22, 2014
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
;       2015/02/19  -   Written by Matthew Argall
;-
function mms_instr_xXYZ2Instr, instr1, instr2
	compile_opt idl2
	on_error, 2

	;Get the rotation angles
	!Null = mms_instr_xXYZ2OCS(instr1, /NO_MATRIX, ROTX=rotx1, ROTY=roty1, ROTZ=rotz1, ORDER=order)
	!Null = mms_instr_xXYZ2OCS(instr2, /NO_MATRIX, ROTX=rotx2, ROTY=roty2, ROTZ=rotz2)
	
	;Calculate the angle between the two coordinate systems
	;   - Make sure the angle is positive
	if rotz1 lt 0 then rotz1 += 2.0 * !pi
	if rotz2 lt 0 then rotz2 += 2.0 * !pi
	rotz12 = rotz2 - rotz1

	;Create the transformation matrix
	xyz2instr = MrEulerMatrix(rotx1, roty1, rotz12, ORDER=order, /MATH)

	return, xyz2instr
end
