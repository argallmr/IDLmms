; docformat = 'rst'
;
; NAME:
;       MMS_Instr_Origins_Instr
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
;   Return the position of the origin of an instrument's coordinate system with respect
;   to the origin of another instrument.
;
; :Categories:
;       MMS
;
; :Examples:
;   Determine the location of EDI1 with respect to EDI2::
;       IDL> print, mms_instr_origins_instr('EDI1', 'EDI2')
;              -2665.4960       1778.1380       0.0000000
;
;   Determine the location of EDI1 with respect to EDI2, in spherical coordinates::
;       IDL> print, mms_instr_origins_instr('EDI1', 'EDI2', /SPHERICAL)
;              3.1415927   4.8095040e-09       3832.1075
;
; :Params:
;       INSTR1:             in, required, type=string
;                           Name of the instrument for which the origin is to be shifted.
;                               For instrument names, see MMS_Instr_Origins_OCS.pro
;       INSTR2:             in, required, type=string
;                           Name of the instrument whose origin is the destination.
;
; :Keywords:
;       SPHERICAL:          in, optional, type=boolean, default=0
;                           If set, `ORIGIN` will return spherical coordinates as
;                               (theta, phi, r), where r is the radius, theta is the
;                               polar anglie and phi is the azimuthal angle.
;
; :Returns:
;       ORIGIN:             3-element vector of the (x,y,z) coordinates of the requested
;                               `INSTRUMENT` in OCS coordinates. Units: millimeters. If
;                               more than one instrument name is given, origins are
;                               returned in a hash with the instrument names as keys.
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
;       2015/02/21  -   Written by Matthew Argall
;-
function mms_instr_origins_instr, instr1, instr2, $
SPHERICAL=spherical
	compile_opt idl2
	on_error, 2

	;Default to cartesian coordinates.
	spherical = keyword_set(spherical)
	if n_elements(instr1) ne 1 then message, 'Instr1 must be scalar.'
	if n_elements(instr2) ne 1 then message, 'Instr2 must be scalar.'

	;Get the instrument origins
	instr1_origin = mms_instr_origins_ocs(instr1, SPHERICAL=spherical)
	instr2_origin = mms_instr_origins_ocs(instr2, SPHERICAL=spherical)

	;Position of Instr1's origin with respect to the origin of Instr2
	if spherical then begin
		origin    = dblarr(3)
		origin[0] = instr1_origin[0] - instr2_origin[0]
		origin[1] = instr1_origin[1] - instr2_origin[1]
		origin[2] = instr1_origin[2] + instr2_origin[2]
	endif else begin
		origin = instr1_origin - instr2_origin
	endelse
	
	return, origin
end
