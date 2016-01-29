; docformat = 'rst'
;
; NAME:
;       MMS_EDI_angle2aVoltage
;
;*****************************************************************************************
;   Copyright (c) 2014, University of New Hampshire                                      ;
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
;   Convert MMS EDI firing angle to an analog voltage.
;
; :Categories:
;       MMS, EDI
;
; :Examples:
;   Convert firing angles to analog voltages and back.
;       IDL> avoltage = mms_edi_angle2avoltage([45, 75], /DEGREES)
;       IDL> solid_angle = mms_edi_avoltage2angle(avoltage[0], avoltage[1]) * !radeg
;       IDL> print, solid_angle
;           45.0000      75.0000      699287.
;
; :Params:
;       SOLID_ANGLE:    in, required, type=2xN fltarr
;                       Polar and azimulathal angles (radians) of the firing direction.
;
; :Keywords:
;       DEGREES:        in, optional, type=boolean, default=0
;                       If set, `SOLID_ANGLE` has units of degrees.
;
; :Returns:
;       AVOLTAGE:       Analog firing voltages, x- and y-components.
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
;       2015/02/25  -   Written by Matthew Argall
;-
function mms_edi_angle2avoltage, solid_angle, $
DEGREES=degrees
	compile_opt idl2
	on_error, 2

	;Convert to radians?
	if keyword_set(degrees) then begin
		theta = solid_angle[0,*] * !dtor
		phi   = solid_angle[1,*] * !dtor
	endif else begin
		theta = solid_angle[0,*]
		phi   = solid_angle[1,*]
	endelse

	;
	; MMS constants for calculation of angles from voltages
	;   vx    = aVx - 13824
	;   vy    = aVy - 13824
	;
	;   vx = 14431.2 * cos(theta) * sin(0.77*phi)
	;   vy = 14431.2 * sin(theta) * sin(0.77*phi)
	;
	;   r = vx^2 + vy^2
	;     = sqrt( 14431.2^2 * cos(theta)^2 * sin(0.77*phi)^2 + 
	;             14431.2^2 * sin(theta)^2 * sin(0.77*phi)^2 )
	;     = sqrt( 14431.2^2 * sin(0.77*phi)^2 * 
	;             (cos(theta)^2 + sin(theta)^2) )
	;     = sqrt( 14431.2^2 * sin(0.77*phi)^2 )
	;     = 14431.2 * sin(0.77*phi)
	;
	;   phi   = 1/0.77 * asin(r / 14431.2)
	;   theta = atan(vy / vz)
	;
	;--------------------
	;
	;  Analytic voltages used in the flight software have this relationship with gun
	;  firing angles:
	;
	;   vx = 14431.2 * cos(theta) * sin(0.77*phi)
	;   vy = 14431.2 * sin(theta) * sin(0.77*phi)
	;
	;  o) The numbers 14431.2 and 0.77 are the coefficients of this model.
	;  o) 13824 (0x3600) is a constant that is used to shift the bipolar analytic
	;  voltages vx,vy into a strictly positive range. These positive numbers are
	;  what is transmitted in telemetry. Therefore, to regain the original bipolar
	;  analytic voltages, the constant must be subtracted on the ground.
	;
	;  You may wonder why we are using a constant that is smaller than 14431.2 for
	;  shifting into a positive range. The reason this works is that we only go up
	;  to theta=95 degrees. With this restriction the sin() in the above equations
	;  does not get larger than sin(0.77 * 95) = 0.957, and therefore the max
	;  negative vx or vy we will encounter is -14431.2 * 0.957 = -13810.7
	;
	;  The choice of 13824 (0x3600) is not arbitrary. It has to do with lookup table
	;  grid spacing.
	;

	avx = 14431.2 * cos(theta) * sin(0.77 * phi) + 13824
	avy = 14431.2 * sin(theta) * sin(0.77 * phi) + 13824

	avoltage = [avx, avy]
	return, avoltage
end


