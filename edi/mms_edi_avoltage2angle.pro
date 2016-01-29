; docformat = 'rst'
;
; NAME:
;       MMS_EDI_aVoltage2Angle
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
;   Convert MMS EDI analog voltages to firing angles.
;
; :Categories:
;       MMS, EDI
;
; :Examples:
;   Convert firing angles to analog voltages and back.
;       IDL> avoltage = mms_edi_angle2avoltage([45, 75], /DEGREES)
;       IDL> solid_angle = mms_edi_avoltage2angle(avoltage[0], avoltage[1]) * !radeg
;       IDL> print, solid_angle[0:1]
;           45.0000      75.0000
;
; :Params:
;       AVX:            in, required, type=lonarr
;                       Analog firing voltages, x-component.
;       AVY:            in, required, type=lonarr
;                       Analog firing voltages, y-component.
;
; :Keywords:
;       CARTESIAN:      in, optional, type=boolean, default=0
;                       If set, the firing directions will be returned in cartesian
;                           coordinates.
;
; :Returns:
;       FIRE_DIR:       3xN array of vectors giving the (theta, phi, r) spherical
;                           coordinates of the GDU firing direction. r represents the
;                           total voltage applied by the gun, theta the polar angle, and
;                           phi the azimuthal angle. Angles are output with units of
;                           radians.
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
;       2015/02/18  -   Written by Matthew Argall
;-
function mms_edi_avoltage2angle, aVx, aVy, $
CARTESIAN=cartesian
	compile_opt idl2
	on_error, 2

	;Convert to cartesian coordinates?
	cartesian = keyword_set(cartesian)

	;
	; MMS constants for calculation of angles from voltages
	;   vx    = aVx - 13824
	;   theta = 1/0.77 * asin( r / 14431.2 )
	;
	;--------------------
	;
	;  Analytic voltages used in the flight software have this relationship with gun
	;  firing angles:
	;
	;  vx = 14431.2 * cos(phi) * sin(0.77*theta)
	;  vy = 14431.2 * sin(phi) * sin(0.77*theta)
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

	;Allocate memory to output
	fire_dir = fltarr(3, n_elements(aVx))

	;Shift analytic voltages back to +/- values.
	Vx  = aVx - 13824.0
	Vy  = aVy - 13824.0
	
	;Radius
	fire_dir[2,*] = sqrt(Vx^2 + Vy^2)
	
	;Polar angle (Radians)
	fire_dir[0,*] = atan(Vy, Vx)
	
	;Azimuthal angle (Radians)
	fire_dir[1,*] = (1.0 / 0.77) * asin(fire_dir[2,*] / 14431.2)
	
	;Cartesian
	if cartesian then fire_dir = cv_coord(FROM_SPHER=fire_dir, /TO_RECT)
	
	return, fire_dir
end