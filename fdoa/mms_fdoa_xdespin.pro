; docformat = 'rst'
;
; NAME:
;       mms_fdoa_xdespin
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
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
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
;+
;   Create a set of transformation matrices that will despin a vector.
;
; :Params:
;       ATTITUDE:       in, required, type=struct
;                       Structure of attitude data returned by mms_fdoa_read_defatt.pro.
;       T_OUT:          in, optional, type=lon64arr (cdf_time_tt2000)
;                       Times to which the phase is to be interpolated.
;
; :Keywords:
;       OFFSET:         in, required, type=boolean, default=0
;                       Fixed angular offset between x-BCS axis and the x-axis of the
;                           coordinate system in which the data is given.
;       SPINUP:         in, optional, type=
;                       If set, spin will be added instead of removed.
;       TYPE:           in, optional, type=intarr
;                       Type of phase to use when despinning. Options are::
;                           'Z'  -  Spacecraft body       z-phase
;                           'P'  -  Major principal axix  P-phase
;                           'L'  -  Angular momentum      L-phase
;
; :Returns:
;       PHASE:          Interpolated phase.
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
;       2015-09-25  -   Written by Matthew Argall.
;-
function mms_fdoa_xdespin, attitude, t_out, $
OFFSET=offset, $
SPINUP=spinup, $
TYPE=type
	compile_opt idl2
	on_error, 2
	
	;Defaults
	spinup = keyword_set(spinup)
	if n_elements(offset) eq 0 then offset = 0
	if n_elements(type)   eq 0 then type   = 'P'
	
	;
	; The spin phase reported by FDOA is relative to the physical
	; BCS x-axis.
	;
	; If the instrument is in BCS already, no phase offset needs to
	; be applied. The offset is the angle between the original CS
	; x-axis and the x-axis of BCS.
	;

	;Interpolate the phase
	;   - Returned in degrees
	phase = mms_fdoa_interp_phase(attitude, t_out) * !dtor

	;If we are spinning up the data, we have to invert the phase
	if spinup then phase = -phase
	
	;Sine and cosine of phase
	cosPhase = cos(phase + offset)
	sinPhase = sin(phase + offset)
	
	;Create the rotation matrix
	; - PHASE is from the sun to the x-axis. We want the phase from
	;   the x-axis to the sun. To accomplish this, we transpose the
	;   typical transformation matrix about Z
	;       |  cos  sin  0 |       |  cos -sin  0 |
	;   T = | -sin  cos  0 |  -->  |  sin  cos  0 |
	;       |   0    0   1 |       |   0    0   1 |
	xdespin        =  fltarr(3, 3, n_elements(t_out))
	xdespin[0,0,*] =  cosPhase
	xdespin[1,0,*] = -sinPhase
	xdespin[0,1,*] =  sinPhase
	xdespin[1,1,*] =  cosPhase
	xdespin[2,2,*] =  1

	return, xdespin
end