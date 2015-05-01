; docformat = 'rst'
;
; NAME:
;       mms_dss_xdespin
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
;   Crate an array of matrices that will transform from a spinning to a
;   not-spinning coordinate system.
;
; :Categories:
;   MMS, DSS
;
; :Params:
;       HK:             in, required, type=structure
;                       Structure of sunpulse data returned by mms_dss_read_sunpulse.
;       EPOCH:          in, optional, type=int64 (cdf_time_tt2000)
;                       CDF epoch times at which the spin-phase is desired.
;
; :Keywords:
;       CS:             in, optional, type=string, default='BCS'
;                       Name of the coordinate system in which the data resides.
;       SPINUP:         in, optional, type=boolean, default=false
;                       If set, data will be spun up instead of despun.
;
; :Returns:
;       SPUN2DESPUN:    Transformation matrix to the despun coordinate system.
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
;       2015/05/01  -   Written by Matthew Argall.
;-
function mms_dss_xdespin, hk, epoch
CS=cs, $
SPINUP=spinup
	compile_opt idl2
	on_error, 2

	;Defaults
	spinup = keyword_set(spinup)
	if n_elements(cs) eq 0 then sc = 'BCS'

	;Calculate phase
	phase = mms_dss_sunpulse2phase(hk, epoch) * (!dpi / 180.0D)
	
	;The spin phase is the angle of rotation away from the s/c-sun
	;line. To despin, we want to rotate against the phase. To spin-
	;up, we rotate with the phase.
	if ~spinup then phase = -phase
	
	;The DSS requires a -76 degree rotation about the z-axis to align
	;with BCS. Aligning BCS with the sun sensor requires a +76 degree
	;rotation.
	offset = 76 * pi/180
	
	;Sine and Cosine of phase
	sinPhase = sin(phase + offset)
	cosPhase = cos(phase + offset)
	
	; So that it looks math-like
	;                 |  cos  sin  0  |
	;   spin2despun = | -sin  cos  0  |
	;                 |   0    0   1  |
	spin2despun        =  dblarr(3, 3, n_elements(phase))
	spin2despun[0,0,*] =  cosPhase
	spin2despun[1,0,*] =  sinPhase
	spin2despun[0,1,*] = -sinPhase
	spin2despun[1,1,*] =  cosPhase
	spin2despun[2,2,*] =  1
end
