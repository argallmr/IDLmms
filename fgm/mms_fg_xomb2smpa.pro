; docformat = 'rst'
;
; NAME:
;    mms_fg_xomb2smpa
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
;   Create a transformation matrix that will rotate the orthogonal magnetometer (OMB)
;   system into the spinning major principal axis (SMPA) system.
;
; :Categories:
;   MMS, DFG, AFG
;
; :Keywords:
;       INVERSE:        in, optional, type=boolean, default=0
;                       If set, the inverse SMPA -> OMB transformation will be returned.
;
; :Returns:
;       OMB2SMPA:       CS transformation matrix from OMB -> SMPA.
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
;   Modification History::
;       2015-05-03  -   Written by Matthew Argall.
;-
function mms_fg_xomb2smpa, $
INVERSE=inverse

	;
	; The OMB coordinate system is defined to be a fixed 225°
	; rotation from the SMPA coordinate system
	;

	;Constants
	theta    = -225.0 * !dpi / 180.0
	cosTheta = cos(theta)
	sinTheta = sin(theta)
	
	;Rotation matrix
	omb2smpa = [[  cosTheta,  sinTheta,  0 ], $
	            [ -sinTheta,  cosTheta,  0 ], $
	            [     0,         0,      1 ]]

	;Return the inverse transformation?
	if keyword_set(inverse) $
		then return, transpose(omb2smpa) $
		else return, omb2smpa
end
