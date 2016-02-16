; docformat = 'rst'
;
; NAME:
;       mms_fdoa_xgei2bcs
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
; PURPOSE:
;+
;   Interpolate MMS definitive attitude quaternions. Spherical linear interpolation
;   is used.
;
; :Params:
;       Q:          in, required, type=4xN fltarr
;                   Quaterions to be interpolated, for GEI -> BCS transformation.
;       T_ATT:      in, required, type=lon64arr (cdf_time_tt2000)
;                   Time associated with `Q`
;       T_OUT:      in, required, type=lon64arr (cdf_time_tt2000)
;                   Times to which `Q` is to be interpolated.
;
; :Keywords:
;       INVERSE:    out, optional, type=boolean, default=0
;                   If set, the transformation from BCS -> GEI is returned.
;
; :Returns:
;       QTERP:      Quaternions, interpolated to `T_OUT` using sperical linear
;                       interpolation.
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
;       2015/11/26  -   Written by Matthew Argall
;-
function mms_fdoa_xgei2bcs, q, t_att, t_out, $
INVERSE=inverse

	; Defaults
	tf_inverse = keyword_set(inverse)

	;
	; Spacecraft attitude (quaternion). 
	;   NOTE: this has noise at level of
	;       0.023 deg for spin phase
	;       0.008 deg for x-y axis
	;

	; 
	; TODO: watch out for data gaps > 0.5 spin period.
	;
	
	; Times cannot be int64
	;   - Reference epoch is the first attitude time stamp
	t0 = t_att[0]
	
	; Inteprolate the quaternions
	;   - Use SLERP for smoother interpolation.
	Qinterp = qterp( double(t_att - t0), q, double(t_out - t0), /SLERP )

	; Invert
	;   - BCS --> GEI
	if tf_inverse then Q[0:2,*] = -Q[0:2,*]

	return, Qinterp
end