; docformat = 'rst'
;
; NAME:
;       mms_fdoa_xdsl2dmpa
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
;   Create a set of coordinate system transformation matrices from DSL coordinates to DMPA.
;
; :Params:
;       DEFATT:         in, required, type=struct
;                       Structure of attitude data returned by mms_fdoa_read_defatt.pro.
;       T_OUT:          in, optional, type=lon64arr (cdf_time_tt2000)
;                       Times to which the phase is to be interpolated.
;
; :Returns:
;       DSL2DMPA:       Rotation matrices from DSL to DMPA.
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
;       2015-11-27  -   Written by Matthew Argall
;       2017-06-21  -   Combined gei2dmpa and dsl2gei in wrong order. Fixed. - MRA
;-
function mms_fdoa_xdsl2dmpa, defatt, time
	compile_opt idl2
	on_error, 2

	;Rotations from GSE to DSL and DMPA
	gei2dsl  = mms_fdoa_xgei2despun(defatt, time, TYPE='L')
	gei2dmpa = mms_fdoa_xgei2despun(defatt, time, TYPE='P')
	
	;DSL --> GEI
	dsl2gei = transpose(temporary(gei2dsl), [1,0,2])
	
	;DSL --> DMPA
	dsl2dmpa = MrMatrix_Multiply(gei2dmpa, dsl2gei)
	
	return, dsl2dmpa
end