; docformat = 'rst'
;
; NAME:
;       mms_parse_ftime
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
;       Parse the TSTART segment of a valid MMS file name.
;
; :Categories:
;       MMS
;
; :Params:
;       TSTART:     in, required, type=string
;                   The filename of the MMS file to be dissected. Format
;                       is yyyymmddHHMMSS or yymmdd. Missing fields default
;                       to 0.
;       YEAR:       out, optional, type=string
;                   The year component.
;       MONTH:      out, optional, type=string
;                   The month component.
;       DAY:        out, optional, type=string
;                   The day component.
;       HOUR:       out, optional, type=string
;                   The hour component.
;       MINUTE:     out, optional, type=string
;                   The minutes component.
;       SECOND:     out, optional, type=string
;                   The seconds component.
;
; :Keywords:
;       TT2000:     out, optional, type=string
;                   `TSTART` converted to a CDF TT2000 epoch time.
;       INTEGER:    out, optional, type=boolean, default=0
;                   If set, `YEAR`, `MONTH`, `DAY`, `HOUR`, `MINUTE`, `SECONDS` are
;                       returned as integers instead of strings.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :Copyright:
;       Matthew Argall 2015
;
; :History::
;   Modification History::
;       2015/11/20  -   Written by Matthew Argall
;-
pro mms_parse_time, tstart, year, month, day, hour, minute, second, $
INTEGER=integer, $
TT2000=tt2000
	compile_opt strictarr
	on_error, 2

	;Must contain at least 'yyyymmdd'
	if min(strlen(tstart), imin) lt 8 then $
		message, 'Improperly formatted TSTART: "' + tstart[imin] + '".'

	;Parse the components
	year   = strmid(tstart,  0, 4)
	month  = strmid(tstart,  4, 2)
	day    = strmid(tstart,  6, 2)
	hour   = strmid(tstart,  8, 2)
	minute = strmid(tstart, 10, 2)
	second = strmid(tstart, 12, 2)
	
	;Convert to integers?
	if keyword_set(integer) then begin
		year   = fix(year)
		month  = fix(month)
		day    = fix(day)
		hour   = fix(hour)
		minute = fix(minute)
		second = fix(second)
	endif
	
	;Compute the CDF TT2000 epoch value
	if arg_present(tt2000) then begin
		tt2000 = MrCDF_Epoch_Compute(fix(year), fix(month),  fix(day), $
		                             fix(hour), fix(minute), fix(second))
	endif
end