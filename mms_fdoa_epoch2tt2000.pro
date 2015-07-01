; docformat = 'rst'
;
; NAME:
;    mms_fdoa_read_defatt
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
;   Convert FDOA epoch times to CDF TT2000 times.
;
; :Categories:
;    MMS, FDOA, ATTITUDE, EPHEMERIS
;
; :Examples:
;   Convert UTC to TT2000
;       IDL> tt2000 = mms_fdoa_epoch2tt2000( '2015-079T00:59:04.312' )
;       IDL> print, tt2000
;           480085211496000000
;       IDL> tt2000 = mms_fdoa_epoch2tt2000( '2015-079/00:59:04.312', /EPHEMERIS )
;       IDL> print, tt2000
;           480085211496000000
;       IDL> print, cdf_encode_tt2000(tt2000)
;           2015-03-20T00:59:04.312000000
;
;   Convert TAI to TT2000
;       IDL> tt2000 = mms_fdoa_epoch2tt2000( 1805504379.312D, /TAI )
;       IDL> print, tt2000
;           480085211496000000
;       IDL> print, cdf_encode_tt2000(tt2000)
;           2015-03-20T00:59:04.312000000
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
;    Modification History::
;       2015/06/29  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Read the header of MMS definitive attitude files.
;
; :Params:
;       FDOA_EPOCH:     in, required, type=double/string
;                       FDOA Attitude or Ephemeris epoch times.
;
; :Keywords:
;       EPHEMERIS:      in, optional, type=boolean, default=0
;                       If set, ephemeris times were given. Attitude times are assumed.
;       TAI:            in, optional, type=boolean, default=0
;                       If set, TAI time offsets are given. UTC strings are assumed.
;
; :Returns:
;       TT2000:         CDF TT2000 times converted from `FDOA_EPOCH`.
;-
function mms_fdoa_epoch2tt2000, fdoa_epoch, $
EPHEMERIS=ephemeris, $
TAI=tai
	compile_opt idl2
	on_error, 2
	;
	; COMMENT           The time tags are given twice. First as calendar UTC time,
	; COMMENT           and then as TAI, expressed as elapsed SI seconds since
	; COMMENT           the mission reference epoch, 1958-001T00:00:00 UTC.
	;
	; COMMENT   Time (UTC)    Elapsed Sec
	; 2015-079T00:59:04.312 1805504379.312
	;
	
	;Defaults
	tai       = keyword_set(tai)
	ephemeris = keyword_set(ephemeris)

;-------------------------------------------------------
; TAI to TT2000 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-------------------------------------------------------
	if tai then begin
		case size(fdoa_epoch, /TNAME) of
			'STRING': _epoch = double(fdoa_epoch)
			'DOUBLE': _epoch = fdoa_epoch
			else: message, 'FDOA_EPOCH must be double precision.'
		endcase
		
		;Ephemeris epoch is given in number of days.
		;   - Convert to seconds.
		if ephemeris then begin
			_epoch = _epoch * 86400.0D
		end
		
		;Convert 1958-001T00:00:00 UTC to TT2000 (TAI)
		ref_epoch = cdf_parse_tt2000('1958-01-01T00:00:00.000.000.000')

		;Convert TAI seconds to nanoseconds
		tai = long64(_epoch * 1d9)
		
		;Convert to TT2000
		tt2000 = tai + ref_epoch

;-------------------------------------------------------
; UTC to TT2000 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-------------------------------------------------------
	endif else begin
		;
		; Expected:
		;   yyyy-mm-ddTHH:MM:SS.mmmuuunnn
		;
		; Times:
		;   - Attitude:  2015-079T00:59:04.312
		;   - Ephemeris: 2015-073/03:00:25.000
		;
		
		;Separate year, day-of-year, and time (extend to nanoseconds)
		year = strmid(fdoa_epoch, 0, 4)
		doy  = strmid(fdoa_epoch, 5, 3)
		time = strmid(fdoa_epoch, 9) + '.000.000'
		
		;Convert doy-of-year to month-day
		date  = MrDOY2Date(fix(doy), fix(year))
		day   = reform(string(date[0,*], FORMAT='(i02)'))
		month = reform(string(date[1,*], FORMAT='(i02)'))
	
		;Re-build the date
		utc    = year + '-' + month + '-' + day + 'T' + time

		;Convert to TT2000
		tt2000 = cdf_parse_tt2000(utc)
	endelse
	
	return, tt2000
end
