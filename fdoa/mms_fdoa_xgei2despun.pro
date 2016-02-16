; docformat = 'rst'
;
; NAME:
;       mms_fdoa_xgei2despun
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
;   Return a transformation matrix from GEI to a despun satellite coordinate system.
;
; :Params:
;       ATTITUDE:       in, required, type=struct
;                       Structure of attitude data returned by mms_fdoa_read_defatt.pro.
;       T_OUT:          in, optional, type=lon64arr (cdf_time_tt2000)
;                       Times to which the phase is to be interpolated.
;
; :Keywords:
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
function mms_fdoa_xgei2despun, attitude, t_out, $
TYPE=type
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(type) eq 0 then type = 'P'

	;Watch out for data gaps > 0.5 spin
	;   - phunwrap will give non-monotonic results...
	;   - check for gaps > 175 degrees, to be safe.
	;   - 175 degrees / (wZ degrees/second) * 1e9 nanoseconds/second
	;   - wZ is the angular frequency (degrees/sec) of the spin axis in BCS.
	att_gap = where((attitude.tt2000[1:*]-attitude.tt2000) gt 175*1.d9/attitude.w[2,*], n_att_gap)
	if n_att_gap gt 0 then begin
		; TODO process phunwrap on continuous segments
		message, /INFORMATIONAL, 'Data gap in definitive attitude: phase results may get corrupted'
	endif
	
	;Get the index of the tag of the specified phase-type
	iType = where(strmatch(tag_names(attitude), type, /FOLD_CASE))
	if iType lt 0 then message, 'Invalid phase type: "' + type + '".'

;---------------------------------------------------------------------
; Interpolate ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Right ascension and declination
	ra  = attitude.(iType)[0,*]
	dec = attitude.(iType)[1,*]
	
	;Unwrap the phase
	ra  = phunwrap(ra,  MAXVAL=360.0)
	dec = phunwrap(dec, MAXVAL=360.0)
	
	;Convert time to double for interpolation
	t0       = attitude.tt2000[0]
	att_sse  = double(attitude.tt2000 - t0) * 1e-9
	time_sse = double(t_out           - t0) * 1e-9
	
	;Extrapolation
	if time_sse[0] lt att_sse[0] then begin
		iExtrap = where(time_sse lt att_sse[0], nExtrap)
		if nExtrap gt 0 then message, 'Extrapolating ' + strtrim(nExtrap, 2) + ' points before.', /INFORMATIONAL
	endif
	if time_sse[-1] gt att_sse[-1] then begin
		iExtrap = where(time_sse gt att_sse[-1], nExtrap)
		if nExtrap gt 0 then message, 'Extrapolating ' + strtrim(nExtrap, 2) + ' points after.', /INFORMATIONAL
	endif
	
	;Interpolate
	ra  = interpol(ra,  att_sse, time_sse)
	dec = interpol(dec, att_sse, time_sse)

;---------------------------------------------------------------------
; Create Transformation Matrix ///////////////////////////////////////
;---------------------------------------------------------------------
	;Convert time to seconds since midnight
	MrCDF_Epoch_Breakdown, t_out, year, month, day
	t_ssm = MrCDF_epoch2ssm(t_out)

	;Create transformation matrix
	gei2despun = MrCS_gei2scs(year, month, day, t_ssm, ra, dec)
	
	return, gei2despun
end