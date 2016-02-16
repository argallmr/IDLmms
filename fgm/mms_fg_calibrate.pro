; docformat = 'rst'
;
; NAME:
;    mms_fg_calibrate
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
;   Calibrate MMS fluxgate magnetometer data.
;
; :Categories:
;    MMS, DFG, AFG
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
;       2015-05-03  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Read fluxgate calibration files.
;
; :Params:
;       B_123:          in, required, type=3xN float
;                       Vector magnetic field in the non-orthogonal sensor 123 system.
;       T:              in, required, type=int64 (cdf_time_tt2000)
;                       Time tags for `B_123`.
;       CAL_PARAMS:     in, required, type=struct
;                       Structure of calibration parameters returned by mms_fg_read_cal.
;
; :Returns:
;       B_OMB:          Magnetic field data in the orthogonalized OMB system.
;-
function mms_fg_calibrate_apply, b_123, t, cal_params, $
MPA=mpa
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Locate B in Cal Param Array //////////////////////////
;-------------------------------------------------------

	;
	;Interpolate the calibration parameters
	;  - Always take the last calibrated point (no interpolation).
	;  - Ken: cal values will always be constant for L1B,
	;         regardless of how new the cal file is.
	;
	;mms_fg_calparams2matrix is super slow because it has to find the
	;inverse of all of the orthogonalization matrices. Since all we are
	;doing is mapping each point in B_123 onto the last calibration time,
	;there is no need to obtain calibration parameters for every value
	;of B_123. We need only to compute the few matrices within our time
	;of interest and apply the same matrix repeatedly to the appropriate
	;values oB_123
	;
	
	;
	;Do not find a calibration parameter at each point
	;
	;    cal = mms_fg_interp_cal(cal_params, time, true)
	;
	;Instead, map each value of B_123 onto the appropriate set of
	;calibration parameters.
	;
	
	;Should not have values before the first calibration time
	if t[0] lt cal_params.epoch[0] then $
		message, 'Times must be after the first calibration period.'
	
	;Map times to closest calibration time.
	inds = value_locate(cal_params.epoch, t)

;-------------------------------------------------------
; Apply Calibration Parameters /////////////////////////
;-------------------------------------------------------
	;Subtract DC offsets
	b_omb      = B_123
	b_omb[0,*] = B_123[0,*] - cal_params.offset[0,inds]
	b_omb[1,*] = B_123[1,*] - cal_params.offset[1,inds]
	b_omb[2,*] = B_123[2,*] - cal_params.offset[2,inds]
	
	;Create orthogonalization matrix
	x123toOMB = mms_fg_calparams2matrix( cal_params.gain, cal_params.dtheta, $
	                                     cal_params.dphi, cal_params.u3 )

	;Orthogonalize the data
	b_omb = mrvector_rotate( x123toOMB[*,*,inds], b_omb )

	;Major principle axis
	mpa = cal_params.mpa[*,inds]

	;Return the data
	return, b_omb
end


;+
;   Calibrate hi- and lo-range fluxgate magnetometer data.
;
; :Params:
;       B_123:          in, required, type=3xN float
;                       Vector magnetic field in the non-orthogonal sensor 123 system.
;       T:              in, required, type=int64 (cdf_time_tt2000)
;                       Time tags for `B_123`.
;       RANGE:          in, required, type=integer
;                       Range flags.
;       T_RANGE:        in, required, type=int64 (cdf_time_tt2000)
;                       Time tags for `range`.
;       HICAL:          in, required, type=struct
;                       Structure of hi-range calibration parameters returned by mms_fg_read_cal.
;       LOCAL:          in, required, type=struct
;                       Structure of lo-range calibration parameters returned by mms_fg_read_cal.
;
; :Keywords:
;       MPA:            out, optional, type=3xN float
;                       The major principle axis, as viewed from BCS.
;
; :Returns:
;       B_OMB:          Magnetic field data in the orthogonalized OMB system.
;-
function mms_fg_calibrate, b_123, t, range, t_range, hiCal, loCal, $
MPA=mpa
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Determine Range //////////////////////////////////////
;-------------------------------------------------------
	;Find the location of mag data within the range data
	;   - RANGE is reported once per packet
	;   - All mag data within said packet is at the same range
	irange = value_locate(t_range, t) > 0
	
	;Check if points do not have a range flag
	if t[0] lt t_range[0] then begin
		iBefore = where(t lt t_range[0], nBefore)
		message, 'First ' + strtrim(nBefore, 2) + ' points lack range info.', /INFORMATIONAL
	endif
	
	;Check if points do not have a range flag
	if t[-1] lt t_range[-1] then begin
		;Packet duration
		dt_range = median(t_range[1:*] - t_range)
	
		;Any points after the last packet?
		iAfter = where(t ge t_range[-1] + dt_range, nAfter)
		message, 'Last ' + strtrim(nAfter, 2) + ' points lack range info.', /INFORMATIONAL
	endif
	
	;Separate hi- and lo-range indices
	;   1 if hi-range
	;   0 if lo-range
	iHi = where(range[irange], nHi, COMPLEMENT=iLo, NCOMPLEMENT=nLo)

;-------------------------------------------------------
; Apply Calibration Parameters /////////////////////////
;-------------------------------------------------------

	;Allocate memory to output
	b_omb = b_123
	
	;Hi-range
	if nHi gt 0 then b_omb[*, iHi] = mms_fg_calibrate_apply(B_123[*, iHi], t[iHi], hiCal, MPA=hiMPA)
	if nLo gt 0 then b_omb[*, iLo] = mms_fg_calibrate_apply(B_123[*, iLo], t[iLo], loCal, MPA=loMPA)
	
	;Also return MPA axis?
	if arg_present(mpa) then begin
		mpa = b_omb
		if nHi gt 0 then mpa[*,iHi] = hiMPA
		if nLo gt 0 then mpa[*,iLo] = loMPA
	endif
	
	;Return the data
	return, b_omb
end
