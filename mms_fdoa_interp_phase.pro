; docformat = 'rst'
;
; NAME:
;       mms_fdoa_interp_phase
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
;   Interpolate spacecraft attitude phase informationl.
;
; :Params:
;       ATTITUDE:       in, required, type=struct
;                       Structure of attitude data returned by mms_fdoa_read_defatt.pro.
;       T_OUT:          in, optional, type=lon64arr (cdf_time_tt2000)
;                       Times to which the phase is to be interpolated.
;
; :Keywords:
;       SMOOTH:         in, required, type=boolean, default=0
;                       If set, the phase will be smoothed with a 60-second
;                           sliding window.
;       PERROR:         in, optional, type=
;                       
;       KEEP:           in, optional, type=intarr
;                       Indices of `T_OUT` to be kept when doing a polynomial `FIT`
;       PDIFF:          out, optional, type=fltarr
;                       Difference between the polynomial fit and original phase data.
;       FIT:            in, optional, type=boolean, default=0
;                       If set, phase will be fit with a 5th order polynomial.
;       MAXERROR:       in, optional, type=
;                       
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
;       2015-09-14  -   Written by Matthew Argall. Adapted from mms_fg_xinterp_phase.pro
;                           written by Ken Brommund for MMS AFG/DFG data processing.
;-
function mms_fdoa_interp_phase, attitude, t_out, $
SMOOTH=smooth, $
PERROR=perror, $
KEEP=keep, $
PDIFF=pdiff, $
FIT=fit, $
MAXERROR=maxerror
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_smooth = keyword_set(tf_smooth)
	if n_elements(type) eq 0 then type = 'P'

	;Watch out for data gaps > 0.5 spin
	;   - phunwrap will give non-monotonic results...
	;   - check for gaps > 175 degrees, to be safe.
	;   - 175 degrees / (wZ degrees/second) * 1e9 nanoseconds/second
	;   - wZ is the angular frequency (degrees/sec) of the spin axis in BCS.
	att_gap = where((attitude.tt2000[1:*]-attitude.tt2000) gt 175*1.d9/atitude.w[2,*], n_att_gap)
	if n_att_gap gt 0 then begin
		; TODO process phunwrap on continuous segments
		message, /INFORMATIONAL, 'Data gap in definitive attitude: phase results may get corrupted'
	endif
	
	;Get the index of the tag of the specified phase-type
	iType = where(strmatch(tag_names(attitude), type, /FOLD_CASE))
	if iType lt 0 then message, 'Invalid phase type: "' + type + '".'
	
	;Unwrap the phase -- use doubles!
	pphase = phunwrap(double(attitude.(iType)), MAXVAL=360.0)
	
	;Interpolate with doubles
	t0    = attitude.tt2000[0]
	t_rel = double(attitude.tt2000-t0)

;---------------------------------------------------------------------
; Smooth the Data ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if tf_smooth then begin
		;Pick a smoothing window of 60.0 seconds
		;   - Determine the median sampling interval and ...
		;   - number of smoothing windows in the data interval.
		windt = 60.0
		dt    = median(attitude.tt2000[1:*]-attitude.tt2000)/1e9
		nwin  = fix(windt/dt)

		;Smooth the unwrapped phase and time
		pfit  = smooth(pphase, nwin)
		t_rel = smooth(t_rel,  nwin)

		;The quantity (PFit - PPhase) loses precision with increasing time.
		;   - Interpolate onto the desired time tags before computing difference.
		diff = interpol(pfit, t_rel, double(attitude.tt2000-t0)) - pphase

		;Pad with NaNs
		pfit = [!values.d_nan, pfit, !values.d_nan]
	endif else begin
		pfit = [!values.d_nan, pphase, !values.d_nan]
	endelse

;---------------------------------------------------------------------
; Fit the Data with Polynomial ///////////////////////////////////////
;---------------------------------------------------------------------
	;Pad with arbitrary times (matches NaNs above)
	t_rel = [t_rel[0]-50, t_rel, t_rel[-1]+50]

	;Fit data
	if tf_fit then begin
		;
		; as of data for 3/19, this method is not successful when applied to one orbit of data.
		;****we may want to do this at a higher level \\\\////  will lead to jumps at 24-hr day boundaries in srvy
		; we really want to calculate smoothed phase, joining at perigee or allowing jumps at perigee.
		; requires pre-processing, or an intermediate file to save results for next day.
		;
		
		;Do a 3rd order polynomial fit to phase
		message, /INFORMATIONAL, 'Polynomial fit to phase'

		;Select all points within our desired time range
		r  = where(attitude.tt2000 gt t_out[keep[0]] and attitude.tt2000 le t_out[keep[-1]])
		t0 = attitude.tt2000[r[0]]
		
		;Polynomial fit of order 5
		order = 5
		p = poly_fit(double(attitude.tt2000[r]-t0), pphase[r], order, YFIT=pfit, YERROR=yerror)

		;Error limits for least squares polynomial.
		maxerr   = 0.05
		maxerror = 100
		if yerror gt maxerror then $
			message, string(yerror, maxerror, FORMAT='("Bad fit to phase: yerror ", F0.3, " greated than allowed ", F0.3)') 
		if keyword_set(maxerror) then maxerr = maxerror
		
		;Error limits for deviations of polynomial from original unwrapped phase.
		pdiff = pfit - pphase[r]
		bad   = where(abs(pdiff) gt maxerr, nbad)
		if nbad gt 0  then begin
			message, /INFORMATIONAL, string(nbad, maxerr, FORMAT= $
			         '(I0, " points with error greater than ", I0, " degrees set to NaN")')
			pfit[bad] = !values.d_nan
		endif
		
		;****we may want to do this at a higher level ////\\\\
		;   - Relative times for interpolation
		t_rel = double(att_tt2000[r] - t0)
		t_rel = [t_rel[0]-50, t_rel, t_rel[-1]+50]

		pfit = [!values.d_nan, pfit, !values.d_nan]
	endif

	;Interpolate polynomial and/or smoothed phase onto output time grid
	phase = interpol(pfit, t_rel, double(t_out-t0))

	return, phase
end