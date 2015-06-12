; docformat = 'rst'
;
; NAME:
;       mms_dss_sunpulse2phase
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
;   Takes sun pulse structure returned by mms_dss_read_sunpulse, returns an array
;   of spin phases (degrees) for each input epoch (tt2000)
;
; :Categories:
;   MMS, DSS
;
; :Params:
;       HK:                 in, required, type=structure
;                           Structure of sunpulse data returned by mms_dss_read_sunpulse.
;       EPOCH:              in, optional, type=int64 (cdf_time_tt2000)
;                           CDF epoch times at which the spin-phase is desired.
;
; :Keywords:
;       FLAG:           out, optional, type=byte
;                       Flag specifying quality of period used to determine phase.
;                           0 if interpolated between consecutive sun pulses, or average
;                             spin rate used to fill a gap < 4 spins.
;                           1 average spin rate used to fill gap > 4 spins
;                           2 average spin rate used for gap < 4 spins 
;                             leads to cumulative phase error > 90 degrees.
;                           3 true number of spins to fill gap > 4 spins is questionable.
;                             Estimates from initial spin rate and final spin rate differ
;                             by more than 0.5 spins, but both round to the same integer
;                             number of spins.
;                           4 true number of spins to fill gap > 4 spins is questionable.
;                             spin period extrapolated from beginning of gap gives cumulative
;                             phase error < 10 degrees, but number of spins disagrees 
;                             with estimate using end spin period, or end spin period is
;                             unavailable.
;                           5 true number of spins to fill gap > 4 spins is questionable.
;                             spin period extrapolated from end of gap gives cumulative
;                             phase error < 10 degrees. but number of spins disagrees
;                             with estimate using beginning spin period, or beginning 
;                             spin period unavailable.
;                           6 true number of spins to fill gap > 4 spins is questionable.
;                             Number of spins to fill gap is average of estimates using 
;                             beginning and ending spin periods.  
;                           10 - 16 extrapolated beyond first/last sunpulse. 10 is added 
;                             to quality flag of period used for extrapolation.
;                           20 - 26 extrapolated more than 3 spins beyond first/last sunpulse
;                             20 is added to quality flag of period used for extrapolation.
;       PERIOD:         out, optional, type=int64arr
;                       Spin period, in nano-seconds.
;       T_FLAG:         out, optional, type=bytarr
;                       Flag for `PERIOD` quality with same meaning as `FLAG`.
;       SUNPULSE:       out, optional, type=int64arr (cdf_time_tt2000)
;                       Sunpulse times.
;
; :Returns:
;       PHASE:          Spin-phase, in degrees
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
;    Modification History::
;        2014-05-01  -   Written by Matthew Argall. Adapted from Ken Bromund's
;                            mms_fg_sunpulse2phase.
;-
function mms_dss_sunpulse2phase, hk, epoch, $
FLAG=flag, $
PERIOD=dPulse, $
T_FLAG=dPulse_flag, $
SUNPULSE=sunpulse
	compile_opt idl2
	on_error, 2
	;
	;-----------------------------
	; FROM: Ken Bromund          |
	;-----------------------------
	;
	; To calculate phase, we need a unique, monotonically increasing array of 
	; sun pulses, with corresponding spin periods.
	; However, we expect to have complications in the 0x101 data:
	; 1) The same sun-pulse time will be repeated in consecutive 0x101 packets.
	; and/or
	; 2) Missing sun pulses. E.g. if spin period is less than the 0x101 cadence of
	; 10 sec.  After I wrote the code to handle this, I learned that 
	; during SDP deployment, the cadence of 0x101 packets will be decreased to 
	; 1 sec.  However, I am leaving the code as-is just in case of error in 
	; operations, and because the same code handles any possible small data gaps.
	;
	; In addition, this code anticipates larger data gaps.
	; 
	; 
	; We are given the spin period in IIFSUNPER (assuming sunssps is 0), but this
	; has several issues:
	; 1) not always available (e.g. when in shadow)
	; 2) based on a clock that exhibits temperature drift, not correct TAI seconds.
	;    25 microsec temperature drift obseved in MRT9b data. 
	; 3) Phase calculation must be based on the sun pulse times, which have jitter,
	;    so even if IIFSUNPER is the correct time, it could not be used with the
	;    sun pulse times without introducing phase discontinuities.  
	; For spin period, I use the actual delta-t between sun-pulse times provided
	; by the CIDP.  These times are relative to TAI, but do exhibit some jitter
	; (observed up to 15 microsec, but sigma is about 1 or 2 microsec)
	;
	; I will use IIFSUNPER only for the phase before the initial point of the file 
	; and after large data gaps (where phase continuity is not an issue).
	; It is also used to validate spin period calculations (ie to validate 
	; correct number of spins between pulses).
	; 
	; A slight improvement could be made by using IIFSUNPER to generate a 
	; 'pseudo sun pulse time' to reduce the data gap size by 1, whenever 
	; sun pulses are missing in the smaller gaps. 
	; For now, I assume that the spin rate is changing
	; slowly enough that it is acceptable to assume a constant, average spin rate 
	; accross small gaps.

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Outline of Algorithm
	; 1) get a unique set of sun pulses
	; 2) find spin period that takes us exactly from one pulse to the next, 
	;    dealing with data gaps
	; 2a) small data gaps, where it is safe to assume the spin period has not 
	;     changed significantly enough to allow for uncertainty in the number of
	;     spins during the gap.
	; 2b) large data gaps, where spin period might have changed enough that there
	;     is uncertainty in the number of spins during the gap.
	; 3) calculate phase, 
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;Make editable copies of data
	;   - Make sure arrays are Nx1, not 1xN
	hk_epoch = reform(hk.epoch)
	sunpulse = reform(hk.sunpulse)
	period   = reform(hk.period) * 1000LL     ;Convert from micro- to nano-seconds
	flag     = reform(hk.flag)
	
	;Number of points
	nPts = n_elements(sunpulse)

	;Valid period
	;    - PERIOD is technically valid if FLAG eq 0 and if PERIOD NE 0, but
	;      I do some sanity checking, too.
	valid_period = flag eq 0 and period lt 50e9 and period gt 2e9

	;Calculate the period
	;   - SUNPULSE is recorded every sun pulse, so the difference between
	;     pulse times should be the spin period. This should be comparable
	;     to PERIOD.
	if nPts eq 1 then begin
		;Cannot determine first period accurately
		if flag[0] ne 0 then $
			message, 'Single period with flag ' + strtrim(flag[0], 2), /INFORMATIONAL
		
		;Use the period given
		dPulse      = period[0]
		dPulse_flag = 0
	endif else begin
		dPulse      = double(sunpulse[1:*] - sunpulse)
		dPulse_flag = bytarr(n_elements(period))
	endelse

	;If the first period is a valid value, it can be used to create
	;an epoch time just prior to the start of the data interval.
	if valid_period[0] then begin
		pseudopulse  = sunpulse[0] - period[0]
		sunpulse     = [ pseudopulse, sunpulse     ]
		dPulse       = [ period[0],   period       ]
		dPulse_flag  = [ 0,           dPulse_flag  ]
		period       = [ period[0],   period       ]
		valid_period = [ 0,           valid_period ]
		flag         = [ 3,           flag         ] 
	endif

;-----------------------------------------------------
; Data Intervals & Gaps \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Detect large gaps, and separate data into segments between large gaps
	;   - Look for gaps in DPULSE
	T_median = median(dPulse)
	iGaps    = MrGapsX(dPulse, T_median, GAPSIZE=nInGap)

	;Assume that the period does not change for gaps less than NMINGAP points
	nMinGap = 4
	iFill   = where(nInGap gt nMinGap, nGaps)
	if nGaps gt 0 then iGaps = iGaps[*,iFill]

	;Warn about large gaps
	if nGaps gt 0 then begin
		msg = string(FORMAT='(%"%i gaps > %0.2 seconds (%i spins) in sunpulse data.")', $
		             nGaps, double(T_median) * 1d-9 * nMinGap, nMinGap)
		message, msg, /INFORMATIONAL
	endif

	;Insert a pseudo sun pulse before each big gap
	;   - The first period is a valid value, so can be used to create
	;     an epoch time just prior to the end of a gap.
	for i = 0, nGaps - 1 do begin
		;Index at which to add a pseudo pulse
		;   - We already took care of the first interval
		;   - Do so only for the gaps larger than NBIGGAPS periods
		;   - IGAP is the index of the gap of interest.
		;   - We want to add a point before the gap ends.
		idx = iGaps[1,i]
		
		;Add a pseudo sunpulse
		if valid_period[idx] then begin
			speudopulse  = pulse[idx] - period[idx]
			sunpulse     = [ sunpulse[1:idx-1],      speudopulse,  sunpulse[idx:*]     ]
			dPulse       = [ dPulse[1:idx-1],        period[idx],  dPulse[idx:*]       ]
			dPulse_flag  = [ dPulse_flag[1:idx-1],   0,            dPulse_flag[idx:*]  ]
			period       = [ period[1:idx-1],        period[idx],  period[idx:*]       ]
			valid_period = [ valid_period[1:idx-1],  0,            valid_period[idx:*] ]
			flag         = [ flag[1:idx-1],          3,            flag[idx:*]         ]
	
			;Bump the gap indices forward one
			iGaps[igap+1:*] += 1
		endif
	endfor
	
	;Number of points
	n_dpulse = n_elements(dpulse)

;-----------------------------------------------------
; Smooth Intervals of Data \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Median smooth
	nMedFilt  = 7
	nHalfFilt = nMedFilt / 3
	istart    = 1

	;There is one more interval than the number of gaps.
	for i = 0, nGaps do begin
		;End of smooth interval
		if i eq nGaps $
			then istop = n_elements(dPulse) - 1 $
			else istop = iGaps[1,i]
		
		;Extract for ease of use
		temp_pulse  = sunpulse[istart:istop]
		temp_dPulse = dPulse[istart:istop]
		temp_Tvalid = valid_period[istart:istop]
		temp_period = period[istart:istop]
		temp_dFlag  = dPulse_flag[istart:istop]
		nPts        = istop - istart + 1
	
		;Median smooth if there are enough points
		if (istop - istart + 1) ge nMedFilt then begin
			;Median smooth
			T_filt = median(temp_dPulse, nMedFilt)
			
			;Correct the first and last half filter window
			T_filt[0:nHalfFilt]                   = T_filt[nHalfFilt+1]
			T_filt[nPts-nHalfFilt-1:nPts-1] = T_filt[nPts-nHalfFilt-2]
		endif else begin
			;Take the median value
			T_filt = replicate( median(temp_dPulse), istop-istart+1)
		endelse
		
		;Use the measured period if it is valid
		iValid = where(temp_Tvalid, nValid)
		if nValid gt 0 then begin
			T_filt[iValid] = temp_period[iValid]
			
		;Warn that no valid sunpulses exist
		endif else begin
			ttemp = MrCDF_Epoch_Encode(temp_pulse[[0, nPts-1]])
			msg = string( FORMAT='(%"No valid periods in interval %s - %s")', $
		                  strmid(ttemp, 0, 20) )
			message, msg, /INFORMATIONAL
		endelse
		
		;Number of spins between sunpulses
		;   - Recall, DPULSE is the time between sun pulses, not the reported period
		;   - There could still be gaps smaller than NMINGAP periods
		nspins = temp_dPulse / T_median
		
		;Find spins that are not integer multiples of the median period
		iNotIntSpin = where( abs( nspins - round(nspins) ) gt 0.25, nNotIntSpin )
		if nNotIntSpin gt 0 then begin
			;Warning message
			ttemp = MrCDF_Epoch_Encode(temp_pulse[[0, nPts-1]])
			msg = string( FORMAT='(%"No valid periods in interval %s - %s")', $
			              strmid(ttemp, 0, 20) )
			message, msg, /INFORMATIONAL
			
			;Set the flag
			temp_dFlag[iNotIntSpin] = 2
		endif
		
		;Use the average period between pulses
		;   - Divide total time by number of expected spins
		dPulse[istart:istop]      /= round(nspins)
		dPulse_flag[istart:istop]  = temp_dFlag
	endfor

;-----------------------------------------------------
; Interpolate Over Large Gaps \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; "Interpolation" involves using the same period for all
	; points in TIMES that fall within a data gap.
	;
	; Figure out the average period within a large data gap
	;   1. Determine the period before the gap
	;   2. Determine the period after the gap
	;   3. Compare the two and decide which period to use
	;
	for i = 0, nGaps - 1 do begin
		;Index of the point just prior to a data gap
		idx = iGaps[0,i]
		
		;Period just prior to a large data gap
		;   - Use the DSS period, if it is valid
		;   - Cannot determine the period before the data begins
		if valid_period[idx] then begin
			T1 = period[idx]
		endif else if idx gt 1 then begin
			T1 = dPulse[idx]
		endif else begin
			ttemp = MrCDF_Epoch_Encode(sunpulse[idx])
			msg = string( FORMAT='(%"Cannot determine period before gap at %s")', $
			              strmid(ttemp, 0, 20) )
			message, msg, /INFORMATIONAL
			T1 = !values.f_nan
		endelse
		
		; Period just after a large data gap
		;   - The DSS period will not be valid, because we added
		;     a pseudopulse above.
		;   - Make sure the next valid point is not also the beginning
		;     of another data gap. This would occur if there is a
		;     single point stranded between two gaps.
		if idx + 1 eq iGaps[1,i] then begin
			ttemp = MrCDF_Epoch_Encode(sunpulse[idx])
			msg = string( FORMAT='(%"Cannot determine period after gap at %s")', $
			              strmid(ttemp, 0, 20) )
			message, msg, /INFORMATIONAL
			T2 = !values.f_nan
		endif else begin
			T2 = dPulse[idx+1]
		endif
		
		;Determine the number of spins in the gap. Use the period
		;before and after the gap. They should be the same.
		nSpin1 = dPulse[idx+1] / T1
		nSpin2 = dPulse[idx+1] / T2
		
		;Check if they are the same
		if round(nSpin1) eq round(nSpin2) then begin
			nSpins           = nSpin1
			dPulse_flag[idx] = 1
			
			; Do they differ by more than half a spin?
			if abs(nSpin1 - nSpin2) ge 0.5 then begin
				ttemp = MrCDF_Epoch_Encode( pulse[idx] )
				msg = string( FORMAT='(%"Spin rates that bound gap at %s differ by >= 0.5 spins.")', $
				              strmid(ttemp, 0, 20) )
				message, msg, /INFORMATIONAL
				dPulse_flag[idx] = 3
			endif
			
		;Use the first spin period if it is < 10 degrees from expected
		;  10 = 360 / (9 * 4) = 360 * 1/9 * 1/4 = 360 * 0.25 * 0.111
		endif else if abs(nSpin1 - round(nSpin1)) lt 0.25 * 0.111 then begin
			nSpins           = nSpin1
			dPulse_flag[idx] = 4
			
			;Warn
			ttemp = MrCDF_Epoch_Encode( pulse[idx] )
			msg = string( FORMAT='(%"Using period at beginning of gap %s.")', $
			              strmid(ttemp, 0, 20) )
			message, msg, /INFORMATIONAL
			
		;Use the second spin period
		endif else if abs(nSpin2 - round(nSpin2)) lt 0.25 * 0.111 then begin
			nSpins           = nSpin2
			dPulse_flag[idx] = 4
			
			;Warn
			ttemp = MrCDF_Epoch_Encode( pulse[idx] )
			msg = string( FORMAT='(%"Using period at beginning of gap %s.")', $
			              strmid(ttemp, 0, 20) )
			message, msg, /INFORMATIONAL
		
		;Use the average period
		endif else begin
			nSpins           = (nSpin1 + nSpin2) / 2.0
			dPulse_flag[idx] = 5
			
			;Warn
			ttemp = MrCDF_Epoch_Encode( pulse[idx] )
			msg = string( FORMAT='(%"Using average period before and after gap %s")', $
			              strmid(ttemp, 0, 20) )
			message, msg, /INFORMATIONAL
		endelse
		
		;Select the average period
		dPulse[idx] /= nSpins
	endfor

;-----------------------------------------------------
; Determine Spin Phase \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Determine where times are located within the sunpulse array
	inds = value_locate(sunpulse, epoch) > 0

	;Compute phase
	;   - degrees * nano-seconds / ( nano-seconds / spin )
	phase = 360.0 * double(epoch - sunpulse[inds]) / double(period[inds])
	phase = phase mod 360.0

;-----------------------------------------------------
; Did We Extrapolate? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Extrapolate before
	iBefore = where(epoch lt sunpulse[0], nBefore)
	if nBefore gt 0 then begin
		nExtrap = MrCDF_epoch2sse(epoch[0], sunpulse[0]) / T_median
		if nExtrap gt 3 then $
			message, 'Extrapolating more than 3 spins before first sunpulse.', /INFORMATIONAL
	endif
	
	;Extrapolate after
	iAfter = where(epoch gt sunpulse[n_elements(sunpulse)-1], nAfter)
	if nAfter gt 0 then begin
		nExtrap = MrCDF_epoch2sse(epoch[n_elements(epoch)-1], sunpulse[n_elements(sunpulse)-1]) / T_median
		if nExtrap gt 3 then $
			message, 'Extrapolating more than 3 spins before first sunpulse.', /INFORMATIONAL
	endif
	
	return, phase
end