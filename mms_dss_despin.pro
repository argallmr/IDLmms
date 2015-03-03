; docformat = 'rst'
;
; NAME:
;       MMS_DSS_Despin
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
;   Despin data.
;
; :Examples:
;   See the main level example program at the end of this document::
;       IDL> .run MMS_DSS_Despin
;
; :Categories:
;       MMS, DSS
;
; :Params:
;       SRT:            in, required, type=lon64arr
;                       Sun reference times from the DSS, in CDF_TIME_TT2000 format.
;       TIME:           in, required, type=lon64arr
;                       Time tags of the `DATA` to be despun, in CDF_TIME_TT2000 format.
;       X1:             in, required, type=3xN
;                       An array of vectors to be despun.
;
; :Keywords:
;       OFFSET:         in, optional, type=float, default=0.0
;                       Angular offset, in radians, from the sun sensor to the instrument
;                           that recorded `DATA`.
;       OMEGA:          in, optional, type=float, default=mean time between sun pulses
;                       Spin frequency of the satellite.
;
; :Returns:
;       DATA_INERT:    `DATA` in the spacecraft inertial reference frame.
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/02/18  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Test MMS_DSS_Despin
;
; :Params:
;       EXAMPLE:            in, optional, type=integer, default=1
;                           Number of the example to run.
;
; :Returns:
;       WIN:                MrWindow graphics window displaying the results.
;-
function mms_dss_despin_test, example
	compile_opt idl2
	on_error, 2

	;Default example
	if n_elements(example) eq 0 then example = 1
	
	;Generate data
	case example of
	;-----------------------------------------------------
	; Test Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		1: begin
			;Despin the data
			test_data = mms_edi_test_data()
			srt       = test_data.srt
			spin_freq = test_data.spin_freq
			time      = test_data.b_time
			b         = test_data.b
			offset    = mms_instr_origins_instr('DFG_123', 'DSS', /SPHERICAL)
			offset    = offset[0]
			
			;Rotate from DFG_123 to OCS
			dfg2ocs = mms_instr_xxyz2ocs('DFG_123')
			b       = rotate_vector(dfg2ocs, b)
		endcase
		
	;-----------------------------------------------------
	; Example Data Set \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		2: begin
			;Experimental parameters
			;   - Spin is right-handed (counter-clockwise) about the z-axis.
			;   - Instrument CS are aligned with the s/c CS, except for a single rotation about z by OFFSET.
			duration    = 10.0D * 60.0D                                 ;seconds
			sample_rate = 4.0                                           ;Samples / second
			spin_rate   = 3.0 / 60.0                                    ;# rev / sec
			offset      = 31.0 * !radeg                                 ;Radians between sun sensor and instrument
			b_vec       = [4.0, 2.0, 0.0]                               ;Magnetic field vector [Bx, By, Bz]

			;Create sun pulses in tt2000 format
			cdf_tt2000, srt,  2015, 03, 12, /COMPUTE_EPOCH              ;Start time
			nSpins    = duration * spin_rate                            ;# rev
			sun_pulse = dindgen(nSpins) / nSpins * duration             ;Seconds since start time of sun pulses
			srt      += long64(sun_pulse * 1D9)                         ;Sun pulse time vector TT2000

			;Create time tags
			cdf_tt2000, time, 2015, 03, 12, /COMPUTE_EPOCH              ;Start time
			nSamples    = duration * sample_rate                        ;# samples
			samples     = dindgen(nSamples) / nSamples * duration       ;Seconds since start time of samples    
			time       += long64(samples * 1D9)                         ;measurement time vector TT2000

			;Measurements
			;   - In the s/c CS, the fields rotate clockwise, against the s/c spin
			;   - In the s/c CS, the fields-instrument offset is opposite the instrument-fields offset
			t_temp    = (time - time[0]) * 1d-9                         ;time tags in seconds.
			spin_freq = 2.0 * !pi * spin_rate                           ;rad / s
			cos_wt    = cos(-spin_freq * t_temp - offset)               ;spin phase
			sin_wt    = sin(-spin_freq * t_temp - offset)               ;spin phase
			b         = fltarr(3, nSamples)                             ;Allocate memory
			b[0,*]    =  cos_wt * b_vec[0] + sin_wt * b_vec[1]          ;Bx in spinning CS
			b[1,*]    = -sin_wt * b_vec[0] + cos_wt * b_vec[1]          ;By in spinning CS
			b[2,*]    = replicate(b_vec[2], nSamples)                   ;Bz in spinning CS (z is spin axis)

			;Add signal to the spin-tone
			b[0,*] += 0.3 * (randomu(3, nSamples) - 0.5)
			b[1,*] += 0.3 * (randomu(2, nSamples) - 0.5)
			b[2,*] += 0.3 * (randomu(6, nSamples) - 0.5)
		endcase
		
	;-----------------------------------------------------
	; Unknown \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		else: message, 'Example (' + strtrim(example, 2) + ') is not a valid option. Try 1-2.'
	endcase
	
	;Despin the data
	b_despun  = mms_dss_despin(srt, time, b, OFFSET=offset, OMEGA=spin_freq)

	;Convert time to seconds
	t0     = time[0]
	t_temp = (time - time[0]) * 1d-9

	;View the results
	bx_plot  = plot(t_temp, b[0,*], LAYOUT=[1,3,1], TITLE='Magnetic Field', XSTYLE=1, YTITLE='$B_{x}$ (nT)', YRANGE=yrange)
	bx_plot -> Refresh, /DISABLE
	
	by_plot  = plot(t_temp, b[1,*], LAYOUT=[1,3,2], /CURRENT, XSTYLE=1, YTITLE='$B_{y}$ (nT)', YRANGE=yrange)
	bz_plot  = plot(t_temp, b[2,*], LAYOUT=[1,3,3], /CURRENT, XSTYLE=1, YTITLE='$B_{z}$ (nT)', YRANGE=yrange, XTITLE='Time (s)', YSTYLE=1)
	bxd_plot = plot(t_temp, b_despun[0,*], OVERPLOT=bx_plot, COLOR='Blue')
	byd_plot = plot(t_temp, b_despun[1,*], OVERPLOT=by_plot, COLOR='Blue')
	byd_plot = plot(t_temp, b_despun[2,*], OVERPLOT=bz_plot, COLOR='Blue')

	;Legend
	leg = legend(TARGET=[bx_plot, bxd_plot], LABEL='Spun CS', /AUTO_TEXT_COLOR, LINESTYLE=6)
	leg[1].label = 'Despun CS'
	
	bx_plot -> Refresh
	return, bx_plot.window
end


;+
;   Despin data.
;-
function mms_dss_despin, srt, time, data, $
OFFSET=offset, $
OMEGA=omega, $
TEST_DATA=test_data
	compile_opt idl2
	on_error, 2
	
	;Use test data?
	if n_elements(test_data) gt 0 then begin
		srt    = test_data.srt
		time   = test_data.b_time
		data   = test_data.b
		omega  = test_data.spin_freq
		!Null  = mms_instr_xxyz2ocs('DFG_123', ROTZ=offset, /NO_MATRIX)
	endif

	;Take the average spin rate by default
	if n_elements(omega)  eq 0 then omega  = (2.0 * !pi) / mean(srt[1:-1] - srt[0:-2])
	if n_elements(offset) eq 0 then offset = 0.0
	
	;Find the sun-reference times
	iSRT = value_locate(srt, time) > 0

	;Seconds into each spin. Convert from nano-seconds to seconds
	dt = (time - srt[iSRT]) * 1.0D-9
	
	;Radians beyond the sun detector ((s * rad/s) + rad)
	phase = (dt * omega) + offset
	
	;Cosine and sign of phase
	cos_wt = cos(phase)
	sin_wt = sin(phase)
	
	;
	; | x1 |        =  |  cos(wt)  sin(wt)  0 |  | x1 |
	; | x2 |           | -sin(wt)  cos(wt)  0 |  | x2 |
	; | x3 |_inert     |     0        0     1 |  | x3 |_spin
	;
	despun = rebin(data, 3, n_elements(time))
	if n_elements(data) eq 3 then begin
		despun[0,*] =  cos_wt * data[0] + sin_wt * data[1]
		despun[1,*] = -sin_wt * data[0] + cos_wt * data[1]
	endif else begin
		despun[0,*] =  cos_wt * data[0,*] + sin_wt * data[1,*]
		despun[1,*] = -sin_wt * data[0,*] + cos_wt * data[1,*]
	endelse

	return, despun
end


;-----------------------------------------------------
; Main Level Example Program: IDL> mms_dss_despin \\\\
;-----------------------------------------------------
;Experimental parameters
;   - Spin is right-handed (counter-clockwise) about the z-axis.
;   - Instrument CS are aligned with the s/c CS, except for a single rotation about z by OFFSET.
duration    = 10.0D * 60.0D                                 ;seconds
sample_rate = 4.0                                           ;Samples / second
spin_rate   = 3.0 / 60.0                                    ;# rev / sec
offset      = 31.0 * !radeg                                 ;Radians between sun sensor and instrument
b_vec       = [3.0, 1.5, 0.0]                               ;Magnetic field vector [Bx, By, Bz]

;Create sun pulses in tt2000 format
cdf_tt2000, srt,  2015, 03, 12, /COMPUTE_EPOCH              ;Start time
nSpins    = duration * spin_rate                            ;# rev
sun_pulse = dindgen(nSpins) / nSpins * duration             ;Seconds since start time of sun pulses
srt      += long64(sun_pulse * 1D9)                         ;Sun pulse time vector TT2000

;Create time tags
cdf_tt2000, time, 2015, 03, 12, /COMPUTE_EPOCH              ;Start time
nSamples    = duration * sample_rate                        ;# samples
samples     = dindgen(nSamples) / nSamples * duration       ;Seconds since start time of samples    
time       += long64(samples * 1D9)                         ;measurement time vector TT2000

;Measurements
;   - In the s/c CS, the fields rotate clockwise, against the s/c spin
;   - In the s/c CS, the fields-instrument offset is opposite the instrument-fields offset
t_temp    = (time - time[0]) * 1d-9                         ;time tags in seconds.
spin_freq = 2.0 * !pi * spin_rate                           ;rad / s
cos_wt    = cos(-spin_freq * t_temp - offset)               ;spin phase
sin_wt    = sin(-spin_freq * t_temp - offset)               ;spin phase
b         = fltarr(3, nSamples)                             ;Allocate memory
b[0,*]    =  cos_wt * b_vec[0] + sin_wt * b_vec[1]          ;Bx in spinning CS
b[1,*]    = -sin_wt * b_vec[0] + cos_wt * b_vec[1]          ;By in spinning CS
b[2,*]    = replicate(b_vec[2], nSamples)                   ;Bz in spinning CS (z is spin axis)

;Add signal to the spin-tone
b[0,*] += 0.3 * (randomu(3, nSamples) - 0.5)
b[1,*] += 0.3 * (randomu(2, nSamples) - 0.5)
b[2,*] += 0.3 * (randomu(6, nSamples) - 0.5)

;Despin the data
b_despun = mms_dss_despin(srt, time, b, OFFSET=offset, OMEGA=omega)

;View the results
yrange   = [-max(b_vec), max(b_vec)] * 1.5
bx_plot  = plot(t_temp, b[0,*], LAYOUT=[1,3,1], TITLE='Magnetic Field', XSTYLE=1, YTITLE='$B_{x}$ (nT)', YRANGE=yrange)
by_plot  = plot(t_temp, b[1,*], LAYOUT=[1,3,2], /CURRENT, XSTYLE=1, YTITLE='$B_{y}$ (nT)', YRANGE=yrange)
bz_plot  = plot(t_temp, b[2,*], LAYOUT=[1,3,3], /CURRENT, XSTYLE=1, YTITLE='$B_{z}$ (nT)', YRANGE=yrange, XTITLE='Time (s)')
bxd_plot = plot(t_temp, b_despun[0,*], OVERPLOT=bx_plot, COLOR='Blue')
byd_plot = plot(t_temp, b_despun[1,*], OVERPLOT=by_plot, COLOR='Blue')
byd_plot = plot(t_temp, b_despun[2,*], OVERPLOT=bz_plot, COLOR='Blue')

;Legend
leg = legend(TARGET=[bx_plot, bxd_plot], LABEL='Spun CS', /AUTO_TEXT_COLOR, LINESTYLE=6)
leg[1].label = 'Despun CS'
end