; docformat = 'rst'
;
; NAME:
;       mms_fig_fields.pro
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;   Create a plot of FIELDS data
;       1) DFG Magnetic Field
;       2) EDP Electric Field
;       3) EDP Spacecraft Potential
;       4) EDI 0-degree ambient counts
;       5) EDI 180-degree ambient counts
;       6) EDI Anisotropy (0/180 counts)
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           Spacecraft for which data is to be plotted.
;       TSTART:             in, required, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, required, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       EIGVECS:        out, optional, type=3x3 float
;                       Rotation matrix (into the minimum variance coordinate system).
;-
function mms_fig_fsm_psd, sc, mode, tstart, tend, $
EIGVECS=eigvecs
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif

;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;FGM Magnetic Field
	mms_fgm_ql_read, sc, 'dfg', mode, 'l2pre', tstart, tend, $
	                 B_DMPA = b_fgm_dmpa, $
	                 TIME   = t_fgm
	
	;FSM Magnetic Field
	mms_fsm_ql_read, sc, mode, tstart, tend, $
	                 B_DMPA = b_fsm_dmpa, $
	                 TIME   = t_fsm

	;Separate |B|
	bmag       = b_fgm_dmpa[3,*]
	b_fgm_dmpa = b_fgm_dmpa[0:2,*]

;-----------------------------------------------------
; Power Spectral Density \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to seconds
	t0        = t_fsm[0]
	t_fsm_ssm = MrCDF_epoch2ssm( temporary(t_fsm), t0)
	t_fgm_ssm = MrCDF_epoch2ssm( temporary(t_fgm), t0)
	
	nfft   = 1024
	nshift = nfft / 4.0
	
	;FSM
;	b_fsm_psd = MrPSD( b_fsm_dmpa, n_elements(t_fsm_ssm), 1.0/32.0, 0, $
;	                   DIMENSION   = 2, $
;	                   FMAX        = 8, $
;	                   FREQUENCIES = f_fsm, $
;	                   T0          = t_fsm_ssm[0], $
;	                   TIME        = t_fsm_psd )
	
	;FGM
;	b_fgm_psd = MrPSD( b_fgm_dmpa, n_elements(t_fgm_ssm), 1.0/16.0, 0, $
;	                   DIMENSION   = 2, $
;	                   FREQUENCIES = f_fgm, $
;	                   T0          = t_fgm_ssm[0], $
;	                   TIME        = t_fgm_psd )

	;Remove leading dimension
;	b_fsm_psd = reform(b_fsm_psd, /OVERWRITE)
;	b_fgm_psd = reform(b_fgm_psd, /OVERWRITE)

	;Median sampling interval
	dt_fsm = median( t_fsm_ssm[1:*] - t_fsm_ssm )
	dt_fgm = median( t_fgm_ssm[1:*] - t_fgm_ssm )

	;Compute the power spectral density
	bx_fsm_psd = fft_powerspectrum(b_fsm_dmpa[0,*], dt_fsm, FREQ=f_fsm)
	by_fsm_psd = fft_powerspectrum(b_fsm_dmpa[1,*], dt_fsm)
	bz_fsm_psd = fft_powerspectrum(b_fsm_dmpa[2,*], dt_fsm)
	bx_fgm_psd = fft_powerspectrum(b_fgm_dmpa[0,*], dt_fgm, FREQ=f_fgm)
	by_fgm_psd = fft_powerspectrum(b_fgm_dmpa[1,*], dt_fgm)
	bz_fgm_psd = fft_powerspectrum(b_fgm_dmpa[2,*], dt_fgm)

;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create the window
	win = MrWindow(LAYOUT=[1,3], XSIZE=600, XGAP=0.5, YGAP=0.5, YSIZE=650, REFRESH=0)
	xrange = [1e-2, 16]

	;Bx FSM
	px_fsm = MrPlot(f_fsm, bx_fsm_psd, $
	                /CURRENT, $
	                /XLOG, $
	                /YLOG, $
	                LAYOUT      = [1,3,1], $
	                NAME        = 'Bx PSD', $
	                TITLE       = 'FSM & FGM PSD', $
	                XRANGE      = xrange, $
	                XTICKFORMAT = '(a1)', $
	                YTITLE      = 'Bx PSD!C(nT^2/Hz)')

	;By FSM
	py_fsm = MrPlot(f_fsm, by_fsm_psd, $
	                /CURRENT, $
	                /XLOG, $
	                /YLOG, $
	                LAYOUT      = [1,3,2], $
	                NAME        = 'By PSD', $
	                XRANGE      = xrange, $
	                XTICKFORMAT = '(a1)', $
	                YTITLE      = 'By PSD!C(nT^2/Hz)')

	;Bz FSM
	pz_fsm = MrPlot(f_fsm, bz_fsm_psd, $
	                /CURRENT, $
	                /XLOG, $
	                /YLOG, $
	                LAYOUT      = [1,3,3], $
	                NAME        = 'Bz PSD', $
	                XRANGE      = xrange, $
	                YTITLE      = 'Bz PSD!C(nT^2/Hz)')

	;Bx FGM
	px_fgm = MrPlot(f_fgm, bx_fgm_psd, $
	                /XLOG, $
	                /YLOG, $
	                COLOR       = 'Blue', $
	                NAME        = 'Bx FGM PSD', $
	                OVERPLOT    = px_fsm)

	;By FGM
	py_fgm = MrPlot(f_fgm, by_fgm_psd, $
	                /YLOG, $
	                COLOR       = 'Blue', $
	                NAME        = 'By FGM PSD', $
	                OVERPLOT    = py_fsm)

	;Bz FGM
	pz_fgm = MrPlot(f_fgm, bz_fgm_psd, $
	                /YLOG, $
	                COLOR       = 'Blue', $
	                NAME        = 'Bz FGM PSD', $
	                OVERPLOT    = pz_fsm)

	;Legend
	lb = MrLegend(ALIGNMENT = 'NE', $
	              POSITION  = [1.0, 1.0], $
	              /RELATIVE, $
	              SAMPLE_WIDTH = 0, $
	              TEXT_COLOR   = ['Black', 'Blue'], $
	              LABEL        = ['FSM', 'DFG'], $
	              TARGET       = [px_fsm, px_fgm])

	win -> Refresh
	return, win
end