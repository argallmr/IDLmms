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
;       1) Bx from all 4 spacecraft
;       2) By from all 4 spacecraft
;       3) Bz from all 4 spacecraft
;       4) Current density from curlometer
;       5) Current density from reciprocal vectors
;
; :Params:
;       TSTART:             in, required, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, required, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       EIGVECS:        out, optional, type=3x3 float
;                       Rotation matrix (into the minimum variance coordinate system).
;-
function mms_fig_currents, mode, tstart, tend, $
EIGVECS=eigvecs
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Accept only srvy or brst
	if mode ne 'srvy' && mode ne 'brst' then message, 'MODE must be "srvy" or "brst".'

;-----------------------------------------------------
; Get Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Magnetic field data
	mms_fgm_ql_read, 'mms1', 'dfg', mode, tstart, tend, B_DMPA=b_dmpa, TIME=t_fgm
	b_dmpa = b_dmpa[0:2,*]
	
	;Get current density from FPI
	fpi_mode = mode eq 'srvy' ? 'fast' : mode
	if fpi_mode eq 'brst' then begin
		;Read DES
		mms_fpi_l1b_moms_read, 'mms1', 'des-moms', tstart, tend, J=ji1, TIME=ti1_fpi
		mms_fpi_l1b_moms_read, 'mms2', 'des-moms', tstart, tend, J=ji2, TIME=ti2_fpi
		mms_fpi_l1b_moms_read, 'mms3', 'des-moms', tstart, tend, J=ji3, TIME=ti3_fpi
		mms_fpi_l1b_moms_read, 'mms4', 'des-moms', tstart, tend, J=ji4, TIME=ti4_fpi
		
		;Read DIS
		mms_fpi_l1b_moms_read, 'mms1', 'dis-moms', tstart, tend, J=je1, TIME=t1_fpi
		mms_fpi_l1b_moms_read, 'mms2', 'dis-moms', tstart, tend, J=je2, TIME=t2_fpi
		mms_fpi_l1b_moms_read, 'mms3', 'dis-moms', tstart, tend, J=je3, TIME=t3_fpi
		mms_fpi_l1b_moms_read, 'mms4', 'dis-moms', tstart, tend, J=je4, TIME=t4_fpi

		;Interpolate ji onto je
		ji1 = MrInterpol(ji1, temporary(ti1_fpi), t1_fpi)
		ji2 = MrInterpol(ji2, temporary(ti2_fpi), t2_fpi)
		ji3 = MrInterpol(ji3, temporary(ti3_fpi), t3_fpi)
		ji4 = MrInterpol(ji4, temporary(ti4_fpi), t4_fpi)
		
		;Total current
		j1_total = temporary(ji1) + temporary(je1)
		j2_total = temporary(ji2) + temporary(je2)
		j3_total = temporary(ji3) + temporary(je3)
		j4_total = temporary(ji4) + temporary(je4)
	endif else begin
		;Srvy data
		mms_fpi_sitl_read, 'mms1', fpi_mode, tstart, tend, J_TOTAL=j1_total, TIME=t1_fpi
		mms_fpi_sitl_read, 'mms2', fpi_mode, tstart, tend, J_TOTAL=j2_total, TIME=t2_fpi
		mms_fpi_sitl_read, 'mms3', fpi_mode, tstart, tend, J_TOTAL=j3_total, TIME=t3_fpi
		mms_fpi_sitl_read, 'mms4', fpi_mode, tstart, tend, J_TOTAL=j4_total, TIME=t4_fpi
	endelse
	
	;Compute the curlometer
	Jrecip = mms_fgm_curldiv(tstart, tend, DIVB=divB, TIME=t_curl)

;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	t0         = t_fgm[0]
	t_fgm_ssm  = MrCDF_epoch2ssm(temporary(t_fgm),  t0)
	t_curl_ssm = MrCDF_epoch2ssm(temporary(t_curl), t0)
	t1_fpi_ssm = MrCDF_epoch2ssm(temporary(t1_fpi), t0)
	t2_fpi_ssm = MrCDF_epoch2ssm(temporary(t2_fpi), t0)
	t3_fpi_ssm = MrCDF_epoch2ssm(temporary(t3_fpi), t0)
	t4_fpi_ssm = MrCDF_epoch2ssm(temporary(t4_fpi), t0)

	yrange = [min(j1_total, MAX=ymax), ymax]
	yrange += abs(yrange) * [-0.1, 0.1]

	;MMS Colors
	colors = mms_color(['blue', 'green', 'red', 'black'])

	;Create the window
	win = MrWindow(OXMARGIN=[12,7], YSIZE=600, YGAP=0.5, REFRESH=0)
	
	;Magnetic field from MMS1
	p1_fgm = MrPlot(t_fgm_ssm, b_dmpa, $
	                /CURRENT, $
	                COLOR       = colors[0:2], $
	                DIMENSION   = 2, $
	                NAME        = 'B FGM', $
	                TITLE       = 'FGM', $
	                XTICKFORMAT = '(a1)', $
	                XTITLE      = '', $
	                YTITLE      = 'B!C(nT)')
	l_b = MrLegend( ALIGNMENT    = 'NW', $
	                LABEL        = ['X', 'Y', 'Z'], $
	                NAME         = 'Leg: B', $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH = 0, $
	                TEXT_COLOR   = colors[0:2], $
	                TARGET       = p1_fgm )
	
	;JX
	p1x_j = MrPlot( t_curl_ssm, Jrecip[0,*], $
	                /CURRENT, $
	                COLOR       = 'magenta', $
	                NAME        = 'Jx Recip', $
	                XTICKFORMAT = '(a1)', $
	                XTITLE      = '', $
	                YRANGE      = yrange, $
	                YTITLE      = 'Jx!C($\mu$A/m^2)')
	p2x_j = MrPlot( t1_fpi_ssm, j1_total[0,*], $
	                COLOR       = colors[3], $
	                NAME        = 'Jx FPI1', $
	                OVERPLOT    = p1x_j)
	p3x_j = MrPlot( t2_fpi_ssm, j2_total[0,*], $
	                COLOR       = colors[2], $
	                NAME        = 'Jx FPI2', $
	                OVERPLOT    = p1x_j)
	p4x_j = MrPlot( t3_fpi_ssm, j3_total[0,*], $
	                COLOR       = colors[1], $
	                NAME        = 'Jx FPI3', $
	                OVERPLOT    = p1x_j)
	p5x_j = MrPlot( t4_fpi_ssm, j4_total[0,*], $
	                COLOR       = colors[0], $
	                NAME        = 'Jx FPI4', $
	                OVERPLOT    = p1x_j)
	l_jx = MrLegend( ALIGNMENT    = 'NW', $
	                 /AUTO_TEXT_COLOR, $
	                 LABEL        = ['CurlB', 'FPI1', 'FPI2', 'FPI3', 'FPI4'], $
	                 NAME         = 'Leg: Jx', $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p1x_j, p2x_j, p3x_j, p4x_j, p5x_j] )
	
	;JY
	p1y_j = MrPlot( t_curl_ssm, Jrecip[1,*], $
	                /CURRENT, $
	                COLOR       = 'magenta', $
	                NAME        = 'Jy Recip', $
	                XTICKFORMAT = '(a1)', $
	                XTITLE      = '', $
	                YRANGE      = yrange, $
	                YTITLE      = 'Jy!C($\mu$A/m^2)')
	p2y_j = MrPlot( t1_fpi_ssm, j1_total[1,*], $
	                COLOR       = colors[3], $
	                NAME        = 'Jy FPI1', $
	                OVERPLOT    = p1y_j)
	p3y_j = MrPlot( t2_fpi_ssm, j2_total[1,*], $
	                COLOR       = colors[2], $
	                NAME        = 'Jy FPI2', $
	                OVERPLOT    = p1y_j)
	p4y_j = MrPlot( t3_fpi_ssm, j3_total[1,*], $
	                COLOR       = colors[1], $
	                NAME        = 'Jy FPI3', $
	                OVERPLOT    = p1y_j)
	p5y_j = MrPlot( t4_fpi_ssm, j4_total[1,*], $
	                COLOR       = colors[0], $
	                NAME        = 'Jy FPI4', $
	                OVERPLOT    = p1y_j)
	
	;JZ
	p1z_j = MrPlot( t_curl_ssm, Jrecip[2,*], $
	                /CURRENT, $
	                COLOR       = 'magenta', $
	                NAME        = 'Jz Recip', $
	                XTICKFORMAT = '(a1)', $
	                YRANGE      = yrange, $
	                YTITLE      = 'Jz!C($\mu$A/m^2)')
	p2z_j = MrPlot( t1_fpi_ssm, j1_total[2,*], $
	                COLOR       = colors[3], $
	                NAME        = 'Jz FPI1', $
	                OVERPLOT    = p1z_j)
	p3z_j = MrPlot( t2_fpi_ssm, j2_total[2,*], $
	                COLOR       = colors[2], $
	                NAME        = 'Jz FPI2', $
	                OVERPLOT    = p1z_j)
	p4z_j = MrPlot( t3_fpi_ssm, j3_total[2,*], $
	                COLOR       = colors[1], $
	                NAME        = 'Jz FPI3', $
	                OVERPLOT    = p1z_j)
	p5z_j = MrPlot( t4_fpi_ssm, j4_total[2,*], $
	                COLOR       = colors[0], $
	                NAME        = 'Jz FPI4', $
	                OVERPLOT    = p1z_j)

	;DivB/mu0
	p_divB = MrPlot( t_curl_ssm, divB, $
	                 /CURRENT, $
	                 NAME        = 'DivB', $
	                 XTICKFORMAT = 'time_labels', $
	                 XTITLE      = 'Time (UTC)', $
	                 YTITLE      = 'Div(B)/$\mu$$\down0$!C($\mu$A/m$\up2$)')

	win -> Refresh

	return, win
end