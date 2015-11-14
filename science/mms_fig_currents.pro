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
function mms_fig_currents, tstart, tend, $
EIGVECS=eigvecs
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return, !Null
	endif

;-----------------------------------------------------
; Get Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Magnetic field data
	mms_fgm_ql_read, 'mms1', 'dfg', 'srvy', tstart, tend, B_DMPA=b_dmpa, TIME=t_fgm
	b_dmpa = b_dmpa[0:2,*]
	
	;Get current density from FPI
	mms_fpi_sitl_read, 'mms1', 'fast', tstart, tend, J_TOTAL=j1_total, TIME=t1_fpi
	mms_fpi_sitl_read, 'mms2', 'fast', tstart, tend, J_TOTAL=j2_total, TIME=t2_fpi
	mms_fpi_sitl_read, 'mms3', 'fast', tstart, tend, J_TOTAL=j3_total, TIME=t3_fpi
	mms_fpi_sitl_read, 'mms4', 'fast', tstart, tend, J_TOTAL=j4_total, TIME=t4_fpi
	
	;Compute the curlometer
	Jrecip = mms_fgm_curldiv(tstart, tend, TIME=t_curl)

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
	yrange += yrange * 1.1

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
	p3y_j = MrPlot( t1_fpi_ssm, j2_total[1,*], $
	                COLOR       = colors[2], $
	                NAME        = 'Jy FPI2', $
	                OVERPLOT    = p1y_j)
	p4y_j = MrPlot( t1_fpi_ssm, j3_total[1,*], $
	                COLOR       = colors[1], $
	                NAME        = 'Jy FPI3', $
	                OVERPLOT    = p1y_j)
	p5y_j = MrPlot( t1_fpi_ssm, j4_total[1,*], $
	                COLOR       = colors[0], $
	                NAME        = 'Jy FPI4', $
	                OVERPLOT    = p1y_j)
	l_jy = MrLegend( ALIGNMENT    = 'NW', $
	                 /AUTO_TEXT_COLOR, $
	                 LABEL        = ['CurlB', 'FPI1', 'FPI2', 'FPI3', 'FPI4'], $
	                 NAME         = 'Leg: Jy', $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p1y_j, p2y_j, p3y_j, p4y_j, p5y_j] )
	
	;JZ
	p1z_j = MrPlot( t_curl_ssm, Jrecip[2,*], $
	                /CURRENT, $
	                COLOR       = 'magenta', $
	                NAME        = 'Jz Recip', $
	                XTICKFORMAT = 'time_labels', $
	                XTITLE      = 'Time (UT)', $
	                YRANGE      = yrange, $
	                YTITLE      = 'Jz!C($\mu$A/m^2)')
	p2z_j = MrPlot( t1_fpi_ssm, j1_total[2,*], $
	                COLOR       = colors[3], $
	                NAME        = 'Jz FPI1', $
	                OVERPLOT    = p1z_j)
	p3z_j = MrPlot( t1_fpi_ssm, j2_total[2,*], $
	                COLOR       = colors[2], $
	                NAME        = 'Jz FPI2', $
	                OVERPLOT    = p1z_j)
	p4z_j = MrPlot( t1_fpi_ssm, j3_total[2,*], $
	                COLOR       = colors[1], $
	                NAME        = 'Jz FPI3', $
	                OVERPLOT    = p1z_j)
	p5z_j = MrPlot( t1_fpi_ssm, j4_total[2,*], $
	                COLOR       = colors[0], $
	                NAME        = 'Jz FPI4', $
	                OVERPLOT    = p1z_j)
	l_jz = MrLegend( ALIGNMENT    = 'NW', $
	                 /AUTO_TEXT_COLOR, $
	                 LABEL        = ['CurlB', 'FPI1', 'FPI2', 'FPI3', 'FPI4'], $
	                 NAME         = 'Leg: Jz', $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p1z_j, p2z_j, p3z_j, p4z_j, p5z_j] )

	win -> Refresh

	return, win
end