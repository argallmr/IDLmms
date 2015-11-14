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
function mms_fig_curlometer, tstart, tend, $
EIGVECS=eigvecs
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg(/QUIET)
		return, !Null
	endif

;-----------------------------------------------------
; Get Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Magnetic field data
	mms_fgm_ql_read, 'mms1', 'dfg', 'srvy', tstart, tend, B_DMPA=b1, TIME=t1
	mms_fgm_ql_read, 'mms2', 'dfg', 'srvy', tstart, tend, B_DMPA=b2, TIME=t2
	mms_fgm_ql_read, 'mms3', 'dfg', 'srvy', tstart, tend, B_DMPA=b3, TIME=t3
	mms_fgm_ql_read, 'mms4', 'dfg', 'srvy', tstart, tend, B_DMPA=b4, TIME=t4
	
	;Compute the curlometer
	Jrecip = mms_fgm_curldiv(tstart, tend, JCURL=Jcurl, TIME=tj)

;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	t0     = t1[0]
	t1_ssm = MrCDF_epoch2ssm(temporary(t1), t0)
	t2_ssm = MrCDF_epoch2ssm(temporary(t2), t0)
	t3_ssm = MrCDF_epoch2ssm(temporary(t3), t0)
	t4_ssm = MrCDF_epoch2ssm(temporary(t4), t0)
	tj_ssm = MrCDF_epoch2ssm(temporary(tj), t0)

	;MMS Colors
	colors = mms_color(['blue', 'green', 'red', 'black'])

	;Create the window
	win = MrWindow(YSIZE=600, YGAP=0.5, REFRESH=0)

	;BX
	p1x_fgm = MrPlot( t1_ssm, b1[0,*], $
	                  /CURRENT, $
	                  COLOR       = colors[3], $
	                  NAME        = 'Bx FGM1', $
	                  TITLE       = 'FGM', $
	                  XTICKFORMAT = '(a1)', $
	                  XTITLE      = '', $
	                  YTITLE      = 'Bx!C(nT)')
	p2x_fgm = MrPlot( t2_ssm, b2[0,*], $
	                  COLOR       = colors[2], $
	                  NAME        = 'Bx FGM2', $
	                  OVERPLOT    = p1x_fgm)
	p3x_fgm = MrPlot( t3_ssm, b3[0,*], $
	                  COLOR       = colors[1], $
	                  NAME        = 'Bx FGM3', $
	                  OVERPLOT    = p1x_fgm)
	p4x_fgm = MrPlot( t4_ssm, b4[0,*], $
	                  COLOR       = colors[0], $
	                  NAME        = 'Bx FGM4', $
	                  OVERPLOT    = p1x_fgm)
	l_fgm = MrLegend( ALIGNMENT    = 'NE', $
	                  /AUTO_TEXT_COLOR, $
	                  LABEL        = ['MMS1', 'MMS2', 'MMS3', 'MMS4'], $
	                  POSITION     = [1.0, 1.0], $
	                  /RELATIVE, $
	                  SAMPLE_WIDTH = 0, $
	                  TARGET       = [p1x_fgm, p2x_fgm, p3x_fgm, p4x_fgm] )
	
	;BY
	p1y_fgm = MrPlot( t1_ssm, b1[1,*], $
	                  /CURRENT, $
	                  COLOR       = colors[3], $
	                  NAME        = 'By FGM1', $
	                  XTICKFORMAT = '(a1)', $
	                  XTITLE      = '', $
	                  YTITLE      = 'By!C(nT)')
	p2y_fgm = MrPlot( t2_ssm, b2[1,*], $
	                  COLOR       = colors[2], $
	                  NAME        = 'By FGM2', $
	                  OVERPLOT    = p1y_fgm)
	p3y_fgm = MrPlot( t3_ssm, b3[1,*], $
	                  COLOR       = colors[1], $
	                  NAME        = 'By FGM3', $
	                  OVERPLOT    = p1y_fgm)
	p4y_fgm = MrPlot( t4_ssm, b4[1,*], $
	                  COLOR       = colors[0], $
	                  NAME        = 'By FGM4', $
	                  OVERPLOT    = p1y_fgm)
	
	;BZ
	p1z_fgm = MrPlot( t1_ssm, b1[2,*], $
	                  /CURRENT, $
	                  COLOR       = colors[3], $
	                  NAME        = 'Bz FGM1', $
	                  XTICKFORMAT = '(a1)', $
	                  XTITLE      = '', $
	                  YTITLE      = 'Bz!C(nT)')
	p2z_fgm = MrPlot( t2_ssm, b2[2,*], $
	                  COLOR       = colors[2], $
	                  NAME        = 'Bz FGM2', $
	                  OVERPLOT    = p1z_fgm)
	p3z_fgm = MrPlot( t3_ssm, b3[2,*], $
	                  COLOR       = colors[1], $
	                  NAME        = 'Bz FGM3', $
	                  OVERPLOT    = p1z_fgm)
	p4z_fgm = MrPlot( t4_ssm, b4[2,*], $
	                  COLOR       = colors[0], $
	                  NAME        = 'Bz FGM4', $
	                  OVERPLOT    = p1z_fgm)

	;J CURL
	p_recip = MrPlot( tj_ssm, Jrecip, $
	                  /CURRENT, $
	                  COLOR       = colors[0:2], $
	                  DIMENSION   = 2, $
	                  NAME        = 'J ReciprocalVectors', $
	                  XTICKFORMAT = '(a1)', $
	                  YTITLE      = 'J!C($\mu$A/m$\up2$)')
	
	;J CURLOMETER
	p_cmtr = MrPlot( tj_ssm, Jcurl, $
	                  /CURRENT, $
	                  COLOR       = colors[0:2], $
	                  DIMENSION   = 2, $
	                  NAME        = 'J Curlometer', $
	                  XTICKFORMAT = 'time_labels', $
	                  XTITLE      = 'Time (UTC)', $
	                  YTITLE      = 'J!C($\mu$A/m$\up2$)')

	win -> Refresh
	return, win
end