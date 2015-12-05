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
function mms_fig_fpi_moms, sc, mode, tstart, tend, $
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
;	mms_fgm_ql_read, 'mms1', 'dfg', mode, tstart, tend, B_DMPA=b_dmpa, TIME=t_fgm
;	b_dmpa = b_dmpa[0:2,*]
	
	;Get current density from FPI
	fpi_mode = mode eq 'srvy' ? 'fast' : mode
	if fpi_mode eq 'brst' then begin
		;Read DES
		mms_fpi_l1b_moms_read, sc, 'des-moms', tstart, tend, $
		                       N     = n_fpi, $
		                       V_GSE = v_gse, $
		                       P_GSE = P_gse, $
		                       T_GSE = T_gse, $
		                       TIME  = tt2000
		
		;Read DIS
;		mms_fpi_l1b_moms_read, 'mms1', 'dis-moms', tstart, tend, J=je1, TIME=t1_fpi

	endif else begin
		;Srvy data
		mms_fpi_sitl_read, 'mms1', fpi_mode, tstart, tend, J_TOTAL=j1_total, TIME=t1_fpi
	endelse

;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	t_ssm  = MrCDF_epoch2ssm(temporary(tt2000))

	;MMS Colors
	colors = mms_color(['blue', 'green', 'red', 'black'])

	;Create the window
	win = MrWindow(OXMARGIN=[12,7], YSIZE=600, YGAP=0.5, REFRESH=0)
	
	;Density
	p1_n = MrPlot(t_ssm, n_fpi, $
	              /CURRENT, $
	              NAME        = 'n', $
	              TITLE       = 'FPI Moments', $
	              XTICKFORMAT = '(a1)', $
	              XTITLE      = '', $
	              YTITLE      = 'n!C(cm$\up-3$)')
	
	;Velocity
	p2_v = MrPlot( t_ssm, v_gse, $
	               /CURRENT, $
	               COLOR       = colors[0:2], $
	               DIMENSION   = 2, $
	               NAME        = 'V FPI', $
	               XTICKFORMAT = '(a1)', $
	               XTITLE      = '', $
	               YTITLE      = 'V!C(km/s)')
	l_v = MrLegend( ALIGNMENT    = 'NW', $
	                LABEL        = ['X', 'Y', 'Z'], $
	                NAME         = 'Leg: V', $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH = 0, $
	                TARGET       = p2_v, $
	                TEXT_COLOR   = colors[0:2] )
	
	;Pressure
	p3_P = MrPlot( t_ssm, P_gse, $
	               /CURRENT, $
	               DIMENSION   = 2, $
	               NAME        = 'P', $
	               XTICKFORMAT = '(a1)', $
	               XTITLE      = '', $
	               YTITLE      = 'P!C(erg/m^3)')
	l_P = MrLegend( ALIGNMENT    = 'NW', $
	                LABEL        = ['P$\downXX$', 'P$\downXY$', 'P$\downXZ$', $
	                                'P$\downYY$', 'P$\downYZ$', 'P$\downZZ$'], $
	                NAME         = 'Leg: P', $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH = 0, $
	                TARGET       = p3_P, $
	                TEXT_COLOR   = p3_P.color )
	
	;Temperature
	p4_T = MrPlot( t_ssm, T_gse, $
	                /CURRENT, $
	                DIMENSION   = 2, $
	                NAME        = 'T', $
	                XTICKFORMAT = 'time_labels', $
	                YTITLE      = 'T!C(K)')
	l_T = MrLegend( ALIGNMENT    = 'NW', $
	                LABEL        = ['T$\downXX$', 'T$\downXY$', 'T$\downXZ$', $
	                                'T$\downYY$', 'T$\downYZ$', 'T$\downZZ$'], $
	                NAME         = 'Leg: T', $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH = 0, $
	                TARGET       = p4_T, $
	                TEXT_COLOR   = p4_T.color )

	win -> Refresh

	return, win
end