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
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;FGM QL SRVY file
	f1_fgm = mms_find_file('mms1', 'dfg', 'srvy', 'ql', $
	                       COUNT     = nfast_edi, $
	                       OPTDESC   = optdesc, $
	                       SDC_ROOT  = sdc_dir, $
	                       SEARCHSTR = searchstr, $
	                       TSTART    = tstart, $
	                       TEND      = tend)
	
	;FGM QL SRVY file
	f2_fgm = mms_find_file('mms2', 'dfg', 'srvy', 'ql', $
	                       COUNT     = nfast_edi, $
	                       OPTDESC   = optdesc, $
	                       SDC_ROOT  = sdc_dir, $
	                       SEARCHSTR = searchstr, $
	                       TSTART    = tstart, $
	                       TEND      = tend)
	
	;FGM QL SRVY file
	f3_fgm = mms_find_file('mms3', 'dfg', 'srvy', 'ql', $
	                       COUNT     = nfast_edi, $
	                       OPTDESC   = optdesc, $
	                       SDC_ROOT  = sdc_dir, $
	                       SEARCHSTR = searchstr, $
	                       TSTART    = tstart, $
	                       TEND      = tend)
	
	;FGM QL SRVY file
	f4_fgm = mms_find_file('mms4', 'dfg', 'srvy', 'ql', $
	                       COUNT     = nfast_edi, $
	                       OPTDESC   = optdesc, $
	                       SDC_ROOT  = sdc_dir, $
	                       SEARCHSTR = searchstr, $
	                       TSTART    = tstart, $
	                       TEND      = tend)

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Magnetic field
	mms_fgm_ql_read, f1_fgm, tstart, tend, B_DMPA=b1_dmpa, TIME=t1_fgm
	mms_fgm_ql_read, f2_fgm, tstart, tend, B_DMPA=b2_dmpa, TIME=t2_fgm
	mms_fgm_ql_read, f3_fgm, tstart, tend, B_DMPA=b3_dmpa, TIME=t3_fgm
	mms_fgm_ql_read, f4_fgm, tstart, tend, B_DMPA=b4_dmpa, TIME=t4_fgm
	
	;Remove the magntidue
	b1_dmpa = transpose(b1_dmpa[0:2,*])
	b2_dmpa = transpose(b2_dmpa[0:2,*])
	b3_dmpa = transpose(b3_dmpa[0:2,*])
	b4_dmpa = transpose(b4_dmpa[0:2,*])
	
	;FDOA Spacecraft Position
	r1_dmpa = mms_fdoa_scpos('mms1', tstart, tend, t1_fgm)
	r2_dmpa = mms_fdoa_scpos('mms2', tstart, tend, t1_fgm)
	r3_dmpa = mms_fdoa_scpos('mms3', tstart, tend, t1_fgm)
	r4_dmpa = mms_fdoa_scpos('mms4', tstart, tend, t1_fgm)
	r1_dmpa = transpose(r1_dmpa)
	r2_dmpa = transpose(r2_dmpa)
	r3_dmpa = transpose(r3_dmpa)
	r4_dmpa = transpose(r4_dmpa)

	;Convert time to seconds
	MrCDF_Epoch_Breakdown, t1_fgm[0], year, month, day
	MrCDF_Epoch_Compute, t0, year, month, day, /TT2000
	t1_fgm_sse = MrCDF_epoch2sse(t1_fgm, t0)
	t2_fgm_sse = MrCDF_epoch2sse(t2_fgm, t0)
	t3_fgm_sse = MrCDF_epoch2sse(t3_fgm, t0)
	t4_fgm_sse = MrCDF_epoch2sse(t4_fgm, t0)
	
	;interpolate B-fields
	b2_dmpa = MrInterpol(b2_dmpa, t2_fgm_sse, t1_fgm_sse)
	b3_dmpa = MrInterpol(b3_dmpa, t3_fgm_sse, t1_fgm_sse)
	b4_dmpa = MrInterpol(b4_dmpa, t4_fgm_sse, t1_fgm_sse)
	
	;Curlometer
	Jcurl = MrReciprocalCurl(r1_dmpa, r2_dmpa, r3_dmpa, r4_dmpa, b1_dmpa, b2_dmpa, b3_dmpa, b4_dmpa)
	Jcmtr = MrCurlometer(r1_dmpa, r2_dmpa, r3_dmpa, r4_dmpa, b1_dmpa, b2_dmpa, b3_dmpa, b4_dmpa)

	;Correct units
	;   - Micro-Amps
	;   - (s^2 A^2 / kg m) * nT / m --> (s^2 A^2 / kg m) (1e-9 kg / A s^2) / km
	;                               --> A/m^2 * 1e-9
	;                               --> uA/m^2 * 1e-9 * 1e6
	mu0 = constants('mu_0')
	Jcurl *= (1e-6/mu0)

;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;MMS Colors
	colors = mms_color(['blue', 'green', 'red', 'black'])

	;Create the window
	win = MrWindow(YSIZE=600, YGAP=0.5, REFRESH=0)
	
	;BX
	p1x_fgm = MrPlot( t1_fgm_sse, B1_dmpa[*,0], $
	                  /CURRENT, $
	                  COLOR       = 'black', $
	                  NAME        = 'Bx FGM1', $
	                  TITLE       = 'FGM', $
	                  XTICKFORMAT = '(a1)', $
	                  XTITLE      = '', $
	                  YTITLE      = 'Bx!C(nT)')
	p2x_fgm = MrPlot( t1_fgm_sse, B2_dmpa[*,0], $
	                  /CURRENT, $
	                  COLOR       = 'red', $
	                  NAME        = 'Bx FGM2', $
	                  OVERPLOT    = p1x_fgm)
	p3x_fgm = MrPlot( t1_fgm_sse, B3_dmpa[*,0], $
	                  /CURRENT, $
	                  COLOR       = 'forest green', $
	                  NAME        = 'Bx FGM3', $
	                  OVERPLOT    = p1x_fgm)
	p4x_fgm = MrPlot( t1_fgm_sse, B4_dmpa[*,0], $
	                  /CURRENT, $
	                  COLOR       = 'blue', $
	                  NAME        = 'Bx FGM4', $
	                  OVERPLOT    = p1x_fgm)
	l_fgm = MrLegend( ALIGNMENT    = 'NE', $
	                  /AUTO_TEXT_COLOR, $
	                  LABEL        = ['MMS1', 'MMS2', 'MMS3', 'MMS4'], $
	                  POSITION     = [1.0, 1.0], $
	                  /RELATIVE, $
	                  SAMPLE_WIDTH = 0, $
	                  TARGET       = p1x_fgm )
	
	;BY
	p1y_fgm = MrPlot( t1_fgm_sse, B1_dmpa[*,1], $
	                  /CURRENT, $
	                  COLOR       = 'black', $
	                  NAME        = 'By FGM1', $
	                  XTICKFORMAT = '(a1)', $
	                  XTITLE      = '', $
	                  YTITLE      = 'By!C(nT)')
	p2y_fgm = MrPlot( t1_fgm_sse, B2_dmpa[*,1], $
	                  /CURRENT, $
	                  COLOR       = 'red', $
	                  NAME        = 'By FGM2', $
	                  OVERPLOT    = p1y_fgm)
	p3y_fgm = MrPlot( t1_fgm_sse, B3_dmpa[*,1], $
	                  /CURRENT, $
	                  COLOR       = 'forest green', $
	                  NAME        = 'By FGM3', $
	                  OVERPLOT    = p1y_fgm)
	p4y_fgm = MrPlot( t1_fgm_sse, B4_dmpa[*,1], $
	                  /CURRENT, $
	                  COLOR       = 'blue', $
	                  NAME        = 'By FGM4', $
	                  OVERPLOT    = p1y_fgm)
	
	;BZ
	p1z_fgm = MrPlot( t1_fgm_sse, B1_dmpa[*,2], $
	                  /CURRENT, $
	                  COLOR       = 'black', $
	                  NAME        = 'Bz FGM1', $
	                  XTICKFORMAT = '(a1)', $
	                  XTITLE      = '', $
	                  YTITLE      = 'Bz!C(nT)')
	p2z_fgm = MrPlot( t1_fgm_sse, B2_dmpa[*,2], $
	                  /CURRENT, $
	                  COLOR       = 'red', $
	                  NAME        = 'Bz FGM2', $
	                  OVERPLOT    = p1z_fgm)
	p3z_fgm = MrPlot( t1_fgm_sse, B3_dmpa[*,2], $
	                  /CURRENT, $
	                  COLOR       = 'forest green', $
	                  NAME        = 'Bz FGM3', $
	                  OVERPLOT    = p1z_fgm)
	p4z_fgm = MrPlot( t1_fgm_sse, B4_dmpa[*,2], $
	                  /CURRENT, $
	                  COLOR       = 'blue', $
	                  NAME        = 'Bz FGM4', $
	                  OVERPLOT    = p1z_fgm)
	
	;J CURL
	p_curl = MrPlot( t1_fgm_sse, Jcurl, $
	                  /CURRENT, $
;	                  COLOR       = 'black', $
	                  DIMENSION   = 1, $
	                  NAME        = 'J ReciprocalVectors', $
	                  XTICKFORMAT = '(a1)', $
	                  YTITLE      = 'J!C($\mu$A/m$\up2$)')
	
	;J CURLOMETER
	p_cmtr = MrPlot( t1_fgm_sse, Jcmtr, $
	                  /CURRENT, $
;	                  COLOR       = 'black', $
	                  DIMENSION   = 2, $
	                  NAME        = 'J Curlometer', $
	                  XTICKFORMAT = 'time_labels', $
	                  XTITLE      = 'Time (UTC)', $
	                  YTITLE      = 'J!C($\mu$A/m$\up2$)')

	win -> Refresh
stop
	return, win
end