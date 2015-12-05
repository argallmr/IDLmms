; docformat = 'rst'
;
; NAME:
;       mms_fig_vdrift.pro
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
;   Create a plot of FIELDS data
;       1) DFG Magnetic Field
;       2) EDP Electric Field
;       3) Vx from ExB and EDI
;       4) Vx from ExB and EDI
;       5) Vx from ExB and EDI
;       6) Q0 Counts from GDU1 & GDU2
;       7) EDP Spacecraft Potential
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
function mms_test_EVxB, sc, tstart, tend, $
EIGVECS=eigvecs
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	sc     = 'mms3'
	tstart = '2015-08-15T13:20:00Z'
	tend   = '2015-08-15T13:30:00Z'

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Electric Field
	mms_edp_ql_read, sc, 'srvy', tstart, tend, $
	                 E_DSL = E_dsl, $
	                 TIME  = t_edp
	
	;Magnetic field
	mms_fgm_ql_read, sc, 'dfg', 'srvy', 'l2pre', tstart, tend, $
	                 B_DMPA = b_dmpa, $
	                 B_GSE  = b_gse, $
	                 TIME   = t_fgm
	
	;Electron velocity
	mms_fpi_sitl_read, sc, 'fast', tstart, tend, $
	                   VE_DSC = v_dsc, $
	                   TIME   = t_fpi


	b_dmpa = b_dmpa[0:2,*]
	b_gse  = b_gse[0:2,*]
;-----------------------------------------------------
; Interpolate \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert time to seconds
	t0        = t_fgm[0]
	t_edp_ssm = MrCDF_epoch2ssm(t_edp, t0)
	t_fgm_ssm = MrCDF_epoch2ssm(t_fgm, t0)
	t_fpi_ssm = MrCDF_epoch2ssm(t_fpi, t0)

	;Interpolate to FGM times
	E_dsl = MrInterpol(E_dsl, t_edp_ssm, t_fgm_ssm)
	v_dsc = MrInterpol(v_dsc, t_fpi_ssm, t_fgm_ssm)

;-----------------------------------------------------
; Calculate ExB Drift Velocity \\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Compute the ExB drift
	;   - 1e-6 converts km/s nT  --> V/m
	;   - 1e3  converts V/m --> mV/m
	E_VxB = -1e-3 * MrVector_Cross(v_dsc, b_dmpa)
	
;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create the window
	win   = MrWindow(YGAP=0.5, REFRESH=0)
	
	;Ex
	px_edp = MrPlot( t_fgm_ssm, E_dsl[0,*], $
	                 /CURRENT, $
	                 NAME        = 'Ex EDP', $
	                 TITLE       = 'EDP vs. VxB', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'Ex!C(mV/m)')
	px_vxb = MrPlot( t_fgm_ssm, E_VxB[0,*], $
	                 COLOR       = 'Blue', $
	                 NAME        = 'Ex VxB', $
	                 OVERPLOT    = px_edp)
	l_fgm = MrLegend( ALIGNMENT    = 'NE', $
	                  LABEL        = ['E', '-VxB'], $
	                  POSITION     = [1.0, 1.0], $
	                  /RELATIVE, $
	                  TEXT_COLOR   = ['black', 'blue'], $
	                  SAMPLE_WIDTH = 0, $
	                  TARGET       = [px_edp, px_vxb] )
	
	;Ey
	py_edp = MrPlot( t_fgm_ssm, E_dsl[1,*], $
	                 /CURRENT, $
	                 NAME        = 'Ey EDP', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'Ey!C(mV/m)')
	py_vxb = MrPlot( t_fgm_ssm, E_VxB[1,*], $
	                 COLOR       = 'Blue', $
	                 NAME        = 'Ey VxB', $
	                 OVERPLOT    = py_edp)
	
	;Ez
	pz_edp = MrPlot( t_fgm_ssm, E_dsl[2,*], $
	                 /CURRENT, $
	                 NAME        = 'Ez EDP', $
	                 XTICKFORMAT = 'time_labels', $
	                 XTITLE      = '', $
	                 YTITLE      = 'Ez!C(mV/m)')
	pz_vxb = MrPlot( t_fgm_ssm, E_VxB[2,*], $
	                 COLOR       = 'Blue', $
	                 NAME        = 'Ez VxB', $
	                 OVERPLOT    = pz_edp)
	
	win -> Refresh
	return, win
end