; docformat = 'rst'
;
; NAME:
;       mms_maxwells_eqns
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
;         contributors may  be used to endorse or promote products derived from this     ;
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
;   Calculate all terms in the Generalized Ohm's Law.
;
; :Categories:
;   MMS, FPI
;
; :Params:
;       FILES:              in, required, type=string/strarr
;                           Name(s) of the AFG or DFG file(s) to be read.
;       MODE:               in, optional, type=string, default='srvy'
;                           Data rate mode. Choices are 'srvy' and 'brst'.
;       TSTART:             in, optional, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, optional, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       E_C:                out, optional, type=3xN fltarr
;                           Electric field due to ion convection. Computed as::
;                               \vec{E} = -\vec{U}_{i} \time \vec{B}
;       E_HALL:             out, optional, type=3xN fltarr
;                           Hall electric field. Computed as::
;                               \vec{E} = \frac{1} {n e} \vec{J}_{tot} \time \vec{B}
;       E_DIVPE:            out, optional, type=3xN fltarr
;                           Electric field due to electron pressure divergence. Computed as::
;                               \vec{E} = \frac{1} {n e} \nabla \time \vec{P}_{e}
;       E_DJDT:             out, optional, type=3xN fltarr
;                           Electric field due electron inertia. Computed as::
;                               \vec{E} = \frac{m_{e}} {n e^{2}} \frac{\partial J_{e}} {\partial t}
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
;       2015/11/13  -   Written by Matthew Argall
;-
function mms_fig_ohms_law, sc, mode, tstart, tend, $
E_C=E_C, $
E_HALL=E_Hall, $
E_DIVPE=E_divPe, $
E_DJDT=E_dJdt
	compile_opt idl2
;	on_error, 2
	
	;Constants
	me = constants('m_e')
	q  = constants('q')
	
	;Which quantities to caluclate?
	get_ec   = arg_present(E_C)
	get_eh   = arg_present(E_Hall)
	get_divp = arg_present(E_divPe)
	get_djdt = arg_present(E_dJdt)
	
;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Total magnetic field
;	mms_fgm_read_ql, sc, 'dfg', mode, tstart, tend, B_DMPA=B, TIME=t_fgm

	;Total electric field
	mms_edp_ql_read, sc, mode, tstart, tend, E_DSL=E, TIME=t_edp
	
	;Density and current density
;	mms_fpi_sitl_read, 'mms1', 'fast', tstart, tend, $
;	                   N_E     = n_e, $
;	                   N_I     = n_i, $
;	                   J_TOTAL = J_total, $
;	                   TIME    = t_fpi
	
	;Contributions to Ohm's Law
	mms_ohms_law, sc, mode, tstart, tend, $
	              E_HALL  = E_Hall, $
	              E_C     = E_C, $
	              E_DIVPE = E_divPe, $
	              E_DJDT  = E_dJdt, $
	              TIME    = t_ohm
	
;-----------------------------------------------------
; Derived Products \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Charge density
;	q   = constants('q')
;	rho = q*n_i - q*n_e
	
	;dB/dT
;	npts  = n_elements(t_fgm_ssm)
;	dB_dt = (B[*,1:*] - B) / rebin(reform(t_fgm_ssm[1:*] - t_fgm_ssm, 1, npts), 3, npts)
	
	;e0 * dE/dt
;	npts  = n_elements(t_edp_ssm)
;	dE_dt = (E[*,1:*] - E) / rebin(reform(t_edp_ssm[1:*] - t_edp_ssm, 1, npts), 3, npts)
	
;-----------------------------------------------------
; Create Plots \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert to seconds
	t0        = t_fgm[0]
;	t_fgm_ssm = MrCDF_epoch2ssm(temporary(t_fgm), t0)
	t_edp_ssm = MrCDF_epoch2ssm(temporary(t_edp), t0)
;	t_fpi_ssm = MrCDF_epoch2ssm(temporary(t_fpi), t0)
	t_ohm_ssm = MrCDF_epoch2ssm(temporary(t_ohm), t0)

	;MMS Colors
	colors = mms_color(['blue', 'green', 'red', 'black'])

	;Create the window
	win = MrWindow(YSIZE=600, YGAP=0.5, REFRESH=0)

	;EX
	p1_Ex = MrPlot( t_edp_ssm, E[0,*], $
	                /CURRENT, $
	                COLOR       = colors[3], $
	                NAME        = 'Ex', $
	                TITLE       = "Generalized Ohm's Law", $
	                XTICKFORMAT = '(a1)', $
	                XTITLE      = '', $
	                YTITLE      = 'Ex!C(mV/m)')
	p2_Ex = MrPlot( t_ohm_ssm, E_C[0,*], $
	                COLOR       = colors[2], $
	                NAME        = 'Ex Convective', $
	                OVERPLOT    = p1_Ex)
	p3_Ex = MrPlot( t_ohm_ssm, E_Hall[0,*], $
	                COLOR       = colors[1], $
	                NAME        = 'Ex Hall', $
	                OVERPLOT    = p1_Ex)
	p4_Ex = MrPlot( t_ohm_ssm, E_divPe[0,*], $
	                COLOR       = colors[0], $
	                NAME        = 'Ex DivPe', $
	                OVERPLOT    = p1_Ex)
	p5_Ex = MrPlot( t_ohm_ssm, E_dJdt[0,*], $
	                COLOR       = 'magenta', $
	                NAME        = 'Ex DivPe', $
	                OVERPLOT    = p1_Ex)
	l_EX = MrLegend( ALIGNMENT    = 'NE', $
	                 /AUTO_TEXT_COLOR, $
	                 LABEL        = ['E', 'E$\downH$', 'E$\downC$', 'E$\downdivPe$', 'E\downInert$'], $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p1_Ex, p2_Ex, p3_Ex, p4_Ex, p5_Ex] )

	;EX
	p1_Ey = MrPlot( t_edp_ssm, E[1,*], $
	                /CURRENT, $
	                COLOR       = colors[3], $
	                NAME        = 'Ey', $
	                TITLE       = "Generalized Ohm's Law", $
	                XTICKFORMAT = '(a1)', $
	                XTITLE      = '', $
	                YTITLE      = 'Ey!C(mV/m)')
	p2_Ey = MrPlot( t_ohm_ssm, E_C[1,*], $
	                COLOR       = colors[2], $
	                NAME        = 'Ey Convective', $
	                OVERPLOT    = p1_Ey)
	p3_Ey = MrPlot( t_ohm_ssm, E_Hall[1,*], $
	                COLOR       = colors[1], $
	                NAME        = 'Ey Hall', $
	                OVERPLOT    = p1_Ey)
	p4_Ey = MrPlot( t_ohm_ssm, E_divPe[1,*], $
	                COLOR       = colors[0], $
	                NAME        = 'Ey DivPe', $
	                OVERPLOT    = p1_Ey)
	p5_Ey = MrPlot( t_ohm_ssm, E_dJdt[1,*], $
	                COLOR       = 'magenta', $
	                NAME        = 'Ey DivPe', $
	                OVERPLOT    = p1_Ey)
	l_Ey = MrLegend( ALIGNMENT    = 'NE', $
	                 /AUTO_TEXT_COLOR, $
	                 LABEL        = ['E', 'E$\downH$', 'E$\downC$', 'E$\downdivPe$', 'E\downInert$'], $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p1_Ey, p2_Ey, p3_Ey, p4_Ey, p5_Ey] )

	;EX
	p1_Ez = MrPlot( t_edp_ssm, E[2,*], $
	                /CURRENT, $
	                COLOR       = colors[3], $
	                NAME        = 'Ez', $
	                TITLE       = "Generalized Ohm's Law", $
	                XTICKFORMAT = '(a1)', $
	                XTITLE      = '', $
	                YTITLE      = 'Ez!C(mV/m)')
	p2_Ez = MrPlot( t_ohm_ssm, E_C[2,*], $
	                COLOR       = colors[2], $
	                NAME        = 'Ez Convective', $
	                OVERPLOT    = p1_Ez)
	p3_Ez = MrPlot( t_ohm_ssm, E_Hall[2,*], $
	                COLOR       = colors[1], $
	                NAME        = 'Ez Hall', $
	                OVERPLOT    = p1_Ez)
	p4_Ez = MrPlot( t_ohm_ssm, E_divPe[2,*], $
	                COLOR       = colors[0], $
	                NAME        = 'Ez DivPe', $
	                OVERPLOT    = p1_Ez)
	p5_Ez = MrPlot( t_ohm_ssm, E_dJdt[2,*], $
	                COLOR       = 'magenta', $
	                NAME        = 'Ez DivPe', $
	                OVERPLOT    = p1_Ez)
	l_Ez = MrLegend( ALIGNMENT    = 'NE', $
	                 /AUTO_TEXT_COLOR, $
	                 LABEL        = ['E', 'E$\downH$', 'E$\downC$', 'E$\downdivPe$', 'E\downInerg$'], $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p1_Ez, p2_Ez, p3_Ez, p4_Ez, p5_Ez] )
	
	win -> Refresh
	return, win
end
