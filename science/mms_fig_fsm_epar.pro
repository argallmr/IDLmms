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
function mms_fig_fsm_epar, sc, mode, tstart, tend, $
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

	sc     = 'mms1'
	mode   = 'brst'
	tstart = '2015-10-16T13:06:40Z'
	tend   = '2015-10-16T13:07:20Z'

;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;EDP and SCM have the same sampling rates

	;EDP Electric Field
	mms_edp_ql_read, sc, mode, tstart, tend, $
	                 E_DSL = E_edp, $
	                 TIME  = t_edp
	
	;FSM Magnetic Field
	fsmopt   = 'fsm-split'
	fsm_mode = mode eq 'brst' ? mode : 'srvy'
	mms_fsm_l2plus_read, sc, fsm_mode, fsmopt, tstart, tend, $
	                     B_DMPA = b_fsm, $
	                     TIME   = t_fsm

;-----------------------------------------------------
; Parallel and Perpendicular \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to seconds
	t0        = t_fsm[0]
	npts      = n_elements(t_edp)
	t_fsm_ssm = MrCDF_epoch2ssm( temporary(t_fsm), t0)
	t_edp_ssm = MrCDF_epoch2ssm( temporary(t_edp), t0)
	
	;Inteperpolate B onto E
	b_fsm = MrInterpol(b_fsm, t_fsm_ssm, t_edp_ssm)
	
	;Parallel and perpendicular directions
	par   = MrVector_Normalize(b_fsm)
	perp1 = MrVector_Normalize( MrVector_Cross([0, 1, 0], B_fsm) )
	perp2 = MrVector_Normalize( MrVector_Cross(par, perp1) )
	
	;Field-aligned coordinate transformation
	T        = fltarr(3, 3, npts)
	T[*,0,*] = par
	T[*,1,*] = perp1
	T[*,2,*] = perp2
	
	;Rotate E
	E_fac = MrVector_Rotate(T, E_edp)
	
	;Parallel and perpendicualr fields
	E_par = reform(E_fac[2,*])
	T_inv = transpose(T, [1,0,2])
	E_fac[2,*] = 0.0
	E_perp     = MrVector_Rotate(T_inv, E_fac)

;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create the window
	win = MrWindow(LAYOUT=[1,4], XSIZE=600, XGAP=0.5, YGAP=0.5, YSIZE=650, REFRESH=0)

	;MMS Colors
	color = mms_color(['blue', 'green', 'red'])

	;B
	gBx = MrPlot( t_edp_ssm, b_fsm[0,*], $
	             /CURRENT, $
	             COLOR       = color[0], $
	             NAME        = 'Bx', $
	             TITLE       = 'Parallel & Perpendicular E-Fields', $
	             XTICKFORMAT = '(a1)', $
	             YTITLE      = 'B!C(nT)')
	gBy = MrPlot( t_edp_ssm, b_fsm[1,*], $
	             COLOR       = color[1], $
	             NAME        = 'By', $
	             OVERPLOT    = gBx)
	gBz = MrPlot( t_edp_ssm, b_fsm[2,*], $
	             COLOR       = color[2], $
	             NAME        = 'Bz', $
	             OVERPLOT    = gBx)
	
	lB = MrLegend( ALIGNMENT = 'NE', $
	               /AUTO_TEXT_COLOR, $
	               POSITION  = [1.0, 1.0], $
	               /RELATIVE, $
	               SAMPLE_WIDTH = 0.0, $
	               LABEL        = ['Bx', 'By', 'Bz'], $
	               TARGET       = [gBx, gBy, gBz])

	;E Parallel
	gEpar = MrPlot( t_edp_ssm, E_par, $
	                /CURRENT, $
	                NAME        = 'E Par', $
	                XTICKFORMAT = '(a1)', $
	                YTITLE      = 'E$\down||$!C(mV/m)')
	
	;E PerpX
	gEperpX = MrPlot( t_edp_ssm, E_perp[0,*], $
	                  /CURRENT, $
	                  NAME        = 'E PerpX', $
	                  XTICKFORMAT = '(a1)', $
	                  YTITLE      = 'E$\downPerpX$!C(mV/m)')
	
	;E PerpY
	gEperpY = MrPlot( t_edp_ssm, E_perp[1,*], $
	                  /CURRENT, $
	                  NAME        = 'E PerpY', $
	                  XTICKFORMAT = '(a1)', $
	                  YTITLE      = 'E$\downPerpY$!C(mV/m)')
	
	;E PerpZ
	gEperpZ = MrPlot( t_edp_ssm, E_perp[2,*], $
	                  /CURRENT, $
	                  NAME        = 'E PerpZ', $
	                  XTICKFORMAT = 'time_labels', $
	                  YTITLE      = 'E$\downPerpZ$!C(mV/m)')

	win -> Refresh
	return, win
end