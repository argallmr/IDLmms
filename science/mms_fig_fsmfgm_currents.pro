; docformat = 'rst'
;
; NAME:
;       mms_fig_fsmfgm_current
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
;       1) Jx from FGM and FSM
;       2) Jy from FGM and FSM
;       3) Jz from FGM and FSM
;       4) div(B) from FGM and FSM
;
; :Params:
;       MODE:               in, required, type=string/strarr
;                           Data rate mode of the files.
;       TSTART:             in, required, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, required, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       EIGVECS:        out, optional, type=3x3 float
;                       Rotation matrix (into the minimum variance coordinate system).
;
; :Returns:
;       WIN:            A MrGraphics window containing plots of current density.
;-
function mms_fig_fsmfgm_currents, mode, tstart, tend, $
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
; Current Density \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Currents
	j_fsm = mms_fsm_curldiv(tstart, tend, mode, DIVB=divb_fsm, TIME=t_fsm)
	j_fgm = mms_fgm_curldiv(tstart, tend, 'dfg', mode, DIVB=divb_fgm, TIME=t_fgm)

;-----------------------------------------------------
; Plot FSM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	t0        = t_fsm[0]
	t_fsm_ssm = MrCDF_epoch2ssm(t_fsm, t0)
	t_fgm_ssm = MrCDF_epoch2ssm(t_fgm, t0)

	yrange = [-2, 2]

	;Create the window
	win = MrWindow(LAYOUT=[1,4], XSIZE=600, XGAP=0.5, YGAP=0.5, YSIZE=650, REFRESH=0)

	;Jx FSM
	px_fsm = MrPlot(t_fsm_ssm, j_fsm[0,*], $
	                /CURRENT, $
	                COLOR       = 'Red', $
	                LAYOUT      = [1,3,1], $
	                NAME        = 'Jx FSM', $
	                TITLE       = 'FSM & DFG Curlometer', $
	                XRANGE      = xrange, $
	                XTICKFORMAT = '(a1)', $
	                YRANGE      = yrange, $
	                YTITLE      = 'Jx!C($\mu$A/m^2)')

	;Jy FSM
	py_fsm = MrPlot(t_fsm_ssm, j_fsm[1,*], $
	                /CURRENT, $
	                COLOR       = 'Red', $
	                LAYOUT      = [1,3,2], $
	                NAME        = 'Jy FSM', $
	                XRANGE      = xrange, $
	                XTICKFORMAT = '(a1)', $
	                YRANGE      = yrange, $
	                YTITLE      = 'Jy!C($\mu$A/m^2)')

	;Jz FSM
	pz_fsm = MrPlot(t_fsm_ssm, j_fsm[2,*], $
	                /CURRENT, $
	                COLOR       = 'Red', $
	                LAYOUT      = [1,3,3], $
	                NAME        = 'Jz FSM', $
	                XRANGE      = xrange, $
	                XTICKFORMAT = '(a1)', $
	                YRANGE      = yrange, $
	                YTITLE      = 'Jz!C($\mu$A/m^2)')

	;DIVB FSM
	pb_fsm = MrPlot(t_fsm_ssm, divb_fsm, $
	                /CURRENT, $
	                COLOR       = 'Red', $
	                LAYOUT      = [1,4,4], $
	                NAME        = 'DivB FSM', $
	                XRANGE      = xrange, $
	                XTICKFORMAT = 'time_labels', $
	                YRANGE      = yrange, $
	                YTITLE      = 'Div(B)!C($\mu$A/m^2)')

;-----------------------------------------------------
; Plot FGM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Bx FGM
	px_fgm = MrPlot(t_fgm_ssm, j_fgm[0,*], $
	                NAME        = 'Jx FGM', $
	                OVERPLOT    = px_fsm)

	;By FGM
	py_fgm = MrPlot(t_fgm_ssm, j_fgm[1,*], $
	                NAME        = 'Jy FGM', $
	                OVERPLOT    = py_fsm)

	;Bz FGM
	pz_fgm = MrPlot(t_fgm_ssm, j_fgm[2,*], $
	                NAME        = 'Jz FGM', $
	                OVERPLOT    = pz_fsm)

	;DIVB FGM
	pb_fgm = MrPlot(t_fgm_ssm, divb_fgm, $
	                NAME     = 'DivB FGM', $
	                OVERPLOT = pb_fsm)

;-----------------------------------------------------
; Create Legend \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Legend
	lb = MrLegend(ALIGNMENT    = 'NE', $
	              POSITION     = [1.0, 1.0], $
	              /RELATIVE, $
	              SAMPLE_WIDTH = 0, $
	              TEXT_COLOR   = ['Red', 'Black'], $
	              LABEL        = ['FSM', 'DFG'], $
	              TARGET       = [px_fsm, px_fgm])

	win -> Refresh
	return, win
end