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
;       2) Ex from EDP & EDI
;       3) Ey from EDP & EDI
;       4) Ez from EDP & EDI
;       5) Q0 Counts from GDU1 & GDU1
;       6) EDP Spacecraft Potential
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
function mms_fig_efield, sc, tstart, tend, $
BC=bc, $
EIGVECS=eigvecs
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg(/QUIET)
		return, !Null
	endif
	
	bc = keyword_set(bc)

;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;FGM QL SRVY file
	files_fgm = mms_find_file(sc, 'dfg', 'srvy', 'ql', $
	                          COUNT     = nfast_edi, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	
	;EDP QL FAST/SLOW file
	files_edp = mms_find_file(sc, 'edp', ['fast', 'slow'], 'ql', $
	                          COUNT     = nfast_edi, $
	                          OPTDESC   = 'dce', $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TIMEORDER = '%Y%M%d%H%m%S', $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	
	;EDP L2 Spacecraft potential
	files_scpot = mms_find_file(sc, 'edp', ['slow', 'fast'], 'l2', $
	                            COUNT     = nfast_edi, $
	                            OPTDESC   = 'scpot', $
	                            SDC_ROOT  = sdc_dir, $
	                            SEARCHSTR = searchstr, $
	                            TIMEORDER = '%Y%M%d%H%m%S', $
	                            TSTART    = tstart, $
	                            TEND      = tend)
	
	;EDI QL SRVY E-field file
	files_edi = mms_find_file(sc, 'edi', 'srvy', 'sl', $
	                          COUNT     = nfast_edi, $
	                          DIRECTORY = '/nfs/edi/sl/', $
	                          OPTDESC   = 'efield', $
;	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	
	;EDI Q0 Counts
	files_q0 = mms_find_file(sc, 'edi', 'srvy', 'l2', $
	                         COUNT     = nfast_edi, $
	                         DIRECTORY = '/nfs/edi/q0/', $
	                         OPTDESC   = 'q0', $
;	                         SDC_ROOT  = sdc_dir, $
	                         SEARCHSTR = searchstr, $
	                         TSTART    = tstart, $
	                         TEND      = tend)

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	mms_fgm_ql_read, files_fgm, tstart, tend, $
	                 B_DMPA = b_dmpa, $
	                 TIME   = t_fgm
	
	mms_edp_ql_read, files_edp, tstart, tend, $
	                 E_DSL = E_dsl, $
	                 TIME  = t_edp
	
	mms_edp_l2_scpot_read, files_scpot, tstart, tend, $
	                       SCPOT = scpot, $
	                       TIME  = t_scpot

	mms_edi_sl_efield_read, files_edi, tstart, tend, $
	                        B_DMPA    = B_edi_dmpa, $
	                        E_DMPA    = E_edi_dmpa, $
	                        E_BC_DMPA = E_bc_dmpa, $
	                        TIME      = t_edi
	                       
	mms_edi_l2_q0_read, files_q0, tstart, tend, $
	                    COUNTS_GD12 = counts_gd12, $
	                    COUNTS_GD21 = counts_gd21, $
	                    TIME_GD12   = t_q0_gd12, $
	                    TIME_GD21   = t_q0_gd21

	;Beam convergence or bestarg
	if bc $
		then E_edi = E_bc_dmpa $
		else E_edi = E_edi_dmpa
	E_bc_dmpa  = !Null
	E_edi_dmpa = !Null

;-----------------------------------------------------
; Median Smooth Q0 Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	counts_gd12 = median(reform(counts_gd12), 5)
	counts_gd21 = median(reform(counts_gd21), 5)
	
;-----------------------------------------------------
; Interpolate EDP data to 5 Second Intervals \\\\\\\\\
;-----------------------------------------------------
	;Breakdown the first time
	MrCDF_Epoch_Breakdown, t_edp[0], year, month, day, hour, minute, second
	
	;Round down to the nearest 5 seconds and recompute
	second -= (second mod 5.0)
	MrCDF_Epoch_Compute, t0, year, month, day, hour, minute, second, /TT2000
	
	;Create a time array at 5-second intervals from T0 to the last time
	t_avg_edp = MrMake_Array(START=t0, INCREMENT=5000000000LL, LAST=t_edp[-1], /LONG64)
	E_avg_dsl = MrInterpol(E_dsl, t_edp, t_avg_edp)

;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to seconds
	t0            = t_fgm[0]
	t_fgm_ssm     = MrCDF_epoch2ssm(t_fgm,     t0)
	t_edp_ssm     = MrCDF_epoch2ssm(t_avg_edp, t0)
	t_scpot_ssm   = MrCDF_epoch2ssm(t_scpot,   t0)
	t_edi_ssm     = MrCDF_epoch2ssm(t_edi,     t0)
	t_q0_gd12_ssm = MrCDF_epoch2ssm(t_q0_gd12, t0)
	t_q0_gd21_ssm = MrCDF_epoch2ssm(t_q0_gd21, t0)

	;MMS Colors
	mms_color = mms_color(['blue', 'green', 'red', 'black'])
	
	xrange = [t_fgm_ssm[0], t_fgm_ssm[-1]]

	;Create the window
	win   = MrWindow(YGAP=0.5, REFRESH=0)
	
	;DFG
	p_bedi = MrPlot( t_edi_ssm, B_edi_dmpa, $
	                 /CURRENT, $
	                 COLOR       = ['sky blue', 'spring green', 'pink'], $
	                 DIMENSION   = 2, $
	                 NAME        = 'B EDI', $
	                 PSYM        = 1, $
	                 TITLE       = sc + 'FIELDS', $
	                 XRANGE      = xrange, $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'B!C(nT)')
	p_fgm = MrPlot( t_fgm_ssm, B_dmpa, $
	                /CURRENT, $
	                COLOR       = ['blue', 'forest green', 'red', 'black'], $
	                DIMENSION   = 2, $
	                NAME        = 'B FGM', $
	                OVERPLOT    = p_bedi, $
	                TITLE       = 'FGM', $
	                XRANGE      = xrange, $
	                XTICKFORMAT = '(a1)', $
	                XTITLE      = '', $
	                YTITLE      = 'B!C(nT)')
	l_fgm = MrLegend( ALIGNMENT    = 'NE', $
	                  LABEL        = ['|B|', 'B$\downX$', 'B$\downY$', 'B$\downZ$'], $
	                  POSITION     = [1.0, 1.0], $
	                  /RELATIVE, $
	                  TEXT_COLOR   = ['black', 'blue', 'forest green', 'red'], $
	                  SAMPLE_WIDTH = 0, $
	                  TARGET       = p_bedi )
	
	;Ex -- EDP & EDI
	p_edpx = MrPlot( t_edp_ssm, E_avg_dsl[0,*], $
	                 /CURRENT, $
	                 NAME        = 'Ex EDP', $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XRANGE      = xrange, $
	                 XTITLE      = '', $
	                 YTITLE      = 'Ex!C(mV/m)')
	p_edix = MrPlot( t_edi_ssm, E_edi[0,*], $
	                 OVERPLOT    = p_edpx, $
	                 COLOR       = 'blue', $
	                 NAME        = 'Ex EDI', $
	                 TITLE       = '', $
	                 XRANGE      = xrange, $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'Ex!C(mV/m)')
	l_ex = MrLegend( ALIGNMENT    = 'NE', $
	                 LABEL        = ['EDP', 'EDI'], $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 TEXT_COLOR   = ['black', 'blue'], $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p_edpx, p_edix] )
	
	;Ey -- EDP & EDI
	p_edpy = MrPlot( t_edp_ssm, E_avg_dsl[1,*], $
	                 /CURRENT, $
	                 NAME        = 'Ey EDP', $
	                 TITLE       = '', $
	                 XRANGE      = xrange, $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'Ey!C(mV/m)')
	p_ediy = MrPlot( t_edi_ssm, E_edi[1,*], $
	                 OVERPLOT    = p_edpy, $
	                 COLOR       = 'blue', $
	                 NAME        = 'Ey EDI', $
	                 TITLE       = '', $
	                 XRANGE      = xrange, $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'Ey!C(mV/m)')
	l_ey = MrLegend( ALIGNMENT    = 'NE', $
	                 LABEL        = ['EDP', 'EDI'], $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 TEXT_COLOR   = ['black', 'blue'], $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p_edpy, p_ediy] )
	
	;Ez -- EDP & EDI
	p_edpz = MrPlot( t_edp_ssm, E_avg_dsl[2,*], $
	                 /CURRENT, $
	                 NAME        = 'Ez EDP', $
	                 TITLE       = '', $
	                 XRANGE      = xrange, $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'Ez!C(mV/m)')
	p_ediz = MrPlot( t_edi_ssm, E_edi[2,*], $
	                 OVERPLOT    = p_edpz, $
	                 COLOR       = 'blue', $
	                 NAME        = 'Ez EDI', $
	                 TITLE       = '', $
	                 XRANGE      = xrange, $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'Ez!C(mV/m)')
	l_ez = MrLegend( ALIGNMENT    = 'NE', $
	                 LABEL        = ['EDP', 'EDI'], $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 TEXT_COLOR   = ['black', 'blue'], $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p_edpz, p_ediz] )
	
	;Q0 GDU1 & GDU2
	p_gdu1_q0 = MrPlot( t_q0_gd12_ssm, counts_gd12, $
	                    /CURRENT, $
	                    NAME        = 'Q0 Counts GDU1', $
	                    PSYM        = 3, $
	                    SYMSIZE     = 3, $
	                    TITLE       = '', $
	                    XRANGE      = xrange, $
	                    XTICKFORMAT = '(a1)', $
	                    XTITLE      = '', $
	                    YRANGE      = [1, max(counts_gd12)], $
	                    YTITLE      = 'Q0!C(Counts)')
	p_gdu2_q0 = MrPlot( t_q0_gd21_ssm, counts_gd21, $
	                    COLOR       = 'Blue', $
	                    NAME        = 'Q0 Counts GDU2', $
	                    OVERPLOT    = p_gdu1_q0, $
	                    PSYM        = 3, $
	                    SYMSIZE     = 3, $
	                    TITLE       = '', $
	                    XRANGE      = xrange, $
	                    XTICKFORMAT = '(a1)', $
	                    XTITLE      = '', $
	                    YRANGE      = [1, max(counts_gd21)], $
	                    YTITLE      = 'Q0!C(Counts)')
	l_q0 = MrLegend( ALIGNMENT    = 'NE', $
	                 LABEL        = ['GDU1', 'GDU2'], $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 TEXT_COLOR   = ['black', 'blue'], $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p_gdu1_q0, p_gdu2_q0] )
	
	;SCPOT
	p_scpot = MrPlot( t_scpot_ssm, -scpot, $
	                  /CURRENT, $
	                  NAME        = 'V EDP', $
	                  TITLE       = '', $
	                  XRANGE      = xrange, $
	                  XTICKFORMAT = 'time_labels', $
	                  XTITLE      = 'Time (UT)', $
	                  YTITLE      = '-SCPot!C(V)')
	
	win -> Refresh
	return, win
end