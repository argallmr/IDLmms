;-
; docformat = 'rst'
;
; NAME:
;       mms_fig_efacpro
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
;   Create a plot of the electric field in a Field-Aligned Coordinate system
;       1) DFG Magnetic Field
;       2) E-perp1 from EDP
;       3) E-perp2 from EDP
;       4) E-par   from EDP & EDI
;       5)   0 degree ambient electron counts from EDI
;       6) 180 degree ambient electron counts from EDI
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           Spacecraft for which data is to be plotted.
;       TSTART:             in, required, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, required, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;-
function mms_fig_fields_fac, sc, tstart, tend
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
	
	;EDI L1A FAST/SLOW Ambient file
	files_edi = mms_find_file(sc, 'edi', ['fast', 'slow'], 'l1a', $
	                          COUNT     = nfast_edi, $
	                          OPTDESC   = 'amb', $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	mms_edi_l1a_amb_read, files_edi, tstart, tend, $
	                      ANISOTROPY = anisotropy, $
	                      GDU_0      = gdu_0, $
	                      GDU_180    = gdu_180, $
	                      T_0        = t_0, $
	                      T_180      = t_180, $
	                      COUNTS_0   = counts_0, $
	                      COUNTS_180 = counts_180
	
	mms_fgm_ql_read, files_fgm, tstart, tend, $
	                 B_DMPA = b_dmpa, $
	                 TIME   = t_fgm
	
	mms_edp_ql_read, files_edp, tstart, tend, $
	                 E_DSL = E_dsl, $
	                 TIME  = t_edp
	
	mms_edp_l2_scpot_read, files_scpot, tstart, tend, $
	                       SCPOT = scpot, $
	                       TIME  = t_scpot

;-----------------------------------------------------
; 0 & 180 Counts \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;0 and 180 Degree Counts
	igdu1_0   = where(gdu_0 eq 1, ngdu1, COMPLEMENT=igdu2_0, NCOMPLEMENT=ngdu2)
	igdu1_180 = where(gdu_0 eq 1, ngdu1, COMPLEMENT=igdu2_180, NCOMPLEMENT=ngdu2)

;-----------------------------------------------------
; Create Field-Aligned Coordinate System \\\\\\\\\\\\\
;-----------------------------------------------------
	;Interpolate mangetic field onto electric field time grid
	B_edp = MrInterpol(B_dmpa[0:2,*], t_fgm, t_edp)
	
	;Transform to FAC
	dmpa2fac = mms_edi_xxyz2bpp(B_edp)
	E_fac    = MrVector_Rotate(dmpa2fac, E_dsl)
	
	;Delete data
	E_dsl = !Null

;-----------------------------------------------------
; Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to seconds
	t0          = t_0[0]
	t_fgm_ssm   = MrCDF_epoch2ssm(t_fgm,   t0)
	t_edp_ssm   = MrCDF_epoch2ssm(t_edp,   t0)
	t_0_ssm     = MrCDF_epoch2ssm(t_0,     t0)
	t_180_ssm   = MrCDF_epoch2ssm(t_180,   t0)
	t_scpot_ssm = MrCDF_epoch2ssm(t_scpot, t0)

	;MMS Colors
	colors = mms_color(['blue', 'green', 'red', 'black'])

	;Create the window
	win   = MrWindow(YGAP=0.5, REFRESH=0)
	
	;DFG
	p_fgm = MrPlot( t_fgm_ssm, B_dmpa, $
	                /CURRENT, $
	                COLOR       = colors, $
	                DIMENSION   = 2, $
	                NAME        = 'B FGM', $
	                TITLE       = 'FGM', $
	                XTICKFORMAT = '(a1)', $
	                XTITLE      = '', $
	                YTITLE      = 'B!C(nT)')
	l_fgm = MrLegend( ALIGNMENT    = 'NE', $
	                  LABEL        = ['|B|', 'B$\downX$', 'B$\downY$', 'B$\downZ$'], $
	                  POSITION     = [1.0, 1.0], $
	                  /RELATIVE, $
	                  SAMPLE_COLOR = ['black', 'blue', 'green', 'red'], $
	                  SAMPLE_WIDTH = 0, $
	                  TARGET       = p_fgm )
	
	;EDP
	p_edp1 = MrPlot( t_edp_ssm, E_fac[0,*], $
	                 /CURRENT, $
	                 DIMENSION   = 2, $
	                 NAME        = 'E-perp1 EDP', $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'E-perp1!C(mV/m)')
	p_edp2 = MrPlot( t_edp_ssm, E_fac[1,*], $
	                 /CURRENT, $
	                 DIMENSION   = 2, $
	                 NAME        = 'E-perp2 EDP', $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'E-perp2!C(mV/m)')
	p_edp3 = MrPlot( t_edp_ssm, E_fac[2,*], $
	                 /CURRENT, $
	                 DIMENSION   = 2, $
	                 NAME        = 'E-par EDP', $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'E-par!C(mV/m)')
	
	;0 Degree Counts
	p_gdu1_0 = MrPlot( t_0_ssm[igdu1_0], counts_0[igdu1_0], $
	                   /CURRENT, $
	                   NAME        = '0 Deg Counts GDU1', $
	                   PSYM        = 3, $
	                   TITLE       = '', $
	                   XTICKFORMAT = '(a1)', $
	                   XTITLE      = '', $
	                   /YLOG, $
	                   YRANGE      = [1, max(counts_0)], $
	                   YTITLE      = '0 Deg PA!C(Counts)')
	p_gdu2_0 = MrPlot( t_0_ssm[igdu2_0], counts_0[igdu2_0], $
	                   COLOR       = 'blue', $
	                   NAME        = '0 Deg Counts GDU2', $
	                   OVERPLOT    = p_gdu1_0, $
	                   PSYM        = 3)
	l_0 = MrLegend( /AUTO_TEXT_COLOR, $
	                ALIGNMENT    = 'NE', $
	                LABEL        = ['GDU1', 'GDU2'], $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH = 0, $
	                TARGET       = [p_gdu1_0, p_gdu2_0])

	;180 Degree Counts
	p_gdu1_180 = MrPlot( t_180_ssm[igdu1_180], counts_180[igdu1_180], $
	                     /CURRENT, $
	                     NAME        = '180 Deg Counts GDU1', $
	                     PSYM        = 3, $
	                     TITLE       = '', $
	                     XTICKFORMAT = '(a1)', $
	                     XTITLE      = '', $
	                     /YLOG, $
	                     YRANGE      = [1, max(counts_180)], $
	                     YTITLE      = '180 Deg PA!C(Counts)')
	p_gdu2_180 = MrPlot( t_180_ssm[igdu2_180], counts_180[igdu2_180], $
	                     OVERPLOT    = p_gdu1_180, $
	                     COLOR       = 'blue', $
	                     NAME        = '180 Deg Counts GDU2', $
	                     PSYM        = 3)
	l_180 = MrLegend( /AUTO_TEXT_COLOR, $
	                  ALIGNMENT    = 'NE', $
	                  LABEL        = ['GDU1', 'GDU2'], $
	                  POSITION     = [1.0, 1.0], $
	                  /RELATIVE, $
	                  SAMPLE_WIDTH = 0, $
	                  TARGET       = [p_gdu1_180, p_gdu2_180])
	
	;SCPOT
	p_scpot = MrPlot( t_scpot_ssm, -scpot, $
	                  /CURRENT, $
	                  NAME        = 'V EDP', $
	                  TITLE       = '', $
	                  XTICKFORMAT = 'time_labels', $
	                  XTITLE      = 'Time (UT)', $
	                  YTITLE      = '-SCPot!C(V)')

;	;Anisotropy
;	p_gdu1_180 = MrPlot( t_0_ssm, anisotropy, $
;	                     /CURRENT, $
;	                     NAME        = 'EDI Anisotropy', $
;	                     TITLE       = '', $
;	                     XTICKFORMAT = 'time_labels', $
;	                     XTITLE      = 'Time (UTC)', $
;	                     /YLOG, $
;	                     YTITLE      = '0/180 PA!C(Counts)')

	win -> Refresh
	return, win
end