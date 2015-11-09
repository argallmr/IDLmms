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
function mms_fig_vdrift_fac, sc, tstart, tend, $
BC=bc, $
EIGVECS=eigvecs, $
RADIAL=radial
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg(/QUIET)
		return, !Null
	endif
	
	bc     = keyword_set(bc)
	radial = keyword_set(radial)

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
	                        B_DMPA        = b_edi_dmpa, $
	                        V_EXB_DMPA    = v_ExB_dmpa, $
	                        V_EXB_BC_DMPA = v_ExB_bc_dmpa, $
	                        TIME          = t_edi
	                       
	mms_edi_l2_q0_read, files_q0, tstart, tend, $
	                    COUNTS_GD12 = counts_gd12, $
	                    COUNTS_GD21 = counts_gd21, $
	                    TIME_GD12   = t_q0_gd12, $
	                    TIME_GD21   = t_q0_gd21

	;Beam convergence or bestarg?
	if bc $
		then v_ExB_edi = v_ExB_bc_dmpa $
		else v_ExB_edi = v_ExB_dmpa
	v_ExB_dmpa    = !Null
	v_ExB_bc_dmpa = !Null

	v_exb_edi = replace_fillval(v_exb_edi, -1e28)
;-----------------------------------------------------
; Median Smooth Q0 Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	counts_gd12 = median(reform(counts_gd12), 5)
	counts_gd21 = median(reform(counts_gd21), 5)

;-----------------------------------------------------
; Rotate to Field-Aligned Coordinates \\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Interpolate mangetic field onto electric field time grid
	B_edp = MrInterpol(B_dmpa[0:2,*], t_fgm, t_edp)

	;Compute the ExB drift
	v_ExB_dsl = MrExB_Drift(E_dsl, B_edp[0:2,*])
	
	;Transform EDP to FAC
	if radial then begin
		;Create transformation
		;   - z'   = b_hat
		;   - phi' = z' x r
		;   - r'   = phi x z'
		r_dmpa   = mms_fdoa_scpos(sc, tstart, tend, t_edp)
		z_hat    = MrVector_Normalize(B_edp)
		phi_hat  = MrVector_Normalize( MrVector_Cross(z_hat, r_dmpa) )
		r_hat    = MrVector_Cross( phi_hat, z_hat )
		
		;Fill transformation matrix
		dmpa2fac = fltarr(3,3,n_elements(t_edp))
		dmpa2fac[*,0,*] = r_hat
		dmpa2fac[*,1,*] = phi_hat
		dmpa2fac[*,2,*] = z_hat
	endif else begin
		dmpa2fac = mms_edi_xxyz2bpp(B_edp)
	endelse
	E_fac         = MrVector_Rotate(dmpa2fac, E_dsl)
	v_ExB_edp_fac = MrVector_Rotate(dmpa2fac, v_ExB_dsl)
	
	;Transform EDI to FAC
	if radial then begin
		;Create transformation
		;   - z'   = b_hat
		;   - phi' = z' x r
		;   - r'   = phi x z'
		r_dmpa   = mms_fdoa_scpos(sc, tstart, tend, t_edi)
		z_hat    = MrVector_Normalize(b_edi_dmpa)
		phi_hat  = MrVector_Normalize( MrVector_Cross(z_hat, r_dmpa) )
		r_hat    = MrVector_Cross( phi_hat, z_hat )
		
		;Fill transformation matrix
		dmpa2fac = fltarr(3,3,n_elements(t_edi))
		dmpa2fac[*,0,*] = r_hat
		dmpa2fac[*,1,*] = phi_hat
		dmpa2fac[*,2,*] = z_hat
	endif else begin
		dmpa2fac  = mms_edi_xxyz2bpp(b_edi_dmpa)
	endelse
	v_ExB_fac_edi = MrVector_Rotate(dmpa2fac, v_ExB_edi)
	
	;Delete data
	dmpa2fac   = !Null
	E_dsl      = !Null
	b_edi_dmpa = !Null
	v_ExB_edi  = !Null
	r_dmpa     = !Null
	phi_hat    = !Null
	z_hat      = !Null
	
	
;-----------------------------------------------------
; Interpolate EDP & FGM to 5 Second Intervals \\\\\\\\
;-----------------------------------------------------
	;Breakdown the first time
	MrCDF_Epoch_Breakdown, t_edp[0], year, month, day, hour, minute, second
	
	;Round down to the nearest 5 seconds and recompute
	second -= (second mod 5.0)
	MrCDF_Epoch_Compute, t0, year, month, day, hour, minute, second, /TT2000
	
	;Create a time array at 5-second intervals from T0 to the last time
	t_avg         = MrMake_Array(START=t0, INCREMENT=5000000000LL, LAST=t_edp[-1], /LONG64)
	E_avg_fac     = MrInterpol(E_fac,         t_edp, t_avg)
	v_ExB_avg_fac = MrInterpol(v_ExB_edp_fac, t_edp, t_avg)

;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to seconds
	t0            = t_avg[0]
	t_fgm_ssm     = MrCDF_epoch2ssm(t_fgm,     t0)
	t_edp_ssm     = MrCDF_epoch2ssm(t_edp,     t0)
	t_avg_ssm     = MrCDF_epoch2ssm(t_avg,     t0)
	t_scpot_ssm   = MrCDF_epoch2ssm(t_scpot,   t0)
	t_edi_ssm     = MrCDF_epoch2ssm(t_edi,     t0)
	t_q0_gd12_ssm = MrCDF_epoch2ssm(t_q0_gd12, t0)
	t_q0_gd21_ssm = MrCDF_epoch2ssm(t_q0_gd21, t0)

	;Set time range
	xrange = [t_fgm_ssm[0], t_fgm_ssm[-1]]

	;Create the window
	win   = MrWindow(YGAP=0.5, REFRESH=0)
	
	;DFG
	p_fgm = MrPlot( t_fgm_ssm, B_dmpa, $
	                /CURRENT, $
	                COLOR       = ['blue', 'green', 'red', 'black'], $
	                DIMENSION   = 2, $
	                NAME        = 'B FGM', $
	                TITLE       = 'FGM', $
	                XRANGE      = xrange, $
	                XTICKFORMAT = '(a1)', $
	                XTITLE      = '', $
	                YTITLE      = 'B!C(nT)')
	l_fgm = MrLegend( ALIGNMENT    = 'NE', $
	                  LABEL        = ['|B|', 'B$\downX$', 'B$\downY$', 'B$\downZ$'], $
	                  NAME         = 'B legend', $
	                  POSITION     = [1.0, 1.0], $
	                  /RELATIVE, $
	                  TEXT_COLOR   = ['black', 'blue', 'green', 'red'], $
	                  SAMPLE_WIDTH = 0, $
	                  TARGET       = p_fgm )
	
	;EDP
	p_edp = MrPlot( t_edp_ssm, E_fac, $
	                /CURRENT, $
	                COLOR       = ['blue', 'green', 'red'], $
	                DIMENSION   = 2, $
	                NAME        = 'E EDP', $
	                TITLE       = '', $
	                XRANGE      = xrange, $
	                XTICKFORMAT = '(a1)', $
	                XTITLE      = '', $
	                YTITLE      = 'E!C(mV/m)')
	l_edp = MrLegend( ALIGNMENT    = 'NE', $
	                  LABEL        = ['E$\downPerp1$', 'E$\downPerp2$', 'E$\down||$'], $
	                  NAME         = 'E legend', $
	                  POSITION     = [1.0, 1.0], $
	                  /RELATIVE, $
	                  TEXT_COLOR   = ['blue', 'green', 'red'], $
	                  SAMPLE_WIDTH = 0, $
	                  TARGET       = p_edp )
	
	;Vx -- EDP & EDI
	p_edpx = MrPlot( t_avg_ssm, v_ExB_avg_fac[0,*], $
	                 /CURRENT, $
	                 NAME        = 'Vperp1 ExB', $
	                 TITLE       = '', $
	                 XRANGE      = xrange, $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'V$\downPerp1$!C(km/s)')
	p_edix = MrPlot( t_edi_ssm, v_ExB_fac_edi[0,*], $
	                 OVERPLOT    = p_edpx, $
	                 COLOR       = 'blue', $
	                 NAME        = 'Vperp1 EDI', $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'V$\downPerp1$!C(km/s)')
	l_ex = MrLegend( ALIGNMENT    = 'NE', $
	                 LABEL        = ['ExB/|B|$\up2$', 'EDI'], $
	                 NAME         = 'Vperp1 legend', $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 TEXT_COLOR   = ['black', 'blue'], $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p_edpx, p_edix] )
	
	;Vy -- EDP & EDI
	p_edpy = MrPlot( t_avg_ssm, v_ExB_avg_fac[1,*], $
	                 /CURRENT, $
	                 NAME        = 'Vperp2 ExB', $
	                 TITLE       = '', $
	                 XRANGE      = xrange, $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'V$\downPerp2$!C(km/s)')
	p_ediy = MrPlot( t_edi_ssm, v_ExB_fac_edi[1,*], $
	                 OVERPLOT    = p_edpy, $
	                 COLOR       = 'blue', $
	                 NAME        = 'Vperp2 EDI', $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'V$\downPerp2$!C(km/s)')
	l_ey = MrLegend( ALIGNMENT    = 'NE', $
	                 LABEL        = ['ExB/|B|$\up2$', 'EDI'], $
	                 NAME         = 'Vperp2 legend', $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 TEXT_COLOR   = ['black', 'blue'], $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p_edpy, p_ediy] )
	
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
	                    OVERPLOT    = p_gdu1_q0, $
	                    PSYM        = 3, $
	                    SYMSIZE     = 3, $
	                    NAME        = 'Q0 Counts GDU2', $
	                    TITLE       = '', $
	                    XTICKFORMAT = '(a1)', $
	                    XTITLE      = '', $
	                    YRANGE      = [1, max(counts_gd21)], $
	                    YTITLE      = 'Q0!C(Counts)')
	l_q0 = MrLegend( ALIGNMENT    = 'NE', $
	                 LABEL        = ['GDU1', 'GDU2'], $
	                 NAME         = 'Q0 legend', $
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