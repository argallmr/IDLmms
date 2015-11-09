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
pro mms_fig_EVvectors, sc, tstart, tend, $
EIGVECS=eigvecs
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg(/QUIET)
		return
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
	                        B_DMPA        = B_edi_dmpa, $
	                        E_DMPA        = E_edi_dmpa, $
	                        V_EXB_DMPA    = v_ExB_dmpa, $
	                        E_BC_DMPA     = E_bc_dmpa, $
	                        V_EXB_BC_DMPA = v_ExB_bc_dmpa, $
	                        TIME          = t_edi
	                       
	mms_edi_l2_q0_read, files_q0, tstart, tend, $
	                    COUNTS_GD12 = counts_gd12, $
	                    COUNTS_GD21 = counts_gd21, $
	                    TIME_GD12   = t_q0_gd12, $
	                    TIME_GD21   = t_q0_gd21

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
	
	;Transform EDP to FAC
	dmpa2fac = mms_edi_xxyz2bpp(B_edp)
	E_fac    = MrVector_Rotate(dmpa2fac, E_dsl)
	
	;Transform EDI to FAC
	dmpa2fac   = mms_edi_xxyz2bpp(B_edi_dmpa)
	E_edi_fac   = MrVector_Rotate(dmpa2fac, E_edi_dmpa)
	v_ExB_fac = MrVector_Rotate(dmpa2fac, v_ExB_dmpa)
	
	;Delete data
	dmpa2fac   = !Null
	E_dsl      = !Null
	E_edi_dmpa = !Null
	v_ExB_dmpa = !Null

;-----------------------------------------------------
; Create |E|*V/|V| Vector \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	EV      = fltarr(2, n_elements(t_edi))
	E_mag   = MrVector_Magnitude(E_edi_fac)
	V_hat   = MrVector_Normalize(v_ExB_fac) 
	EV[0,*] = V_hat[0,*] * E_mag
	EV[1,*] = V_hat[1,*] * E_mag

	E_mag = !Null
	V_hat = !Null
	
;-----------------------------------------------------
; Interpolate EDP data to 1 Minute Intervals \\\\\\\\\
;-----------------------------------------------------
	;Breakdown the first time
	MrCDF_Epoch_Breakdown, t_edi[0], year, month, day, hour, minute, second
	
	;Round down to the nearest 5 seconds and recompute
	second -= (second mod 60.0)
	minute -= (minute mod 1.0)
	MrCDF_Epoch_Compute, t0, year, month, day, hour, minute, second, /TT2000
	
	;Create a time array at 5-second intervals from T0 to the last time
	t_avg_edi = MrMake_Array(START=t0, INCREMENT=60000000000LL, LAST=t_edp[-1], /LONG64)
	EV_avg      = fltarr(2, n_elements(t_avg_edi))
	EV_avg[0,*] = Interpol(EV[0,*], t_edi, t_avg_edi)
	EV_avg[1,*] = Interpol(EV[1,*], t_edi, t_avg_edi)
	
	;Delete data
	EV    = !Null
;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to seconds
	t0            = t_fgm[0]
	t_fgm_ssm     = MrCDF_epoch2ssm(t_fgm,     t0)
;	t_edp_ssm     = MrCDF_epoch2ssm(t_avg_edp, t0)
	t_scpot_ssm   = MrCDF_epoch2ssm(t_scpot,   t0)
	t_edi_ssm     = MrCDF_epoch2ssm(t_edi,     t0)
	t_edi_avg_ssm = MrCDF_epoch2ssm(t_avg_edi, t0)
	t_q0_gd12_ssm = MrCDF_epoch2ssm(t_q0_gd12, t0)
	t_q0_gd21_ssm = MrCDF_epoch2ssm(t_q0_gd21, t0)
	
	;Define the x-range for all plots
	xrange = [t_fgm_ssm[0], t_fgm_ssm[-1]]

	;MMS Colors
	mms_color = mms_color(['blue', 'green', 'red', 'black'])

	;Create the window
	cgWindow
	cgControl, EXECUTE=0
	p = cgLayout([1,6], YGAP=0.5)
	
	;B EDI
	cgPlot, t_edi_ssm, B_edi_dmpa[0,*], $
	        /ADDCMD, $
	        COLOR       = 'sky blue', $
	        POSITION    = p[*,0], $
	        PSYM        = 1, $
	        XRANGE      = xrange, $
	        XTICKFORMAT = '(a1)', $
	        YRANGE      = [min(B_edi_dmpa, MAX=bmax), bmax]
	cgPlot, t_edi_ssm, B_edi_dmpa[1,*], $
	        /ADDCMD, $
	        /OVERPLOT, $
	        COLOR       = 'spring green', $
	        PSYM        = 1
	cgPlot, t_edi_ssm, B_edi_dmpa[2,*], $
	        /ADDCMD, $
	        /OVERPLOT, $
	        COLOR       = 'pink', $
	        PSYM        = 1

	;B DFG
	cgPlot, t_fgm_ssm, B_dmpa[0,*], $
	        /ADDCMD, $
	        /OVERPLOT, $
	        COLOR       = 'blue'
	cgPlot, t_fgm_ssm, B_dmpa[1,*], $
	        /ADDCMD, $
	        /OVERPLOT, $
	        COLOR       = 'forest green'
	cgPlot, t_fgm_ssm, B_dmpa[2,*], $
	        /ADDCMD, $
	        /OVERPLOT, $
	        COLOR       = 'red'
	
	;E EDI
	cgPlot, t_edi_ssm, E_edi_fac[0,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,1], $
	        COLOR       = 'blue', $
	        XRANGE      = xrange, $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'E!C(mV/m)'
	cgPlot, t_edi_ssm, E_edi_fac[1,*], $
	        /ADDCMD, $
	        /OVERPLOT, $
	        COLOR       = 'forest green'
	cgPlot, t_edi_ssm, E_edi_fac[2,*], $
	        /ADDCMD, $
	        /OVERPLOT, $
	        COLOR       = 'red'
	
	;V EDI
	cgPlot, t_edi_ssm, v_ExB_fac[0,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,2], $
	        COLOR       = 'blue', $
	        XRANGE      = xrange, $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'V$\downExB$!C(km/s)'
	cgPlot, t_edi_ssm, v_ExB_fac[1,*], $
	        /ADDCMD, $
	        /OVERPLOT, $
	        COLOR       = 'forest green'
	cgPlot, t_edi_ssm, v_ExB_fac[2,*], $
	        /ADDCMD, $
	        /OVERPLOT, $
	        COLOR       = 'red'
	
	;Q0
	cgPlot, t_edi_ssm, counts_gd12, $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,4], $
	        PSYM        = 2, $
	        SYMSIZE     = 0.5, $
	        XRANGE      = xrange, $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'Q0!C(counts)'
	cgPlot, t_edi_ssm, counts_gd21, $
	        /ADDCMD, $
	        /OVERPLOT, $
	        COLOR       = 'blue', $
	        POSITION    = p[*,4], $
	        PSYM        = 2, $
	        SYMSIZE     = 0.5, $
	        XTICKFORMAT = '(a1)'

	;-SCP
	cgPlot, t_scpot_ssm, scpot, $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,5], $
	        XRANGE      = xrange, $
	        XTICKFORMAT = 'time_labels', $
	        YTITLE      = '-SCPot!C(V)'
	
	;Aspect ratio = width/height
	refvect      = max( sqrt(EV_avg[0,*]^2 + EV_avg[1,*]^2), imax )
	yrange       = [-refvect, refvect]
	EV_avg[0,*] *= (xrange[1]-xrange[0]) / (yrange[1]-yrange[0])
	cgDrawVectors, EV_avg[0,*], EV_avg[1,*], t_edi_avg_ssm, replicate(0, n_elements(t_edi_avg_ssm)), $
	               /ADDCMD, $
	               /DATA, $
	               /NOERASE, $
	               LENGTH          = 0.005, $
	               VECCOLORS       = 'Black', $
	               POSITION        = p[*,3], $
	               REFERENCEVECTOR = refvect, $
	               YTITLE          = '|E|*V/|V|!C(mV/m)', $
	               YRANGE          = yrange, $
	               YSTYLE          = 1, $
	               XRANGE          = xrange, $
	               XTICKFORMAT     = '(a1)'
	; Draw reference arrow and legend.
	cgArrow, p[0], 0.95, p[0]+0.1, 0.95, /NORMAL, /ADDCMD
	x = (p[0] + p[0]+0.1) / 2.0
	y = 0.95 - ((2.0*!D.Y_CH_SIZE)/!D.Y_Size)
	cgText, x, y, String(refvect, Format='(f0.1)') + ' (mV/m)', /ADDCMD, /NORMAL
	
	;Execute all of the commands
	cgControl, EXECUTE=1
end