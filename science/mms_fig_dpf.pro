; docformat = 'rst'
;
; NAME:
;       mms_fig_dpf.pro
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
;       2015-09-23  -   Written by Matthew Argall
;-
function mms_fig_dpf, sc, tstart, tend, $
BC=bc, $
RADIAL=radial, $
NSMOOTH=nsmooth
	compile_opt strictarr
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 && obj_valid(win) then obj_destroy, win
		void = cgErrorMSG(/QUIET)
		return, !Null
	endif
	
	bc     = keyword_set(bc)
	radial = keyword_set(radial)
	if n_elements(nsmooth) eq 0 then nsmooth = 0

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
	
	;EPD-EIS L1B file
	files_epd = mms_find_file(sc, 'epd-eis', 'srvy', 'l1b', $
	                          COUNT     = nsrvy_epd, $
	                          OPTDESC   = 'electronenergy', $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
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
	
	;EDI SL SRVY E-field file
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
	
	mms_epd_eis_l1b_read, files_epd, tstart, tend, $
	                      E_COUNTS_T0 = e_counts_t0, $
	                      TIME        = t_epd
	
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

	if bc $
		then E_edi = E_bc_dmpa $
		else E_edi = E_edi_dmpa
	E_edi_dmpa = !Null
	E_bc_dmpa  = !Null
;-----------------------------------------------------
; Spectrogram of Bx \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	dt     = median( double(t_fgm[1:*]-t_fgm) * 1d-9 )
	df     = 1/dt
	dt     = 1/float(round(df))
	df     = 1/dt
	nfft   = 2048
	nshift = nfft / 4.0

	Bx_psd = MrPSD(reform(b_dmpa[0,*]), nfft, dt, nshift, $
	               FREQUENCIES = frequencies, $
	               NDIFF       = nDiff, $
	               T0          = MrCDF_epoch2ssm(t_fgm[0]), $
	               TIME        = t_psd)


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
	if radial then begin
		;Create transformation
		;   - z'   = b_hat
		;   - phi' = z' x r
		;   - r'   = phi x z'
		z_hat    = MrVector_Normalize(b_edp)
		r_dmpa   = mms_fdoa_scpos(sc, tstart, tend, reform(t_edp))
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
	E_fac = MrVector_Rotate(dmpa2fac, E_dsl)
	
	;Transform EDI to FAC
	if radial then begin
		;Create transformation
		;   - z'   = b_hat
		;   - phi' = z' x r
		;   - r'   = phi x z'
		z_hat    = MrVector_Normalize(b_edi_dmpa)
		r_dmpa   = mms_fdoa_scpos(sc, tstart, tend, reform(t_edi))
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
	E_edi_fac = MrVector_Rotate(dmpa2fac, E_edi)
	
	;Delete data
	dmpa2fac   = !Null
	E_dsl      = !Null
	E_edi      = !Null
	
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
	E_avg_fac = MrInterpol(E_fac, t_edp, t_avg_edp)
	
	;Delete data
	E_fac = !Null
	t_edp = !Null
	
;-----------------------------------------------------
; Smooth the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if nsmooth ne 0 then begin
		for i = 0, 2 do begin
			E_avg_fac[i,*] = smooth(E_avg_fac[i,*], nsmooth, /EDGE_TRUNCATE, /NAN)
			E_edi_fac[i,*] = smooth(E_edi_fac[i,*], nsmooth, /EDGE_TRUNCATE, /NAN)
		endfor
	endif

;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert time to seconds
	t0            = t_fgm[0]
	t_fgm_ssm     = MrCDF_epoch2ssm(t_fgm,     t0)
	t_epd_ssm     = MrCDF_epoch2ssm(t_epd,     t0)
	t_edp_ssm     = MrCDF_epoch2ssm(t_avg_edp, t0)
	t_scpot_ssm   = MrCDF_epoch2ssm(t_scpot,   t0)
	t_edi_ssm     = MrCDF_epoch2ssm(t_edi,     t0)
	t_q0_gd12_ssm = MrCDF_epoch2ssm(t_q0_gd12, t0)
	t_q0_gd21_ssm = MrCDF_epoch2ssm(t_q0_gd21, t0)

	;Time range
	xrange = [t_fgm_ssm[0], t_fgm_ssm[-1]]
	title  = strupcase(sc) + ' ' + strmid(tstart, 0, 10)

	;MMS Colors
	mms_color = mms_color(['blue', 'green', 'red', 'black'])

	;Create the window
	win   = MrWindow(OXMARGIN=[10,13], YSIZE=690, YGAP=0.5, REFRESH=0)
	
	;DFG
	p_xfgm = MrPlot( t_fgm_ssm, B_dmpa[0,*], $
	                /CURRENT, $
;	                COLOR       = ['blue', 'green', 'red', 'black'], $
	                NAME        = 'Bx FGM', $
	                TITLE       = title, $
	                XTICKFORMAT = '(a1)', $
	                XRANGE      = xrange, $
	                XTITLE      = '', $
	                YTITLE      = 'Bx!C(nT)')
	
	p = p_xfgm.position
	p_zfgm = MrPlot( t_fgm_ssm, B_dmpa[2,*], $
	                /CURRENT, $
	                COLOR       = 'blue', $
	                NAME        = 'Bz FGM', $
	                POSITION    = p, $
	                TITLE       = '', $
	                XTICKFORMAT = '(a1)', $
	                XRANGE      = xrange, $
	                XSTYLE      = 4, $
	                XTITLE      = '', $
	                YSTYLE      = 4, $
	                YTITLE      = 'Bz!C(nT)')
	a_fgm = MrAxis( 'Y', $
	                COLOR    = 'blue', $
	                NAME     = 'Bz Axis', $
	                LOCATION = 'right', $
	                TARGET   = p_zfgm, $
	                TITLE    = 'Bz!C(nT)')

;	l_fgm = MrLegend( ALIGNMENT    = 'NW', $
;	                  LABEL        = ['|B|', 'B$\downX$', 'B$\downY$', 'B$\downZ$'], $
;	                  POSITION     = [1.0, 1.0], $
;	                  /RELATIVE, $
;	                  TEXT_COLOR   = ['black', 'blue', 'green', 'red'], $
;	                  SAMPLE_WIDTH = 0, $
;	                  TARGET       = p_xfgm )
	                  
	p_psd = MrImage( Bx_psd, t_psd, frequencies, $
	                 /AXES, $
	                 /CURRENT, $
	                 /LOG, $
	                 /SCALE, $
	                 /YLOG, $
	                 CTINDEX     = 13, $
	                 NAME        = 'Bx PSD', $
	                 XTICKFORMAT = '(a1)', $
	                 YTITLE      = 'f!C(Hz)' )
	cb_psd = MrColorbar(NAME   = 'Bx PSD CB', $
	                    TARGET = p_psd, $
	                    TITLE  = 'Bx PSD!C(nT$\up2$/Hz)', $
	                    WIDTH  = 1.5)

	;EPD
	p_epd = MrPlot( t_epd_ssm, e_counts_t0, $
	                /CURRENT, $
	                DIMENSION   = 2, $
	                NAME        = 'e- Counts EPD', $
	                TITLE       = '', $
	                XTICKFORMAT = '(a1)', $
	                XRANGE      = xrange, $
	                XTITLE      = '', $
	                /YLOG, $
	                YRANGE      = [1, max(e_counts_t0)], $
	                YTITLE      = 'e- Counts')
	l_fgm = MrLegend( ALIGNMENT    = 'NW', $
	                  LABEL        = ['40', '70', '116', '231', '662'] + ' keV', $
	                  NAME         = ['EPD legend'], $
	                  POSITION     = [1.0, 1.0], $
	                  /RELATIVE, $
;	                  TEXT_COLOR   = ['black', 'blue', 'green', 'red'], $
	                  SAMPLE_WIDTH = 0, $
	                  TARGET       = p_epd )
	
	;E-perp1 -- EDP & EDI
	p_edpx = MrPlot( t_edp_ssm, E_avg_fac[0,*], $
	                 /CURRENT, $
	                 NAME        = 'Eperp1 EDP', $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XRANGE      = xrange, $
	                 XTITLE      = '', $
	                 YTITLE      = 'E$\downPerp1$!C(mV/m)')
	p_edix = MrPlot( t_edi_ssm, E_edi_fac[0,*], $
	                 OVERPLOT    = p_edpx, $
	                 COLOR       = 'blue', $
	                 NAME        = 'Eperp1 EDI', $
	                 TITLE       = '', $
	                 XRANGE      = xrange, $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'E$\downPerp1$!C(mV/m)')
	l_ex = MrLegend( ALIGNMENT    = 'SW', $
	                 LABEL        = ['DCE', 'EDI'], $
	                 NAME         = ['Eperp1 legend'], $
	                 POSITION     = [1.0, 0.0], $
	                 /RELATIVE, $
	                 TEXT_COLOR   = ['black', 'blue'], $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p_edpx, p_edix] )
	
	;E-perp2 -- EDP & EDI
	p_edpy = MrPlot( t_edp_ssm, E_avg_fac[1,*], $
	                 /CURRENT, $
	                 NAME        = 'Eperp2 EDP', $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XRANGE      = xrange, $
	                 XTITLE      = '', $
	                 YTITLE      = 'E$\downPerp2$!C(mV/m)')
	p_ediy = MrPlot( t_edi_ssm, E_edi_fac[1,*], $
	                 OVERPLOT    = p_edpy, $
	                 COLOR       = 'blue', $
	                 NAME        = 'Eperp2 EDI', $
	                 TITLE       = '', $
	                 XRANGE      = xrange, $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '', $
	                 YTITLE      = 'E$\downPerp2$!C(mV/m)')
	l_ey = MrLegend( ALIGNMENT    = 'NW', $
	                 LABEL        = ['DCE', 'EDI'], $
	                 NAME         = ['Eperp2 legend'], $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 TEXT_COLOR   = ['black', 'blue'], $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = [p_edpy, p_ediy] )
	
	;E-par -- EDP & EDI
	p_edpz = MrPlot( t_edp_ssm, E_avg_fac[2,*], $
	                 /CURRENT, $
	                 NAME        = 'Epar EDP', $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XRANGE      = xrange, $
	                 XTITLE      = '', $
	                 YTITLE      = 'E$\down||$!C(mV/m)')
;	p_ediz = MrPlot( t_edi_ssm, E_edi_fac[2,*], $
;	                 OVERPLOT    = p_edpz, $
;	                 COLOR       = 'blue', $
;	                 NAME        = 'Ez EDI', $
;	                 TITLE       = '', $
;	                 XTICKFORMAT = '(a1)', $
;	                 XTITLE      = '', $
;	                 YTITLE      = 'E$\down||$!C(mV/m)')
	l_ez = MrLegend( ALIGNMENT    = 'NW', $
	                 LABEL        = ['DCE', 'EDI'], $
	                 NAME         = ['Epar legend'], $
	                 POSITION     = [1.0, 1.0], $
	                 /RELATIVE, $
	                 TEXT_COLOR   = ['black', 'blue'], $
	                 SAMPLE_WIDTH = 0, $
	                 TARGET       = p_edpz )
	
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
	                    XTICKFORMAT = '(a1)', $
	                    XTITLE      = '', $
	                    YRANGE      = [1, max(counts_gd21)], $
	                    YTITLE      = 'Q0!C(Counts)')
	l_q0 = MrLegend( ALIGNMENT    = 'NW', $
	                 LABEL        = ['GDU1', 'GDU2'], $
	                 NAME         = ['Q0 legend'], $
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
	win -> Refresh, /DISABLE
	win['Bz FGM'].position = win['Bx FGM'].position
	win -> Refresh
	return, win
end
