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
function mms_fig_xprox4, sc, mode, tstart, tend, $
DELAY=delay, $
EIGVECS=eigvecs
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return, !Null
	endif

;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;FGM Magnetic Field
	mms_fgm_ql_read, 'mms1', 'dfg', mode, 'l2pre', tstart, tend, B_GSE=b1, TIME=t1_fgm
	mms_fgm_ql_read, 'mms2', 'dfg', mode, 'l2pre', tstart, tend, B_GSE=b2, TIME=t2_fgm
	mms_fgm_ql_read, 'mms3', 'dfg', mode, 'l2pre', tstart, tend, B_GSE=b3, TIME=t3_fgm
	mms_fgm_ql_read, 'mms4', 'dfg', mode, 'l2pre', tstart, tend, B_GSE=b4, TIME=t4_fgm
	
	;FPI Ion Density and Velocity
	if mode eq 'srvy' then begin
		mms_fpi_sitl_read, 'mms1', 'fast', tstart, tend, VI_DSC=v1, N_I=n1, TIME=t1_fpi
		mms_fpi_sitl_read, 'mms2', 'fast', tstart, tend, VI_DSC=v2, N_I=n2, TIME=t2_fpi
		mms_fpi_sitl_read, 'mms3', 'fast', tstart, tend, VI_DSC=v3, N_I=n3, TIME=t3_fpi
		mms_fpi_sitl_read, 'mms4', 'fast', tstart, tend, VI_DSC=v4, N_I=n4, TIME=t4_fpi
	endif else begin
		mms_fpi_l1b_moms_read, 'mms1', 'des-moms', tstart, tend, N=n1, V_GSE=v1, TIME=t1_fpi
		mms_fpi_l1b_moms_read, 'mms2', 'des-moms', tstart, tend, N=n2, V_GSE=v2, TIME=t2_fpi
		mms_fpi_l1b_moms_read, 'mms3', 'des-moms', tstart, tend, N=n3, V_GSE=v3, TIME=t3_fpi
		mms_fpi_l1b_moms_read, 'mms4', 'des-moms', tstart, tend, N=n4, V_GSE=v4, TIME=t4_fpi
	endelse
		
	;EDP Electric Field
	edp_mode = mode eq 'brst' ? mode : 'fast'
	mms_edp_ql_read, 'mms1', edp_mode, tstart, tend, E_DSL=E1, TIME=t1_edp
	mms_edp_ql_read, 'mms2', edp_mode, tstart, tend, E_DSL=E2, TIME=t2_edp
	mms_edp_ql_read, 'mms3', edp_mode, tstart, tend, E_DSL=E3, TIME=t3_edp
	mms_edp_ql_read, 'mms4', edp_mode, tstart, tend, E_DSL=E4, TIME=t4_edp
	
	;EDI Electrons
	edi_mode = mode eq 'brst' ? mode : 'fast'
	mms_edi_amb_ql_read, 'mms1', edi_mode, tstart, tend, E_DSL=E1, TIME=t1_edp
	mms_edi_amb_ql_read, 'mms2', edi_mode, tstart, tend, E_DSL=E2, TIME=t2_edp
	mms_edi_amb_ql_read, 'mms3', edi_mode, tstart, tend, E_DSL=E3, TIME=t3_edp
	mms_edi_amb_ql_read, 'mms4', edi_mode, tstart, tend, E_DSL=E4, TIME=t4_edp

;-----------------------------------------------------
; Delay Times \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Reference time
	t0 = t1_fgm[0]

	if n_elements(delay) gt 0 then begin
		tt_delay       = MrCDF_Epoch_Parse(delay, PATTERN='%Y-%M-%dT%H:%m:%S%f', /TO_TT2000)
		tt_delay[1:3] -= tt_delay[0]

		;FGM
		t2_fgm -= tt_delay[1]
		t3_fgm -= tt_delay[2]
		t4_fgm -= tt_delay[3]
		
		;FPI
		t2_fpi -= tt_delay[1]
		t3_fpi -= tt_delay[2]
		t4_fpi -= tt_delay[3]
		
		;EDP
		t2_edp -= tt_delay[1]
		t3_edp -= tt_delay[2]
		t4_edp -= tt_delay[3]
	endif

;-----------------------------------------------------
; Rotate to GSE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get definitive attitude data and rotate
	if E1[0] eq -1 then begin
		t1_edp = t1_fgm[0]
		E1     = fltarr(3)
	endif else begin
;		defatt1 = mms_fdoa_defatt('mms1', tstart, tend)
;		E1      = mms_rot_despun2gse(temporary(defatt1), t1_edp, temporary(E1), TYPE='L')
	endelse
	
	;MMS2
	if E2[0] eq -1 then begin
		t2_edp = t2_fgm[0]
		E2     = fltarr(3)
	endif else begin
;		defatt2 = mms_fdoa_defatt('mms2', tstart, tend)
;		E2      = mms_rot_despun2gse(temporary(defatt2), t2_edp, temporary(E2), TYPE='L')
	endelse
	
	;MMS3
	if E3[0] eq -1 then begin
		t3_edp = t3_fgm[0]
		E3     = fltarr(3)
	endif else begin
;		defatt3 = mms_fdoa_defatt('mms3', tstart, tend)
;		E3      = mms_rot_despun2gse(temporary(defatt3), t3_edp, temporary(E3), TYPE='L')
	endelse
	
	;MMS4
	if E4[0] eq -1 then begin
		t4_edp = t4_fgm[0]
		E4     = fltarr(3)
	endif else begin
;		defatt4 = mms_fdoa_defatt('mms4', tstart, tend)
;		E4      = mms_rot_despun2gse(temporary(defatt4), t4_edp, temporary(E4), TYPE='L')
	endelse

;-----------------------------------------------------
; BL \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	title  = 'X-Line Proximity ' + strmid(tstart, 0, 10)
	xrange = MrCDF_Epoch_Parse([tstart, tend], PATTERN='%Y-%M-%dT%H:%m:%S', /TO_TT2000)
	xrange = MrCDF_epoch2ssm(xrange, t0)

	;MMS Colors
	mms_color = mms_color(['blue', 'green', 'red', 'black'])

	;Create the window
	win   = MrWindow(OXMARGIN=[10,4], XSIZE=600, YGAP=0.5, REFRESH=0)
	
	Brange = [ min( [ [B1[2,*]], [B2[2,*]], [B3[2,*]], [B4[2,*]] ], MAX=bmax), bmax ]
	
	;MMS1
	p1_BL = MrPlot(MrCDF_epoch2ssm(t1_fgm, t0), B1[2,*], $
	               /CURRENT, $
	               COLOR       = mms_color[3], $
	               NAME        = 'BL MMS1', $
	               TITLE       = title, $
	               XRANGE      = xrange, $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = Brange, $
	               YTITLE      = 'B$\downL$!C(nT)')
	
	;MMS2
	p2_BL = MrPlot(MrCDF_epoch2ssm(t2_fgm, t0), B2[2,*], $
	               COLOR    = mms_color[2], $
	               NAME     = 'BL MMS2', $
	               OVERPLOT = p1_BL)
	
	;MMS3
	p3_BL = MrPlot(MrCDF_epoch2ssm(t3_fgm, t0), B3[2,*], $
	               COLOR    = mms_color[1], $
	               NAME     = 'BL MMS3', $
	               OVERPLOT = p1_BL)
	
	;MMS4
	p4_BL = MrPlot(MrCDF_epoch2ssm(t4_fgm, t0), B4[2,*], $
	               COLOR    = mms_color[0], $
	               NAME     = 'BL MMS4', $
	               OVERPLOT = p1_BL)

;-----------------------------------------------------
; BM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	Brange = [ min( [ [B1[1,*]], [B2[1,*]], [B3[1,*]], [B4[1,*]] ], MAX=bmax), bmax ]
	
	;MMS1
	p1_BM = MrPlot(MrCDF_epoch2ssm(t1_fgm, t0), B1[1,*], $
	               /CURRENT, $
	               COLOR       = mms_color[3], $
	               NAME        = 'BM MMS1', $
	               XRANGE      = xrange, $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = Brange, $
	               YTITLE      = 'B$\downM$!C(nT)')
	
	;MMS2
	p2_BM = MrPlot(MrCDF_epoch2ssm(t2_fgm, t0), B2[1,*], $
	               COLOR    = mms_color[2], $
	               NAME     = 'BM MMS2', $
	               OVERPLOT = p1_BM)
	
	;MMS3
	p3_BM = MrPlot(MrCDF_epoch2ssm(t3_fgm, t0), B3[1,*], $
	               COLOR    = mms_color[1], $
	               NAME     = 'BM MMS3', $
	               OVERPLOT = p1_BM)
	
	;MMS4
	p4_BM = MrPlot(MrCDF_epoch2ssm(t4_fgm, t0), B4[1,*], $
	               COLOR    = mms_color[0], $
	               NAME     = 'BM MMS4', $
	               OVERPLOT = p1_BM)

;-----------------------------------------------------
; NI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	nrange = [ min( [ [n1], [n2], [n3], [n4] ], MAX=nmax), nmax ]

	;MMS1
	p1_N = MrPlot(MrCDF_epoch2ssm(t1_fpi, t0), n1, $
	               /CURRENT, $
	               COLOR       = mms_color[3], $
	               NAME        = 'N MMS1', $
	               XRANGE      = xrange, $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = vrange, $
	               YTITLE      = 'n!C(cm^-3)')
	
	;MMS2
	p2_N = MrPlot(MrCDF_epoch2ssm(t2_fpi, t0), n2, $
	               COLOR    = mms_color[2], $
	               NAME     = 'N MMS2', $
	               OVERPLOT = p1_N)
	
	;MMS3
	p3_N = MrPlot(MrCDF_epoch2ssm(t3_fpi, t0), n3, $
	               COLOR    = mms_color[1], $
	               NAME     = 'N MMS3', $
	               OVERPLOT = p1_N)
	
	;MMS4
	p4_N = MrPlot(MrCDF_epoch2ssm(t4_fpi, t0), n4, $
	               COLOR    = mms_color[0], $
	               NAME     = 'N MMS4', $
	               OVERPLOT = p1_N)

;-----------------------------------------------------
; VL \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	vrange = [ min( [ [V1[2,*]], [V2[2,*]], [V3[2,*]], [V4[2,*]] ], MAX=vmax), vmax ]
	
	;MMS1
	p1_VL = MrPlot(MrCDF_epoch2ssm(t1_fpi, t0), v1[2,*], $
	               /CURRENT, $
	               COLOR       = mms_color[3], $
	               NAME        = 'VL MMS1', $
	               XRANGE      = xrange, $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = vrange, $
	               YTITLE      = 'V$\downL$!C(km/s)')
	
	;MMS2
	p2_VL = MrPlot(MrCDF_epoch2ssm(t2_fpi, t0), v2[2,*], $
	               COLOR    = mms_color[2], $
	               NAME     = 'VL MMS2', $
	               OVERPLOT = p1_VL)
	
	;MMS3
	p3_VL = MrPlot(MrCDF_epoch2ssm(t3_fpi, t0), v3[2,*], $
	               COLOR    = mms_color[1], $
	               NAME     = 'VL MMS3', $
	               OVERPLOT = p1_VL)
	
	;MMS4
	p4_VL = MrPlot(MrCDF_epoch2ssm(t4_fpi, t0), v4[2,*], $
	               COLOR    = mms_color[0], $
	               NAME     = 'VL MMS4', $
	               OVERPLOT = p1_VL)

;-----------------------------------------------------
; EN \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	Erange = [ min( [ [E1[0,*]], [E2[0,*]], [E3[0,*]], [E4[0,*]] ], MAX=emax), emax ]

	;MMS1
	p1_EN = MrPlot(MrCDF_epoch2ssm(t1_edp, t0), E1[0,*], $
	               /CURRENT, $
	               COLOR       = mms_color[3], $
	               NAME        = 'EN MMS1', $
	               XRANGE      = xrange, $
	               XTICKFORMAT = 'time_labels', $
	               YRANGE      = Erange, $
	               YTITLE      = 'E$\downN$!C(mV/m)')
	
	;MMS2
	p2_EN = MrPlot(MrCDF_epoch2ssm(t2_edp, t0), E2[0,*], $
	               COLOR    = mms_color[2], $
	               NAME     = 'EN MMS2', $
	               OVERPLOT = p1_EN)
	
	;MMS3
	p3_EN = MrPlot(MrCDF_epoch2ssm(t3_edp, t0), E3[0,*], $
	               COLOR    = mms_color[1], $
	               NAME     = 'EN MMS3', $
	               OVERPLOT = p1_EN)
	
	;MMS4
	p4_EN = MrPlot(MrCDF_epoch2ssm(t4_edp, t0), E4[0,*], $
	               COLOR    = mms_color[0], $
	               NAME     = 'EN MMS4', $
	               OVERPLOT = p1_EN)

;-----------------------------------------------------
; Make Pretty \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Add legend
	gL = MrLegend(ALIGNMENT  ='SE', $
	              LABEL      = ['MMS1', 'MMS2', 'MMS3', 'MMS4'], $
	              POSITION   = [1.0, 0.0], $
	              /RELATIVE, $
	              SAMPLE_WIDTH = 0, $
	              TEXT_COLOR = mms_color[3:0:-1], $
	              TARGET     = [P1_Bl, P2_BL, P3_BL, P4_BL])
	
	win -> Refresh
	return, win
end