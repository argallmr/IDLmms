; docformat = 'rst'
;
; NAME:
;    mms_test_rotations
;
; PURPOSE:
;+
;   Test transformations using MEC data.
;
; :Categories:
;    MMS, Diagnostics
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 348
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/11/29  -   Written by Matthew Argall
;-
function mms_test_mec_rotations, sc, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win_gse) then obj_destroy, win_gse
		if obj_valid(win_gsm) then obj_destroy, win_gsm
		MrPrintF, 'LogErr'
		return, !Null
	endif

	sc       = 'mms3'
	tstart   = '2015-08-15T11:00:00Z'
	tend     = '2015-08-15T16:00:00Z'

;-------------------------------------------------------
; Read Data ////////////////////////////////////////////
;-------------------------------------------------------
	
	;Read all data
	mms_fgm_ql_read, sc, 'dfg', 'srvy', 'l2pre', tstart, tend, $
	                 B_BCS  = b_bcs, $
	                 B_DMPA = b_dmpa, $
	                 B_GSE  = b_gse, $
	                 B_GSM  = b_gsm, $
	                 TIME   = t_fgm
	                 
	
	;MEC
	mms_mec_read, sc, 'epht89d', tstart, tend, $
;	              LPHASE   = lphase, $
;	              LVEC     = lvec, $
	              QECI2BCS = qeci2bcs, $
	              QECI2GSE = qeci2gse, $
	              QECI2GSM = qeci2gsm, $
	              TIME     = t_mec
	
	;Extract magnetic field data of interest
	b_bcs  = b_bcs[0:2,*]
	b_dmpa = b_dmpa[0:2,*]
	b_gse  = b_gse[0:2,*]
	b_gsm  = b_gsm[0:2,*]
	
	;Delete leftovers
	fgm_l2pre = !Null

;-------------------------------------------------------
; Interpolate ./////////////////////////////////////////
;-------------------------------------------------------
	;Work with double-precision time (convert to SSM)
	t0        = t_fgm[0]
	t_fgm_ssm = MrCDF_epoch2ssm(t_fgm, t0)
	t_mec_ssm = MrCDF_epoch2ssm(t_mec, t0)
	
	;Interpolate
	qeci2bcs = qterp(t_mec_ssm, qeci2bcs, t_fgm_ssm)
	qeci2gse = qterp(t_mec_ssm, qeci2gse, t_fgm_ssm)
	qeci2gsm = qterp(t_mec_ssm, qeci2gsm, t_fgm_ssm)

;-------------------------------------------------------
; Rotate Vectors ///////////////////////////////////////
;-------------------------------------------------------
	
	;BCS --> ECI
	b_unh_eci = qtvrot(b_bcs, qeci2bcs, /INVERT)
	
	;ECI --> GSE
	b_unh_gse = qtvrot(b_unh_eci, qeci2gse)
	
	;ECI --> GSM
	b_unh_gsm = qtvrot(b_unh_eci, qeci2gsm)

;-------------------------------------------------------
; GSE Results //////////////////////////////////////////
;-------------------------------------------------------
	;Create a window
	win_gse = MrWindow(LAYOUT=[1,3], YGAP=0.5, REFRESH=0)
	
	;BX Component
	p_bx1_gse = MrPlot(t_fgm_ssm, b_gse[0,*], $
	                   /CURRENT, $
	                   NAME        = 'Bx FGM', $
	                   TITLE       = 'MEC GSE Transformation Comparision', $
	                   XTICKFORMAT = '(a1)', $
	                   YTITLE      = 'B$\downX$!C(nT)')
	p_bx2_gse = MrPlot(t_fgm_ssm, b_unh_gse[0,*], $
	                   COLOR    = 'blue', $
	                   NAME     = 'Bx MEC', $
	                   OVERPLOT = p_bx1_gse)
	
	;By Component
	p_by1_gse = MrPlot(t_fgm_ssm, b_gse[1,*], $
	                   /CURRENT, $
	                   NAME        = 'By FGM', $
	                   XTICKFORMAT = '(a1)', $
	                   YTITLE      = 'B$\downY$!C(nT)')
	p_by2_gse = MrPlot(t_fgm_ssm, b_unh_gse[1,*], $
	                   COLOR    = 'blue', $
	                   NAME     = 'By MEC', $
	                   OVERPLOT = p_by1_gse)
	
	;Bz Component
	p_bz1_gse = MrPlot(t_fgm_ssm, b_gse[2,*], $
	                   /CURRENT, $
	                   NAME        = 'Bz FGM', $
	                   XTICKFORMAT = 'time_labels', $
	                   YTITLE      = 'B$\downZ$!C(nT)')
	p_bz2_gse = MrPlot(t_fgm_ssm, b_unh_gse[2,*], $
	                   COLOR    = 'blue', $
	                   NAME     = 'Bz MEC', $
	                   OVERPLOT = p_bz1_gse)
	
	;Legend
	l = MrLegend(ALIGNMENT    = 'NE', $
	             LABEL        = ['FGM', 'QMEC'], $
	             POSITION     = [1.0, 1.0], $
	             /RELATIVE, $
	             SAMPLE_WIDTH = 0, $
	             TEXT_COLOR   = ['Black', 'Blue'], $
	             TARGET       = [p_bx1_gse, p_bx2_gse])
	
	;Refresh the window
	win_gse -> Refresh

;-------------------------------------------------------
; GSM Results //////////////////////////////////////////
;-------------------------------------------------------
	;Create a window
	win_gsm = MrWindow(LAYOUT=[1,3], YGAP=0.5, REFRESH=0)
	
	;BX Component
	p_bx1_gsm = MrPlot(t_fgm_ssm, b_gsm[0,*], $
	                   /CURRENT, $
	                   NAME        = 'Bx FGM', $
	                   TITLE       = 'MEC GSM Transformation Comparision', $
	                   XTICKFORMAT = '(a1)', $
	                   YTITLE      = 'B$\downX$!C(nT)')
	p_bx2_gsm = MrPlot(t_fgm_ssm, b_unh_gsm[0,*], $
	                   COLOR    = 'blue', $
	                   NAME     = 'Bx MEC', $
	                   OVERPLOT = p_bx1_gsm)
	
	;By Component
	p_by1_gsm = MrPlot(t_fgm_ssm, b_gsm[1,*], $
	                   /CURRENT, $
	                   NAME        = 'By FGM', $
	                   XTICKFORMAT = '(a1)', $
	                   YTITLE      = 'B$\downY$!C(nT)')
	p_by2_gsm = MrPlot(t_fgm_ssm, b_unh_gsm[1,*], $
	                   COLOR    = 'blue', $
	                   NAME     = 'By MEC', $
	                   OVERPLOT = p_by1_gsm)
	
	;Bz Component
	p_bz1_gsm = MrPlot(t_fgm_ssm, b_gsm[2,*], $
	                   /CURRENT, $
	                   NAME        = 'Bz FGM', $
	                   XTICKFORMAT = 'time_labels', $
	                   YTITLE      = 'B$\downZ$!C(nT)')
	p_bz2_gsm = MrPlot(t_fgm_ssm, b_unh_gsm[2,*], $
	                   COLOR    = 'blue', $
	                   NAME     = 'Bz MEC', $
	                   OVERPLOT = p_bz1_gsm)
	
	;Legend
	l = MrLegend(ALIGNMENT    = 'NE', $
	             LABEL        = ['FGM', 'QMEC'], $
	             POSITION     = [1.0, 1.0], $
	             /RELATIVE, $
	             SAMPLE_WIDTH = 0, $
	             TEXT_COLOR   = ['Black', 'Blue'], $
	             TARGET       = [p_bx1_gsm, p_bx2_gsm])
	
	;Update the windwo
	win_gsm -> Refresh
	
	return, [win_gse, win_gsm]
end
