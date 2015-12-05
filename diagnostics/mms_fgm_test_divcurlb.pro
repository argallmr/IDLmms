; docformat = 'rst'
;
; NAME:
;    mms_fgm_test_divcurlB
;
; PURPOSE:
;+
;   Compare the curlometer and reciprocal vector spacial gradient techniques.
;
; :Categories:
;    MMS, EDI, EDP, QL
;
; :Params:
;
; :Keywords:
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
;       2015/11/11  -   Written by Matthew Argall
;-
function mms_fgm_test_divcurlB, mode, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif

;Test intervals
;mms4 2015-10-01: 15:30 - 16:30
;mms3 2015-10-12: 14:00 - 15:30
;mms3 2015-10-13: 15:30 - 16:30
;mms3 2015-10-18: 12:45 - 15:00
;mms3 2015-10-30: 15:00 - 15:30

	mode     = 'brst'
	instr    = 'dfg'
	tstart   = '2015-10-16T13:06:00Z'
	tend     = '2015-10-16T13:08:00Z'

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Magnetic field
	;   - DMPA Coordinates
	mms_fgm_ql_read, 'mms1', instr, mode, 'l2pre', tstart, tend, B_GSE=b1, TIME=t1
	mms_fgm_ql_read, 'mms2', instr, mode, 'l2pre', tstart, tend, B_GSE=b2, TIME=t2
	mms_fgm_ql_read, 'mms3', instr, mode, 'l2pre', tstart, tend, B_GSE=b3, TIME=t3
	mms_fgm_ql_read, 'mms4', instr, mode, 'l2pre', tstart, tend, B_GSE=b4, TIME=t4

	;Remove the magntidue
	b1 = b1[0:2,*]
	b2 = b2[0:2,*]
	b3 = b3[0:2,*]
	b4 = b4[0:2,*]
	
	;FDOA Spacecraft Position
	;   - DMPA Coordinates
	r1 = mms_fdoa_scpos('mms1', tstart, tend, t1)
	r2 = mms_fdoa_scpos('mms2', tstart, tend, t1)
	r3 = mms_fdoa_scpos('mms3', tstart, tend, t1)
	r4 = mms_fdoa_scpos('mms4', tstart, tend, t1)

;-----------------------------------------------------
; Interpolate Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert time to seconds
	MrCDF_Epoch_Breakdown, t1[0], year, month, day
	t0 = MrCDF_Epoch_Compute(year, month, day, /TT2000)
	t1_sse = MrCDF_epoch2sse(t1, t0)
	t2_sse = MrCDF_epoch2sse(temporary(t2), t0)
	t3_sse = MrCDF_epoch2sse(temporary(t3), t0)
	t4_sse = MrCDF_epoch2sse(temporary(t4), t0)
	
	;interpolate B-fields
	b2 = MrInterpol(b2, temporary(t2_sse), t1_sse)
	b3 = MrInterpol(b3, temporary(t3_sse), t1_sse)
	b4 = MrInterpol(b4, temporary(t4_sse), temporary(t1_sse))

;-----------------------------------------------------
; Compute Curl & Divergence \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Curl via Reciprocal Vectors
	;   - 1e-12/mu0 converts 1/km * nT  --> A/m^2
	;   - 1e6 converts A/m^2 --> uA/m^2
	mu0 = constants('mu_0')
	Jrecip = (1e-6/mu0) * MrReciprocalCurl(r1, r2, r3, r4, b1, b2, b3, b4)

	;Curl Via Curlomter
	;   - Returns in units of uA/m^2
	Jcurl = MrCurlometer(r1, r2, r3, r4, b1, b2, b3, b4)
	
	;Divergence via Reciprocal Vectors
	;   - 1e-12/u0  converts 1/km nT --> A/m^2
	;   - 1e6 converts A/m^2 --> uA/m^2
	divB  = MrReciprocalDivergence(r1, r2, r3, r4, b1, b2, b3, b4)
	divB *= (1e-6/mu0)
	
	;Delete data
	r1 = (r2 = (r3 = (r4 = !Null)))
	b1 = (b2 = (b3 = (b4 = !Null)))

;-------------------------------------------------------
; Plot Data ////////////////////////////////////////////
;-------------------------------------------------------
	t_ssm = MrCDF_epoch2ssm(temporary(t1))
	xrange = hms_to_ssm(['13:06:40', '13:07:20'])
	yrange = [-2,2]
	
	;Jx
	pJx = Plot(t_ssm, Jrecip[0,*], $
	           LAYOUT      = [1,4,1], $
	           NAME        = '$J_{X}$', $
	           TITLE       = 'Curlometer vs. Reciiprocal Vectors', $
	           XRANGE      = xrange, $
	           XSTYLE      = 1, $
	           XTICKFORMAT = '(a1)', $
	           YRANGE      = yrange, $
	           YSTYLE      = 1, $
	           YTITLE      = '$J_{X}$!C($\muA/m$)')
	oJx = Plot(t_ssm, Jcurl[0,*], $
	           COLOR       = 'Blue', $
	           NAME        = '$J_{X,cmtr}$', $
	           OVERPLOT    = pJx)
	
	;Jy
	pJy = Plot(t_ssm, Jrecip[1,*], $
	           /CURRENT, $
	           LAYOUT      = [1,4,2], $
	           NAME        = '$J_{Y}$', $
	           XRANGE      = xrange, $
	           XSTYLE      = 1, $
	           XTICKFORMAT = '(a1)', $
	           YRANGE      = yrange, $
	           YSTYLE      = 1, $
	           YTITLE      = '$J_{Y}$!C($\uA/m$)')
	oJy = Plot(t_ssm, Jcurl[1,*], $
	           COLOR       = 'Blue', $
	           NAME        = '$J_{Y,cmtr}$', $
	           OVERPLOT    = pJy)
	
	;Jz
	pJz = Plot(t_ssm, Jrecip[2,*], $
	           /CURRENT, $
	           LAYOUT      = [1,4,3], $
	           NAME        = '$J_{Z}$', $
	           XRANGE      = xrange, $
	           XSTYLE      = 1, $
	           XTICKFORMAT = '(a1)', $
	           YRANGE      = yrange, $
	           YSTYLE      = 1, $
	           YTITLE      = '$J_{Z}$!C($\uA/m$)')
	oJz = Plot(t_ssm, Jcurl[2,*], $
	           COLOR       = 'Blue', $
	           NAME        = '$J_{Z,cmtr}$', $
	           OVERPLOT    = pJz)
	
	;DivB/mu0
	pdivB = Plot(t_ssm, divB, $
	             /CURRENT, $
	             LAYOUT      = [1,4,4], $
	             NAME        = 'DivB', $
	             XRANGE      = xrange, $
	             XSTYLE      = 1, $
	             XTICKFORMAT = 'time_labels', $
	             YRANGE      = yrange, $
	             YSTYLE      = 1, $
	             YTITLE      = '$\nabla \cdot B/\mu0$!C($\uA/m$)')
	
	;Legend
	gL = Legend(POSITION = [1.0, 1.0], $
	            /AUTO_TEXT_COLOR, $
	            /RELATIVE, $
	            SAMPLE_WIDTH = 0.0, $
	            TARGET       = [pJx, oJx])
	
	return, pJx.window
end
