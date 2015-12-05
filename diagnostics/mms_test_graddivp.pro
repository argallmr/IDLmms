; docformat = 'rst'
;
; NAME:
;    mms_edi_compare_ql
;
; PURPOSE:
;+
;   Compare test divergence and gradient of the pressure tensor.
;
;   NOTE::
;       1. Using ion moments results in a two orders of magnitude difference.
;       2. Using FDOA data instead of MEC results in miniscule differences.
;       3. Substituing the tensor
;                  | p 0 0 |
;              P = | 0 p 0 |
;                  | 0 0 p |
;          for the symmetric pressure tensor produces the same results as
;          grad(p). That is to say
;              grad(p) = div( I*p )
;          where I is the identity matrix and p is the scalar pressure tensor.
;
; :Categories:
;    MMS, FPI, Diagnostics, Spacial Gradients
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
;       2015/11/29  -   Written by Matthew Argall
;-
function mms_test_graddivP, mode, tstart, tend
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

	instr    = 'dfg'
	tstart   = '2015-10-16T13:06:00Z'
	tend     = '2015-10-16T13:08:00Z'

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Pressure and Density
	;   - Returns data in GSE
	mms_fpi_l1b_moms_read, 'mms1', 'des-moms', tstart, tend, N=n1, P_GSE=p1, TIME=t1_fpi
	mms_fpi_l1b_moms_read, 'mms2', 'des-moms', tstart, tend, N=n2, P_GSE=p2, TIME=t2_fpi
	mms_fpi_l1b_moms_read, 'mms3', 'des-moms', tstart, tend, N=n3, P_GSE=p3, TIME=t3_fpi
	mms_fpi_l1b_moms_read, 'mms4', 'des-moms', tstart, tend, N=n4, P_GSE=p4, TIME=t4_fpi
	
	;MEC Spacecraft Position
	mms_mec_read, 'mms1', 'epht89d', tstart, tend, R_GSE=r1, TIME=t1_mec
	mms_mec_read, 'mms2', 'epht89d', tstart, tend, R_GSE=r2, TIME=t2_mec
	mms_mec_read, 'mms3', 'epht89d', tstart, tend, R_GSE=r3, TIME=t3_mec
	mms_mec_read, 'mms4', 'epht89d', tstart, tend, R_GSE=r4, TIME=t4_mec
	
	;FDOA Spacecraft Position
	;   - GSE Coordinates
;	r1 = mms_fdoa_scpos('mms1', tstart, tend, t1_fpi)
;	r2 = mms_fdoa_scpos('mms2', tstart, tend, t1_fpi)
;	r3 = mms_fdoa_scpos('mms3', tstart, tend, t1_fpi)
;	r4 = mms_fdoa_scpos('mms4', tstart, tend, t1_fpi)

;-----------------------------------------------------
; Interpolate Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert time to seconds
	t0         = t1_fpi[0]
	t1_fpi_ssm = MrCDF_epoch2ssm(temporary(t1_fpi), t0)
	t2_fpi_ssm = MrCDF_epoch2ssm(temporary(t2_fpi), t0)
	t3_fpi_ssm = MrCDF_epoch2ssm(temporary(t3_fpi), t0)
	t4_fpi_ssm = MrCDF_epoch2ssm(temporary(t4_fpi), t0)
	t1_mec_ssm = MrCDF_epoch2ssm(temporary(t1_mec), t0)
	t2_mec_ssm = MrCDF_epoch2ssm(temporary(t2_mec), t0)
	t3_mec_ssm = MrCDF_epoch2ssm(temporary(t3_mec), t0)
	t4_mec_ssm = MrCDF_epoch2ssm(temporary(t4_mec), t0)
	
	;Interpolate density, pressure-fields
	r1 = MrInterpol(r1, temporary(t1_mec_ssm), t1_fpi_ssm)
	r2 = MrInterpol(r2, temporary(t2_mec_ssm), t1_fpi_ssm)
	r3 = MrInterpol(r3, temporary(t3_mec_ssm), t1_fpi_ssm)
	r4 = MrInterpol(r4, temporary(t4_mec_ssm), t1_fpi_ssm)
	
	n2 = MrInterpol(n2, t2_fpi_ssm, t1_fpi_ssm)
	n3 = MrInterpol(n3, t3_fpi_ssm, t1_fpi_ssm)
	n4 = MrInterpol(n4, t3_fpi_ssm, t1_fpi_ssm)
	
	p2 = MrInterpol(p2, temporary(t2_fpi_ssm), t1_fpi_ssm)
	p3 = MrInterpol(p3, temporary(t3_fpi_ssm), t1_fpi_ssm)
	p4 = MrInterpol(p4, temporary(t4_fpi_ssm), t1_fpi_ssm)
	
	;Scalar pressure
	sp1 = (p1[0,*] + p1[3,*] + p1[5,*]) / 3.0
	sp2 = (p2[0,*] + p2[3,*] + p2[5,*]) / 3.0
	sp3 = (p3[0,*] + p3[3,*] + p3[5,*]) / 3.0
	sp4 = (p4[0,*] + p4[3,*] + p4[5,*]) / 3.0
	
	;Total number of points
	npts = n_elements(t1_fpi_ssm)

;-----------------------------------------------------
; Compute Curl & Divergence \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Charge
	q = constants('q')
	
	;Average density
	n = (n1 + n2 + n3 + n4) / 4.0

	;Curl via Reciprocal Vectors
	;   - 1e-12/mu0 converts 1/km * nT  --> A/m^2
	;   - 1e6 converts A/m^2 --> uA/m^2
	divP  = MrReciprocalDivergence(r1, r2, r3, r4, p1, p2, p3, p4)
	divP *= (1e-15 / (q * rebin(n, 3, npts)))

	;Curl Via Curlomter
	;   - Returns in units of uA/m^2
	gradP  = MrReciprocalGradient(r1, r2, r3, r4, sp1, sp2, sp3, sp4)
	gradP *= (1e-15 / (q * rebin(n, 3, npts)))

;-------------------------------------------------------
; Plot Data ////////////////////////////////////////////
;-------------------------------------------------------
	win    = MrWindow(LAYOUT=[1,3], YGAP=0.5, REFRESH=0)
	xrange = hms_to_ssm(['13:06:40', '13:07:20'])
	
	pxrange = [min( [divP[0,*], gradP[0,*]], MAX=pmax ), pmax]
	pyrange = [min( [divP[1,*], gradP[1,*]], MAX=pmax ), pmax]
	pzrange = [min( [divP[2,*], gradP[2,*]], MAX=pmax ), pmax]
	
	;Ex
	gPx = MrPlot(t1_fpi_ssm, divP[0,*], $
	             /CURRENT, $
	             NAME        = 'divPx', $
	             TITLE       = 'div(Pe) vs. grad(pe)', $
	             XRANGE      = xrange, $
	             XSTYLE      = 1, $
	             XTICKFORMAT = '(a1)', $
	             YRANGE      = pxrange, $
	             YSTYLE      = 1, $
	             YTITLE      = 'divP$\downX$!C(mV/m)')
	oPx = MrPlot(t1_fpi_ssm, gradP[0,*], $
	             COLOR       = 'Blue', $
	             NAME        = 'gradPx', $
	             OVERPLOT    = gPx)
	
	;Ey
	gPy = MrPlot(t1_fpi_ssm, divP[1,*], $
	             /CURRENT, $
	             NAME        = 'divPy', $
	             XRANGE      = xrange, $
	             XSTYLE      = 1, $
	             XTICKFORMAT = '(a1)', $
	             YRANGE      = pyrange, $
	             YSTYLE      = 1, $
	             YTITLE      = 'divP$\downY$!C(mV/m)')
	oPy = MrPlot(t1_fpi_ssm, gradP[1,*], $
	             COLOR       = 'Blue', $
	             NAME        = 'gradPy', $
	             OVERPLOT    = gPy)
	
	;Ez
	gPz = MrPlot(t1_fpi_ssm, divP[2,*], $
	             /CURRENT, $
	             NAME        = 'divPz', $
	             XRANGE      = xrange, $
	             XSTYLE      = 1, $
	             XTICKFORMAT = 'time_labels', $
	             YRANGE      = pzrange, $
	             YSTYLE      = 1, $
	             YTITLE      = 'divP$\downZ$!C(mV/m)')
	oPz = MrPlot(t1_fpi_ssm, gradP[2,*], $
	             COLOR       = 'Blue', $
	             NAME        = 'gradPz', $
	             OVERPLOT    = gPz)
	
	;Legend
	gL = MRLegend(ALIGNMENT    = 'NE', $
	              POSITION     = [1.0, 1.0], $
	              /AUTO_TEXT_COLOR, $
	              /RELATIVE, $
	              SAMPLE_WIDTH = 0.0, $
	              TARGET       = [gPx, oPx])
	
	win -> Refresh
	return, win
end
