; docformat = 'rst'
;
; NAME:
;    mms_edi_test_interp
;
; PURPOSE:
;+
;   Test different interpolation method for the magnetic field.
;
;   Results:
;       1) The difference between bulk spline interpolation and spline
;          interpolation in a sliding 15 second window is on the order
;          of floating poitn roundoff error.
;       2) The difference between bulk spline and bulk linear
;          interpolation is on the order of 0.01nT, and is more pronounced
;          in the spin-plane components than in the spin-axis component.
;
; :Categories:
;    MMS, QL
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
;       2015/10/18  -   Written by Matthew Argall
;-
function mms_edi_test_interp, sc, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		void = cgErrorMsg()
		return, !Null
	endif

	sc       = 'mms2'                 ;Slow                            ;Fast
	tstart   = '2015-05-09T16:08:00Z' ;01-Aug-2015 05:08:46.030963219  01-Aug-2015 00:41:57.83222159
	tend     = '2015-05-09T16:13:00Z' ;01-Aug-2015 13:19:18.340386093  01-Aug-2015 05:07:01.822075718
	edi_dir  = '/nfs/edi/temp/'
	sdc_dir  = '/nfs/'
	hk_dir   = '/nfs/hk/'
	max_tt_distance = long64(0.25 * 1d9)
	verbose = 0

;-------------------------------------------------------
; Find Files ///////////////////////////////////////////
;-------------------------------------------------------
	
	;EDI QL FAST file
	instr   = 'edi'
	mode    = 'fast'
	level   = 'l1a'
	optdesc = 'efield'
	ffast_edi = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfast_edi, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	
	;EDI QL SLOW file
	instr   = 'edi'
	mode    = 'slow'
	level   = 'l1a'
	optdesc = 'efield'
	fslow_edi = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nslow_edi, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	if nfast_edi + nslow_edi eq 0 then message, 'No EDI fast or slow survey files found.'
	
	;FG L1B file
	instr   = 'dfg'
	mode    = 'srvy'
	level   = 'l1b'
	optdesc = ''
	files_fg = mms_find_file(sc, instr, mode, level, $
	                         COUNT     = nfiles_fg, $
	                         OPTDESC   = optdesc, $
	                         SDC_ROOT  = sdc_dir, $
	                         SEARCHSTR = searchstr, $
	                         TSTART    = tstart, $
	                         TEND      = tend)
	if nfiles_fg eq 0 then message, 'No FG files found: "' + searchstr + '".'

;-------------------------------------------------------
; Read Files ///////////////////////////////////////////
;-------------------------------------------------------
	
	;FG
	fgm_l1b = mms_fg_read_l1b(files_fg, tstart, tend)
	
	;EDI Slow
	if nslow_edi gt 0 then begin
		edi_slow_l1b = mms_edi_create_l1b(fslow_edi, tstart, tend, $
		                                  /CS_123, $
		                                  /CS_BCS, $
		                                  QUALITY = quality)
	endif
	
	;EDI Fast
	if nfast_edi gt 0 then begin
		edi_fast_l1b = mms_edi_create_l1b(ffast_edi, tstart, tend, $
		                                  /CS_123, $
		                                  /CS_BCS, $
		                                  QUALITY = quality)
	endif
	
	;Comine slow and fast into survey
	edi_srvy_l1b = mms_edi_combine_srvy(temporary(edi_slow_l1b), temporary(edi_fast_l1b))

;-------------------------------------------------------
; Arrange Data /////////////////////////////////////////
;-------------------------------------------------------
	;Extract data
	t_fgm  = fgm_l1b.tt2000
	b_fgm  = fgm_l1b.b_bcs[0:2,*]
	t_gd12 = edi_srvy_l1b.tt2000_gd12
	t_gd21 = edi_srvy_l1b.tt2000_gd21
	
	;Delete extra data
	fg_l1b       = !Null
	edi_srvy_l1b = !Null
	
	;Combine GDU1 and GDU2 into the same array
	ngd12 = n_elements(t_gd12)
	ngd21 = n_elements(t_gd21)
	t_edi = [reform(t_gd12), reform(t_gd21)]
	gdu   = [lonarr(ngd12)+1, lonarr(ngd21)+2]

	;Sort and find uniq values
	isort = sort(t_edi)
	t_edi = t_edi[isort]
	gdu   = gdu[isort]

;-------------------------------------------------------
; Remove Data Gaps /////////////////////////////////////
;-------------------------------------------------------

	;
	; Check for gaps in magnetic field data: for each EDI data point
	; calculate which B data point is closest in time
	;
	;     Y    is the index of T_FG.
	;     YOUT is the fractional index within T_FG at which T_EDI is located.
	;
	; This is similar to using Value_Locate, except we will use the Round()
	; where as Value_Locate would use Floor().
	;
	t0     = t_fgm[0]
	ww_idx = interpol( dindgen(n_elements(t_fgm)), t_fgm-t0, t_edi-t0 )
	B_idx  = round(ww_idx)

	;If there is no magnetic field data within MAX_TT_DISTANCE nanoseconds
	;from an EDI data point, then remove the EDI data point.
	delta_tt = abs(t_edi - t_fgm[B_idx])
	ikeep    = where(delta_tt lt MAX_TT_DISTANCE, nkeep, $
	                 COMPLEMENT=iremove, NCOMPLEMENT=nremove)

	if nremove gt 0 and verbose then begin
		message, 'Removing EDI data points with no B data close-by: ' + $
		         strtrim(nremove, 2), /CONTINUE
	endif

	if nkeep lt 2 then begin
		msg = 'Less than 2 EDI data points after eliminating data in B gaps.'
		if verbose then message, msg, /cont
		return, { status:1, msg:msg }
	endif
	
	;Remove data within data gaps
	if nremove gt 0 then begin
		t_edi  = t_edi[ikeep]
		gdu    = gdu[ikeep]
		t_gd12 = t_edi[ where(gdu eq 1) ]
		t_gd21 = t_edi[ where(gdu eq 2) ]
	endif

;-------------------------------------------------------
; Interpolate //////////////////////////////////////////
;-------------------------------------------------------
	
	;Convert to seconds
	t0         = t_fgm[0]
	t_fgm_ssm  = MrCDF_epoch2ssm(t_fgm,  t0)
	t_edi_ssm  = MrCDF_epoch2ssm(t_edi,  t0)
	t_gd12_ssm = MrCDF_epoch2ssm(t_gd12, t0)
	t_gd21_ssm = MrCDF_epoch2ssm(t_gd21, t0)

	;Use sliding 15 second window
	b_avg = mms_edi_bavg(t_fgm, b_fgm, t_gd12, t_gd21, DT=dt)
	
	;Bulk inteprolate
	b_lin      = fltarr(3, n_elements(t_gd12_ssm))
	b_lin[0,*] = interpol(b_fgm[0,*], t_fgm_ssm, t_gd12_ssm)
	b_lin[1,*] = interpol(b_fgm[1,*], t_fgm_ssm, t_gd12_ssm)
	b_lin[2,*] = interpol(b_fgm[2,*], t_fgm_ssm, t_gd12_ssm)
	
	;Bulk spline interpolate
	b_spl      = fltarr(3, n_elements(t_gd12_ssm))
	b_spl[0,*] = spline(t_fgm_ssm, b_fgm[0,*], t_gd12_ssm, 0.01)
	b_spl[1,*] = spline(t_fgm_ssm, b_fgm[1,*], t_gd12_ssm, 0.01)
	b_spl[2,*] = spline(t_fgm_ssm, b_fgm[2,*], t_gd12_ssm, 0.01)

;-------------------------------------------------------
; View Results /////////////////////////////////////////
;-------------------------------------------------------
	win = MrWindow(NAME='EDI Interp', YSIZE=690, YGAP=0.5, REFRESH=0)
	
	;Bx
	pBx     = MrPlot(t_fgm_ssm, b_fgm[0,*], $
	                 /CURRENT, $
	                 NAME        = 'Bx', $
	                 TITLE       = 'Interpolation Test', $
	                 XTICKFORMAT = '(a1)', $
	                 YTITLE      = 'Bx (nT)')
	pBx_avg = MrPlot(t_gd12_ssm, b_avg.b_gd12[0,*], $
	                 COLOR       = 'Blue', $
	                 NAME        = 'Bx Avg', $
	                 OVERPLOT    = pBx, $
	                 PSYM        = 6, $
	                 XTICKFORMAT = '(a1)')
	pBx_lin = MrPlot(t_gd12_ssm, b_lin[0,*], $
	                 COLOR       = 'Forest Green', $
	                 NAME        = 'Bx Linear', $
	                 OVERPLOT    = pBx, $
	                 PSYM        = 4)
	pBx_spl = MrPlot(t_gd12_ssm, b_spl[0,*], $
	                 COLOR       = 'Red', $
	                 NAME        = 'Bx Spline', $
	                 OVERPLOT    = pBx, $
	                 PSYM        = 5)
	
	;By
	pBy     = MrPlot(t_fgm_ssm, b_fgm[1,*], $
	                 /CURRENT, $
	                 NAME        = 'By', $
	                 XTICKFORMAT = '(a1)', $
	                 YTITLE      = 'Bx (nT)')
	pBy_avg = MrPlot(t_gd12_ssm, b_avg.b_gd12[1,*], $
	                 COLOR       = 'Blue', $
	                 NAME        = 'By Avg', $
	                 OVERPLOT    = pBy, $
	                 PSYM        = 6, $
	                 XTICKFORMAT = '(a1)')
	pBy_lin = MrPlot(t_gd12_ssm, b_lin[1,*], $
	                 COLOR       = 'Forest Green', $
	                 NAME        = 'By Linear', $
	                 OVERPLOT    = pBy, $
	                 PSYM        = 4)
	pBy_spl = MrPlot(t_gd12_ssm, b_spl[1,*], $
	                 COLOR       = 'Red', $
	                 NAME        = 'By Spline', $
	                 OVERPLOT    = pBy, $
	                 PSYM        = 5)
	
	;Bz
	pBz     = MrPlot(t_fgm_ssm, b_fgm[2,*], $
	                 /CURRENT, $
	                 NAME        = 'Bz', $
	                 XTICKFORMAT = '(a1)', $
	                 YTITLE      = 'Bz (nT)')
	pBz_avg = MrPlot(t_gd12_ssm, b_avg.b_gd12[2,*], $
	                 COLOR       = 'Blue', $
	                 NAME        = 'Bz Avg', $
	                 OVERPLOT    = pBz, $
	                 PSYM        = 6)
	pBz_lin = MrPlot(t_gd12_ssm, b_lin[2,*], $
	                 COLOR       = 'Forest Green', $
	                 NAME        = 'Bz Linear', $
	                 OVERPLOT    = pBz, $
	                 PSYM        = 4)
	pBz_spl = MrPlot(t_gd12_ssm, b_spl[2,*], $
	                 COLOR       = 'Red', $
	                 NAME        = 'Bz Spline', $
	                 OVERPLOT    = pBz, $
	                 PSYM        = 5)
	
	;Difference in splin interpolations
	pdb     = MrPlot(t_gd12_ssm, b_spl - b_avg.b_gd12, $
	                 /CURRENT, $
	                 DIMENSION   = 2, $
	                 NAME        = 'Bz', $
	                 XTICKFORMAT = '(a1)', $
	                 YTITLE      = 'dB$\downSpl$')
	
	;Difference between linear and spline interpolations
	pdb     = MrPlot(t_gd12_ssm, (b_spl - b_lin) / b, $
	                 /CURRENT, $
	                 DIMENSION   = 2, $
	                 NAME        = 'Bz', $
	                 XTICKFORMAT = 'time_labels', $
	                 YTITLE      = 'B$\downSpl$-B$\downLin$')



	win -> Refresh
	return, win
end
