; docformat = 'rst'
;
; NAME:
;    mms_fsm_plot_calhist
;
; PURPOSE:
;+
;   Plot FSM histogrammed calibration data.
;
; :Categories:
;    MMS, FSM, L2Plus
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
;       2016/08/31  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;
;   f1 = A0 * exp( -Z1^2 / 2 )      Z1 = (X - A1) / A2
;   f2 = A3 * exp( -Z2^2 / 2 )      Z2 = (X - A4) / A5
;   F  = f1 + f2
;
;   A0 = A3 = amplitude
;   A1 = A4 = mean
;   A2 = A5 = stddev
;-
pro MrCurveFit_BiGaussian, X, A, F, PDer
	compile_opt idl2
	on_error, 2
	
	;Exponent
	nPts = n_elements(x)
	Z1 = A[2] eq 0 ? replicate(10.0, nPts) : (X - A[1]) / A[2]
	Z2 = A[5] eq 0 ? replicate(10.0, nPts) : (X - A[4]) / A[5]
	
	;Exponential term
	EZ1 = exp( -Z1^2 / 2.0 )
	EZ2 = exp( -Z2^2 / 2.0 )
	
	;Function
	F = A[0]*EZ1 + A[3]*EZ2

	;Derivatives
	if arg_present(PDer) then begin
		;Allocate memory
		PDer = fltarr(n_elements(x), 6)
		
		;dF/d(A0 | A1 | A2)
		PDer[0,0] = EZ1
		if A[2] ne 0 then PDer[0,1] = A[0] * EZ1 * Z1 / A[2]
		PDer[0,2] = Z1 * PDer[*,1]
		
		;dF/d(A3 | A4 | A5)
		PDer[0,3] = EZ2
		if A[5] ne 0 then PDer[0,4] = A[3] * EZ2 * Z2 / A[5]
		PDer[0,5] = Z2 * PDer[*,4]
	endif
end


;+
;
;-
function mms_fsm_bkgd_fit_fgmfloor, sc, mode
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Inputs and constants
	theComp   = 'z'
	theFlag   = 1
	theF      = 0.1
	sc        = 'mms4'
	mode      = 'srvy'
	instr     = 'fsm'
	level     = 'l2plus'
	optdesc   = 'cal-dfg-week'
	tstart    = '2015-11-01T00:00:00'
	tend      = '2015-11-01T24:00:00'
	dropbox   = '/nfs/fsm/temp'
	sdc_root  = '/nfs/'
	timeorder = '%Y%M%d'
	png_file  = ''

;------------------------------------;
; Read Data                          ;
;------------------------------------;
	
	;Find files within time range
	cal_file = mms_find_file( sc, instr, mode, level, $
	                          COUNT          = nFiles, $
	                          DROPBOX        = dropbox, $
	                          OPTDESC        = optdesc, $
	                          SDC_ROOT       = sdc_root, $
	                          TIMEORDER      = timeorder, $
	                          TSTART         = tstart, $
	                          TEND           = tend )

;------------------------------------;
; Variable Names                     ;
;------------------------------------;

	;Variable name prefix and suffix
	prefix    = sc + '_' + instr + '_'
	suffix    = '_' + mode + '_' + level

	;Create Variable Names
	psd_hist_vname    = prefix + 'psd'   + '_hist'  + suffix

	;Read data
	psd_hist = MrCDF_Read( cal_file, psd_hist_vname, $
	                       DEPEND_0 = f, $
	                       DEPEND_1 = psd_bins, $
	                       DEPEND_2 = comp, $
	                       DEPEND_3 = flag )
	
	;Transpose data
	psd_hist = transpose(psd_hist, [3,0,2,1])

;------------------------------------;
; Variable Names                     ;
;------------------------------------;
	;Pick out the component
	case theComp of
		'x': iComp = 0
		'y': iComp = 1
		'z': iComp = 2
		else: message, 'COMP not recognized: "' + comp + "'.'
	endcase
	
	;Pick out the mode
	iFlag = where(flag eq theFlag, nFlag)
	if nFlag eq 0 then message, 'Invalid flag (' + strtrim(theFlag, 2) + ').'
	
	;Pick out the desired frequency
	iFreq = value_locate(f, theF)
	
	;Pick component
	psd_hist = psd_hist[*,*,iFlag,iComp]

;------------------------------------;
; Fit                                ;
;------------------------------------;
	;
	; Gaussian:
	;   f = amp / sqrt( 2 sigma^2 pi ) exp( -(x-mu)^2 / (2 sigma^2) )
	;
	
	;Initial conditions
	params     = [200.0, -7.0, 0.5, 500.0, -3.5, 0.75]
	nFreqs     = n_elements(f)
	floorPeak  = fltarr(nFreqs)
	signalPeak = fltarr(nFreqs)
	
	;Loop through each frequency.
	;   - Use parameters iteratively
	for i = 0, nFreqs-1 do begin
		fit = curvefit(psd_bins, reform(psd_hist[i,*]), !Null, params, FUNCTION_NAME='MrCurveFit_BiGaussian')
		floorPeak[i]  = params[1]
		signalPeak[i] = params[4]
	endfor
	
	;A single, demonstrative fit
	params = [200.0, -7.0, 0.5, 500.0, -3.5, 0.75]
	fit    = curvefit(psd_bins, reform(psd_hist[iFreq,*]), xxx, params, FUNCTION_NAME='MrCurveFit_BiGaussian')

;------------------------------------;
; Plot                               ;
;------------------------------------;
	win = MrWindow(OXMARGIN=[10,15], REFRESH=0, XGAP=0.5, XSIZE=800, YGAP=5, YSIZE=650)
	
	;Flags:
	;   0 - slow, lo, pre,  deck32
	;   1 - fast,           deck32
	;   2 - slow, hi,       deck32
	;   4 - slow, lo, post, deck32
	;   8 - slow, lo, pre,  deck64
	;   9 - fast            deck64
	;  10 - slow, hi,       deck64
	;  12 - slow, lo, post, deck64
	
	
	im_psd = MrImage( psd_hist, f, psd_bins, $
	                  /AXES, $
	                  /CURRENT, $
	                  /LOG, $
	                  /SCALE, $
	                  NAME        = 'PSD', $
	                  TITLE       = 'DFG Fast Deck32', $
	                  XTITLE      = 'f (Hz)', $
	                  YTITLE      = 'PSD!Clog(nT^2/Hz)' )
	cb_psd = MrColorbar( ORIENTATION = 1, $
	                     NAME        = 'CB: PSD', $
	                     TARGET      = im_psd, $
	                     TITLE       = 'Occurrence' )
	bar_psd = MrPlotS( [f[iFreq], f[iFreq]], [psd_bins[0], psd_bins[-1]], $
	                   /DATA, $
	                   LINESTYLE = 2, $
	                   TARGET    = im_psd )
	
	;CUT
	p_psd = MrPlot( psd_bins, psd_hist[iFreq,*], $
	                /CURRENT, $
	                XTITLE = 'PSD log(nT^2/Hz)', $
	                YTITLE = 'Occurrence' )
	
	;FIT
	p_fit = MrPlot( psd_bins, fit, $
	                COLOR    = 'Blue', $
	                OVERPLOT = p_psd )
	l_fit = MrLegend( ALIGNMENT    = 'NE', $
	                  POSITION     = [1.0, 1.0], $
	                  /RELATIVE, $
	                  LABEL        = ['Data', 'Bi-Gaussian'], $
	                  SAMPLE_WIDTH = 0.0, $
	                  TARGET       = p_fit, $
	                  TEXT_COLOR   = ['Black', 'Blue'] )
	
	;Results
	im_psd = MrImage( psd_hist, f, psd_bins, $
	                  /AXES, $
	                  /LOG, $
	                  /SCALE, $
	                  NAME        = 'PSD', $
	                  TITLE       = 'DFG Fast Deck32', $
	                  XTITLE      = 'f (Hz)', $
	                  YTITLE      = 'PSD!Clog(nT^2/Hz)' )
	im_psd -> Refresh, /DISABLE
	cb_psd = MrColorbar( ORIENTATION = 1, $
	                     NAME        = 'CB: PSD', $
	                     TARGET      = im_psd, $
	                     TITLE       = 'Occurrence' )
	
	;Overplot both fits
	p_psd = MrPlot( f, floorPeak, $
	                OVERPLOT = im_psd )
	p_psd = MrPlot( f, signalPeak, $
	                OVERPLOT = im_psd )

;------------------------------------;
; Finish                             ;
;------------------------------------;
	win -> SetGlobal, RANGE=[1,1000]

	;Refresh the window
	win    -> Refresh
	im_psd -> Refresh

	;Save the graphic
	if png_file ne '' then win -> Save, png_file

	return, win
end