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
;       2016/06/05  -   Written by Matthew Argall
;-
function mms_fsm_test_calhist
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif

	; Data file to plot
	file_dfg = '/nfs/fsm/temp/mms4_fsm_srvy_l2plus_cal-dfg_20151107153843_v0.0.0.cdf'
	file_scm = '/nfs/fsm/temp/mms4_fsm_srvy_l2plus_cal-scm_20151107153843_v0.0.0.cdf'
;	file_dfg = '/nfs/fsm/temp/mms1_fsm_brst_l2plus_cal-dfg_20151108133344_v0.0.0.cdf'
;	file_scm = '/nfs/fsm/temp/mms1_fsm_brst_l2plus_cal-scm_20151108133344_v0.0.0.cdf'
	iComp = 2
	iFlag = 3
	
	case iComp of
		0: comp = 'x'
		1: comp = 'y'
		2: comp = 'z'
		else: message, 'Invalid value for ICOMP'
	endcase
	
	case iFlag of
		0: region = 'slow-lo-pre'
		1: region = 'slow-hi'
		2: region = 'slow-lo-post'
		3: region = 'fast'
		else: message, 'Invalid value for ICOMP.'
	endcase
	
	png_file = '' ;'/home/argall/mms1_fsm_B' + comp + '_' + region + '_CalHist.png'

;------------------------------------;
; Get Data                           ;
;------------------------------------;

	; Parse file name to get variable name prefix and suffix
	mms_dissect_filename, file_dfg, SC=sc, INSTR=instr, MODE=mode, LEVEL=level
	prefix = sc + '_' + instr + '_'
	suffix = '_' + mode + '_' + level
	tf_brst = mode eq 'brst'

	; Create Variable Names
	fgm_amph_vname   = prefix + 'amp'   + '_hist_dfg'  + suffix
	fgm_ampf_vname   = prefix + 'amp'   + '_floor_dfg' + suffix
	fgm_phaseh_vname = prefix + 'phase' + '_hist_dfg'  + suffix
	fgm_phasef_vname = prefix + 'phase' + '_floor_dfg' + suffix
	fgm_psdh_vname   = prefix + 'psd'   + '_hist_dfg'  + suffix
	fgm_psdf_vname   = prefix + 'psd'   + '_floor_dfg' + suffix
	
	scm_amph_vname   = prefix + 'amp'   + '_hist_scm'  + suffix
	scm_ampf_vname   = prefix + 'amp'   + '_floor_scm' + suffix
	scm_phaseh_vname = prefix + 'phase' + '_hist_scm'  + suffix
	scm_phasef_vname = prefix + 'phase' + '_floor_scm' + suffix
	scm_psdh_vname   = prefix + 'psd'   + '_hist_scm'  + suffix
	scm_psdf_vname   = prefix + 'psd'   + '_floor_scm' + suffix
	
	gainh_vname   = prefix + 'gain'       + '_hist_scm'  + suffix
	gainf_vname   = prefix + 'gain'       + '_floor_scm' + suffix
	offseth_vname = prefix + 'phaseshift' + '_hist_scm'  + suffix
	offsetf_vname = prefix + 'phaseshift' + '_floor_scm' + suffix
	psdrath_vname = prefix + 'psdrat'     + '_hist_scm'  + suffix
	psdratf_vname = prefix + 'psdrat'     + '_floor_scm' + suffix
	
	;
	; Read data
	;

	;FGM
	amph_fgm   = MrCDF_Read(file_dfg, fgm_amph_vname, DEPEND_0=f_fgm, DEPEND_1=amp_bins, DEPEND_2=hflag, DEPEND_3=comp)
	ampf_fgm   = MrCDF_Read(file_dfg, fgm_ampf_vname)
	phaseh_fgm = MrCDF_Read(file_dfg, fgm_phaseh_vname, DEPEND_1=phase_bins)
	phasef_fgm = MrCDF_Read(file_dfg, fgm_phasef_vname)
	psdh_fgm   = MrCDF_Read(file_dfg, fgm_psdh_vname, DEPEND_1=psd_bins)
	psdf_fgm   = MrCDF_Read(file_dfg, fgm_psdf_vname)

	;SCM
	amph_scm   = MrCDF_Read(file_scm, scm_amph_vname, DEPEND_0=f_scm)
	ampf_scm   = MrCDF_Read(file_scm, scm_ampf_vname)
	phaseh_scm = MrCDF_Read(file_scm, scm_phaseh_vname)
	phasef_scm = MrCDF_Read(file_scm, scm_phasef_vname)
	psdh_scm   = MrCDF_Read(file_scm, scm_psdh_vname)
	psdf_scm   = MrCDF_Read(file_scm, scm_psdf_vname)

	;BOTH
;	gainh   = MrCDF_Read(file_scm, gainh_vname, DEPEND_1=gain_bins)
;	gainf   = MrCDF_Read(file_scm, gainf_vname)
;	offseth = MrCDF_Read(file_scm, offseth_vname, DEPEND_1=offset_bins)
;	offsetf = MrCDF_Read(file_scm, offsetf_vname)
;	psdrath = MrCDF_Read(file_scm, psdrath_vname, DEPEND_1=psdrat_bins)
;	psdratf = MrCDF_Read(file_scm, psdratf_vname)

	;Subset of data
	amph_fgm   = transpose( reform( amph_fgm[*, iFlag, iComp, *] ) )
	phaseh_fgm = transpose( reform( phaseh_fgm[*, iFlag, iComp, *] ) )
	psdh_fgm   = transpose( reform( psdh_fgm[*, iFlag, iComp, *] ) )
	ampf_fgm   = reform( ampf_fgm[iFlag, iComp, *] )
	phasef_fgm = reform( phasef_fgm[iFlag, iComp, *] )
	psdf_fgm   = reform( psdf_fgm[iFlag, iComp, *] )

	amph_scm   = transpose( reform( amph_scm[*, 0, iComp, *] ) )
	phaseh_scm = transpose( reform( phaseh_scm[*, 0, iComp, *] ) )
	psdh_scm   = transpose( reform( psdh_scm[*, 0, iComp, *] ) )
	ampf_scm   = reform( ampf_scm[0, iComp, *] )
	phasef_scm = reform( phasef_scm[0, iComp, *] )
	psdf_scm   = reform( psdf_scm[0, iComp, *] )

;	gainh   = transpose( reform( gainh[*, iFlag, iComp, *] ) )
;	offseth = transpose( reform( offseth[*, iFlag, iComp, *] ) )
;	psdrath = transpose( reform( psdrath[*, iFlag, iComp, *] ) )
;	gainf   = transpose( reform( gainf[iFlag, iComp, *, *] ) )
;	offsetf = transpose( reform( offsetf[iFlag, iComp, *, *] ) )
;	psdratf = transpose( reform( psdratf[iFlag, iComp, *, *] ) )

;------------------------------------;
; Plot Spectral Information          ;
;------------------------------------;
	win = MrWindow(LAYOUT=[2,3], OXMARGIN=[10,15], REFRESH=0, XGAP=20, XSIZE=1100, YGAP=0.5, YSIZE=900)

	;FGM AMP
	im_amph_fgm = MrImage( amph_fgm, f_fgm, amp_bins, $
	                       /AXES, $
	                       /CURRENT, $
	                       /LOG, $
	                       /SCALE, $
	                       NAME        = 'Hist Amp FGM', $
	                       TITLE       = 'FGM', $
	                       XTICKFORMAT = '(a1)', $
	                       YTITLE      = 'Log10( Amplitude )')
	p_ampf_fgm = MrPlot( f_fgm, ampf_fgm, $
	                     COLOR    = 'White', $
	                     NAME     = 'Floor Amp FGM', $
	                     OVERPLOT = im_amph_fgm )
	cb_amph_fgm = MrColorbar( ORIENTATION = 1, $
	                          NAME        = 'CB: Hist Amp FGM', $
	                          TARGET      = im_amph_fgm, $
	                          TITLE       = 'Occurrence')

	;SCM AMP
	im_amph_scm = MrImage( amph_scm, f_scm, amp_bins, $
	                       /AXES, $
	                       /CURRENT, $
	                       /LOG, $
	                       /SCALE, $
	                       NAME        = 'Hist Amp SCM', $
	                       TITLE       = 'SCM', $
	                       XTICKFORMAT = '(a1)')
	p_ampf_scm = MrPlot( f_scm, ampf_scm, $
	                     COLOR    = 'White', $
	                     NAME     = 'Floor Amp SCM', $
	                     OVERPLOT = im_amph_scm )
	cb_amph_scm = MrColorbar( ORIENTATION = 1, $
	                          NAME        = 'CB: Hist Amp SCM', $
	                          TARGET      = im_amph_scm, $
	                          TITLE       = 'Occurrence')

	;GAIN
;	im_gainh_scm = MrImage( gainh, f, gain_bins, $
;	                        /AXES, $
;	                        /CURRENT, $
;	                        /LOG, $
;	                        /SCALE, $
;	                        NAME        = 'Gain', $
;	                        TITLE       = 'Comparison', $
;	                        XTICKFORMAT = '(a1)', $
;	                        YTITLE      = 'Log10( Gain )')
;	p_gainf_scm = MrPlot( f, gainf, $
;	                      COLOR    = 'White', $
;	                      NAME     = 'Floor Gain', $
;	                      OVERPLOT = im_gainh_scm )
;	cb_gainh_scm = MrColorbar( ORIENTATION = 1, $
;	                           NAME        = 'CB: Gain', $
;	                           TARGET      = im_gainh_scm, $
;	                           TITLE       = 'Occurrence')

	;FGM PHASE
	im_phaseh_fgm = MrImage( phaseh_fgm, f_fgm, phase_bins, $
	                         /AXES, $
	                         /CURRENT, $
	                         /LOG, $
	                         /SCALE, $
	                         NAME        = 'Hist Phase FGM', $
	                         XTICKFORMAT = '(a1)', $
	                         YTITLE      = 'Phase')
	p_phasef_fgm = MrPlot( f_fgm, phasef_fgm, $
	                       COLOR    = 'White', $
	                       NAME     = 'Floor Phase FGM', $
	                       OVERPLOT = im_phaseh_fgm )
	cb_phaseh_fgm = MrColorbar( ORIENTATION = 1, $
	                            NAME        = 'CB: Hist Phase FGM', $
	                            TARGET      = im_phaseh_fgm, $
	                            TITLE       = 'Occurrence')

	;SCM PHASE
	im_phaseh_scm = MrImage( phaseh_scm, f_scm, phase_bins, $
	                         /AXES, $
	                         /CURRENT, $
	                         /LOG, $
	                         /SCALE, $
	                         NAME        = 'Hist Phase SCM', $
	                         XTICKFORMAT = '(a1)', $
	                         YTITLE      = 'Phase')
	p_phasef_scm = MrPlot( f_scm, phasef_scm, $
	                       COLOR    = 'White', $
	                       NAME     = 'Floor Phase SCM', $
	                       OVERPLOT = im_phaseh_scm )
	cb_phaseh_scm = MrColorbar( ORIENTATION = 1, $
	                            NAME        = 'CB: Hist Phase SCM', $
	                            TARGET      = im_phaseh_scm, $
	                            TITLE       = 'Occurrence')

	;PHASE OFFSET
;	im_offseth_scm = MrImage( offseth, f, offset_bins, $
;	                          /AXES, $
;	                          /CURRENT, $
;	                          /LOG, $
;	                          /SCALE, $
;	                          NAME        = 'Phase Shift', $
;	                          XTICKFORMAT = '(a1)', $
;	                          YTITLE      = 'Phase')
;	p_offsetf_scm = MrPlot( f, offsetf, $
;	                        COLOR    = 'White', $
;	                        NAME     = 'Floor Phase Shift', $
;	                        OVERPLOT = im_offseth_scm )
;	cb_offseth_scm = MrColorbar( ORIENTATION = 1, $
;	                             NAME        = 'CB: Phase Shift', $
;	                             TARGET      = im_offseth_scm, $
;	                             TITLE       = 'Occurrence')

	;FGM PSD
	im_psdh_fgm = MrImage( psdh_fgm, f_fgm, psd_bins, $
	                       /AXES, $
	                       /CURRENT, $
	                       /LOG, $
	                       /SCALE, $
	                       NAME        = 'Hist PSD FGM', $
	                       XTITLE      = 'Frequency', $
	                       YTITLE      = 'Log10( PSD )')
	p_psdf_fgm = MrPlot( f_fgm, psdf_fgm, $
	                     COLOR    = 'White', $
	                     NAME     = 'Floor PSD FGM', $
	                     OVERPLOT = im_psdh_fgm )
	cb_psdh_fgm = MrColorbar( ORIENTATION = 1, $
	                          NAME        = 'CB: Hist PSD FGM', $
	                          TARGET      = im_psdh_fgm, $
	                          TITLE       = 'Occurrence')

	;SCM PSD
	im_psdh_scm = MrImage( psdh_scm, f_scm, psd_bins, $
	                       /AXES, $
	                       /CURRENT, $
	                       /LOG, $
	                       /SCALE, $
	                       NAME        = 'Hist PSD SCM', $
	                       XTITLE      = 'Frequency', $
	                       YTITLE      = 'Log10( PSD )')
	p_psdf_scm = MrPlot( f_scm, psdf_scm, $
	                     COLOR    = 'White', $
	                     NAME     = 'Floor PSD SCM', $
	                     OVERPLOT = im_psdh_scm )
	cb_psdh_scm = MrColorbar( ORIENTATION = 1, $
	                          NAME        = 'CB: Hist PSD SCM', $
	                          TARGET      = im_psdh_scm, $
	                          TITLE       = 'Occurrence')

	;PSD RATIO
;	im_psdrath_scm = MrImage(psdrath, f, psdrat_bins, $
;	                         /AXES, $
;	                         /CURRENT, $
;	                         /LOG, $
;	                         /SCALE, $
;	                         NAME        = 'Hist PSD Ratio', $
;	                         XTITLE      = 'Frequency', $
;	                         YTITLE      = 'Log10( PSD Ratio )')
;	p_psdratf_scm = MrPlot(f, psdratf, $
;	                       COLOR    = 'White', $
;	                       NAME     = 'Floor PSD Ratio', $
;	                       OVERPLOT = im_psdrath_scm )
;	cb_psdrath_scm = MrColorbar(ORIENTATION = 1, $
;	                            NAME        = 'CB: Hist PSD Ratio', $
;	                            TARGET      = im_psdrath_scm, $
;	                            TITLE       = 'Occurrence')

;------------------------------------;
; 50% Area                           ;
;------------------------------------;

;	;Compute the relative area
;	nF       = n_elements(f)
;	nBins    = n_elements(amp_bins)
;	area     = amph_fgm * 0.1 ;rebin(reform(amp_bins, 1, nBins), nF, nBins)
;	area_rel = total(area, 2, /CUMULATIVE) / rebin(total(area, 2), nF, nBins)
;	
;	;Find the 50% mark
;	hmin = min( abs(area_rel - 0.5), ihmin, DIMENSION=2 )
;
;	;Convert to ND indices
;	inds = array_indices(area, ihmin)
;	icol = reform(inds[0,*])
;	irow = reform(inds[1,*])
;
;	h_floor = amp_bins[irow]
;
;	p = MrPlot(f, h_floor, $
;	           COLOR    = 'White', $
;	           NAME     = 'Test Floor', $
;	           OVERPLOT = im_amph_fgm)

;------------------------------------;
; Finish                             ;
;------------------------------------;
	;Update properties
	win -> SetGlobal, XTICKS=8
	
	;Refresh the window
	win -> Refresh
	
	;Save the graphic
	if png_file ne '' then win -> Save, png_file

	return, win
end
