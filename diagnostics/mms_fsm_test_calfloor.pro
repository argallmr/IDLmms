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
function mms_fsm_test_calfloor
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win)      then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif

	; Data file to plot
	fgm_file = '/nfs/fsm/temp/mms4_fsm_srvy_l2plus_cal-dfg_20151107153843_v0.0.0.cdf'
	scm_file = '/nfs/fsm/temp/mms4_fsm_srvy_l2plus_cal-scm_20151107153843_v0.0.0.cdf'
	iComp    = 2
	
	case iComp of
		0: comp = 'x'
		1: comp = 'y'
		2: comp = 'z'
		else: message, 'Invalid value for ICOMP'
	endcase
	png_file = ''; '/home/argall/mms1_fsm_B' + comp + '_CalFloor.png'

;------------------------------------;
; Get Data                           ;
;------------------------------------;

	; Parse file name to get variable name prefix and suffix
	mms_dissect_filename, fgm_file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level
	prefix = sc + '_' + instr + '_'
	suffix = '_' + mode + '_' + level

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

	;
	; Read data
	;
	
	;FGM
	ampf_fgm   = MrCDF_Read(fgm_file, fgm_ampf_vname,   DEPEND_0=f_fgm, DEPEND_1=hist_flag_fgm)
	phasef_fgm = MrCDF_Read(fgm_file, fgm_phasef_vname)
	psdf_fgm   = MrCDF_Read(fgm_file, fgm_psdf_vname)

	;SCM
	ampf_scm   = MrCDF_Read(scm_file, scm_ampf_vname, DEPEND_0=f_scm, DEPEND_1=hist_flag_scm)
	phasef_scm = MrCDF_Read(scm_file, scm_phasef_vname)
	psdf_scm   = MrCDF_Read(scm_file, scm_psdf_vname)

	;Subset of data
	ampf_fgm   = reform( ampf_fgm[*, iComp, *] )
	phasef_fgm = reform( phasef_fgm[*, iComp, *] )
	psdf_fgm   = reform( psdf_fgm[*, iComp, *] )
	
	ampf_scm   = reform( ampf_scm[*, iComp, *] )
	phasef_scm = reform( phasef_scm[*, iComp, *] )
	psdf_scm   = reform( psdf_scm[*, iComp, *] )

;------------------------------------;
; Noise Floor from SCM               ;
;------------------------------------;
	;Read the nosie floor data from LPP
	lpp_file = '/home/argall/data/lpp_scm/noise_mms4_z_201606_srvy.txt'
	lpp_data = MrFile_Read_Ascii( lpp_file, $
	                              NHEADER      = 1, $
	                              COLUMN_NAMES = ['f', 'Amp'], $
	                              COLUMN_TYPES = ['FLOAT', 'FLOAT'], $
	                              GROUPS       = [1, 2] )

;------------------------------------;
; Plot Spectral Information          ;
;------------------------------------;
	win = MrWindow(LAYOUT=[2,3], OXMARGIN=[10,15], REFRESH=0, XGAP=10, XSIZE=1000, YSIZE=900)

	colors = ['Red', 'Blue', 'Forest Green', 'Black']

	;FGM AMP
	p_ampf_fgm = MrPlot( f_fgm, ampf_fgm, $
	                     /CURRENT, $
	                     COLOR     = colors, $
	                     DIMENSION = 2, $
	                     NAME      = 'Floor Amp FGM', $
	                     TITLE     = 'FGM', $
	                     XTITLE    = 'frequency', $
	                     XLOG      = 1, $
	                     XRANGE    = [f_fgm[1], f_fgm[-1]], $
	                     YTITLE    = 'Log10( Amp )' )
	l_ampf_fgm = MrLegend( ALIGNMENT    ='NW', $
	                       LABEL        = string(hist_flag_fgm, FORMAT='(i0)'), $
	                       NAME         = 'LGD: Floor Amp FGM', $
	                       POSITION     = [1.0, 1.0], $
	                       /RELATIVE, $
	                       SAMPLE_WIDTH = 0.0, $
	                       TARGET       = p_ampf_fgm, $
	                       TEXT_COLOR   = colors)

	;SCM AMP
	p_ampf_scm = MrPlot( f_scm, ampf_scm, $
	                     /CURRENT, $
;	                     COLOR     = colors, $
;	                     DIMENSION = 2, $
	                     NAME      = 'Floor Amp SCM', $
	                     TITLE     = 'SCM', $
	                     XTITLE    = 'frequency', $
	                     XLOG      = 1, $
	                     XRANGE    = [f_scm[1], f_scm[-1]], $
	                     YTITLE    = 'Log10( Amp )' )
	l_ampf_scm = MrLegend( ALIGNMENT    ='NW', $
	                       LABEL        = string(hist_flag_scm, FORMAT='(i0)'), $
	                       NAME         = 'LGD: Floor Amp SCM', $
	                       POSITION     = [1.0, 1.0], $
	                       /RELATIVE, $
	                       SAMPLE_WIDTH = 0.0, $
	                       TARGET       = p_ampf_scm, $
	                       TEXT_COLOR   = colors[-1])

	;FGM PHASE
	p_phasef_fgm = MrPlot( f_fgm, phasef_fgm, $
	                       /CURRENT, $
	                       COLOR     = colors, $
	                       DIMENSION = 2, $
	                       NAME      = 'Floor Phase FGM', $
	                       XTITLE    = 'frequency', $
	                       YTITLE    = 'Phase' )
	l_phasef_fgm = MrLegend( ALIGNMENT    ='NW', $
	                         LABEL        = string(hist_flag_fgm, FORMAT='(i0)'), $
	                         NAME         = 'LGD: Floor Amp FGM', $
	                         POSITION     = [1.0, 1.0], $
	                         /RELATIVE, $
	                         SAMPLE_WIDTH = 0.0, $
	                         TARGET       = p_phasef_fgm, $
	                         TEXT_COLOR   = colors)

	;SCM PHASE
	p_phasef_scm = MrPlot( f_scm, phasef_scm, $
	                       /CURRENT, $
;	                       COLOR     = colors, $
;	                       DIMENSION = 2, $
	                       NAME      = 'Floor Phase SCM', $
	                       XTITLE    = 'frequency', $
	                       YTITLE    = 'Phase' )
	l_phasef_scm = MrLegend( ALIGNMENT    ='NW', $
	                         LABEL        = string(hist_flag_scm, FORMAT='(i0)'), $
	                         NAME         = 'LGD: Floor Amp SCM', $
	                         POSITION     = [1.0, 1.0], $
	                         /RELATIVE, $
	                         SAMPLE_WIDTH = 0.0, $
	                         TARGET       = p_phasef_scm, $
	                         TEXT_COLOR   = colors[-1])

	;FGM PSD
	p_psdf_fgm = MrPlot( f_fgm, psdf_fgm, $
	                     /CURRENT, $
	                     COLOR     = colors, $
	                     DIMENSION = 2, $
	                     NAME      = 'Floor PSD FGM', $
	                     XTITLE    = 'frequency', $
	                     XLOG      = 1, $
	                     XRANGE    = [f_fgm[1], f_fgm[-1]], $
	                     YTITLE    = 'Log10( PSD )' )
	l_psdf_fgm = MrLegend( ALIGNMENT    ='NW', $
	                       LABEL        = string(hist_flag_fgm, FORMAT='(i0)'), $
	                       NAME         = 'LGD: Floor PSD FGM', $
	                       POSITION     = [1.0, 1.0], $
	                       /RELATIVE, $
	                       SAMPLE_WIDTH = 0.0, $
	                       TARGET       = p_psdf_fgm, $
	                       TEXT_COLOR   = colors)

	;SCM PSD
	p_psdf_scm = MrPlot( f_scm, psdf_scm, $
	                     /CURRENT, $
;	                     COLOR     = colors, $
;	                     DIMENSION = 2, $
	                     NAME      = 'Floor PSD SCM', $
	                     XTITLE    = 'frequency', $
	                     XLOG      = 1, $
	                     XRANGE    = [f_scm[1], f_scm[-1]], $
	                     YTITLE    = 'PSD!CnT/sqrt(Hz)' )
	p_ampf_lpp = MrPlot( lpp_data.f, MrLog(lpp_data.amp), $
	                     COLOR     = 'Blue', $
	                     LINESTYLE = 2, $
	                     OVERPLOT  = p_psdf_scm, $
	                     PSYM      = 17 )
	l_psdf_scm = MrLegend( ALIGNMENT    ='NW', $
	                       LABEL        = string(hist_flag_scm, FORMAT='(i0)'), $
	                       NAME         = 'LGD: Floor PSD SCM', $
	                       POSITION     = [1.0, 1.0], $
	                       /RELATIVE, $
	                       SAMPLE_WIDTH = 0.0, $
	                       TARGET       = p_psdf_scm, $
	                       TEXT_COLOR   = colors[-1])

;------------------------------------;
; Finish                             ;
;------------------------------------;
	;Refresh the window
	win -> Refresh
	
	;Save the graphic
	if png_file ne '' then win -> Save, png_file
	
	;Return the graphics
	return, win
end