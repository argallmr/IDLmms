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
;-
function mms_fsm_plot_calhist, sc, mode
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
; Noise Floor from SCM               ;
;------------------------------------;
	;Read the nosie floor data from LPP
	lpp_file = mode eq 'srvy' ? '/home/argall/data/lpp_scm/noise_mms4_z_201606_srvy.txt' : $
	                            '/home/argall/data/lpp_scm/noise_mms4_z_201606_brst.txt'
	lpp_data = MrFile_Read_Ascii( lpp_file, $
	                              NHEADER      = 1, $
	                              COLUMN_NAMES = ['f', 'Amp'], $
	                              COLUMN_TYPES = ['FLOAT', 'FLOAT'], $
	                              GROUPS       = [1, 2] )

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
	case theComp of
		'x': iComp = 0
		'y': iComp = 1
		'z': iComp = 2
		else: message, 'COMP not recognized: "' + comp + "'.'
	endcase

	;Variable name prefix and suffix
	prefix    = sc + '_' + instr + '_'
	suffix    = '_' + mode + '_' + level

	;Create Variable Names
	amp_hist_vname    = prefix + 'amp'   + '_hist'  + suffix
	phase_hist_vname  = prefix + 'phase' + '_hist'  + suffix
	psd_hist_vname    = prefix + 'psd'   + '_hist'  + suffix
	amp_floor_vname   = prefix + 'amp'   + '_floor' + suffix
	phase_floor_vname = prefix + 'phase' + '_floor' + suffix
	psd_floor_vname   = prefix + 'psd'   + '_floor' + suffix

	;Read data
	amp_hist    = MrCDF_Read(cal_file, amp_hist_vname,   DEPEND_0=f, DEPEND_1=amp_bins, DEPEND_2=comp, DEPEND_3=flag)
	phase_hist  = MrCDF_Read(cal_file, phase_hist_vname, DEPEND_1=phase_bins)
	psd_hist    = MrCDF_Read(cal_file, psd_hist_vname,   DEPEND_1=psd_bins)
	amp_floor   = MrCDF_Read(cal_file, amp_floor_vname)
	phase_floor = MrCDF_Read(cal_file, phase_floor_vname)
	psd_floor   = MrCDF_Read(cal_file, psd_floor_vname)
	
	;Transpose data
	amp_hist    = transpose(amp_hist,   [3,0,2,1])
	phase_hist  = transpose(phase_hist, [3,0,2,1])
	psd_hist    = transpose(psd_hist,   [3,0,2,1])
	amp_floor   = transpose(amp_floor,   [2,1,0])
	phase_floor = transpose(phase_floor, [2,1,0])
	psd_floor   = transpose(psd_floor,   [2,1,0])
	
	;Pick component
	amp_hist    = amp_hist[*,*,*,iComp]
	phase_hist  = phase_hist[*,*,*,iComp]
	psd_hist    = psd_hist[*,*,*,iComp]
	amp_floor   = amp_floor[*,*,iComp]
	phase_floor = phase_floor[*,*,iComp]
	psd_floor   = psd_floor[*,*,iComp]

;------------------------------------;
; Plot                               ;
;------------------------------------;
	win = MrWindow(LAYOUT=[2,4], OXMARGIN=[10,15], REFRESH=0, XGAP=0.5, XSIZE=900, YGAP=0.5, YSIZE=900)
	
	;Flags:
	;   0 - slow, lo, pre,  deck32
	;   1 - fast,           deck32
	;   2 - slow, hi,       deck32
	;   4 - slow, lo, post, deck32
	;   8 - slow, lo, pre,  deck64
	;   9 - fast            deck64
	;  10 - slow, hi,       deck64
	;  12 - slow, lo, post, deck64
	
	
	;PSD - 0 - DECK32
	im_psd = MrImage( psd_hist[*,*,0], f, psd_bins, $
	                  /AXES, $
	                  /CURRENT, $
	                  /LOG, $
	                  /SCALE, $
	                  NAME        = 'PSD 0', $
	                  TITLE       = 'Deck 32 (lo-pre, hi, lo-post, fast)', $
	                  XTICKFORMAT = '(a1)', $
	                  YTITLE      = 'PSD!Clog(nT^2/Hz)' )
	p_psd = MrPlot( f, psd_floor[*,0], $
	                COLOR    = 'Black', $
	                NAME     = 'PSD 0 Floor', $
	                OVERPLOT = im_psd )
	
	;PSD - 8 - DECK64
	im_psd = MrImage( psd_hist[*,*,4], f, psd_bins, $
	                  /AXES, $
	                  /CURRENT, $
	                  /LOG, $
	                  /SCALE, $
	                  NAME        = 'PSD 8', $
	                  TITLE       = 'Deck 64 (lo-pre, hi, lo-post, fast)', $
	                  XTICKFORMAT = '(a1)', $
	                  YTICKFORMAT = '(a1)' )
	p_psd = MrPlot( f, psd_floor[*,4], $
	                COLOR    = 'Black', $
	                NAME     = 'PSD 8 Floor', $
	                OVERPLOT = im_psd )
	cb_psd = MrColorbar( ORIENTATION = 1, $
	                     NAME        = 'CB: PSD 8', $
	                     TARGET      = im_psd, $
	                     TITLE       = 'Occurrence')
	
	;PSD - 2 - DECK32
	im_psd = MrImage( psd_hist[*,*,2], f, psd_bins, $
	                  /AXES, $
	                  /CURRENT, $
	                  /LOG, $
	                  /SCALE, $
	                  NAME        = 'PSD 2', $
	                  XTICKFORMAT = '(a1)', $
	                  YTITLE      = 'PSD!Clog(nT^2/Hz)' )
	p_psd = MrPlot( f, psd_floor[*,2], $
	                COLOR    = 'Black', $
	                NAME     = 'PSD 2 Floor', $
	                OVERPLOT = im_psd )
	
	;PSD - 10 - DECK64
	im_psd = MrImage( psd_hist[*,*,6], f, psd_bins, $
	                  /AXES, $
	                  /CURRENT, $
	                  /LOG, $
	                  /SCALE, $
	                  NAME        = 'PSD 10', $
	                  XTICKFORMAT = '(a1)', $
	                  YTICKFORMAT = '(a1)' )
	p_psd = MrPlot( f, psd_floor[*,6], $
	                COLOR    = 'Black', $
	                NAME     = 'PSD 10 Floor', $
	                OVERPLOT = im_psd )
	cb_psd = MrColorbar( ORIENTATION = 1, $
	                     NAME        = 'CB: PSD 10', $
	                     TARGET      = im_psd, $
	                     TITLE       = 'Occurrence')
	
	;PSD - 4 - DECK32
	im_psd = MrImage( psd_hist[*,*,3], f, psd_bins, $
	                  /AXES, $
	                  /CURRENT, $
	                  /LOG, $
	                  /SCALE, $
	                  NAME        = 'PSD 4', $
	                  XTICKFORMAT = '(a1)', $
	                  YTITLE      = 'PSD!Clog(nT^2/Hz)' )
	p_psd = MrPlot( f, psd_floor[*,3], $
	                COLOR    = 'Black', $
	                NAME     = 'PSD 4 Floor', $
	                OVERPLOT = im_psd )
	
	;PSD - 12 - DECK64
	im_psd = MrImage( psd_hist[*,*,7], f, psd_bins, $
	                  /AXES, $
	                  /CURRENT, $
	                  /LOG, $
	                  /SCALE, $
	                  NAME        = 'PSD 12', $
	                  XTICKFORMAT = '(a1)', $
	                  YTICKFORMAT = '(a1)' )
	p_psd = MrPlot( f, psd_floor[*,7], $
	                COLOR    = 'Black', $
	                NAME     = 'PSD 12 Floor', $
	                OVERPLOT = im_psd )
	cb_psd = MrColorbar( ORIENTATION = 1, $
	                     NAME        = 'CB: PSD 12', $
	                     TARGET      = im_psd, $
	                     TITLE       = 'Occurrence')
	
	;PSD - 1 - DECK32
	im_psd = MrImage( psd_hist[*,*,1], f, psd_bins, $
	                  /AXES, $
	                  /CURRENT, $
	                  /LOG, $
	                  /SCALE, $
	                  NAME        = 'PSD 1', $
	                  XTITLE      = 'f (Hz)', $
	                  YTITLE      = 'PSD!Clog(nT^2/Hz)' )
	p_psd = MrPlot( f, psd_floor[*,1], $
	                COLOR    = 'Black', $
	                NAME     = 'PSD 1 Floor', $
	                OVERPLOT = im_psd )
	
	;PSD - 9 - DECK64
	im_psd = MrImage( psd_hist[*,*,5], f, psd_bins, $
	                  /AXES, $
	                  /CURRENT, $
	                  /LOG, $
	                  /SCALE, $
	                  NAME        = 'PSD 9', $
	                  XTITLE      = 'f (Hz)', $
	                  YTICKFORMAT = '(a1)' )
	p_psd = MrPlot( f, psd_floor[*,5], $
	                COLOR    = 'Black', $
	                NAME     = 'PSD 9 Floor', $
	                OVERPLOT = im_psd )
	cb_psd = MrColorbar( ORIENTATION = 1, $
	                     NAME        = 'CB: PSD 9', $
	                     TARGET      = im_psd, $
	                     TITLE       = 'Occurrence')

;------------------------------------;
; Finish                             ;
;------------------------------------;
	win -> SetGlobal, RANGE=[1,1000]

	;Refresh the window
	win -> Refresh

	;Save the graphic
	if png_file ne '' then win -> Save, png_file

	return, win
end