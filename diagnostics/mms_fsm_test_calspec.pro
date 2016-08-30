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
function mms_fsm_test_calspec
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win)      then obj_destroy, win
		if obj_valid(hist_win) then obj_destroy, hist_win
		MrPrintF, 'LogErr'
		return, !Null
	endif

	; Data file to plot
	dfg_file = '/nfs/fsm/temp/mms4_fsm_srvy_l2plus_cal-dfg_20151107153843_v0.0.0.cdf'
	scm_file = '/nfs/fsm/temp/mms4_fsm_srvy_l2plus_cal-scm_20151107153843_v0.0.0.cdf'
	iComp = 2
	
	case iComp of
		0: comp = 'x'
		1: comp = 'y'
		2: comp = 'z'
		else: message, 'Invalid value for ICOMP'
	endcase
	png_file = '' ;'/home/argall/mms1_fsm_B' + comp + '_CalSpec.png'

;------------------------------------;
; Get Data                           ;
;------------------------------------;

	; Parse file name to get variable name prefix and suffix
	mms_dissect_filename, dfg_file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level
	prefix = sc + '_' + instr + '_'
	suffix = '_' + mode + '_' + level

	; Create Variable Names
	fgm_amp_vname   = prefix + 'amp'        + '_omb_dfg' + suffix
	fgm_phase_vname = prefix + 'phase'      + '_omb_dfg' + suffix
	fgm_psd_vname   = prefix + 'psd'        + '_omb_dfg' + suffix
	scm_amp_vname   = prefix + 'amp'        + '_omb_scm' + suffix
	scm_phase_vname = prefix + 'phase'      + '_omb_scm' + suffix
	scm_psd_vname   = prefix + 'psd'        + '_omb_scm' + suffix
	gain_vname      = prefix + 'gain'       + '_omb_scm' + suffix
	offset_vname    = prefix + 'phaseshift' + '_omb_scm' + suffix
	psdrat_vname    = prefix + 'psdrat'     + '_omb_scm' + suffix
	flag_vname      = prefix + 'flag'                    + suffix

	; Read data
	amp_fgm   = MrCDF_Read(dfg_file, fgm_amp_vname, DEPEND_0=t_fgm, DEPEND_1=f_fgm)
	phase_fgm = MrCDF_Read(dfg_file, fgm_phase_vname)
	psd_fgm   = MrCDF_Read(dfg_file, fgm_psd_vname)
	amp_scm   = MrCDF_Read(scm_file, scm_amp_vname, DEPEND_0=t_scm, DEPEND_1=f_scm)
	phase_scm = MrCDF_Read(scm_file, scm_phase_vname)
	psd_scm   = MrCDF_Read(scm_file, scm_psd_vname)
;	gain      = MrCDF_Read(scm_file, gain_vname)
;	offset    = MrCDF_Read(scm_file, offset_vname)
;	psdrat    = MrCDF_Read(scm_file, psdrat_vname)
;	t         = MrCDF_Read(scm_file, 'Epoch')

	;Trim
	t_fgm = MrCDF_epoch2sse(t_fgm)
	t_scm = MrCDF_epoch2sse(t_scm)
	amp_fgm   = transpose( reform( amp_fgm[*, iComp, *]   ) )
	phase_fgm = transpose( reform( phase_fgm[*, iComp, *] ) )
	psd_fgm   = transpose( reform( psd_fgm[*, iComp, *]   ) )
	amp_scm   = transpose( reform( amp_scm[*, iComp, *]   ) )
	phase_scm = transpose( reform( phase_scm[*, iComp, *] ) )
	psd_scm   = transpose( reform( psd_scm[*, iComp, *]   ) )
;	gain      = transpose( reform( gain[*, iComp, *]      ) )
;	offset    = transpose( reform( offset[*, iComp, *]    ) )
;	psdrat    = transpose( reform( psdrat[*, iComp, *]    ) )

;------------------------------------;
; Plot Spectral Information          ;
;------------------------------------;
	win = MrWindow(LAYOUT=[2,3], OXMARGIN=[10,15], REFRESH=0, XGAP=20, XSIZE=1100, YGAP=0.5, YSIZE=900)

	;FGM AMP
	im_amp_fgm = MrImage( amp_fgm, t_fgm, f_fgm, $
	                      /AXES, $
	                      /CURRENT, $
	                      /LOG, $
	                      /SCALE, $
	                      NAME        = 'FGM AMP', $
	                      TITLE       = 'FGM', $
	                      XTICKFORMAT = '(a1)', $
	                      YRANGE      = [f_fgm[1], f_fgm[-1]], $
	                      YTITLE      = 'Frequency')
	cb_amp_fgm = MrColorbar(ORIENTATION = 1, $
	                        NAME        = 'CB: FGM AMP', $
	                        TARGET      = im_amp_fgm, $
	                        TITLE       = 'Amplitude!C(nT)')

	;SCM AMP
	im_amp_scm = MrImage( amp_scm, t_scm, f_scm, $
	                      /AXES, $
	                      /CURRENT, $
	                      /LOG, $
	                      /SCALE, $
	                      NAME        = 'SCM AMP', $
	                      TITLE       = 'SCM', $
	                      XTICKFORMAT = '(a1)', $
	                      YRANGE      = [f_scm[1], f_scm[-1]])
	cb_amp_scm = MrColorbar(ORIENTATION = 1, $
	                        NAME        = 'CB: SCM AMP', $
	                        TARGET      = im_amp_scm, $
	                        TITLE       = 'Amplitude!C(nT)')

	;GAIN
;	im_gain = MrImage( gain, t, f, $
;	                   /AXES, $
;	                   /CURRENT, $
;	                   /LOG, $
;	                   /SCALE, $
;	                   NAME        = 'Gain', $
;	                   TITLE       = 'Gain', $
;	                   XTICKFORMAT = '(a1)', $
;	                   YRANGE      = [f[1], f[-1]])
;	cb_amp_scm = MrColorbar(ORIENTATION = 1, $
;	                        NAME        = 'CB: Gain', $
;	                        TARGET      = im_gain, $
;	                        TITLE       = 'Gain')

	;FGM PHASE
	im_phase_fgm = MrImage( phase_fgm, t_fgm, f_fgm, $
	                        /AXES, $
	                        /CURRENT, $
	                        /SCALE, $
	                        NAME        = 'FGM PHASE', $
	                        XTITLE      = 'Time', $
	                        XTICKFORMAT = '(a1)', $
	                        YRANGE      = [f_fgm[1], f_fgm[-1]], $
	                        YTITLE      = 'Frequency')
	cb_phase_fgm = MrColorbar(ORIENTATION = 1, $
	                          NAME        = 'CB: FGM PHASE', $
	                          TARGET      = im_phase_fgm, $
	                          TITLE       = 'Phase!C(Deg)')

	;SCM PHASE
	im_phase_scm = MrImage( phase_scm, t_scm, f_scm, $
	                        /AXES, $
	                        /CURRENT, $
	                        /SCALE, $
	                        NAME        = 'SCM PHASE', $
	                        XTITLE      = 'Time', $
	                        XTICKFORMAT = '(a1)', $
	                        YRANGE      = [f_scm[1], f_scm[-1]])
	cb_phase_scm = MrColorbar(ORIENTATION = 1, $
	                          NAME        = 'CB: SCM PHASE', $
	                          TARGET      = im_phase_scm, $
	                          TITLE       = 'Phase!C(Deg)')

	;PHASE OFFSET
;	im_offset = MrImage( offset, t, f, $
;	                     /AXES, $
;	                     /CURRENT, $
;	                     /SCALE, $
;	                     NAME        = 'Phase Offset', $
;	                     XTICKFORMAT = '(a1)', $
;	                     YRANGE      = [f[1], f[-1]])
;	cb_phase_scm = MrColorbar(ORIENTATION = 1, $
;	                          NAME        = 'CB: Phase Offset', $
;	                          TARGET      = im_offset, $
;	                          TITLE       = 'Phase Diff.!C(Deg)')

	;FGM PSD
	im_psd_fgm = MrImage( psd_fgm, t_fgm, f_fgm, $
	                      /AXES, $
	                      /CURRENT, $
	                      /LOG, $
	                      /SCALE, $
	                      NAME        = 'FGM PSD', $
	                      XTITLE      = 'Time', $
	                      XTICKFORMAT = 'time_labels', $
	                      YRANGE      = [f_fgm[1], f_fgm[-1]], $
	                      YTITLE      = 'Frequency')
	cb_amp_fgm = MrColorbar(ORIENTATION = 1, $
	                        NAME        = 'CB: FGM PSD', $
	                        TARGET      = im_psd_fgm, $
	                        TITLE       = 'PSD!C(nT^2/Hz)')

	;SCM PSD
	im_psd_scm = MrImage( psd_scm, t_scm, f_scm, $
	                      /AXES, $
	                      /CURRENT, $
	                      /LOG, $
	                      /SCALE, $
	                      NAME        = 'SCM PSD', $
	                      XTITLE      = 'Time', $
	                      XTICKFORMAT = 'time_labels', $
	                      YRANGE      = [f_scm[1], f_scm[-1]])
	cb_amp_scm = MrColorbar(ORIENTATION = 1, $
	                        NAME        = 'CB: SCM PSD', $
	                        TARGET      = im_psd_scm, $
	                        TITLE       = 'PSD!C(nT^2/Hz)')

	;PSD Ratio
;	im_psd_scm = MrImage( psdrat, t, f, $
;	                      /AXES, $
;	                      /CURRENT, $
;	                      /LOG, $
;	                      /SCALE, $
;	                      NAME        = 'PSD Ratio', $
;	                      XTITLE      = 'Time', $
;	                      XTICKFORMAT = 'time_labels', $
;	                      YRANGE      = [f[1], f[-1]])
;	cb_amp_scm = MrColorbar(ORIENTATION = 1, $
;	                        NAME        = 'CB: PSD Ratio', $
;	                        TARGET      = im_psd_scm, $
;	                        TITLE       = 'PSD Rat')

;------------------------------------;
; Finish                             ;
;------------------------------------;
	;Make pretty
	win -> SetGlobal, XTICKS=3

	;Refresh the window
	win -> Refresh
	
	;Save the graphic
	if png_file ne '' then win -> Save, png_file
	
	;Return the graphics
	return, win
end
