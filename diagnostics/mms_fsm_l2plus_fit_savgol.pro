; docformat = 'rst'
;
; NAME:
;       mms_fsm_l2plus_fit_savgol
;
; PURPOSE:
;+
;   Smooth FSM background data via the Savitky-Golay method.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016/10/31  -   Written by Matthew Argall
;-
pro mms_fsm_l2plus_fit_savgol
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		on_error, 2
;		tvlct, r, g, b
;		device, DECOMPOSED=decomposed_in
		if n_elements(cdfID) gt 0 then cdf_close, cdfID
		message, /REISSUE_LAST
		return
	endif
	
;	tvlct, r, g, b, /GET
;	device, GET_DECOMPOSED=decomposed_in

;-------------------------------------------
; File & Variable Names ////////////////////
;-------------------------------------------

	;File to read
	file = '/nfs/fsm/temp/mms4_fsm_srvy_l2plus_cal-dfg_20151101153733_v1.0.0.cdf'
	
	;Variable names
	hist_vname = 'mms4_fsm_psd_hist_srvy_l2plus'
	bins_vname = 'mms4_fsm_psd_bins_srvy_l2plus'
	f_vname    = 'mms4_fsm_f_srvy_l2plus'
	flag_vname = 'mms4_fsm_flag_hist_srvy_l2plus'
	comp_vname = 'mms4_fsm_component_index_srvy_l2plus'

;-------------------------------------------
; Read Data ////////////////////////////////
;-------------------------------------------

	;Open file
	cdfID = cdf_open(file)

	;COMPONENTS
	cdf_control, cdfID, VARIABLE=comp_vname, GET_VAR_INFO=info
	cdf_varget, cdfID, comp_vname, comp, REC_COUNT=info.maxrec+1
	
	;FREQUENCIES
	cdf_control, cdfID, VARIABLE=f_vname, GET_VAR_INFO=info
	cdf_varget, cdfID, f_vname, f, REC_COUNT=info.maxrec+1
	
	;BINS
	cdf_control, cdfID, VARIABLE=bins_vname, GET_VAR_INFO=info
	cdf_varget, cdfID, bins_vname, bins, REC_COUNT=info.maxrec+1
	
	;FLAGS
	cdf_control, cdfID, VARIABLE=flag_vname, GET_VAR_INFO=info
	cdf_varget, cdfID, flag_vname, flag, REC_COUNT=info.maxrec+1
	
	;HISTOGRAM
	cdf_control, cdfID, VARIABLE=hist_vname, GET_VAR_INFO=info
	cdf_varget, cdfID, hist_vname, hist, REC_COUNT=info.maxrec+1

;-------------------------------------------
; Reduce Data //////////////////////////////
;-------------------------------------------
	;Pick the component and flag
	theFlag = 1
	theComp = 2
	fc      = 0.5      ;Cut-off frequency of high-pass filter
	f0      = 1.0      ;Frequency at which to apply SAVGOL
	iFlag   = where( flag eq theFlag, nFlag )
	iComp   = where( comp eq theComp, nComp )
	if0     = value_locate( f, f0 )
	
	;Select subset of data
	hist = reform( hist[*, iFlag, iComp, if0] )

;-------------------------------------------
; Savitky-Golay Filter /////////////////////
;-------------------------------------------
	;Create the filter
	nLeft  = 6
	nRight = 6
	degree = 4
	filt0   = savgol( nLeft, nRight, 0, degree )
	filt1   = savgol( nLeft, nRight, 1, degree )
	
	;Apply the filter
	result = convol( float(hist), filt0, /EDGE_TRUNCATE)
	deriv1 = convol( float(hist), filt1, /EDGE_TRUNCATE)
	

;-------------------------------------------
; Plot Results /////////////////////////////
;-------------------------------------------
	;Set a few colors
	device, DECOMPOSED=0
	tvlct, [[255], [  0], [  0]], 1 ;RED
	tvlct, [[  0], [255], [  0]], 2 ;GREEN
	tvlct, [[  0], [  0], [255]], 3 ;BLUE

	;Original signal (asterisks)
	p1 = MrPlot( bins, hist, $
	             NAME   = 'Data', $
	             PSYM   = 2, $
	             TITLE  = 'f=' + string(f0, FORMAT='(f4.1)'), $
	             XTITLE = 'PSD (nT^2/Hz)', $
	             YRANGE = [min(deriv1),max(result)], $
	             YTITLE = 'Occurrence' )
	
	;SAVGOL smooth (X's w/ dashed line)
	p2 = MrPlot( bins, result, $
	             COLOR     = 'Red', $
	             LINESTYLE = 2, $
	             NAME      = 'SavGol', $
	             OVERPLOT  = p1, $
	             PSYM      = -7 )
	
	
	;1st derivative (triangles w/ dashed dot dot)
	p3 = MrPlot( bins, deriv1, $
	             COLOR     = 'Blue', $
	             LINESTYLE = 4, $
	             NAME      = '1st Deriv', $
	             OVERPLOT  = p1, $
	             PSYM      = -5 )
	
	;Legend
	lgd = MrLegend( ALIGNMENT    = 'NE', $
	                /AUTO_TEXT_COLOR, $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                TARGET       = [p1, p2, p3], $
	                SAMPLE_WIDTH = 0.0 )
	
	;Restore the original color table
;	tvlct, r, g, b
;	device, DECOMPOSED=decomposed_in
end