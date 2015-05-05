


;+
;   View the results of rotating beams into their bpp.
;
; :Params:
;       DATA_STRUCT:        in, required, type=structure
;                           Results of the averaging and BPP-finding process.
;
; :Keywords:
;       DOCS:               in, optional, type=boolean, default=0
;                           If set, data is given in despun OCS instead of BPP.
;-
function mms_fg_compare_l1b
	compile_opt idl2
;	on_error, 2

;-----------------------------------------------------
; Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	edi_dir      = '/Users/argall/Documents/Work/Data/MMS/EDI/'
	dfg_dir      = '/Users/argall/Documents/Work/Data/MMS/DFG/'
	afg_dir      = '/Users/argall/Documents/Work/Data/MMS/AFG/'
	cal_dir      = '/Users/argall/Documents/Work/Data/MMS/FG_Cal/'
	sunpulse_dir = '/Users/argall/Documents/Work/Data/MMS/HK/'
	attitude_dir = '/Users/argall/Documents/Work/Data/MMS/Ephemeris/'
	
	sc           = 'mms3'
	instr        = 'dfg'
	mode         = 'f128'
	tstart       = '2015-04-18T00:00:00Z'
	tend         = '2015-04-18T01:00:00Z'

;-----------------------------------------------------
; Calibrate FG Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if instr eq 'dfg' $
		then data_dir = dfg_dir $
		else data_dir = afg_dir
	
	;Get FG data
	b_bcs = mms_fg_bcs(sc, instr, mode, tstart, tend, $
	                   CAL_DIR  = cal_dir, $
	                   DATA_DIR = dfg_dir, $
	                   EPOCH    = epoch_bcs)

;-----------------------------------------------------
; Get Official FG Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	mode  = 'srvy'
	level = 'l1b'
	
	;Create the file name
	l1b_fname = mms_construct_filename(sc, instr, mode, level, /TOKENS, $
	                                   DIRECTORY = data_dir)
	
	;Search for the file
	l1b_files = MrFile_Search(l1b_fname, /CLOSEST, $
	                          COUNT     = nFiles, $
	                          TSTART    = tstart, $
	                          TEND      = tend, $
	                          TIMEORDER = '%Y%M%d')
	if nFiles eq 0 then message, 'No FG L1B files found: "' + l1b_fname + '".'
	
	;Create variable names
	b_vname = mms_construct_varname(sc, instr, mode + '_bcs')
	
	;Get the data
	b_l1b_bcs = MrCDF_nRead(l1b_files, b_vname, $
	                        DEPEND_0 = epoch_l1b, $
	                        TSTART   = tstart, $
	                        TEND     = tend)

;-----------------------------------------------------
; Plot the Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert to seconds since midnight
	MrCDF_Epoch, t0, fix(strmid(tstart, 0, 4)), fix(strmid(tstart, 5, 2)), fix(strmid(tstart, 8, 2)), TT2000, /COMPUTE_EPOCH
	t_ssm     = MrCDF_epoch2sse(epoch_bcs, t0)
	t_ssm_l1b = MrCDF_epoch2sse(epoch_l1b, t0)

	;Create a window
	win = window(WINDOW_TITLE='My calibrated data & Official L1B')
	win -> Refresh, /DISABLE
	
	yrange = [min(b_l1b_bcs, MAX=bMax), bMax]
	
	
	;FG L1B
	pBx = Plot(t_ssm_l1b, b_l1b_bcs[0,*], /CURRENT, $
	           LAYOUT      = [1,3,1], $
	           NAME        = 'Bx L1A', $
	           TITLE       = strupcase(instr) + ' B-Field (OCS) ' + strmid(tstart, 0, 10), $
	           XSTYLE      = 1, $
	           XTICKFORMAT = 'time_labels', $
	           XTITLE      = 'Time (UTC)', $
	           YRANGE      = yrange, $
	           YSTYLE      = 1, $
	           YTITLE      = '$B_{x}$ (nT)')

	pBy = Plot(t_ssm_l1b, b_l1b_bcs[1,*], /CURRENT, $
	           LAYOUT      = [1,3,2], $
	           NAME        = 'By L1A', $
	           XSTYLE      = 1, $
	           XTICKFORMAT = 'time_labels', $
	           XTITLE      = 'Time (UTC)', $
	           YRANGE      = yrange, $
	           YSTYLE      = 1, $
	           YTITLE      = '$B_{y}$ (nT)')

	pBz = Plot(t_ssm_l1b, b_l1b_bcs[2,*], /CURRENT, $
	           LAYOUT      = [1,3,3], $
	           NAME        = 'Bz L1A', $
	           XSTYLE      = 1, $
	           XTICKFORMAT = 'time_labels', $
	           XTITLE      = 'Time (UTC)', $
	           YRANGE      = yrange, $
	           YSTYLE      = 1, $
	           YTITLE      = '$B_{z}$ (nT)')

	;FG Calibrated
	opBx = Plot(t_ssm, b_bcs[0,*], $
	            COLOR       = 'Blue', $
	            NAME        = 'Bx BCS', $
	            OVERPLOT    = pBx, $
	            XSTYLE      = 1, $
	            XTICKFORMAT = 'time_labels', $
	            XTITLE      = 'Time (UTC)', $
	            YRANGE      = yrange, $
	            YSTYLE      = 1, $
	            YTITLE      = '$B_{x}$ (nT)')

	opBy = Plot(t_ssm, b_bcs[1,*], $
	            COLOR       = 'Green', $
	            NAME        = 'By BCS', $
	            OVERPLOT    = pBy, $
	            XSTYLE      = 1, $
	            XTICKFORMAT = 'time_labels', $
	            XTITLE      = 'Time (UTC)', $
	            YRANGE      = yrange, $
	            YSTYLE      = 1, $
	            YTITLE      = '$B_{y}$ (nT)')

	opBz = Plot(t_ssm, b_bcs[2,*], $
	            COLOR       = 'Red', $
	            NAME        = 'Bz BCS', $
	            OVERPLOT    = pBz, $
	            XSTYLE      = 1, $
	            XTICKFORMAT = 'time_labels', $
	            XTITLE      = 'Time (UTC)', $
	            YRANGE      = yrange, $
	            YSTYLE      = 1, $
	            YTITLE      = '$B_{z}$ (nT)')

	;Refresh the window and return
	win -> Refresh
	return, win
end
