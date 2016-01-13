;+
;
;-
function mms_fdoa_test_bcs2gei, sc, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif

	sc       = 'mms2'                 ;Slow                            ;Fast
	tstart   = '2015-05-09T16:08:00Z' ;01-Aug-2015 05:08:46.030963219  01-Aug-2015 00:41:57.83222159
	tend     = '2015-05-09T16:13:00Z' ;01-Aug-2015 13:19:18.340386093  01-Aug-2015 05:07:01.822075718
	sdc_dir  = '/nfs/'
	att_dir  = filepath('', ROOT_DIR='/nfs', SUBDIRECTORY=['ancillary', sc, 'defatt'])

;-------------------------------------------------------
; Search for Files /////////////////////////////////////
;-------------------------------------------------------
	
	;FG L1B file
	instr   = 'dfg'
	mode    = 'srvy'
	level   = 'l1b'
	optdesc = ''
	fg_l1b_files = mms_find_file(sc, instr, mode, level, $
	                            COUNT     = nfiles_fg, $
	                            OPTDESC   = optdesc, $
	                            SDC_ROOT  = sdc_dir, $
	                            SEARCHSTR = searchstr, $
	                            TSTART    = tstart, $
	                            TEND      = tend)
	if nfiles_fg eq 0 then message, 'No FG L1B files found: "' + searchstr + '".'
	
	;FG L1B file
	instr   = 'dfg'
	mode    = 'srvy'
	level   = 'ql'
	optdesc = ''
	fg_ql_files = mms_find_file(sc, instr, mode, level, $
	                            COUNT     = nfiles_fg, $
	                            OPTDESC   = optdesc, $
	                            SDC_ROOT  = sdc_dir, $
	                            SEARCHSTR = searchstr, $
	                            TSTART    = tstart, $
	                            TEND      = tend)
	if nfiles_fg eq 0 then message, 'No FG QL files found: "' + searchstr + '".'
	

	; Attitude file
	;   - Do not throw errors for attitude files. They are used only
	;     to rotate from BCS to SMPA, which is very nearly a unitary
	;     transformation.
	;   - Sunpulse times are used to despin data.
	str = filepath( ROOT_DIR=att_dir, strupcase(sc) + '_DEFATT_%Y%D_%Y%D.V*' );
	files_att = MrFile_Search( str, $
	                           /CLOSEST, $
	                           COUNT     = nfiles_att, $
	                           TSTART    = tstart, $
	                           TEND      = tend, $
	                           TIMEORDER = '%Y%D', $
	                           VREGEX    = 'V([0-9]{2})' )
	if nfiles_att eq 0 then message, 'No attitude files found: "' + str + '".'

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Attitude
	defatt = mms_fdoa_read_defatt(files_att, tstart, tend, HEADER=att_hdr)
	
	;FG
	fg_l1b = mms_fg_read_l1b(fg_l1b_files, tstart, tend)
	fg_ql  = mms_fg_read_ql( fg_ql_files,  tstart, tend)

;-----------------------------------------------------
; BCS -> GEI with Quaternions \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Turn times into doubles (seconds)
	t0    = min( [defatt.tt2000[0], fg_l1b.tt2000[0]] )
	t_att = double(defatt.tt2000 - t0) * 1e-9
	t_fg  = double(fg_l1b.tt2000 - t0) * 1e-9

	;Interpolate the quaternion
	Q     = qterp(t_att, defatt.q, t_fg, /SLERP)

	;Rotate to GEI
	b_gei = qtvrot(fg_l1b.b_bcs[0:2,*], Q)

;-----------------------------------------------------
; Show the Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	px = plot(t_fg, b_gei[0,*], $
	          COLOR  = 'Blue', $
	          TITLE  = 'Results of BCS -> GEI Transformation', $
	          XTITLE = 'Seconds', $
	          YTITLE = 'B!C(nT)' )
	py = plot(t_fg, b_gei[1,*], /CURRENT, OVERPLOT=px, COLOR='Green')
	pz = plot(t_fg, b_gei[2,*], /CURRENT, OVERPLOT=px, COLOR='Red')
stop
	return, px.window
end
