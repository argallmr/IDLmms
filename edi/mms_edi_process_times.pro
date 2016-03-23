; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ql_process
;
; PURPOSE:
;+
;   Process EDI ambient mode data to produce a quick-look data product with counts
;   sorted by 0 and 180 degree pitch angle.
;
;   Calling Sequence::
;       FILES = mms_edi_amb_l2_process(SC, OPTDESC, TSTART)
;       FILES = mms_edi_amb_l2_process('', '', TSTART)
;
; :Categories:
;    MMS, EDI
;
; :Params:
;       SC:                 in, optional, type=string/strarr, default=all
;                           Spacecraft identifiers for the data to process. The empty
;                                string is equivalent to choosing the default value.
;                                Options are: 'mms1', 'mms2', 'mms3', and/or 'mms4'
;       OPTDESC:            in, optional, type=string/strarr, default=all
;                           Optional descriptors of the files to be processed. Options are::
;                               'amb', 'amb-pm2', 'efield', 'q0', 'data29'
;       TSTART:             in, required, type=string, default=current date
;                           The file start time of all files to be processed. The format
;                               is 'YYYYMMDDhhmmss' for burst files and 'YYYYMMDD' for
;                               survey.
;
; :Keywords:
;       LEVEL:              in, optional, type=string/strarr
;                           Level of data to create (i.e. output level). By default, this
;                               is 'l2' for all except efield files, for which it is 'l2pre'.
;                               options are: 'ql', 'l2pre', 'l2'.
;       NO_LOG:             in, optional, type=boolean, default=0
;                           If set, no log file will be created and messages will be
;                               directed to the current error logging file (defaults to the
;                               console window -- see MrStdLog.pro)
;       DATA_PATH_ROOT:     in, optional, type=string, default=!mms_unh_init.data_dir
;                           Root directory of the SDC directory structure where files are
;                               stored. The structure looks like
;                               DATA_DIR/sc/instr/mode/level[/optdesc]/year/month[/day],
;                               where "/day" is used only for brst data.
;       DROPBOX_ROOT:       in, optional, type=string, default=!mms_unh_init.dropbox
;                           Directory in which to save data. Externally, files are moved from
;                               DROPBOX into `DATA_DIR`.
;       LOG_PATH_ROOT:      in, optional, type=string, default='/nfs/edi'
;                           Root directory in which to save log files. Files are actually saved to
;                               LOG_DIR/sc/instr/mode/level/year/month[/day] to mimick the
;                               MMS SDC data directory structure. "/day" is included only if
;                               burst files are being processed.
;
; :Author:
;    Matthew Argall::
;        University of New Hampshire
;        Morse Hall Room 348
;        8 College Road
;        Durham, NH 03824
;        matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/03/13  -   Written by Matthew Argall
;-
PRO mms_edi_process_times, sc, optdesc, tstart, $
LEVEL=level, $
NO_LOG=no_log, $
DATA_PATH_ROOT=data_path_in, $
DROPBOX_ROOT=dropbox_in, $
LOG_PATH_ROOT=log_path_in
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(oLog) gt 0 then begin
			oLog -> AddError
			obj_destroy, oLog
		endif else begin
			print, !error_state.msg
			print, transpose(mrtraceback())
		endelse
		return
	endif
	
	;Everything starts out ok
	status = 0
	
	;Initialize
	unh_edi_init
	
	;Calculate the current date
	caldat, systime(/JULIAN, /UTC), month, day, year, hour, minute, second
	date = string(FORMAT='(%"%04i%02i%02i")', year, month, day)
	time = string(FORMAT='(%"%02i%02i%02i")', hour, minute, second)
	
	;Error destination: Console or file?
	tf_log = ~keyword_set(no_log)
	if tf_log then begin
		logDir = filepath('', ROOT_DIR=!edi_init.log_path_root, SUBDIRECTORY='batch_logs')
		if ~file_test(logDir, /DIRECTORY) then file_mkdir, logDir
		fLog   = filepath('mms_process_times_' + date + '_' + time + '.log', ROOT_DIR=logDir)
	endif else begin
		fLog      = 'StdErr'
	endelse
	
	;Create error logger
	oLog = MrLogFile(fLog)

	;Options
	all_sc      = ['mms1', 'mms2', 'mms3', 'mms4']
	all_optdesc = ['amb', 'amb-pm2', 'efield', 'q0', 'data29']
	all_level   = ['ql', 'l2pre', 'l2']
	all_mode    = ['srvy', 'brst']
	all_instr   = 'edi'

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Defaults
	if n_elements(sc)      eq 0 || sc[0]   eq '' then sc      = ['mms1', 'mms2', 'mms3', 'mms4']
	if n_elements(optdesc) eq 0 || optdesc eq '' then optdesc = ['amb', 'amb-pm2', 'efield', 'q0', 'data29']
	if n_elements(tstart)  eq 0 || tstart  eq '' then message, 'TSTART must be provided.'
	
	;Default to creating L2 (or L2Pre in the case of efield)
	if n_elements(level) eq 0 then level = ''

	;Keywords
	if n_elements(data_path_in) eq 0 then data_path = !edi_init.data_path_root else data_path = data_path_in
	if n_elements(dropbox_in)   eq 0 then dropbox   = !edi_init.dropbox_root   else dropbox   = dropbox_in
	if n_elements(log_path_in)  eq 0 then log_path  = !edi_init.log_path_root  else log_path  = log_path_in

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of elements
	nSC      = n_elements(sc)
	nOptdesc = n_elements(optdesc)
	nTStart  = n_elements(tstart)
	nLevel   = n_elements(level)
	
	;Unique values
	if n_elements(uniq(sc, sort(sc))) ne nSC then message, 'SC must contain only unique values.'
	
	;Valid SC, MODE, and PACMO
	if min(MrIsMember(all_sc, scID)) eq 0 $
		then message, 'Invalid spacecraft ID given.'
	if min(MrIsMember(all_optdesc, mode)) eq 0 $
		then message, 'Invalid optional descriptor given.'
	
	;Directories must be read and/or writable
	if tf_log && ~file_test(log_path, /DIRECTORY, /WRITE) $
		then message, 'LOG_PATH must exist and be writeable.'
	if ~file_test(data_path, /DIRECTORY, /READ) $
		then message, 'DATA_PATH directory must exist and be readable.'
	if ~file_test(dropbox, /DIRECTORY, /READ, /WRITE) $
		then message, 'DROPBOX directory must exist and be read- and writeable.'
	
;-----------------------------------------------------
; Loop Over Date & Mode \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Allocate memory
	nalloc   = 100
	files    = strarr(nalloc)
	status   = bytarr(nalloc)
	telapsed = dblarr(nalloc)
	count    = 0
	
	;Record start time
	oLog -> AddText, 'Processing EDI data.'
	oLog -> AddText, 'Start: ' + systime(/UTC)
	oLog -> AddText, ''
	t_begin = systime(1)

	;Loop
	for i = 0, nSC - 1 do begin
	for j = 0, nOptdesc - 1 do begin
	for k = 0, nLevel - 1 do begin
	for m = 0, nTStart - 1 do begin

	;-----------------------------------------------------
	; Setup to Process \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
	
		;Which mode?
		;   - Survey has time formatted as YYYYMMDD (length 8)
		;   - Burst has time formatted as YYYYMMDDhhmmss (length 14)
		mode  = strlen(tstart[m]) eq 8 ? 'srvy' : 'brst'
		instr = 'edi'
	
		;Starting a new sc/mode
		oLog -> AddText, '------------------------------------------------'
		oLog -> AddText, '################################################'
		oLog -> AddText, string(sc[i], instr, mode, level[k], optdesc[j], tstart[m], $
		                        FORMAT='(%"PROCESSING %s %s %s %s %s %s")')
		oLog -> AddText, ''
		
		;Report start time
		oLog -> AddText, 'Processing started at:  ' + systime(/UTC)
		f_begin = systime(1)

	;-----------------------------------------------------
	; Process Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		
		;AMB
		if optdesc[j] eq 'amb' then begin
			case level[k] of
				'ql': code = mms_edi_amb_ql_sdc(sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				'l2': code = mms_edi_amb_l2_sdc(sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				'':   code = mms_edi_amb_l2_sdc(sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				else: ;Skip this one
			endcase
		
		;AMB-PM2
		endif else if optdesc[j] eq 'amb-pm2' then begin
			case level[k] of
				'ql': code = mms_edi_amb_ql_sdc(sc[i], mode, tstart[m], PACK_MODE=2, FILE_OUT=file_out, NO_LOG=no_log)
				'l2': code = mms_edi_amb_l2_sdc(sc[i], mode, tstart[m], PACK_MODE=2, FILE_OUT=file_out, NO_LOG=no_log)
				'':   code = mms_edi_amb_l2_sdc(sc[i], mode, tstart[m], PACK_MODE=2, FILE_OUT=file_out, NO_LOG=no_log)
				else: ;Skip this one
			endcase
		
		;EFIELD
		endif else if optdesc[j] eq 'efield' then begin
			case level[k] of
				'l2':    code = mms_edi_amb_l2_sdc(   sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				'l2pre': code = mms_edi_amb_l2pre_sdc(sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				'':      code = mms_edi_amb_l2pre_sdc(sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				else: ;Skip this one
			endcase
		
		;Q0
		endif else if optdesc[j] eq 'q0' then begin
			case level[k] of
				'ql': code = mms_edi_q0_ql_sdc(sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				'l2': code = mms_edi_q0_l2_sdc(sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				'':   code = mms_edi_q0_l2_sdc(sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				else: ;Skip this one
			endcase
		
		;DATA29
		;   - Only available in burst mode
		endif else if optdesc[j] eq 'data29' && mode eq 'brst' then begin
			case level[k] of
				'l2': code = mms_edi_data29_l2_sdc(sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				'':   code = mms_edi_data29_l2_sdc(sc[i], mode, tstart[m], FILE_OUT=file_out, NO_LOG=no_log)
				else: ;Skip this one
			endcase
		
		;UNKNOWN
		endif else begin
			message, 'Invalid optional descriptor: "' + optdesc[j] + '".'
		endelse

	;-----------------------------------------------------
	; Finish Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------

		;End of processing time
		f_end = systime(1)
		oLog -> AddText, 'Processing finished at: ' + systime(/UTC)
		
		;Save results
		if status_out eq 0 $
			then files[count] = file_out $
			else files[count] = strjoin([sc[i], instr, mode, level[k], optdesc[j], tstart[m]], '_')
		status[count]   = status_out
		telapsed[count] = f_end - f_begin
		count++
		
		;Allocate more memory (double allocation each time)
		if count ge nalloc then begin
			nalloc  += count
			status   = [status,   bytarr(count)]
			files    = [files,    strarr(count)]
			telapsed = [telapsed, dblarr(count)]
		endif

		;Report results
		oLog -> AddText, 'Finished processing'
		if n1 gt 0 then oLog -> AddText, '   ' + fmode + ' file:   "' + f_mode1[i1] + '"'
		if n2 gt 0 then oLog -> AddText, '   Slow file:   "' + f_mode2[i2] + '"'
		oLog -> AddText, '   Output file: "' + file_out   + '"'
		oLog -> AddText, '   Error status: ' + string(status_out, FORMAT='(i0)')
		oLog -> AddText, '   Proc time:    ' + string(telapsed[count-1]/60.0, FORMAT='(f0.2)') + ' min'
		oLog -> AddText, '##############################################'
		oLog -> AddText, '----------------------------------------------'
		oLog -> AddText, ''
	endfor ;tstart
	endfor ;level
	endfor ;optdesc
	endfor ;sc
	
	;Finish processing
	t_end = systime(1)
	
;-----------------------------------------------------
; Executive Summary \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Trim results
	files  = files[0:count - 1]
	status = status[0:count - 1]

	;Number of errors
	ierror = where(status ge 100, nerror)
	iwarn  = where(status gt   0 and status lt 100, nwarn)

	;Time elapsed
	dt     = t_end - t_begin
	dt_hr  = floor((dt) / 3600.0)
	dt_min = floor( (dt mod 3600.0) / 60.0 )
	dt_sec = dt mod 60

	;Log summary information
	oLog -> AddText, ''
	oLog -> AddText, '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
	oLog -> AddText, '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
	oLog -> AddText, 'EXECUTIVE SUMMARY'
	oLog -> AddText, '   Number of Files:    ' + strtrim(count,  2)
	oLog -> AddText, '   Number of Warnings: ' + strtrim(nwarn,  2)
	oLog -> AddText, '   Number of Errors:   ' + strtrim(nerror, 2)
	oLog -> AddText, '   Total Time Elapsed: ' + string(FORMAT='(%"%ihr %imin %0.2fsec")', dt_hr, dt_min, dt_sec)
	oLog -> AddText, '   Status    Name'
	oLog -> AddText, '    ' + string(status, FORMAT='(i3)') + '       ' + files
	oLog -> AddText, '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
	oLog -> AddText, '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
	
	;Close log object
	obj_destroy, oLog
end