; docformat = 'rst'
;
; NAME:
;    mms_edi_efield_l2_process
;
; PURPOSE:
;+
; Process L1A EDI electric field mode data to produce an electric field
; and associated error.
;
;   Calling Sequence::
;       FILES = mms_edi_data29_l2_process()
;       FILES = mms_edi_data29_l2_process(SC)
;       FILES = mms_edi_data29_l2_process(SC, DATE_START)
;       FILES = mms_edi_data29_l2_process(SC, DATE_START, '')
;       FILES = mms_edi_data29_l2_process(SC, DATE_START, DATE_END)
;
; :Categories:
;    MMS, EDI, L2, DATA29
;
; :Params:
;       SC:         in, optional, type=string/strarr, default=['mms1', 'mms2', 'mms3', 'mms4']
;                   Spacecraft identifiers for the data to process. Options are::
;                       'mms1', 'mms2', 'mms3', and/or 'mms4'
;       TSTART:     in, optional, type=string, default=current date
;                   First date of data to be processed. Formatted as 'YYYYMMDD' or
;                       'YYYYMMDDhhmmss'. The empty string is equivalent to choosing
;                       the default value.
;       TEND:       in, optional, type=string, default=`TSTART`
;                   Last date of data to be processed. Formatted as 'YYYYMMDD' or
;                       'YYYYMMDDhhmmss'. All files between `TSTART` and TEND
;                       are processed. If TEND is not defined, the value of `TSTART`
;                       is used and the file for which the start date is closest to
;                       TSTART without going over will be processed. If TEND
;                       is the empty string, then the current date is used.
;
; :Keywords:
;       NO_LOG:     in, optional, type=boolean, default=0
;                   If set, no log file will be created and messages will be
;                       directed to the current error logging file (defaults to the
;                       console window -- see MrStdLog.pro)
;       DATA_PATH:  in, optional, type=string, default=!mms_unh_init.data_dir
;                   Root directory of the SDC directory structure where files are
;                       stored. The structure looks like
;                       DATA_DIR/sc/instr/mode/level[/optdesc]/year/month[/day],
;                       where "/day" is used only for brst data.
;       DROPBOX:    in, optional, type=string, default=!mms_unh_init.dropbox
;                   Directory in which to save data. Externally, files are moved from
;                       DROPBOX into `DATA_DIR`.
;       LOG_PATH:   in, optional, type=string, default='/nfs/edi'
;                   Root directory in which to save log files. Files are actually saved to
;                       LOG_DIR/sc/instr/mode/level/year/month[/day] to mimick the
;                       MMS SDC data directory structure. "/day" is included only if
;                       burst files are being processed.
;-
pro mms_edi_data29_l2_process, sc, tstart, tend, $
NO_LOG=no_log, $
DATA_PATH_ROOT=data_path_root, $
DROPBOX_ROOT=dropbox_root, $
LOG_PATH_ROOT=log_path_root
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(oLog) then begin
			oLog -> AddError
			obj_destroy, oLog
		endif else begin
			print, !error_state.msg
			print, '   ' + transpose(MrTraceback())
		endelse
		return
	endif

	;Everything starts out ok
	status = 0
	
	;Initialize
	unh_edi_amb_init
	
	;Calculate the current date
	caldat, systime(/JULIAN, /UTC), month, day, year, hour, minute, second
	date = string(FORMAT='(%"%04i%02i%02i")', year, month, day)
	time = string(FORMAT='(%"%02i%02i%02i")', hour, minute, second)
	
	;Error destination: Console or file?
	tf_log = ~keyword_set(no_log)
	if tf_log then begin
		logDir = filepath('', ROOT_DIR=!edi_amb_init.log_path_root, SUBDIRECTORY='batch_logs')
		if ~file_test(logDir, /DIRECTORY) then file_mkdir, logDir
		fLog   = filepath('mms_edi_data29_l2_unh_' + date + '_' + time + '.log', ROOT_DIR=logDir)
	endif else begin
		fLog      = 'StdErr'
	endelse
	
	;Create error logger
	oLog = MrLogFile(fLog)

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Parameters
	if n_elements(sc)     eq 0 || sc[0]   eq '' then sc     = ['mms1', 'mms2', 'mms3', 'mms4']
	if n_elements(tstart) eq 0 || tstart  eq '' then tstart = date
	if n_elements(tend)   eq 0 $
		then tend = tstart $
		else if tend eq '' then tend = date

	;Keywords
	if n_elements(data_path_root) eq 0 then data_path = !edi_amb_init.data_path_root else data_path = data_path_root
	if n_elements(dropbox_root)   eq 0 then dropbox   = !edi_amb_init.dropbox_root   else dropbox   = dropbox_root
	if n_elements(log_path_root)  eq 0 then log_path  = !edi_amb_init.log_path_root  else log_path  = log_path_root
	if n_elements(unh_path_root)  eq 0 then unh_path  = !edi_amb_init.unh_data_root  else unh_path = unh_data_root

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of elements
	nSC    = n_elements(sc)
	nMode  = n_elements(mode)
	nStart = n_elements(tstart)
	nEnd   = n_elements(tend)
	
	;Unique values
	if n_elements(uniq(sc,   sort(sc)))   ne nSC   then message, 'SC must contain only unique values.'
	if nStart ne 1 then message, 'TSTART must be a scalar string.'
	if nEnd   ne 1 then message, 'TEND must be a scalar string.'
	
	;Valid SC and MODE
	if min(MrIsMember(['mms1', 'mms2', 'mms3', 'mms4'], sc)) eq 0 $
		then message, 'Invalid spacecraft ID given.'
	
	;Directories must be writable
	if tf_log && ~file_test(log_path, /DIRECTORY, /WRITE) $
		then message, 'LOG_PATH_ROOT must exist and be writeable.'
	if ~file_test(data_path, /DIRECTORY, /READ) $
		then message, 'DATA_PATH_ROOT directory must exist and be readable.'
	if ~file_test(dropbox, /DIRECTORY, /READ, /WRITE) $
		then message, 'DROPBOX_ROOT directory must exist and be read- and writeable.'

;-----------------------------------------------------
; Reformat Times \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Parse the start and end times
	mms_parse_time, tstart, syr, smo, sday, shr, smin, ssec, /INTEGER
	mms_parse_time, tend,   eyr, emo, eday, ehr, emin, esec, /INTEGER

	;Form the start and stop times
	tstart = string(syr, smo, sday, shr, smin, ssec, $
	                FORMAT='(%"%04i-%02i-%02iT%02i:%02i:%02iZ")')
	tend   = string(eyr, emo, eday, ehr, emin, esec, $
	                FORMAT='(%"%04i-%02i-%02iT%02i:%02i:%02iZ")')

	;Constants for source file
	instr   = 'edi'
	mode    = 'brst'
	level   = 'l1a'
	optdesc = 'efield'

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
	oLog -> AddText, 'Processing EDI E-Field data.'
	oLog -> AddText, 'Start: ' + systime(/UTC)
	oLog -> AddText, ''
	t_begin = systime(1)

	;Loop
	for k = 0, n_elements(sc)   - 1 do begin
		;Starting a new sc/mode
		oLog -> AddText, '------------------------------------------------'
		oLog -> AddText, '################################################'
		oLog -> AddText, string(sc[k], instr, mode, level, optdesc, tstart, tend, $
		                        FORMAT='(%"PROCESSING %s %s %s %s %s %s - %s")')
		oLog -> AddText, ''
	
	;-----------------------------------------------------
	; Find  Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Find files
		;   - If processing 'SRVY' files, 'SRVY' source files are expected.
		;   - 'FAST' and 'SLOW' files must be combined at the L2PRE level.
		edi_files = mms_find_file(sc[k], instr, mode, level, $
		                          COUNT    = nFiles, $
		                          OPTDESC  = optdesc, $
		                          SDC_ROOT = data_path, $
		                          TSTART   = tstart, $
		                          TEND     = tend)

		;No files found
		if nFiles eq 0 then begin
			oLog -> AddText, string(sc[k], instr, mode, level, optdesc, tstart, tend, $
			                        FORMAT='(%"No %s %s %s %s %s files found in interval %s - %s")')
			oLog -> AddError, 'No files found. Skipping.'
			continue
		endif
	
	;-----------------------------------------------------
	; Sort Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Gather start times of all files
		mms_dissect_filename, edi_files, TT2000=tt2000
		
		;Extract the uniq times
		tt2000 = tt2000[uniq(tt2000, sort(tt2000))]
		nTimes = n_elements(tt2000)

	;-----------------------------------------------------
	; Process Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		for i = 0, nTimes - 1 do begin
			;Locate the files for each unique time
			iFile = where(tt2000 eq tt2000[i], nFile)
			
			;Should have only one file of each type per interval
			if nFile gt 1 then begin
				oLog -> AddError, 'More than one ' + mode + ' file found for ' + $
				                  MrCDF_Epoch_Encode(tt2000[i])
				oLog -> AddText, edi_files[iFile]
				continue
			endif
			
			;Reformat the time
			MrCDF_Epoch_Breakdown, tt2000[i], yr, mo, day, hr, mnt, sec
			fstart = string(FORMAT='(%"%04i%02i%02i%02i%02i%02i")', yr, mo, day, hr, mnt, sec)

			;Report start and end
			oLog -> AddText, 'Processing started at:  ' + systime(/UTC)
			f_begin = systime(1)
			
			;Process the data
			status_out = mms_edi_data29_l2_sdc(sc[k], fstart, FILE_OUT=file_out)

			;End of processing time
			f_end = systime(1)
			oLog -> AddText, 'Processing finished at: ' + systime(/UTC)

		;-----------------------------------------------------
		; Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		;-----------------------------------------------------
			
			;Save results
			;   - STATUS_OUT can be > 0 and still produce a valid cdf file
			;   - If FILE_OUT = "", then a fatal error occurred without producing a file
			;   - In this case, write information to the log file so we know which file
			;     caused the error.
			if file_out ne '' $
				then files[count] = temporary(file_out) $
				else files[count] = strjoin([sc[k], 'edi', mode, 'l2', 'data29', fstart], '_')
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
			oLog -> AddText, '   ' + mode + ' file:   "' + edi_files[iFile] + '"'
			oLog -> AddText, '   Output file: "' + files[count-1]   + '"'
			oLog -> AddText, '   Error status: ' + string(status_out, FORMAT='(i0)')
			oLog -> AddText, '   Proc time:    ' + string(telapsed[count-1]/60.0, FORMAT='(f0.2)') + ' min'
			oLog -> AddText, '##############################################'
			oLog -> AddText, '----------------------------------------------'
			oLog -> AddText, ''
		endfor ;date
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
	iwarn  = where(status lt 100, nwarn)

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