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
;       FILES = mms_edi_amb_l2_process()
;       FILES = mms_edi_amb_l2_process(SC)
;       FILES = mms_edi_amb_l2_process(SC, MODE)
;       FILES = mms_edi_amb_l2_process(SC, MODE, DATE_START)
;       FILES = mms_edi_amb_l2_process(SC, MODE, DATE_START, '')
;       FILES = mms_edi_amb_l2_process(SC, MODE, DATE_START, DATE_END)
;
; :Categories:
;    MMS, EDI, QL, Ambient
;
; :Params:
;       SC:                 in, optional, type=string/strarr, default=['mms1', 'mms2', 'mms3', 'mms4']
;                           Spacecraft identifiers for the data to process. Options are::
;                               'mms1', 'mms2', 'mms3', and/or 'mms4'
;       MODE:               in, required, type=string/strarr, default=['srvy', 'brst']
;                           Data rate mode of the files to be processed. Choices are:
;                               'slow', 'fast', 'srvy', 'brst'. The 'srvy' option combines
;                               'slow' and 'fast' into a single 'srvy' file.
;       TSTART:             in, optional, type=string, default=current date
;                           First date of data to be processed. Formatted as 'YYYYMMDD' or
;                               'YYYYMMDDhhmmss'. The empty string is equivalent to choosing
;                               the default value.
;       TEND:               in, optional, type=string, default=`TSTART`
;                           Last date of data to be processed. Formatted as 'YYYYMMDD' or
;                               'YYYYMMDDhhmmss'. All files between `TSTART` and TEND
;                               are processed. If TEND is not defined, the value of `TSTART`
;                               is used and the file for which the start date is closest to
;                               TSTART without going over will be processed. If TEND
;                               is the empty string, then the current date is used.
;
; :Keywords:
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
;       PACK_MODE:          in, optional, type=integer, default=1
;                           Packing mode. Options are:
;                               1 - Magnetic field is focused between channels 2 & 3
;                               2 - Magnetic field is focused on channels 1
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
;       2015/10/26  -   Written by Matthew Argall
;-
PRO mms_edi_amb_l2_process, sc, mode, tstart, tend, $
NO_LOG=no_log, $
DATA_PATH_ROOT=data_path_in, $
DROPBOX_ROOT=dropbox_in, $
LOG_PATH_ROOT=log_path_in, $
PACK_MODE=pacmo
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
		fLog   = filepath('mms_edi_amb_l2_' + date + '_' + time + '.log', ROOT_DIR=logDir)
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
	if n_elements(mode)   eq 0 || mode[0] eq '' then mode   = ['srvy', 'brst']
	if n_elements(tstart) eq 0 || tstart  eq '' then tstart = date
	if n_elements(tend)   eq 0 $
		then tend = tstart $
		else if tend eq '' then tend = date

	;Keywords
	if n_elements(pacmo)        eq 0 then pacmo     = 1
	if n_elements(data_path_in) eq 0 then data_path = !edi_init.data_path_root else data_path = data_path_in
	if n_elements(dropbox_in)   eq 0 then dropbox   = !edi_init.dropbox_root   else dropbox   = dropbox_in
	if n_elements(log_path_in)  eq 0 then log_path  = !edi_init.log_path_root  else log_path  = log_path_in

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of elements
	nSC    = n_elements(sc)
	nMode  = n_elements(mode)
	nStart = n_elements(tstart)
	nEnd   = n_elements(tend)
	nPacmo = n_elements(pacmo)
	
	;Unique values
	if n_elements(uniq(sc,    sort(sc)))    ne nSC    then message, 'SC must contain only unique values.'
	if n_elements(uniq(mode,  sort(mode)))  ne nMode  then message, 'MODE must contain only unique values.'
	if n_elements(uniq(pacmo, sort(pacmo))) ne nPacmo then message, 'PACMO must contain only unique values.'
	if nStart ne 1 then message, 'TSTART must be a scalar string.'
	if nEnd   ne 1 then message, 'TEND must be a scalar string.'
	
	;Valid SC, MODE, and PACMO
	if min(MrIsMember(['mms1', 'mms2', 'mms3', 'mms4'], sc)) eq 0 $
		then message, 'Invalid spacecraft ID given.'
	if min(MrIsMember(['brst', 'srvy'], mode)) eq 0 $
		then message, 'MODE mode be "brst" and/or "srvy"'
	if min(MrIsMember([1, 2], pacmo)) eq 0 $
		then message, 'PACMO must be 1 and/or 2.'
	
	;Directories must be read and/or writable
	if tf_log && ~file_test(log_path, /DIRECTORY, /WRITE) $
		then message, 'LOG_PATH must exist and be writeable.'
	if ~file_test(data_path, /DIRECTORY, /READ) $
		then message, 'DATA_PATH directory must exist and be readable.'
	if ~file_test(dropbox, /DIRECTORY, /READ, /WRITE) $
		then message, 'DROPBOX directory must exist and be read- and writeable.'
	
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
	
	;Constants for source files
	instr = 'edi'
	level = 'l1a'
	
	;Constants for output
	outinstr = 'edi'
	outlevel = 'l2'
	
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
	oLog -> AddText, 'Processing EDI Ambient data.'
	oLog -> AddText, 'Start: ' + systime(/UTC)
	oLog -> AddText, ''
	t_begin = systime(1)

	;Loop
	for p = 0, n_elements(pacmo) - 1 do begin
	for j = 0, n_elements(mode)  - 1 do begin
	for k = 0, n_elements(sc)    - 1 do begin
		;Optional descriptor
		optdesc = pacmo[p] eq 1 ? 'amb' : 'amb-pm' + string(pacmo[p], FORMAT='(i0)')

		;Starting a new sc/mode
		oLog -> AddText, '------------------------------------------------'
		oLog -> AddText, '################################################'
		oLog -> AddText, string(sc[k], instr, mode[j], level, optdesc, tstart, tend, $
		                        FORMAT='(%"PROCESSING %s %s %s %s %s %s - %s")')
		oLog -> AddText, ''

	;-----------------------------------------------------
	; Find FAST/BRST Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;If we are processing "srvy" data, we must search
		;for 'fast' and 'slow' files. Search for "fast" first.
		fmode = mode[j] eq 'srvy' ? 'fast' : mode[j]

		;Find files
		f_mode1 = mms_find_file(sc[k], instr, fmode, level, $
		                        COUNT   = cnt1, $
		                        OPTDESC = optdesc, $
		                        TSTART  = tstart, $
		                        TEND    = tend)
		
		;No FAST/BRST files found
		if cnt1 eq 0 then begin
			oLog -> AddText, string(sc[k], instr, fmode, level, optdesc, tstart, tend, $
			                        FORMAT='(%"No %s %s %s %s %s files found in interval %s - %s")')
		endif
	
	;-----------------------------------------------------
	; Find SLOW Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		cnt2 = 0
		if mode[j] eq 'srvy' then begin
			f_mode2 = mms_find_file(sc[k], instr, 'slow', level, $
			                        COUNT   = cnt2, $
			                        OPTDESC = optdesc, $
			                        TSTART  = tstart, $
			                        TEND    = tend)
		
			;No SLOW files found
			if cnt2 eq 0 then begin
				oLog -> AddText, string(sc[k], instr, 'slow', level, optdesc, tstart, tend, $
				                        FORMAT='(%"No %s %s %s %s %s files found in interval %s - %s")')
			endif
		endif
	
		;Zero files found
		if cnt1 + cnt2 eq 0 then begin
			oLog -> AddError, 'No files found. Skipping.'
			continue
		endif
	
	;-----------------------------------------------------
	; Sort Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Gather start times of all files
		if cnt1 gt 0 then mms_dissect_filename, f_mode1, TT2000=tt2000_mode1
		if cnt2 gt 0 then mms_dissect_filename, f_mode2, TT2000=tt2000_mode2
		
		;Combine the start times into a single array
		if cnt1 gt 0 && cnt2 gt 0 then begin
			tt2000 = [tt2000_mode1, tt2000_mode2]
		endif else if cnt1 gt 0 then begin
			tt2000 = tt2000_mode1
		endif else begin
			tt2000 = tt2000_mode2
		endelse
		
		;Extract the uniq times
		tt2000 = tt2000[uniq(tt2000, sort(tt2000))]
		nTimes = n_elements(tt2000)
	
	;-----------------------------------------------------
	; Process Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		for i = 0, nTimes - 1 do begin
			;Locate the files for each unique time
			if cnt1 gt 0 then i1 = where(tt2000_mode1 eq tt2000[i], n1) else n1 = 0
			if cnt2 gt 0 then i2 = where(tt2000_mode2 eq tt2000[i], n2) else n2 = 0
			
			;Should have only one file of each type per interval
			if n1 gt 1 then begin
				oLog -> AddError, 'More than one ' + fmode + ' file found for ' + $
				                  MrCDF_Epoch_Encode(tt2000[i])
				oLog -> AddText, f_mode1[i1]
				continue
			endif
			if n2 gt 1 then begin
				oLog -> AddError, 'More than one SLOW file found for ' + $
				                  MrCDF_Epoch_Encode(tt2000[i])
				oLog -> AddText, f_mode2[i2]
				continue
			endif
			
			;Reformat the time
			;   - File start time for brst or srvy mode
			MrCDF_Epoch_Breakdown, tt2000[i], yr, mo, day, hr, mnt, sec
			fstart = mode[j] eq 'brst' ? string(FORMAT='(%"%04i%02i%02i%02i%02i%02i")', yr, mo, day, hr, mnt, sec) $
			                           : string(FORMAT='(%"%04i%02i%02i")', yr, mo, day)

			;Report start and end
			oLog -> AddText, 'Processing started at:  ' + systime(/UTC)
			f_begin = systime(1)
			
			;Process data
			status_out = mms_edi_amb_l2_sdc(sc[k], mode[j], fstart, $
			                                FILE_OUT  = file_out, $
			                                PACK_MODE = pacmo[p])

			;End of processing time
			f_end = systime(1)
			oLog -> AddText, 'Processing finished at: ' + systime(/UTC)
	
		;-----------------------------------------------------
		; Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		;-----------------------------------------------------
			
			;Save results
			if status_out eq 0 $
				then files[count] = file_out $
				else files[count] = strjoin([sc[k], outinstr, mode[j], outlevel, optdesc, fstart], '_')
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
		endfor ;date
	endfor ;sc
	endfor ;mode
	endfor ;pacmo
	
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

	;Print error code look-up table
	if nwarn + nerror gt 0 then begin
		;Get the error codes and their messages
		txt = mms_edi_amb_error_codes(/TXT)

		;Print them to the log file.
		oLog -> AddText, '-----------------------------------------'
		oLog -> AddText, ''
		oLog -> AddText, txt
		oLog -> AddText, ''
		oLog -> AddText, '-----------------------------------------'
	endif

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