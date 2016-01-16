; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ql_process
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Process EDI ambient mode data to produce a quick-look data product with counts
;   sorted by 0 and 180 degree pitch angle.
;
;   Calling Sequence::
;       FILES = mms_edi_ql_amb_process()
;       FILES = mms_edi_ql_amb_process(SC)
;       FILES = mms_edi_ql_amb_process(SC, MODE)
;       FILES = mms_edi_ql_amb_process(SC, MODE, DATE_START)
;       FILES = mms_edi_ql_amb_process(SC, MODE, DATE_START, '')
;       FILES = mms_edi_ql_amb_process(SC, MODE, DATE_START, DATE_END)
;
; :Categories:
;    MMS, EDI, QL, Ambient
;
; :Params:
;       SC:         in, optional, type=string/strarr, default=['mms1'\, 'mms2'\, 'mms3'\, 'mms4']
;                   Spacecraft for which to process data. The empty string is equivalent
;                       to choosing the default value. Options are::
;                       mms1, mms2, mms3, or mms4
;       MODE:       in, optional, type=string/strarr, default='srvy'
;                   Telemetry mode of the data to be processed. The empty string is
;                       equivalent to choosing the default value. Options are: 
;                       'srvy' and/or 'brst'
;       DATE_START: in, optional, type=string, default=current date
;                   First date of data to be processed. Formatted as 'YYYYMMDD' or
;                       'YYYYMMDDhhmmss'. The empty string is equivalent to choosing
;                       the default value.
;       DATE_END:   in, optional, type=string, default=`DATE_START`
;                   Last date of data to be processed. Formatted as 'YYYYMMDD' or
;                       'YYYYMMDDhhmmss'. All files between `DATE_START` and DATE_END
;                       are processed. If not defined, the value of `DATE_START`
;                       will be used and the file for which the start date is closes to
;                       DATE_START without going over will be processed. If DATE_END
;                       is the empty string, then the current date is used.
;
; :Keywords:
;       COUNT:      out, optional, type=integer
;                   Number of files created.
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
;
; :Returns:
;       STATUS:     out, required, type=byte
;                   Error code. Values are::
;                       0        -  Hunkey Dorey
;                       1-99     -  Warning
;                       100-255  -  Error
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
function mms_edi_amb_ql_process_v2, sc, mode, date_start, date_end, $
COUNT=count, $
NO_LOG=no_log, $
DATA_PATH=data_path, $
DROPBOX=dropbox, $
LOG_PATH=log_path
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		oLog -> AddError
		obj_destroy, oLog
		status = 1
		return, status
	endif
	
	;Everything starts out ok
	status = 0
	
	;Initialize
	mms_unh_init
	
	;Create an error logging object
	oLog = MrLogFile(filepath('mms_edi_amb_ql.log', ROOT_DIR=!mms_init.log_path), /TIMESTAMP)

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Calculate the current date
	caldat, systime(/JULIAN), month, day, year
	date = string(FORMAT='(%"%4i%2i%2i")', year, month, day)

	;Parameters
	if n_elements(sc)         eq 0 || sc[0]      eq '' then sc         = ['mms1', 'mms2', 'mms3', 'mms4']
	if n_elements(mode)       eq 0 || mode[0]    eq '' then mode       = ['srvy', 'brst']
	if n_elements(date_start) eq 0 || date_start eq '' then date_start = date
	if n_elements(date_end)   eq 0 $
		then date_end = date_start $
		else if date_end eq '' then date_end = date

	;Keywords
	no_log = keyword_set(no_log)
	if n_elements(data_path_in) eq 0 then data_path = !mms_init.data_path else data_path = data_path_in
	if n_elements(dropbox_in)   eq 0 then dropbox   = !mms_init.dropbox   else dropbox   = dropbox_in
	if n_elements(log_path_in)  eq 0 then log_path  = !mms_init.log_path  else log_path  = log_path_in

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of elements
	nSC    = n_elements(sc)
	nMode  = n_elements(mode)
	nStart = n_elements(date_start)
	nEnd   = n_elements(date_end)
	
	;Unique values
	if n_elements(uniq(sc,   sort(sc)))   ne nSC   then message, 'SC must contain only unique values.'
	if n_elements(uniq(mode, sort(mode))) ne nMode then message, 'MODE must contain only unique values.'
	if nStart ne 1 then message, 'DATE_START must be a scalar string.'
	if nEnd   ne 1 then message, 'DATE_END must be a scalar string.'
	
	;Valid SC and MODE
	if min(MrIsMember(['mms1', 'mms2', 'mms3', 'mms4'], sc)) eq 0 $
		then message, 'Invalid spacecraft ID given.'
	if min(MrIsMember(['brst', 'srvy'], mode)) eq 0 $
		then message, 'MODE mode be "brst" and/or "srvy"'
	
	;Directories must be writable
	if ~no_log && ~file_test(log_path, /DIRECTORY, /WRITE) $
		then message, 'LOG_PATH must exist and be writeable.'
	if ~file_test(data_path, /DIRECTORY, /READ, /WRITE) $
		then message, 'DATA_PATH directory must exist and be readable.'
	if ~file_test(dropbox, /DIRECTORY, /READ, /WRITE) $
		then message, 'DROPBOX directory must exist and be read- and writeable.'
	

;-----------------------------------------------------
; Generate Dates \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Parse the start and end times
	mms_parse_time, date_start, syr, smo, sday, shr, smin, ssec, /INTEGER
	mms_parse_time, date_end,   eyr, emo, eday, ehr, emin, esec, /INTEGER

	;Form the start and stop times
	tstart = string(syr, smo, sday, shr, smin, ssec, $
	                FORMAT='(%"%04i-%02i-%02iT%02i:%02i:%02iZ")')
	tend   = string(eyr, emo, eday, ehr, emin, esec, $
	                FORMAT='(%"%04i-%02i-%02iT%02i:%02i:%02iZ")')

	;Constants
	instr   = 'edi'
	level   = 'l1a'
	optdesc = 'amb'

;-----------------------------------------------------
; Loop Over Date & Mode \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Allocate memory
	nalloc     = 100
	ql_files   = strarr(nalloc)
	ql_code    = bytarr(nalloc)
	ql_count   = 0

	;Loop
	for j = 0, n_elements(mode) - 1 do begin
	for k = 0, n_elements(sc)   - 1 do begin
		;Starting a new sc/mode
		oLog -> AddText, '------------------------------------------------'
		oLog -> AddText, '################################################'
		oLog -> AddText, string(sc[k], instr, mode[j], level, optdesc, tstart, tend, $
		                        FORMAT='(%"PROCESSING %s %s %s %s %s %s - %s")')
		oLog -> AddText, ''
		
		
		;L1A has SLOW and FAST, not SRVY
		;   - Search for FAST first, then SLOW
		fmode = mode[j] eq 'brst' ? 'brst' : 'fast'
	
	;-----------------------------------------------------
	; Find FAST/BRST Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Find FAST/BRST files
		fast_files = mms_find_file(sc[k], instr, fmode, level, $
		                           COUNT   = fast_count, $
		                           OPTDESC = optdesc, $
		                           TSTART  = tstart, $
		                           TEND    = tend)
		
		;No FAST/BRST files found
		if fast_count eq 0 then begin
			oLog -> AddText, string(sc[k], instr, fmode, level, optdesc, tstart, tend, $
			                        FORMAT='(%"No %s %s %s %s %s files found in interval %s - %s")')
		endif
	
	;-----------------------------------------------------
	; Find SLOW Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if fmode eq 'fast' then begin
			slow_files = mms_find_file(sc[k], instr, 'slow', level, $
			                           COUNT   = slow_count, $
			                           OPTDESC = optdesc, $
			                           TSTART  = tstart, $
			                           TEND    = tend)
		
			;No SLOW files found
			if slow_count eq 0 then begin
				oLog -> AddText, string(sc[k], instr, 'slow', level, optdesc, tstart, tend, $
				                        FORMAT='(%"No %s %s %s %s %s files found in interval %s - %s")')
			endif
		endif else begin
			slow_count = 0
		endelse
	
		;Zero files found
		if fast_count + slow_count eq 0 then begin
			oLog -> AddError, 'No files found. Skipping.'
			continue
		endif
	
	;-----------------------------------------------------
	; Sort Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Gather start times of all files
		if fast_count gt 0 then mms_dissect_filename, fast_files, TT2000=fast_tt2000
		if slow_count gt 0 then mms_dissect_filename, slow_files, TT2000=slow_tt2000
		
		;Combine the start times into a single array
		if fast_count gt 0 && slow_count gt 0 then begin
			tt2000 = [fast_tt2000, slow_tt2000]
		endif else if fast_count gt 0 then begin
			tt2000 = fast_tt2000
		endif else begin
			tt2000 = slow_tt2000
		endelse
		
		;Extract the uniq times
		tt2000 = tt2000[uniq(tt2000, sort(tt2000))]
		nTimes = n_elements(tt2000)
	
	;-----------------------------------------------------
	; Process Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		for i = 0, nTimes - 1 do begin
			;Locate the files for each unique time
			ifast = where(fast_tt2000 eq tt2000[i], nfast)
			if slow_count gt 0 $
				then islow = where(slow_tt2000 eq tt2000[i], nslow) $
				else nslow = 0
			
			;Should have only one file of each type per interval
			if nfast gt 1 then begin
				oLog -> AddError, 'More than one FAST file found for ' + $
				                  MrCDF_Epoch_Encode(tt2000[i])
				for i = 0, nfast do oLog -> AddText, fast_files[ifast[i]]
				continue
			endif
			if nslow gt 1 then begin
				oLog -> AddError, 'More than one SLOW file found for ' + $
				                  MrCDF_Epoch_Encode(tt2000[i])
				for i = 0, nfast do oLog -> AddText, slow_files[islow[i]]
				continue
			endif
			
			;Extract the files
			if nfast eq 1 $
				then amb_fast = fast_files[ifast] $
				else amb_fast = ''
			if nslow eq 1 $
				then amb_slow = slow_files[islow] $
				else amb_slow = ''
			
			;process the data
			code = mms_edi_amb_ql_sdc(amb_fast, amb_slow, amb_ql)
	
		;-----------------------------------------------------
		; Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		;-----------------------------------------------------
			
			;Save results
			ql_files[ql_count] = amb_ql
			ql_code[ql_count]  = code
			ql_count++
			
			;Allocate more memory (double allocation each time)
			if ql_count gt nalloc then begin
				nalloc   += ql_count
				ql_code   = [ql_code,  bytarr(ql_count)]
				ql_files  = [ql_files, strarr(ql_count)]
			endif
			
			;Report results
			oLog -> AddText, 'Finished processing'
			oLog -> AddText, '   Fast file:   "' + amb_fast + '"'
			oLog -> AddText, '   Slow file:   "' + amb_slow + '"'
			oLog -> AddText, '   Output file: "' + amb_ql   + '"'
			oLog -> AddText, '   Error code:  "' + string(code, FORMAT='(i0)') + '"'
			oLog -> AddText, '##############################################'
			oLog -> AddText, '----------------------------------------------'
			oLog -> AddText, ''
		endfor
	endfor ;mode
	endfor ;date
	
;-----------------------------------------------------
; Executive Summary \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Trim results
	ql_files = ql_files[0:ql_count - 1]
	ql_code  = ql_code[0:ql_count - 1]

	;Number of errors
	ierror = where(ql_code ne 0, nerror, COMPLEMENT=iwrite)
	if nerror ne 0 then status = 1

	;Log summary information
	oLog -> AddText, ''
	oLog -> AddText, '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
	oLog -> AddText, '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
	oLog -> AddText, 'EXECUTIVE SUMMARY'
	oLog -> AddText, '   Number of Files:  ' + strtrim(ql_count, 2)
	oLog -> AddText, '   Number of Errors: ' + strtrim(nerror,   2)
	oLog -> AddText, '   Code    Name'
	oLog -> AddText, '    ' + string(ql_code, FORMAT='(i3)') + '    ' + ql_files
	oLog -> AddText, '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
	oLog -> AddText, '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
	
	;Close log object
	obj_destroy, oLog
	return, status
end