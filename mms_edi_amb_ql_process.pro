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
;                   First date of data to be processed. Formatted as 'YYYY-MM-DD'. The
;                       empty string is equivalent to choosing the default value.
;       DATE_END:   in, optional, type=string, default=`DATE_START`
;                   Last date of data to be processed. Formatted as 'YYYY-MM-DD'. If
;                       not defined, the value of `DATE_START` will be used and one
;                       day of data will be processed. If DATE_END is the empty string,
;                       then it will be set equal to the current date.
;
; :Keywords:
;       COUNT:      out, optional, type=integer
;                   Number of files created.
;       CREATE_LOG: in, optional, type=boolean, default=1
;                   If set, a log file will be created. If not, messages will be
;                       directed to the current error logging file (defaults to the
;                       console window -- see MrStdLog.pro)
;       FLATTEN:    in, optional, type=boolean, default=0
;                   If set, all files will be saved to their root directories, without
;                       creating the standard SDC directory structure. This means
;                       `OUTPUT_DIR` and `LOG_DIR` will be the destination directories.
;       OUTPUT_DIR: in, optional, type=string, default='/nfs/edi'
;                   Root directory in which to save data. Files are actually saved to
;                       OUTPUT_DIR/sc/instr/mode/level/year/month[/day] to mimick the
;                       MMS SDC data directory structure. "/day" is included only if
;                       burst files are being processed.
;       LOG_DIR:    in, optional, type=string, default='/nfs/edi'
;                   Root directory in which to save log files. Files are actually saved to
;                       LOG_DIR/sc/instr/mode/level/year/month[/day] to mimick the
;                       MMS SDC data directory structure. "/day" is included only if
;                       burst files are being processed.
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
function mms_edi_amb_ql_process, sc, mode, date_start, date_end, $
COUNT=count, $
CREATE_LOG=create_log, $
FLATTEN=flatten, $
OUTPUT_DIR=output_dir, $
LOG_DIR=log_dir
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		count = 0
		return, ''
	endif

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Calculate the current date
	caldat, systime(/JULIAN, /UTC), month, day, year
	date = string(FORMAT='(%"%4i-%2i-%2i")', year, month, day)

	;Parameters
	if n_elements(sc)         eq 0 || sc[0]      eq '' then sc         = ['mms1', 'mms2', 'mms3', 'mms4']
	if n_elements(mode)       eq 0 || mode[0]    eq '' then mode       = 'srvy'
	if n_elements(date_start) eq 0 || date_start eq '' then date_start = date
	if n_elements(date_end)   eq 0 $
		then date_end = date_start $
		else if date_end eq '' then date_end = date
	
	;Keywords
	create_log = keyword_set(create_log)
	flatten    = keyword_set(flatten)
	if n_elements(sdc_root)   eq 0 then sdc_root   = '/nfs'
	if n_elements(output_dir) eq 0 then output_dir = filepath('', ROOT_DIR=sdc_root, $
	                                                          SUBDIRECTORY=['edi'])
	if n_elements(log_dir)    eq 0 then log_dir    = filepath('', ROOT_DIR=sdc_root, $
	                                                          SUBDIRECTORY=['edi', 'logs'])

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
	if create_log && ~file_test(log_dir, /DIRECTORY, /READ) $
		then message, 'LOG_DIR must exist and be readable.'
	if ~file_test(output_dir) $
		then message, 'OUTPUT_DIR must exist and be readable.'
	

;-----------------------------------------------------
; Mods \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Mods to data processing
	mods = ['v0.0.0 - Original version.']

;-----------------------------------------------------
; Generate Dates \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Generate a series of dates
	syr  = fix(strmid(date_start, 0, 4))
	smo  = fix(strmid(date_start, 5, 2))
	sday = fix(strmid(date_start, 8, 2))
	eyr  = fix(strmid(date_end, 0, 4))
	emo  = fix(strmid(date_end, 5, 2))
	eday = fix(strmid(date_end, 8, 2))
	times = timegen( START = julday(smo, sday, syr), $
	                 FINAL = julday(emo, eday, eyr), $
	                 UNITS = 'days', STEP_SIZE = 1 )

	;Convert to start and end times
	caldat, times, mo, day, yr, hr, mnt, sec
	dates = string(yr,  FORMAT='(i04)') + '-' + $
	        string(mo,  FORMAT='(i02)') + '-' + $
	        string(day, FORMAT='(i02)')
	tstart = dates + 'T00:00:00Z'
	tend   = dates + 'T24:00:00Z'
	
	;Undefine varaibles
	undefine, syr, sm, sday, eyr, emo, eday, times
	undefine, mo, day, yr, hr, mnt, sec, dates
	
;-----------------------------------------------------
; Loop Over Date & Mode \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Allocate memory
	nt     = n_elements(tstart)
	nm     = n_elements(mode)
	nsc    = n_elements(sc)
	nalloc = nsc * nm * nt
	files  = strarr(nalloc)
	count  = 0

	;Loop
	for i = 0, n_elements(tstart) - 1 do begin
	for j = 0, n_elements(mode)   - 1 do begin
	for k = 0, n_elements(sc)     - 1 do begin
		;Start time of file
		year   = strmid(tstart[i], 0, 4)
		month  = strmid(tstart[i], 5, 2)
		day    = strmid(tstart[i], 8, 2)
		fstart = strjoin([year, month, day], '-')
		if mode[j] eq 'brst' then fstart += 'T' + strmid(tstart[i], 11, 2) + $
		                                    ':' + strmid(tstart[i], 14, 2) + $
		                                    ':' + strmid(tstart[i], 17, 2)
	
	;-----------------------------------------------------
	; Create a Log File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Create a log file
		;   - If not, all errors will be written to console (IDL's stderr)
		if create_log then begin
			;Create the directory if it does not exist
			logdir = log_dir
			if ~flatten then mms_mkdir, log_dir, sc[k], 'edi', mode[j], 'ql', 'amb', year+month+day, OUT=logdir
			
			;File version
			version = stregex(mods[-1], '(v[0-9]+\.[0-9]+\.[0-9]+)', /SUBEXP, /EXTRACT)
			version = version[1]
		
			;Create the file name
			log_fname = mms_construct_filename(sc[k], 'edi', mode[j], 'ql', $
			                                   DIRECTORY = logmo, $
			                                   OPTDESC   = 'amb', $
			                                   TSTART    = fstart, $
			                                   VERSION   = version)
			
			;Create the error logging object
			!Null = MrStdLog(log_fname)
		endif

	;-----------------------------------------------------
	; Find EDI Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Input file mode
		fmode = mode[j] eq 'brst' ? mode[j] : ['fast', 'slow']
	
		;Search for the files
		file_edi = mms_find_file(sc[k], 'edi', fmode, 'l1a', $
		                         COUNT     = nedi, $
		                         OPTDESC   = 'amb', $
		                         SDC_ROOT  = sdc_root, $
		                         SEARCHSTR = searchstr, $
		                         TSTART    = tstart[i], $
		                         TEND      = tend[i])
		
		;Did we find any files?
		if nEDI eq 0 then begin
			MrPrintF, 'LogErr', tstart[i], tend[i], FORMAT='(%"EDI files not found for interval %s - %s")'
			continue
		endif

	;-----------------------------------------------------
	; SRVY data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if mode[j] eq 'srvy' then begin
			;Write parents to log file
			MrPrintF, 'LogText', ''
			MrPrintF, 'LogText', '---------------------------------'
			MrPrintF, 'LogText', '| Parent Files                  |'
			MrPrintF, 'LogText', '---------------------------------'
			MrPrintF, 'LogText', file_edi
			MrPrintF, 'LogText', '---------------------------------'
			MrPrintF, 'LogText', ''
			
			;Process data
			edi_ql = mms_edi_amb_create(file_edi)
			if n_elements(edi_ql) eq 0 then continue
			
			;Write data
			;Create the directory if it does not exist
			outdir = output_dir
			if ~flatten then mms_mkdir, output_dir, sc[k], 'edi', mode[j], 'l2', 'amb', year+month+day, OUT=outdir

			;Gather metadata
			meta_data = { sc:        sc[k], $
			              instr:     'edi', $
			              mode:      mode, $
			              mods:      mods, $
			              level:     'l2', $
			              optdesc:   'amb', $
			              tstart:    fstart, $
			              parents:   file_edi, $
			              directory: outdir $
			            }
	
			;Create the file
			files[count] = mms_edi_amb_l2_write(temporary(edi_ql), temporary(meta_data))
			
			;Write destination to log file
			if create_log then MrPrintF, 'LogText', 'File written to: "' + files[count] + '".'
			
			;Increase the file count
			count++

			;Double the allocation?
			if count ge nalloc then files = [files, strarr(count)]

	;-----------------------------------------------------
	; BRST data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		endif else begin
			;Dissect FGM file names
			mms_dissect_filename, file_edi, TSTART=edi_tstart
	
			;Loop through all EDI files
			for m = 0, n_elements(file_edi) - 1 do begin
				;Write parents to log file
				MrPrintF, 'LogText', ''
				MrPrintF, 'LogText', '---------------------------------'
				MrPrintF, 'LogText', '| Parent Files                  |'
				MrPrintF, 'LogText', '---------------------------------'
				MrPrintF, 'LogText', file_edi[m]
				MrPrintF, 'LogText', '---------------------------------'
				MrPrintF, 'LogText', ''
	
				;Create the ambient quicklook file
				edi_ql = mms_edi_amb_create(file_edi[m])
				if n_elements(edi_ql) eq 0 then continue
				
				;Create the directory if it does not exist
				outdir = output_dir
				if ~flatten then mms_mkdir, output_dir, sc[k], 'edi', mode[j], 'l2', 'amb', year+month+day, OUT=outdir
				
				;File time
				MrTimeParser, edi_tstart[m], '%Y%M%d%H%m%S', '%Y-%M-%dT%H:%m:%S', fstart
				
				;Gather metadata
				meta_data = { sc:        sc[k], $
				              instr:     'edi', $
				              mode:      mode, $
				              mods:      mods, $
				              level:     'l2', $
				              optdesc:   'amb', $
				              tstart:    fstart, $
				              parents:   file_edi, $
				              directory: outdir $
				            }
		
				;Create the file
				files[count] = mms_edi_amb_l2_write(temporary(edi_ql), temporary(meta_data))
				
				;Write destination to log file
				if create_log then MrPrintF, 'LogText', 'File written to: "' + files[count] + '".'
				
				;Increase the file count
				count++
	
				;Double the allocation?
				if count ge nalloc then files = [files, strarr(count)]
			endfor
		endelse
	endfor ;tstart
	endfor ;mode
	endfor ;date
	
	;Trim extra elements
	if count eq 1 $
		then files = files[0] $
		else files = files[0:count-1]
	
	;Return the files
	return, files
end