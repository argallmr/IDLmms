; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ql_sdc
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
;       FLATTEN:    in, optional, type=boolean, default=1
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
; :Returns:
;       STATUS:     out, required, type=byte
;                   An error code. Values are:::
;                       0        -  Everything OK
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
;       2015/11/20  -   Written by Matthew Argall
;-
function mms_edi_amb_ql_sdc, fast_file, slow_file, ql_file
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Write error
		MrPrintF, 'LogErr'
		
		;Close log file
		log = MrStdLog(-2)
		
		;Error status
		status  = 1
		ql_file = ''
		
		;Return error code
		return, status
	endif

	;Initialize
	mms_unh_init

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; EDI can be in ambient mode or electric field mode. It may be
	; possible to have a fast or slow file without its complement.
	;
	
	;Data rate mode of the input files
	fmode1  = ''
	fmode2  = ''
	tf_brst = 0

	;First file
	if n_elements(fast_file) gt 0 && fast_file[0] ne '' then begin
		;Check if the file exists
		if ~file_test(fast_file, /READ, /REGULAR) $
			then message, 'EDI AMB file must exist and be readable: "' + fast_file + '".'
		
		;Which mode?
		mms_dissect_filename, fast_file, MODE=fmode1
		tf_brst = fmode1 eq 'brst'
	endif
	
	;Second file
	if n_elements(slow_file) gt 0 && slow_file[0] ne '' then begin
		;Check if the file exists
		if tf_brst then message, 'Only one input parameter allowed for burst processing.'
		if ~file_test(slow_file, /READ, /REGULAR) $
			then message, 'EDI AMB file must exist and be readable: "' + slow_file + '".'
			
		;Which mode
		mms_dissect_filename, slow_file, MODE=fmode2
	endif
	
	;Combine the files
	if fmode1 ne '' then amb_files = fast_file
	if fmode2 ne '' then amb_files = fmode1 eq '' ? slow_file : [amb_files, slow_file]
	
	;Output
	outmode    = tf_brst ? 'brst' : 'srvy'
	outlevel   = 'ql'

;-----------------------------------------------------
; Dissect the Filename \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	mms_dissect_filename, amb_files[0], SC=sc,          INSTR=instr, $
	                                    TSTART=fstart,  VERSION=version, $
	                                    OPTDESC=optdesc

;-----------------------------------------------------
; Create a Log File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Create a log file
	;   - If not, all errors will be written to console (IDL's stderr)
	;
	
	;Current time
	caldat, systime(0, /JULIAN, /UTC), month, day, year, hour, minute, second
	now = string(FORMAT='(%"%04i%02i%02i%02i%02i%02i")', year, month, day, hour, minute, second)

	;Create the file name
	;   - Take file name components from the first input file.
	flog = mms_build_path(!mms_init.log_path, sc, instr, outmode, outlevel, $
	                      optdesc, fstart, version, now + '.log', DEPTH='dd')

	;Create the error logging object
	oLog = MrStdLog(flog)

;-----------------------------------------------------
; Process Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Write parents to log file
	MrPrintF, 'LogText', ''
	MrPrintF, 'LogText', '---------------------------------'
	MrPrintF, 'LogText', '| Parent Files                  |'
	MrPrintF, 'LogText', '---------------------------------'
	if fmode1 ne '' then MrPrintF, 'LogText', fast_file
	if fmode2 ne '' then MrPrintF, 'LogText', slow_file
	MrPrintF, 'LogText', '---------------------------------'
	MrPrintF, 'LogText', ''

	;Process data
	edi_ql = mms_edi_amb_create(amb_files)
	if n_elements(edi_ql) eq 0 then return, 1

;-----------------------------------------------------
; Write Data to File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Create the file
	ql_file = mms_edi_amb_ql_write(temporary(edi_ql), PARENTS=file_basename(amb_files))
	
	;Write destination to log file
	MrPrintF, 'LogText', 'File written to: "' + ql_file + '".'
	
	;Return STATUS=0 -- everything OK
	return, 0
end