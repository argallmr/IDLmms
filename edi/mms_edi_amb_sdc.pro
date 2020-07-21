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
; :Categories:
;    MMS, EDI, QL, Ambient
;
; :Params:
;       SC:                 in, required, type=string
;                           Spacecraft ID of the data to be processed. Choices are:
;                               'mms1', 'mms2', 'mms3', 'mms4'
;       MODE:               in, required, type=string
;                           Data rate mode of the data to be processd. Choices are:
;                               'slow', 'fast', 'srvy', 'brst'
;       TSTART:             in, required, type=string
;                           Start time of the file(s) to be processed, formatted as
;                               'YYYYMMDDhhmmss' for burst mode and 'YYYYMMDD' otherwise.
;                               TSTART must match the start time in the file names to
;                               be processed.
;
; :Keywords:
;       ALTERNATING:        in, optional, type=boolean, default=1
;                           If set, alternating mode data files will be produce.
;       DATA_PATH_ROOT:     in, optional, type=string, default=!mms_init.data_path
;                           Root of the SDC-like directory structure where data files
;                               find their final resting place.
;       DROPBOX_ROOT:       in, optional, type=string, default=!mms_init.dropbox
;                           Directory into which data files are initially saved.
;       FIELD_ALIGNED:      in, optional, type=boolean, default=1
;                           If set, field_aligned mode data files will be produce.
;       FILE_OUT:           out, optional, type=string
;                           Named variable to receive the name of the output file.
;       LOG_PATH_ROOT:      in, optional, type=string, default=!mms_init.log_path
;                           Root directory into which log files are saved.
;       PERPENDICULAR:      in, optional, type=boolean, default=1
;                           If set, perpendicular mode data files will be produce.
;
; :Returns:
;       STATUS:             out, required, type=byte
;                           An error code. Values are:::
;                               OK      = 0
;                               Warning = 1-99
;                               Error   = 100-255
;                                   100      -  Trapped error
;                                   101      -  Bad inputs given
;                                   102      -  No EDI files found
;                                   105      -  Error from mms_edi_amb_create
;                                   110      -  Error from mms_edi_amb_ql_write
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
;       2015/11/24  -   Errors return error code 100 (error) instead of 1 (warning) - MRA
;       2016/01/15  -   Changed in puts from FAST_FILE, SLOW_FILE, QL_FILE to
;                           SC, MODE, TSTART. - MRA
;       2019/08/21  -   Added the ALTERNATIONG, FIELD_ALIGNED, and PERPENDICULAR keywords. - MRA
;-
function mms_edi_amb_sdc, sc, mode, level, tstart, $
ALTERNATING=alternating, $
DATA_PATH_ROOT=data_path, $
DROPTBOX_ROOT=dropbox, $
FIELD_ALIGNED=field_aligned, $
FILE_OUT=file_out, $
LOG_PATH_ROOT=log_path, $
PERPENDICULAR=perpendicular
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Write error
		MrPrintF, 'LogErr'
		
		;Close log file
		log = MrStdLog(-2)
		
		;Unexpected trapped error
		if n_elements(status) eq 0 then status  = 100
		ql_file = ''
		
		;Return error status
		return, status
	endif
	
	;Start timer
	t0 = systime(1)

	;Initialize
	;   - Setup directory structure
	unh_edi_amb_init

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Assume error with inputs
	status = 100

	;Check type
	if ~isa(sc,     /SCALAR, 'STRING') then message, 'SC must be a scalar string.'
	if ~isa(mode,   /SCALAR, 'STRING') then message, 'MODE must be a scalar string.'
	if ~isa(level,  /SCALAR, 'STRING') then message, 'LEVEL must be a scalar string.'
	if ~isa(tstart, /SCALAR, 'STRING') then message, 'TSTART must be a scalar string.'
	
	;Check value
	if max(sc eq ['mms1', 'mms2', 'mms3', 'mms4']) eq 0 $
		then message, 'SC must be "mms1", "mms2", "mms3", or "mms4".'
	if max(level eq ['ql', 'l2']) eq 0 $
		then message, 'LEVEL must be "ql" or "l2".'
	if max(mode eq ['brst', 'srvy']) eq 0 $
		then message, 'MODE must be "srvy" or "brst".'
	
	;Defaults
	if n_elements(data_path) eq 0 then data_path = !edi_amb_init.data_path
	if n_elements(dropbox)   eq 0 then dropbox   = !edi_amb_init.dropbox
	if n_elements(log_path)  eq 0 then log_path  = !edi_amb_init.log_path

	;Check permissions
	if ~file_test(log_path, /DIRECTORY, /WRITE) $
		then message, 'LOG_PATH_ROOT must exist and be writeable.'
	if ~file_test(data_path, /DIRECTORY, /READ) $
		then message, 'DATA_PATH_ROOT directory must exist and be readable.'
	if ~file_test(dropbox, /DIRECTORY, /READ, /WRITE) $
		then message, 'DROPBOX_ROOT directory must exist and be read- and writeable.'
	
	;Which data products to create
	tf_alt  = n_elements(alternationg)  eq 0 ? 1B : keyword_set(alternating)
	tf_fa   = n_elements(field_aligned) eq 0 ? 1B : keyword_set(field_aligned)
	tf_perp = n_elements(perpendicular) eq 0 ? 1B : keyword_set(perpendicular)
	if ~tf_fa && ~tf_perp && ~tf_alt then begin
		tf_fa   = 1B
		tf_perp = 1B
		tf_alt  = 1B
	endif

	;Constants for data to be processed
	instr   = 'edi'
	lvl     = 'l1a'
	optdesc = 'amb'
	status  = 0
	
	;Constants for output
	outmode    = mode
	outlevel   = level
	outoptdesc = 'amb'

;-----------------------------------------------------
; Create Log File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Parse input time
	mms_parse_time, tstart, syr, smo, sday, shr, smin, ssec
	
	;Current time
	caldat, systime(0, /JULIAN, /UTC), month, day, year, hour, minute, second
	now = string(FORMAT='(%"%04i%02i%02i_%02i%02i%02i")', year, month, day, hour, minute, second)

	;Build log file
	fLog = strjoin([sc, instr, outmode, outlevel, outoptdesc, tstart, now], '_') + '.log'
	
	;Build log directory
	;   - Create the directory if it does not exist
	;   - log_path/amb/ql/mode/year/month[/day]
	fDir = mode eq 'brst' ? filepath('', ROOT_DIR=log_path, SUBDIRECTORY=[sc, instr, mode, outlevel, optdesc, syr, smo, sday]) $
	                      : filepath('', ROOT_DIR=log_path, SUBDIRECTORY=[sc, instr, mode, outlevel, optdesc, syr, smo])
	if ~file_test(fDir, /DIRECTORY) then file_mkdir, fDir
	
	;Create the log file
	!Null = MrStdLog(filepath(fLog, ROOT_DIR=fDir))

;-----------------------------------------------------
; Find FAST/BRST file \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if mode eq 'brst' || mode eq 'srvy' || mode eq 'fast' then begin
		;fast or burst?
		fmode = mode eq 'brst' ? mode : 'fast'
	
		;Search for the file
		edi_files = mms_latest_file(dropbox, sc, instr, fmode, level, tstart, $
		                            OPTDESC=optdesc, ROOT=data_path)
		
		;No FAST/BRST files found
		if edi_files eq '' then begin
			MrPrintF, 'LogText', string(sc, instr, fmode, level, optdesc, tstart, $
			                            FORMAT='(%"No %s %s %s %s %s files found for start time %s.")')
		endif
	endif
	
;-----------------------------------------------------
; Find SLOW Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;No "slow" files if we are searching for "brst"
	if mode eq 'srvy' || mode eq 'slow' then begin
		slow_file = mms_latest_file(dropbox, sc, instr, 'slow', lvl, tstart, $
		                            OPTDESC=optdesc, ROOT=data_path)
		
		;No SLOW files found
		if slow_file eq '' then begin
			MrPrintF, 'LogText', string(sc, instr, 'slow', lvl, optdesc, tstart, $
			                            FORMAT='(%"No %s %s %s %s %s files found for start time %s.")')
		endif
		
		;Combine slow and fast
		if mode eq 'srvy' && edi_files ne '' then begin
			if slow_file ne '' then edi_files = [slow_file, edi_files]
		endif else begin
			edi_files = slow_file
		endelse
	endif

	;Zero files found
	if edi_files[0] eq '' then begin
		status = 101
		message, 'No EDI files found.'
	endif

;-----------------------------------------------------
; Process Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Write parents to log file
	MrPrintF, 'LogText', ''
	MrPrintF, 'LogText', '---------------------------------'
	MrPrintF, 'LogText', '| Parent Files                  |'
	MrPrintF, 'LogText', '---------------------------------'
	MrPrintF, 'LogText', edi_files
	MrPrintF, 'LogText', '---------------------------------'
	MrPrintF, 'LogText', ''

	;Process data
	edi_data = mms_edi_amb_create(edi_files, STATUS=status)
	if status ne 0 then message, 'Unable to create amb ' + outlevel + ' data.'
	
	;Do not create field-aligned data product
	tags = tag_names(edi_data)
	if ~tf_fa then begin
		fa_modes = ['amb', 'amb-pm2']
		tf_member = MrIsMember(tags, fa_modes, imember, COUNT=nMember, /FOLD_CASE)
		if nMember gt 0 then edi_data = MrStruct_RemoveTags(edi_data, fa_modes[iMember]) $
	endif
	
	;Do not create perpendicular data product
	tags = tag_names(edi_data)
	if ~tf_fa then begin
		perp_modes = ['amb-perp-c', 'amb-perp-ob', 'amb-perp-om']
		tf_member = MrIsMember(tags, perp_modes, imember, COUNT=nMember, /FOLD_CASE)
		if nMember gt 0 then edi_data = MrStruct_RemoveTags(edi_data, perp_modes[iMember]) $
	endif
	
	;Do not create perpendicular data product
	tags = tag_names(edi_data)
	if ~tf_fa then begin
		alt_modes = ['amb-alt-cc', 'amb-alt-oc', 'amb-alt-oob', 'amb-alt-oom']
		tf_member = MrIsMember(tags, alt_modes, imember, COUNT=nMember, /FOLD_CASE)
		if nMember gt 0 then edi_data = MrStruct_RemoveTags(edi_data, alt_modes[iMember]) $
	endif
	
;-----------------------------------------------------
; Write Data to File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Create the file
	if outlevel eq 'ql' then begin
		file_out = mms_edi_amb_ql_write(sc, mode, tstart, temporary(edi_data), $
		                                DROPBOX_ROOT   = dropbox, $
		                                DATA_PATH_ROOT = data_path, $
		                                OPTDESC        = outoptdesc, $
		                                PARENTS        = file_basename(edi_files))
	endif else begin
		file_out = mms_edi_amb_l2_write(sc, mode, tstart, temporary(edi_data), $
		                                DROPBOX_ROOT   = dropbox, $
		                                DATA_PATH_ROOT = data_path, $
		                                OPTDESC        = outoptdesc, $
		                                PARENTS        = file_basename(edi_files))
	endelse
	if file_out eq '' then message, 'Error writing QL file.'

	;Time elapsed
	dt     = systime(1) - t0
	dt_hr  = floor((dt) / 3600.0)
	dt_min = floor( (dt mod 3600.0) / 60.0 )
	dt_sec = dt mod 60
	
	;Write destination to log file
	MrPrintF, 'LogText', file_out, FORMAT='(%"File written to:    \"%s\".")'
	MrPrintF, 'LogText', dt_hr, dt_min, dt_sec, FORMAT='(%"Total process time: %ihr %imin %0.3fs")'
	
	;Return STATUS: 0 => everything OK
	return, status
end