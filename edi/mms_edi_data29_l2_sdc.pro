; docformat = 'rst'
;
; NAME:
;    mms_edi_efield_l2pre_sdc
;
; PURPOSE:
;+
;   Process EDI electric field mode data to produce a Level 2 data product.
;
; :Categories:
;    MMS, EDI, QL, EField
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
;       DATA_PATH_ROOT:     in, optional, type=string, default=!mms_init.data_path
;                           Root of the SDC-like directory structure where data files
;                               find their final resting place.
;       DROPBOX_ROOT:       in, optional, type=string, default=!mms_init.dropbox
;                           Directory into which data files are initially saved.
;       FILE_OUT:           out, optional, type=string
;                           Named variable to receive the name of the output file.
;       LOG_PATH_ROOT:      in, optional, type=string, default=!mms_init.log_path
;                           Root directory into which log files are saved.
;
; :Returns:
;       STATUS:             out, required, type=byte
;                           An error code. Values are:::
;                               OK      = 0
;                               Warning = 1-99
;                               Error   = 100-255
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
;       2016/01/16  -   Written by Matthew Argall
;-
function mms_edi_data29_l2_sdc, sc, tstart, $
DATA_PATH_ROOT=data_path_root, $
DROPTBOX_ROOT=dropbox_root, $
FILE_OUT=l2_file, $
LOG_PATH_ROOT=log_path_root, $
UNH_DATA_PATH=unh_data_path
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Write error
		MrPrintF, 'LogErr'
		
		;Close log file
		log = MrStdLog(-2)
		
		;Unexpected trapped error
		if n_elements(status) eq 0 || status eq 0 then status  = 100
		l2_file = ''
		
		;Return error status
		return, status
	endif

	;Initialize
	;   - Setup directory structure
	unh_edi_amb_init

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Check type
	if ~isa(sc,     /SCALAR, 'STRING') then message, 'SC must be a scalar string.'
	if ~isa(tstart, /SCALAR, 'STRING') then message, 'TSTART must be a scalar string.'
	
	;Check value
	if max(sc eq ['mms1', 'mms2', 'mms3', 'mms4']) eq 0 $
		then message, 'SC must be "mms1", "mms2", "mms3", or "mms4".'
	
	;Defaults
	if n_elements(data_path_root) eq 0 then data_path = !edi_amb_init.data_path_root
	if n_elements(dropbox_root)   eq 0 then dropbox   = !edi_amb_init.dropbox_root
	if n_elements(log_path_root)  eq 0 then log_path  = !edi_amb_init.log_path_root
	if n_elements(unh_data_root)  eq 0 then unh_path  = !edi_amb_init.unh_data_root

	;Check permissions
	if ~file_test(log_path, /DIRECTORY, /WRITE) $
		then message, 'LOG_PATH_ROOT must exist and be writeable.'
	if ~file_test(data_path, /DIRECTORY, /READ) $
		then message, 'DATA_PATH_ROOT directory must exist and be readable.'
	if ~file_test(dropbox, /DIRECTORY, /READ, /WRITE) $
		then message, 'DROPBOX_ROOT directory must exist and be read- and writeable.'

	;Constants for source files
	instr   = 'edi'
	mode    = 'brst'
	level   = 'l1a'
	optdesc = 'efield'
	status  = 0
	
	;Constants for output
	outmode    = 'brst'
	outlevel   = 'l2'
	outoptdesc = 'data29'

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
	fDir = mode eq 'brst' ? filepath('', ROOT_DIR=log_path, SUBDIRECTORY=[sc, instr, outmode, outlevel, outoptdesc, syr, smo, sday]) $
	                      : filepath('', ROOT_DIR=log_path, SUBDIRECTORY=[sc, instr, outmode, outlevel, outoptdesc, syr, smo])
	if ~file_test(fDir, /DIRECTORY) then file_mkdir, fDir

	;Create the log file
	!Null = MrStdLog(filepath(fLog, ROOT_DIR=fDir))

;-----------------------------------------------------
; Find file \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Search for the file
	edi_files = mms_latest_file(dropbox, sc, instr, mode, level, tstart, $
	                            OPTDESC=optdesc, ROOT=data_path)
	
	;No FAST/BRST files found
	if edi_files eq '' then begin
		MrPrintF, 'LogText', string(sc, instr, mode, level, optdesc, tstart, $
		                            FORMAT='(%"No %s %s %s %s %s files found for start time %s.")')
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
	d29_data = mms_edi_data29_l2_create(edi_files, STATUS=status)
	if status ge 100 then message, 'Error creating L2 data.'

;-----------------------------------------------------
; Write Data to File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	parents = file_basename(edi_files)

	;Create the file
	l2_file = mms_edi_data29_l2_write(sc, mode, tstart, temporary(d29_data), $
	                                  DROPBOX   = dropbox, $
	                                  DATA_PATH = unh_path, $
	                                  OPTDESC   = outoptdesc, $
	                                  PARENTS   = parents)
	if l2_file eq '' then message, 'Error writing L2 file.'
	
	;Write destination to log file
	MrPrintF, 'LogText', 'File written to: "' + l2_file + '".'
	
	;Close the log file by returning output to stderr
	!Null = MrStdLog('stderr')
	
	;Return STATUS: 0 => everything OK
	return, status
end