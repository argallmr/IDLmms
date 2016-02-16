;
; docformat = 'rst'
;
; NAME:
;   mms_edi_amb_cal_find 
;
; PURPOSE:
;+
;   Find an MMS data file.
;
; :Categories:
;    MMS, EDI, AMB, CAL
;
; :Params:
;       SC:             in, required, type=string
;                       Spacecraft (e.g., 'mms1').
;       TSTART:         in, required, type=string
;                       Start time, formatted as either is 'YYYYMMDD' or 'YYYYMMDDhhmmss'.
;                           This program first searches for the calibration file closest
;                           to TSTART without going over. If it finds nothing, it will
;                           search for the first cal file after TSTART. The latest version
;                           of the cal file is always selected.
;
; :Keywords:
;       CAL_PATH_ROOT:  in, optional, type=integer
;                       Root of the calibration file directory, which is assumed to be
;                           CAL_PATH_ROOT/`SC`/edi/. If not given, the program will
;                           check for the environment variable CAL_PATH_ROOT. If that
;                           is not found, it will check for the system variable
;                           !edi_amb_init (see unh_edi_amb_init.pro). If none of these
;                           efforts are successful, an error is thrown.
;
; :Returns:
;       FILES:          Name of the latest version of the calibration file closest in
;                           time to `TSTART`.
;
; :See Also:
;   mms_latest_version.pro
;   mms_latest_zversion.pro
;   mms_latest_file.pro
;   mms_edi_amb_cal_read.pro
;   unh_edi_amb_init.pro
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 348
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/01/30  -   Written by Matthew Argall
;-
function mms_edi_amb_cal_find, sc, tstart, $
CAL_PATH_ROOT=cal_path_root
	compile_opt idl2
	on_error, 2
	
;------------------------------------------------------
; Check Inputs                                        |
;------------------------------------------------------

	;Parse the input time
	mms_parse_time, tstart, TT2000=tt2000_start

	;Cal path. Can be in two places:
	;   - The environment variable CAL_PATH_ROOT
	;   - The system variable !EDI_AMB_INIT (unh_edi_amb_init.pro)
	if n_elements(cal_path_root) eq 0 then begin
		cal_path_root = getenv('CAL_PATH_ROOT')
		if cal_path_root eq '' then begin
			defsysv, '!edi_amb_init', EXIST=tf_exist
			if tf_exist $
				then cal_path_root = !edi_amb_init.cal_path_root $
				else message, 'Cannot determine CAL_PATH_ROOT.'
		endif
	endif
	
	;Callibrations are stored in CAL_PATH_ROOT/SC/EDI
	cal_path = filepath('', ROOT_DIR=cal_path_root, SUBDIRECTORY=[sc, 'edi'])
	
;------------------------------------------------------
; Search for Files                                    |
;------------------------------------------------------

	;Search for all versions and all times
	fname     = mms_forge_filename(sc, 'edi', 'cal', 'l2', '*', OPTDESC='amb')
	cal_files = file_search(cal_path, fname, COUNT=count)
	
	;No cal files
	if count eq 0 then begin
		MrPrintF, 'LogWarn', 'No EDI cal file found for spacecraft ' + sc + '.'
		return, ''
	endif
	
	;TT2000 value of file's start time
	mms_dissect_filename, cal_files, TT2000=tt2000_file
	
;------------------------------------------------------
; Most Recent Cal File                                |
;------------------------------------------------------

	;Find the file before the given time
	ical = where(tt2000_file le tt2000_start, count)
	if count gt 0 then begin
		;Select the last file before the given time
		;   - There may be more than one version with the same start time.
		tt2000    = tt2000_file[count-1]
		ical      = where(tt2000_file eq tt2000, count)
		cal_files = cal_files[ical]
	
	;No cal file found
	endif else begin
		;Issue warning
		MrPrintF, 'LogWarn', 'No EDI cal file found on or before ' + tstart + '.'
		cal_file = ''
		count    = 0
	endelse
	
;------------------------------------------------------
; Latest Version                                      |
;------------------------------------------------------
	
	;Search for the latest version
	if count gt 0 then cal_file = mms_latest_version(cal_files)

	return, cal_file
end