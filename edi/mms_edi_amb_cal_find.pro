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
;       2015/02/25  -   Remove TSTART parameter because there is only one cal file. - MRA
;       2016/03/23  -   Full SDC directory tree used. - MRA
;-
function mms_edi_amb_cal_find, sc, $
CAL_PATH_ROOT=cal_path_root
	compile_opt idl2
	on_error, 2
	
	;Only one cal file per spacecraft. TSTART is constant.
	tstart = '20150312'
	
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
			defsysv, '!edi_init', EXIST=tf_exist
			if tf_exist $
				then cal_path_root = !edi_init.cal_path_root $
				else message, 'Cannot determine CAL_PATH_ROOT.'
		endif
	endif
	
	;Callibrations are stored in CAL_PATH_ROOT/SC/EDI
	cal_path = filepath('', ROOT_DIR=cal_path_root, $
	                    SUBDIRECTORY=[sc, 'edi', 'cal', 'l2', 'amb'])
	
;------------------------------------------------------
; Search for Files                                    |
;------------------------------------------------------

	;Search for all versions and all times
	fname     = mms_forge_filename(sc, 'edi', 'cal', 'l2', tstart, OPTDESC='amb')
	cal_files = file_search(cal_path, fname, COUNT=count)
	
	;No cal files
	if count eq 0 then begin
		MrPrintF, 'LogWarn', 'No EDI cal file found for spacecraft ' + sc + '.'
		return, ''
	endif
	
	;TT2000 value of file's start time
	mms_dissect_filename, cal_files, TT2000=tt2000_file
	
;------------------------------------------------------
; Latest Version                                      |
;------------------------------------------------------
	
	;Search for the latest version
	if count gt 0 then cal_file = mms_latest_version(cal_files)

	return, cal_file
end