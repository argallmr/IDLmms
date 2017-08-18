; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_cal_addvalue
;
; PURPOSE:
;+
;   Add values to the EDI calibration file.
;
; :Categories:
;    MMS, EDI, L2, Ambient
;
; :See Also:
;   mms_edi_amb_cal_apply.pro
;   mms_edi_amb_cal_delvalue.pro
;   mms_edi_amb_cal_find.pro
;   mms_edi_amb_cal_read.pro
;   mms_edi_amb_cal_update.pro
;   mms_edi_amb_cal_write.pro
;
; :History:
;    Modification History::
;       2016/01/08  -   Written by Matthew Argall
;       2016/02/23  -   Added optics state during relative and absolute calibrations. - MRA
;       2016/02/25  -   Replace individual values, added VERSION and CLOBBER keywords. - MRA
;-
;*****************************************************************************************
;+
;   Add values to the EDI calibration file.
;
; :Params:
;       FILE:               in, required, type=string
;                           Calibration file to which to add values.
;       CAL_DATA:           in, required, type=structarr
;                           EDI ambient data structure with any of the fields below.
;                             Time tags must be given. A new entry will be appended
;                             in the appropriate location. If a duplicate entry is given,
;                             variables present in the structure will overwrite variables
;                             in the cal file. Other cal file variables remain unchanged.
;                               TT2000_REL   -  TT2000 time stamps for relative calibration data
;                               TT2000_ABS   -  TT2000 time stamps for absolute calibration data
;                               REL_OPTICS   -  Optics state at the time relative calibrations were performed
;                               RELCAL_GDU1  -  Relative calibration factors for GDU1
;                               RELCAL_GDU2  -  Relative calibration factors for GDU2
;                               ABS_OPTICS   -  Optics state at the time absolute calibrations were performed
;                               ABSCAL_GDU1  -  Absolute calibration factors for GDU1
;                               ABSCAL_GDU2  -  Absolute calibration factors for GDU2
;
; :Keywords:
;       VERSION:            in, optional, type=string
;                           The version number of the output file, as 'X.Y.Z', where
;                               X, Y, and Z are integers. If not given, 1 will be added
;                               to the Y-version number of `FILE`, and Z will be reset
;                               to zero.
;       CLOBBER:            in, optional, type=boolean, default=0
;                           If set, then the output cal file will clober any existing
;                               file by the same name.
;
; :Returns:
;       CAL_FILE:           Name of the file created. It will be the same as `FILE`, but
;                               with its Y-version number incremented by 1.
;-
function mms_edi_amb_cal_sav2cdf, gdu1_sav, gdu2_sav, time, $
CAL_PATH_ROOT=cal_path_root, $
CLOBBER=clobber, $
DROPBOX_ROOT=dropbox_root, $
VERSION=version
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(oAmb) then obj_destroy, oAmb
		MrPrintF, 'LogErr'
		return, ''
	endif
	
	;Check if the file exists
	if ~file_test(gdu1_sav, /READ, /WRITE, /REGULAR) then message, 'GDU1_SAV must exist and be readable.'
	if ~file_test(gdu2_sav, /READ, /WRITE, /REGULAR) then message, 'GDU2_SAV must exist and be readable.'

	;Cal path. Can be in two places:
	;   - The environment variable CAL_PATH_ROOT
	;   - The system variable !EDI_INIT (unh_edi_init.pro)
	if n_elements(cal_path_root) eq 0 then begin
		cal_path_root = getenv('CAL_PATH_ROOT')
		if cal_path_root eq '' then begin
			defsysv, '!edi_init', EXIST=tf_exist
			if tf_exist $
				then cal_path_root = !edi_init.cal_path_root $
				else message, 'Cannot determine CAL_PATH_ROOT.'
		endif
	endif
	
	;Auto-determine version number
	tf_get_version = n_elements(version) eq 0
	
;-----------------------------------------------------
; Restore Input Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the file name
	parts = stregex( gdu1_sav, '(mms[1-4])_' + $         ;Spacecraft ID
	                           '(gdu[1-2])_' + $         ;GDU #
	                           '(relcal|abscal)' + $     ;Cal type
	                           '.sav', /SUBEXP, /EXTRACT )
	
	;Make sure it is valid
	if parts[0] eq '' then message, 'Filename is formatted incorrectly.'
	
	;Pick out the pieces
	sc       = parts[1]
	gdu      = parts[2]
	cal_type = parts[3]
	
	;Restore the data
	restore, gdu1_sav
	gdu1 = temporary(data)
	restore, gdu2_sav
	gdu2 = temporary(data)
	
;-----------------------------------------------------
; Find Optics State \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;File start time
	if ~MrTokens_IsMatch(time, '%Y-%M-%dT%H:%m:%S') $
		then message, 'TIME must be formatted as "%Y-%M-%dT%H:%m:%S".'
	
	;Find a data file on the day of operation
	files = mms_find_file( sc, 'edi', 'fast', 'l1a', $
	                       COUNT   = nFiles, $
	                       OPTDESC = 'amb*', $
	                       TSTART  = strmid(time, 0, 10) + 'T00:00:00', $
	                       TEND    = strmid(time, 0, 10) + 'T24:00:00' )
	if nFiles eq 0 then message, 'No L1A files found. Cannot determine optics state.'
	
	;Read the optics state from the first file
	oAmb   = MrCDF_File(files[0])
	optics = oAmb -> Read( strjoin( [sc, 'edi', 'amb', 'optics'], '_' ) )
	obj_destroy, oAmb
	
	;Unique optics states
	optics = optics[ uniq(optics, sort(optics)) ]
	if n_elements(optics) ne 1 then message, 'More than one optics states on this day.'

;-----------------------------------------------------
; Save to Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Convert time to TT2000
	;   - Requires particular formatting
	MrTimeParser, time, '%Y-%M-%dT%H:%m:%S', '%Y-%M-%dT%H:%m:%S.%1%2%3', t_temp
	tt2000 = MrCDF_Epoch_Parse( temporary(t_temp) )
	
	;Find the cal file
	old_cal = mms_edi_amb_cal_find( sc, $
	                                CAL_PATH_ROOT = cal_path_root )

	;Create structure with data that will be added to CDF file
	temp = { tt2000_rel:  tt2000, $
	         optics_rel:  optics, $
	         relcal_gdu1: transpose(gdu1.relcal), $
	         relcal_gdu2: transpose(gdu2.relcal) }

	;Add entry in cal file
	cal_file = mms_edi_amb_cal_addvalue( old_cal, temporary(temp), $
	                                     CAL_PATH_ROOT = cal_path_root, $
	                                     CLOBBER       = clobber, $
	                                     DROPBOX_ROOT  = dropbox_root, $
	                                     VERSION       = version )

	return, cal_file
end