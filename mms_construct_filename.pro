; docformat = 'rst'
;
; NAME:
;       MMS_CONSTRUCT_FILENAME
;
; PURPOSE:
;+
;       The purpose of this program is to create a Cluster CDF file name using the user
;       input information::
;
;       scId_instrumentId_mode_dataLevel_optionalDataProductDescriptor_startTime_vX.Y.Z.cdf
;
; :Categories:
;   MMS
;
; :Examples:
;   See the example program at the end of this file::
;       IDL> .r mms_construct_filename
;
; :Params:
;       SPACECRAFT:     out, optional, type=string
;                       The spacecraft ID. Possible values are::
;                           'mms1'
;                           'mms2'
;                           'mms3'
;                           'mms4'
;       INSTRUMENT:     out, optional, type=string
;                       Instrument ID. Possible values are::
;                           'hpca'              'edi'
;                           'aspoc'             'adp'
;                           'epd'               'sdp'
;                           'epd-eis'           'adp-sdp'
;                           'epd-feeps'         'afg'
;                           'fpi'               'dfg'
;                           'des'               'afg-dfg'
;                           'dis'               'scm'
;                           'des-dis'           'fields'
;       MODE:           out, optional, type=string
;                       Data capture mode. Possible values are::
;                           'fast'
;                           'slow'
;                           'brst'
;                           'srvy'
;       LEVEL:          out, optional, type=string
;                       Level of data production. Possible values are::
;                           'l1a'
;                           'l1b'
;                           'l2'
;                           'ql'
;                           'l2pre'
;                           'l2plus'
;
; :Keywords:
;       OPTDESC:        in, optional, type=string, default=''
;                       Optional field that may not be needed for all
;                           products (e.g. Quicklook and SITL).  Where it is used, 
;                           identifiers should be short (e.g. 3-8 character)
;                           descriptors that are helpful to end-users.  If a descriptor
;                           contains multiple components, hyphens are used to separate
;                           those components.
;       TOKENS:         in, optional, type=boolean, default=0
;                       If set, then the default value of `START_TIME` is '%Y%M%d'.
;                           This pattern is recognized by MrTokens and can be used
;                           to find files with year, month, and day in the file name.
;       TSTART:         in, optional, type=string, default='*'
;                       Start time of the data interval formatted as yyyymmddhhmmss,
;                           with irrelevant, least significant, fields dropped
;                           when files start on regular hourly or minute boundaries.
;       VERSION:        in, optional, type=string, default='*'
;                       Version of the data file, formatted as 'vX.Y.Z'
;                           X - Interface number.  Increments in this number represent a
;                                   significant change to the processing software and/or to the contents of the 
;                                   file. These changes will likely break existing code that expects a specific 
;                                   file format (e.g. file reading software).  Additionally, increments in this 
;                                   number may require code changes to analysis software that expects the 
;                                   data to have been created using specific processing algorithms. The user 
;                                   should consult the appropriate meta-data for or changelogs.
;                           Y - Quality number. This number represents a change in the quality of
;                                   the data in the file, such as change in calibration or increase in fidelity. 
;                                   Changes should not impact software, but may require consideration when 
;                                   processing data.
;                           Z - Bug fix/revision number. This number changes to indicate minor
;                                   changes to the contents of the file due to reprocessing of missing data.  
;                                   Any dependent data products should generally be reprocessed if this value 
;                                   changes.
;
; :Returns:
;       FILENAME:       The MMS file name.
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/02/06  -   Written by Matthew Argall
;
function mms_construct_filename, sc, instrument, mode, level, $
TSTART=tstart, $
OPTDESC=optdesc, $
DIRECTORY=directory, $
TOKENS=tokens, $
VERSION=version
	compile_opt strictarr
	on_error, 2
	
	tokens = keyword_set(tokens)
	if tokens $
		then tstart = '%Y%M%d' $
		else if n_elements(tstart) eq 0 then tstart = '*'
	
	;if no directory was supplied, get the current directory
	;if no base was chosen, go with the complete base
	if n_elements(directory)   eq 0 then directory = ''
	if n_elements(version)     eq 0 then version   = '*'
	desc = n_elements(optdesc) eq 0 ? '' : optdesc + '_'
	
	;Spacecraft ID
	if size(sc, /TNAME) eq 'STRING' then begin
		_sc = strlowcase(sc)
		case _sc of
			'mms1': ;Ok
			'mms2': ;Ok
			'mms3': ;Ok
			'mms4': ;Ok
			'1':    _sc = 'mms1'
			'2':    _sc = 'mms2'
			'3':    _sc = 'mms3'
			'4':    _sc = 'mms4'
			else: message, 'SC ("' + sc + '") must be {mms1 | mms2 | mms3 | mms4}.'
		endcase
	endif else begin
		case sc of
			1: _sc = 'mms1'
			2: _sc = 'mms2'
			3: _sc = 'mms3'
			4: _sc = 'mms4'
			else: message, 'SC ("' + strtrim(sc, 2) + '") must be {1 | 2 | 3 | 4}.'
		endcase
	endelse
	
	;Instrument IDs
;	inst_ids = ['hpca', 'aspoc', 'epd', 'epd-eis', 'epd-feeps', 'fpi', $
;	            'des', 'dis', 'des-dis', 'fields', 'edi', 'adp', 'sdp', 'adp-sdp', $
;	            'afg', 'dfg', 'dsp', 'afg-dfg', 'scm']
;	if max(strlowcase(instrument) eq inst_ids) eq 0 $
;		then message, 'Invalid instrument ID: "' + instrument + '".'
;
;	;Level
;	data_modes = ['fast', 'slow', 'brst', 'srvy']
;	if max(strlowcase(mode) eq data_modes) eq 0 $
;		then message, 'Invalid data mode: "' + mode + '".'
;
;	;Mode
;	data_levels = ['l1a', 'l1b', 'ql', 'l2', 'l2pre', 'l2plus']
;	if max(strlowcase(level) eq data_levels) eq 0 $
;		then message, 'Invalid data level: "' + level + '".'
	
	;Start Time
;	if tokens eq 0 && tstart ne '*' then begin
;		if stregex(tstart, '[0-9]{8}[0-9]*', /BOOLEAN) eq 0 $
;			then message, 'Invalid start time: "' + tstart + '".'
;	endif
	
	;Version
	if version ne '*' then begin
		if stregex(version, '[0-9]+\.[0-9]+\.[0-9]+', /BOOLEAN) eq 0 $
			then message, 'Invalid version: "' + version + '".'
	endif
	
	;Create the MMS filename
	filename = _sc        + '_' + $
	           instrument + '_' + $
	           mode       + '_' + $
	           level      + '_' + $
	           desc       + $
	           tstart    + '_' + $
	           'v' + version + $
	           '.cdf'

	;Add the directory
	if directory ne '' then filename = filepath(filename, ROOT_DIR=directory)
	
	return, filename
end

;MAIN level program to see how mms_construct_file.pro works.
;To test,
;      IDL> .run mms_construct_file
sc         = 1
instrument = 'afg'
mode       = 'srvy'
level      = 'l2pre'
optdesc    = 'duration-1h1m'
start_time = '20150313'
version    = '1.1.2'

filename = mms_construct_filename(sc, instrument, mode, level, OPTDESC=optdesc, $
                                  START_TIME=start_time, VERSION=version)
print, filename
end