; docformat = 'rst'
;
; NAME:
;       C_CONSTRUCT_FILENAME
;
; PURPOSE:
;+
;       The purpose of this program is to create a Cluster CDF file name using the user
;       input information::
;
;       scId_instrumentId_paramName_optionalDescriptor
;
; :Categories:
;   MMS
;
; :Examples:
;   See the example program at the end of this file::
;       IDL> .r mms_construct_varname
;
; :Params:
;       SPACECRAFT:         out, optional, type=string
;                           The spacecraft ID. Possible values are::
;                               'mms1'
;                               'mms2'
;                               'mms3'
;                               'mms4'
;       INSTRUMENT:         out, optional, type=string
;                           Instrument ID. Possible values are::
;                               'hpca'              'edi'
;                               'aspoc'             'adp'
;                               'epd'               'sdp'
;                               'epd-eis'           'adp-sdp'
;                               'epd-feeps'         'afg'
;                               'fpi'               'dfg'
;                               'des'               'afg-dfg'
;                               'dis'               'scm'
;                               'des-dis'           'fields'
;       MODE:               out, optional, type=string
;                           Data capture mode. Possible values are::
;                               'fast'
;                               'slow'
;                               'brst'
;                               'srvy'
;       LEVEL:              out, optional, type=string
;                           Level of data production. Possible values are::
;                               'l1a'
;                               'l1b'
;                               'l2'
;                               'ql'
;                               'l2pre'
;                               'l2plus'
;
; :Keywords:
;       DESCRIPTOR:         in, optional, type=string, default=''
;                           Optional field that may not be needed for all
;                               products (e.g. Quicklook and SITL).  Where it is used, 
;                               identifiers should be short (e.g. 3-8 character)
;                               descriptors that are helpful to end-users.  If a descriptor
;                               contains multiple components, hyphens are used to separate
;                               those components.
;       PATTERN:            in, optional, type=boolean, default=0
;                           If set, then the default value of `START_TIME` is '%Y%M%d'.
;                               This pattern is recognized by MrTokens and can be used
;                               to find files with year, month, and day in the file name.
;       START_TIME:         in, optional, type=string, default='*'
;                           Start time of the data interval formatted as yyyymmddhhmmss,
;                               with irrelevant, least significant, fields dropped
;                               when files start on regular hourly or minute boundaries.
;       VERSION:            in, optional, type=string, default='*'
;                           Version of the data file, formatted as 'vX.Y.Z'
;                               X - Interface number.  Increments in this number represent a
;                                       significant change to the processing software and/or to the contents of the 
;                                       file. These changes will likely break existing code that expects a specific 
;                                       file format (e.g. file reading software).  Additionally, increments in this 
;                                       number may require code changes to analysis software that expects the 
;                                       data to have been created using specific processing algorithms. The user 
;                                       should consult the appropriate meta-data for or changelogs.
;                               Y - Quality number. This number represents a change in the quality of
;                                       the data in the file, such as change in calibration or increase in fidelity. 
;                                       Changes should not impact software, but may require consideration when 
;                                       processing data.
;                               Z - Bug fix/revision number. This number changes to indicate minor
;                                       changes to the contents of the file due to reprocessing of missing data.  
;                                       Any dependent data products should generally be reprocessed if this value 
;                                       changes.
;
; :Returns:
;       VARNAME:             The MMS variable name.
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2015/02/06  -   Written by Matthew Argall
;
function mms_construct_varname, sc, instrument, param_name, $
DESCRIPTOR=descriptor
	compile_opt strictarr
	on_error, 2
	
	desc = n_elements(descriptor) eq 0 ? '' : '_' + descriptor
	
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
	inst_ids = ['hpca', 'aspoc', 'epd', 'epd-eis', 'epd-feeps', 'fpi', $
	            'des', 'dis', 'des-dis', 'fields', 'edi', 'adp', 'sdp', 'adp-sdp', $
	            'afg', 'dfg', 'dsp', 'afg-dfg', 'scm']
	if max(strlowcase(instrument) eq inst_ids) eq 0 $
		then message, 'Invalid instrument ID: "' + inst_ids + '".'

	
	;create the leading string of cluster filename
	;e.g. 'C1_CP_' + team + data_product + '_20050125_144700_20050125_145200_V######.cdf'
	varname = _sc + '_' + $
	          instrument  + '_' + $
	          param_name  +  $
	          desc

	
	return, varname
end

;MAIN level program to see how mms_construct_file.pro works.
;To test,
;      IDL> .run mms_construct_file
sc         = 1
instrument = 'afg'
param_name = 'bmrg'
descriptor = 'merged'

filename = mms_construct_varname(sc, instrument, param_name, DESCRIPTOR=descriptor)
print, filename
end