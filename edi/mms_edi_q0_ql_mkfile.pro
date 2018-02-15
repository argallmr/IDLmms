; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ql_write
;
; PURPOSE:
;+
;   Write EDI qualty zero background counts to file.
;
; :Categories:
;    MMS, EDI, QL, Ambient
;
; :Params:
;       SC:                 in, required, type=string
;                           Spacecraft ID: 'mms1', 'mms2', 'mms3', or 'mms4'
;       MODE:               in, required, type=string
;                           Data rate mode: 'slow', 'fast', 'srvy', or 'brst'
;       TSTART:             in, required, type=string
;                           Start time of the data file to be written, formatted as
;                           'YYYYMMDDhhmmss' for burst mode files, and 'YYYYMMDD'
;                               otherwise.
;       Q0_DATA:            in, required, type=struct
;                           EDI ambient data structure with the following fields::
;                               TT2000_GD12 - TT2000 time tags for the Gun1-Detector2 pair
;                               TT2000_GD21 - TT2000 time tags for the Gun2-Detector1 pair
;                               ENERGY_GD12 - Energy state for GD12
;                               ENERGY_GD21 - Energy state for GD21
;                               Q0_GD12     - Quality 0 counts for GD12
;                               Q0_GD21     - Quality 0 counts for GD21
;
; :Keywords:
;       DROPBOX_ROOT:       in, optional, type=string, default=pwd
;                           Directory into which files are saved. It is expected that
;                               externally to this program, files are moved into their
;                               final destination in `DATA_PATH`.
;       DATA_PATH_ROOT:     in, optional, type=string, default=pwd
;                           Root of an MMS SDC-like directory structure. This is used
;                               in conjunction with `DROPBOX` to determine the z-version
;                               of the output file.
;       OPTDESC:            in, optional, type=string, default='amb'
;                           Optional filename descriptor, with parts separated by a hyphen.
;       PARENTS:            in, optional, type=string/strarr, default=''
;                           Names of the parent files required to make `AMB_DATA`.
;       STATUS:             out, optional, type=byte
;                           An error code. Values are::
;                               OK      = 0
;                               Warning = 1-99
;                               Error   = 100-255
;
; :Returns:
;       Q0_FILE:           Name of the file created.
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
;       2016/03/23  -   Written by Matthew Argall
;       2018/01/13  -   Updated y-version number to v2.1.0. - MRA
;-
function mms_edi_q0_ql_mkfile, sc, mode, tstart, $
DROPBOX_ROOT=dropbox, $
DATA_PATH_ROOT=data_path, $
EMPTY_FILE=empty_file, $
OPTDESC=optdesc, $
PARENTS=parents, $
STATUS=status
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Close and delete file
		if obj_valid(oq0) then obj_destroy, oq0
		if n_elements(q0_file) gt 0 && file_test(q0_file) then file_delete, q0_file
		
		;Report error
		if n_elements(status) eq 0 || status eq 0 then status = 100
		MrPrintF, 'LogErr'
		
		;Return
		return, ''
	endif
	
	;Everything starts out ok
	status = 0
	
	;Defaults
	tf_empty = keyword_set(empty_file)
	if n_elements(sc)      eq 0 || sc      eq '' then sc      = 'mms#'
	if n_elements(mode)    eq 0 || mode    eq '' then mode    = 'mode'
	if n_elements(optdesc) eq 0                  then optdesc = 'q0'
	if n_elements(parents) eq 0                  then parents = ' '
	if n_elements(tstart)  eq 0 || tstart  eq '' then begin
		MrCDF_Epoch_Breakdown, q0_data.tt2000_0[0], yr, mo, day, hr, mn, sec
		if mode eq 'brst' || mode eq 'mode' $
			then tstart = string(FORMAT='(%"%04i%02i%02i%02i%02i%02i")', yr, mo, day, hr, mn, sec) $
			else tstart = string(FORMAT='(%"%04i%02i%02i")', yr, mo, day)
	endif
	
	;Check if the system variable exists
	defsysv, '!edi_amb_init', EXISTS=tf_sysv
	if tf_sysv then begin
		if n_elements(dropbox)   eq 0 then dropbox   = !edi_amb_init.dropbox_root
		if n_elements(data_path) eq 0 then data_path = !edi_amb_init.data_path_root
	endif else begin
		if n_elements(dropbox)   eq 0 then cd, CURRENT=dropbox
		if n_elements(data_path) eq 0 then cd, CURRENT=data_path
	endelse

;------------------------------------;
; Version History                    ;
;------------------------------------;
	;Mods to data processing
	mods = [ 'v0.0.0 - First version.', $.
	         'v0.0.1 - Filled energy variables.', $
	         'v0.0.2 - Energy written properly.', $ 
	         'v1.0.0 - Update variable names.', $ 
	         'v1.1.0 - Added optics state.', $ 
	         'v1.2.0 - Update PI_Name.', $ 
	         'v1.3.0 - Fixed optics datatype.', $ 
	         'v2.0.0 - Removed unused Epoch variable.', $ 
	         'v2.1.0 - Data for optics is now written to file.' ]
	
	;Get the version
	version = stregex(mods[-1], '^v([0-9]+)\.([0-9]+)\.([0-9]+)', /SUBEXP, /EXTRACT)
	vx      = strtrim(version[1], 2)
	vy      = strtrim(version[2], 2)
	vz      = strtrim(version[3], 2)
	
	;Constants for destination file
	instr   = 'edi'
	level   = 'ql'

;------------------------------------;
; Check Inputs                       ;
;------------------------------------;

;------------------------------------;
; Create Output File Name            ;
;------------------------------------;
	;Output file
	version = vx + '.' + vy + '.' + vz
	q0_file = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)
	
	;Find the latest z-version
	;   - Look in both DROPBOX and DATA_PATH
	vz = mms_latest_zversion(dropbox, q0_file, ROOT=data_path)
	
	;Reform the file name
	version = vx + '.' + vy + '.' + string(vz, FORMAT='(i0)')
	q0_file = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)
	q0_file = filepath(q0_file, ROOT_DIR=dropbox)

	;Notify where file is located
	MrPrintF, 'LogText', 'Creating EDI Q0 file at "' + q0_file + '".'

;------------------------------------;
; Create File                        ;
;------------------------------------;

	;Open the CDF file
	oq0 = MrCDF_File(q0_file, /CREATE, /CLOBBER)
	if obj_valid(oq0) eq 0 then return, ''

;------------------------------------------------------
; Global Attributes                                   |
;------------------------------------------------------
	if n_elements(optdesc) eq 0 then begin
		data_type      = mode + '_' + level
		logical_source = instr + '_' + mode + '_' + level
	endif else begin
		data_type      = mode + '_' + level + '_' + optdesc
		logical_source = instr + '_' + mode + '_' + level + '_' + optdesc
	endelse
	logical_file_id = cgRootName(q0_file)
	source_name = 'MMS' + strmid(sc, 3) + '>MMS Satellite Number ' + strmid(sc, 3)
	MrTimeParser, MrTimeStamp(/UTC), '%Y-%M-%dT%H:%m:%S', '%Y%M%d', gen_date

	;   - Instrument Type (1+)
	;           Electric Fields (space)
	;           Magnetic Fields (space)
	;           Particles (space)
	;           Plasma and Solar Wind
	;           Spacecraft Potential Control
	oq0 -> WriteGlobalAttr, /CREATE, 'Data_type',                  data_type
	oq0 -> WriteGlobalAttr, /CREATE, 'Data_version',               version
	oq0 -> WriteGlobalAttr, /CREATE, 'Descriptor',                 'EDI'
	oq0 -> WriteGlobalAttr, /CREATE, 'Discipline',                 'Space Physics>Magnetospheric Science'
	oq0 -> WriteGlobalAttr, /CREATE, 'File_naming_convention',     'source_descriptor_datatype_yyyyMMdd'
	oq0 -> WriteGlobalAttr, /CREATE, 'Generation_date',            gen_date
	oq0 -> WriteGlobalAttr, /CREATE, 'Instrument_type',            'Particles (space)'
	oq0 -> WriteGlobalAttr, /CREATE, 'Logical_file_id',            logical_file_id
	oq0 -> WriteGlobalAttr, /CREATE, 'Logical_source',             logical_source
	oq0 -> WriteGlobalAttr, /CREATE, 'Logical_source_description', 'Level 2 EDI Quality 0 Counts'
	oq0 -> WriteGlobalAttr, /CREATE, 'Mission_group',              'MMS'
	oq0 -> WriteGlobalAttr, /CREATE, 'PI_affiliation',             'UNH'
	oq0 -> WriteGlobalAttr, /CREATE, 'PI_name',                    'Roy Torbert, Hans Vaith'
	oq0 -> WriteGlobalAttr, /CREATE, 'Project',                    'STP>Solar Terrestrial Physics'
	oq0 -> WriteGlobalAttr, /CREATE, 'Source_name',                source_name
	oq0 -> WriteGlobalAttr, /CREATE, 'TEXT',                       'EDI Q0 data. The EDI instrument paper and data products guides' + $
	                                                               'can be found at the following two links: ' + $
	                                                               'http://link.springer.com/article/10.1007%2Fs11214-015-0182-7, ' + $
	                                                               'https://lasp.colorado.edu/mms/sdc/public/datasets/fields/'
	oq0 -> WriteGlobalAttr, /CREATE, 'HTTP_LINK',                  ['http://mms-fields.unh.edu/', $
	                                                                 'http://mms.gsfc.nasa.gov/index.html']
	oq0 -> WriteGlobalAttr, /CREATE, 'LINK_TEXT',                  ['UNH FIELDS Home Page', $
	                                                                 'NASA MMS Home']
	oq0 -> WriteGlobalAttr, /CREATE, 'MODS',                       mods
	oq0 -> WriteGlobalAttr, /CREATE, 'Acknowledgements',           ' '
	oq0 -> WriteGlobalAttr, /CREATE, 'Generated_by',               'University of New Hampshire'
	oq0 -> WriteGlobalAttr, /CREATE, 'Parents',                    parents
	oq0 -> WriteGlobalAttr, /CREATE, 'Skeleton_version',           ' '
	oq0 -> WriteGlobalAttr, /CREATE, 'Rules_of_use',               ' '
	oq0 -> WriteGlobalAttr, /CREATE, 'Time_resolution',            ' '

;------------------------------------------------------
; Variables                                           |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName[_coordSys][_paramQualifier][_subModeLevel][_mode][_level]
	prefix  = strjoin([sc, instr], '_') + '_'
	suffix  = '_' + strjoin([mode, level], '_')
	
	epoch_timetag_vname = 'epoch_timetag'
	epoch_gdu1_vname    = 'epoch_gdu1'
	epoch_gdu2_vname    = 'epoch_gdu2'
	optics_vname        = prefix + 'optics_state' + suffix
	e_gdu1_vname        = prefix + 'energy_gdu1' + suffix
	e_gdu2_vname        = prefix + 'energy_gdu2' + suffix
	q0_gdu1_vname       = prefix + 'counts_gdu1' + suffix
	q0_gdu2_vname       = prefix + 'counts_gdu2' + suffix

	;Write variable data to file
	;   - All are detector quantities, so GD12 --> GDU2 and GD21 --> GDU2
	oq0 -> CreateVar, epoch_timetag_vname, 'CDF_TIME_TT2000'
	oq0 -> CreateVar, epoch_gdu1_vname,    'CDF_TIME_TT2000'
	oq0 -> CreateVar, epoch_gdu2_vname,    'CDF_TIME_TT2000'
	oq0 -> CreateVar, optics_vname,        'CDF_UINT1', COMPRESSION='GZIP', GZIP_LEVEL=6
	oq0 -> CreateVar, e_gdu1_vname,        'CDF_UINT2', COMPRESSION='GZIP', GZIP_LEVEL=6
	oq0 -> CreateVar, e_gdu2_vname,        'CDF_UINT2', COMPRESSION='GZIP', GZIP_LEVEL=6
	oq0 -> CreateVar, q0_gdu1_vname,       'CDF_UINT2', COMPRESSION='GZIP', GZIP_LEVEL=6
	oq0 -> CreateVar, q0_gdu2_vname,       'CDF_UINT2', COMPRESSION='GZIP', GZIP_LEVEL=6
	
;------------------------------------------------------
; Variable Attributes                                 |
;------------------------------------------------------
	;Create the variable attributes
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'CATDESC'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_0'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'DISPLAY_TYPE'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'FIELDNAM'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'FILLVAL'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'FORMAT'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'LABLAXIS'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'SCALETYP'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'SI_CONVERSION'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'TIME_BASE'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'UNITS'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMIN'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMAX'
	oq0 -> CreateAttr, /VARIABLE_SCOPE, 'VAR_TYPE'
	
	;TT2000_TIMETAGS
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'CATDESC',       'Packet time tags'
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'FIELDNAM',      'Time'
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'FILLVAL',       MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'FORMAT',        'I16'
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'LABLAXIS',      'UT'
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'SI_CONVERSION', '1e-9>s'
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'TIME_BASE',     'J2000'
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'UNITS',         'ns'
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	oq0 -> WriteVarAttr, epoch_timetag_vname, 'VAR_TYPE',      'support_data'
	
	;TT2000_GDU1
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'CATDESC',       'TT2000 time tags for quality 0 EDI GDU1 counts.'
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'FIELDNAM',      'Time'
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'FORMAT',        'I16'
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'LABLAXIS',      'UT'
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'SI_CONVERSION', '1e-9>s'
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'TIME_BASE',     'J2000'
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'UNITS',         'ns'
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	oq0 -> WriteVarAttr, epoch_gdu1_vname, 'VAR_TYPE',      'support_data'
	
	;TT2000_GDU2
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'CATDESC',       'TT2000 time tags for quality 0 EDI GDU2 counts.'
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'FIELDNAM',      'Time'
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'FILLVAL',       MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'FORMAT',        'I16'
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'LABLAXIS',      'UT'
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'SI_CONVERSION', '1e-9>s'
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'TIME_BASE',     'J2000'
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'UNITS',         'ns'
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	oq0 -> WriteVarAttr, epoch_gdu2_vname, 'VAR_TYPE',      'support_data'

	;OPTICS_STATE
	oq0 -> WriteVarAttr, optics_vname, 'CATDESC',       'Optics state'
	oq0 -> WriteVarAttr, optics_vname, 'DEPEND_0',       epoch_timetag_vname
	oq0 -> WriteVarAttr, optics_vname, 'FIELDNAM',      'Optics state'
	oq0 -> WriteVarAttr, optics_vname, 'FILLVAL',        65535US
	oq0 -> WriteVarAttr, optics_vname, 'FORMAT',        'I4'
	oq0 -> WriteVarAttr, optics_vname, 'LABLAXIS',      'Optics'
	oq0 -> WriteVarAttr, optics_vname, 'VALIDMIN',      0US
	oq0 -> WriteVarAttr, optics_vname, 'VALIDMAX',      1000US
	oq0 -> WriteVarAttr, optics_vname, 'VAR_TYPE',      'support_data'

	;ENERGY_GDU1
	oq0 -> WriteVarAttr, e_gdu1_vname, 'CATDESC',       'GDU1 energy'
	oq0 -> WriteVarAttr, e_gdu1_vname, 'DEPEND_0',       epoch_gdu1_vname
	oq0 -> WriteVarAttr, e_gdu1_vname, 'FIELDNAM',      'Energy'
	oq0 -> WriteVarAttr, e_gdu1_vname, 'FILLVAL',        65535US
	oq0 -> WriteVarAttr, e_gdu1_vname, 'FORMAT',        'I4'
	oq0 -> WriteVarAttr, e_gdu1_vname, 'LABLAXIS',      'Energy'
	oq0 -> WriteVarAttr, e_gdu1_vname, 'SI_CONVERSION', '1.602e-19>J'
	oq0 -> WriteVarAttr, e_gdu1_vname, 'UNITS',         'eV'
	oq0 -> WriteVarAttr, e_gdu1_vname, 'VALIDMIN',      0US
	oq0 -> WriteVarAttr, e_gdu1_vname, 'VALIDMAX',      1000US
	oq0 -> WriteVarAttr, e_gdu1_vname, 'VAR_TYPE',      'support_data'

	;ENERGY_GDU2
	oq0 -> WriteVarAttr, e_gdu2_vname, 'CATDESC',       'GDU2 energy'
	oq0 -> WriteVarAttr, e_gdu2_vname, 'DEPEND_0',      epoch_gdu2_vname
	oq0 -> WriteVarAttr, e_gdu2_vname, 'FIELDNAM',      'Energy'
	oq0 -> WriteVarAttr, e_gdu2_vname, 'FILLVAL',       65535US
	oq0 -> WriteVarAttr, e_gdu2_vname, 'FORMAT',        'I4'
	oq0 -> WriteVarAttr, e_gdu2_vname, 'LABLAXIS',      'Energy'
	oq0 -> WriteVarAttr, e_gdu2_vname, 'SI_CONVERSION', '1.602e-19>J'
	oq0 -> WriteVarAttr, e_gdu2_vname, 'UNITS',         'eV'
	oq0 -> WriteVarAttr, e_gdu2_vname, 'VALIDMIN',      0US
	oq0 -> WriteVarAttr, e_gdu2_vname, 'VALIDMAX',      1000US
	oq0 -> WriteVarAttr, e_gdu2_vname, 'VAR_TYPE',      'support_data'

	;Q0_GDU1
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'CATDESC',      'GDU1 quality 0 counts.'
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'DEPEND_0',      epoch_gdu1_vname
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'DISPLAY_TYPE', 'time_series'
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'FIELDNAM',     'Quality-0 Counts GDU1'
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'FILLVAL',      65535US
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'FORMAT',       'I5'
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'LABLAXIS',     'Q0'
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'SCALETYP',     'log'
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'UNITS',        'counts'
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'VALIDMIN',     0US
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'VALIDMAX',     65534US
	oq0 -> WriteVarAttr, q0_gdu1_vname, 'VAR_TYPE',     'data'

	;Q0_GDU2
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'CATDESC',      'GDU2 quality 0 counts.'
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'DEPEND_0',      epoch_gdu2_vname
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'DISPLAY_TYPE', 'time_series'
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'FIELDNAM',     'Quality-0 Counts GDU1'
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'FILLVAL',      65535US
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'FORMAT',       'I5'
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'LABLAXIS',     'Q0'
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'SCALETYP',     'log'
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'UNITS',        'counts'
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'VALIDMIN',     0US
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'VALIDMAX',     65534US
	oq0 -> WriteVarAttr, q0_gdu2_vname, 'VAR_TYPE',     'data'

;------------------------------------------------------
; Create an "Empty" File?                             |
;------------------------------------------------------
	;
	; An "Empty" file contains a single time stamp at the beginning of the
	; day. All other variables are empty.
	;

	if tf_empty then begin
		mms_parse_time, tstart, year, month, day
		epoch_val = MrCDF_Epoch_Compute(year, month, day, /TT2000)
		oq0 -> WriteVar, epoch_vname, epoch_val
	endif

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, oq0
	return, q0_file
end