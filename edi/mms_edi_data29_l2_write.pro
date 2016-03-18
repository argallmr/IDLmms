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
;       D29_DATA:           in, required, type=struct
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
;
; :Returns:
;       D29_FILE:           Name of the file created.
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
;       2016/02/17  -   Written by Matthew Argall
;-
function mms_edi_data29_l2_write, sc, mode, tstart, d29_data, $
DROPBOX_ROOT=dropbox, $
DATA_PATH_ROOT=data_path, $
OPTDESC=optdesc, $
PARENTS=parents
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(od29) then obj_destroy, od29
		if n_elements(d29_file) gt 0 && file_test(d29_file) then file_delete, d29_file
		MrPrintF, 'LogErr'
		return, ''
	endif

;------------------------------------;
; Version History                    ;
;------------------------------------;
	;Mods to data processing
	mods = [ 'v0.0.0 - First version.', $
	         'v0.1.0 - Update PI_name.' ]
	
	;Get the version
	version = stregex(mods[-1], '^v([0-9]+)\.([0-9]+)\.([0-9]+)', /SUBEXP, /EXTRACT)
	vx      = strtrim(version[1], 2)
	vy      = strtrim(version[2], 2)
	vz      = strtrim(version[3], 2)
	
	;Constants for destination file
	instr   = 'edi'
	level   = 'l2'

;------------------------------------;
; Check Inputs                       ;
;------------------------------------;
	
	;Defaults
	if n_elements(sc)      eq 0 || sc      eq '' then sc      = 'mms#'
	if n_elements(mode)    eq 0 || mode    eq '' then mode    = 'mode'
	if n_elements(optdesc) eq 0                  then optdesc = 'data29'
	if n_elements(parents) eq 0                  then parents = ' '
	if n_elements(tstart)  eq 0 || tstart  eq '' then begin
		MrCDF_Epoch_Breakdown, d29_data.tt2000_0[0], yr, mo, day, hr, mn, sec
		if mode eq 'brst' || mode eq 'mode' $
			then tstart = string(FORMAT='(%"%04i%02i%02i%02i%02i%02i")', yr, mo, day, hr, mn, sec) $
			else tstart = string(FORMAT='(%"%04i%02i%02i")', yr, mo, day)
	endif
	
	;Check if the system variable exists
	defsysv, '!edi_amb_init', EXISTS=tf_sysv
	if tf_sysv then begin
		if n_elements(dropbox)   eq 0 then dropbox   = !edi_amb_init.dropbox_root
		if n_elements(data_path) eq 0 then data_path = !edi_amb_init.unh_path_root
	endif else begin
		if n_elements(dropbox)   eq 0 then cd, CURRENT=dropbox
		if n_elements(data_path) eq 0 then cd, CURRENT=data_path
	endelse

;------------------------------------;
; Create Output File Name            ;
;------------------------------------;
	;Output file
	version = vx + '.' + vy + '.' + vz
	d29_file = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)
	
	;Find the latest z-version
	;   - Look in both DROPBOX and DATA_PATH
	vz = mms_latest_zversion(dropbox, d29_file, ROOT=data_path)
	
	;Reform the file name
	version = vx + '.' + vy + '.' + string(vz, FORMAT='(i0)')
	d29_file = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)
	d29_file = filepath(d29_file, ROOT_DIR=dropbox)

	;Notify where file is located
	MrPrintF, 'LogText', 'Creating EDI DATA29 file at "' + d29_file + '".'

;------------------------------------;
; Check Data & Create File           ;
;------------------------------------;
	;
	; Check sizes
	;
	if ~isa(d29_data.tt2000_data29, 'LONG64') then message, 'd29_data.tt2000_data29 must be LONG64.'
	if ~isa(d29_data.tt2000_gd12,   'LONG64') then message, 'd29_data.tt2000_gd12 must be LONG64.'
	if ~isa(d29_data.tt2000_gd21,   'LONG64') then message, 'd29_data.tt2000_gd21 must be LONG64.'
	if ~isa(d29_data.energy_gd12,   'UINT')   then message, 'd29_data.energy_gd12 must be UINT.'
	if ~isa(d29_data.energy_gd21,   'UINT')   then message, 'd29_data.energy_gd21 must be UINT.'
	if ~isa(d29_data.data29_gd12,   'UINT')   then message, 'd29_data.data29_gd12 must be UINT.'
	if ~isa(d29_data.data29_gd21,   'UINT')   then message, 'd29_data.data29_gd21 must be UINT.'

	;Open the CDF file
	od29 = MrCDF_File(d29_file, /CREATE, /CLOBBER)
	if obj_valid(od29) eq 0 then return, ''

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
	logical_file_id = cgRootName(d29_file)
	source_name = 'MMS' + strmid(sc, 3) + '>MMS Satellite Number ' + strmid(sc, 3)
	MrTimeParser, MrTimeStamp(/UTC), '%Y-%M-%dT%H:%m:%S', '%Y%M%d', gen_date

	;   - Instrument Type (1+)
	;           Electric Fields (space)
	;           Magnetic Fields (space)
	;           Particles (space)
	;           Plasma and Solar Wind
	;           Spacecraft Potential Control
	od29 -> WriteGlobalAttr, /CREATE, 'Data_type',                  data_type
	od29 -> WriteGlobalAttr, /CREATE, 'Data_version',               version
	od29 -> WriteGlobalAttr, /CREATE, 'Descriptor',                 'EDI'
	od29 -> WriteGlobalAttr, /CREATE, 'Discipline',                 'Space Physics>Magnetospheric Science'
	od29 -> WriteGlobalAttr, /CREATE, 'File_naming_convention',     'source_descriptor_datatype_yyyyMMdd'
	od29 -> WriteGlobalAttr, /CREATE, 'Generation_date',            gen_date
	od29 -> WriteGlobalAttr, /CREATE, 'Instrument_type',            'Particles (space)'
	od29 -> WriteGlobalAttr, /CREATE, 'Logical_file_id',            logical_file_id
	od29 -> WriteGlobalAttr, /CREATE, 'Logical_source',             logical_source
	od29 -> WriteGlobalAttr, /CREATE, 'Logical_source_description', 'Level 2 EDI data29 Counts'
	od29 -> WriteGlobalAttr, /CREATE, 'Mission_group',              'MMS'
	od29 -> WriteGlobalAttr, /CREATE, 'PI_affiliation',             'UNH'
	od29 -> WriteGlobalAttr, /CREATE, 'PI_name',                    'Roy Torbert, Hans Vaith'
	od29 -> WriteGlobalAttr, /CREATE, 'Project',                    'STP>Solar Terrestrial Physics'
	od29 -> WriteGlobalAttr, /CREATE, 'Source_name',                source_name
	od29 -> WriteGlobalAttr, /CREATE, 'TEXT',                       'EDI data29. The instrument paper ' + $
	                                                                'for EDI can be found at: ' + $
	                                                                'http://link.springer.com/article/10.1007%2Fs11214-015-0182-7'
	od29 -> WriteGlobalAttr, /CREATE, 'HTTP_LINK',                  ['http://mms-fields.unh.edu/', $
	                                                                 'http://mms.gsfc.nasa.gov/index.html']
	od29 -> WriteGlobalAttr, /CREATE, 'LINK_TEXT',                  ['UNH FIELDS Home Page', $
	                                                                 'NASA MMS Home']
	od29 -> WriteGlobalAttr, /CREATE, 'MODS',                       mods
	od29 -> WriteGlobalAttr, /CREATE, 'Acknowledgements',           ' '
	od29 -> WriteGlobalAttr, /CREATE, 'Generated_by',               'University of New Hampshire'
	od29 -> WriteGlobalAttr, /CREATE, 'Parents',                    parents
	od29 -> WriteGlobalAttr, /CREATE, 'Skeleton_version',           ' '
	od29 -> WriteGlobalAttr, /CREATE, 'Rules_of_use',               ' '
	od29 -> WriteGlobalAttr, /CREATE, 'Time_resolution',            ' '

;------------------------------------------------------
; Variables                                           |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName[_coordSys][_paramQualifier][_subModeLevel][_mode][_level]
	prefix  = strjoin([sc, instr], '_') + '_'
	suffix  = '_' + strjoin([mode, level], '_')
	
	epoch_vname         = 'Epoch'
	epoch_d29_vname     = 'epoch_data29'
	epoch_gdu1_vname    = 'epoch_gdu1'
	epoch_gdu2_vname    = 'epoch_gdu2'
	epoch_timetag_vname = 'epoch_timetag'
	optics_vname        = prefix + 'optics'      + suffix
	e_gdu1_vname        = prefix + 'energy_gdu1' + suffix
	e_gdu2_vname        = prefix + 'energy_gdu2' + suffix
	d29_gdu1_vname      = prefix + 'data29_gdu1' + suffix
	d29_gdu2_vname      = prefix + 'data29_gdu2' + suffix

	;Write variable data to file
	;   - All are detector quantities, so GD12 --> GDU2 and GD21 --> GDU2
	od29 -> CreateVar, epoch_vname, 'CDF_TIME_TT2000', /ZVARIABLE
	od29 -> WriteVar, /CREATE, epoch_d29_vname,     d29_data.tt2000_data29,  CDF_TYPE='CDF_TIME_TT2000'
	od29 -> WriteVar, /CREATE, epoch_gdu1_vname,    d29_data.tt2000_gd21,    CDF_TYPE='CDF_TIME_TT2000'
	od29 -> WriteVar, /CREATE, epoch_gdu2_vname,    d29_data.tt2000_gd12,    CDF_TYPE='CDF_TIME_TT2000'
	od29 -> WriteVar, /CREATE, epoch_timetag_vname, d29_data.tt2000_timetag, CDF_TYPE='CDF_TIME_TT2000'
	od29 -> WriteVar, /CREATE, optics_vname,        d29_data.optics,      COMPRESSION='GZIP', GZIP_LEVEL=6
	od29 -> WriteVar, /CREATE, e_gdu1_vname,        d29_data.energy_gd21, COMPRESSION='GZIP', GZIP_LEVEL=6
	od29 -> WriteVar, /CREATE, e_gdu2_vname,        d29_data.energy_gd12, COMPRESSION='GZIP', GZIP_LEVEL=6
	od29 -> WriteVar, /CREATE, d29_gdu1_vname,      d29_data.data29_gd21, COMPRESSION='GZIP', GZIP_LEVEL=6
	od29 -> WriteVar, /CREATE, d29_gdu2_vname,      d29_data.data29_gd12, COMPRESSION='GZIP', GZIP_LEVEL=6
	
;------------------------------------------------------
; Variable Attributes                                 |
;------------------------------------------------------
	;Create the variable attributes
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'CATDESC'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_0'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'DISPLAY_TYPE'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'FIELDNAM'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'FILLVAL'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'FORMAT'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'LABLAXIS'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'SCALETYP'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'SI_CONVERSION'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'TIME_BASE'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'UNITS'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMIN'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMAX'
	od29 -> CreateAttr, /VARIABLE_SCOPE, 'VAR_TYPE'
	
	;Epoch
	od29 -> WriteVarAttr, epoch_vname, 'CATDESC',       'A place holder.'
	od29 -> WriteVarAttr, epoch_vname, 'FIELDNAM',      'Time'
	od29 -> WriteVarAttr, epoch_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_vname, 'FORMAT',        'I16'
	od29 -> WriteVarAttr, epoch_vname, 'LABLAXIS',      'UT'
	od29 -> WriteVarAttr, epoch_vname, 'SI_CONVERSION', '1e-9>s'
	od29 -> WriteVarAttr, epoch_vname, 'TIME_BASE',     'J2000'
	od29 -> WriteVarAttr, epoch_vname, 'UNITS',         'ns'
	od29 -> WriteVarAttr, epoch_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_vname, 'VAR_TYPE',      'support_data'
	
	;TT2000_DATA29
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'CATDESC',       'Pitch angle 90 degree electrons from GDU1 - may contain traces of EDI electron beams'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FIELDNAM',      'Time'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FORMAT',        'I16'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'LABLAXIS',      'UT'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'SI_CONVERSION', '1e-9>s'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'TIME_BASE',     'J2000'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'UNITS',         'ns'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VAR_TYPE',      'support_data'
	
	;TT2000_GDU1
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'CATDESC',       'TT2000 time tags for some GDU1 quantities.'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FIELDNAM',      'Time'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FORMAT',        'I16'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'LABLAXIS',      'UT'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'SI_CONVERSION', '1e-9>s'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'TIME_BASE',     'J2000'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'UNITS',         'UT'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VAR_TYPE',      'support_data'
	
	;TT2000_GDU1
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'CATDESC',       'TT2000 time tags for some GDU2 quantities.'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FIELDNAM',      'Time'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FORMAT',        'I16'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'LABLAXIS',      'UT'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'SI_CONVERSION', '1e-9>s'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'TIME_BASE',     'J2000'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'UNITS',         'UT'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VAR_TYPE',      'support_data'
	
	;EPOCH_TIMETAG
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'CATDESC',       'TT2000 time tags each packet.'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FIELDNAM',      'Time'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'FORMAT',        'I16'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'LABLAXIS',      'UT'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'SI_CONVERSION', '1e-9>s'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'TIME_BASE',     'J2000'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'UNITS',         'UT'
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu1_vname, 'VAR_TYPE',      'support_data'
	
	;TT2000_GDU2
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'CATDESC',       'TT2000 time tags for quality 0 EDI GDU2 counts.'
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'FIELDNAM',      'Time'
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'FILLVAL',       MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'FORMAT',        'I16'
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'LABLAXIS',      'UT'
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'SI_CONVERSION', '1e-9>s'
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'TIME_BASE',     'J2000'
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'UNITS',         'UT'
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	od29 -> WriteVarAttr, epoch_gdu2_vname, 'VAR_TYPE',      'support_data'

	;OPTICS
	od29 -> WriteVarAttr, optics_vname, 'CATDESC',       'EDI optics state.'
	od29 -> WriteVarAttr, optics_vname, 'DEPEND_0',       epoch_timetag_vname
	od29 -> WriteVarAttr, optics_vname, 'FIELDNAM',      'Optics state'
	od29 -> WriteVarAttr, optics_vname, 'FILLVAL',        65535US
	od29 -> WriteVarAttr, optics_vname, 'FORMAT',        'I4'
	od29 -> WriteVarAttr, optics_vname, 'LABLAXIS',      'Optics'
	od29 -> WriteVarAttr, optics_vname, 'VALIDMIN',      0US
	od29 -> WriteVarAttr, optics_vname, 'VALIDMAX',      65533US
	od29 -> WriteVarAttr, optics_vname, 'VAR_TYPE',      'support_data'

	;ENERGY_GDU1
	od29 -> WriteVarAttr, e_gdu1_vname, 'CATDESC',       'GDU1 energy'
	od29 -> WriteVarAttr, e_gdu1_vname, 'DEPEND_0',       epoch_gdu1_vname
	od29 -> WriteVarAttr, e_gdu1_vname, 'FIELDNAM',      'Energy'
	od29 -> WriteVarAttr, e_gdu1_vname, 'FILLVAL',        65535US
	od29 -> WriteVarAttr, e_gdu1_vname, 'FORMAT',        'I4'
	od29 -> WriteVarAttr, e_gdu1_vname, 'LABLAXIS',      'Energy'
	od29 -> WriteVarAttr, e_gdu1_vname, 'SI_CONVERSION', '1.602e-19>J'
	od29 -> WriteVarAttr, e_gdu1_vname, 'UNITS',         'eV'
	od29 -> WriteVarAttr, e_gdu1_vname, 'VALIDMIN',      0US
	od29 -> WriteVarAttr, e_gdu1_vname, 'VALIDMAX',      1000US
	od29 -> WriteVarAttr, e_gdu1_vname, 'VAR_TYPE',      'support_data'

	;ENERGY_GDU2
	od29 -> WriteVarAttr, e_gdu2_vname, 'CATDESC',       'GDU2 energy'
	od29 -> WriteVarAttr, e_gdu2_vname, 'DEPEND_0',      epoch_gdu2_vname
	od29 -> WriteVarAttr, e_gdu2_vname, 'FIELDNAM',      'Energy'
	od29 -> WriteVarAttr, e_gdu2_vname, 'FILLVAL',       65535US
	od29 -> WriteVarAttr, e_gdu2_vname, 'FORMAT',        'I4'
	od29 -> WriteVarAttr, e_gdu2_vname, 'LABLAXIS',      'Energy'
	od29 -> WriteVarAttr, e_gdu2_vname, 'SI_CONVERSION', '1.602e-19>J'
	od29 -> WriteVarAttr, e_gdu2_vname, 'UNITS',         'eV'
	od29 -> WriteVarAttr, e_gdu2_vname, 'VALIDMIN',      0US
	od29 -> WriteVarAttr, e_gdu2_vname, 'VALIDMAX',      1000US
	od29 -> WriteVarAttr, e_gdu2_vname, 'VAR_TYPE',      'support_data'

	;DATA29_GDU1
	od29 -> WriteVarAttr, d29_gdu1_vname, 'CATDESC',      'GDU1 pitch angle 90 degree electrons - may contain traces of EDI electron beams'
	od29 -> WriteVarAttr, d29_gdu1_vname, 'DEPEND_0',      epoch_d29_vname
	od29 -> WriteVarAttr, d29_gdu1_vname, 'DISPLAY_TYPE', 'time_series'
	od29 -> WriteVarAttr, d29_gdu1_vname, 'FIELDNAM',     'Pitch angle 90 electrons from GDU1'
	od29 -> WriteVarAttr, d29_gdu1_vname, 'FILLVAL',      65535US
	od29 -> WriteVarAttr, d29_gdu1_vname, 'FORMAT',       'I5'
	od29 -> WriteVarAttr, d29_gdu1_vname, 'LABLAXIS',     'data29'
	od29 -> WriteVarAttr, d29_gdu1_vname, 'SCALETYP',     'log'
	od29 -> WriteVarAttr, d29_gdu1_vname, 'UNITS',        'counts'
	od29 -> WriteVarAttr, d29_gdu1_vname, 'VALIDMIN',     0US
	od29 -> WriteVarAttr, d29_gdu1_vname, 'VALIDMAX',     65534US
	od29 -> WriteVarAttr, d29_gdu1_vname, 'VAR_TYPE',     'data'

	;DATA29_GDU2
	od29 -> WriteVarAttr, d29_gdu2_vname, 'CATDESC',      'GDU2 pitch angle 90 degree electrons - may contain traces of EDI electron beams'
	od29 -> WriteVarAttr, d29_gdu2_vname, 'DEPEND_0',      epoch_d29_vname
	od29 -> WriteVarAttr, d29_gdu2_vname, 'DISPLAY_TYPE', 'time_series'
	od29 -> WriteVarAttr, d29_gdu2_vname, 'FIELDNAM',     'Pitch angle 90 electrons from GDU2'
	od29 -> WriteVarAttr, d29_gdu2_vname, 'FILLVAL',      65535US
	od29 -> WriteVarAttr, d29_gdu2_vname, 'FORMAT',       'I5'
	od29 -> WriteVarAttr, d29_gdu2_vname, 'LABLAXIS',     'data29'
	od29 -> WriteVarAttr, d29_gdu2_vname, 'SCALETYP',     'log'
	od29 -> WriteVarAttr, d29_gdu2_vname, 'UNITS',        'counts'
	od29 -> WriteVarAttr, d29_gdu2_vname, 'VALIDMIN',     0US
	od29 -> WriteVarAttr, d29_gdu2_vname, 'VALIDMAX',     65534US
	od29 -> WriteVarAttr, d29_gdu2_vname, 'VAR_TYPE',     'data'

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, od29
	return, d29_file
end