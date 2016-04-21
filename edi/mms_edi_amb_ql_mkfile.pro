; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ql_write
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
;                           Spacecraft ID: 'mms1', 'mms2', 'mms3', or 'mms4'
;       MODE:               in, required, type=string
;                           Data rate mode: 'slow', 'fast', 'srvy', or 'brst'
;       TSTART:             in, required, type=string
;                           Start time of the data file to be written, formatted as
;                           'YYYYMMDDhhmmss' for burst mode files, and 'YYYYMMDD'
;                               otherwise.
;       AMB_DATA:           in, required, type=struct
;                           EDI ambient data structure with the following fields::
;                               TT2000_0    - TT2000 time tags for 0-pitch angle sorted data
;                               TT2000_180  - TT2000 time tags for 180-pitch angle sorted data
;                               TT2000_TT   - TT2000 time tags for packet-resolution data
;                               ENERGY_GDU1 - Electron energy for GDU1
;                               ENERGY_GDU2 - Electron energy for GDU2
;                               PACK_MODE   - Packing mode
;                               COUNTS1_0   - Counts1 data sorted by 0-degree pitch mode
;                               COUNTS1_180 - Counts1 data sorted by 180-degree pitch mode
;                               COUNTS2_0   - Counts2 data sorted by 0-degree pitch mode (brst only)
;                               COUNTS2_180 - Counts2 data sorted by 180-degree pitch mode (brst only)
;                               COUNTS3_0   - Counts3 data sorted by 0-degree pitch mode (brst only)
;                               COUNTS3_180 - Counts3 data sorted by 180-degree pitch mode (brst only)
;                               COUNTS4_0   - Counts4 data sorted by 0-degree pitch mode (brst only)
;                               COUNTS4_180 - Counts4 data sorted by 180-degree pitch mode (brst only)
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
;       EMPTY_FILE:         in, optional, type=boolean, default=0
;                           If set, and "empty" file will be created. An empty file contains
;                               a single epoch value marking the beginning of the day.
;       OPTDESC:            in, optional, type=string, default='amb'
;                           Optional filename descriptor, with parts separated by a hyphen.
;       PARENTS:            in, optional, type=string/strarr, default=''
;                           Names of the parent files required to make `AMB_DATA`.
;       STATUS:             out, required, type=byte
;                           An error code. Values are:::
;                               OK      = 0
;                               Warning = 1-99
;                               Error   = 100-255
;                                   100      -  Unexpected trapped error
;
; :Returns:
;       AMB_FILE:           Name of the file created.
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
;-
function mms_edi_amb_ql_mkfile, sc, mode, tstart, $
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
		
		;Close and delete the file, if it was created
		if obj_valid(oamb) then obj_destroy, oamb
		if n_elements(amb_file) gt 0 && file_test(amb_file) then file_delete, amb_file
		
		;Report error
		if n_elements(status) eq 0 || status eq 0 then status = 100
		MrPrintF, 'LogErr'
		
		;Return
		return, ''
	endif

;------------------------------------;
; Check Inputs                       ;
;------------------------------------;
	
	;Defaults
	tf_empty = keyword_set(empty_file)
	if n_elements(sc)      eq 0 || sc      eq '' then sc      = 'mms#'
	if n_elements(mode)    eq 0 || mode    eq '' then mode    = 'mode'
	if n_elements(optdesc) eq 0                  then optdesc = 'amb'
	if n_elements(parents) eq 0                  then parents = ' '
	if n_elements(tstart)  eq 0 || tstart  eq '' then begin
		MrCDF_Epoch_Breakdown, amb_data.tt2000_0[0], yr, mo, day, hr, mn, sec
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
	mods = [ 'v0.0.0 - Original version.', $
	         'v0.1.0 - Added PACK_MODE variable.', $
	         'v1.0.0 - Removed PACK_MODE. Add relative calibrations.', $
	         'v2.0.0 - Added optics state.', $
	         'v3.0.0 - Update PI_name. Counts are CDF_UINT4. Single time tag for counts.', $
	         'v3.1.0 - Update metadata.' ]

	;Get the version
	version = stregex(mods[-1], '^v([0-9]+)\.([0-9]+)\.([0-9]+)', /SUBEXP, /EXTRACT)
	vx      = strtrim(version[1], 2)
	vy      = strtrim(version[2], 2)
	vz      = strtrim(version[3], 2)
	
	;Constants for output file
	instr   = 'edi'
	level   = 'ql'

;------------------------------------;
; Get Y-Version From Cal File        ;
;------------------------------------;
	ical = where(strpos(parents, '_cal_') ne -1, ncal)
	if ncal gt 0 then begin
		cal_file = parents[ical[0]]

		;Get the Y-version number
		mms_dissect_filename, cal_file, VY=vy
	
	;Use the Y-Version in MODS.
	;   - Already taken care of above.
	endif else begin
		MrPrintF, 'LogWarn', 'No cal file in PARENTS. Using MODS.'
	endelse

;------------------------------------;
; Create Output File Name            ;
;------------------------------------;
	;Output file
	version = vx + '.' + vy + '.' + vz
	amb_file = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)
	
	;Find the latest z-version
	;   - Look in both DROPBOX and DATA_PATH
	vz = mms_latest_zversion(dropbox, amb_file, ROOT=data_path)
	
	;Reform the file name
	version = vx + '.' + vy + '.' + string(vz, FORMAT='(i0)')
	amb_file = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)
	amb_file = filepath(amb_file, ROOT_DIR=dropbox)

	;Notify where file is located
	MrPrintF, 'LogText', 'Creating EDI AMB file at "' + amb_file + '".'

;------------------------------------;
; Create File                        ;
;------------------------------------;

	;Open the CDF file
	oamb = MrCDF_File(amb_file, /CREATE, /CLOBBER)
	if obj_valid(oamb) eq 0 then return, ''

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
	logical_file_id = cgRootName(amb_file)
	source_name = 'MMS' + strmid(sc, 3) + '>MMS Satellite Number ' + strmid(sc, 3)
	MrTimeParser, MrTimeStamp(/UTC), '%Y-%M-%dT%H:%m:%S', '%Y%M%d', gen_date

	;   - Instrument Type (1+)
	;           Electric Fields (space)
	;           Magnetic Fields (space)
	;           Particles (space)
	;           Plasma and Solar Wind
	;           Spacecraft Potential Control
	oamb -> WriteGlobalAttr, /CREATE, 'Data_type',                  data_type
	oamb -> WriteGlobalAttr, /CREATE, 'Data_version',               version
	oamb -> WriteGlobalAttr, /CREATE, 'Descriptor',                 'EDI'
	oamb -> WriteGlobalAttr, /CREATE, 'Discipline',                 'Space Physics>Magnetospheric Science'
	oamb -> WriteGlobalAttr, /CREATE, 'File_naming_convention',     'source_descriptor_datatype_yyyyMMdd'
	oamb -> WriteGlobalAttr, /CREATE, 'Generation_date',            gen_date
	oamb -> WriteGlobalAttr, /CREATE, 'Instrument_type',            'Particles (space)'
	oamb -> WriteGlobalAttr, /CREATE, 'Logical_file_id',            logical_file_id
	oamb -> WriteGlobalAttr, /CREATE, 'Logical_source',             logical_source
	oamb -> WriteGlobalAttr, /CREATE, 'Logical_source_description', 'Quick-look EDI Ambient Counts'
	oamb -> WriteGlobalAttr, /CREATE, 'Mission_group',              'MMS'
	oamb -> WriteGlobalAttr, /CREATE, 'PI_affiliation',             'UNH'
	oamb -> WriteGlobalAttr, /CREATE, 'PI_name',                    'Roy Torbert, Hans Vaith'
	oamb -> WriteGlobalAttr, /CREATE, 'Project',                    'STP>Solar Terrestrial Physics'
	oamb -> WriteGlobalAttr, /CREATE, 'Source_name',                source_name
	oamb -> WriteGlobalAttr, /CREATE, 'TEXT',                       'EDI ambient data. The EDI instrument paper and data products guide ' + $
	                                                                'can be found at the following two links: ' + $
	                                                                'http://link.springer.com/article/10.1007%2Fs11214-015-0182-7, ' + $
	                                                                'https://lasp.colorado.edu/mms/sdc/public/datasets/fields/'
	oamb -> WriteGlobalAttr, /CREATE, 'HTTP_LINK',                  ['http://mms-fields.unh.edu/', $
	                                                                 'http://mms.gsfc.nasa.gov/index.html']
	oamb -> WriteGlobalAttr, /CREATE, 'LINK_TEXT',                  ['UNH FIELDS Home Page', $
	                                                                 'NASA MMS Home']
	oamb -> WriteGlobalAttr, /CREATE, 'MODS',                       mods
	oamb -> WriteGlobalAttr, /CREATE, 'Acknowledgements',           ' '
	oamb -> WriteGlobalAttr, /CREATE, 'Generated_by',               'University of New Hampshire'
	oamb -> WriteGlobalAttr, /CREATE, 'Parents',                    parents
	oamb -> WriteGlobalAttr, /CREATE, 'Skeleton_version',           ' '
	oamb -> WriteGlobalAttr, /CREATE, 'Rules_of_use',               ' '
	oamb -> WriteGlobalAttr, /CREATE, 'Time_resolution',            ' '

;------------------------------------------------------
; Variables                                           |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName_optionalDescriptor
	
	epoch_vname           = 'Epoch'
	t_tt_vname            = 'epoch_timetag'
	optics_vname          = mms_construct_varname(sc, instr, 'optics',  'state')
	e_gdu1_vname          = mms_construct_varname(sc, instr, 'energy',  'gdu1')
	e_gdu2_vname          = mms_construct_varname(sc, instr, 'energy',  'gdu2')
	gdu_0_vname           = mms_construct_varname(sc, instr, 'gdu',     '0')
	gdu_180_vname         = mms_construct_varname(sc, instr, 'gdu',     '180')
	counts1_0_vname       = mms_construct_varname(sc, instr, 'counts1', '0')
	counts2_0_vname       = mms_construct_varname(sc, instr, 'counts2', '0')
	counts3_0_vname       = mms_construct_varname(sc, instr, 'counts3', '0')
	counts4_0_vname       = mms_construct_varname(sc, instr, 'counts4', '0')
	counts1_180_vname     = mms_construct_varname(sc, instr, 'counts1', '180')
	counts2_180_vname     = mms_construct_varname(sc, instr, 'counts2', '180')
	counts3_180_vname     = mms_construct_varname(sc, instr, 'counts3', '180')
	counts4_180_vname     = mms_construct_varname(sc, instr, 'counts4', '180')

	;Write variable data to file
	oamb -> CreateVar, epoch_vname,   'CDF_TIME_TT2000'
	oamb -> CreateVar, t_tt_vname,    'CDF_TIME_TT2000'
	oamb -> CreateVar, optics_vname,  'CDF_UINT2', COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> CreateVar, e_gdu1_vname,  'CDF_UINT2', COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> CreateVar, e_gdu2_vname,  'CDF_UINT2', COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> CreateVar, gdu_0_vname,   'CDF_UINT1', COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> CreateVar, gdu_180_vname, 'CDF_UINT1', COMPRESSION='GZIP', GZIP_LEVEL=6

	;Put group variables by pitch angle.
	if mode eq 'brst' then begin
		oamb -> CreateVar, counts1_0_vname,   'CDF_UINT4', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, counts2_0_vname,   'CDF_UINT4', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, counts3_0_vname,   'CDF_UINT4', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, counts4_0_vname,   'CDF_UINT4', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, counts1_180_vname, 'CDF_UINT4', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, counts2_180_vname, 'CDF_UINT4', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, counts3_180_vname, 'CDF_UINT4', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, counts4_180_vname, 'CDF_UINT4', COMPRESSION='GZIP', GZIP_LEVEL=6
	endif else begin
		oamb -> CreateVar, counts1_0_vname,    'CDF_UINT4', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, counts1_180_vname,  'CDF_UINT4', COMPRESSION='GZIP', GZIP_LEVEL=6
	endelse

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
		oamb -> WriteVar, epoch_vname, epoch_val
	endif
	
;------------------------------------------------------
; Variable Attributes                                 |
;------------------------------------------------------
	;Create the variable attributes
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'CATDESC'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_0'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'DISPLAY_TYPE'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'FIELDNAM'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'FILLVAL'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'FORMAT'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'LABLAXIS'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'SCALETYP'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'SI_CONVERSION'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'TIME_BASE'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'UNITS'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMIN'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMAX'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'VAR_TYPE'
	
	;Epoch
	oamb -> WriteVarAttr, epoch_vname, 'CATDESC',       'TT2000 time tags for EDI electron counts.'
	oamb -> WriteVarAttr, epoch_vname, 'FIELDNAM',      'Time'
	oamb -> WriteVarAttr, epoch_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oamb -> WriteVarAttr, epoch_vname, 'FORMAT',        'I16'
	oamb -> WriteVarAttr, epoch_vname, 'LABLAXIS',      'UT'
	oamb -> WriteVarAttr, epoch_vname, 'SI_CONVERSION', '1e-9>s'
	oamb -> WriteVarAttr, epoch_vname, 'TIME_BASE',     'J2000'
	oamb -> WriteVarAttr, epoch_vname, 'UNITS',         'ns'
	oamb -> WriteVarAttr, epoch_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	oamb -> WriteVarAttr, epoch_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	oamb -> WriteVarAttr, epoch_vname, 'VAR_TYPE',      'support_data'
	
	;EPOCH_TIMETAG
	oamb -> WriteVarAttr, t_tt_vname, 'CATDESC',       'TT2000 time tags for EDI support data.'
	oamb -> WriteVarAttr, t_tt_vname, 'FIELDNAM',      'Time'
	oamb -> WriteVarAttr, t_tt_vname, 'FILLVAL',       MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oamb -> WriteVarAttr, t_tt_vname, 'FORMAT',        'I16'
	oamb -> WriteVarAttr, t_tt_vname, 'LABLAXIS',      'UT'
	oamb -> WriteVarAttr, t_tt_vname, 'SI_CONVERSION', '1e-9>s'
	oamb -> WriteVarAttr, t_tt_vname, 'TIME_BASE',     'J2000'
	oamb -> WriteVarAttr, t_tt_vname, 'UNITS',         'ns'
	oamb -> WriteVarAttr, t_tt_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	oamb -> WriteVarAttr, t_tt_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 31), /CDF_EPOCH
	oamb -> WriteVarAttr, t_tt_vname, 'VAR_TYPE',      'support_data'

	;OPTICS
	oamb -> WriteVarAttr, optics_vname, 'CATDESC',       'Optics state'
	oamb -> WriteVarAttr, optics_vname, 'DEPEND_0',       t_tt_vname
	oamb -> WriteVarAttr, optics_vname, 'FIELDNAM',      'Optics state'
	oamb -> WriteVarAttr, optics_vname, 'FILLVAL',        255B
	oamb -> WriteVarAttr, optics_vname, 'FORMAT',        'I4'
	oamb -> WriteVarAttr, optics_vname, 'LABLAXIS',      'Optics'
	oamb -> WriteVarAttr, optics_vname, 'VALIDMIN',      0B
	oamb -> WriteVarAttr, optics_vname, 'VALIDMAX',      253B
	oamb -> WriteVarAttr, optics_vname, 'VAR_TYPE',      'support_data'

	;ENERGY_GDU1
	oamb -> WriteVarAttr, e_gdu1_vname, 'CATDESC',       'GDU1 energy'
	oamb -> WriteVarAttr, e_gdu1_vname, 'DEPEND_0',       t_tt_vname
	oamb -> WriteVarAttr, e_gdu1_vname, 'FIELDNAM',      'Energy'
	oamb -> WriteVarAttr, e_gdu1_vname, 'FILLVAL',        65535US
	oamb -> WriteVarAttr, e_gdu1_vname, 'FORMAT',        'I4'
	oamb -> WriteVarAttr, e_gdu1_vname, 'LABLAXIS',      'Energy'
	oamb -> WriteVarAttr, e_gdu1_vname, 'SI_CONVERSION', '1.602e-19>J'
	oamb -> WriteVarAttr, e_gdu1_vname, 'UNITS',         'eV'
	oamb -> WriteVarAttr, e_gdu1_vname, 'VALIDMIN',      0US
	oamb -> WriteVarAttr, e_gdu1_vname, 'VALIDMAX',      1000US
	oamb -> WriteVarAttr, e_gdu1_vname, 'VAR_TYPE',      'support_data'

	;ENERGY_GDU2
	oamb -> WriteVarAttr, e_gdu2_vname, 'CATDESC',       'GDU2 energy'
	oamb -> WriteVarAttr, e_gdu2_vname, 'DEPEND_0',      t_tt_vname
	oamb -> WriteVarAttr, e_gdu2_vname, 'FIELDNAM',      'Energy'
	oamb -> WriteVarAttr, e_gdu2_vname, 'FILLVAL',       65535US
	oamb -> WriteVarAttr, e_gdu2_vname, 'FORMAT',        'I4'
	oamb -> WriteVarAttr, e_gdu2_vname, 'LABLAXIS',      'Energy'
	oamb -> WriteVarAttr, e_gdu2_vname, 'SI_CONVERSION', '1.602e-19>J'
	oamb -> WriteVarAttr, e_gdu2_vname, 'UNITS',         'eV'
	oamb -> WriteVarAttr, e_gdu2_vname, 'VALIDMIN',      0US
	oamb -> WriteVarAttr, e_gdu2_vname, 'VALIDMAX',      1000US
	oamb -> WriteVarAttr, e_gdu2_vname, 'VAR_TYPE',      'support_data'

	;GDU_0
	oamb -> WriteVarAttr, gdu_0_vname, 'CATDESC',       'Sorts 0 degree counts by GDU'
	oamb -> WriteVarAttr, gdu_0_vname, 'DEPEND_0',       epoch_vname
	oamb -> WriteVarAttr, gdu_0_vname, 'FIELDNAM',      'GDU Identifier'
	oamb -> WriteVarAttr, gdu_0_vname, 'FILLVAL',        255B
	oamb -> WriteVarAttr, gdu_0_vname, 'FORMAT',        'I1'
	oamb -> WriteVarAttr, gdu_0_vname, 'VALIDMIN',      1B
	oamb -> WriteVarAttr, gdu_0_vname, 'VALIDMAX',      2B
	oamb -> WriteVarAttr, gdu_0_vname, 'VAR_TYPE',      'support_data'

	;GDU_180
	oamb -> WriteVarAttr, gdu_180_vname, 'CATDESC',       'Sorts 180 degree counts by GDU'
	oamb -> WriteVarAttr, gdu_180_vname, 'DEPEND_0',       epoch_vname
	oamb -> WriteVarAttr, gdu_180_vname, 'FIELDNAM',      'GDU Identifier'
	oamb -> WriteVarAttr, gdu_180_vname, 'FILLVAL',        255B
	oamb -> WriteVarAttr, gdu_180_vname, 'FORMAT',        'I1'
	oamb -> WriteVarAttr, gdu_180_vname, 'VALIDMIN',      1B
	oamb -> WriteVarAttr, gdu_180_vname, 'VALIDMAX',      2B
	oamb -> WriteVarAttr, gdu_180_vname, 'VAR_TYPE',      'support_data'

	;COUNTS1_0
	oamb -> WriteVarAttr, counts1_0_vname, 'CATDESC',      'Field-aligned electrons from the counts1 anode. Actual ' + $
	                                                       'pitch-angle depends on the packing mode. See the EDI ' + $
	                                                       'data products guide for more details.'
	oamb -> WriteVarAttr, counts1_0_vname, 'DEPEND_0',      epoch_vname
	oamb -> WriteVarAttr, counts1_0_vname, 'DISPLAY_TYPE', 'time_series'
	oamb -> WriteVarAttr, counts1_0_vname, 'FIELDNAM',     'Electron Counts PA0'
	oamb -> WriteVarAttr, counts1_0_vname, 'FILLVAL',      4294967295UL
	oamb -> WriteVarAttr, counts1_0_vname, 'FORMAT',       'I5'
	oamb -> WriteVarAttr, counts1_0_vname, 'LABLAXIS',     'counts'
	oamb -> WriteVarAttr, counts1_0_vname, 'SCALETYP',     'log'
	oamb -> WriteVarAttr, counts1_0_vname, 'UNITS',        'counts'
	oamb -> WriteVarAttr, counts1_0_vname, 'VALIDMIN',     0UL
	oamb -> WriteVarAttr, counts1_0_vname, 'VALIDMAX',     4294967293UL
	oamb -> WriteVarAttr, counts1_0_vname, 'VAR_TYPE',     'data'

	;COUNTS1_180
	oamb -> WriteVarAttr, counts1_180_vname, 'CATDESC',      'Anti-field-aligned electrons from the counts1 anode. Actual ' + $
	                                                         'pitch-angle depends on the packing mode. See the EDI ' + $
	                                                         'data products guide for more details.'
	oamb -> WriteVarAttr, counts1_180_vname, 'DEPEND_0',      epoch_vname
	oamb -> WriteVarAttr, counts1_180_vname, 'DISPLAY_TYPE', 'time_series'
	oamb -> WriteVarAttr, counts1_180_vname, 'FIELDNAM',     'Electron Counts PA180'
	oamb -> WriteVarAttr, counts1_180_vname, 'FILLVAL',      4294967295UL
	oamb -> WriteVarAttr, counts1_180_vname, 'FORMAT',       'I5'
	oamb -> WriteVarAttr, counts1_180_vname, 'LABLAXIS',     'counts'
	oamb -> WriteVarAttr, counts1_180_vname, 'SCALETYP',     'log'
	oamb -> WriteVarAttr, counts1_180_vname, 'UNITS',        'counts'
	oamb -> WriteVarAttr, counts1_180_vname, 'VALIDMIN',     0UL
	oamb -> WriteVarAttr, counts1_180_vname, 'VALIDMAX',     4294967293UL
	oamb -> WriteVarAttr, counts1_180_vname, 'VAR_TYPE',     'data'

	;BURST DATA
	if mode eq 'brst' then begin
		;COUNTS2_PA0
		oamb -> WriteVarAttr, counts2_0_vname, 'CATDESC',      'Field-aligned electrons from the counts2 anode. Actual ' + $
		                                                       'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                       'data products guide for more details.'
		oamb -> WriteVarAttr, counts2_0_vname, 'DEPEND_0',      epoch_vname
		oamb -> WriteVarAttr, counts2_0_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts2_0_vname, 'FIELDNAM',     'Electron Counts PA0'
		oamb -> WriteVarAttr, counts2_0_vname, 'FILLVAL',      4294967295UL
		oamb -> WriteVarAttr, counts2_0_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts2_0_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts2_0_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts2_0_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts2_0_vname, 'VALIDMIN',     0UL
		oamb -> WriteVarAttr, counts2_0_vname, 'VALIDMAX',     4294967293UL
		oamb -> WriteVarAttr, counts2_0_vname, 'VAR_TYPE',     'data'

		;COUNTS3_PA0
		oamb -> WriteVarAttr, counts3_0_vname, 'CATDESC',      'Field-aligned electrons from the counts3 anode. Actual ' + $
		                                                       'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                       'data products guide for more details.'
		oamb -> WriteVarAttr, counts3_0_vname, 'DEPEND_0',      epoch_vname
		oamb -> WriteVarAttr, counts3_0_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts3_0_vname, 'FIELDNAM',     'Electron Counts PA0'
		oamb -> WriteVarAttr, counts3_0_vname, 'FILLVAL',      4294967295UL
		oamb -> WriteVarAttr, counts3_0_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts3_0_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts3_0_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts3_0_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts3_0_vname, 'VALIDMIN',     0UL
		oamb -> WriteVarAttr, counts3_0_vname, 'VALIDMAX',     4294967293UL
		oamb -> WriteVarAttr, counts3_0_vname, 'VAR_TYPE',     'data'

		;COUNTS4_PA0
		oamb -> WriteVarAttr, counts4_0_vname, 'CATDESC',      'Field-aligned electrons from the counts4 anode. Actual ' + $
		                                                       'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                       'data products guide for more details.'
		oamb -> WriteVarAttr, counts4_0_vname, 'DEPEND_0',      epoch_vname
		oamb -> WriteVarAttr, counts4_0_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts4_0_vname, 'FIELDNAM',     'Electron Counts PA0'
		oamb -> WriteVarAttr, counts4_0_vname, 'FILLVAL',      4294967295UL
		oamb -> WriteVarAttr, counts4_0_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts4_0_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts4_0_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts4_0_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts4_0_vname, 'VALIDMIN',     0UL
		oamb -> WriteVarAttr, counts4_0_vname, 'VALIDMAX',     4294967293UL
		oamb -> WriteVarAttr, counts4_0_vname, 'VAR_TYPE',     'data'

		;COUNTS2_PA180
		oamb -> WriteVarAttr, counts2_180_vname, 'CATDESC',      'Anti-field-aligned electrons from the counts2 anode. Actual ' + $
		                                                         'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                         'data products guide for more details.'
		oamb -> WriteVarAttr, counts2_180_vname, 'DEPEND_0',      epoch_vname
		oamb -> WriteVarAttr, counts2_180_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts2_180_vname, 'FIELDNAM',     'Electron Counts PA180'
		oamb -> WriteVarAttr, counts2_180_vname, 'FILLVAL',      4294967295UL
		oamb -> WriteVarAttr, counts2_180_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts2_180_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts2_180_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts2_180_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts2_180_vname, 'VALIDMIN',     0UL
		oamb -> WriteVarAttr, counts2_180_vname, 'VALIDMAX',     4294967293UL
		oamb -> WriteVarAttr, counts2_180_vname, 'VAR_TYPE',     'data'

		;COUNTS3_PA180
		oamb -> WriteVarAttr, counts3_180_vname, 'CATDESC',      'Anti-field-aligned electrons from the counts3 anode. Actual ' + $
		                                                         'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                         'data products guide for more details.'
		oamb -> WriteVarAttr, counts3_180_vname, 'DEPEND_0',      epoch_vname
		oamb -> WriteVarAttr, counts3_180_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts3_180_vname, 'FIELDNAM',     'Electron Counts PA180'
		oamb -> WriteVarAttr, counts3_180_vname, 'FILLVAL',      4294967295UL
		oamb -> WriteVarAttr, counts3_180_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts3_180_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts3_180_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts3_180_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts3_180_vname, 'VALIDMIN',     0UL
		oamb -> WriteVarAttr, counts3_180_vname, 'VALIDMAX',     4294967293UL
		oamb -> WriteVarAttr, counts3_180_vname, 'VAR_TYPE',     'data'

		;COUNTS4_PA180
		oamb -> WriteVarAttr, counts4_180_vname, 'CATDESC',      'Anti-field-aligned electrons from the counts4 anode. Actual ' + $
		                                                         'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                         'data products guide for more details.'
		oamb -> WriteVarAttr, counts4_180_vname, 'DEPEND_0',      epoch_vname
		oamb -> WriteVarAttr, counts4_180_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts4_180_vname, 'FIELDNAM',     'Electron Counts PA180'
		oamb -> WriteVarAttr, counts4_180_vname, 'FILLVAL',      4294967295UL
		oamb -> WriteVarAttr, counts4_180_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts4_180_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts4_180_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts4_180_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts4_180_vname, 'VALIDMIN',     0UL
		oamb -> WriteVarAttr, counts4_180_vname, 'VALIDMAX',     4294967293UL
		oamb -> WriteVarAttr, counts4_180_vname, 'VAR_TYPE',     'data'
	endif

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	;Close the file
	obj_destroy, oamb
	
	;Return
	if n_elements(status) eq 0 then status = 0
	return, amb_file
end