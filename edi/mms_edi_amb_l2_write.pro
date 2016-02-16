; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ql_write
;
; PURPOSE:
;+
;   Process EDI ambient mode data to produce a level 2 data product with relative
;   and absolute calibrations.
;
; :Categories:
;    MMS, EDI, L2, Ambient
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
;       2015/10/26  -   Written by Matthew Argall
;       2015/01/16  -   Determine the correct output file version more reliably.
;                           Change inputs to make program more versatile. - MRA
;-
function mms_edi_amb_l2_write, sc, mode, tstart, amb_data, $
DROPBOX_ROOT=dropbox, $
DATA_PATH_ROOT=data_path, $
OPTDESC=optdesc, $
PARENTS=parents
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(oamb) then obj_destroy, oamb
		if n_elements(amb_file) gt 0 && file_test(amb_file) then file_delete, amb_file
		MrPrintF, 'LogErr'
		return, ''
	endif

;------------------------------------;
; Version History                    ;
;------------------------------------;
	;Mods to data processing
	mods = [ 'v0.0.0 - Original version.' ]
	
	;Get the version
	version = stregex(mods[-1], '^v([0-9]+)\.([0-9]+)\.([0-9]+)', /SUBEXP, /EXTRACT)
	vx      = strtrim(version[1], 2)
	vy      = strtrim(version[2], 2)
	vz      = strtrim(version[3], 2)
	
	;Constants for output file
	instr   = 'edi'
	level   = 'q2'

;------------------------------------;
; Check Inputs                       ;
;------------------------------------;
	
	;Defaults
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
; Check Data                         ;
;------------------------------------;
	;
	; Check sizes
	;
	if ~isa(amb_data.tt2000_0,     'LONG64') then message, 'amb_data.tt2000_0 must be LONG64.'
	if ~isa(amb_data.tt2000_180,   'LONG64') then message, 'amb_data.tt2000_180 must be LONG64.'
	if ~isa(amb_data.tt2000_tt,    'LONG64') then message, 'amb_data.epoch_timetag must be LONG64.'
	if ~isa(amb_data.energy_gdu1,  'UINT')   then message, 'amb_data.energy_gdu1 must be UINT.'
	if ~isa(amb_data.energy_gdu2,  'UINT')   then message, 'amb_data.energy_gdu2 must be UINT.'
	if ~isa(amb_data.gdu_0,        'BYTE')   then message, 'amb_data.gdu_0 must be BYTE.'
	if ~isa(amb_data.gdu_180,      'BYTE')   then message, 'amb_data.gdu_180 must be BYTE.'
	if ~isa(amb_data.counts1_0,    'FLOAT')  then message, 'amb_data.counts1_0 must be UINT.'
	if ~isa(amb_data.counts1_180,  'FLOAT')  then message, 'amb_data.counts1_180 must be UINT.'
	if mode eq 'brst' then begin
		if ~isa(amb_data.counts2_0,    'FLOAT') then message, 'amb_data.counts2_0 must be UINT.'
		if ~isa(amb_data.counts3_0,    'FLOAT') then message, 'amb_data.counts3_0 must be UINT.'
		if ~isa(amb_data.counts4_0,    'FLOAT') then message, 'amb_data.counts4_0 must be UINT.'
		if ~isa(amb_data.counts2_180,  'FLOAT') then message, 'amb_data.counts2_180 must be UINT.'
		if ~isa(amb_data.counts3_180,  'FLOAT') then message, 'amb_data.counts3_180 must be UINT.'
		if ~isa(amb_data.counts4_180,  'FLOAT') then message, 'amb_data.counts4_180 must be UINT.'
	endif

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
	oamb -> WriteGlobalAttr, /CREATE, 'Data_Type',                  data_type
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
	oamb -> WriteGlobalAttr, /CREATE, 'PI_name',                    'Hans Vaith'
	oamb -> WriteGlobalAttr, /CREATE, 'Project',                    'STP>Solar Terrestrial Physics'
	oamb -> WriteGlobalAttr, /CREATE, 'Source_name',                source_name
	oamb -> WriteGlobalAttr, /CREATE, 'TEXT',                       'EDI ambient data. Instrument papers ' + $
	                                                                'for EDI can be found at: ' + $
	                                                                'http://link.springer.com/article/10.1007%2Fs11214-015-0182-7'
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
	
	t_0_vname             = 'epoch_pa0'
	t_180_vname           = 'epoch_pa180'
	t_tt_vname            = 'epoch_timetag'
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
	pa1_0_vname           = mms_construct_varname(sc, instr, 'pa1',     '0')
	pa2_0_vname           = mms_construct_varname(sc, instr, 'pa2',     '0')
	pa3_0_vname           = mms_construct_varname(sc, instr, 'pa3',     '0')
	pa4_0_vname           = mms_construct_varname(sc, instr, 'pa4',     '0')
	pa1_180_vname         = mms_construct_varname(sc, instr, 'pa1',     '180')
	pa2_180_vname         = mms_construct_varname(sc, instr, 'pa2',     '180')
	pa3_180_vname         = mms_construct_varname(sc, instr, 'pa3',     '180')
	pa4_180_vname         = mms_construct_varname(sc, instr, 'pa4',     '180')
	pa1_0_delta_minus   = 'pa1_0_delta_minus'
	pa1_0_delta_plus    = 'pa1_0_delta_plus'
	pa2_0_delta_minus   = 'pa2_0_delta_minus'
	pa2_0_delta_plus    = 'pa2_0_delta_plus'
	pa3_0_delta_minus   = 'pa3_0_delta_minus'
	pa3_0_delta_plus    = 'pa3_0_delta_plus'
	pa4_0_delta_minus   = 'pa4_0_delta_minus'
	pa4_0_delta_plus    = 'pa4_0_delta_plus'
	pa1_180_delta_minus = 'pa1_180_delta_minus'
	pa1_180_delta_plus  = 'pa1_180_delta_plus'
	pa2_180_delta_minus = 'pa2_180_delta_minus'
	pa2_180_delta_plus  = 'pa2_180_delta_plus'
	pa3_180_delta_minus = 'pa3_180_delta_minus'
	pa3_180_delta_plus  = 'pa3_180_delta_plus'
	pa4_180_delta_minus = 'pa4_180_delta_minus'
	pa4_180_delta_plus  = 'pa4_180_delta_plus'

	;Write variable data to file
	oamb -> WriteVar, /CREATE, t_0_vname,          transpose(amb_data.tt2000_0),    CDF_TYPE='CDF_TIME_TT2000'
	oamb -> WriteVar, /CREATE, t_180_vname,        transpose(amb_data.tt2000_180),  CDF_TYPE='CDF_TIME_TT2000'
	oamb -> WriteVar, /CREATE, t_tt_vname,         transpose(amb_data.tt2000_tt),   CDF_TYPE='CDF_TIME_TT2000'
	oamb -> WriteVar, /CREATE, e_gdu1_vname,       transpose(amb_data.energy_gdu1), COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, e_gdu2_vname,       transpose(amb_data.energy_gdu2), COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, gdu_0_vname,        transpose(amb_data.gdu_0),       COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, gdu_180_vname,      transpose(amb_data.gdu_180),     COMPRESSION='GZIP', GZIP_LEVEL=6
	
	;Group counts by pitch angle
	if mode eq 'brst' then begin
		oamb -> WriteVar, /CREATE, counts1_0_vname,    transpose(amb_data.counts1_0),   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts2_0_vname,    transpose(amb_data.counts2_0),   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts3_0_vname,    transpose(amb_data.counts3_0),   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts4_0_vname,    transpose(amb_data.counts4_0),   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts1_180_vname,  transpose(amb_data.counts1_180), COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts2_180_vname,  transpose(amb_data.counts2_180), COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts3_180_vname,  transpose(amb_data.counts3_180), COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts4_180_vname,  transpose(amb_data.counts4_180), COMPRESSION='GZIP', GZIP_LEVEL=6
	endif else begin
		oamb -> WriteVar, /CREATE, counts1_0_vname,    transpose(amb_data.counts1_0),   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts1_180_vname,  transpose(amb_data.counts1_180), COMPRESSION='GZIP', GZIP_LEVEL=6
	endelse

	;Pitch angle
	if has_tag(amb_data, 'pa0') then begin
		;BRST
		if mode eq 'brst' then begin
			;Pitch angle
			oamb -> WriteVar, /CREATE, pa1_0_vname,         transpose(amb_data.pa0[*,0]),      COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa2_0_vname,         transpose(amb_data.pa0[*,1]),      COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa3_0_vname,         transpose(amb_data.pa0[*,2]),      COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa4_0_vname,         transpose(amb_data.pa0[*,3]),      COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa1_180_vname,       transpose(amb_data.pa180[*,0]),    COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa2_180_vname,       transpose(amb_data.pa180[*,1]),    COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa3_180_vname,       transpose(amb_data.pa180[*,2]),    COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa4_180_vname,       transpose(amb_data.pa180[*,3]),    COMPRESSION='GZIP', GZIP_LEVEL=6
			
			;Delta Minus
			oamb -> WriteVar, /CREATE, pa1_0_delta_minus,   transpose(amb_data.pa0_lo[*,0]),   COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa2_0_delta_minus,   transpose(amb_data.pa0_lo[*,1]),   COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa3_0_delta_minus,   transpose(amb_data.pa0_lo[*,2]),   COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa4_0_delta_minus,   transpose(amb_data.pa0_lo[*,3]),   COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa1_180_delta_minus, transpose(amb_data.pa180_lo[*,0]), COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa2_180_delta_minus, transpose(amb_data.pa180_lo[*,1]), COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa3_180_delta_minus, transpose(amb_data.pa180_lo[*,2]), COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa4_180_delta_minus, transpose(amb_data.pa180_lo[*,3]), COMPRESSION='GZIP', GZIP_LEVEL=6
			
			;Delta Plus
			oamb -> WriteVar, /CREATE, pa1_0_delta_plus,    transpose(amb_data.pa0_hi[*,0]),   COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa2_0_delta_plus,    transpose(amb_data.pa0_hi[*,1]),   COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa3_0_delta_plus,    transpose(amb_data.pa0_hi[*,2]),   COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa4_0_delta_plus,    transpose(amb_data.pa0_hi[*,3]),   COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa1_180_delta_plus,  transpose(amb_data.pa180_hi[*,0]), COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa2_180_delta_plus,  transpose(amb_data.pa180_hi[*,1]), COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa3_180_delta_plus,  transpose(amb_data.pa180_hi[*,2]), COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa4_180_delta_plus,  transpose(amb_data.pa180_hi[*,3]), COMPRESSION='GZIP', GZIP_LEVEL=6
		;'SRVY'
		endif else begin
			oamb -> WriteVar, /CREATE, pa1_0_vname,         transpose(amb_data.pa0[*,0]),      COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa1_180_vname,       transpose(amb_data.pa180[*,0]),    COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa1_0_delta_minus,   transpose(amb_data.pa0_lo[*,0]),   COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa1_180_delta_minus, transpose(amb_data.pa180_lo[*,0]), COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa1_0_delta_plus,    transpose(amb_data.pa0_hi[*,0]),   COMPRESSION='GZIP', GZIP_LEVEL=6
			oamb -> WriteVar, /CREATE, pa1_180_delta_plus,  transpose(amb_data.pa180_hi[*,0]), COMPRESSION='GZIP', GZIP_LEVEL=6
		endelse
	
	;No pitch-angle data
	;   - Create empty variables
	endif else begin
		;BRST
		if mode eq 'brst' then begin
			;Pitch angle
			oamb -> CreateVar, pa1_0_vname,         'FLOAT'
			oamb -> CreateVar, pa2_0_vname,         'FLOAT'
			oamb -> CreateVar, pa3_0_vname,         'FLOAT'
			oamb -> CreateVar, pa4_0_vname,         'FLOAT'
			oamb -> CreateVar, pa1_180_vname,       'FLOAT'
			oamb -> CreateVar, pa2_180_vname,       'FLOAT'
			oamb -> CreateVar, pa3_180_vname,       'FLOAT'
			oamb -> CreateVar, pa4_180_vname,       'FLOAT'
			
			;Delta Minus
			oamb -> CreateVar, pa1_0_delta_minus,   'FLOAT'
			oamb -> CreateVar, pa2_0_delta_minus,   'FLOAT'
			oamb -> CreateVar, pa3_0_delta_minus,   'FLOAT'
			oamb -> CreateVar, pa4_0_delta_minus,   'FLOAT'
			oamb -> CreateVar, pa1_180_delta_minus, 'FLOAT'
			oamb -> CreateVar, pa2_180_delta_minus, 'FLOAT'
			oamb -> CreateVar, pa3_180_delta_minus, 'FLOAT'
			oamb -> CreateVar, pa4_180_delta_minus, 'FLOAT'
			
			;Delta Plus
			oamb -> CreateVar, pa1_0_delta_plus,    'FLOAT'
			oamb -> CreateVar, pa2_0_delta_plus,    'FLOAT'
			oamb -> CreateVar, pa3_0_delta_plus,    'FLOAT'
			oamb -> CreateVar, pa4_0_delta_plus,    'FLOAT'
			oamb -> CreateVar, pa1_180_delta_plus,  'FLOAT'
			oamb -> CreateVar, pa2_180_delta_plus,  'FLOAT'
			oamb -> CreateVar, pa3_180_delta_plus,  'FLOAT'
			oamb -> CreateVar, pa4_180_delta_plus,  'FLOAT'
		;SRVY
		endif else begin
			oamb -> CreateVar, pa1_0_vname,         'FLOAT'
			oamb -> CreateVar, pa1_180_vname,       'FLOAT'
			oamb -> CreateVar, pa1_0_delta_minus,   'FLOAT'
			oamb -> CreateVar, pa1_180_delta_minus, 'FLOAT'
			oamb -> CreateVar, pa1_0_delta_plus,    'FLOAT'
			oamb -> CreateVar, pa1_180_delta_plus,  'FLOAT'
		endelse
	endelse
	
;------------------------------------------------------
; Variable Attributes                                 |
;------------------------------------------------------
	;Create the variable attributes
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'CATDESC'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'DELTA_PLUS_VAR'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'DELTA_MINUS_VAR'
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
	
	;TT2000_PA0
	oamb -> WriteVarAttr, t_0_vname, 'CATDESC',       'TT2000 time tags for EDU 0 degree pitch angle electron counts.'
	oamb -> WriteVarAttr, t_0_vname, 'FIELDNAM',      'Time'
	oamb -> WriteVarAttr, t_0_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oamb -> WriteVarAttr, t_0_vname, 'FORMAT',        'I16'
	oamb -> WriteVarAttr, t_0_vname, 'LABLAXIS',      'UT'
	oamb -> WriteVarAttr, t_0_vname, 'SI_CONVERSION', '1e-9>s'
	oamb -> WriteVarAttr, t_0_vname, 'TIME_BASE',     'J2000'
	oamb -> WriteVarAttr, t_0_vname, 'UNITS',         'UT'
	oamb -> WriteVarAttr, t_0_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015, 3, 1), /CDF_EPOCH
	oamb -> WriteVarAttr, t_0_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2015, 3, 1), /CDF_EPOCH
	oamb -> WriteVarAttr, t_0_vname, 'VAR_TYPE',      'support_data'
	
	;TT2000_PA180
	oamb -> WriteVarAttr, t_180_vname, 'CATDESC',       'TT2000 time tags for EDU 180 degree pitch angle electron counts.'
	oamb -> WriteVarAttr, t_180_vname, 'FIELDNAM',      'Time'
	oamb -> WriteVarAttr, t_180_vname, 'FILLVAL',       MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oamb -> WriteVarAttr, t_180_vname, 'FORMAT',        'I16'
	oamb -> WriteVarAttr, t_180_vname, 'LABLAXIS',      'UT'
	oamb -> WriteVarAttr, t_180_vname, 'SI_CONVERSION', '1e-9>s'
	oamb -> WriteVarAttr, t_180_vname, 'TIME_BASE',     'J2000'
	oamb -> WriteVarAttr, t_180_vname, 'UNITS',         'UT'
	oamb -> WriteVarAttr, t_180_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015, 3, 1), /CDF_EPOCH
	oamb -> WriteVarAttr, t_180_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2015, 3, 1), /CDF_EPOCH
	oamb -> WriteVarAttr, t_180_vname, 'VAR_TYPE',      'support_data'

	;EPOCH_TIMETAG
	oamb -> WriteVarAttr, t_tt_vname, 'CATDESC',       'TT2000 time tags for EDU support data.'
	oamb -> WriteVarAttr, t_tt_vname, 'FIELDNAM',      'Time'
	oamb -> WriteVarAttr, t_tt_vname, 'FILLVAL',       MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oamb -> WriteVarAttr, t_tt_vname, 'FORMAT',        'I16'
	oamb -> WriteVarAttr, t_tt_vname, 'LABLAXIS',      'UT'
	oamb -> WriteVarAttr, t_tt_vname, 'SI_CONVERSION', '1e-9>s'
	oamb -> WriteVarAttr, t_tt_vname, 'TIME_BASE',     'J2000'
	oamb -> WriteVarAttr, t_tt_vname, 'UNITS',         'UT'
	oamb -> WriteVarAttr, t_tt_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015, 3, 1), /CDF_EPOCH
	oamb -> WriteVarAttr, t_tt_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2015, 3, 1), /CDF_EPOCH
	oamb -> WriteVarAttr, t_tt_vname, 'VAR_TYPE',      'support_data'

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
	oamb -> WriteVarAttr, gdu_0_vname, 'DEPEND_0',       t_0_vname
	oamb -> WriteVarAttr, gdu_0_vname, 'FIELDNAM',      'GDU Identifier'
	oamb -> WriteVarAttr, gdu_0_vname, 'FILLVAL',        255
	oamb -> WriteVarAttr, gdu_0_vname, 'FORMAT',        'I1'
	oamb -> WriteVarAttr, gdu_0_vname, 'VALIDMIN',      1B
	oamb -> WriteVarAttr, gdu_0_vname, 'VALIDMAX',      2B
	oamb -> WriteVarAttr, gdu_0_vname, 'VAR_TYPE',      'meta_data'

	;GDU_180
	oamb -> WriteVarAttr, gdu_180_vname, 'CATDESC',       'Sorts 180 degree counts by GDU'
	oamb -> WriteVarAttr, gdu_180_vname, 'DEPEND_0',       t_180_vname
	oamb -> WriteVarAttr, gdu_180_vname, 'FIELDNAM',      'GDU Identifier'
	oamb -> WriteVarAttr, gdu_180_vname, 'FILLVAL',        255
	oamb -> WriteVarAttr, gdu_180_vname, 'FORMAT',        'I1'
	oamb -> WriteVarAttr, gdu_180_vname, 'VALIDMIN',      1B
	oamb -> WriteVarAttr, gdu_180_vname, 'VALIDMAX',      2B
	oamb -> WriteVarAttr, gdu_180_vname, 'VAR_TYPE',      'meta_data'

	;COUNTS1_0
	oamb -> WriteVarAttr, counts1_0_vname, 'CATDESC',      'Counts for electrons with pitch angles given by pa1_0'
	oamb -> WriteVarAttr, counts1_0_vname, 'DEPEND_0',      t_0_vname
	oamb -> WriteVarAttr, counts1_0_vname, 'DISPLAY_TYPE', 'time_series'
	oamb -> WriteVarAttr, counts1_0_vname, 'FIELDNAM',     '0 degree electron counts'
	oamb -> WriteVarAttr, counts1_0_vname, 'FILLVAL',      65535US
	oamb -> WriteVarAttr, counts1_0_vname, 'FORMAT',       'I5'
	oamb -> WriteVarAttr, counts1_0_vname, 'LABLAXIS',     'counts'
	oamb -> WriteVarAttr, counts1_0_vname, 'SCALETYP',     'log'
	oamb -> WriteVarAttr, counts1_0_vname, 'UNITS',        'counts'
	oamb -> WriteVarAttr, counts1_0_vname, 'VALIDMIN',     0US
	oamb -> WriteVarAttr, counts1_0_vname, 'VALIDMAX',     65534US
	oamb -> WriteVarAttr, counts1_0_vname, 'VAR_TYPE',     'data'

	;COUNTS1_180
	oamb -> WriteVarAttr, counts1_180_vname, 'CATDESC',      'Counts for electrons with pitch angles given by pa1_180'
	oamb -> WriteVarAttr, counts1_180_vname, 'DEPEND_0',      t_180_vname
	oamb -> WriteVarAttr, counts1_180_vname, 'DISPLAY_TYPE', 'time_series'
	oamb -> WriteVarAttr, counts1_180_vname, 'FIELDNAM',     '180 degree electron counts'
	oamb -> WriteVarAttr, counts1_180_vname, 'FILLVAL',      65535US
	oamb -> WriteVarAttr, counts1_180_vname, 'FORMAT',       'I5'
	oamb -> WriteVarAttr, counts1_180_vname, 'LABLAXIS',     'counts'
	oamb -> WriteVarAttr, counts1_180_vname, 'SCALETYP',     'log'
	oamb -> WriteVarAttr, counts1_180_vname, 'UNITS',        'counts'
	oamb -> WriteVarAttr, counts1_180_vname, 'VALIDMIN',     0US
	oamb -> WriteVarAttr, counts1_180_vname, 'VALIDMAX',     65534US
	oamb -> WriteVarAttr, counts1_180_vname, 'VAR_TYPE',     'data'

	;PA1_PA0
	oamb -> WriteVarAttr, pa1_0_vname, 'CATDESC',         'Pitch angle of counts1_0 particles.'
	oamb -> WriteVarAttr, pa1_0_vname, 'DEPEND_0',        t_0_vname
	oamb -> WriteVarAttr, pa1_0_vname, 'DELTA_MINUS_VAR', pa1_0_delta_minus
	oamb -> WriteVarAttr, pa1_0_vname, 'DELTA_PLUS_VAR',  pa1_0_delta_plus
	oamb -> WriteVarAttr, pa1_0_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, pa1_0_vname, 'FIELDNAM',        'Pitch Angle'
	oamb -> WriteVarAttr, pa1_0_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, pa1_0_vname, 'FORMAT',          'F7.2'
	oamb -> WriteVarAttr, pa1_0_vname, 'LABLAXIS',        'PA'
	oamb -> WriteVarAttr, pa1_0_vname, 'SCALETYP',        'linear'
	oamb -> WriteVarAttr, pa1_0_vname, 'UNITS',           'degrees'
	oamb -> WriteVarAttr, pa1_0_vname, 'VALIDMIN',        0
	oamb -> WriteVarAttr, pa1_0_vname, 'VALIDMAX',        180.0
	oamb -> WriteVarAttr, pa1_0_vname, 'VAR_TYPE',        'data'

	;PA1_180
	oamb -> WriteVarAttr, pa1_180_vname, 'CATDESC',         'Pitch angle of counts1_180 particles.'
	oamb -> WriteVarAttr, pa1_180_vname, 'DEPEND_0',        t_180_vname
	oamb -> WriteVarAttr, pa1_180_vname, 'DELTA_MINUS_VAR', pa1_180_delta_minus
	oamb -> WriteVarAttr, pa1_180_vname, 'DELTA_PLUS_VAR',  pa1_180_delta_plus
	oamb -> WriteVarAttr, pa1_180_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, pa1_180_vname, 'FIELDNAM',        'Pitch Angle'
	oamb -> WriteVarAttr, pa1_180_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, pa1_180_vname, 'FORMAT',          'F7.2'
	oamb -> WriteVarAttr, pa1_180_vname, 'LABLAXIS',        'PA'
	oamb -> WriteVarAttr, pa1_180_vname, 'SCALETYP',        'linear'
	oamb -> WriteVarAttr, pa1_180_vname, 'UNITS',           'degrees'
	oamb -> WriteVarAttr, pa1_180_vname, 'VALIDMIN',        0
	oamb -> WriteVarAttr, pa1_180_vname, 'VALIDMAX',        180.0
	oamb -> WriteVarAttr, pa1_180_vname, 'VAR_TYPE',        'data'

	;PA1_0_DELTA_MINUS
	oamb -> WriteVarAttr, pa1_0_delta_minus, 'CATDESC',  'Lower bound for pa1_0.'
	oamb -> WriteVarAttr, pa1_0_delta_minus, 'FIELDNAM', 'dPA'
	oamb -> WriteVarAttr, pa1_0_delta_minus, 'FILLVAL',  -1e31
	oamb -> WriteVarAttr, pa1_0_delta_minus, 'FORMAT',   'F7.2'
	oamb -> WriteVarAttr, pa1_0_delta_minus, 'UNITS',    'degrees'
	oamb -> WriteVarAttr, pa1_0_delta_minus, 'VALIDMIN', -180.0
	oamb -> WriteVarAttr, pa1_0_delta_minus, 'VALIDMAX', 180.0
	oamb -> WriteVarAttr, pa1_0_delta_minus, 'VAR_TYPE', 'support_data'

	;PA1_0_DELTA_PLUS
	oamb -> WriteVarAttr, pa1_0_delta_plus, 'CATDESC',  'Upper bound of pitch angle.'
	oamb -> WriteVarAttr, pa1_0_delta_plus, 'FIELDNAM', 'dPA'
	oamb -> WriteVarAttr, pa1_0_delta_plus, 'FILLVAL',  -1e31
	oamb -> WriteVarAttr, pa1_0_delta_plus, 'FORMAT',   'F7.2'
	oamb -> WriteVarAttr, pa1_0_delta_plus, 'UNITS',    'degrees'
	oamb -> WriteVarAttr, pa1_0_delta_plus, 'VALIDMIN', -180.0
	oamb -> WriteVarAttr, pa1_0_delta_plus, 'VALIDMAX', 180.0
	oamb -> WriteVarAttr, pa1_0_delta_plus, 'VAR_TYPE', 'support_data'

	;PA1_180_DELTA_MINUS
	oamb -> WriteVarAttr, pa1_180_delta_minus, 'CATDESC',  'Lower bound of pa1_180.'
	oamb -> WriteVarAttr, pa1_180_delta_minus, 'FIELDNAM', 'dPA'
	oamb -> WriteVarAttr, pa1_180_delta_minus, 'FILLVAL',  -1e31
	oamb -> WriteVarAttr, pa1_180_delta_minus, 'FORMAT',   'F7.2'
	oamb -> WriteVarAttr, pa1_180_delta_minus, 'UNITS',    'degrees'
	oamb -> WriteVarAttr, pa1_180_delta_minus, 'VALIDMIN', -180.0
	oamb -> WriteVarAttr, pa1_180_delta_minus, 'VALIDMAX', 180.0
	oamb -> WriteVarAttr, pa1_180_delta_minus, 'VAR_TYPE', 'support_data'

	;PA1_180_DELTA_PLUS
	oamb -> WriteVarAttr, pa1_180_delta_plus, 'CATDESC',  'Upper bound of pa1_180.'
	oamb -> WriteVarAttr, pa1_180_delta_plus, 'FIELDNAM', 'dPA'
	oamb -> WriteVarAttr, pa1_180_delta_plus, 'FILLVAL',  -1e31
	oamb -> WriteVarAttr, pa1_180_delta_plus, 'FORMAT',   'F7.2'
	oamb -> WriteVarAttr, pa1_180_delta_plus, 'UNITS',    'degrees'
	oamb -> WriteVarAttr, pa1_180_delta_plus, 'VALIDMIN', -180.0
	oamb -> WriteVarAttr, pa1_180_delta_plus, 'VALIDMAX', 180.0
	oamb -> WriteVarAttr, pa1_180_delta_plus, 'VAR_TYPE', 'support_data'

	;BURST DATA
	if mode eq 'brst' then begin
		;COUNTS2_PA0
		oamb -> WriteVarAttr, counts2_0_vname, 'CATDESC',      'Counts for electrons with pitch angles given by pa2_0'
		oamb -> WriteVarAttr, counts2_0_vname, 'DEPEND_0',      t_0_vname
		oamb -> WriteVarAttr, counts2_0_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts2_0_vname, 'FIELDNAM',     'Electron Counts PA0'
		oamb -> WriteVarAttr, counts2_0_vname, 'FILLVAL',      65535US
		oamb -> WriteVarAttr, counts2_0_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts2_0_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts2_0_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts2_0_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts2_0_vname, 'VALIDMIN',     0US
		oamb -> WriteVarAttr, counts2_0_vname, 'VALIDMAX',     65534US
		oamb -> WriteVarAttr, counts2_0_vname, 'VAR_TYPE',     'data'

		;COUNTS3_PA0
		oamb -> WriteVarAttr, counts3_0_vname, 'CATDESC',      'Counts for electrons with pitch angles given by pa3_0'
		oamb -> WriteVarAttr, counts3_0_vname, 'DEPEND_0',      t_0_vname
		oamb -> WriteVarAttr, counts3_0_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts3_0_vname, 'FIELDNAM',     'Electron Counts PA0'
		oamb -> WriteVarAttr, counts3_0_vname, 'FILLVAL',      65535US
		oamb -> WriteVarAttr, counts3_0_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts3_0_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts3_0_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts3_0_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts3_0_vname, 'VALIDMIN',     0US
		oamb -> WriteVarAttr, counts3_0_vname, 'VALIDMAX',     65534US
		oamb -> WriteVarAttr, counts3_0_vname, 'VAR_TYPE',     'data'

		;COUNTS4_PA0
		oamb -> WriteVarAttr, counts4_0_vname, 'CATDESC',      'Counts for electrons with pitch angles given by pa4_0'
		oamb -> WriteVarAttr, counts4_0_vname, 'DEPEND_0',      t_0_vname
		oamb -> WriteVarAttr, counts4_0_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts4_0_vname, 'FIELDNAM',     'Electron Counts PA0'
		oamb -> WriteVarAttr, counts4_0_vname, 'FILLVAL',      65535US
		oamb -> WriteVarAttr, counts4_0_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts4_0_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts4_0_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts4_0_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts4_0_vname, 'VALIDMIN',     0US
		oamb -> WriteVarAttr, counts4_0_vname, 'VALIDMAX',     65534US
		oamb -> WriteVarAttr, counts4_0_vname, 'VAR_TYPE',     'data'

		;COUNTS2_PA180
		oamb -> WriteVarAttr, counts2_180_vname, 'CATDESC',      'Counts for electrons with pitch angles given by pa2_180'
		oamb -> WriteVarAttr, counts2_180_vname, 'DEPEND_0',      t_180_vname
		oamb -> WriteVarAttr, counts2_180_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts2_180_vname, 'FIELDNAM',     'Electron Counts PA180'
		oamb -> WriteVarAttr, counts2_180_vname, 'FILLVAL',      65535US
		oamb -> WriteVarAttr, counts2_180_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts2_180_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts2_180_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts2_180_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts2_180_vname, 'VALIDMIN',     0US
		oamb -> WriteVarAttr, counts2_180_vname, 'VALIDMAX',     65534US
		oamb -> WriteVarAttr, counts2_180_vname, 'VAR_TYPE',     'data'

		;COUNTS3_PA180
		oamb -> WriteVarAttr, counts3_180_vname, 'CATDESC',      'Counts for electrons with pitch angles given by pa3_180'
		oamb -> WriteVarAttr, counts3_180_vname, 'DEPEND_0',      t_180_vname
		oamb -> WriteVarAttr, counts3_180_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts3_180_vname, 'FIELDNAM',     'Electron Counts PA180'
		oamb -> WriteVarAttr, counts3_180_vname, 'FILLVAL',      65535US
		oamb -> WriteVarAttr, counts3_180_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts3_180_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts3_180_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts3_180_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts3_180_vname, 'VALIDMIN',     0US
		oamb -> WriteVarAttr, counts3_180_vname, 'VALIDMAX',     65534US
		oamb -> WriteVarAttr, counts3_180_vname, 'VAR_TYPE',     'data'

		;COUNTS4_PA180
		oamb -> WriteVarAttr, counts4_180_vname, 'CATDESC',      'Counts for electrons with pitch angles given by pa4_180'
		oamb -> WriteVarAttr, counts4_180_vname, 'DEPEND_0',      t_180_vname
		oamb -> WriteVarAttr, counts4_180_vname, 'DISPLAY_TYPE', 'time_series'
		oamb -> WriteVarAttr, counts4_180_vname, 'FIELDNAM',     'Electron Counts PA180'
		oamb -> WriteVarAttr, counts4_180_vname, 'FILLVAL',      65535US
		oamb -> WriteVarAttr, counts4_180_vname, 'FORMAT',       'I5'
		oamb -> WriteVarAttr, counts4_180_vname, 'LABLAXIS',     'counts'
		oamb -> WriteVarAttr, counts4_180_vname, 'SCALETYP',     'log'
		oamb -> WriteVarAttr, counts4_180_vname, 'UNITS',        'counts'
		oamb -> WriteVarAttr, counts4_180_vname, 'VALIDMIN',     0US
		oamb -> WriteVarAttr, counts4_180_vname, 'VALIDMAX',     65534US
		oamb -> WriteVarAttr, counts4_180_vname, 'VAR_TYPE',     'data'

		;PA2_0
		oamb -> WriteVarAttr, pa2_0_vname, 'CATDESC',         'Pitch angle of counts2_0 particles.'
		oamb -> WriteVarAttr, pa2_0_vname, 'DEPEND_0',        t_0_vname
		oamb -> WriteVarAttr, pa2_0_vname, 'DELTA_MINUS_VAR', pa2_0_delta_minus
		oamb -> WriteVarAttr, pa2_0_vname, 'DELTA_PLUS_VAR',  pa2_0_delta_plus
		oamb -> WriteVarAttr, pa2_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, pa2_0_vname, 'FIELDNAM',        'Pitch Angle'
		oamb -> WriteVarAttr, pa2_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, pa2_0_vname, 'FORMAT',          'F7.2'
		oamb -> WriteVarAttr, pa2_0_vname, 'LABLAXIS',        'PA'
		oamb -> WriteVarAttr, pa2_0_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, pa2_0_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, pa2_0_vname, 'VALIDMIN',        0
		oamb -> WriteVarAttr, pa2_0_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, pa2_0_vname, 'VAR_TYPE',        'data'

		;PA3_0
		oamb -> WriteVarAttr, pa3_0_vname, 'CATDESC',         'Pitch angle of counts3_0 particles.'
		oamb -> WriteVarAttr, pa3_0_vname, 'DEPEND_0',        t_0_vname
		oamb -> WriteVarAttr, pa3_0_vname, 'DELTA_MINUS_VAR', pa3_0_delta_minus
		oamb -> WriteVarAttr, pa3_0_vname, 'DELTA_PLUS_VAR',  pa3_0_delta_plus
		oamb -> WriteVarAttr, pa3_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, pa3_0_vname, 'FIELDNAM',        'Pitch Angle'
		oamb -> WriteVarAttr, pa3_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, pa3_0_vname, 'FORMAT',          'F7.2'
		oamb -> WriteVarAttr, pa3_0_vname, 'LABLAXIS',        'PA'
		oamb -> WriteVarAttr, pa3_0_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, pa3_0_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, pa3_0_vname, 'VALIDMIN',        0
		oamb -> WriteVarAttr, pa3_0_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, pa3_0_vname, 'VAR_TYPE',        'data'

		;PA4_0
		oamb -> WriteVarAttr, pa4_0_vname, 'CATDESC',         'Pitch angle of counts4_0 particles.'
		oamb -> WriteVarAttr, pa4_0_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, pa4_0_vname, 'DELTA_MINUS_VAR', pa4_0_delta_minus
		oamb -> WriteVarAttr, pa4_0_vname, 'DELTA_PLUS_VAR',  pa4_0_delta_plus
		oamb -> WriteVarAttr, pa4_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, pa4_0_vname, 'FIELDNAM',        'Pitch Angle'
		oamb -> WriteVarAttr, pa4_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, pa4_0_vname, 'FORMAT',          'F7.2'
		oamb -> WriteVarAttr, pa4_0_vname, 'LABLAXIS',        'PA'
		oamb -> WriteVarAttr, pa4_0_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, pa4_0_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, pa4_0_vname, 'VALIDMIN',        0
		oamb -> WriteVarAttr, pa4_0_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, pa4_0_vname, 'VAR_TYPE',        'data'

		;PA2_180
		oamb -> WriteVarAttr, pa2_180_vname, 'CATDESC',         'Pitch angle of counts2_180 particles.'
		oamb -> WriteVarAttr, pa2_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, pa2_180_vname, 'DELTA_MINUS_VAR', pa2_180_delta_minus
		oamb -> WriteVarAttr, pa2_180_vname, 'DELTA_PLUS_VAR',  pa2_180_delta_plus
		oamb -> WriteVarAttr, pa2_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, pa2_180_vname, 'FIELDNAM',        'Pitch Angle'
		oamb -> WriteVarAttr, pa2_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, pa2_180_vname, 'FORMAT',          'F7.2'
		oamb -> WriteVarAttr, pa2_180_vname, 'LABLAXIS',        'PA'
		oamb -> WriteVarAttr, pa2_180_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, pa2_180_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, pa2_180_vname, 'VALIDMIN',        0
		oamb -> WriteVarAttr, pa2_180_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, pa2_180_vname, 'VAR_TYPE',        'data'

		;PA3_180
		oamb -> WriteVarAttr, pa3_180_vname, 'CATDESC',         'Pitch angle of counts3_180 particles.'
		oamb -> WriteVarAttr, pa3_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, pa3_180_vname, 'DELTA_MINUS_VAR', pa3_180_delta_minus
		oamb -> WriteVarAttr, pa3_180_vname, 'DELTA_PLUS_VAR',  pa3_180_delta_plus
		oamb -> WriteVarAttr, pa3_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, pa3_180_vname, 'FIELDNAM',        'Pitch Angle'
		oamb -> WriteVarAttr, pa3_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, pa3_180_vname, 'FORMAT',          'F7.2'
		oamb -> WriteVarAttr, pa3_180_vname, 'LABLAXIS',        'PA'
		oamb -> WriteVarAttr, pa3_180_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, pa3_180_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, pa3_180_vname, 'VALIDMIN',        0
		oamb -> WriteVarAttr, pa3_180_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, pa3_180_vname, 'VAR_TYPE',        'data'

		;PA4_180
		oamb -> WriteVarAttr, pa4_180_vname, 'CATDESC',         'Pitch angle of counts4_180 particles.'
		oamb -> WriteVarAttr, pa4_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, pa4_180_vname, 'DELTA_MINUS_VAR', pa4_180_delta_minus
		oamb -> WriteVarAttr, pa4_180_vname, 'DELTA_PLUS_VAR',  pa4_180_delta_plus
		oamb -> WriteVarAttr, pa4_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, pa4_180_vname, 'FIELDNAM',        'Pitch Angle'
		oamb -> WriteVarAttr, pa4_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, pa4_180_vname, 'FORMAT',          'F7.2'
		oamb -> WriteVarAttr, pa4_180_vname, 'LABLAXIS',        'PA'
		oamb -> WriteVarAttr, pa4_180_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, pa4_180_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, pa4_180_vname, 'VALIDMIN',        0
		oamb -> WriteVarAttr, pa4_180_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, pa4_180_vname, 'VAR_TYPE',        'data'

		;PA2_0_DELTA_MINUS
		oamb -> WriteVarAttr, pa2_0_delta_minus, 'CATDESC',  'Lower bound of pa2_0.'
		oamb -> WriteVarAttr, pa2_0_delta_minus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa2_0_delta_minus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa2_0_delta_minus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa2_0_delta_minus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa2_0_delta_minus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa2_0_delta_minus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa2_0_delta_minus, 'VAR_TYPE', 'support_data'

		;PA3_0_DELTA_MINUS
		oamb -> WriteVarAttr, pa3_0_delta_minus, 'CATDESC',  'Lower bound of pa3_0.'
		oamb -> WriteVarAttr, pa3_0_delta_minus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa3_0_delta_minus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa3_0_delta_minus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa3_0_delta_minus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa3_0_delta_minus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa3_0_delta_minus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa3_0_delta_minus, 'VAR_TYPE', 'support_data'

		;PA4_0_DELTA_MINUS
		oamb -> WriteVarAttr, pa4_0_delta_minus, 'CATDESC',  'Lower bound of pa4_0.'
		oamb -> WriteVarAttr, pa4_0_delta_minus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa4_0_delta_minus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa4_0_delta_minus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa4_0_delta_minus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa4_0_delta_minus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa4_0_delta_minus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa4_0_delta_minus, 'VAR_TYPE', 'support_data'

		;PA2_0_DELTA_PLUS
		oamb -> WriteVarAttr, pa2_0_delta_plus, 'CATDESC',  'Upper bound of pa2_0.'
		oamb -> WriteVarAttr, pa2_0_delta_plus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa2_0_delta_plus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa2_0_delta_plus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa2_0_delta_plus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa2_0_delta_plus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa2_0_delta_plus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa2_0_delta_plus, 'VAR_TYPE', 'support_data'

		;PA3_0_DELTA_PLUS
		oamb -> WriteVarAttr, pa3_0_delta_plus, 'CATDESC',  'Upper bound of pa3_0.'
		oamb -> WriteVarAttr, pa3_0_delta_plus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa3_0_delta_plus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa3_0_delta_plus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa3_0_delta_plus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa3_0_delta_plus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa3_0_delta_plus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa3_0_delta_plus, 'VAR_TYPE', 'support_data'

		;PA4_0_DELTA_PLUS
		oamb -> WriteVarAttr, pa4_0_delta_plus, 'CATDESC',  'Upper bound of pa4_0.'
		oamb -> WriteVarAttr, pa4_0_delta_plus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa4_0_delta_plus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa4_0_delta_plus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa4_0_delta_plus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa4_0_delta_plus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa4_0_delta_plus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa4_0_delta_plus, 'VAR_TYPE', 'support_data'

		;PA2_180_DELTA_MINUS
		oamb -> WriteVarAttr, pa2_180_delta_minus, 'CATDESC',  'Lower bound of pa2_180.'
		oamb -> WriteVarAttr, pa2_180_delta_minus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa2_180_delta_minus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa2_180_delta_minus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa2_180_delta_minus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa2_180_delta_minus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa2_180_delta_minus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa2_180_delta_minus, 'VAR_TYPE', 'support_data'

		;PA3_180_DELTA_MINUS
		oamb -> WriteVarAttr, pa3_180_delta_minus, 'CATDESC',  'Lower bound of pa3_180.'
		oamb -> WriteVarAttr, pa3_180_delta_minus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa3_180_delta_minus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa3_180_delta_minus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa3_180_delta_minus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa3_180_delta_minus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa3_180_delta_minus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa3_180_delta_minus, 'VAR_TYPE', 'support_data'

		;PA4_180_DELTA_MINUS
		oamb -> WriteVarAttr, pa4_180_delta_minus, 'CATDESC',  'Lower bound of pa4_180.'
		oamb -> WriteVarAttr, pa4_180_delta_minus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa4_180_delta_minus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa4_180_delta_minus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa4_180_delta_minus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa4_180_delta_minus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa4_180_delta_minus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa4_180_delta_minus, 'VAR_TYPE', 'support_data'

		;PA2_180_DELTA_PLUS
		oamb -> WriteVarAttr, pa2_180_delta_plus, 'CATDESC',  'Upper bound of pa2_180.'
		oamb -> WriteVarAttr, pa2_180_delta_plus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa2_180_delta_plus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa2_180_delta_plus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa2_180_delta_plus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa2_180_delta_plus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa2_180_delta_plus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa2_180_delta_plus, 'VAR_TYPE', 'support_data'

		;PA3_180_DELTA_PLUS
		oamb -> WriteVarAttr, pa3_180_delta_plus, 'CATDESC',  'Upper bound of pa3_180.'
		oamb -> WriteVarAttr, pa3_180_delta_plus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa3_180_delta_plus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa3_180_delta_plus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa3_180_delta_plus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa3_180_delta_plus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa3_180_delta_plus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa3_180_delta_plus, 'VAR_TYPE', 'support_data'

		;PA4_180_DELTA_PLUS
		oamb -> WriteVarAttr, pa4_180_delta_plus, 'CATDESC',  'Upper bound of pa4_180.'
		oamb -> WriteVarAttr, pa4_180_delta_plus, 'FIELDNAM', 'dPA'
		oamb -> WriteVarAttr, pa4_180_delta_plus, 'FILLVAL',  -1e31
		oamb -> WriteVarAttr, pa4_180_delta_plus, 'FORMAT',   'F7.2'
		oamb -> WriteVarAttr, pa4_180_delta_plus, 'UNITS',    'degrees'
		oamb -> WriteVarAttr, pa4_180_delta_plus, 'VALIDMIN', -180.0
		oamb -> WriteVarAttr, pa4_180_delta_plus, 'VALIDMAX', 180.0
		oamb -> WriteVarAttr, pa4_180_delta_plus, 'VAR_TYPE', 'support_data'
	endif

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, oamb
	return, amb_file
end