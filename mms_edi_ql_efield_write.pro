;
; Name
;   mms_edi_l2_q0_write
;
; Purpose
;   Create a MATLAB save file of inputs needed for Bestarg.
;
; Calling Sequence
;   EDI_QL_FILE = mms_edi_ql_efield_write(EDI_QL)
;     Write EDI quick-look data constained in the structure EDI_QL
;     and created by mms_edi_create_ql_efield.m to a CDF file named
;     EDI_QL_FILE.
;
; Parameters
;   EDI_QL:         in, required, type=string
;
; Returns
;   EDI_QL_FILE     out, required, type=string
;
; MATLAB release(s) MATLAB 7.14.0.739 (R2012a)
; Required Products None
;
; History:
;   2015-09-10      Written by Matthew Argall
;
function mms_edi_ql_efield_write, meta, time, tri, tof
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(oedi) then obj_destroy, oedi
		if n_elements(amb_file) gt 0 && file_test(amb_file) then file_delete, amb_file
		MrPrintF, 'LogErr'
		return, ''
	endif


;------------------------------------;
; Create CDF File                    ;
;------------------------------------;
	; Extract for ease of use
	sc      = meta.sc
	instr   = meta.instr
	mode    = meta.mode
	level   = 'ql'
	optdesc = meta.optdesc
	tstart  = meta.tstart

	;
	; Check sizes
	;
	if ~isa(tof.v_ExB_dmpa,  'FLOAT') then message, 'tri.v_ExB_dmpa must be FLOAT.'
	if ~isa(tof.E_dmpa,      'FLOAT') then message, 'tri.E_dmpa must be FLOAT.'
	if ~isa(tri.v_ExB_dmpa,  'FLOAT') then message, 'tri.v_ExB_dmpa must be FLOAT.'
	if ~isa(tri.E_dmpa,      'FLOAT') then message, 'tri.E_dmpa must be FLOAT.'
	                                  
	; Describe the modifications to each version
	mods    = [  'v0.0.0 -- First version.' $
	          ]
	version = stregex( mods[-1], '^v([0-9]+\.[0-9]+\.[0-9]+)', /SUBEXP, /EXTRACT )
	version = version[1]

	; Create the output filename
	edi_file = mms_construct_filename( sc, instr, mode, level,      $
	                                   DIRECTORY = meta.directory, $
	                                   OPTDESC   = optdesc,        $
	                                   TSTART    = tstart,         $
	                                   VERSION   = version )

	;Open the CDF file
	oedi = MrCDF_File(edi_file, /CREATE, /CLOBBER)
	if obj_valid(oedi) eq 0 then return, ''

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
	logical_file_id = cgRootName(edi_file)
	source_name = 'MMS' + strmid(sc, 3) + '>MMS Satellite Number ' + strmid(sc, 3)
	MrTimeParser, MrTimeStamp(/UTC), '%Y-%M-%dT%H:%m:%S', '%Y%M%d', gen_date

	;   - Instrument Type (1+)
	;           Electric Fields (space)
	;           Magnetic Fields (space)
	;           Particles (space)
	;           Plasma and Solar Wind
	;           Spacecraft Potential Control
	oedi -> WriteGlobalAttr, /CREATE, 'Data_Type',                  data_type
	oedi -> WriteGlobalAttr, /CREATE, 'Data_version',               version
	oedi -> WriteGlobalAttr, /CREATE, 'Descriptor',                 'EDI'
	oedi -> WriteGlobalAttr, /CREATE, 'Discipline',                 'Space Physics>Magnetospheric Science'
	oedi -> WriteGlobalAttr, /CREATE, 'File_naming_convention',     'source_descriptor_datatype_yyyyMMdd'
	oedi -> WriteGlobalAttr, /CREATE, 'Generation_date',            gen_date
	oedi -> WriteGlobalAttr, /CREATE, 'Instrument_type',            'Electric Fields (space)'
	oedi -> WriteGlobalAttr, /CREATE, 'Logical_file_id',            logical_file_id
	oedi -> WriteGlobalAttr, /CREATE, 'Logical_source',             logical_source
	oedi -> WriteGlobalAttr, /CREATE, 'Logical_source_description', 'Quick-look EDI electric field'
	oedi -> WriteGlobalAttr, /CREATE, 'Mission_group',              'MMS'
	oedi -> WriteGlobalAttr, /CREATE, 'PI_affiliation',             'SWRI, UNH'
	oedi -> WriteGlobalAttr, /CREATE, 'PI_name',                    'J. Burch, R. Torbert'
	oedi -> WriteGlobalAttr, /CREATE, 'Project',                    'STP>Solar Terrestrial Physics'
	oedi -> WriteGlobalAttr, /CREATE, 'Source_name',                source_name
	oedi -> WriteGlobalAttr, /CREATE, 'TEXT',                       'EDI electric field data. Instrument papers ' + $
	                                                                'for EDI can be found at: ' + $
	                                                                'http://link.springer.com/article/10.1007%2Fs11214-015-0182-7'
	oedi -> WriteGlobalAttr, /CREATE, 'HTTP_LINK',                  ['http://mms-fields.unh.edu/', $
	                                                                 'http://mms.gsfc.nasa.gov/index.html']
	oedi -> WriteGlobalAttr, /CREATE, 'LINK_TEXT',                  ['UNH FIELDS Home Page', $
	                                                                 'NASA MMS Home']
	oedi -> WriteGlobalAttr, /CREATE, 'MODS',                       mods
	oedi -> WriteGlobalAttr, /CREATE, 'Acknowledgements',           ' '
	oedi -> WriteGlobalAttr, /CREATE, 'Generated_by',               'University of New Hampshire'
	oedi -> WriteGlobalAttr, /CREATE, 'Parents',                    meta.parents
	oedi -> WriteGlobalAttr, /CREATE, 'Skeleton_version',           ' '
	oedi -> WriteGlobalAttr, /CREATE, 'Rules_of_use',               ' '
	oedi -> WriteGlobalAttr, /CREATE, 'Time_resolution',            ' '

;------------------------------------------------------
; Variables                                           |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName_optionalDescriptor
	t_vname     = 'Epoch'
	v_tof_vname = mms_construct_varname(sc, instr, 'v_ExB', 'tof')
	E_tof_vname = mms_construct_varname(sc, instr, 'E',     'tof')
	v_tri_vname = mms_construct_varname(sc, instr, 'v_ExB', 'tri')
	E_tri_vname = mms_construct_varname(sc, instr, 'E',     'tri')

	;Write variable data to file
	oedi -> WriteVar, /CREATE, t_vname,     transpose(time), CDF_TYPE='CDF_TIME_TT2000'
	oedi -> WriteVar, /CREATE, v_tof_vname, tof.v_ExB_dmpa,  COMPRESSION='GZIP', GZIP_LEVEL=6
	oedi -> WriteVar, /CREATE, E_tof_vname, tof.E_dmpa,      COMPRESSION='GZIP', GZIP_LEVEL=6
	oedi -> WriteVar, /CREATE, v_tri_vname, tri.v_ExB_dmpa,  COMPRESSION='GZIP', GZIP_LEVEL=6
	oedi -> WriteVar, /CREATE, E_tri_vname, tri.E_dmpa,      COMPRESSION='GZIP', GZIP_LEVEL=6

;------------------------------------------------------
; Variable Attributes                                 |
;------------------------------------------------------
	;Create the variable attributes
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'CATDESC'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'DELTA_PLUS_VAR'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'DELTA_MINUS_VAR'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_0'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'DISPLAY_TYPE'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'FIELDNAM'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'FILLVAL'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'FORMAT'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'LABLAXIS'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'SCALETYP'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'SI_CONVERSION'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'TIME_BASE'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'UNITS'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMIN'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMAX'
	oedi -> CreateAttr, /VARIABLE_SCOPE, 'VAR_TYPE'
	
	;EPOCH
	oedi -> WriteVarAttr, t_vname, 'CATDESC',       'TT2000 time tags velocity and electric field data.'
	oedi -> WriteVarAttr, t_vname, 'FIELDNAM',      'Time'
	oedi -> WriteVarAttr, t_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oedi -> WriteVarAttr, t_vname, 'FORMAT',        'I16'
	oedi -> WriteVarAttr, t_vname, 'LABLAXIS',      'UT'
	oedi -> WriteVarAttr, t_vname, 'SI_CONVERSION', '1e-9>s'
	oedi -> WriteVarAttr, t_vname, 'TIME_BASE',     'J2000'
	oedi -> WriteVarAttr, t_vname, 'UNITS',         'UT'
	oedi -> WriteVarAttr, t_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015, 3, 1), /CDF_EPOCH
	oedi -> WriteVarAttr, t_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2015, 3, 1), /CDF_EPOCH
	oedi -> WriteVarAttr, t_vname, 'VAR_TYPE',      'support_data'

	;V_EXB_TOF
	oedi -> WriteVarAttr, v_tof_vname, 'CATDESC',       'ExB Drift velocity from the Time-of-Flight (ToF) method.'
	oedi -> WriteVarAttr, v_tof_vname, 'DEPEND_0',       t_vname
	oedi -> WriteVarAttr, v_tof_vname, 'DISPLAY_TYPE',  'time_series'
	oedi -> WriteVarAttr, v_tof_vname, 'FIELDNAM',      'ExB Drift Velocity'
	oedi -> WriteVarAttr, v_tof_vname, 'FILLVAL',        -1e31
	oedi -> WriteVarAttr, v_tof_vname, 'FORMAT',        'F12.4'
	oedi -> WriteVarAttr, v_tof_vname, 'LABLAXIS',      'Velocity'
	oedi -> WriteVarAttr, v_tof_vname, 'SCALETYP',      'linear'
	oedi -> WriteVarAttr, v_tof_vname, 'SI_CONVERSION', '1e3>m/s'
	oedi -> WriteVarAttr, v_tof_vname, 'UNITS',         'km/s'
	oedi -> WriteVarAttr, v_tof_vname, 'VALIDMIN',      -3e8
	oedi -> WriteVarAttr, v_tof_vname, 'VALIDMAX',      3e8
	oedi -> WriteVarAttr, v_tof_vname, 'VAR_TYPE',      'data'

	;E_TOF
	oedi -> WriteVarAttr, E_tof_vname, 'CATDESC',       'Electric field from the Time-of-Flight (ToF) method.'
	oedi -> WriteVarAttr, E_tof_vname, 'DEPEND_0',       t_vname
	oedi -> WriteVarAttr, E_tof_vname, 'DISPLAY_TYPE',  'time_series'
	oedi -> WriteVarAttr, E_tof_vname, 'FIELDNAM',      'Electric Field'
	oedi -> WriteVarAttr, E_tof_vname, 'FILLVAL',        -1e31
	oedi -> WriteVarAttr, E_tof_vname, 'FORMAT',        'F12.4'
	oedi -> WriteVarAttr, E_tof_vname, 'LABLAXIS',      'E-Field'
	oedi -> WriteVarAttr, E_tof_vname, 'SCALETYP',      'linear'
	oedi -> WriteVarAttr, E_tof_vname, 'SI_CONVERSION', '1e-3>V/m'
	oedi -> WriteVarAttr, E_tof_vname, 'UNITS',         'mV/s'
	oedi -> WriteVarAttr, E_tof_vname, 'VALIDMIN',      -1e3
	oedi -> WriteVarAttr, E_tof_vname, 'VALIDMAX',      1e3
	oedi -> WriteVarAttr, E_tof_vname, 'VAR_TYPE',      'data'

	;V_EXB_TRI
	oedi -> WriteVarAttr, v_tri_vname, 'CATDESC',       'ExB Drift velocity from the triangulation (Tri) method.'
	oedi -> WriteVarAttr, v_tri_vname, 'DEPEND_0',       t_vname
	oedi -> WriteVarAttr, v_tri_vname, 'DISPLAY_TYPE',  'time_series'
	oedi -> WriteVarAttr, v_tri_vname, 'FIELDNAM',      'ExB Drift Velocity'
	oedi -> WriteVarAttr, v_tri_vname, 'FILLVAL',        -1e31
	oedi -> WriteVarAttr, v_tri_vname, 'FORMAT',        'F12.4'
	oedi -> WriteVarAttr, v_tri_vname, 'LABLAXIS',      'Velocity'
	oedi -> WriteVarAttr, v_tri_vname, 'SCALETYP',      'linear'
	oedi -> WriteVarAttr, v_tri_vname, 'SI_CONVERSION', '1e3>m/s'
	oedi -> WriteVarAttr, v_tri_vname, 'UNITS',         'km/s'
	oedi -> WriteVarAttr, v_tri_vname, 'VALIDMIN',      -3e8
	oedi -> WriteVarAttr, v_tri_vname, 'VALIDMAX',      3e8
	oedi -> WriteVarAttr, v_tri_vname, 'VAR_TYPE',      'data'

	;E_TRI
	oedi -> WriteVarAttr, E_tri_vname, 'CATDESC',       'Electric field from the triangulation (Tri) method.'
	oedi -> WriteVarAttr, E_tri_vname, 'DEPEND_0',       t_vname
	oedi -> WriteVarAttr, E_tri_vname, 'DISPLAY_TYPE',  'time_series'
	oedi -> WriteVarAttr, E_tri_vname, 'FIELDNAM',      'Electric Field'
	oedi -> WriteVarAttr, E_tri_vname, 'FILLVAL',        -1e31
	oedi -> WriteVarAttr, E_tri_vname, 'FORMAT',        'F12.4'
	oedi -> WriteVarAttr, E_tri_vname, 'LABLAXIS',      'E-Field'
	oedi -> WriteVarAttr, E_tri_vname, 'SCALETYP',      'linear'
	oedi -> WriteVarAttr, E_tri_vname, 'SI_CONVERSION', '1e-3>V/m'
	oedi -> WriteVarAttr, E_tri_vname, 'UNITS',         'mV/s'
	oedi -> WriteVarAttr, E_tri_vname, 'VALIDMIN',      -1e3
	oedi -> WriteVarAttr, E_tri_vname, 'VALIDMAX',      1e3
	oedi -> WriteVarAttr, E_tri_vname, 'VAR_TYPE',      'data'

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, oedi
	return, edi_file
end