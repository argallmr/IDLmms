; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_cal_write
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
;       CAL_DATA:           in, optional, type=struct
;                           EDI ambient data structure with the fields below. The
;                             4 relative and/or 4 absolute calibration fields must be
;                             present. If not provided, empty variables are written::
;                               TT2000_REL   -  TT2000 time stamps for relative calibration data
;                               TT2000_ABS   -  TT2000 time stamps for absolute calibration data
;                               REL_OPTICS   -  Optics state at the time relative calibrations were performed
;                               RELCAL_GDU1  -  Relative calibration factors for GDU1
;                               RELCAL_GDU2  -  Relative calibration factors for GDU2
;                               ABS_OPTICS   -  Optics state at the time absolute calibrations were performed
;                               ABSCAL_GDU1  -  Absolute calibration factors for GDU1
;                               ABSCAL_GDU2  -  Absolute calibration factors for GDU2
;                                
;
; :Keywords:
;       CAL_PATH_ROOT:      in, optional, type=string
;                           Root directory in which the calibration file is written.
;                               `CAL_FILE` will be output to CAL_PATH_ROOT/`SC`/edi. If
;                               not given, the default is to look for the environment
;                               variable CAL_PATH_ROOT then the system variable !edi_amb_init.
;       OPTDESC:            in, optional, type=string, default='amb'
;                           Optional filename descriptor, with parts separated by a hyphen.
;       PARENTS:            in, optional, type=string/strarr, default=''
;                           Names of the parent files required to make `AMB_DATA`.
;
; :Returns:
;       CAL_FILE:           Name of the file created.
;
; :History:
;    Modification History::
;       2015/10/26  -   Written by Matthew Argall
;       2015/02/23  -   Added optics state for absolute and relative calibrations. - MRA
;-
function mms_edi_amb_cal_write, sc, cal_data, $
CAL_PATH_ROOT=cal_path, $
OPTDESC=optdesc, $
PARENTS=parents
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(ocal) then obj_destroy, ocal
		if n_elements(cal_file) gt 0 && file_test(cal_file) then file_delete, cal_file
		MrPrintF, 'LogErr'
		return, ''
	endif

;------------------------------------;
; Version History                    ;
;------------------------------------;
	;Mods to data processing
	mods = [ 'v0.0.0 - Original version.', $
	         'v1.0.0 - Added optics state.' ]
	
	;Get the version
	version = stregex(mods[-1], '^v([0-9]+)\.([0-9]+)\.([0-9]+)', /SUBEXP, /EXTRACT)
	vx      = strtrim(version[1], 2)
	vy      = strtrim(version[2], 2)
	vz      = strtrim(version[3], 2)
	
	;Constants for output file
	instr  = 'edi'
	mode   = 'cal'
	level  = 'l2'
	tstart = '20150312'

;------------------------------------;
; Check Inputs                       ;
;------------------------------------;
	
	;Defaults
	if n_elements(sc)      eq 0 || sc      eq '' then sc      = 'mms#'
	if n_elements(optdesc) eq 0                  then optdesc = 'amb'

	;Cal path. Can be in two places:
	;   - The environment variable CAL_PATH_ROOT
	;   - The system variable !EDI_INIT (unh_edi_init.pro)
	if n_elements(cal_path) eq 0 then begin
		cal_path = getenv('CAL_PATH_ROOT')
		if cal_path eq '' then begin
			defsysv, '!edi_init', EXIST=tf_exist
			if tf_exist $
				then cal_path = !edi_init.cal_path_root $
				else message, 'Cannot determine CAL_PATH_ROOT.'
		endif
	endif

;------------------------------------;
; Create Output File Name            ;
;------------------------------------;
	;Output file
	version = vx + '.' + vy + '.' + vz
	cal_file = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)
	
	;Find the latest z-version
	vz = mms_latest_zversion(cal_path, cal_file)
	
	;Reform the file name
	version = vx + '.' + vy + '.' + string(vz, FORMAT='(i0)')
	cal_file = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)

	;Cal path
	cal_path = filepath('', ROOT_DIR=cal_path, SUBDIRECTORY=[sc, instr])
	if ~file_test(cal_path) then file_mkdir, cal_path

	;Create the fully-qualified file name
	cal_file = filepath(cal_file, ROOT_DIR=cal_path)

	;Notify where file is located
	MrPrintF, 'LogText', 'Creating EDI AMB calibration file at "' + cal_file + '".'

;------------------------------------;
; Check Data                         ;
;------------------------------------;

	;Open the CDF file
	ocal = MrCDF_File(cal_file, /CREATE, /CLOBBER)
	if obj_valid(ocal) eq 0 $
		then message, 'Error creating calibration file.'

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
	logical_file_id = cgRootName(cal_file)
	source_name = 'MMS' + strmid(sc, 3) + '>MMS Satellite Number ' + strmid(sc, 3)
	MrTimeParser, MrTimeStamp(/UTC), '%Y-%M-%dT%H:%m:%S', '%Y%M%d', gen_date

	;   - Instrument Type (1+)
	;           Electric Fields (space)
	;           Magnetic Fields (space)
	;           Particles (space)
	;           Plasma and Solar Wind
	;           Spacecraft Potential Control
	ocal -> WriteGlobalAttr, /CREATE, 'Data_type',                  data_type
	ocal -> WriteGlobalAttr, /CREATE, 'Data_version',               version
	ocal -> WriteGlobalAttr, /CREATE, 'Descriptor',                 'EDI'
	ocal -> WriteGlobalAttr, /CREATE, 'Discipline',                 'Space Physics>Magnetospheric Science'
	ocal -> WriteGlobalAttr, /CREATE, 'File_naming_convention',     'source_descriptor_datatype_yyyyMMdd'
	ocal -> WriteGlobalAttr, /CREATE, 'Generation_date',            gen_date
	ocal -> WriteGlobalAttr, /CREATE, 'Instrument_type',            'Particles (space)'
	ocal -> WriteGlobalAttr, /CREATE, 'Logical_file_id',            logical_file_id
	ocal -> WriteGlobalAttr, /CREATE, 'Logical_source',             logical_source
	ocal -> WriteGlobalAttr, /CREATE, 'Logical_source_description', 'EDI Ambient Calibration File'
	ocal -> WriteGlobalAttr, /CREATE, 'Mission_group',              'MMS'
	ocal -> WriteGlobalAttr, /CREATE, 'PI_affiliation',             'UNH'
	ocal -> WriteGlobalAttr, /CREATE, 'PI_name',                    'Hans Vaith'
	ocal -> WriteGlobalAttr, /CREATE, 'Project',                    'STP>Solar Terrestrial Physics'
	ocal -> WriteGlobalAttr, /CREATE, 'Source_name',                source_name
	ocal -> WriteGlobalAttr, /CREATE, 'TEXT',                       'EDI ambient data. Instrument papers ' + $
	                                                                'for EDI can be found at: ' + $
	                                                                'http://link.springer.com/article/10.1007%2Fs11214-015-0182-7'
	ocal -> WriteGlobalAttr, /CREATE, 'HTTP_LINK',                  ['http://mms-fields.unh.edu/', $
	                                                                 'http://mms.gsfc.nasa.gov/index.html']
	ocal -> WriteGlobalAttr, /CREATE, 'LINK_TEXT',                  ['UNH FIELDS Home Page', $
	                                                                 'NASA MMS Home']
	ocal -> WriteGlobalAttr, /CREATE, 'MODS',                       mods
	ocal -> WriteGlobalAttr, /CREATE, 'Acknowledgements',           ' '
	ocal -> WriteGlobalAttr, /CREATE, 'Generated_by',               'University of New Hampshire'
	ocal -> WriteGlobalAttr, /CREATE, 'Parents',                    ' '
	ocal -> WriteGlobalAttr, /CREATE, 'Skeleton_version',           ' '
	ocal -> WriteGlobalAttr, /CREATE, 'Rules_of_use',               ' '
	ocal -> WriteGlobalAttr, /CREATE, 'Time_resolution',            ' '

;------------------------------------------------------
; Variables                                           |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName[_coordSys][_paramQualifier][_subModeLevel][_mode][_level]
	prefix  = strjoin([sc, instr], '_') + '_'
	suffix  = '_' + strjoin([mode, level], '_')
	
	;Variable names
	t_vname            = 'Epoch'
	trel_vname         = 'epoch_rel'
	tabs_vname         = 'epoch_abs'
	phi_vname          = prefix + 'phi'
	theta_vname        = prefix + 'theta'
	rel_opt_vname      = prefix + 'optics_rel'  + suffix
	rel_gdu1_vname     = prefix + 'rel_gdu1' + suffix
	rel_gdu2_vname     = prefix + 'rel_gdu2' + suffix
	abs_opt_vname      = prefix + 'optics_abs'  + suffix
	abs_gdu1_vname     = prefix + 'abs_gdu1' + suffix
	abs_gdu2_vname     = prefix + 'abs_gdu2' + suffix
	phi_labels_vname   = prefix + 'phi_labels'
	theta_labels_vname = prefix + 'theta_labels'

	;CDF wants dimensions to be ordered as [DIM3, DIM2, DIM1, RECS], but 
	;IDL truncates trailing shallow dimensions (e.g. NxMx1 becomes NxM).
	;To prevent the CDF from allocating space incorrectly, we allocate
	;the number of records (WriteVar will handle the rest).
	ncal = n_elements(cal_data)

;------------------------------------------------------
; Support Data                                        |
;------------------------------------------------------
	nPhi   = 32
	nTheta = 129
	
	dPhi   = 360.0 / 32.0
	dTheta = 360.0 / 512.0

	;Support data
	phi   = findgen(nPhi)  * dPhi
	theta = findgen(nTheta) * dTheta
	ocal -> CreateVar, t_vname, 'CDF_TIME_TT2000'
	ocal -> WriteVar, /CREATE, phi_vname,   phi,   COMPRESSION='GZIP', GZIP_LEVEL=6, /REC_NOVARY
	ocal -> WriteVar, /CREATE, theta_vname, theta, COMPRESSION='GZIP', GZIP_LEVEL=6, /REC_NOVARY

;------------------------------------------------------
; Relative Cals                                       |
;------------------------------------------------------

	;Write Data
	if ncal ne 0 && MrStruct_HasTag(cal_data, 'tt2000_rel') then begin
		;Number of records
		nrecs = n_elements(tt2000_rel)
	
		;Write
		ocal -> WriteVar, trel_vname,     cal_data.tt2000_rel,  /CREATE, CDF_TYPE='CDF_TIME_TT2000'
		ocal -> WriteVar, rel_opt_vname,  cal_data.rel_optics,  /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal -> WriteVar, rel_gdu1_vname, cal_data.relcal_gdu1, /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal -> WriteVar, rel_gdu2_vname, cal_data.relcal_gdu2, /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
	
	;Create Variables
	endif else begin
		ocal -> CreateVar, trel_vname,     'CDF_TIME_TT2000'
		ocal -> CreateVar, rel_opt_vname,  'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal -> CreateVar, rel_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6, DIMENSIONS=[nTheta, nPhi]
		ocal -> CreateVar, rel_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6, DIMENSIONS=[nTheta, nPhi]
	endelse

;------------------------------------------------------
; Absolute Cals                                       |
;------------------------------------------------------
	
	;Write data
	if ncal gt 0 && MrStruct_HasTag(cal_data, 'tt2000_abs') then begin 
		ocal -> WriteVar, tabs_vname, cal_data.tt2000_abs, /CREATE, CDF_TYPE='CDF_TIME_TT2000'
		ocal -> WriteVar, abs_opt_vname,  cal_data.abs_optics,  /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal -> WriteVar, abs_gdu1_vname, cal_data.abscal_gdu1, /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal -> WriteVar, abs_gdu2_vname, cal_data.abscal_gdu2, /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
	
	;Create Variables
	endif else begin
		ocal -> CreateVar, tabs_vname,     'CDF_TIME_TT2000'
		ocal -> CreateVar, abs_opt_vname,  'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal -> CreateVar, abs_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		ocal -> CreateVar, abs_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
	endelse

;------------------------------------------------------
; Metadata                                            |
;------------------------------------------------------
	
	;Label pointers
	ph_lbl_vals = 'Phi'   + string(indgen(32),  FORMAT='(i0)')
	th_lbl_vals = 'Theta' + string(indgen(129), FORMAT='(i0)')
	ocal -> WriteVar, /CREATE, phi_labels_vname,   temporary(ph_lbl_vals), /REC_NOVARY
	ocal -> WriteVar, /CREATE, theta_labels_vname, temporary(th_lbl_vals), /REC_NOVARY
	
;------------------------------------------------------
; Variable Attributes                                 |
;------------------------------------------------------
	;Create the variable attributes
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'CATDESC'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'DELTA_PLUS'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_0'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_1'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_2'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'DISPLAY_TYPE'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'FIELDNAM'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'FILLVAL'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'FORMAT'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'LABLAXIS'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'LABL_PTR_1'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'LABL_PTR_2'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'SCALETYP'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'SI_CONVERSION'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'TIME_BASE'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'UNITS'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMIN'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMAX'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'VAR_NOTES'
	ocal -> CreateAttr, /VARIABLE_SCOPE, 'VAR_TYPE'
	
	;EPOCH
	ocal -> WriteVarAttr, t_vname, 'CATDESC',       'A place holder'
	ocal -> WriteVarAttr, t_vname, 'FIELDNAM',      'Time'
	ocal -> WriteVarAttr, t_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	ocal -> WriteVarAttr, t_vname, 'FORMAT',        'I16'
	ocal -> WriteVarAttr, t_vname, 'LABLAXIS',      'UT'
	ocal -> WriteVarAttr, t_vname, 'SI_CONVERSION', '1e-9>s'
	ocal -> WriteVarAttr, t_vname, 'TIME_BASE',     'J2000'
	ocal -> WriteVarAttr, t_vname, 'UNITS',         'UT'
	ocal -> WriteVarAttr, t_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	ocal -> WriteVarAttr, t_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 32), /CDF_EPOCH
	ocal -> WriteVarAttr, t_vname, 'VAR_TYPE',      'support_data'
	
	;EPOCH_REL
	ocal -> WriteVarAttr, trel_vname, 'CATDESC',       'TT2000 time tags for EDI relative calibration parameters.'
	ocal -> WriteVarAttr, trel_vname, 'FIELDNAM',      'Time'
	ocal -> WriteVarAttr, trel_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	ocal -> WriteVarAttr, trel_vname, 'FORMAT',        'I16'
	ocal -> WriteVarAttr, trel_vname, 'LABLAXIS',      'UT'
	ocal -> WriteVarAttr, trel_vname, 'SI_CONVERSION', '1e-9>s'
	ocal -> WriteVarAttr, trel_vname, 'TIME_BASE',     'J2000'
	ocal -> WriteVarAttr, trel_vname, 'UNITS',         'UT'
	ocal -> WriteVarAttr, trel_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	ocal -> WriteVarAttr, trel_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 32), /CDF_EPOCH
	ocal -> WriteVarAttr, trel_vname, 'VAR_TYPE',      'support_data'
	
	;EPOCH_ABS
	ocal -> WriteVarAttr, tabs_vname, 'CATDESC',       'TT2000 time tags for EDI absolute calibration parameters.'
	ocal -> WriteVarAttr, tabs_vname, 'FIELDNAM',      'Time'
	ocal -> WriteVarAttr, tabs_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	ocal -> WriteVarAttr, tabs_vname, 'FORMAT',        'I16'
	ocal -> WriteVarAttr, tabs_vname, 'LABLAXIS',      'UT'
	ocal -> WriteVarAttr, tabs_vname, 'SI_CONVERSION', '1e-9>s'
	ocal -> WriteVarAttr, tabs_vname, 'TIME_BASE',     'J2000'
	ocal -> WriteVarAttr, tabs_vname, 'UNITS',         'UT'
	ocal -> WriteVarAttr, tabs_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	ocal -> WriteVarAttr, tabs_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2075, 12, 32), /CDF_EPOCH
	ocal -> WriteVarAttr, tabs_vname, 'VAR_TYPE',      'support_data'
	
	;PHI
	ocal -> WriteVarAttr, phi_vname, 'CATDESC',       'Look-direction azimuth angle'
	ocal -> WriteVarAttr, phi_vname, 'DELTA_PLUS',    360.0/32.0
	ocal -> WriteVarAttr, phi_vname, 'FIELDNAM',      'Phi'
	ocal -> WriteVarAttr, phi_vname, 'FILLVAL',       -1e31
	ocal -> WriteVarAttr, phi_vname, 'FORMAT',        'F12.4'
	ocal -> WriteVarAttr, phi_vname, 'LABLAXIS',      'Phi'
	ocal -> WriteVarAttr, phi_vname, 'SI_CONVERSION', '0.01745>rad'
	ocal -> WriteVarAttr, phi_vname, 'UNITS',         'deg'
	ocal -> WriteVarAttr, phi_vname, 'VALIDMIN',      0.0
	ocal -> WriteVarAttr, phi_vname, 'VALIDMAX',      360.0
	ocal -> WriteVarAttr, phi_vname, 'VAR_TYPE',      'support_data'
	
	;THETA
	ocal -> WriteVarAttr, theta_vname, 'CATDESC',       'Look-direction polar angle'
	ocal -> WriteVarAttr, theta_vname, 'DELTA_PLUS',    360.0/512.0
	ocal -> WriteVarAttr, theta_vname, 'FIELDNAM',      'Theta'
	ocal -> WriteVarAttr, theta_vname, 'FILLVAL',        -1e31
	ocal -> WriteVarAttr, theta_vname, 'FORMAT',        'F12.4'
	ocal -> WriteVarAttr, theta_vname, 'LABLAXIS',      'Theta'
	ocal -> WriteVarAttr, theta_vname, 'SI_CONVERSION', '0.01745>rad'
	ocal -> WriteVarAttr, theta_vname, 'UNITS',         'deg'
	ocal -> WriteVarAttr, theta_vname, 'VALIDMIN',      0.0
	ocal -> WriteVarAttr, theta_vname, 'VALIDMAX',      90.0
	ocal -> WriteVarAttr, theta_vname, 'VAR_TYPE',      'support_data'
	
	;OPTICS_REL
	ocal -> WriteVarAttr, rel_opt_vname, 'CATDESC',       'Optics state during relative calibrations'
	ocal -> WriteVarAttr, rel_opt_vname, 'FIELDNAM',      'Optics state'
	ocal -> WriteVarAttr, rel_opt_vname, 'FILLVAL',       65535US
	ocal -> WriteVarAttr, rel_opt_vname, 'FORMAT',        'I4'
	ocal -> WriteVarAttr, rel_opt_vname, 'LABLAXIS',      'Optics state'
	ocal -> WriteVarAttr, rel_opt_vname, 'VALIDMIN',      0US
	ocal -> WriteVarAttr, rel_opt_vname, 'VALIDMAX',      1000US
	ocal -> WriteVarAttr, rel_opt_vname, 'VAR_TYPE',      'support_data'

	;REL_CAL_GDU1
	ocal -> WriteVarAttr, rel_gdu1_vname, 'CATDESC',       'EDI relative calibration factor as a function of polar and azimuthal look angle for GDU1.'
	ocal -> WriteVarAttr, rel_gdu1_vname, 'DEPEND_0',      trel_vname
	ocal -> WriteVarAttr, rel_gdu1_vname, 'DEPEND_1',      theta_vname
	ocal -> WriteVarAttr, rel_gdu1_vname, 'DEPEND_2',      phi_vname
	ocal -> WriteVarAttr, rel_gdu1_vname, 'DISPLAY_TYPE',   'spectrogram'
	ocal -> WriteVarAttr, rel_gdu1_vname, 'FIELDNAM',      'Relative calibration factor'
	ocal -> WriteVarAttr, rel_gdu1_vname, 'FILLVAL',        -1e31
	ocal -> WriteVarAttr, rel_gdu1_vname, 'FORMAT',        'F12.6'
	ocal -> WriteVarAttr, rel_gdu1_vname, 'LABL_PTR_1',    theta_labels_vname
	ocal -> WriteVarAttr, rel_gdu1_vname, 'LABL_PTR_2',    phi_labels_vname
;	ocal -> WriteVarAttr, rel_gdu1_vname, 'SI_CONVERSION', ' '
;	ocal -> WriteVarAttr, rel_gdu1_vname, 'UNITS',         ' '
	ocal -> WriteVarAttr, rel_gdu1_vname, 'VALIDMIN',      0.0
	ocal -> WriteVarAttr, rel_gdu1_vname, 'VALIDMAX',      100.0
	ocal -> WriteVarAttr, rel_gdu1_vname, 'VAR_NOTES',     'Relative calibrations are obtained via a linear fit of electron counts during intervals ' + $
	                                                       'during which count rates are assumed to be constant.'
	ocal -> WriteVarAttr, rel_gdu1_vname, 'VAR_TYPE',      'data'

	;REL_CAL_GDU2
	ocal -> WriteVarAttr, rel_gdu2_vname, 'CATDESC',       'EDI relative calibration factor as a function of polar and azimuthal look angle for GDU2.'
	ocal -> WriteVarAttr, rel_gdu2_vname, 'DEPEND_0',       trel_vname
	ocal -> WriteVarAttr, rel_gdu2_vname, 'DEPEND_1',       theta_vname
	ocal -> WriteVarAttr, rel_gdu2_vname, 'DEPEND_2',       phi_vname
	ocal -> WriteVarAttr, rel_gdu2_vname, 'DISPLAY_TYPE',   'spectrogram'
	ocal -> WriteVarAttr, rel_gdu2_vname, 'FIELDNAM',      'Relative calibration factor'
	ocal -> WriteVarAttr, rel_gdu2_vname, 'FILLVAL',        -1e31
	ocal -> WriteVarAttr, rel_gdu2_vname, 'FORMAT',        'F12.6'
	ocal -> WriteVarAttr, rel_gdu2_vname, 'LABL_PTR_1',    theta_labels_vname
	ocal -> WriteVarAttr, rel_gdu2_vname, 'LABL_PTR_2',    phi_labels_vname
;	ocal -> WriteVarAttr, rel_gdu2_vname, 'SI_CONVERSION', ' '
;	ocal -> WriteVarAttr, rel_gdu2_vname, 'UNITS',         ' '
	ocal -> WriteVarAttr, rel_gdu2_vname, 'VALIDMIN',      0.0
	ocal -> WriteVarAttr, rel_gdu2_vname, 'VALIDMAX',      100.0
	ocal -> WriteVarAttr, rel_gdu2_vname, 'VAR_NOTES',     'Relative calibrations are obtained via a linear fit of electron counts during intervals ' + $
	                                                       'during which count rates are assumed to be constant.'
	ocal -> WriteVarAttr, rel_gdu2_vname, 'VAR_TYPE',      'data'
	
	;OPTICS_ABS
	ocal -> WriteVarAttr, abs_opt_vname, 'CATDESC',       'Optics state during absolute calibrations'
	ocal -> WriteVarAttr, abs_opt_vname, 'FIELDNAM',      'Optics state'
	ocal -> WriteVarAttr, abs_opt_vname, 'FILLVAL',       65535US
	ocal -> WriteVarAttr, abs_opt_vname, 'FORMAT',        'I4'
	ocal -> WriteVarAttr, abs_opt_vname, 'LABLAXIS',      'Optics state'
	ocal -> WriteVarAttr, abs_opt_vname, 'VALIDMIN',      0US
	ocal -> WriteVarAttr, abs_opt_vname, 'VALIDMAX',      1000US
	ocal -> WriteVarAttr, abs_opt_vname, 'VAR_TYPE',      'support_data'

	;ABS_CAL_GDU1
	ocal -> WriteVarAttr, abs_gdu1_vname, 'CATDESC',       'EDI absolute calibration factor for GDU1.'
	ocal -> WriteVarAttr, abs_gdu1_vname, 'DEPEND_0',       tabs_vname
	ocal -> WriteVarAttr, abs_gdu1_vname, 'DISPLAY_TYPE',   'spectrogram'
	ocal -> WriteVarAttr, abs_gdu1_vname, 'FIELDNAM',      'Absolute calibration factor'
	ocal -> WriteVarAttr, abs_gdu1_vname, 'FILLVAL',        -1e31
	ocal -> WriteVarAttr, abs_gdu1_vname, 'FORMAT',        'F12.6'
	ocal -> WriteVarAttr, abs_gdu1_vname, 'LABLAXIS',      'AbsCal GDU1'
	ocal -> WriteVarAttr, abs_gdu1_vname, 'SI_CONVERSION', '1e4>m^-2 s^-1'
	ocal -> WriteVarAttr, abs_gdu1_vname, 'UNITS',         'cm^-2 s^-1'
	ocal -> WriteVarAttr, abs_gdu1_vname, 'VALIDMIN',      0.0
	ocal -> WriteVarAttr, abs_gdu1_vname, 'VALIDMAX',      1e8
	ocal -> WriteVarAttr, abs_gdu1_vname, 'VAR_NOTES',     'Absolute calibrations are obtained by comparing FPI electron fluxes in the energy ' + $
	                                                       'bin closest to EDI operation enery to EDI electron counts. They have an error of ' + $
	                                                       '+/- 20%, which is accounted for during the calibration process.'
	ocal -> WriteVarAttr, abs_gdu1_vname, 'VAR_TYPE',      'data'

	;ABS_CAL_GDU2
	ocal -> WriteVarAttr, abs_gdu2_vname, 'CATDESC',       'EDI absolute calibration factor for GDU2.'
	ocal -> WriteVarAttr, abs_gdu2_vname, 'DEPEND_0',       tabs_vname
	ocal -> WriteVarAttr, abs_gdu2_vname, 'DISPLAY_TYPE',   'spectrogram'
	ocal -> WriteVarAttr, abs_gdu2_vname, 'FIELDNAM',      'Relative calibration factor'
	ocal -> WriteVarAttr, abs_gdu2_vname, 'FILLVAL',        -1e31
	ocal -> WriteVarAttr, abs_gdu2_vname, 'FORMAT',        'F12.6'
	ocal -> WriteVarAttr, abs_gdu2_vname, 'LABLAXIS',      'AbsCal GDU2'
	ocal -> WriteVarAttr, abs_gdu2_vname, 'SI_CONVERSION', '1e4>m^-2 s^-1'
	ocal -> WriteVarAttr, abs_gdu2_vname, 'UNITS',         'cm^-2 s^-1'
	ocal -> WriteVarAttr, abs_gdu2_vname, 'VALIDMIN',      0.0
	ocal -> WriteVarAttr, abs_gdu2_vname, 'VALIDMAX',      1e8
	ocal -> WriteVarAttr, abs_gdu1_vname, 'VAR_NOTES',     'Absolute calibrations are obtained by comparing FPI electron fluxes in the energy ' + $
	                                                       'bin closest to EDI operation enery to EDI electron counts. They have an error of ' + $
	                                                       '+/- 20%, which is accounted for during the calibration process.'
	ocal -> WriteVarAttr, abs_gdu2_vname, 'VAR_TYPE',      'data'

	;PHI_LABELS
	ocal -> WriteVarAttr, phi_labels_vname, 'CATDESC',       'Labels for phi look-direction.'
	ocal -> WriteVarAttr, phi_labels_vname, 'FIELDNAM',      'Phi Labels'
	ocal -> WriteVarAttr, phi_labels_vname, 'FORMAT',        'A5'
	ocal -> WriteVarAttr, phi_labels_vname, 'VAR_TYPE',      'metadata'

	;THETA_LABELS
	ocal -> WriteVarAttr, theta_labels_vname, 'CATDESC',       'Labels for theta look-direction.'
	ocal -> WriteVarAttr, theta_labels_vname, 'FIELDNAM',      'Theta Labels'
	ocal -> WriteVarAttr, theta_labels_vname, 'FORMAT',        'A8'
	ocal -> WriteVarAttr, theta_labels_vname, 'VAR_TYPE',      'metadata'

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, ocal
	return, cal_file
end