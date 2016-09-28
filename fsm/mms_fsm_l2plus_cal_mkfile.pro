; docformat = 'rst'
;
; NAME:
;    mms_fsm_l2plus_cal_mkfile
;
; PURPOSE:
;+
;   Create a skeleton FSM calibration file.
;
; :Categories:
;    MMS, FSM, L2Plus
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
;       FSM_FILE:           Name of the file created.
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
;       2016/09/05  -   Written by Matthew Argall
;-
function mms_fsm_l2plus_cal_mkfile, sc, mode, tstart, data, $
BRST=brst, $
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
		if obj_valid(ofsm) then obj_destroy, ofsm
		if n_elements(amb_file) gt 0 && file_test(amb_file) then file_delete, amb_file
		
		;Report error
		if n_elements(status) eq 0 || status eq 0 then status = 100

		MrPrintF, 'LogErr'
		
		;Return
		return, ''
	endif
	
	tf_brst = keyword_set(brst)
	if n_elements(optdesc) eq 0 then optdesc = 'amb'

;------------------------------------;
; Version History                    ;
;------------------------------------;
	
	mods = [ 'v0.0.0 - Original version.' $
	       ]
	
	;Get the version
	version = stregex(mods[-1], '^v([0-9]+)\.([0-9]+)\.([0-9]+)', /SUBEXP, /EXTRACT)
	vx      = strtrim(version[1], 2)
	vy      = strtrim(version[2], 2)
	vz      = strtrim(version[3], 2)
	
	;Constants for output file
	instr   = 'fsm'
	level   = 'l2plus'

;------------------------------------;
; Check Inputs                       ;
;------------------------------------;
	
	;Defaults
	tf_empty = keyword_set(empty_file)
	if n_elements(sc)      eq 0 || sc      eq '' then sc      = 'mms#'
	if n_elements(mode)    eq 0 || mode    eq '' then mode    = 'mode'
	if n_elements(optdesc) eq 0                  then optdesc = 'cal-mag'
	if n_elements(parents) eq 0                  then parents = ' '
	
	;Check if the system variable exists
	defsysv, '!fsm_init', EXISTS=tf_sysv
	if tf_sysv then begin
		if n_elements(dropbox)   eq 0 then dropbox   = !fsm_init.dropbox_root
		if n_elements(data_path) eq 0 then data_path = !fsm_init.data_path_root
	endif else begin
		if n_elements(dropbox)   eq 0 then cd, CURRENT=dropbox
		if n_elements(data_path) eq 0 then cd, CURRENT=data_path
	endelse

;------------------------------------;
; Check Data                         ;
;------------------------------------;
	if ~data -> HasKey('f')          then message, 'FSM_DATA must have key "f".'
	if ~data -> HasKey('flag')       then message, 'FSM_DATA must have key "flag".'
	if ~data -> HasKey('amp_bins')   then message, 'FSM_DATA must have key "amp_bins".'
	if ~data -> HasKey('phase_bins') then message, 'FSM_DATA must have key "phase_bins".'
	if ~data -> HasKey('psd_bins')   then message, 'FSM_DATA must have key "psd_bins".'

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
	fsm_file = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)

	;Find the latest z-version
	;   - Look in both DROPBOX and DATA_PATH
	vz = mms_latest_zversion(dropbox, fsm_file, ROOT=data_path)
	
	;Reform the file name
	version = vx + '.' + vy + '.' + string(vz, FORMAT='(i0)')
	fsm_file = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)
	fsm_file = filepath(fsm_file, ROOT_DIR=dropbox)

	;Notify where file is located
	MrPrintF, 'LogText', 'Creating FSM file at "' + fsm_file + '".'

;------------------------------------;
; Create File                        ;
;------------------------------------;

	;Open the CDF file
	ofsm = MrCDF_File(fsm_file, /CREATE, /CLOBBER)
	if obj_valid(ofsm) eq 0 then return, ''

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
	logical_file_id = cgRootName(fsm_file)
	source_name = 'MMS' + strmid(sc, 3) + '>MMS Satellite Number ' + strmid(sc, 3)
	MrTimeParser, MrTimeStamp(/UTC), '%Y-%M-%dT%H:%m:%S', '%Y%M%d', gen_date

	;   - Instrument Type (1+)
	;           Electric Fields (space)
	;           Magnetic Fields (space)
	;           Particles (space)
	;           Plasma and Solar Wind
	;           Spacecraft Potential Control
	ofsm -> WriteGlobalAttr, /CREATE, 'Data_type',                  data_type
	ofsm -> WriteGlobalAttr, /CREATE, 'Data_version',               version
	ofsm -> WriteGlobalAttr, /CREATE, 'Descriptor',                 'FSM'
	ofsm -> WriteGlobalAttr, /CREATE, 'Discipline',                 'Space Physics>Magnetospheric Science'
	ofsm -> WriteGlobalAttr, /CREATE, 'File_naming_convention',     'source_descriptor_datatype_yyyyMMdd'
	ofsm -> WriteGlobalAttr, /CREATE, 'Generation_date',            gen_date
	ofsm -> WriteGlobalAttr, /CREATE, 'Instrument_type',            'Particles (space)'
	ofsm -> WriteGlobalAttr, /CREATE, 'Logical_file_id',            logical_file_id
	ofsm -> WriteGlobalAttr, /CREATE, 'Logical_source',             logical_source
	ofsm -> WriteGlobalAttr, /CREATE, 'Logical_source_description', 'Level 2-Plus fluxgate-searchcoil merged magnetic field data'
	ofsm -> WriteGlobalAttr, /CREATE, 'Mission_group',              'MMS'
	ofsm -> WriteGlobalAttr, /CREATE, 'PI_affiliation',             'UNH, LPP, IWF, UCLA, UCLA, IWF, UNH'
	ofsm -> WriteGlobalAttr, /CREATE, 'PI_name',                    'R.B. Torbert, O. LeContel, W. Magnes, C.T. Russell, R. Strangeway, ' + $
	                                                                'D. Fischer, M.R. Argall.'
	ofsm -> WriteGlobalAttr, /CREATE, 'Project',                    'STP>Solar Terrestrial Physics'
	ofsm -> WriteGlobalAttr, /CREATE, 'Source_name',                source_name
	ofsm -> WriteGlobalAttr, /CREATE, 'TEXT',                       'FSM calibration data used when creating the FSM data product. ' + $
	                                                                'A description of the method and of the data can be found at: ' + $
	                                                                ''
	ofsm -> WriteGlobalAttr, /CREATE, 'HTTP_LINK',                  ['http://mms-fields.unh.edu/', $
	                                                                 'http://mms.gsfc.nasa.gov/index.html']
	ofsm -> WriteGlobalAttr, /CREATE, 'LINK_TEXT',                  ['UNH FIELDS Home Page', $
	                                                                 'NASA MMS Home']
	ofsm -> WriteGlobalAttr, /CREATE, 'MODS',                       mods
	ofsm -> WriteGlobalAttr, /CREATE, 'Acknowledgements',           ' '
	ofsm -> WriteGlobalAttr, /CREATE, 'Generated_by',               'University of New Hampshire'
	ofsm -> WriteGlobalAttr, /CREATE, 'Parents',                    parents
	ofsm -> WriteGlobalAttr, /CREATE, 'Skeleton_version',           ' '
	ofsm -> WriteGlobalAttr, /CREATE, 'Rules_of_use',               ' '
	ofsm -> WriteGlobalAttr, /CREATE, 'Time_resolution',            ' '

;------------------------------------------------------
; Variable Names                                      |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName[_coordSys][_paramQualifier][_subModeLevel][_mode][_level]
	prefix  = strjoin([sc, instr], '_') + '_'
	suffix  = '_' + strjoin([mode, level], '_')
	
	t_vname           = 'Epoch'
	f_vname           = prefix + 'f'                + suffix
	flag_vname        = prefix + 'flag'             + suffix
	comp_index_vname  = prefix + 'comp_index'       + suffix
	amp_bins_vname    = prefix + 'amp_bins'         + suffix
	phase_bins_vname  = prefix + 'phase_bins'       + suffix
	psd_bins_vname    = prefix + 'psd_bins'         + suffix
	amp_hist_vname    = prefix + 'amp_hist'         + suffix
	phase_hist_vname  = prefix + 'phase_hist'       + suffix
	psd_hist_vname    = prefix + 'psd_hist'         + suffix
	amp_floor_vname   = prefix + 'amp_floor'        + suffix
	phase_floor_vname = prefix + 'phase_floor'      + suffix
	psd_floor_vname   = prefix + 'psd_floor'        + suffix
	f_labl_vname      = prefix + 'f_labls'          + suffix
	flag_labl_vname   = prefix + 'flag_labls'       + suffix
	comp_labl_vname   = prefix + 'comp_labls'       + suffix
	amp_labl_vname    = prefix + 'amp_bins_labls'   + suffix
	phase_labl_vname  = prefix + 'phase_bins_labls' + suffix
	psd_labl_vname    = prefix + 'psd_bins_labls'   + suffix

;------------------------------------------------------
; Metadata                                            |
;------------------------------------------------------
	;Dimensions
	nF     = n_elements(data['f'])
	nComp  = n_elements(data['comp'])
	nFlag  = n_elements(data['flag'])
	nAmp   = n_elements(data['amp_bins'])
	nPhase = n_elements(data['phase_bins'])
	nPSD   = n_elements(data['psd_bins'])
	
	;Component
	comp_index = [0B, 1B, 2B]
	comp_labl  = ['x', 'y', 'z']
	
	;Frequency
	powten = floor(alog10(nF)) + 1
	fmt    = '(i0' + string(powten, FORMAT='(i0)') + ')'
	f_labl = 'freq' + string(lindgen(nF), FORMAT=fmt)
	
	;Flag
	powten    = floor(alog10(nFlag)) + 1
	fmt       = '(i0' + string(powten, FORMAT='(i0)') + ')'
	flag_labl = 'amp' + string(lindgen(nFlag), FORMAT=fmt)
	
	;Amplitude
	powten   = floor(alog10(nAmp)) + 1
	fmt      = '(i0' + string(powten, FORMAT='(i0)') + ')'
	amp_labl = 'amp' + string(lindgen(nAmp), FORMAT=fmt)
	
	;Phase
	powten     = floor(alog10(nPhase)) + 1
	fmt        = '(i0' + string(powten, FORMAT='(i0)') + ')'
	phase_labl = 'phase' + string(lindgen(nPhase), FORMAT=fmt)
	
	;PSD
	powten   = floor(alog10(nPSD)) + 1
	fmt      = '(i0' + string(powten, FORMAT='(i0)') + ')'
	psd_labl = 'psd' + string(lindgen(nPSD), FORMAT=fmt)

;------------------------------------------------------
; Create Variables                                    |
;------------------------------------------------------

	;Write variable data to file
	ofsm -> CreateVar, t_vname,           'CDF_TIME_TT2000'
	ofsm -> CreateVar, amp_hist_vname,    'CDF_UINT4', DIMENSIONS=[nAmp,   nComp, nFlag], COMPRESSION='GZIP', GZIP_LEVEL=6
	ofsm -> CreateVar, phase_hist_vname,  'CDF_UINT4', DIMENSIONS=[nPhase, nComp, nFlag], COMPRESSION='GZIP', GZIP_LEVEL=6
	ofsm -> CreateVar, psd_hist_vname,    'CDF_UINT4', DIMENSIONS=[nPSD,   nComp, nFlag], COMPRESSION='GZIP', GZIP_LEVEL=6
	ofsm -> CreateVar, amp_floor_vname,   'CDF_FLOAT', DIMENSIONS=[nComp, nFlag],       COMPRESSION='GZIP', GZIP_LEVEL=6
	ofsm -> CreateVar, phase_floor_vname, 'CDF_FLOAT', DIMENSIONS=[nComp, nFlag],       COMPRESSION='GZIP', GZIP_LEVEL=6
	ofsm -> CreateVar, psd_floor_vname,   'CDF_FLOAT', DIMENSIONS=[nComp, nFlag],       COMPRESSION='GZIP', GZIP_LEVEL=6
	ofsm -> CreateVar, f_vname,           'CDF_FLOAT', DIMENSIONS=nF,     COMPRESSION='GZIP', GZIP_LEVEL=6, /REC_NOVARY
	ofsm -> CreateVar, flag_vname,        'CDF_UINT1', DIMENSIONS=nFlag,  COMPRESSION='GZIP', GZIP_LEVEL=6, /REC_NOVARY
	ofsm -> CreateVar, amp_bins_vname,    'CDF_FLOAT', DIMENSIONS=nAmp,   COMPRESSION='GZIP', GZIP_LEVEL=6, /REC_NOVARY
	ofsm -> CreateVar, phase_bins_vname,  'CDF_FLOAT', DIMENSIONS=nPhase, COMPRESSION='GZIP', GZIP_LEVEL=6, /REC_NOVARY
	ofsm -> CreateVar, psd_bins_vname,    'CDF_FLOAT', DIMENSIONS=nPSD,   COMPRESSION='GZIP', GZIP_LEVEL=6, /REC_NOVARY
	
	;Metadata
	ofsm -> WriteVar, /CREATE, comp_index_vname, comp_index, /REC_NOVARY
	ofsm -> WriteVar, /CREATE, comp_labl_vname,  comp_labl,  /REC_NOVARY
	ofsm -> WriteVar, /CREATE, f_labl_vname,     f_labl,     /REC_NOVARY
	ofsm -> WriteVar, /CREATE, flag_labl_vname,  flag_labl,  /REC_NOVARY
	ofsm -> WriteVar, /CREATE, amp_labl_vname,   amp_labl,   /REC_NOVARY
	ofsm -> WriteVar, /CREATE, phase_labl_vname, phase_labl, /REC_NOVARY
	ofsm -> WriteVar, /CREATE, psd_labl_vname,   psd_labl,   /REC_NOVARY

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
		ofsm -> WriteVar, t_vname, epoch_val
	endif
	
;------------------------------------------------------
; Create Variable Attributes                          |
;------------------------------------------------------
	;Create the variable attributes
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'CATDESC'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_0'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_1'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_2'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_3'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'DISPLAY_TYPE'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'FIELDNAM'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'FILLVAL'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'FORMAT'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'LABLAXIS'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'LABL_PTR_1'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'LABL_PTR_2'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'LABL_PTR_3'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'SCALETYP'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'SI_CONVERSION'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'TIME_BASE'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'UNITS'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMIN'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMAX'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'VAR_NOTES'
	ofsm -> CreateAttr, /VARIABLE_SCOPE, 'VAR_TYPE'
	
;------------------------------------------------------
; Support Data Attributes                             |
;------------------------------------------------------
	
	;TT2000
	ofsm -> WriteVarAttr, t_vname, 'CATDESC',       'TT2000 time tags for FSM noise floor calibrations.'
	ofsm -> WriteVarAttr, t_vname, 'FIELDNAM',      'Time'
	ofsm -> WriteVarAttr, t_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	ofsm -> WriteVarAttr, t_vname, 'FORMAT',        'I16'
	ofsm -> WriteVarAttr, t_vname, 'LABLAXIS',      'UT'
	ofsm -> WriteVarAttr, t_vname, 'SI_CONVERSION', '1e-9>s'
	ofsm -> WriteVarAttr, t_vname, 'TIME_BASE',     'J2000'
	ofsm -> WriteVarAttr, t_vname, 'UNITS',         'ns'
	ofsm -> WriteVarAttr, t_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	ofsm -> WriteVarAttr, t_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2065, 12, 31), /CDF_EPOCH
	ofsm -> WriteVarAttr, t_vname, 'VAR_TYPE',      'support_data'

	;FREQUENCY
	ofsm -> WriteVarAttr, f_vname, 'CATDESC',       'Frequency bins at which the noise floor is computed.'
	ofsm -> WriteVarAttr, f_vname, 'FIELDNAM',      'Frequency'
	ofsm -> WriteVarAttr, f_vname, 'FILLVAL',        -1e31
	ofsm -> WriteVarAttr, f_vname, 'FORMAT',        'F10.4'
	ofsm -> WriteVarAttr, f_vname, 'LABL_PTR_1',    f_labl_vname
	ofsm -> WriteVarAttr, f_vname, 'SI_CONVERSION', '1e0>Hz'
	ofsm -> WriteVarAttr, f_vname, 'UNITS',         'Hz'
	ofsm -> WriteVarAttr, f_vname, 'VALIDMIN',      0.0
	ofsm -> WriteVarAttr, f_vname, 'VALIDMAX',      8192.0
	ofsm -> WriteVarAttr, f_vname, 'VAR_TYPE',      'support_data'

	;COMPONENT INDEX
	ofsm -> WriteVarAttr, comp_index_vname, 'CATDESC',       'Index indicating the magnetic field component: 0=x, 1=y, 2=z.'
	ofsm -> WriteVarAttr, comp_index_vname, 'FIELDNAM',      'Component Index'
	ofsm -> WriteVarAttr, comp_index_vname, 'FILLVAL',        65535US
	ofsm -> WriteVarAttr, comp_index_vname, 'FORMAT',        'I1'
	ofsm -> WriteVarAttr, comp_index_vname, 'LABL_PTR_1',    comp_labl_vname
	ofsm -> WriteVarAttr, comp_index_vname, 'LABLAXIS',      'Comp Index'
	ofsm -> WriteVarAttr, comp_index_vname, 'VALIDMIN',      0B
	ofsm -> WriteVarAttr, comp_index_vname, 'VALIDMAX',      2B
	ofsm -> WriteVarAttr, comp_index_vname, 'VAR_TYPE',      'support_data'

	;AMPLITUDE BINS
	ofsm -> WriteVarAttr, amp_bins_vname, 'CATDESC',       'Wave amplitude bins.'
	ofsm -> WriteVarAttr, amp_bins_vname, 'FIELDNAM',      'Amplitude bins'
	ofsm -> WriteVarAttr, amp_bins_vname, 'FILLVAL',       -1e31
	ofsm -> WriteVarAttr, amp_bins_vname, 'FORMAT',        'F9.4'
	ofsm -> WriteVarAttr, amp_bins_vname, 'LABL_PTR_1',    amp_labl_vname
	ofsm -> WriteVarAttr, amp_bins_vname, 'LABLAXIS',      'Amp'
	ofsm -> WriteVarAttr, amp_bins_vname, 'SI_CONVERSION', 'nT'
	ofsm -> WriteVarAttr, amp_bins_vname, 'UNITS',         '1e-9>T'
	ofsm -> WriteVarAttr, amp_bins_vname, 'VALIDMIN',      0.0
	ofsm -> WriteVarAttr, amp_bins_vname, 'VALIDMAX',      1e10
	ofsm -> WriteVarAttr, amp_bins_vname, 'VAR_TYPE',      'support_data'

	;PHASE BINS
	ofsm -> WriteVarAttr, phase_bins_vname, 'CATDESC',       'Phase bins into which magnetic field spectral data is sorted.'
	ofsm -> WriteVarAttr, phase_bins_vname, 'FIELDNAM',      'Phase bins'
	ofsm -> WriteVarAttr, phase_bins_vname, 'FILLVAL',        -1e31
	ofsm -> WriteVarAttr, phase_bins_vname, 'FORMAT',        'F9.4'
	ofsm -> WriteVarAttr, phase_bins_vname, 'LABL_PTR_1',    phase_labl_vname
	ofsm -> WriteVarAttr, phase_bins_vname, 'LABLAXIS',      'Phase'
	ofsm -> WriteVarAttr, phase_bins_vname, 'SI_CONVERSION', ' '
	ofsm -> WriteVarAttr, phase_bins_vname, 'UNITS',         'degrees'
	ofsm -> WriteVarAttr, phase_bins_vname, 'VALIDMIN',      -180.0
	ofsm -> WriteVarAttr, phase_bins_vname, 'VALIDMAX',      180.0
	ofsm -> WriteVarAttr, phase_bins_vname, 'VAR_TYPE',      'support_data'

	;PSD BINS
	ofsm -> WriteVarAttr, psd_bins_vname, 'CATDESC',       'Power spectral density bins into which the magnetic field spectra is sorted.'
	ofsm -> WriteVarAttr, psd_bins_vname, 'FIELDNAM',      'PSD Bins'
	ofsm -> WriteVarAttr, psd_bins_vname, 'FILLVAL',        -1e31
	ofsm -> WriteVarAttr, psd_bins_vname, 'FORMAT',        'I9.4'
	ofsm -> WriteVarAttr, psd_bins_vname, 'SI_CONVERSION', '10^(PSD*1e-18)>T^2/Hz'
	ofsm -> WriteVarAttr, psd_bins_vname, 'UNITS',         'Log10(nT^2/Hz)'
	ofsm -> WriteVarAttr, psd_bins_vname, 'VALIDMIN',      0.0
	ofsm -> WriteVarAttr, psd_bins_vname, 'VALIDMAX',      1e10
	ofsm -> WriteVarAttr, psd_bins_vname, 'VAR_TYPE',      'support_data'
	
;------------------------------------------------------
; Data Attributes                                     |
;------------------------------------------------------

	;AMPLITUDE HISTOGRAM
	ofsm -> WriteVarAttr, amp_hist_vname, 'CATDESC',         'Histogrammemd magnetic field phase.'
	ofsm -> WriteVarAttr, amp_hist_vname, 'DEPEND_0',        f_vname
	ofsm -> WriteVarAttr, amp_hist_vname, 'DEPEND_1',        amp_bins_vname
	ofsm -> WriteVarAttr, amp_hist_vname, 'DEPEND_2',        comp_index_vname
	ofsm -> WriteVarAttr, amp_hist_vname, 'DEPEND_3',        flag_vname
	ofsm -> WriteVarAttr, amp_hist_vname, 'DISPLAY_TYPE',    'spectrogram'
	ofsm -> WriteVarAttr, amp_hist_vname, 'FIELDNAM',        'Amplitude'
	ofsm -> WriteVarAttr, amp_hist_vname, 'FILLVAL',         4294967295UL
	ofsm -> WriteVarAttr, amp_hist_vname, 'FORMAT',          'E12.5'
	ofsm -> WriteVarAttr, amp_hist_vname, 'LABLAXIS',        'Amp'
	ofsm -> WriteVarAttr, amp_hist_vname, 'LABL_PTR_1',      amp_labl_vname
	ofsm -> WriteVarAttr, amp_hist_vname, 'LABL_PTR_2',      comp_labl_vname
	ofsm -> WriteVarAttr, amp_hist_vname, 'LABL_PTR_3',      flag_labl_vname
	ofsm -> WriteVarAttr, amp_hist_vname, 'SCALETYP',        'log'
	ofsm -> WriteVarAttr, amp_hist_vname, 'SI_CONVERSION',   ' '
	ofsm -> WriteVarAttr, amp_hist_vname, 'UNITS',           'Occurrence'
	ofsm -> WriteVarAttr, amp_hist_vname, 'VALIDMIN',        0UL
	ofsm -> WriteVarAttr, amp_hist_vname, 'VALIDMAX',        4294967294UL
	ofsm -> WriteVarAttr, amp_hist_vname, 'VAR_TYPE',        'data'

	;PHASE HISTOGRAM
	ofsm -> WriteVarAttr, phase_hist_vname, 'CATDESC',         'Histogrammemd magnetic field phase.'
	ofsm -> WriteVarAttr, phase_hist_vname, 'DEPEND_0',        f_vname
	ofsm -> WriteVarAttr, phase_hist_vname, 'DEPEND_1',        phase_bins_vname
	ofsm -> WriteVarAttr, phase_hist_vname, 'DEPEND_2',        comp_index_vname
	ofsm -> WriteVarAttr, phase_hist_vname, 'DEPEND_3',        flag_vname
	ofsm -> WriteVarAttr, phase_hist_vname, 'DISPLAY_TYPE',    'spectrogram'
	ofsm -> WriteVarAttr, phase_hist_vname, 'FIELDNAM',        'Phase'
	ofsm -> WriteVarAttr, phase_hist_vname, 'FILLVAL',         4294967295UL
	ofsm -> WriteVarAttr, phase_hist_vname, 'FORMAT',          'E12.5'
	ofsm -> WriteVarAttr, phase_hist_vname, 'LABLAXIS',        'Flux'
	ofsm -> WriteVarAttr, phase_hist_vname, 'LABL_PTR_1',      phase_labl_vname
	ofsm -> WriteVarAttr, phase_hist_vname, 'LABL_PTR_2',      comp_labl_vname
	ofsm -> WriteVarAttr, phase_hist_vname, 'LABL_PTR_3',      flag_labl_vname
	ofsm -> WriteVarAttr, phase_hist_vname, 'SCALETYP',        'log'
	ofsm -> WriteVarAttr, phase_hist_vname, 'SI_CONVERSION',   ' '
	ofsm -> WriteVarAttr, phase_hist_vname, 'UNITS',           'Occurrence'
	ofsm -> WriteVarAttr, phase_hist_vname, 'VALIDMIN',        0UL
	ofsm -> WriteVarAttr, phase_hist_vname, 'VALIDMAX',        4294967294UL
	ofsm -> WriteVarAttr, phase_hist_vname, 'VAR_TYPE',        'data'

	;PSD HISTOGRAM
	ofsm -> WriteVarAttr, psd_hist_vname, 'CATDESC',         'Histogrammemd magnetic field power spectral density.'
	ofsm -> WriteVarAttr, psd_hist_vname, 'DEPEND_0',        f_vname
	ofsm -> WriteVarAttr, psd_hist_vname, 'DEPEND_1',        psd_bins_vname
	ofsm -> WriteVarAttr, psd_hist_vname, 'DEPEND_2',        comp_index_vname
	ofsm -> WriteVarAttr, psd_hist_vname, 'DEPEND_3',        flag_vname
	ofsm -> WriteVarAttr, psd_hist_vname, 'DISPLAY_TYPE',    'spectrogram'
	ofsm -> WriteVarAttr, psd_hist_vname, 'FIELDNAM',        'Power spectral density occurrence'
	ofsm -> WriteVarAttr, psd_hist_vname, 'FILLVAL',         4294967295UL
	ofsm -> WriteVarAttr, psd_hist_vname, 'FORMAT',          'E12.5'
	ofsm -> WriteVarAttr, psd_hist_vname, 'LABLAXIS',        'PSD'
	ofsm -> WriteVarAttr, psd_hist_vname, 'LABL_PTR_1',      psd_labl_vname
	ofsm -> WriteVarAttr, psd_hist_vname, 'LABL_PTR_2',      comp_labl_vname
	ofsm -> WriteVarAttr, psd_hist_vname, 'LABL_PTR_3',      flag_labl_vname
	ofsm -> WriteVarAttr, psd_hist_vname, 'SCALETYP',        'log'
	ofsm -> WriteVarAttr, psd_hist_vname, 'SI_CONVERSION',   ' '
	ofsm -> WriteVarAttr, psd_hist_vname, 'UNITS',           'Occurrence'
	ofsm -> WriteVarAttr, psd_hist_vname, 'VALIDMIN',        0UL
	ofsm -> WriteVarAttr, psd_hist_vname, 'VALIDMAX',        4294967294UL
	ofsm -> WriteVarAttr, psd_hist_vname, 'VAR_TYPE',        'data'

	;AMPLITUDE FLOOR
	ofsm -> WriteVarAttr, amp_floor_vname, 'CATDESC',         'Magnetic field amplitude noise floor.'
	ofsm -> WriteVarAttr, amp_floor_vname, 'DEPEND_0',        f_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'DEPEND_1',        comp_index_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'DEPEND_2',        flag_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'DISPLAY_TYPE',    'time_series'
	ofsm -> WriteVarAttr, amp_floor_vname, 'FIELDNAM',        'Amplitude noise Floor'
	ofsm -> WriteVarAttr, amp_floor_vname, 'FILLVAL',         -1e31
	ofsm -> WriteVarAttr, amp_floor_vname, 'FORMAT',          'F9.4'
	ofsm -> WriteVarAttr, amp_floor_vname, 'LABLAXIS',        'Amp'
	ofsm -> WriteVarAttr, amp_floor_vname, 'LABL_PTR_1',      comp_labl_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'LABL_PTR_2',      flag_labl_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'SCALETYP',        'linear'
	ofsm -> WriteVarAttr, amp_floor_vname, 'SI_CONVERSION',   '10^(Amp)*1e-9>T'
	ofsm -> WriteVarAttr, amp_floor_vname, 'UNITS',           'alog10(nT)'
	ofsm -> WriteVarAttr, amp_floor_vname, 'VALIDMIN',        0.0
	ofsm -> WriteVarAttr, amp_floor_vname, 'VALIDMAX',        1e3
	ofsm -> WriteVarAttr, amp_floor_vname, 'VAR_TYPE',        'data'

	;PHASE FLOOR
	ofsm -> WriteVarAttr, amp_floor_vname, 'CATDESC',         'Magnetic field phase noise floor.'
	ofsm -> WriteVarAttr, amp_floor_vname, 'DEPEND_0',        f_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'DEPEND_1',        comp_index_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'DEPEND_2',        flag_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'DISPLAY_TYPE',    'time_series'
	ofsm -> WriteVarAttr, amp_floor_vname, 'FIELDNAM',        'Phase noise Floor'
	ofsm -> WriteVarAttr, amp_floor_vname, 'FILLVAL',         -1e31
	ofsm -> WriteVarAttr, amp_floor_vname, 'FORMAT',          'F9.4'
	ofsm -> WriteVarAttr, amp_floor_vname, 'LABLAXIS',        'Amp'
	ofsm -> WriteVarAttr, amp_floor_vname, 'LABL_PTR_1',      comp_labl_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'LABL_PTR_2',      flag_labl_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'SCALETYP',        'linear'
	ofsm -> WriteVarAttr, amp_floor_vname, 'SI_CONVERSION',   ' '
	ofsm -> WriteVarAttr, amp_floor_vname, 'UNITS',           'degrees'
	ofsm -> WriteVarAttr, amp_floor_vname, 'VALIDMIN',        0.0
	ofsm -> WriteVarAttr, amp_floor_vname, 'VALIDMAX',        1e3
	ofsm -> WriteVarAttr, amp_floor_vname, 'VAR_TYPE',        'data'

	;PSD FLOOR
	ofsm -> WriteVarAttr, amp_floor_vname, 'CATDESC',         'Magnetic field power spectral density noise floor.'
	ofsm -> WriteVarAttr, amp_floor_vname, 'DEPEND_0',        f_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'DEPEND_1',        comp_index_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'DEPEND_2',        flag_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'DISPLAY_TYPE',    'time_series'
	ofsm -> WriteVarAttr, amp_floor_vname, 'FIELDNAM',        'Power spectral density noise Floor'
	ofsm -> WriteVarAttr, amp_floor_vname, 'FILLVAL',         -1e31
	ofsm -> WriteVarAttr, amp_floor_vname, 'FORMAT',          'F9.4'
	ofsm -> WriteVarAttr, amp_floor_vname, 'LABLAXIS',        'Amp'
	ofsm -> WriteVarAttr, amp_floor_vname, 'LABL_PTR_1',      comp_labl_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'LABL_PTR_2',      flag_labl_vname
	ofsm -> WriteVarAttr, amp_floor_vname, 'SCALETYP',        'linear'
	ofsm -> WriteVarAttr, amp_floor_vname, 'SI_CONVERSION',   '10^(PSD)*1e-18>T^2/Hz'
	ofsm -> WriteVarAttr, amp_floor_vname, 'UNITS',           'alog10(nT^2/Hz)'
	ofsm -> WriteVarAttr, amp_floor_vname, 'VALIDMIN',        0.0
	ofsm -> WriteVarAttr, amp_floor_vname, 'VALIDMAX',        1e3
	ofsm -> WriteVarAttr, amp_floor_vname, 'VAR_TYPE',        'data'
	
;------------------------------------------------------
; Metadata Attributes                                 |
;------------------------------------------------------

	;FREQUENCY LABELS
	ofsm -> WriteVarAttr, f_labl_vname, 'CATDESC',         'Frequency labels'
	ofsm -> WriteVarAttr, f_labl_vname, 'FIELDNAM',        'Frequency lables'
	ofsm -> WriteVarAttr, f_labl_vname, 'FORMAT',          'A6'
	ofsm -> WriteVarAttr, f_labl_vname, 'VAR_TYPE',        'metadata'

	;COMPONENT LABELS
	ofsm -> WriteVarAttr, comp_labl_vname, 'CATDESC',         'Component labels'
	ofsm -> WriteVarAttr, comp_labl_vname, 'FIELDNAM',        'Component lables'
	ofsm -> WriteVarAttr, comp_labl_vname, 'FORMAT',          'A1'
	ofsm -> WriteVarAttr, comp_labl_vname, 'VAR_TYPE',        'metadata'

	;HISTOGRAM FLAG LABELS
	ofsm -> WriteVarAttr, flag_labl_vname, 'CATDESC',         'Component labels'
	ofsm -> WriteVarAttr, flag_labl_vname, 'FIELDNAM',        'Component lables'
	ofsm -> WriteVarAttr, flag_labl_vname, 'FORMAT',          'A1'
	ofsm -> WriteVarAttr, flag_labl_vname, 'VAR_TYPE',        'metadata'

	;AMPLITUDE LABELS
	ofsm -> WriteVarAttr, amp_labl_vname, 'CATDESC',         'Amplitude labels'
	ofsm -> WriteVarAttr, amp_labl_vname, 'FIELDNAM',        'Amplitude lables'
	ofsm -> WriteVarAttr, amp_labl_vname, 'FORMAT',          'A1'
	ofsm -> WriteVarAttr, amp_labl_vname, 'VAR_TYPE',        'metadata'

	;PHASE LABELS
	ofsm -> WriteVarAttr, phase_labl_vname, 'CATDESC',         'Phase labels'
	ofsm -> WriteVarAttr, phase_labl_vname, 'FIELDNAM',        'Phase lables'
	ofsm -> WriteVarAttr, phase_labl_vname, 'FORMAT',          'A1'
	ofsm -> WriteVarAttr, phase_labl_vname, 'VAR_TYPE',        'metadata'

	;PSD LABELS
	ofsm -> WriteVarAttr, psd_labl_vname, 'CATDESC',         'Power spectral density labels'
	ofsm -> WriteVarAttr, psd_labl_vname, 'FIELDNAM',        'PSD labels'
	ofsm -> WriteVarAttr, psd_labl_vname, 'FORMAT',          'A1'
	ofsm -> WriteVarAttr, psd_labl_vname, 'VAR_TYPE',        'metadata'

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, ofsm
	return, fsm_file
end