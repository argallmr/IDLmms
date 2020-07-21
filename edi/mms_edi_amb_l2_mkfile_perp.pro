; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_l2_mkfile_perp
;
; PURPOSE:
;+
;   Make a CDF file for perpendicular EDI ambient-mode data.
;
; :Categories:
;    MMS, EDI, L2, Ambient
;
; :Params:
;       SC:                 in, required, type=string
;                           Spacecraft ID: 'mms1', 'mms2', 'mms3', or 'mms4'
;       MODE:               in, required, type=string
;                           Data rate mode: 'slow', 'fast', 'srvy', or 'brst'
;       OPTDESC:            in, optional, type=string, default='amb-perp'
;                           Optional filename descriptor, with parts separated by a hyphen.
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
;       2019/03/15  -   Written by Matthew Argall
;       2019/07/10  -   Each trajectory has its own labl_ptr_1 variable and Flux and dFlux
;                           variables have more specific lablaxis values, as per SPDF. - MRA
;-
function mms_edi_amb_l2_mkfile_perp, sc, mode, optdesc, tstart, amb_data, $
BRST=brst, $
DROPBOX_ROOT=dropbox, $
DATA_PATH_ROOT=data_path, $
EMPTY_FILE=empty_file, $
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
	
	;Defaults
	status  = 0
	tf_brst = keyword_set(brst)

;------------------------------------;
; Version History                    ;
;------------------------------------;
	
	;PERP-C
	if stregex(optdesc, 'amb-perp-c', /BOOLEAN) then begin
		;BRST
		if tf_brst then begin
			mods = [ 'v1.0.0 - Original version. ' ]
		
		;SRVY
		endif else begin
			mods = [ 'v1.0.0 - Original version.' ]
		endelse
	
	;PERP-OM
	endif else if stregex(optdesc, 'amb-perp-om', /BOOLEAN) then begin
		;Mods to data processing
		if tf_brst then begin
			mods = [ 'v1.0.0 - Original version. ' ]
		endif else begin
			mods = [ 'v1.0.0 - Original version.' ]
		endelse
	
	;PERP-OB
	endif else if stregex(optdesc, 'amb-perp-ob', /BOOLEAN) then begin
		;Mods to data processing
		if tf_brst then begin
			mods = [ 'v1.0.0 - Original version. ' ]
		endif else begin
			mods = [ 'v1.0.0 - Original version.' ]
		endelse
	
	;UNKNOWN
	endif else begin
		mods = ' '
	endelse
		
	;Get the version
	version = stregex(mods[-1], '^v([0-9]+)\.([0-9]+)\.([0-9]+)', /SUBEXP, /EXTRACT)
	vx      = strtrim(version[1], 2)
	vy      = strtrim(version[2], 2)
	vz      = strtrim(version[3], 2)
	
	;Constants for output file
	instr   = 'edi'
	level   = 'l2'

;------------------------------------;
; Check Inputs                       ;
;------------------------------------;
	
	;Defaults
	tf_empty = keyword_set(empty_file)
	if n_elements(sc)      eq 0 || sc      eq '' then sc      = 'mms#'
	if n_elements(mode)    eq 0 || mode    eq '' then mode    = 'mode'
	if n_elements(optdesc) eq 0                  then optdesc = 'amb-perp'
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
		if n_elements(dropbox)   eq 0 then dropbox   = !edi_init.dropbox_root
		if n_elements(data_path) eq 0 then data_path = !edi_init.data_path_root
	endif else begin
		if n_elements(dropbox)   eq 0 then cd, CURRENT=dropbox
		if n_elements(data_path) eq 0 then cd, CURRENT=data_path
	endelse

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

;------------------------------------;
; Errors                             ;
;------------------------------------;
	
	;Delta +/- for time
	;   - We accumulate over half the sampling time in srvy mode.
	if mode eq 'brst' $
		then t_delta = long64(1D/1024D/2D * 1d9) $
		else t_delta = long64(16D/1024D/2D * 1d9)
	
	;
	;Trajectory error
	;
	
	;One-Sided
	if optdesc eq 'amb-pm2' then begin
		traj_delta = 9.0
	
	;Centered (brst mode uses 1 anode while survey uses 2)
	endif else begin
		if mode eq 'brst' $
			then traj_delta = 9.0 $
			else traj_delta = 13.0
	endelse

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
	oamb -> WriteGlobalAttr, /CREATE, 'Logical_source_description', 'Level 2 EDI Ambient electron flux'
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
; Variable Names                                      |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName[_coordSys][_paramQualifier][_subModeLevel][_mode][_level]
	prefix  = strjoin([sc, instr], '_') + '_'
	suffix  = '_' + strjoin([mode, level], '_')
	
	t_vname                   = 'Epoch'
	t_tt_vname                = 'epoch_timetag'
	optics_vname              = prefix + 'optics_state'       + suffix
	e_gdu1_vname              = prefix + 'energy_gdu1'        + suffix
	e_gdu2_vname              = prefix + 'energy_gdu2'        + suffix
	flip_90_vname             = prefix + 'flip_90'            + suffix
	flux1_90_gdu1_vname       = prefix + 'flux1_90_gdu1'      + suffix
	flux2_90_gdu1_vname       = prefix + 'flux2_90_gdu1'      + suffix
	flux3_90_gdu1_vname       = prefix + 'flux3_90_gdu1'      + suffix
	flux4_90_gdu1_vname       = prefix + 'flux4_90_gdu1'      + suffix
	flux1_90_gdu2_vname       = prefix + 'flux1_90_gdu2'      + suffix
	flux2_90_gdu2_vname       = prefix + 'flux2_90_gdu2'      + suffix
	flux3_90_gdu2_vname       = prefix + 'flux3_90_gdu2'      + suffix
	flux4_90_gdu2_vname       = prefix + 'flux4_90_gdu2'      + suffix
	traj1_dbcs_90_gdu1_vname  = prefix + 'traj1_dbcs_90_gdu1' + suffix
	traj2_dbcs_90_gdu1_vname  = prefix + 'traj2_dbcs_90_gdu1' + suffix
	traj3_dbcs_90_gdu1_vname  = prefix + 'traj3_dbcs_90_gdu1' + suffix
	traj4_dbcs_90_gdu1_vname  = prefix + 'traj4_dbcs_90_gdu1' + suffix
	traj1_dbcs_90_gdu2_vname  = prefix + 'traj1_dbcs_90_gdu2' + suffix
	traj2_dbcs_90_gdu2_vname  = prefix + 'traj2_dbcs_90_gdu2' + suffix
	traj3_dbcs_90_gdu2_vname  = prefix + 'traj3_dbcs_90_gdu2' + suffix
	traj4_dbcs_90_gdu2_vname  = prefix + 'traj4_dbcs_90_gdu2' + suffix
	traj1_gse_90_gdu1_vname   = prefix + 'traj1_gse_90_gdu1'  + suffix
	traj2_gse_90_gdu1_vname   = prefix + 'traj2_gse_90_gdu1'  + suffix
	traj3_gse_90_gdu1_vname   = prefix + 'traj3_gse_90_gdu1'  + suffix
	traj4_gse_90_gdu1_vname   = prefix + 'traj4_gse_90_gdu1'  + suffix
	traj1_gse_90_gdu2_vname   = prefix + 'traj1_gse_90_gdu2'  + suffix
	traj2_gse_90_gdu2_vname   = prefix + 'traj2_gse_90_gdu2'  + suffix
	traj3_gse_90_gdu2_vname   = prefix + 'traj3_gse_90_gdu2'  + suffix
	traj4_gse_90_gdu2_vname   = prefix + 'traj4_gse_90_gdu2'  + suffix
;	traj1_gsm_0_vname         = prefix + 'traj1_gsm_0'        + suffix
;	traj2_gsm_0_vname         = prefix + 'traj2_gsm_0'        + suffix
;	traj3_gsm_0_vname         = prefix + 'traj3_gsm_0'        + suffix
;	traj4_gsm_0_vname         = prefix + 'traj4_gsm_0'        + suffix
;	traj1_gsm_180_vname       = prefix + 'traj1_gsm_180'      + suffix
;	traj2_gsm_180_vname       = prefix + 'traj2_gsm_180'      + suffix
;	traj3_gsm_180_vname       = prefix + 'traj3_gsm_180'      + suffix
;	traj4_gsm_180_vname       = prefix + 'traj4_gsm_180'      + suffix

	delta1_90_gdu1_vname = prefix + 'flux1_90_delta_gdu1' + suffix
	delta2_90_gdu1_vname = prefix + 'flux2_90_delta_gdu1' + suffix
	delta3_90_gdu1_vname = prefix + 'flux3_90_delta_gdu1' + suffix
	delta4_90_gdu1_vname = prefix + 'flux4_90_delta_gdu1' + suffix
	delta1_90_gdu2_vname = prefix + 'flux1_90_delta_gdu2' + suffix
	delta2_90_gdu2_vname = prefix + 'flux2_90_delta_gdu2' + suffix
	delta3_90_gdu2_vname = prefix + 'flux3_90_delta_gdu2' + suffix
	delta4_90_gdu2_vname = prefix + 'flux4_90_delta_gdu2' + suffix
	
	traj1_dbcs_90_gdu1_labl_vname = prefix + 'traj1_dbcs_90_gdu1_labl' + suffix
	traj2_dbcs_90_gdu1_labl_vname = prefix + 'traj2_dbcs_90_gdu1_labl' + suffix
	traj3_dbcs_90_gdu1_labl_vname = prefix + 'traj3_dbcs_90_gdu1_labl' + suffix
	traj4_dbcs_90_gdu1_labl_vname = prefix + 'traj4_dbcs_90_gdu1_labl' + suffix
	traj1_dbcs_90_gdu2_labl_vname = prefix + 'traj1_dbcs_90_gdu2_labl' + suffix
	traj2_dbcs_90_gdu2_labl_vname = prefix + 'traj2_dbcs_90_gdu2_labl' + suffix
	traj3_dbcs_90_gdu2_labl_vname = prefix + 'traj3_dbcs_90_gdu2_labl' + suffix
	traj4_dbcs_90_gdu2_labl_vname = prefix + 'traj4_dbcs_90_gdu2_labl' + suffix
	
	traj1_gse_90_gdu1_labl_vname = prefix + 'traj1_gse_90_gdu1_labl' + suffix
	traj2_gse_90_gdu1_labl_vname = prefix + 'traj2_gse_90_gdu1_labl' + suffix
	traj3_gse_90_gdu1_labl_vname = prefix + 'traj3_gse_90_gdu1_labl' + suffix
	traj4_gse_90_gdu1_labl_vname = prefix + 'traj4_gse_90_gdu1_labl' + suffix
	traj1_gse_90_gdu2_labl_vname = prefix + 'traj1_gse_90_gdu2_labl' + suffix
	traj2_gse_90_gdu2_labl_vname = prefix + 'traj2_gse_90_gdu2_labl' + suffix
	traj3_gse_90_gdu2_labl_vname = prefix + 'traj3_gse_90_gdu2_labl' + suffix
	traj4_gse_90_gdu2_labl_vname = prefix + 'traj4_gse_90_gdu2_labl' + suffix

;------------------------------------------------------
; Create Variables                                    |
;------------------------------------------------------

	;Write variable data to file
	oamb -> CreateVar, t_vname,       'CDF_TIME_TT2000'
	oamb -> CreateVar, t_tt_vname,    'CDF_TIME_TT2000'
	oamb -> CreateVar, optics_vname,  'CDF_UINT1', COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> CreateVar, e_gdu1_vname,  'CDF_UINT2', COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> CreateVar, e_gdu2_vname,  'CDF_UINT2', COMPRESSION='GZIP', GZIP_LEVEL=6
;	oamb -> CreateVar, flip_90_vname, 'CDF_UINT2', COMPRESSION='GZIP', GZIP_LEVEL=6
	
	;Group counts by pitch angle
	if mode eq 'brst' then begin
		;
		;Flux
		;
		
		oamb -> CreateVar, flux1_90_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, flux2_90_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, flux3_90_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, flux4_90_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, flux1_90_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, flux2_90_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, flux3_90_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, flux4_90_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		
		;
		;Errors
		;
		
		;90
		oamb -> CreateVar, delta1_90_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, delta2_90_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, delta3_90_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, delta4_90_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, delta1_90_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, delta2_90_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, delta3_90_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, delta4_90_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		
	endif else begin
		;Flux
		oamb -> CreateVar, flux1_90_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, flux1_90_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		
		;Errors
		oamb -> CreateVar, delta1_90_gdu1_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, delta1_90_gdu2_vname, 'CDF_FLOAT', COMPRESSION='GZIP', GZIP_LEVEL=6
	endelse

	;BRST
	if mode eq 'brst' then begin
		
		;DBCS Trajectories
		oamb -> CreateVar, traj1_dbcs_90_gdu1_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj2_dbcs_90_gdu1_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj3_dbcs_90_gdu1_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj4_dbcs_90_gdu1_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj1_dbcs_90_gdu2_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj2_dbcs_90_gdu2_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj3_dbcs_90_gdu2_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj4_dbcs_90_gdu2_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		
		;GSE Trajectories
		oamb -> CreateVar, traj1_gse_90_gdu1_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj2_gse_90_gdu1_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj3_gse_90_gdu1_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj4_gse_90_gdu1_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj1_gse_90_gdu2_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj2_gse_90_gdu2_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj3_gse_90_gdu2_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj4_gse_90_gdu2_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
	
	;'SRVY'
	endif else begin
		;DBCS
		oamb -> CreateVar, traj1_dbcs_90_gdu1_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj1_dbcs_90_gdu2_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		
		;GSE
		oamb -> CreateVar, traj1_gse_90_gdu1_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> CreateVar, traj1_gse_90_gdu2_vname, 'CDF_FLOAT', 1, DIMENSIONS=2, COMPRESSION='GZIP', GZIP_LEVEL=6
	endelse
	
	;Metadata
	
	if mode eq 'brst' then begin
		oamb -> WriteVar, /CREATE, traj1_dbcs_90_gdu1_labl_vname, [ 'Phi',  'Theta'] + ' (Ch1 DBCS PA90 GDU1)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj2_dbcs_90_gdu1_labl_vname, [ 'Phi',  'Theta'] + ' (Ch2 DBCS PA90 GDU1)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj3_dbcs_90_gdu1_labl_vname, [ 'Phi',  'Theta'] + ' (Ch3 DBCS PA90 GDU1)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj4_dbcs_90_gdu1_labl_vname, [ 'Phi',  'Theta'] + ' (Ch4 DBCS PA90 GDU1)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj1_dbcs_90_gdu2_labl_vname, [ 'Phi',  'Theta'] + ' (Ch1 DBCS PA90 GDU2)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj2_dbcs_90_gdu2_labl_vname, [ 'Phi',  'Theta'] + ' (Ch2 DBCS PA90 GDU2)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj3_dbcs_90_gdu2_labl_vname, [ 'Phi',  'Theta'] + ' (Ch3 DBCS PA90 GDU2)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj4_dbcs_90_gdu2_labl_vname, [ 'Phi',  'Theta'] + ' (Ch4 DBCS PA90 GDU2)', /REC_NOVARY
	
		oamb -> WriteVar, /CREATE, traj1_gse_90_gdu1_labl_vname, [ 'Phi',  'Theta'] + ' (Ch1 GSE PA90 GDU1)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj2_gse_90_gdu1_labl_vname, [ 'Phi',  'Theta'] + ' (Ch2 GSE PA90 GDU1)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj3_gse_90_gdu1_labl_vname, [ 'Phi',  'Theta'] + ' (Ch3 GSE PA90 GDU1)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj4_gse_90_gdu1_labl_vname, [ 'Phi',  'Theta'] + ' (Ch4 GSE PA90 GDU1)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj1_gse_90_gdu2_labl_vname, [ 'Phi',  'Theta'] + ' (Ch1 GSE PA90 GDU2)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj2_gse_90_gdu2_labl_vname, [ 'Phi',  'Theta'] + ' (Ch2 GSE PA90 GDU2)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj3_gse_90_gdu2_labl_vname, [ 'Phi',  'Theta'] + ' (Ch3 GSE PA90 GDU2)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj4_gse_90_gdu2_labl_vname, [ 'Phi',  'Theta'] + ' (Ch4 GSE PA90 GDU2)', /REC_NOVARY
	endif else begin
		oamb -> WriteVar, /CREATE, traj1_dbcs_90_gdu1_labl_vname, [ 'Phi',  'Theta'] + ' (Ch1 DBCS PA90 GDU1)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj1_dbcs_90_gdu2_labl_vname, [ 'Phi',  'Theta'] + ' (Ch1 DBCS PA90 GDU2)', /REC_NOVARY
	
		oamb -> WriteVar, /CREATE, traj1_gse_90_gdu1_labl_vname, [ 'Phi',  'Theta'] + ' (Ch1 GSE PA90 GDU1)', /REC_NOVARY
		oamb -> WriteVar, /CREATE, traj1_gse_90_gdu2_labl_vname, [ 'Phi',  'Theta'] + ' (Ch1 GSE PA90 GDU2)', /REC_NOVARY
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
		oamb -> WriteVar, t_vname, epoch_val
	endif
	
;------------------------------------------------------
; Variable Attributes                                 |
;------------------------------------------------------
	;Create the variable attributes
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'CATDESC'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'DELTA_PLUS'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'DELTA_PLUS_VAR'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'DELTA_MINUS'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'DELTA_MINUS_VAR'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'DEPEND_0'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'DISPLAY_TYPE'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'FIELDNAM'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'FILLVAL'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'FORMAT'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'LABLAXIS'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'LABL_PTR_1'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'SCALETYP'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'SI_CONVERSION'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'TIME_BASE'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'UNITS'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMIN'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'VALIDMAX'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'VAR_NOTES'
	oamb -> CreateAttr, /VARIABLE_SCOPE, 'VAR_TYPE'
	
;------------------------------------------------------
; Support Data                                        |
;------------------------------------------------------
	
	;TT2000
	oamb -> WriteVarAttr, t_vname, 'CATDESC',       'TT2000 time tags for perpendicular electron fluxes and trajectories.'
	oamb -> WriteVarAttr, t_vname, 'DELTA_MINUS',    t_delta
	oamb -> WriteVarAttr, t_vname, 'DELTA_PLUS',     t_delta
	oamb -> WriteVarAttr, t_vname, 'FIELDNAM',      'Time'
	oamb -> WriteVarAttr, t_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oamb -> WriteVarAttr, t_vname, 'FORMAT',        'I16'
	oamb -> WriteVarAttr, t_vname, 'LABLAXIS',      'UT'
	oamb -> WriteVarAttr, t_vname, 'SI_CONVERSION', '1e-9>s'
	oamb -> WriteVarAttr, t_vname, 'TIME_BASE',     'J2000'
	oamb -> WriteVarAttr, t_vname, 'UNITS',         'ns'
	oamb -> WriteVarAttr, t_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	oamb -> WriteVarAttr, t_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2065, 12, 31), /CDF_EPOCH
	oamb -> WriteVarAttr, t_vname, 'VAR_TYPE',      'support_data'

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
	oamb -> WriteVarAttr, t_tt_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2065, 12, 31), /CDF_EPOCH
	oamb -> WriteVarAttr, t_tt_vname, 'VAR_TYPE',      'support_data'

	;OPTICS
	oamb -> WriteVarAttr, optics_vname, 'CATDESC',       'Optics state'
	oamb -> WriteVarAttr, optics_vname, 'DEPEND_0',       t_tt_vname
	oamb -> WriteVarAttr, optics_vname, 'FIELDNAM',      'Optics state'
	oamb -> WriteVarAttr, optics_vname, 'FILLVAL',        255B
	oamb -> WriteVarAttr, optics_vname, 'FORMAT',        'I4'
	oamb -> WriteVarAttr, optics_vname, 'LABLAXIS',      'Optics'
	oamb -> WriteVarAttr, optics_vname, 'VALIDMIN',      0B
	oamb -> WriteVarAttr, optics_vname, 'VALIDMAX',      254B
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

	;FLIP_90
;	oamb -> WriteVarAttr, flip_90_vname, 'CATDESC',      'Indicator for rapid detector look direction changes'
;	oamb -> WriteVarAttr, flip_90_vname, 'DEPEND_0',     t_vname
;	oamb -> WriteVarAttr, flip_90_vname, 'FIELDNAM',     'Flip flag PA90'
;	oamb -> WriteVarAttr, flip_90_vname, 'FILLVAL',      255B
;	oamb -> WriteVarAttr, flip_90_vname, 'FORMAT',       'I1'
;	oamb -> WriteVarAttr, flip_90_vname, 'UNITS',        ' '
;	oamb -> WriteVarAttr, flip_90_vname, 'VALIDMIN',     0B
;	oamb -> WriteVarAttr, flip_90_vname, 'VALIDMAX',     254B
;	oamb -> WriteVarAttr, flip_90_vname, 'VAR_TYPE',     'support_data'
;	oamb -> WriteVarAttr, flip_90_vname, 'VAR_NOTES',    'Bits (i.e. 2^N) and definitions: GDU 1 & 2 roles ' + $
;	                                                     '0 = flip between 0 and 180; ' + $
;	                                                     '1 = flip between field-aligned to perpendicular.'
	
;------------------------------------------------------
; Electron Flux (Channel 1)                           |
;------------------------------------------------------
	;
	;Channel
	;   - In SRVY mode, only one channel is used, but it is not
	;     always channel 1. In this case, just say, 
	;     "electrons from each GDU" or "electrons from GDU[1,2]
	;   - In BRST mode, all channels are used, so say
	;     "electrons from channel of each GDU" or
	;     "electrons from channel # of GDU[1,2]" 
	;
	ch = mode eq 'brst' $
		? ' channel ' + ['1', '2', '3', '4'] + ' of' $
		: strarr(4)

	;FLUX1_90_GDU1
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'CATDESC',         'Perpendicular electron flux from' + ch[0] + ' GDU1.'
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'DEPEND_0',        t_vname
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'DELTA_MINUS_VAR', delta1_90_gdu1_vname
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'DELTA_PLUS_VAR',  delta1_90_gdu1_vname
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'FIELDNAM',        'Perpendicular electron flux GDU1'
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'FORMAT',          'E12.5'
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'LABLAXIS',        'Flux (Ch' + ch[0] + ' PA90 GDU1)'
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'SCALETYP',        'log'
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'UNITS',           'cm^-2 s^-1'
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'VALIDMIN',        0.0
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'VALIDMAX',        1e20
	oamb -> WriteVarAttr, flux1_90_gdu1_vname, 'VAR_TYPE',        'data'

	;FLUX1_90_GDU2
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'CATDESC',         'Perpendicular electron flux from' + ch[0] + ' GDU2.'
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'DEPEND_0',        t_vname
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'DELTA_MINUS_VAR', delta1_90_gdu2_vname
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'DELTA_PLUS_VAR',  delta1_90_gdu2_vname
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'FIELDNAM',        'Perpendicular electron flux GDU2'
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'FORMAT',          'E12.5'
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'LABLAXIS',        'Flux (Ch' + ch[0] + ' PA90 GDU2)'
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'SCALETYP',        'log'
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'UNITS',           'cm^-2 s^-1'
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'VALIDMIN',        0.0
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'VALIDMAX',        1e20
	oamb -> WriteVarAttr, flux1_90_gdu2_vname, 'VAR_TYPE',        'data'
	
;------------------------------------------------------
; Trajectories DBCS, GSE (Channel 1)                  |
;------------------------------------------------------

	;TRAJ1_DBCS_90_GDU1
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[0] + ' GDU1 in DBCS coordinates.'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'DEPEND_0',        t_vname
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'DELTA_MINUS',     traj_delta
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'DELTA_PLUS',      traj_delta
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'FIELDNAM',        'Trajectories for perpendicular electrons GDU1'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'FORMAT',          'F9.4'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'LABL_PTR_1',      traj1_dbcs_90_gdu1_labl_vname
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'SCALETYP',        'linear'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'SI_CONVERSION',   '0.01745>rad'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'UNITS',           'degrees'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'VALIDMIN',        -180.0
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'VALIDMAX',        180.0
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'VAR_TYPE',        'data'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
	                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
	                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
	                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
	                                                                   'details about errors, contact the EDI instrument team.'

	;TRAJ1_DBCS_90_GDU1
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[0] + ' GDU2 in DBCS coordinates.'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'DEPEND_0',        t_vname
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'DELTA_MINUS',     traj_delta
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'DELTA_PLUS',      traj_delta
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'FIELDNAM',        'Trajectories for perpendicular electrons GDU2'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'FORMAT',          'F9.4'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'LABL_PTR_1',      traj1_dbcs_90_gdu2_labl_vname
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'SCALETYP',        'linear'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'SI_CONVERSION',   '0.01745>rad'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'UNITS',           'degrees'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'VALIDMIN',        -180.0
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'VALIDMAX',        180.0
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'VAR_TYPE',        'data'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
	                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
	                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
	                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
	                                                                   'details about errors, contact the EDI instrument team.'

	;TRAJ1_GSE_90_GDU1
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[0] + ' GDU1 in GSE coordinates.'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'DEPEND_0',        t_vname
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'DELTA_MINUS',     traj_delta
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'DELTA_PLUS',      traj_delta
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'FIELDNAM',        'Trajectories for perpendicular electrons GDU1'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'FORMAT',          'F9.4'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'LABL_PTR_1',      traj1_gse_90_gdu1_labl_vname
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'SCALETYP',        'linear'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'SI_CONVERSION',   '0.01745>rad'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'UNITS',           'degrees'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'VALIDMIN',        -180.0
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'VALIDMAX',        180.0
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'VAR_TYPE',        'data'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
	                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
	                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
	                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
	                                                                   'details about errors, contact the EDI instrument team.'

	;TRAJ1_GSE_90_GDU1
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[0] + ' GDU2 in GSE coordinates.'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'DEPEND_0',        t_vname
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'DELTA_MINUS',     traj_delta
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'DELTA_PLUS',      traj_delta
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'FIELDNAM',        'Trajectories for perpendicular electrons GDU2'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'FORMAT',          'F9.4'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'LABL_PTR_1',      traj1_gse_90_gdu2_labl_vname
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'SCALETYP',        'linear'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'SI_CONVERSION',   '0.01745>rad'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'UNITS',           'degrees'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'VALIDMIN',        -180.0
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'VALIDMAX',        180.0
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'VAR_TYPE',        'data'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
	                                                                  '(theta) representing the azimuthal (polar) directions, in the ' + $
	                                                                  'indicated coordinate system. They are opposite to the nominal look-direction ' + $
	                                                                  "of the instrument. Errors represent an omni-directional error. For more " + $
	                                                                  'details about errors, contact the EDI instrument team.'

;------------------------------------------------------
; Errors (Channel 1)                                  |
;------------------------------------------------------

	;DELTA1_90_GDU1
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'CATDESC',       'Error in perpendicular electron flux from' + ch[0] + ' GDU1.'
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'DEPEND_0',      t_vname
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'FIELDNAM',      'Error in perpendicular electron flux GDU1'
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'FILLVAL',       -1e31
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'FORMAT',        'E12.5'
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'LABLAXIS',      'dFlux (Ch' + ch[0] + ' PA90 GDU1)'
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'SI_CONVERSION', '1e4>m^-2 s^-1'
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'UNITS',         'cm^-2 s^-1'
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'VALIDMIN',      0.0
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'VALIDMAX',      1e20
	oamb -> WriteVarAttr, delta1_90_gdu1_vname, 'VAR_TYPE',      'support_data'

	;DELTA1_90_GDU2
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'CATDESC',       'Error in perpendicular electron flux from' + ch[0] + ' GDU2.'
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'DEPEND_0',      t_vname
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'FIELDNAM',      'Error in perpendicular electron flux GDU2'
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'FILLVAL',       -1e31
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'FORMAT',        'E12.5'
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'LABLAXIS',      'dFlux (Ch' + ch[0] + ' PA90 GDU2)'
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'SI_CONVERSION', '1e4>m^-2 s^-1'
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'UNITS',         'cm^-2 s^-1'
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'VALIDMIN',      0.0
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'VALIDMAX',      1e20
	oamb -> WriteVarAttr, delta1_90_gdu2_vname, 'VAR_TYPE',      'support_data'
	
;------------------------------------------------------
; Channels 2-4                                        |
;------------------------------------------------------
	
	;BURST DATA
	if mode eq 'brst' then begin
	;------------------------------------------------------
	; Flux                                                |
	;------------------------------------------------------
		;FLUX2_90_GDU1
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'CATDESC',         'Perpendicular electron flux from' + ch[1] + ' GDU1.'
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'DELTA_MINUS_VAR', delta2_90_gdu1_vname
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'DELTA_PLUS_VAR',  delta2_90_gdu1_vname
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'FIELDNAM',        'Perpendicular electron flux GDU1'
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'LABLAXIS',        'Flux (Ch' + ch[1] + ' PA90 GDU1)'
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux2_90_gdu1_vname, 'VAR_TYPE',        'data'

		;FLUX3_90_GDU1
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'CATDESC',         'Perpendicular electron flux from' + ch[2] + ' GDU1.'
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'DELTA_MINUS_VAR', delta3_90_gdu1_vname
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'DELTA_PLUS_VAR',  delta3_90_gdu1_vname
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'FIELDNAM',        'Perpendicular electron flux GDU1'
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'LABLAXIS',        'Flux (Ch' + ch[2] + ' PA90 GDU1)'
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux3_90_gdu1_vname, 'VAR_TYPE',        'data'

		;FLUX4_90_GDU1
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'CATDESC',         'Perpendicular electron flux from' + ch[3] + ' GDU1.'
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'DELTA_MINUS_VAR', delta4_90_gdu1_vname
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'DELTA_PLUS_VAR',  delta4_90_gdu1_vname
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'FIELDNAM',        'Perpendicular electron flux GDU1'
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'LABLAXIS',        'Flux (Ch' + ch[3] + ' PA90 GDU1)'
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux4_90_gdu1_vname, 'VAR_TYPE',        'data'

		;FLUX2_90_GDU2
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'CATDESC',         'Perpendicular electron flux from' + ch[1] + ' GDU2.'
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'DELTA_MINUS_VAR', delta2_90_gdu2_vname
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'DELTA_PLUS_VAR',  delta2_90_gdu2_vname
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'FIELDNAM',        'Perpendicular electron flux GDU2'
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'LABLAXIS',        'Flux (Ch' + ch[1] + ' PA90 GDU2)'
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux2_90_gdu2_vname, 'VAR_TYPE',        'data'

		;FLUX3_90_GDU2
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'CATDESC',         'Perpendicular electron flux from' + ch[2] + ' GDU2.'
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'DELTA_MINUS_VAR', delta3_90_gdu2_vname
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'DELTA_PLUS_VAR',  delta3_90_gdu2_vname
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'FIELDNAM',        'Perpendicular electron flux GDU2'
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'LABLAXIS',        'Flux (Ch' + ch[2] + ' PA90 GDU2)'
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux3_90_gdu2_vname, 'VAR_TYPE',        'data'

		;FLUX4_90_GDU2
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'CATDESC',         'Perpendicular electron flux from' + ch[3] + ' GDU2.'
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'DELTA_MINUS_VAR', delta4_90_gdu2_vname
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'DELTA_PLUS_VAR',  delta4_90_gdu2_vname
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'FIELDNAM',        'Perpendicular electron flux GDU2'
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'LABLAXIS',        'Flux (Ch' + ch[3] + ' PA90 GDU2)'
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux4_90_gdu2_vname, 'VAR_TYPE',        'data'

	;------------------------------------------------------
	; Flux Error                                          |
	;------------------------------------------------------
		
		;DELTA2_90_GDU1
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'CATDESC',       'Error in perpendicular electron flux from' + ch[1] + ' GDU1.'
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'DEPEND_0',      t_vname
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'FIELDNAM',      'Error for perpendicular electron flux GDU1'
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'LABLAXIS',      'dFlux (Ch' + ch[1] + ' PA90 GDU1)'
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'SI_CONVERSION', '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'UNITS',         'cm^-2 s^-1'
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'VALIDMAX',      1e20
		oamb -> WriteVarAttr, delta2_90_gdu1_vname, 'VAR_TYPE',      'support_data'

		;DELTA3_90_GDU1
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'CATDESC',       'Error in perpendicular electron flux from' + ch[2] + ' GDU1.'
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'DEPEND_0',      t_vname
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'FIELDNAM',      'Error for perpendicular electron flux GDU1'
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'LABLAXIS',      'dFlux (Ch' + ch[2] + ' PA90 GDU1)'
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'SI_CONVERSION', '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'UNITS',         'cm^-2 s^-1'
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'VALIDMAX',      1e20
		oamb -> WriteVarAttr, delta3_90_gdu1_vname, 'VAR_TYPE',      'support_data'

		;DELTA4_90_GDU1
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'CATDESC',       'Error in perpendicular electron flux from' + ch[3] + ' GDU1.'
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'DEPEND_0',      t_vname
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'FIELDNAM',      'Error for perpendicular electron flux GDU1'
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'LABLAXIS',      'dFlux (Ch' + ch[3] + ' PA90 GDU1)'
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'SI_CONVERSION', '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'UNITS',         'cm^-2 s^-1'
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'VALIDMAX',      1e20
		oamb -> WriteVarAttr, delta4_90_gdu1_vname, 'VAR_TYPE',      'support_data'
		
		;DELTA2_90_GDU2
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'CATDESC',       'Error in perpendicular electron flux from' + ch[1] + ' GDU2.'
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'DEPEND_0',      t_vname
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'FIELDNAM',      'Error for perpendicular electron flux GDU2'
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'LABLAXIS',      'dFlux (Ch' + ch[1] + ' PA90 GDU2)'
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'SI_CONVERSION', '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'UNITS',         'cm^-2 s^-1'
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'VALIDMAX',      1e20
		oamb -> WriteVarAttr, delta2_90_gdu2_vname, 'VAR_TYPE',      'support_data'

		;DELTA3_90_GDU2
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'CATDESC',       'Error in perpendicular electron flux from' + ch[2] + ' GDU2.'
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'DEPEND_0',      t_vname
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'FIELDNAM',      'Error for perpendicular electron flux GDU2'
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'LABLAXIS',      'dFlux (Ch' + ch[2] + ' PA90 GDU2)'
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'SI_CONVERSION', '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'UNITS',         'cm^-2 s^-1'
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'VALIDMAX',      1e20
		oamb -> WriteVarAttr, delta3_90_gdu2_vname, 'VAR_TYPE',      'support_data'

		;DELTA4_90_GDU2
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'CATDESC',       'Error in perpendicular electron flux from' + ch[3] + ' GDU2.'
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'DEPEND_0',      t_vname
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'FIELDNAM',      'Error for perpendicular electron flux GDU2'
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'LABLAXIS',      'dFlux (Ch' + ch[3] + ' PA90 GDU2)'
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'SI_CONVERSION', '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'UNITS',         'cm^-2 s^-1'
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'VALIDMAX',      1e20
		oamb -> WriteVarAttr, delta4_90_gdu2_vname, 'VAR_TYPE',      'support_data'
	
	;------------------------------------------------------
	; Trajectories DBCS                                   |
	;------------------------------------------------------

		;TRAJ2_DBCS_90_GDU1
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[1] + ' GDU1 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU1'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'LABL_PTR_1',      traj2_dbcs_90_gdu1_labl_vname
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

		;TRAJ3_DBCS_90_GDU1
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[2] + ' GDU1 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU1'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'LABL_PTR_1',      traj3_dbcs_90_gdu1_labl_vname
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

		;TRAJ4_DBCS_90_GDU1
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[3] + ' GDU1 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU1'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'LABL_PTR_1',      traj4_dbcs_90_gdu1_labl_vname
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

		;TRAJ2_DBCS_90_GDU2
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[1] + ' GDU2 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU2'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'LABL_PTR_1',      traj2_dbcs_90_gdu2_labl_vname
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

		;TRAJ3_DBCS_90_GDU2
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[2] + ' GDU2 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU2'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'LABL_PTR_1',      traj3_dbcs_90_gdu2_labl_vname
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

		;TRAJ4_DBCS_90_GDU2
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[3] + ' GDU2 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU2'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'LABL_PTR_1',      traj4_dbcs_90_gdu2_labl_vname
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

	;------------------------------------------------------
	; Trajectories GSE                                    |
	;------------------------------------------------------
		
		;TRAJ2_GSE_90_GDU1
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[1] + ' GDU1 in GSE coordinates.'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU1'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'LABL_PTR_1',      traj2_gse_90_gdu1_labl_vname
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

		;TRAJ3_GSE_90_GDU1
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[2] + ' GDU1 in GSE coordinates.'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU1'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'LABL_PTR_1',      traj3_gse_90_gdu1_labl_vname
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

		;TRAJ4_GSE_90_GDU1
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[3] + ' GDU1 in GSE coordinates.'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU1'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'LABL_PTR_1',      traj4_gse_90_gdu1_labl_vname
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

		;TRAJ2_GSE_90_GDU2
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[1] + ' GDU2 in GSE coordinates.'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU2'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'LABL_PTR_1',      traj2_gse_90_gdu2_labl_vname
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

		;TRAJ3_GSE_90_GDU2
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[2] + ' GDU2 in GSE coordinates.'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU2'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'LABL_PTR_1',      traj3_gse_90_gdu2_labl_vname
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'

		;TRAJ4_GSE_90_GDU2
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'CATDESC',         'Trajectory of perpendicular electrons from' + ch[3] + ' GDU2 in GSE coordinates.'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'DEPEND_0',        t_vname
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'FIELDNAM',        'Trajectory of perpendicular electrons GDU2'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'LABL_PTR_1',      traj4_gse_90_gdu2_labl_vname
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'SI_CONVERSION',   '0.01745>rad'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                                   '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                                   'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                                   "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                                   'details about errors, contact the EDI instrument team.'
	endif
	
;------------------------------------------------------
; Metadata                                            |
;------------------------------------------------------

	;TRAJ1_DBCS_90_GDU1_LABL
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_labl_vname, 'CATDESC',  'Labels for trajectories from GDU1, channel 1 at pitch angle 90 in DBCS coordinates.'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_labl_vname, 'FIELDNAM', 'Traj labl (GDU1 Ch1 PA90 DBCS)'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_labl_vname, 'FORMAT',   'A5'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu1_labl_vname, 'VAR_TYPE', 'metadata'

	;TRAJ1_DBCS_90_GDU2_LABL
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_labl_vname, 'CATDESC',  'Labels for trajectories from GDU2, channel 1 at pitch angle 90 in DBCS coordinates.'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_labl_vname, 'FIELDNAM', 'Traj labl (GDU2 Ch1 PA90 DBCS)'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_labl_vname, 'FORMAT',   'A5'
	oamb -> WriteVarAttr, traj1_dbcs_90_gdu2_labl_vname, 'VAR_TYPE', 'metadata'

	;TRAJ1_GSE_90_GDU1_LABL
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_labl_vname, 'CATDESC',  'Labels for trajectories from GDU1, channel 1 at pitch angle 90 in GSE coordinates.'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_labl_vname, 'FIELDNAM', 'Traj labl (GDU1 Ch1 PA90 GSE)'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_labl_vname, 'FORMAT',   'A5'
	oamb -> WriteVarAttr, traj1_gse_90_gdu1_labl_vname, 'VAR_TYPE', 'metadata'

	;TRAJ1_GSE_90_GDU2_LABL
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_labl_vname, 'CATDESC',  'Labels for trajectories from GDU2, channel 1 at pitch angle 90 in GSE coordinates.'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_labl_vname, 'FIELDNAM', 'Traj labl (GDU2 Ch1 PA90 GSE)'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_labl_vname, 'FORMAT',   'A5'
	oamb -> WriteVarAttr, traj1_gse_90_gdu2_labl_vname, 'VAR_TYPE', 'metadata'

	if mode eq 'brst' then begin
		;TRAJ2_DBCS_90_GDU1_LABL
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_labl_vname, 'CATDESC',  'Labels for trajectories from GDU1, channel 2 at pitch angle 90 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_labl_vname, 'FIELDNAM', 'Traj labl (GDU1 Ch2 PA90 DBCS)'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu1_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ3_DBCS_90_GDU1_LABL
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_labl_vname, 'CATDESC',  'Labels for trajectories from GDU1, channel 3 at pitch angle 90 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_labl_vname, 'FIELDNAM', 'Traj labl (GDU1 Ch3 PA90 DBCS)'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu1_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ4_DBCS_90_GDU1_LABL
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_labl_vname, 'CATDESC',  'Labels for trajectories from GDU1, channel 4 at pitch angle 90 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_labl_vname, 'FIELDNAM', 'Traj labl (GDU1 Ch4 PA90 DBCS)'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu1_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ2_DBCS_90_GDU2_LABL
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_labl_vname, 'CATDESC',  'Labels for trajectories from GDU2, channel 2 at pitch angle 90 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_labl_vname, 'FIELDNAM', 'Traj labl (GDU2 Ch2 PA90 DBCS)'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj2_dbcs_90_gdu2_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ3_DBCS_90_GDU2_LABL
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_labl_vname, 'CATDESC',  'Labels for trajectories from GDU2, channel 3 at pitch angle 90 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_labl_vname, 'FIELDNAM', 'Traj labl (GDU2 Ch3 PA90 DBCS)'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj3_dbcs_90_gdu2_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ4_DBCS_90_GDU2_LABL
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_labl_vname, 'CATDESC',  'Labels for trajectories from GDU2, channel 4 at pitch angle 90 in DBCS coordinates.'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_labl_vname, 'FIELDNAM', 'Traj labl (GDU2 Ch4 PA90 DBCS)'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj4_dbcs_90_gdu2_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ2_GSE_90_GDU1_LABL
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_labl_vname, 'CATDESC',  'Labels for trajectories from GDU1, channel 2 at pitch angle 90 in GSE coordinates.'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_labl_vname, 'FIELDNAM', 'Trajectory labels'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_labl_vname, 'FORMAT',   'Traj labl (GDU1 Ch2 PA90 GSE)'
		oamb -> WriteVarAttr, traj2_gse_90_gdu1_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ3_GSE_90_GDU1_LABL
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_labl_vname, 'CATDESC',  'Labels for trajectories from GDU1, channel 3 at pitch angle 90 in GSE coordinates.'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_labl_vname, 'FIELDNAM', 'Traj labl (GDU1 Ch3 PA90 GSE)'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj3_gse_90_gdu1_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ4_GSE_90_GDU1_LABL
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_labl_vname, 'CATDESC',  'Labels for trajectories from GDU1, channel 4 at pitch angle 90 in GSE coordinates.'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_labl_vname, 'FIELDNAM', 'Traj labl (GDU1 Ch4 PA90 GSE)'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj4_gse_90_gdu1_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ2_GSE_90_GDU2_LABL
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_labl_vname, 'CATDESC',  'Labels for trajectories from GDU2, channel 2 at pitch angle 90 in GSE coordinates.'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_labl_vname, 'FIELDNAM', 'Traj labl (GDU2 Ch2 PA90 GSE)'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj2_gse_90_gdu2_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ3_GSE_90_GDU2_LABL
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_labl_vname, 'CATDESC',  'Labels for trajectories from GDU2, channel 3 at pitch angle 90 in GSE coordinates.'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_labl_vname, 'FIELDNAM', 'Traj labl (GDU2 Ch3 PA90 GSE)'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj3_gse_90_gdu2_labl_vname, 'VAR_TYPE', 'metadata'
		
		;TRAJ4_GSE_90_GDU2_LABL
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_labl_vname, 'CATDESC',  'Labels for trajectories from GDU2, channel 4 at pitch angle 90 in GSE coordinates.'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_labl_vname, 'FIELDNAM', 'Traj labl (GDU2 Ch4 PA90 GSE)'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_labl_vname, 'FORMAT',   'A5'
		oamb -> WriteVarAttr, traj4_gse_90_gdu2_labl_vname, 'VAR_TYPE', 'metadata'
	endif

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, oamb
	return, amb_file
end