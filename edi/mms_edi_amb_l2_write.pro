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
;       2015/10/26  -   Written by Matthew Argall
;       2015/01/16  -   Determine the correct output file version more reliably.
;                           Change inputs to make program more versatile. - MRA
;       2015/02/27  -   Added the STATUS keyword. - MRA
;       2015/03/03  -   Separated version histories by mode and packing mode. - MRA
;-
function mms_edi_amb_l2_write, sc, mode, tstart, amb_data, $
BRST=brst, $
DROPBOX_ROOT=dropbox, $
DATA_PATH_ROOT=data_path, $
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
	
	tf_brst = keyword_set(brst)
	if n_elements(optdesc) eq 0 then optdesc = 'amb'


;------------------------------------;
; Version History                    ;
;------------------------------------;
	;AMB
	if optdesc eq 'amb' then begin
		;Mods to data processing
		if tf_brst then begin
			mods = [ 'v0.0.0 - Original version.', $
			         'v1.0.0 - Include trajectory vectors and optics state.', $
			         'v1.1.0 - Update metadata: counts -> flux.', $
			         'v1.2.0 - Added flux error.', $
			         'v1.3.0 - Trajectory vector errors are now deltas.', $
			         'v1.4.0 - Fixed dead-time correction and error values.', $
			         'v1.5.0 - Factor of 2 for accumulation time & 2 for abscal factor in srvy mode.', $
			         'v1.6.0 - No factor of 2 for accumulation time in srvy mode.', $
			         'v2.0.0 - Reduced file size with scalar errors. Update metadata.', $
			         'v2.1.0 - Correct fill value for fluxes.', $
			         'v3.0.0 - Omni-directional error for trajectories.' ]
		endif else begin
			mods = [ 'v0.0.0 - Original version.', $
			         'v1.0.0 - Include trajectory vectors and optics state.', $
			         'v1.1.0 - Update metadata: counts -> flux.', $
			         'v1.2.0 - Added flux error.', $
			         'v1.3.0 - Trajectory vector errors are now deltas.', $
			         'v1.4.0 - Fixed dead-time correction and error values.', $
			         'v1.5.0 - Factor of 2 for accumulation time & 2 for abscal factor in srvy mode.', $
			         'v1.6.0 - No factor of 2 for accumulation time in srvy mode.', $
			         'v2.0.0 - Reduced file size with scalar errors. Update metadata.', $
			         'v2.1.0 - Correct fill value for fluxes.', $
			         'v3.0.0 - Omni-directional error for trajectories. Correct time deltas.' ]
		endelse
	
	;AMB-PM2
	endif else if optdesc eq 'amb-pm2' then begin
		;BRST
		if tf_brst then begin
			mods = [ 'v1.0.0 - Original version.', $
			         'v1.1.0 - Correct fill value for fluxes.', $
			         'v2.0.0 - Omni-directional error for trajectories.' ]
		
		;SRVY
		endif else begin
			mods = [ 'v1.0.0 - Original version.', $
			         'v1.1.0 - Correct fill value for fluxes.', $
			         'v2.0.0 - Omni-directional error for trajectories.' ]
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
	if ~isa(amb_data.tt2000_0,       'LONG64') then message, 'amb_data.tt2000_0 must be LONG64.'
	if ~isa(amb_data.tt2000_180,     'LONG64') then message, 'amb_data.tt2000_180 must be LONG64.'
	if ~isa(amb_data.tt2000_timetag, 'LONG64') then message, 'amb_data.epoch_timetag must be LONG64.'
	if ~isa(amb_data.optics,         'BYTE')   then message, 'amb_data.optics must be BYTE.'
	if ~isa(amb_data.energy_gdu1,    'UINT')   then message, 'amb_data.energy_gdu1 must be UINT.'
	if ~isa(amb_data.energy_gdu2,    'UINT')   then message, 'amb_data.energy_gdu2 must be UINT.'
	if ~isa(amb_data.gdu_0,          'BYTE')   then message, 'amb_data.gdu_0 must be BYTE.'
	if ~isa(amb_data.gdu_180,        'BYTE')   then message, 'amb_data.gdu_180 must be BYTE.'
	if ~isa(amb_data.counts1_0,      'FLOAT')  then message, 'amb_data.counts1_0 must be FLOAT.'
	if ~isa(amb_data.counts1_180,    'FLOAT')  then message, 'amb_data.counts1_180 must be FLOAT.'
	if mode eq 'brst' then begin
		if ~isa(amb_data.counts2_0,    'FLOAT') then message, 'amb_data.counts2_0 must be FLOAT.'
		if ~isa(amb_data.counts3_0,    'FLOAT') then message, 'amb_data.counts3_0 must be FLOAT.'
		if ~isa(amb_data.counts4_0,    'FLOAT') then message, 'amb_data.counts4_0 must be FLOAT.'
		if ~isa(amb_data.counts2_180,  'FLOAT') then message, 'amb_data.counts2_180 must be FLOAT.'
		if ~isa(amb_data.counts3_180,  'FLOAT') then message, 'amb_data.counts3_180 must be FLOAT.'
		if ~isa(amb_data.counts4_180,  'FLOAT') then message, 'amb_data.counts4_180 must be FLOAT.'
	endif

	;Open the CDF file
	oamb = MrCDF_File(amb_file, /CREATE, /CLOBBER)
	if obj_valid(oamb) eq 0 then return, ''

;------------------------------------;
; Errors                             ;
;------------------------------------;
	
	;Delta +/- for time
	;   - We accumulate over half the sampling time in srvy mode.
	if mode eq 'brst' $
		then t_delta = long64(1.0D/1024.0D/2.0D * 1d9) $
		else t_delta = long64(16.0D/1024.0D/2.0D * 1d9)
	
	;Trajectory error
	if optdesc eq 'amb-pm2' then begin
		traj_delta = 9.0
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
	;   scId_instrumentId_paramName[_coordSys][_paramQualifier][_subModeLevel][_mode][_level]
	prefix  = strjoin([sc, instr], '_') + '_'
	suffix  = '_' + strjoin([mode, level], '_')
	
	t_0_vname           = 'epoch_0'
	t_180_vname         = 'epoch_180'
	t_tt_vname          = 'epoch_timetag'
	optics_vname        = prefix + 'optics_state'  + suffix
	e_gdu1_vname        = prefix + 'energy_gdu1'   + suffix
	e_gdu2_vname        = prefix + 'energy_gdu2'   + suffix
	gdu_0_vname         = prefix + 'gdu_0'         + suffix
	gdu_180_vname       = prefix + 'gdu_180'       + suffix
	flux1_0_vname       = prefix + 'flux1_0'       + suffix
	flux2_0_vname       = prefix + 'flux2_0'       + suffix
	flux3_0_vname       = prefix + 'flux3_0'       + suffix
	flux4_0_vname       = prefix + 'flux4_0'       + suffix
	flux1_180_vname     = prefix + 'flux1_180'     + suffix
	flux2_180_vname     = prefix + 'flux2_180'     + suffix
	flux3_180_vname     = prefix + 'flux3_180'     + suffix
	flux4_180_vname     = prefix + 'flux4_180'     + suffix
	traj1_gse_0_vname   = prefix + 'traj1_gse_0'   + suffix
	traj2_gse_0_vname   = prefix + 'traj2_gse_0'   + suffix
	traj3_gse_0_vname   = prefix + 'traj3_gse_0'   + suffix
	traj4_gse_0_vname   = prefix + 'traj4_gse_0'   + suffix
	traj1_gse_180_vname = prefix + 'traj1_gse_180' + suffix
	traj2_gse_180_vname = prefix + 'traj2_gse_180' + suffix
	traj3_gse_180_vname = prefix + 'traj3_gse_180' + suffix
	traj4_gse_180_vname = prefix + 'traj4_gse_180' + suffix
	traj1_gsm_0_vname   = prefix + 'traj1_gsm_0'   + suffix
	traj2_gsm_0_vname   = prefix + 'traj2_gsm_0'   + suffix
	traj3_gsm_0_vname   = prefix + 'traj3_gsm_0'   + suffix
	traj4_gsm_0_vname   = prefix + 'traj4_gsm_0'   + suffix
	traj1_gsm_180_vname = prefix + 'traj1_gsm_180' + suffix
	traj2_gsm_180_vname = prefix + 'traj2_gsm_180' + suffix
	traj3_gsm_180_vname = prefix + 'traj3_gsm_180' + suffix
	traj4_gsm_180_vname = prefix + 'traj4_gsm_180' + suffix

	flux1_0_delta_vname     = prefix + 'flux1_0_delta'     + suffix
	flux2_0_delta_vname     = prefix + 'flux2_0_delta'     + suffix
	flux3_0_delta_vname     = prefix + 'flux3_0_delta'     + suffix
	flux4_0_delta_vname     = prefix + 'flux4_0_delta'     + suffix
	flux1_180_delta_vname   = prefix + 'flux1_180_delta'   + suffix
	flux2_180_delta_vname   = prefix + 'flux2_180_delta'   + suffix
	flux3_180_delta_vname   = prefix + 'flux3_180_delta'   + suffix
	flux4_180_delta_vname   = prefix + 'flux4_180_delta'   + suffix

	;Write variable data to file
	oamb -> WriteVar, /CREATE, t_0_vname,     amb_data.tt2000_0,       CDF_TYPE='CDF_TIME_TT2000'
	oamb -> WriteVar, /CREATE, t_180_vname,   amb_data.tt2000_180,     CDF_TYPE='CDF_TIME_TT2000'
	oamb -> WriteVar, /CREATE, t_tt_vname,    amb_data.tt2000_timetag, CDF_TYPE='CDF_TIME_TT2000'
	oamb -> WriteVar, /CREATE, optics_vname,  amb_data.optics,      COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, e_gdu1_vname,  amb_data.energy_gdu1, COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, e_gdu2_vname,  amb_data.energy_gdu2, COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, gdu_0_vname,   amb_data.gdu_0,       COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, gdu_180_vname, amb_data.gdu_180,     COMPRESSION='GZIP', GZIP_LEVEL=6
	
	;Group counts by pitch angle
	if mode eq 'brst' then begin
		;Flux
		oamb -> WriteVar, /CREATE, flux1_0_vname,    amb_data.counts1_0,   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux2_0_vname,    amb_data.counts2_0,   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux3_0_vname,    amb_data.counts3_0,   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux4_0_vname,    amb_data.counts4_0,   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux1_180_vname,  amb_data.counts1_180, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux2_180_vname,  amb_data.counts2_180, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux3_180_vname,  amb_data.counts3_180, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux4_180_vname,  amb_data.counts4_180, COMPRESSION='GZIP', GZIP_LEVEL=6
		
		;Errors
		oamb -> WriteVar, /CREATE, flux1_0_delta_vname,   amb_data.delta1_0,   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux2_0_delta_vname,   amb_data.delta2_0,   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux3_0_delta_vname,   amb_data.delta3_0,   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux4_0_delta_vname,   amb_data.delta4_0,   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux1_180_delta_vname, amb_data.delta1_180, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux2_180_delta_vname, amb_data.delta2_180, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux3_180_delta_vname, amb_data.delta3_180, COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux4_180_delta_vname, amb_data.delta4_180, COMPRESSION='GZIP', GZIP_LEVEL=6
	endif else begin
		;Flux
		oamb -> WriteVar, /CREATE, flux1_0_vname,    amb_data.counts1_0,   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux1_180_vname,  amb_data.counts1_180, COMPRESSION='GZIP', GZIP_LEVEL=6
		
		;Errors
		oamb -> WriteVar, /CREATE, flux1_0_delta_vname,    amb_data.delta1_0,   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, flux1_180_delta_vname,  amb_data.delta1_180, COMPRESSION='GZIP', GZIP_LEVEL=6
	endelse

	;BRST
	if mode eq 'brst' then begin
		;GSE Trajectories
		oamb -> WriteVar, /CREATE, traj1_gse_0_vname,         amb_data.traj_0_gse[*,*,0],      COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj2_gse_0_vname,         amb_data.traj_0_gse[*,*,1],      COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj3_gse_0_vname,         amb_data.traj_0_gse[*,*,2],      COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj4_gse_0_vname,         amb_data.traj_0_gse[*,*,3],      COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj1_gse_180_vname,       amb_data.traj_180_gse[*,*,0],    COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj2_gse_180_vname,       amb_data.traj_180_gse[*,*,1],    COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj3_gse_180_vname,       amb_data.traj_180_gse[*,*,2],    COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj4_gse_180_vname,       amb_data.traj_180_gse[*,*,3],    COMPRESSION='GZIP', GZIP_LEVEL=6
		
		;GSM Trajectories
		oamb -> WriteVar, /CREATE, traj1_gsm_0_vname,         amb_data.traj_0_gsm[*,*,0],      COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj2_gsm_0_vname,         amb_data.traj_0_gsm[*,*,1],      COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj3_gsm_0_vname,         amb_data.traj_0_gsm[*,*,2],      COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj4_gsm_0_vname,         amb_data.traj_0_gsm[*,*,3],      COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj1_gsm_180_vname,       amb_data.traj_180_gsm[*,*,0],    COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj2_gsm_180_vname,       amb_data.traj_180_gsm[*,*,1],    COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj3_gsm_180_vname,       amb_data.traj_180_gsm[*,*,2],    COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj4_gsm_180_vname,       amb_data.traj_180_gsm[*,*,3],    COMPRESSION='GZIP', GZIP_LEVEL=6
	
	;'SRVY'
	endif else begin
		;GSE
		oamb -> WriteVar, /CREATE, traj1_gse_0_vname,         amb_data.traj_0_gse[*,*,0],      COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj1_gse_180_vname,       amb_data.traj_180_gse[*,*,0],    COMPRESSION='GZIP', GZIP_LEVEL=6
		
		;GSM
		oamb -> WriteVar, /CREATE, traj1_gsm_0_vname,         amb_data.traj_0_gsm[*,*,0],      COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, traj1_gsm_180_vname,       amb_data.traj_180_gsm[*,*,0],    COMPRESSION='GZIP', GZIP_LEVEL=6
	endelse
	
	;Metadata
	traj_labl_vname = prefix + 'traj_labl'
	oamb -> WriteVar, /CREATE, traj_labl_vname, [ 'Phi',  'Theta'], /REC_NOVARY
	
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
	
	;TT2000_0
	oamb -> WriteVarAttr, t_0_vname, 'CATDESC',       'TT2000 time tags for EDU 0 degree pitch angle electron counts.'
	oamb -> WriteVarAttr, t_0_vname, 'DELTA_MINUS',    t_delta
	oamb -> WriteVarAttr, t_0_vname, 'DELTA_PLUS',     t_delta
	oamb -> WriteVarAttr, t_0_vname, 'FIELDNAM',      'Time'
	oamb -> WriteVarAttr, t_0_vname, 'FILLVAL',        MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oamb -> WriteVarAttr, t_0_vname, 'FORMAT',        'I16'
	oamb -> WriteVarAttr, t_0_vname, 'LABLAXIS',      'UT'
	oamb -> WriteVarAttr, t_0_vname, 'SI_CONVERSION', '1e-9>s'
	oamb -> WriteVarAttr, t_0_vname, 'TIME_BASE',     'J2000'
	oamb -> WriteVarAttr, t_0_vname, 'UNITS',         'UT'
	oamb -> WriteVarAttr, t_0_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	oamb -> WriteVarAttr, t_0_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2065, 12, 31), /CDF_EPOCH
	oamb -> WriteVarAttr, t_0_vname, 'VAR_TYPE',      'support_data'
	
	;TT2000_180
	oamb -> WriteVarAttr, t_180_vname, 'CATDESC',       'TT2000 time tags for EDU 180 degree pitch angle electron counts.'
	oamb -> WriteVarAttr, t_180_vname, 'DELTA_MINUS',    t_delta
	oamb -> WriteVarAttr, t_180_vname, 'DELTA_PLUS',     t_delta
	oamb -> WriteVarAttr, t_180_vname, 'FIELDNAM',      'Time'
	oamb -> WriteVarAttr, t_180_vname, 'FILLVAL',       MrCDF_Epoch_Compute(9999, 12, 31, 23, 59, 59, 999, 999, 999), /CDF_EPOCH
	oamb -> WriteVarAttr, t_180_vname, 'FORMAT',        'I16'
	oamb -> WriteVarAttr, t_180_vname, 'LABLAXIS',      'UT'
	oamb -> WriteVarAttr, t_180_vname, 'SI_CONVERSION', '1e-9>s'
	oamb -> WriteVarAttr, t_180_vname, 'TIME_BASE',     'J2000'
	oamb -> WriteVarAttr, t_180_vname, 'UNITS',         'UT'
	oamb -> WriteVarAttr, t_180_vname, 'VALIDMIN',      MrCDF_Epoch_Compute(2015,  3,  1), /CDF_EPOCH
	oamb -> WriteVarAttr, t_180_vname, 'VALIDMAX',      MrCDF_Epoch_Compute(2065, 12, 31), /CDF_EPOCH
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

	;GDU_0
	oamb -> WriteVarAttr, gdu_0_vname, 'CATDESC',       'Sorts 0 degree counts by GDU'
	oamb -> WriteVarAttr, gdu_0_vname, 'DEPEND_0',       t_0_vname
	oamb -> WriteVarAttr, gdu_0_vname, 'FIELDNAM',      'GDU Identifier'
	oamb -> WriteVarAttr, gdu_0_vname, 'FILLVAL',        255B
	oamb -> WriteVarAttr, gdu_0_vname, 'FORMAT',        'I1'
	oamb -> WriteVarAttr, gdu_0_vname, 'VALIDMIN',      1B
	oamb -> WriteVarAttr, gdu_0_vname, 'VALIDMAX',      2B
	oamb -> WriteVarAttr, gdu_0_vname, 'VAR_TYPE',      'support_data'

	;GDU_180
	oamb -> WriteVarAttr, gdu_180_vname, 'CATDESC',       'Sorts 180 degree counts by GDU'
	oamb -> WriteVarAttr, gdu_180_vname, 'DEPEND_0',       t_180_vname
	oamb -> WriteVarAttr, gdu_180_vname, 'FIELDNAM',      'GDU Identifier'
	oamb -> WriteVarAttr, gdu_180_vname, 'FILLVAL',        255B
	oamb -> WriteVarAttr, gdu_180_vname, 'FORMAT',        'I1'
	oamb -> WriteVarAttr, gdu_180_vname, 'VALIDMIN',      1B
	oamb -> WriteVarAttr, gdu_180_vname, 'VALIDMAX',      2B
	oamb -> WriteVarAttr, gdu_180_vname, 'VAR_TYPE',      'support_data'

	;FLUX1_0
	oamb -> WriteVarAttr, flux1_0_vname, 'CATDESC',         'Flux for electrons with trajectories given by traj1_0'
	oamb -> WriteVarAttr, flux1_0_vname, 'DEPEND_0',        t_0_vname
	oamb -> WriteVarAttr, flux1_0_vname, 'DELTA_MINUS_VAR', flux1_0_delta_vname
	oamb -> WriteVarAttr, flux1_0_vname, 'DELTA_PLUS_VAR',  flux1_0_delta_vname
	oamb -> WriteVarAttr, flux1_0_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, flux1_0_vname, 'FIELDNAM',        '0 degree electron counts'
	oamb -> WriteVarAttr, flux1_0_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, flux1_0_vname, 'FORMAT',          'E12.5'
	oamb -> WriteVarAttr, flux1_0_vname, 'LABLAXIS',        'Flux'
	oamb -> WriteVarAttr, flux1_0_vname, 'SCALETYP',        'log'
	oamb -> WriteVarAttr, flux1_0_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
	oamb -> WriteVarAttr, flux1_0_vname, 'UNITS',           'cm^-2 s^-1'
	oamb -> WriteVarAttr, flux1_0_vname, 'VALIDMIN',        0.0
	oamb -> WriteVarAttr, flux1_0_vname, 'VALIDMAX',        1e20
	oamb -> WriteVarAttr, flux1_0_vname, 'VAR_TYPE',        'data'

	;FLUX1_180
	oamb -> WriteVarAttr, flux1_180_vname, 'CATDESC',         'Flux for electrons with trajectories given by traj1_180'
	oamb -> WriteVarAttr, flux1_180_vname, 'DEPEND_0',        t_180_vname
	oamb -> WriteVarAttr, flux1_180_vname, 'DELTA_MINUS_VAR', flux1_180_delta_vname
	oamb -> WriteVarAttr, flux1_180_vname, 'DELTA_PLUS_VAR',  flux1_180_delta_vname
	oamb -> WriteVarAttr, flux1_180_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, flux1_180_vname, 'FIELDNAM',        '180 degree electron counts'
	oamb -> WriteVarAttr, flux1_180_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, flux1_180_vname, 'FORMAT',          'E12.5'
	oamb -> WriteVarAttr, flux1_180_vname, 'LABLAXIS',        'Flux'
	oamb -> WriteVarAttr, flux1_180_vname, 'SCALETYP',        'log'
	oamb -> WriteVarAttr, flux1_180_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
	oamb -> WriteVarAttr, flux1_180_vname, 'UNITS',           'cm^-2 s^-1'
	oamb -> WriteVarAttr, flux1_180_vname, 'VALIDMIN',        0.0
	oamb -> WriteVarAttr, flux1_180_vname, 'VALIDMAX',        1e20
	oamb -> WriteVarAttr, flux1_180_vname, 'VAR_TYPE',        'data'

	;TRAJ1_GSE_0
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'CATDESC',         'Trajectory of flux1 0-degree pitch angle electrons in GSE coordinates.'
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'DEPEND_0',        t_0_vname
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'DELTA_MINUS',     traj_delta
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'DELTA_PLUS',      traj_delta
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'FIELDNAM',        'Electron trajectory'
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'FORMAT',          'F9.4'
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'LABL_PTR_1',      traj_labl_vname
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'SCALETYP',        'linear'
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'SI_CONVERSION',   '57.2958>rad'
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'UNITS',           'degrees'
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'VALIDMIN',        -180.0
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'VALIDMAX',        180.0
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'VAR_TYPE',        'data'
	oamb -> WriteVarAttr, traj1_gse_0_vname, 'VAR_NOTES',      'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
	                                                           '(theta) representing the azimuthal (polar) directions, in the ' + $
	                                                           'indicated coordinate system. They are opposite to the nominal look-direction ' + $
	                                                           "of the instrument. Errors represent an omni-directional error. For more " + $
	                                                           'details about errors, contact the EDI instrument team.'

	;TRAJ1_GSE_180
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'CATDESC',         'Trajectory of flux1 180-degree pitch angle electrons in GSE coordinates.'
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'DEPEND_0',        t_180_vname
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'DELTA_MINUS',     traj_delta
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'DELTA_PLUS',      traj_delta
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'FIELDNAM',        'Electron trajectory'
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'FORMAT',          'F9.4'
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'LABL_PTR_1',      traj_labl_vname
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'SCALETYP',        'linear'
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'SI_CONVERSION',   '57.2958>rad'
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'UNITS',           'degrees'
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'VALIDMIN',        -180.0
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'VALIDMAX',        180.0
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'VAR_TYPE',        'data'
	oamb -> WriteVarAttr, traj1_gse_180_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
	                                                              '(theta) representing the azimuthal (polar) directions, in the ' + $
	                                                              'indicated coordinate system. They are opposite to the nominal look-direction ' + $
	                                                              "of the instrument. Errors represent an omni-directional error. For more " + $
	                                                              'details about errors, contact the EDI instrument team.'

	;TRAJ1_GSM_0
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'CATDESC',         'Trajectory of flux1 0-degree pitch angle electrons in GSM coordinates.'
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'DEPEND_0',        t_0_vname
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'DELTA_MINUS',     traj_delta
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'DELTA_PLUS',      traj_delta
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'FIELDNAM',        'Electron trajectory'
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'FORMAT',          'F9.4'
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'LABL_PTR_1',      traj_labl_vname
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'SCALETYP',        'linear'
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'SI_CONVERSION',   '57.2958>rad'
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'UNITS',           'degrees'
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'VALIDMIN',        -180.0
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'VALIDMAX',        180.0
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'VAR_TYPE',        'data'
	oamb -> WriteVarAttr, traj1_gsm_0_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
	                                                            '(theta) representing the azimuthal (polar) directions, in the ' + $
	                                                            'indicated coordinate system. They are opposite to the nominal look-direction ' + $
	                                                            "of the instrument. Errors represent an omni-directional error. For more " + $
	                                                            'details about errors, contact the EDI instrument team.'

	;TRAJ1_GSM_180
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'CATDESC',         'Trajectory of flux1 180-degree pitch angle electrons in GSM coordinates.'
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'DEPEND_0',        t_180_vname
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'DELTA_MINUS',     traj_delta
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'DELTA_PLUS',      traj_delta
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'FIELDNAM',        'Electron trajectory'
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'FILLVAL',         -1e31
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'FORMAT',          'F9.4'
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'LABL_PTR_1',      traj_labl_vname
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'SCALETYP',        'linear'
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'SI_CONVERSION',   '57.2958>rad'
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'UNITS',           'degrees'
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'VALIDMIN',        -20.0
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'VALIDMAX',        20.0
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'VAR_TYPE',        'data'
	oamb -> WriteVarAttr, traj1_gsm_180_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
	                                                              '(theta) representing the azimuthal (polar) directions, in the ' + $
	                                                              'indicated coordinate system. They are opposite to the nominal look-direction ' + $
	                                                              "of the instrument. Errors represent an omni-directional error. For more " + $
	                                                              'details about errors, contact the EDI instrument team.'
	
	;
	; ERRORS: Channel 1
	;

	;FLUX1_0_DELTA
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'CATDESC',       'Error for flux1 0-degree pitch angle electron flux.'
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'DEPEND_0',      t_0_vname
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'FIELDNAM',      'Flux error for 0PA electrons'
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'FILLVAL',       -1e31
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'FORMAT',        'E12.5'
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'LABLAXIS',      'dFlux'
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-2'
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'UNITS',         'cm^-2 s^-2'
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'VALIDMIN',      0.0
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'VALIDMAX',      1e20
	oamb -> WriteVarAttr, flux1_0_delta_vname, 'VAR_TYPE',      'support_data'

	;FLUX1_180_DELTA
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'CATDESC',       'Error for flux1 180-degree pitch angle electron flux.'
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'DEPEND_0',      t_180_vname
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'FIELDNAM',      'Flux error for 180PA electrons'
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'FILLVAL',       -1e31
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'FORMAT',        'E12.5'
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'LABLAXIS',      'dFlux'
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-2'
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'UNITS',         'cm^-2 s^-2'
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'VALIDMIN',      0.0
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'VALIDMAX',      1e20
	oamb -> WriteVarAttr, flux1_180_delta_vname, 'VAR_TYPE',      'support_data'
	
	;BURST DATA
	if mode eq 'brst' then begin
		;FLUX2_PA0
		oamb -> WriteVarAttr, flux2_0_vname, 'CATDESC',         'Flux for electrons with trajectories given by traj2_0'
		oamb -> WriteVarAttr, flux2_0_vname, 'DEPEND_0',         t_0_vname
		oamb -> WriteVarAttr, flux2_0_vname, 'DELTA_MINUS_VAR', flux2_0_delta_vname
		oamb -> WriteVarAttr, flux2_0_vname, 'DELTA_PLUS_VAR',  flux2_0_delta_vname
		oamb -> WriteVarAttr, flux2_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux2_0_vname, 'FIELDNAM',        'Electron Flux PA0'
		oamb -> WriteVarAttr, flux2_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux2_0_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux2_0_vname, 'LABLAXIS',        'Flux'
		oamb -> WriteVarAttr, flux2_0_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux2_0_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux2_0_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux2_0_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux2_0_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux2_0_vname, 'VAR_TYPE',        'data'

		;FLUX3_PA0
		oamb -> WriteVarAttr, flux3_0_vname, 'CATDESC',         'Flux for electrons with trajectories given by traj3_0'
		oamb -> WriteVarAttr, flux3_0_vname, 'DEPEND_0',         t_0_vname
		oamb -> WriteVarAttr, flux3_0_vname, 'DELTA_MINUS_VAR', flux3_0_delta_vname
		oamb -> WriteVarAttr, flux3_0_vname, 'DELTA_PLUS_VAR',  flux3_0_delta_vname
		oamb -> WriteVarAttr, flux3_0_vname, 'DISPLAY_TYPE'   , 'time_series'
		oamb -> WriteVarAttr, flux3_0_vname, 'FIELDNAM',        'Electron Flux PA0'
		oamb -> WriteVarAttr, flux3_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux3_0_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux3_0_vname, 'LABLAXIS',        'Flux'
		oamb -> WriteVarAttr, flux3_0_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux3_0_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux3_0_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux3_0_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux3_0_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux3_0_vname, 'VAR_TYPE',        'data'

		;FLUX4_PA0
		oamb -> WriteVarAttr, flux4_0_vname, 'CATDESC',         'Flux for electrons with trajectories given by traj4_0'
		oamb -> WriteVarAttr, flux4_0_vname, 'DEPEND_0',        t_0_vname
		oamb -> WriteVarAttr, flux4_0_vname, 'DELTA_MINUS_VAR', flux4_0_delta_vname
		oamb -> WriteVarAttr, flux4_0_vname, 'DELTA_PLUS_VAR',  flux4_0_delta_vname
		oamb -> WriteVarAttr, flux4_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux4_0_vname, 'FIELDNAM',        'Electron Flux PA0'
		oamb -> WriteVarAttr, flux4_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux4_0_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux4_0_vname, 'LABLAXIS',        'Flux'
		oamb -> WriteVarAttr, flux4_0_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux4_0_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux4_0_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux4_0_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux4_0_vname, 'VALIDMAX',        1e15
		oamb -> WriteVarAttr, flux4_0_vname, 'VAR_TYPE',        'data'

		;FLUX2_PA180
		oamb -> WriteVarAttr, flux2_180_vname, 'CATDESC',         'Flux for electrons with trajectories given by traj2_180'
		oamb -> WriteVarAttr, flux2_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, flux2_180_vname, 'DELTA_MINUS_VAR', flux2_180_delta_vname
		oamb -> WriteVarAttr, flux2_180_vname, 'DELTA_PLUS_VAR',  flux2_180_delta_vname
		oamb -> WriteVarAttr, flux2_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux2_180_vname, 'FIELDNAM',        'Electron Flux PA180'
		oamb -> WriteVarAttr, flux2_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux2_180_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux2_180_vname, 'LABLAXIS',        'Flux'
		oamb -> WriteVarAttr, flux2_180_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux2_180_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux2_180_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux2_180_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux2_180_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux2_180_vname, 'VAR_TYPE',        'data'

		;FLUX3_PA180
		oamb -> WriteVarAttr, flux3_180_vname, 'CATDESC',          'Flux for electrons with trajectories given by traj3_180'
		oamb -> WriteVarAttr, flux3_180_vname, 'DEPEND_0',          t_180_vname
		oamb -> WriteVarAttr, flux3_180_vname, 'DELTA_MINUS_VAR', flux3_180_delta_vname
		oamb -> WriteVarAttr, flux3_180_vname, 'DELTA_PLUS_VAR',  flux3_180_delta_vname
		oamb -> WriteVarAttr, flux3_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux3_180_vname, 'FIELDNAM',        'Electron Flux PA180'
		oamb -> WriteVarAttr, flux3_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux3_180_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux3_180_vname, 'LABLAXIS',        'Flux'
		oamb -> WriteVarAttr, flux3_180_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux3_180_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux3_180_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux3_180_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux3_180_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux3_180_vname, 'VAR_TYPE',        'data'

		;FLUX4_PA180
		oamb -> WriteVarAttr, flux4_180_vname, 'CATDESC',         'Flux for electrons with trajectories given by traj4_180'
		oamb -> WriteVarAttr, flux4_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, flux4_180_vname, 'DELTA_MINUS_VAR', flux4_180_delta_vname
		oamb -> WriteVarAttr, flux4_180_vname, 'DELTA_PLUS_VAR',  flux4_180_delta_vname
		oamb -> WriteVarAttr, flux4_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, flux4_180_vname, 'FIELDNAM',        'Electron Flux PA180'
		oamb -> WriteVarAttr, flux4_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, flux4_180_vname, 'FORMAT',          'E12.5'
		oamb -> WriteVarAttr, flux4_180_vname, 'LABLAXIS',        'Flux'
		oamb -> WriteVarAttr, flux4_180_vname, 'SCALETYP',        'log'
		oamb -> WriteVarAttr, flux4_180_vname, 'SI_CONVERSION',   '1e4>m^-2 s^-1'
		oamb -> WriteVarAttr, flux4_180_vname, 'UNITS',           'cm^-2 s^-1'
		oamb -> WriteVarAttr, flux4_180_vname, 'VALIDMIN',        0.0
		oamb -> WriteVarAttr, flux4_180_vname, 'VALIDMAX',        1e20
		oamb -> WriteVarAttr, flux4_180_vname, 'VAR_TYPE',        'data'
		
		;
		; DELTA FLUX: Channels 2-4
		;
		
		;FLUX2_0_DELTA
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'CATDESC',       'Error for flux2 0-degree pitch angle electron flux.'
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'DEPEND_0',      t_0_vname
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'FIELDNAM',      'Flux error for 0PA electrons'
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'LABLAXIS',      'dFlux'
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'SI_CONVERSION', '1e4>m^-2 s^-2'
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'UNITS',         'cm^-2 s^-2'
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'VALIDMAX',      1e30
		oamb -> WriteVarAttr, flux2_0_delta_vname, 'VAR_TYPE',      'support_data'

		;FLUX2_180_DELTA
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'CATDESC',       'Error for flux2 180-degree pitch angle electron flux.'
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'DEPEND_0',      t_180_vname
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'FIELDNAM',      'Flux error for 180PA electrons'
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'LABLAXIS',      'dFlux'
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'SI_CONVERSION', '1e4>m^-2 s^-2'
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'UNITS',         'cm^-2 s^-2'
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'VALIDMAX',      1e30
		oamb -> WriteVarAttr, flux2_180_delta_vname, 'VAR_TYPE',      'support_data'

		;FLUX3_0_DELTA
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'CATDESC',       'Error for flux3 0-degree pitch angle electron flux.'
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'DEPEND_0',      t_0_vname
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'FIELDNAM',      'Flux error for 0PA electrons'
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'LABLAXIS',      'dFlux'
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'SI_CONVERSION', '1e4>m^-2 s^-2'
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'UNITS',         'cm^-2 s^-2'
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'VALIDMAX',      1e30
		oamb -> WriteVarAttr, flux3_0_delta_vname, 'VAR_TYPE',      'support_data'

		;FLUX3_180_DELTA
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'CATDESC',       'Error for flux3 180-degree pitch angle electron flux.'
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'DEPEND_0',      t_180_vname
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'FIELDNAM',      'Flux error for 180PA electrons'
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'LABLAXIS',      'dFlux'
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'SI_CONVERSION', '1e4>m^-2 s^-2'
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'UNITS',         'cm^-2 s^-2'
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'VALIDMAX',      1e30
		oamb -> WriteVarAttr, flux3_180_delta_vname, 'VAR_TYPE',      'support_data'

		;FLUX4_0_DELTA
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'CATDESC',       'Error for flux4 0-degree pitch angle electron flux.'
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'DEPEND_0',      t_0_vname
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'FIELDNAM',      'Flux error for 0PA electrons'
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'LABLAXIS',      'dFlux'
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'SI_CONVERSION', '1e4>m^-2 s^-2'
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'UNITS',         'cm^-2 s^-2'
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'VALIDMAX',      1e30
		oamb -> WriteVarAttr, flux4_0_delta_vname, 'VAR_TYPE',      'support_data'

		;FLUX4_180_DELTA
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'CATDESC',       'Error for flux4 180-degree pitch angle electron flux.'
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'DEPEND_0',      t_180_vname
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'FIELDNAM',      'Flux error for 180PA electrons'
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'FILLVAL',       -1e31
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'FORMAT',        'E12.5'
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'LABLAXIS',      'dFlux'
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'SI_CONVERSION', '1e4>m^-2 s^-2'
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'UNITS',         'cm^-2 s^-2'
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'VALIDMIN',      0.0
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'VALIDMAX',      1e30
		oamb -> WriteVarAttr, flux4_180_delta_vname, 'VAR_TYPE',      'support_data'
		
		;
		; DELTA TRAJECTORY: Channels 2-4
		;

		;TRAJ2_GSE_0
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'CATDESC',         'Trajectory of flux2 0-degree pitch angle electrons in GSE coordinates.'
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'DEPEND_0',        t_0_vname
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj2_gse_0_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                            '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                            'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                            "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                            'details about errors, contact the EDI instrument team.'
	
		;TRAJ3_GSE_0
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'CATDESC',         'Trajectory of flux3 0-degree pitch angle electrons in GSE coordinates.'
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'DEPEND_0',        t_0_vname
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj3_gse_0_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                            '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                            'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                            "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                            'details about errors, contact the EDI instrument team.'

		;TRAJ4_GSE_0
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'CATDESC',         'Trajectory of flux4 0-degree pitch angle electrons in GSE coordinates.'
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'DEPEND_0',        t_0_vname
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj4_gse_0_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                            '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                            'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                            "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                            'details about errors, contact the EDI instrument team.'

		;TRAJ2_GSE_180
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'CATDESC',         'Trajectory of flux2 180-degree pitch angle electrons in GSE coordinates.'
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj2_gse_180_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                              '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                              'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                              "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                              'details about errors, contact the EDI instrument team.'

		;TRAJ3_GSE_180
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'CATDESC',         'Trajectory of flux3 180-degree pitch angle electrons in GSE coordinates.'
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj3_gse_180_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                              '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                              'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                              "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                              'details about errors, contact the EDI instrument team.'

		;TRAJ4_GSE_180
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'CATDESC',         'Trajectory of flux4 180-degree pitch angle electrons in GSE coordinates.'
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj4_gse_180_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                              '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                              'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                              "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                              'details about errors, contact the EDI instrument team.'

		;TRAJ2_GSM_0
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'CATDESC',         'Trajectory of flux2 0-degree pitch angle electrons in GSM coordinates.'
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'DEPEND_0',        t_0_vname
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj2_gsm_0_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                            '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                            'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                            "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                            'details about errors, contact the EDI instrument team.'

		;TRAJ3_GSM_0
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'CATDESC',         'Trajectory of flux3 0-degree pitch angle electrons in GSM coordinates.'
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'DEPEND_0',        t_0_vname
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj3_gsm_0_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                            '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                            'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                            "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                            'details about errors, contact the EDI instrument team.'

		;TRAJ4_GSM_0
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'CATDESC',         'Trajectory of flux4 0-degree pitch angle electrons in GSM coordinates.'
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'DEPEND_0',        t_0_vname
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj4_gsm_0_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                            '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                            'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                            "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                            'details about errors, contact the EDI instrument team.'

		;TRAJ2_GSM_180
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'CATDESC',         'Trajectory of flux2 180-degree pitch angle electrons in GSM coordinates.'
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj2_gsm_180_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                              '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                              'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                              "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                              'details about errors, contact the EDI instrument team.'

		;TRAJ3_GSM_180
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'CATDESC',         'Trajectory of flux3 180-degree pitch angle electrons in GSM coordinates.'
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj3_gsm_180_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                              '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                              'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                              "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                              'details about errors, contact the EDI instrument team.'

		;TRAJ4_GSM_180
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'CATDESC',         'Trajectory of flux4 180-degree pitch angle electrons in GSM coordinates.'
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'DEPEND_0',        t_180_vname
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'DELTA_MINUS',     traj_delta
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'DELTA_PLUS',      traj_delta
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'DISPLAY_TYPE',    'time_series'
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'FIELDNAM',        'Electron trajectory'
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'FILLVAL',         -1e31
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'FORMAT',          'F9.4'
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'LABL_PTR_1',      traj_labl_vname
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'SCALETYP',        'linear'
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'UNITS',           'degrees'
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'VALIDMIN',        -180.0
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'VALIDMAX',        180.0
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'VAR_TYPE',        'data'
		oamb -> WriteVarAttr, traj4_gsm_180_vname, 'VAR_NOTES',       'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
		                                                              '(theta) representing the azimuthal (polar) directions, in the ' + $
		                                                              'indicated coordinate system. They are opposite to the nominal look-direction ' + $
		                                                              "of the instrument. Errors represent an omni-directional error. For more " + $
		                                                              'details about errors, contact the EDI instrument team.'
	endif
	
	;
	; METADATA
	;

	;TRAJ_LABL
	oamb -> WriteVarAttr, traj_labl_vname, 'CATDESC',         'Trajectory labels'
	oamb -> WriteVarAttr, traj_labl_vname, 'DISPLAY_TYPE',    'time_series'
	oamb -> WriteVarAttr, traj_labl_vname, 'FIELDNAM',        'Trajectory labels'
	oamb -> WriteVarAttr, traj_labl_vname, 'FORMAT',          'A5'
	oamb -> WriteVarAttr, traj_labl_vname, 'VAR_TYPE',        'metadata'

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, oamb
	return, amb_file
end