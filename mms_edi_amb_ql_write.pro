; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ql_write
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
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
;       AMB_DATA:       in, optional, type=struct
;                       EDI ambient data structure with the following fields::
;                           TT2000_0    - TT2000 time tags for 0-pitch angle sorted data
;                           TT2000_180  - TT2000 time tags for 180-pitch angle sorted data
;                           TT2000_TT   - TT2000 time tags for packet-resolution data
;                           ENERGY_GDU1 - Electron energy for GDU1
;                           ENERGY_GDU2 - Electron energy for GDU2
;                           PACK_MODE   - Packing mode
;                           COUNTS1_0   - Counts1 data sorted by 0-degree pitch mode
;                           COUNTS1_180 - Counts1 data sorted by 180-degree pitch mode
;                           COUNTS2_0   - Counts2 data sorted by 0-degree pitch mode (brst only)
;                           COUNTS2_180 - Counts2 data sorted by 180-degree pitch mode (brst only)
;                           COUNTS3_0   - Counts3 data sorted by 0-degree pitch mode (brst only)
;                           COUNTS3_180 - Counts3 data sorted by 180-degree pitch mode (brst only)
;                           COUNTS4_0   - Counts4 data sorted by 0-degree pitch mode (brst only)
;                           COUNTS4_180 - Counts4 data sorted by 180-degree pitch mode (brst only)
;       META:           in, optional, type=string/struct, default='[pwd]/mms_edi_amb.cdf'
;                       Either a file name or a structure of metadata. If a filename is
;                           given that does not conform to the MMS file naming conventions,
;                           metadata will be generic. If META is a structure, it must have
;                           the following tags::
;                               SC         - MMS spacecraft ID
;                               INSTR      - MMS instrument ID
;                               MODE       - MMS telemetry mode
;                               LEVEL      - Data product level
;                               OPTDESC    - Optional descriptor
;                               TSTART     - Start time
;                               DIRECTORY  - Directory in which to save the file
;                               MODS       - Version history array
;                               PARENTS    - Names of files used to produce `AMB_DATA`
;
; :Returns:
;       AMB_FILE:       Name of the file created.
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
;-
function mms_edi_amb_ql_write, sc, mode, tstart, amb_data, $
DROPBOX=dropbox, $
DATA_PATH=data_path, $
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
	mods = [ 'v0.0.0 - Original version.', $
	         'v0.1.0 - Added PACK_MODE variable.' ]
	
	;Get the version
	version = stregex(mods[-1], '^v([0-9]+)\.([0-9]+)\.([0-9]+)', /SUBEXP, /EXTRACT)
	vx      = strtrim(version[1], 2)
	vy      = strtrim(version[2], 2)
	vz      = strtrim(version[3], 2)
	
	;Constants for output file
	instr   = 'edi'
	level   = 'ql'
	optdesc = 'amb'

;------------------------------------;
; Check Inputs                       ;
;------------------------------------;
	
	;Defaults
	if n_elements(sc)      eq 0 || sc      eq '' then sc     = 'mms#'
	if n_elements(mode)    eq 0 || mode    eq '' then mode   = 'mode'
	if n_elements(outdir)  eq 0 || outdir  eq '' then cd, CURRENT=outdir
	if n_elements(tstart)  eq 0 || tstart  eq '' then begin
		MrCDF_Epoch_Breakdown, amb_data.tt2000_0[0], yr, mo, day, hr, mn, sec
		tstart = string(FORMAT='(%"%04i%02i%02i%02i%02i%02i")', yr, mo, day, hr, mn, sec)
	endif
	
	;Check if the system variable exists
	defsysv, '!mms_init', EXISTS=tf_sysv
	if tf_sysv then begin
		if n_elements(dropbox)   eq 0 then dropbox   = !mms_init.dropbox
		if n_elements(data_path) eq 0 then data_path = !mms_init.data_path
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
	if ~isa(amb_data.pack_mode,    'BYTE')   then message, 'amb_data.pack_mode must be BYTE.'
	if ~isa(amb_data.counts1_0,    'UINT')   then message, 'amb_data.counts1_0 must be UINT.'
	if ~isa(amb_data.counts1_180,  'UINT')   then message, 'amb_data.counts1_180 must be UINT.'
	if mode eq 'brst' then begin
		if ~isa(amb_data.counts2_0,    'UINT') then message, 'amb_data.counts2_0 must be UINT.'
		if ~isa(amb_data.counts3_0,    'UINT') then message, 'amb_data.counts3_0 must be UINT.'
		if ~isa(amb_data.counts4_0,    'UINT') then message, 'amb_data.counts4_0 must be UINT.'
		if ~isa(amb_data.counts2_180,  'UINT') then message, 'amb_data.counts2_180 must be UINT.'
		if ~isa(amb_data.counts3_180,  'UINT') then message, 'amb_data.counts3_180 must be UINT.'
		if ~isa(amb_data.counts4_180,  'UINT') then message, 'amb_data.counts4_180 must be UINT.'
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
	oamb -> WriteGlobalAttr, /CREATE, 'PI_affiliation',             'SWRI, UNH'
	oamb -> WriteGlobalAttr, /CREATE, 'PI_name',                    'J. Burch, R. Torbert'
	oamb -> WriteGlobalAttr, /CREATE, 'Project',                    'STP>Solar Terrestrial Physics'
	oamb -> WriteGlobalAttr, /CREATE, 'Source_name',                source_name
	oamb -> WriteGlobalAttr, /CREATE, 'TEXT',                       'EDI ambient data. The instrument paper ' + $
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
	
	t_0_vname             = 'epoch_0'
	t_180_vname           = 'epoch_180'
	t_tt_vname            = 'epoch_timetag'
	e_gdu1_vname          = mms_construct_varname(sc, instr, 'energy',  'gdu1')
	e_gdu2_vname          = mms_construct_varname(sc, instr, 'energy',  'gdu2')
	gdu_0_vname           = mms_construct_varname(sc, instr, 'gdu',     '0')
	gdu_180_vname         = mms_construct_varname(sc, instr, 'gdu',     '180')
	pack_mode_vname       = mms_construct_varname(sc, instr, 'pack',    'mode')
	counts1_0_vname       = mms_construct_varname(sc, instr, 'counts1', '0')
	counts2_0_vname       = mms_construct_varname(sc, instr, 'counts2', '0')
	counts3_0_vname       = mms_construct_varname(sc, instr, 'counts3', '0')
	counts4_0_vname       = mms_construct_varname(sc, instr, 'counts4', '0')
	counts1_180_vname     = mms_construct_varname(sc, instr, 'counts1', '180')
	counts2_180_vname     = mms_construct_varname(sc, instr, 'counts2', '180')
	counts3_180_vname     = mms_construct_varname(sc, instr, 'counts3', '180')
	counts4_180_vname     = mms_construct_varname(sc, instr, 'counts4', '180')

	;Write variable data to file
	oamb -> WriteVar, /CREATE, t_0_vname,          transpose(amb_data.tt2000_0),    CDF_TYPE='CDF_TIME_TT2000'
	oamb -> WriteVar, /CREATE, t_180_vname,        transpose(amb_data.tt2000_180),  CDF_TYPE='CDF_TIME_TT2000'
	oamb -> WriteVar, /CREATE, t_tt_vname,         transpose(amb_data.tt2000_tt),   CDF_TYPE='CDF_TIME_TT2000'
	oamb -> WriteVar, /CREATE, e_gdu1_vname,       transpose(amb_data.energy_gdu1), COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, e_gdu2_vname,       transpose(amb_data.energy_gdu2), COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, gdu_0_vname,        transpose(amb_data.gdu_0),       COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, gdu_180_vname,      transpose(amb_data.gdu_180),     COMPRESSION='GZIP', GZIP_LEVEL=6
	oamb -> WriteVar, /CREATE, pack_mode_vname,    transpose(amb_data.pack_mode),   COMPRESSION='GZIP', GZIP_LEVEL=6

	;Put group variables by pitch angle.
	if mode eq 'brst' then begin
		oamb -> WriteVar, /CREATE, counts1_0_vname,   transpose(amb_data.counts1_0),   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts2_0_vname,   transpose(amb_data.counts2_0),   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts3_0_vname,   transpose(amb_data.counts3_0),   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts4_0_vname,   transpose(amb_data.counts4_0),   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts1_180_vname, transpose(amb_data.counts1_180), COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts2_180_vname, transpose(amb_data.counts2_180), COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts3_180_vname, transpose(amb_data.counts3_180), COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts4_180_vname, transpose(amb_data.counts4_180), COMPRESSION='GZIP', GZIP_LEVEL=6
	endif else begin
		oamb -> WriteVar, /CREATE, counts1_0_vname,    transpose(amb_data.counts1_0),   COMPRESSION='GZIP', GZIP_LEVEL=6
		oamb -> WriteVar, /CREATE, counts1_180_vname,  transpose(amb_data.counts1_180), COMPRESSION='GZIP', GZIP_LEVEL=6
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
	oamb -> WriteVarAttr, t_0_vname, 'CATDESC',       'TT2000 time tags for field-aligned angle electron counts. ' + $
	                                                  'Field-aligned means the reference anode is set to detect 0-degree ' + $
	                                                  'pitch-angle electrons.'
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
	oamb -> WriteVarAttr, t_180_vname, 'CATDESC',       'TT2000 time tags for anti-field-aligned angle electron counts. ' + $
	                                                    'Field-aligned means the reference anode is set to detect 180-degree ' + $
	                                                    'pitch-angle electrons.'
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
	oamb -> WriteVarAttr, t_tt_vname, 'CATDESC',       'TT2000 time tags for EDI support data.'
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

	;PACK_MODE
	oamb -> WriteVarAttr, pack_mode_vname, 'CATDESC',       'Flag indicating how GDU anodes relate ' + $
	                                                        'to magnetic field direction. See the data ' + $
	                                                        'product guide for more information.'
	oamb -> WriteVarAttr, pack_mode_vname, 'DEPEND_0',       t_tt_vname
	oamb -> WriteVarAttr, pack_mode_vname, 'FIELDNAM',      'Packing Mode Flag'
	oamb -> WriteVarAttr, pack_mode_vname, 'FILLVAL',        255
	oamb -> WriteVarAttr, pack_mode_vname, 'FORMAT',        'I1'
	oamb -> WriteVarAttr, pack_mode_vname, 'VALIDMIN',      0B
	oamb -> WriteVarAttr, pack_mode_vname, 'VALIDMAX',      3B
	oamb -> WriteVarAttr, pack_mode_vname, 'VAR_TYPE',      'meta_data'

	;COUNTS1_0
	oamb -> WriteVarAttr, counts1_0_vname, 'CATDESC',      'Field-aligned electrons from the counts1 anode. Actual ' + $
	                                                       'pitch-angle depends on the packing mode. See the EDI ' + $
	                                                       'data products guide for more details.'
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
	oamb -> WriteVarAttr, counts1_180_vname, 'CATDESC',      'Anti-field-aligned electrons from the counts1 anode. Actual ' + $
	                                                         'pitch-angle depends on the packing mode. See the EDI ' + $
	                                                         'data products guide for more details.'
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

	;BURST DATA
	if mode eq 'brst' then begin
		;COUNTS2_PA0
		oamb -> WriteVarAttr, counts2_0_vname, 'CATDESC',      'Field-aligned electrons from the counts2 anode. Actual ' + $
		                                                       'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                       'data products guide for more details.'
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
		oamb -> WriteVarAttr, counts3_0_vname, 'CATDESC',      'Field-aligned electrons from the counts3 anode. Actual ' + $
		                                                       'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                       'data products guide for more details.'
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
		oamb -> WriteVarAttr, counts4_0_vname, 'CATDESC',      'Field-aligned electrons from the counts4 anode. Actual ' + $
		                                                       'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                       'data products guide for more details.'
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
		oamb -> WriteVarAttr, counts2_180_vname, 'CATDESC',      'Anti-field-aligned electrons from the counts2 anode. Actual ' + $
		                                                         'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                         'data products guide for more details.'
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
		oamb -> WriteVarAttr, counts3_180_vname, 'CATDESC',      'Anti-field-aligned electrons from the counts3 anode. Actual ' + $
		                                                         'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                         'data products guide for more details.'
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
		oamb -> WriteVarAttr, counts4_180_vname, 'CATDESC',      'Anti-field-aligned electrons from the counts1 anode. Actual ' + $
		                                                         'pitch-angle depends on the packing mode. See the EDI ' + $
		                                                         'data guide for more details.'
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
	endif

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, oamb
	return, amb_file
end