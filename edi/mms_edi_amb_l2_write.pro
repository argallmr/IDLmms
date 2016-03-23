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
function mms_edi_amb_l2_write, amb_file, amb_data
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
		return, status
	endif
	
	;Everything starts out ok
	status = 0
	
	;Parse the file name
	mms_dissect_filename, amb_file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level
	
;------------------------------------;
; Check Data and Open File           ;
;------------------------------------;
	;
	; Check sizes
	;
	if ~isa(amb_data.tt2000,         'LONG64') then message, 'amb_data.tt2000 must be LONG64.'
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
	oamb = MrCDF_File(amb_file, /MODIFY)
	if obj_valid(oamb) eq 0 then message, 'Could not open file for writing: "' + amb_file + '".'

;------------------------------------------------------
; Variable Names                                      |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName[_coordSys][_paramQualifier][_subModeLevel][_mode][_level]
	prefix  = strjoin([sc, instr], '_') + '_'
	suffix  = '_' + strjoin([mode, level], '_')
	
	t_vname             = 'Epoch'
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

;------------------------------------------------------
; Write Data                                          |
;------------------------------------------------------

	;Write variable data to file
	oamb -> WriteVar, t_vname,       amb_data.tt2000
	oamb -> WriteVar, t_tt_vname,    amb_data.tt2000_timetag
	oamb -> WriteVar, optics_vname,  amb_data.optics
	oamb -> WriteVar, e_gdu1_vname,  amb_data.energy_gdu1
	oamb -> WriteVar, e_gdu2_vname,  amb_data.energy_gdu2
	oamb -> WriteVar, gdu_0_vname,   amb_data.gdu_0
	oamb -> WriteVar, gdu_180_vname, amb_data.gdu_180
	
	;Group counts by pitch angle
	if mode eq 'brst' then begin
		;Flux
		oamb -> WriteVar, flux1_0_vname,    amb_data.counts1_0
		oamb -> WriteVar, flux2_0_vname,    amb_data.counts2_0
		oamb -> WriteVar, flux3_0_vname,    amb_data.counts3_0
		oamb -> WriteVar, flux4_0_vname,    amb_data.counts4_0
		oamb -> WriteVar, flux1_180_vname,  amb_data.counts1_180
		oamb -> WriteVar, flux2_180_vname,  amb_data.counts2_180
		oamb -> WriteVar, flux3_180_vname,  amb_data.counts3_180
		oamb -> WriteVar, flux4_180_vname,  amb_data.counts4_180
		
		;Errors
		oamb -> WriteVar, flux1_0_delta_vname,   amb_data.delta1_0
		oamb -> WriteVar, flux2_0_delta_vname,   amb_data.delta2_0
		oamb -> WriteVar, flux3_0_delta_vname,   amb_data.delta3_0
		oamb -> WriteVar, flux4_0_delta_vname,   amb_data.delta4_0
		oamb -> WriteVar, flux1_180_delta_vname, amb_data.delta1_180
		oamb -> WriteVar, flux2_180_delta_vname, amb_data.delta2_180
		oamb -> WriteVar, flux3_180_delta_vname, amb_data.delta3_180
		oamb -> WriteVar, flux4_180_delta_vname, amb_data.delta4_180
	endif else begin
		;Flux
		oamb -> WriteVar, flux1_0_vname,    amb_data.counts1_0
		oamb -> WriteVar, flux1_180_vname,  amb_data.counts1_180
		
		;Errors
		oamb -> WriteVar, flux1_0_delta_vname,    amb_data.delta1_0
		oamb -> WriteVar, flux1_180_delta_vname,  amb_data.delta1_180
	endelse

	;BRST
	if mode eq 'brst' then begin
		;GSE Trajectories
		oamb -> WriteVar, traj1_gse_0_vname,   amb_data.traj_0_gse[*,*,0]
		oamb -> WriteVar, traj2_gse_0_vname,   amb_data.traj_0_gse[*,*,1]
		oamb -> WriteVar, traj3_gse_0_vname,   amb_data.traj_0_gse[*,*,2]
		oamb -> WriteVar, traj4_gse_0_vname,   amb_data.traj_0_gse[*,*,3]
		oamb -> WriteVar, traj1_gse_180_vname, amb_data.traj_180_gse[*,*,0]
		oamb -> WriteVar, traj2_gse_180_vname, amb_data.traj_180_gse[*,*,1]
		oamb -> WriteVar, traj3_gse_180_vname, amb_data.traj_180_gse[*,*,2]
		oamb -> WriteVar, traj4_gse_180_vname, amb_data.traj_180_gse[*,*,3]
		
		;GSM Trajectories
		oamb -> WriteVar, traj1_gsm_0_vname,   amb_data.traj_0_gsm[*,*,0]
		oamb -> WriteVar, traj2_gsm_0_vname,   amb_data.traj_0_gsm[*,*,1]
		oamb -> WriteVar, traj3_gsm_0_vname,   amb_data.traj_0_gsm[*,*,2]
		oamb -> WriteVar, traj4_gsm_0_vname,   amb_data.traj_0_gsm[*,*,3]
		oamb -> WriteVar, traj1_gsm_180_vname, amb_data.traj_180_gsm[*,*,0]
		oamb -> WriteVar, traj2_gsm_180_vname, amb_data.traj_180_gsm[*,*,1]
		oamb -> WriteVar, traj3_gsm_180_vname, amb_data.traj_180_gsm[*,*,2]
		oamb -> WriteVar, traj4_gsm_180_vname, amb_data.traj_180_gsm[*,*,3]
	
	;'SRVY'
	endif else begin
		;GSE
		oamb -> WriteVar, traj1_gse_0_vname,   amb_data.traj_0_gse[*,*,0]
		oamb -> WriteVar, traj1_gse_180_vname, amb_data.traj_180_gse[*,*,0]
		
		;GSM
		oamb -> WriteVar, traj1_gsm_0_vname,   amb_data.traj_0_gsm[*,*,0]
		oamb -> WriteVar, traj1_gsm_180_vname, amb_data.traj_180_gsm[*,*,0]
	endelse
	
;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, oamb
	return, status
end