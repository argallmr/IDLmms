; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_l2_write_perp
;
; PURPOSE:
;+
;   Write CDF files of perpendicular pitch angle mode EDI ambient data.
;
; :Categories:
;    MMS, EDI, L2, Ambient
;
; :Params:
;       AMB_FILE:           in, required, type=string
;                           Name of the file to which data is written.
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
; :See Also:
;   mms_edi_amb_l2_create.pro
;   mms_edi_amb_l2_mkfile_alt.pro
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
;-
function mms_edi_amb_l2_write_perp, amb_file, amb_data
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
	mms_dissect_filename, amb_file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level, OPTDESC=optdesc
	
	;Preliminary dataset?
	tf_abscal = ~stregex(optdesc, 'noabs', /BOOLEAN)
	datatype  = tf_abscal ? 'FLOAT' : 'ULONG'

;------------------------------------;
; Check Data and Open File           ;
;------------------------------------;
	;
	; Check sizes
	;
	if ~isa(amb_data.epoch_perp,        'LONG64') then message, 'amb_data.epoch_perp must be LONG64.'
	if ~isa(amb_data.epoch_timetag,     'LONG64') then message, 'amb_data.epoch_timetag must be LONG64.'
	if ~isa(amb_data.optics,            'BYTE')   then message, 'amb_data.optics must be BYTE.'
	if ~isa(amb_data.energy_gdu1,       'UINT')   then message, 'amb_data.energy_gdu1 must be UINT.'
	if ~isa(amb_data.energy_gdu2,       'UINT')   then message, 'amb_data.energy_gdu2 must be UINT.'
;	if ~isa(amb_data.flip_90,           'BYTE')   then message, 'amb_data.flip_90 must be BYTE.'
	if ~isa(amb_data.counts_90_gdu1,    datatype) then message, 'amb_data.counts_90_gdu1 must be ' + datatype + '.'
	if ~isa(amb_data.counts_90_gdu2,    datatype) then message, 'amb_data.counts_90_gdu2 must be ' + datatype + '.'
	if ~isa(amb_data.delta_90_gdu2,     datatype) then message, 'amb_data.delta_90_gdu2 must be '  + datatype + '.'
	if ~isa(amb_data.delta_90_gdu2,     datatype) then message, 'amb_data.delta_90_gdu2 must be '  + datatype + '.'
	if ~isa(amb_data.traj_dbcs_90_gdu1, 'FLOAT')  then message, 'amb_data.traj_dbcs_90_gdu1 must be FLOAT.'
	if ~isa(amb_data.traj_dbcs_90_gdu2, 'FLOAT')  then message, 'amb_data.traj_dbcs_90_gdu2 must be FLOAT.'
	if ~isa(amb_data.traj_gse_90_gdu1,  'FLOAT')  then message, 'amb_data.traj_gse_90_gdu1 must be FLOAT.'
	if ~isa(amb_data.traj_gse_90_gdu2,  'FLOAT')  then message, 'amb_data.traj_gse_90_gdu2 must be FLOAT.'

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
	
	t_vname                   = 'Epoch'
	t_tt_vname                = 'epoch_timetag'
	optics_vname              = prefix + 'optics_state'       + suffix
	e_gdu1_vname              = prefix + 'energy_gdu1'        + suffix
	e_gdu2_vname              = prefix + 'energy_gdu2'        + suffix
;	flip_90_vname             = prefix + 'flip_90'            + suffix
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

	delta1_90_gdu1_vname = prefix + 'flux1_90_delta_gdu1' + suffix
	delta2_90_gdu1_vname = prefix + 'flux2_90_delta_gdu1' + suffix
	delta3_90_gdu1_vname = prefix + 'flux3_90_delta_gdu1' + suffix
	delta4_90_gdu1_vname = prefix + 'flux4_90_delta_gdu1' + suffix
	delta1_90_gdu2_vname = prefix + 'flux1_90_delta_gdu2' + suffix
	delta2_90_gdu2_vname = prefix + 'flux2_90_delta_gdu2' + suffix
	delta3_90_gdu2_vname = prefix + 'flux3_90_delta_gdu2' + suffix
	delta4_90_gdu2_vname = prefix + 'flux4_90_delta_gdu2' + suffix

;------------------------------------------------------
; Write Support Data                                  |
;------------------------------------------------------

	;Write variable data to file
	oamb -> WriteVar, t_vname,       amb_data.epoch_perp
	oamb -> WriteVar, t_tt_vname,    amb_data.epoch_timetag
	oamb -> WriteVar, optics_vname,  amb_data.optics
	oamb -> WriteVar, e_gdu1_vname,  amb_data.energy_gdu1
	oamb -> WriteVar, e_gdu2_vname,  amb_data.energy_gdu2
;	oamb -> WriteVar, flip_90_vname, amb_data.flip_90

;------------------------------------------------------
; Write Flux Data                                     |
;------------------------------------------------------
	if mode eq 'brst' then begin
		;Flux
		oamb -> WriteVar, flux1_90_gdu1_vname, amb_data.counts_90_gdu1[*,0]
		oamb -> WriteVar, flux2_90_gdu1_vname, amb_data.counts_90_gdu1[*,1]
		oamb -> WriteVar, flux3_90_gdu1_vname, amb_data.counts_90_gdu1[*,2]
		oamb -> WriteVar, flux4_90_gdu1_vname, amb_data.counts_90_gdu1[*,3]
		oamb -> WriteVar, flux1_90_gdu2_vname, amb_data.counts_90_gdu2[*,0]
		oamb -> WriteVar, flux2_90_gdu2_vname, amb_data.counts_90_gdu2[*,1]
		oamb -> WriteVar, flux3_90_gdu2_vname, amb_data.counts_90_gdu2[*,2]
		oamb -> WriteVar, flux4_90_gdu2_vname, amb_data.counts_90_gdu2[*,3]
		
		;Errors
		oamb -> WriteVar, delta1_90_gdu1_vname, amb_data.delta_90_gdu1[*,0]
		oamb -> WriteVar, delta2_90_gdu1_vname, amb_data.delta_90_gdu1[*,1]
		oamb -> WriteVar, delta3_90_gdu1_vname, amb_data.delta_90_gdu1[*,2]
		oamb -> WriteVar, delta4_90_gdu1_vname, amb_data.delta_90_gdu1[*,3]
		oamb -> WriteVar, delta1_90_gdu2_vname, amb_data.delta_90_gdu2[*,0]
		oamb -> WriteVar, delta2_90_gdu2_vname, amb_data.delta_90_gdu2[*,1]
		oamb -> WriteVar, delta3_90_gdu2_vname, amb_data.delta_90_gdu2[*,2]
		oamb -> WriteVar, delta4_90_gdu2_vname, amb_data.delta_90_gdu2[*,3]
	endif else begin
		;Flux
		oamb -> WriteVar, flux1_90_gdu1_vname, amb_data.counts_90_gdu1
		oamb -> WriteVar, flux1_90_gdu2_vname, amb_data.counts_90_gdu2
		
		;Errors
		oamb -> WriteVar, delta1_90_gdu1_vname, amb_data.delta_90_gdu1
		oamb -> WriteVar, delta1_90_gdu2_vname, amb_data.delta_90_gdu2
	endelse

;------------------------------------------------------
; Write Trajectory Data                               |
;------------------------------------------------------
	
	;BRST
	if mode eq 'brst' then begin
		
		;DBCS Trajectories
		oamb -> WriteVar, traj1_dbcs_90_gdu1_vname, amb_data.traj_dbcs_90_gdu1[*,*,0]
		oamb -> WriteVar, traj2_dbcs_90_gdu1_vname, amb_data.traj_dbcs_90_gdu1[*,*,1]
		oamb -> WriteVar, traj3_dbcs_90_gdu1_vname, amb_data.traj_dbcs_90_gdu1[*,*,2]
		oamb -> WriteVar, traj4_dbcs_90_gdu1_vname, amb_data.traj_dbcs_90_gdu1[*,*,3]
		oamb -> WriteVar, traj1_dbcs_90_gdu2_vname, amb_data.traj_dbcs_90_gdu2[*,*,0]
		oamb -> WriteVar, traj2_dbcs_90_gdu2_vname, amb_data.traj_dbcs_90_gdu2[*,*,1]
		oamb -> WriteVar, traj3_dbcs_90_gdu2_vname, amb_data.traj_dbcs_90_gdu2[*,*,2]
		oamb -> WriteVar, traj4_dbcs_90_gdu2_vname, amb_data.traj_dbcs_90_gdu2[*,*,3]
		
		;GSE Trajectories
		oamb -> WriteVar, traj1_gse_90_gdu1_vname, amb_data.traj_gse_90_gdu1[*,*,0]
		oamb -> WriteVar, traj2_gse_90_gdu1_vname, amb_data.traj_gse_90_gdu1[*,*,1]
		oamb -> WriteVar, traj3_gse_90_gdu1_vname, amb_data.traj_gse_90_gdu1[*,*,2]
		oamb -> WriteVar, traj4_gse_90_gdu1_vname, amb_data.traj_gse_90_gdu1[*,*,3]
		oamb -> WriteVar, traj1_gse_90_gdu2_vname, amb_data.traj_gse_90_gdu2[*,*,0]
		oamb -> WriteVar, traj2_gse_90_gdu2_vname, amb_data.traj_gse_90_gdu2[*,*,1]
		oamb -> WriteVar, traj3_gse_90_gdu2_vname, amb_data.traj_gse_90_gdu2[*,*,2]
		oamb -> WriteVar, traj4_gse_90_gdu2_vname, amb_data.traj_gse_90_gdu2[*,*,3]
	
	;'SRVY'
	endif else begin
		;DBCS
		oamb -> WriteVar, traj1_dbcs_90_gdu1_vname, amb_data.traj_dbcs_90_gdu1
		oamb -> WriteVar, traj1_dbcs_90_gdu2_vname, amb_data.traj_dbcs_90_gdu2
		
		;GSE
		oamb -> WriteVar, traj1_gse_90_gdu1_vname, amb_data.traj_gse_90_gdu1
		oamb -> WriteVar, traj1_gse_90_gdu2_vname, amb_data.traj_gse_90_gdu2
	endelse
	
;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, oamb
	return, status
end