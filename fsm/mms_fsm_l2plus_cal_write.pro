; docformat = 'rst'
;
; NAME:
;    mms_fsm_l2plus_cal_write
;
; PURPOSE:
;+
;   Write FSM data to file.
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
;       FSM_DATA:           in, required, type=struct
;                           FSM data structure with the following fields::
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
;                           Names of the parent files required to make `fsm_data`.
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
function mms_fsm_l2plus_cal_write, fsm_file, data
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Close and delete the file, if it was created
		if obj_valid(ofsm) then obj_destroy, ofsm
		if n_elements(fsm_file) gt 0 && file_test(fsm_file) then file_delete, fsm_file
		
		;Report error
		if n_elements(status) eq 0 || status eq 0 then status = 100
		MrPrintF, 'LogErr'
		
		;Return
		return, status
	endif
	
	;Everything starts out ok
	status = 0
	
	;Parse the file name
	mms_dissect_filename, fsm_file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level, OPTDESC=optdesc

;------------------------------------;
; Check Data and Open File           ;
;------------------------------------;
	;
	; Check sizes
	;
	if ~isa(data['f'],           'FLOAT') then message, 'data["f"] must be FLOAT.'
	if ~isa(data['flag'],        'BYTE')  then message, 'data["flag"] must be BYTE.'
	if ~isa(data['amp_bins'],    'FLOAT') then message, 'data["amp_bins"] must be FLOAT.'
	if ~isa(data['phase_bins'],  'FLOAT') then message, 'data["phase_bins"] must be FLOAT.'
	if ~isa(data['psd_bins'],    'FLOAT') then message, 'data["psd_bins"] must be FLOAT.'
	if ~isa(data['amp_hist'],    'ULONG') then message, 'data["amp_hist"] must be ULONG.'
	if ~isa(data['phase_hist'],  'ULONG') then message, 'data["phase_hist"] must be ULONG.'
	if ~isa(data['psd_hist'],    'ULONG') then message, 'data["psd_hist"] must be ULONG.'
	if ~isa(data['amp_floor'],   'FLOAT') then message, 'data["amp_floor"] must be FLOAT.'
	if ~isa(data['phase_floor'], 'FLOAT') then message, 'data["phase_floor"] must be FLOAT.'
	if ~isa(data['psd_floor'],   'FLOAT') then message, 'data["psd_floor"] must be FLOAT.'

	;Open the CDF file
	ofsm = MrCDF_File(fsm_file, /MODIFY)
	if obj_valid(ofsm) eq 0 then message, 'Could not open file for writing: "' + fsm_file + '".'

;------------------------------------------------------
; Variable Names                                      |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName[_coordSys][_paramQualifier][_subModeLevel][_mode][_level]
	prefix  = strjoin([sc, instr], '_') + '_'
	suffix  = '_' + strjoin([mode, level], '_')
	
	t_vname           = 'Epoch'
	f_vname           = prefix + 'f'           + suffix
	flag_vname        = prefix + 'flag'        + suffix
	comp_index_vname  = prefix + 'comp_index'  + suffix
	amp_bins_vname    = prefix + 'amp_bins'    + suffix
	phase_bins_vname  = prefix + 'phase_bins'  + suffix
	psd_bins_vname    = prefix + 'psd_bins'    + suffix
	amp_hist_vname    = prefix + 'amp_hist'    + suffix
	phase_hist_vname  = prefix + 'phase_hist'  + suffix
	psd_hist_vname    = prefix + 'psd_hist'    + suffix
	amp_floor_vname   = prefix + 'amp_floor'   + suffix
	phase_floor_vname = prefix + 'phase_floor' + suffix
	psd_floor_vname   = prefix + 'psd_floor'   + suffix
	
;------------------------------------------------------
; Write Data                                          |
;------------------------------------------------------

	;Write variable data to file
;	ofsm -> WriteVar, t_vname,          data.tt2000
	ofsm -> WriteVar, f_vname,           data['f']
	ofsm -> WriteVar, flag_vname,        data['flag']
	ofsm -> WriteVar, amp_bins_vname,    data['amp_bins']
	ofsm -> WriteVar, phase_bins_vname,  data['phase_bins']
	ofsm -> WriteVar, psd_bins_vname,    data['psd_bins']
	ofsm -> WriteVar, amp_hist_vname,    transpose(data['amp_hist'],   [1,2,3,0])
	ofsm -> WriteVar, phase_hist_vname,  transpose(data['phase_hist'], [1,2,3,0])
	ofsm -> WriteVar, psd_hist_vname,    transpose(data['psd_hist'],   [1,2,3,0])
	ofsm -> WriteVar, amp_floor_vname,   transpose(data['amp_floor'],   [1,2,0])
	ofsm -> WriteVar, phase_floor_vname, transpose(data['phase_floor'], [1,2,0])
	ofsm -> WriteVar, psd_floor_vname,   transpose(data['psd_floor'],   [1,2,0])
	
;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, ofsm
	return, status
end