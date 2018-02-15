; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ql_write
;
; PURPOSE:
;+
;   Write EDI qualty zero background counts to file.
;
; :Categories:
;    MMS, EDI, QL, Ambient
;
; :Params:
;       Q0_FILE:            in, required, type=string
;                           Name of the Q0 file to which data is written.
;       Q0_DATA:            in, required, type=struct
;                           EDI ambient data structure with the following fields::
;                               TT2000_GD12 - TT2000 time tags for the Gun1-Detector2 pair
;                               TT2000_GD21 - TT2000 time tags for the Gun2-Detector1 pair
;                               OPTICS      - Optics state.
;                               ENERGY_GD12 - Energy state for GD12
;                               ENERGY_GD21 - Energy state for GD21
;                               COUNTS_GD12 - Quality 0 counts for GD12
;                               COUNTS_GD21 - Quality 0 counts for GD21
;
; :Returns:
;       STATUS:             out, required, type=byte
;                           An error code. Values are:::
;                               OK      = 0
;                               Warning = 1-99
;                               Error   = 100-255
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
;       2016/02/17  -   Written by Matthew Argall
;       2016/03/23  -   Separated file creating from file writing. - MRA
;       2018/02/13  -   Handles cases when data from GDU1 and/or GDU2 are not available,
;                           as in the case of one-gun operations. - MRA
;-
function mms_edi_q0_ql_write, q0_file, q0_data
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Close and delete file
		if obj_valid(oq0) then obj_destroy, oq0
		if n_elements(q0_file) gt 0 && file_test(q0_file) then file_delete, q0_file
		
		;Report error
		if n_elements(status) eq 0 || status eq 0 then status = 100
		MrPrintF, 'LogErr'
		
		;Return
		return, status
	endif
	
	;Everything starts out ok
	status = 0
	
	;Dissect the file name
	mms_dissect_filename, q0_file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level

;------------------------------------;
; Check Data & Open File             ;
;------------------------------------;
	;
	; Check sizes
	;
	if ~isa(q0_data.tt2000_timetag, 'LONG64') then message, 'q0_data.tt2000_timetag must be LONG64.'
	if ~isa(q0_data.optics,         'BYTE')   then message, 'q0_data.optics must be BYTE.'
	if q0_data.n_gdu1 gt 0 then begin
		if ~isa(q0_data.tt2000_gdu1, 'LONG64') then message, 'q0_data.tt2000_gdu1 must be LONG64.'
		if ~isa(q0_data.energy_gdu1, 'UINT')   then message, 'q0_data.energy_gdu1 must be UINT.'
		if ~isa(q0_data.counts_gdu1, 'UINT')   then message, 'q0_data.counts_gdu1 must be UINT.'
	endif
	if q0_data.n_gdu2 gt 0 then begin
		if ~isa(q0_data.tt2000_gdu2, 'LONG64') then message, 'q0_data.tt2000_gdu2 must be LONG64.'
		if ~isa(q0_data.energy_gdu2, 'UINT')   then message, 'q0_data.energy_gdu2 must be UINT.'
		if ~isa(q0_data.counts_gdu2, 'UINT')   then message, 'q0_data.counts_gdu2 must be UINT.'
	endif

	;Open the CDF file
	oq0 = MrCDF_File(q0_file, /MODIFY)
	if obj_valid(oq0) eq 0 then message, 'Could not open file for writing: "' + q0_file + '".'

;------------------------------------------------------
; Variable Names                                      |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName[_coordSys][_paramQualifier][_subModeLevel][_mode][_level]
	prefix  = strjoin([sc, instr], '_') + '_'
	suffix  = '_' + strjoin([mode, level], '_')
	
	epoch_vname         = 'Epoch'
	epoch_gdu1_vname    = 'epoch_gdu1'
	epoch_gdu2_vname    = 'epoch_gdu2'
	epoch_timetag_vname = 'epoch_timetag'
	optics_vname        = prefix + 'optics_state' + suffix
	e_gdu1_vname        = prefix + 'energy_gdu1' + suffix
	e_gdu2_vname        = prefix + 'energy_gdu2' + suffix
	counts_gdu1_vname   = prefix + 'counts_gdu1' + suffix
	counts_gdu2_vname   = prefix + 'counts_gdu2' + suffix

;------------------------------------------------------
; Write Data                                          |
;------------------------------------------------------

	;Write variable data to file
	;   - All are detector quantities, so GD12 --> GDU2 and GD21 --> GDU2
	if q0_data.n_gdu1 gt 0 then oq0 -> WriteVar, epoch_gdu1_vname,    q0_data.tt2000_gdu1
	if q0_data.n_gdu2 gt 0 then oq0 -> WriteVar, epoch_gdu2_vname,    q0_data.tt2000_gdu2
	oq0 -> WriteVar, epoch_timetag_vname, q0_data.tt2000_timetag
	oq0 -> WriteVar, optics_vname,        q0_data.optics
	if q0_data.n_gdu1 gt 0 then oq0 -> WriteVar, e_gdu1_vname,        q0_data.energy_gdu1
	if q0_data.n_gdu2 gt 0 then oq0 -> WriteVar, e_gdu2_vname,        q0_data.energy_gdu2
	if q0_data.n_gdu1 gt 0 then oq0 -> WriteVar, counts_gdu1_vname,   q0_data.counts_gdu1
	if q0_data.n_gdu2 gt 0 then oq0 -> WriteVar, counts_gdu2_vname,   q0_data.counts_gdu2
	
;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	obj_destroy, oq0
	return, status
end