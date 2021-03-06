; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ql_write_fa
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
;       AMB_FILE:           in, required, type=string
;                           Name of the ambient file to which data is written.
;       AMB_DATA:           in, required, type=struct
;                           EDI ambient data structure with the following fields::
;                               TT2000      - TT2000 time tags for counts data
;                               TT2000_TT   - TT2000 time tags for packet-resolution data
;                               OPTICS      - Optics state
;                               ENERGY_GDU1 - Electron energy for GDU1
;                               ENERGY_GDU2 - Electron energy for GDU2
;                               COUNTS1_0   - Counts1 data sorted by 0-degree pitch mode
;                               COUNTS1_180 - Counts1 data sorted by 180-degree pitch mode
;                               COUNTS2_0   - Counts2 data sorted by 0-degree pitch mode (brst only)
;                               COUNTS2_180 - Counts2 data sorted by 180-degree pitch mode (brst only)
;                               COUNTS3_0   - Counts3 data sorted by 0-degree pitch mode (brst only)
;                               COUNTS3_180 - Counts3 data sorted by 180-degree pitch mode (brst only)
;                               COUNTS4_0   - Counts4 data sorted by 0-degree pitch mode (brst only)
;                               COUNTS4_180 - Counts4 data sorted by 180-degree pitch mode (brst only)
;
; :Returns:
;       STATUS:             out, required, type=byte
;                           An error code. Values are:::
;                               OK      = 0
;                               Warning = 1-99
;                               Error   = 100-255
;                                   100      -  Unexpected trapped error
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
;       2016/03/23  -   Separated file creating from file writing. - MRA
;-
function mms_edi_amb_ql_write_fa, amb_file, amb_data
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
	
	;Parse the file name
	mms_dissect_filename, amb_file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level

;------------------------------------;
; Check Data & Create File           ;
;------------------------------------;
	;
	; Check sizes
	;
	if ~isa(amb_data.epoch_fa,      'LONG64') then message, 'amb_data.epoch_fa must be LONG64.'
	if ~isa(amb_data.epoch_timetag, 'LONG64') then message, 'amb_data.epoch_timetag must be LONG64.'
	if ~isa(amb_data.optics,        'BYTE')   then message, 'amb_data.optics must be BYTE.'
	if ~isa(amb_data.energy_gdu1,   'UINT')   then message, 'amb_data.energy_gdu1 must be UINT.'
	if ~isa(amb_data.energy_gdu2,   'UINT')   then message, 'amb_data.energy_gdu2 must be UINT.'
	if ~isa(amb_data.gdu_0,         'BYTE')   then message, 'amb_data.gdu_0 must be BYTE.'
	if ~isa(amb_data.gdu_180,       'BYTE')   then message, 'amb_data.gdu_180 must be BYTE.'
	if ~isa(amb_data.counts_0,      'ULONG')  then message, 'amb_data.counts_0 must be ULONG.'
	if ~isa(amb_data.counts_180,    'ULONG')  then message, 'amb_data.counts_180 must be ULONG.'

	;Open the CDF file
	oamb = MrCDF_File(amb_file, /MODIFY)
	if obj_valid(oamb) eq 0 then message, 'Could not open file for writing: "' + q0_file + '".'

;------------------------------------------------------
; Variable Names                                      |
;------------------------------------------------------
	; Variable naming convention
	;   scId_instrumentId_paramName_optionalDescriptor
	
	epoch_vname           = 'Epoch'
	t_tt_vname            = 'epoch_timetag'
	optics_vname          = mms_construct_varname(sc, instr, 'optics',  'state')
	e_gdu1_vname          = mms_construct_varname(sc, instr, 'energy',  'gdu1')
	e_gdu2_vname          = mms_construct_varname(sc, instr, 'energy',  'gdu2')
	gdu_0_vname           = mms_construct_varname(sc, instr, 'gdu',     '0')
	gdu_180_vname         = mms_construct_varname(sc, instr, 'gdu',     '180')
	counts1_0_vname       = mms_construct_varname(sc, instr, 'counts1', '0')
	counts2_0_vname       = mms_construct_varname(sc, instr, 'counts2', '0')
	counts3_0_vname       = mms_construct_varname(sc, instr, 'counts3', '0')
	counts4_0_vname       = mms_construct_varname(sc, instr, 'counts4', '0')
	counts1_180_vname     = mms_construct_varname(sc, instr, 'counts1', '180')
	counts2_180_vname     = mms_construct_varname(sc, instr, 'counts2', '180')
	counts3_180_vname     = mms_construct_varname(sc, instr, 'counts3', '180')
	counts4_180_vname     = mms_construct_varname(sc, instr, 'counts4', '180')

;------------------------------------------------------
; Write Data                                          |
;------------------------------------------------------

	;Write variable data to file
	oamb -> WriteVar, epoch_vname,   amb_data.epoch_fa
	oamb -> WriteVar, t_tt_vname,    amb_data.epoch_timetag
	oamb -> WriteVar, optics_vname,  amb_data.optics
	oamb -> WriteVar, e_gdu1_vname,  amb_data.energy_gdu1
	oamb -> WriteVar, e_gdu2_vname,  amb_data.energy_gdu2
	oamb -> WriteVar, gdu_0_vname,   amb_data.gdu_0
	oamb -> WriteVar, gdu_180_vname, amb_data.gdu_180

	;Put group variables by pitch angle.
	if mode eq 'brst' then begin
		oamb -> WriteVar, counts1_0_vname,   amb_data.counts_0[*,0]
		oamb -> WriteVar, counts2_0_vname,   amb_data.counts_0[*,1]
		oamb -> WriteVar, counts3_0_vname,   amb_data.counts_0[*,2]
		oamb -> WriteVar, counts4_0_vname,   amb_data.counts_0[*,3]
		oamb -> WriteVar, counts1_180_vname, amb_data.counts_180[*,0]
		oamb -> WriteVar, counts2_180_vname, amb_data.counts_180[*,1]
		oamb -> WriteVar, counts3_180_vname, amb_data.counts_180[*,2]
		oamb -> WriteVar, counts4_180_vname, amb_data.counts_180[*,3]
	endif else begin
		oamb -> WriteVar, counts1_0_vname,    amb_data.counts_0
		oamb -> WriteVar, counts1_180_vname,  amb_data.counts_180
	endelse

;------------------------------------------------------
; Close the File                                      |
;------------------------------------------------------
	;Close the file
	obj_destroy, oamb
	
	;Return
	if n_elements(status) eq 0 then status = 0
	return, status
end