; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_create
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
;   Process EDI AMB L1A data, sorting counts by pitch angle instead of GDU and,
;   for burst data, calculate the pitch angle of each anode.
;
; :Categories:
;    MMS, EDI
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
;       2015/10/27  -   Written by Matthew Argall
;       2015/11/04  -   Calculate pitch angle for ambient data. - MRA
;       2016/01/29  -   Split the QL and L2 processes into separate programs. - MRA
;-
;*****************************************************************************************
;+
;   Process EDI AMB L1A data, sorting counts by pitch angle instead of GDU and,
;   for burst data, calculate the pitch angle of each anode.
;
;   Calling Sequences:
;       fname = mms_edi_ql_amb_create( fast_file )
;       fname = mms_edi_ql_amb_create( slow_file )
;       fname = mms_edi_ql_amb_create( brst_file )
;       fname = mms_edi_ql_amb_create( ..., tstart, tend )
;       fname = mms_edi_ql_amb_create( fast_file, slow_file )
;       fname = mms_edi_ql_amb_create( ..., tstart, thend )
;
; :Params:
;       SC:         in, required, type=string/strarr
;                   Either the spacecraft identifier ('mms1', 'mms2', 'mms3', 'mms4')
;                       of the spacecraft for which to process data or the EDI data
;                       file(s) to be processed. If files, they may be 'fast' and/or 'slow'
;                       mode data files.
;       MODE:       in, required, type=string/strarr
;                   Either the mode ('srvy', 'brst') of data to process or FGM
;                       data file names used to calculate pitch angle if 'brst' files
;                       are given for `SC`. 
;       TSTART:     in, optional, types=string
;                   An ISO-8601 string indicating the start time of the interval to process.
;       TEND:       in, optional, types=string
;                   An ISO-8601 string indicating the end time of the interval to process.
;
; :Keywords:
;       OUTDIR:     in, optional, type=string, default='/nfs/edi/amb/'
;                   Directory in which to save data.
;       STATUS:     out, required, type=byte
;                   An error code. Values are:::
;                       OK      = 0
;                       Warning = 1-99
;                       Error   = 100-255
;                           105      -  Trapped error
;
; :Returns:
;       EDI_OUT:    Structure of processed data. Fields are::
;                       TT2000_0    - TT2000 time tags for 0-pitch angle sorted data
;                       TT2000_180  - TT2000 time tags for 180-pitch angle sorted data
;                       TT2000_TT   - TT2000 time tags for packet-resolution data
;                       ENERGY_GDU1 - Electron energy for GDU1
;                       ENERGY_GDU2 - Electron energy for GDU2
;                       PACK_MODE   - Packing mode
;                       COUNTS1_0   - Counts1 data sorted by 0-degree pitch mode
;                       COUNTS1_180 - Counts1 data sorted by 180-degree pitch mode
;                       COUNTS2_0   - Counts2 data sorted by 0-degree pitch mode (brst only)
;                       COUNTS2_180 - Counts2 data sorted by 180-degree pitch mode (brst only)
;                       COUNTS3_0   - Counts3 data sorted by 0-degree pitch mode (brst only)
;                       COUNTS3_180 - Counts3 data sorted by 180-degree pitch mode (brst only)
;                       COUNTS4_0   - Counts4 data sorted by 0-degree pitch mode (brst only)
;                       COUNTS4_180 - Counts4 data sorted by 180-degree pitch mode (brst only)
;                       PA1_0       - Pitch angle associated with COUNTS1_0 (L2 only)
;                       PA1_180     - Pitch angle associated with COUNTS1_180 (L2 only)
;                       PA2_0       - Pitch angle associated with COUNTS2_0 (L2 only)
;                       PA2_180     - Pitch angle associated with COUNTS2_180 (L2 only)
;                       PA3_0       - Pitch angle associated with COUNTS3_0 (L2 only)
;                       PA3_180     - Pitch angle associated with COUNTS3_180 (L2 only)
;                       PA4_0       - Pitch angle associated with COUNTS4_0 (L2 only)
;                       PA4_180     - Pitch angle associated with COUNTS4_180 (L2 only)
;-
function mms_edi_amb_l2_create, amb_files, cal_file, tstart, tend, $
FGM_FILES=fgm_files, $
STATUS=status
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;TODO: Give error codes to specific errors.
		if n_elements(status) eq 0 || status eq 0 $
			then status = 105
		
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Everything starts out ok
	status = 0
	
;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Total number of files given
	nEDI = n_elements(amb_files)
	nCal = n_elements(cal_file)
	nFGM = n_elements(fgm_files)
	
	;Check if files exist and are readable
	if nEDI eq 0 then message, 'No EDI files given'
	if nCal eq 0 then message, 'No EDI calibration file given.'
	if min(file_test(amb_files, /READ, /REGULAR)) eq 0 $
		then message, 'EDI files must exist and be readable.'
	
	;Burst mode flag
	tf_brst =stregex(amb_files[0], 'brst', /BOOLEAN)

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Read Data
	;   - Automatically combines slow and fast survey data
	;   - Will check sc, instr, mode, level, optdesc
	;   - Expand AZIMUTH and POLAR angles to COUNTS time resolution
	edi = mms_edi_amb_l1a_read(amb_files, tstart, tend, /EXPAND_ANGLES)
	
	;Calibrations
	cals = mms_edi_amb_cal_read(cal_file)

;-----------------------------------------------------
; Apply Calibrations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Read Calibration File
	cal_cnts = mms_edi_amb_cal_apply(edi, cals, /ABSCAL)

	;Replace old counts with new counts
	edi = MrStruct_ReplaceValue(temporary(edi), temporary(cal_cnts))

;-----------------------------------------------------
; Sort by 0 and 180 Pitch Angle \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_brst $
		then counts_0_180 = mms_edi_amb_brst_sort_cnts(edi) $
		else counts_0_180 = mms_edi_amb_srvy_sort_cnts(edi)

;-----------------------------------------------------
; Pitch Angles (L2) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;If an error occurs while calculating pitch angles,
	;make a note and move on.
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogText', '--------------------------------------------'
		MrPrintF, 'LogText', '|      Error Calculating Pitch Angles      |'
		MrPrintF, 'LogText', '--------------------------------------------'
		MrPrintF, 'LogErr'
		nFGM = 0
		
	;Calculate pitch angles
	endif else if nFGM gt 0 then begin
		mms_dissect_filename, fgm_files, LEVEL=level_fgm
		
		;Read FGM data
		;   - L2 is preferred
		;   - Send warning about any other level
		case level_fgm[0] of
			'l2': fgm = mms_fg_read_l2(fgm_files, tstart, tend)
			'l2pre': begin
				MrPrintF, 'LogWarn', 'FGM L2PRE data provided. Expected L2.'
				fgm = mms_fg_read_l2pre(fgm_files, tstart, tend)
			endcase
			'ql': begin
				MrPrintF, 'LogWarn', 'FGM quick-look data provided. Expected L2.'
				fgm = mms_fg_read_ql(fgm_files, tstart, tend)
			endcase
			'l1b': begin
				MrPrintF, 'LogWarn', 'FGM L1B data provided. Expected L2.'
				fgm = mms_fg_read_l1b(fgm_files, tstart, tend)
			endcase
			else: message, 'Unexpected FGM data level: "' + level_fgm[0] + '".'
		endcase
		
		;Calculate pitch angles
		if tf_brst $
			then pa_0_180 = mms_edi_amb_brst_calc_pa(edi, temporary(fgm)) $
			else pa_0_180 = mms_edi_amb_srvy_calc_pa(edi, temporary(fgm))
	endif
	catch, /CANCEL

;-----------------------------------------------------
; Output Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Establish a new error handler
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, ''
	endif

	;Create the output structure
	edi_out = { tt2000_tt:   reform(edi.epoch_timetag), $
	            energy_gdu1: reform(edi.energy_gdu1), $
	            energy_gdu2: reform(edi.energy_gdu2), $
	            optics:      reform(edi.optics), $
	            pack_mode:   reform(edi.pack_mode) $
	          }
	edi = !Null

	;Burst mode counts
	edi_out = create_struct(edi_out, temporary(counts_0_180))

	;Pitch angle (burst only)
	if nFGM gt 0 then edi_out = create_struct(edi_out, temporary(pa_0_180))

	status = 0
	return, edi_out
end