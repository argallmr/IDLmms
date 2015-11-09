; docformat = 'rst'
;
; NAME:
;    mms_edi_ql_amb_procone
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
;    MMS, EDI, QL
;
; :Params:
;       SC:         in, optional, type=string/strarr, default=['mms1'\, 'mms2'\, 'mms3'\, 'mms4']
;                   Spacecraft for which to process data. Options are:: mms1, mms2, mms3, or mms4
;       MODE:       in, optional, type=string/strarr, default=['srvy'\, 'brst']
;                   Telemetry mode of the data to be processed. Options are: 'srvy' or 'brst'
;       DATE_START: in, optional, type=string/strarr, default='2015-03-13'
;                   First date of data to be processed.
;                   Telemetry mode of the data to be processed. Options are: 'srvy' or 'brst'
;       DATE_END:   in, optional, type=string/strarr, default=three days before current date
;                   Last date of data to be processed.
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
pro mms_edi_ql_amb_procall, sc, mode, date_start, date_end
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Defaults
	if n_elements(sc)         eq 0 || sc[0]      eq '' then sc         = ['mms1', 'mms2', 'mms3', 'mms4']
	if n_elements(mode)       eq 0 || mode[0]    eq '' then mode       = ['srvy', 'brst']
	if n_elements(date_start) eq 0 || date_start eq '' then date_start = '2015-03-13'
	if n_elements(date_end)   eq 0 || date_end   eq '' then begin
		caldat, systime(/JULIAN, /UTC), month, day, year
		date_end = string(FORMAT='(%"%4i-%2i-%2i")', year, month, day)
	endif

	;Constants
	output_dir      = '/nfs/edi/amb/ql'
	log_dir         = '/nfs/edi/amb/logs'
	create_log_file = 1B

;-----------------------------------------------------
; Generate Dates \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Generate a series of dates
	syr  = fix(strmid(date_start, 0, 4))
	smo  = fix(strmid(date_start, 5, 2))
	sday = fix(strmid(date_start, 8, 2))
	eyr  = fix(strmid(date_end, 0, 4))
	emo  = fix(strmid(date_end, 5, 2))
	eday = fix(strmid(date_end, 8, 2))
	times = timegen( START = julday(smo, sday, syr), $
	                 FINAL = julday(emo, eday, eyr), $
	                 UNITS = 'days', STEP_SIZE = 1 )

	;Convert to start and end times
	caldat, times, mo, day, yr, hr, mnt, sec
	dates = string(yr,  FORMAT='(i04)') + '-' + $
	        string(mo,  FORMAT='(i02)') + '-' + $
	        string(day, FORMAT='(i02)')
	tstart = dates + 'T00:00:00Z'
	tend   = dates + 'T24:00:00Z'
	
;-----------------------------------------------------
; Loop Over Date & Mode \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	for i = 0, n_elements(tstart) - 1 do begin
	for j = 0, n_elements(mode)   - 1 do begin
	for k = 0, n_elements(sc)     - 1 do begin
		;Create the ambient quicklook file
		file = mms_edi_ql_amb_procone(sc[k], mode[j], tstart[i], tend[i], $
		                              CREATE_LOG_FILE = create_log_file, $
		                              OUTPUT_DIR      = output_Dir, $
		                              LOG_DIR         = log_dir)
	endfor ;tstart
	endfor ;mode
	endfor ;date
end