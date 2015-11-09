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
;       DATE:       in, optional, type=string/strarr, default=three days before current date
;                   Date of data to be processed.
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
pro mms_edi_ql_amb_procone, sc, mode, date, $
CREATE_LOG_FILE=create_log_file, $
OUTPUT_DIR=output_dir, $
LOG_DIR=log_dir
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
	if total(sc eq ['mms1', 'mms2', 'mms3', 'mms4']) ne 1 then message, 'SC must be {"mms1"|"mms2"|"mms3"|'mms4'}"
	if total(mode eq ['srvy', 'brst'])               ne 1 then message, 'MODE must be {"srvy"|"brst"}.'
	if n_elements(date) eq 0 || date eq '' then begin
		caldat, systime(/JULIAN, /UTC), month, day, year
		date   = string(FORMAT='(%"%04i-%02i-%02i")', year, month, day)
	endif
	
	;Constants
	create_log_file = keyword_set(create_log_file)
	if n_elements(output_dir) eq 0 then output_dir = '/nfs/edi/amb/ql'
	if n_elements(log_dir)    eq 0 then log_dir    = '/nfs/edi/amb/logs/'
	
	;Create a log file
	;   - If not, all errors will be written to console (IDL's stderr)
	if create_log_file then begin
		;Remove delimiters from date.
		fstart = strmid(date, 0, 4) + strmid(date, 5, 2) + strmid(date, 8, 2)
	
		;Create the file name
		log_fname = mms_edi_construct_filename(sc, 'edi', mode, 'ql', $
		                                       OPTDESC = 'amb', $
		                                       TSTART  = fstart, $
		                                       VERSION = 'v0.0.0')
		
		;Create the error logging object
		!Null = MrStdLog(log_fname)
	endif

;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	tstart = date + 'T00:00:00Z'
	tend   = date + 'T24:00:00Z'
	
	;Create the ambient quicklook file
	file = mms_edi_ql_amb_create(sc, mode, tstart, tend, $
	                             OUTDIR = filepath('', ROOT_DIR=outdir, SUBDIRECTORY=mode))
end