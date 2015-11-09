; docformat = 'rst'
;
; NAME:
;       mms_epd_eis_l1b_read
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;         contributors may  be used to endorse or promote products derived from this     ;
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
;   Read electric field double probe (EPS) L1B data.
;
; :Categories:
;   MMS, EPD, EIS
;
; :Params:
;       FILES:              in, required, type=string/strarr
;                           Name(s) of the EDP file(s) to read.
;       TSTART:             in, optional, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, optional, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       E_T0_COUNTS:    out, optional, type=bytarr
;                       Electron counts.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/09/22  -   Written by Matthew Argall
;-
pro mms_epd_eis_l1b_read, files, tstart, tend, $
E_COUNTS_T0=e_counts_t0, $
E_FLUX_T0=e_flux_t0, $
TIME=time
	compile_opt idl2
	on_error, 2
	
;-----------------------------------------------------
; Check Input Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of files given
	nFiles = n_elements(files)

	;Dissect the file name
	mms_dissect_filename, files, $
	                      INSTR   = instr, $
	                      LEVEL   = level, $
	                      MODE    = mode, $
	                      OPTDESC = optdesc, $
	                      SC      = sc
	
	;Ensure files exist
	if min(file_test(files, /READ)) eq 0 then message, 'Files must exist and be readable.'
	
	;Level, Mode
	if min(instr eq 'epd-eis') eq 0 then message, 'Only EPD-EIS files are allowed.'
	if min(level eq 'l1b')     eq 0 then message, 'Only L1B files are allowed.'
;	if min(mode  eq mode[0])   eq 0 then tf_sort = 1 else tf_sort = keyword_set(tf_sort)
	if min(mode  eq mode[0])   eq 0 then message, 'All files must have the same telemetry mode.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	endif
	instr = strjoin(strsplit(instr, '-', /EXTRACT), '_')

;-----------------------------------------------------
; File and Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	e_counts_t0_vname = mms_construct_varname(sc, instr, 'electronenergy_electron', 'counts_t0')
	e_flux_t0_vname   = mms_construct_varname(sc, instr, 'electronenergy_electron', 'flux_t0')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Everything is OK
	status = 0

	;E_DSL & TIME
	if status eq 0 && ( arg_present(e_counts_t0) || arg_present(time) ) $
		then e_counts_t0 = MrCDF_nRead(files, e_counts_t0_vname, $
		                               DEPEND_0 = time, $
		                               STATUS   = status, $
		                               TSTART   = tstart, $
		                               TEND     = tend)

	;E_DSL & TIME
	if status eq 0 && arg_present(e_flux_t0) $
		then e_flux_t0 = MrCDF_nRead(files, e_flux_t0_vname, $
		                               DEPEND_0 = time, $
		                               STATUS   = status, $
		                               TSTART   = tstart, $
		                               TEND     = tend)
end
