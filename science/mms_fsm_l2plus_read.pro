; docformat = 'rst'
;
; NAME:
;       mms_fsm_l2plus_read
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
;   Read electric field double probe (EDP) quick look (QL) data.
;
;   Calling Sequence::
;       mms_edp_ql_read, FILE, TSTART, TEND
;       mms_edp_ql_read, SC, MODE, OPTDESC, TSTART, TEND
;
; :Categories:
;   MMS, EDP, SDP, ADP
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
;       BITMASK:        out, optional, type=bytarr
;                       Named variable to receive the bitmask values. Depends on `TIME`.
;       E_DSL:          out, optional, type=3xN fltarr
;                       Named variable to receive the 3D electric field. Depends on `TIME`.
;       OPTDESC:        out, optional, type=bytarr
;                       File name optional descriptor. Used only if `SC` is given.
;       QUALITY:        out, optional, type=bytarr
;                       Named variable to receive the quality flag. Depends on `TIME`.
;       SORT:           in, optional, type=boolean, default=0
;                       If set, data will be sorted according to their time tags. If
;                           the given files have more than one mode between them, SORT
;                           is automatically set to true. 
;       TIME:           out, optional, type=lon64arr (cdf_time_tt2000)
;                       Named variable to receive the epoch time tags for `BITMASK`,
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
;       2015/09/07  -   Written by Matthew Argall
;-
pro mms_fsm_l2plus_read, arg1, arg2, arg3, arg4, arg5, $
B_BCS=b_bcs, $
B_DMPA=b_dmpa, $
B_GSE=b_gse, $
B_GSM=b_gsm, $
TIME=time
	compile_opt idl2
	on_error, 2
	
;-----------------------------------------------------
; Check Input Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of files given
	if max(stregex(arg1, '^mms[1-4]$', /BOOLEAN, /FOLD_CASE)) eq 1 then begin
		sc      = arg1
		mode    = arg2
		optdesc = arg3
		fstart  = arg4
		fend    = arg5
		
		;Find the files
		theFiles = mms_find_file(sc, 'dfg-scm', mode, 'l2plus', $
		                         COUNT     = nfiles, $
		                         DIRECTORY = '/nfs/fsm/temp', $
		                         OPTDESC   = optdesc, $
		                         SEARCHSTR = searchstr, $
		                         TSTART    = fstart, $
		                         TEND      = fend)
		if nfiles eq 0 then message, 'Unable to find FSM files: "' + searchstr[0] + '".'
	endif else begin
		theFiles = arg1
		fstart   = arg2
		fend     = arg3
	endelse
	
;-----------------------------------------------------
; Dissect File Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Dissect the file name
	mms_dissect_filename, theFiles, $
	                      INSTR   = _instr, $
	                      LEVEL   = _level, $
	                      MODE    = _mode, $
	                      OPTDESC = _optdesc, $
	                      SC      = _sc
	
	;Ensure files exist
	if min(file_test(theFiles, /READ)) eq 0 then message, 'Files must exist and be readable.'
	
	;Level, Mode
	if min(_instr eq 'dfg-scm') eq 0 then message, 'Only FSM files are allowed.'
	if min(_level eq 'l2plus')  eq 0 then message, 'Only L2PLUS files are allowed.'
	if min(_mode  eq _mode[0])  eq 0 then message, 'All files must be of the same mode.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		_sc      = _sc[0]
		_instr   = _instr[0]
		_mode    = _mode[0]
		_level   = _level[0]
		_optdesc = _optdesc[0]
	endif

;-----------------------------------------------------
; File and Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	fsm_intr ='dfg_scm'

	;Create the variable names
	b_bcs_vname  = mms_construct_varname(_sc, fsm_intr, 'b_bcs')
	b_dmpa_vname = mms_construct_varname(_sc, fsm_intr, 'b_dmpa')
	b_gse_vname  = mms_construct_varname(_sc, fsm_intr, 'b_gse')
	b_gsm_vname  = mms_construct_varname(_sc, fsm_intr, 'b_gsm')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Everything is OK
	status = 0

	;B_GSE & TIME
	if status eq 0 && ( arg_present(b_gse) || arg_present(time) ) $
		then b_gse = MrCDF_nRead(theFiles, b_gse_vname, $
	                             DEPEND_0 = time, $
	                             STATUS   = status, $
	                             TSTART   = fstart, $
	                             TEND     = fend)
	
	;B_BCS
	if status eq 0 && arg_present(b_bcs) $
		then b_bcs = MrCDF_nRead(theFiles, b_bcs_vname, $
		                         STATUS = status, $
		                         TSTART = fstart, $
		                         TEND   = fend)
	
	;B_DMPA
	if status eq 0 && arg_present(b_dmpa) $
		then b_dmpa = MrCDF_nRead(theFiles, b_dmpa_vname, $
		                          STATUS = status, $
		                          TSTART = fstart, $
		                          TEND   = fend)
	
	;B_GSM
	if status eq 0 && arg_present(b_gsm) $
		then b_gsm = MrCDF_nRead(theFiles, b_gsm_vname, $
		                         STATUS = status, $
		                         TSTART = fstart, $
		                         TEND   = fend)
	
	;Reissue error
	;   - Register error, but do not halt
	if status ne 0 then MrPrintF, 'LogErr'
end
