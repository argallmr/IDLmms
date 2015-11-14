; docformat = 'rst'
;
; NAME:
;       mms_edp_ql_read
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
;       mms_edp_ql_read, SC, MODE, TSTART, TEND
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
pro mms_edp_ql_read, files, tstart, tend, arg4, $
BITMASK=bitmask, $
E_DSL=e_dsl, $
OPTDESC=optdesc, $
QUALITY=quality, $
SORT=tf_sort, $
TIME=time
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_sort = keyword_set(tf_sort)
	
;-----------------------------------------------------
; Check Input Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of files given
	nFiles = n_elements(files)
	
	if stregex(files, 'mms[1-4]', /BOOLEAN, /FOLD_CASE) then begin
		sc     = files
		mode   = tstart
		fstart = tend
		fend   = arg4
		if n_elements(optdesc) eq 0 then optdesc = 'dce'
		
		;Combine fast and slow survey?
		_mode = mode eq 'srvy' ? ['fast', 'slow'] : mode
		
		;Find the files
		theFiles = mms_find_file(sc, 'edp', _mode, 'ql', $
		                         COUNT     = nfiles, $
		                         OPTDESC   = optdesc, $
		                         SEARCHSTR = searchstr, $
		                         TSTART    = fstart, $
		                         TEND      = fend, $
		                         TIMEORDER = '%Y%M%d%H%m%S')
		if nfiles eq 0 then message, 'Unable to find EDP files: "' + searchstr[0] + '".'
	endif else begin
		theFiles = files
	endelse

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
	if min(_instr eq 'edp')   eq 0 then message, 'Only EDP files are allowed.'
	if min(_level eq 'ql')    eq 0 then message, 'Only Quick-Look (QL) files are allowed.'
	if min(_mode  eq _mode[0]) eq 0 then tf_sort = 1 else tf_sort = keyword_set(tf_sort)
;	if min(mode  eq mode[0]) eq 0 then message, 'All files must have the same telemetry mode.'

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

	;Create the variable names
	e_vname    = mms_construct_varname(_sc, _instr, _optdesc, 'xyz_dsl')
	mask_vname = mms_construct_varname(_sc, _instr, _optdesc, 'bitmask')
	q_vname    = mms_construct_varname(_sc, _instr, _optdesc, 'quality')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Everything is OK
	status = 0

	;E_DSL & TIME
	if status eq 0 && ( arg_present(E_dsl) || arg_present(time) ) $
		then e_dsl = MrCDF_nRead(theFiles, e_vname, $
	                             DEPEND_0 = time, $
	                             STATUS   = status, $
	                             TSTART   = fstart, $
	                             TEND     = fend)
	
	;QUALITY
	if status eq 0 && arg_present(quality) $
		then quality = MrCDF_nRead(theFiles, q_vname, $
		                           STATUS = status, $
		                           TSTART = fstart, $
		                           TEND   = fend)
	
	;QUALITY
	if status eq 0 && arg_present(bitmask) $
		then bitmask = MrCDF_nRead(theFiles, mask_vname, $
		                           STATUS = status, $
		                           TSTART = fstart, $
		                           TEND   = fend)
	
	;Reissue error
	if status ne 0 then message, /REISSUE_LAST

;-----------------------------------------------------
; Remove FillVals \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(E_dsl) gt 0 then E_dsl = replace_fillval(E_dsl, -1e31)

;-----------------------------------------------------
; Sort By Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_sort then begin
		it   = sort(time)
		time = time[it]
		if n_elements(e_dsl)   gt 0 then e_dsl   = e_dsl[*,it]
		if n_elements(quality) gt 0 then quality = quality[it]
		if n_elements(bitmask) gt 0 then bitmask = bitmask[it]
	endif
end
