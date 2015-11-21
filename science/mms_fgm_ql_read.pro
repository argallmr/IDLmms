; docformat = 'rst'
;
; NAME:
;       mms_fg_ql_read
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
;   Read fluxgate quicklook data.
;
;   Calling Sequenc::
;      mms_fgm_ql_read, FILES, TSTART, TEND
;      mms_fgm_ql_read, SC, INSTR, MODE, LEVEL, TSTART, TEND
;
; :Categories:
;   MMS, DFG, AFG, FGM
;
; :Params:
;       FILES:              in, required, type=string/strarr
;                           Name(s) of the AFG or DFG file(s) to be read.
;       TSTART:             in, optional, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, optional, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Returns:
;       FG_QL_STRUCT:       Structure of fluxgate quicklook data. Tags are::
;                               'tt2000'      - TT2000 epoch times
;                               'b_dmpa'      - 4xN (Bx, By, Bz, |B|) in DMPA
;                               'b_gsm_dmpa'  - 4xN (Bx, By, Bz, |B|) in GSM-DMPA
;                               'pos_gse'     - (x, y, z) s/c position in GSE
;                               'pos_gsm'     - (x, y, z) s/c position in GSM
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
pro mms_fgm_ql_read, arg1, arg2, arg3, arg4, arg5, arg6, $
B_DMPA=b_dmpa, $
B_GSE=b_gse, $
B_GSM=b_gsm, $
B_GSM_DMPA=b_gsm_dmpa, $
EPOCH_STATE=epoch_state, $
LMN_FRAME=lmn_frame, $
POS_GSE=pos_gse, $
POS_GSM=pos_gsm, $
RADEC_GSE=radec_gse, $
SORT=tf_sort, $
TIME=time
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; In/Outputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Defaults
	tf_sort = keyword_set(tf_sort)
	
	;Number of consecutive inputs with data
	nparams = n_elements(arg1)     eq 0 ? 0 : $
	              n_elements(arg2) eq 0 ? 1 : $
	              n_elements(arg3) eq 0 ? 2 : $
	              n_elements(arg4) eq 0 ? 3 : $
	              n_elements(arg5) eq 0 ? 4 : $
	              n_elements(arg6) eq 0 ? 5 : $
	              6
	
	;What was requested?
	get_time       = arg_present(time)
	get_b_dmpa     = arg_present(b_dmpa)
	get_b_gsm_dmpa = arg_present(b_gsm_dmpa)
	get_b_gse      = arg_present(b_gse)
	get_b_gsm      = arg_present(b_gsm)
	get_r_gse      = arg_present(r_gse)
	get_r_gsm      = arg_present(r_gsm)
	get_t_state    = arg_present(epoch_state)
	
	;Does the data level contain the data product?
	level = nparams eq 6 ? arg4 : ''
	case level of
		'': ;do nothing
		'ql':    if (get_b_bse || get_b_gsm) $
			         then message, 'GSE and GSM coords not available in QL.'
		'l2pre': if (get_b_gsm_dmpa) $
			         then message, 'GSM_DMPA coords not available in QL.'
		else: message, 'LEVEL "' + level + '" not recognized.'
	endcase

;-----------------------------------------------------
; Get Files (?) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;FGM QL SRVY file
	if nparams eq 6 then begin
		sc     = arg1
		instr  = arg2
		mode   = arg3
		level  = arg4
		fstart = arg5
		fend   = arg6

		;Grab the files
		theFiles = mms_find_file(sc, instr, mode, level, $
		                         COUNT     = nfiles, $
		                         OPTDESC   = optdesc, $
		                         SDC_ROOT  = sdc_dir, $
		                         SEARCHSTR = searchstr, $
		                         TSTART    = fstart, $
		                         TEND      = fend)
		if nfiles eq 0 then message, 'Unable to find FGM files: "' + searchstr + '".'
	endif else if nparams eq 3 then begin
		theFiles = arg1
		fstart   = arg2
		fend     = arg3
	endif else begin
		message, 'Incorrect number of defined parameters.'
	endelse
;-----------------------------------------------------
; Check Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of files given
	nFiles = n_elements(theFiles)

	;Dissect the file name
	mms_dissect_filename, theFiles, $
	                      INSTR   = instr, $
	                      LEVEL   = level, $
	                      MODE    = mode, $
	                      OPTDESC = optdesc, $
	                      SC      = sc
	
	;Ensure L1A EDI files were given
	if min(file_test(theFiles, /READ)) eq 0 then message, 'Files must exist and be readable.'
	
	;AFG or DFG
	!Null = where( (instr ne 'afg') and (instr ne 'dfg'), count)
	if count gt 0 then message, 'Only AFG and DFG files are allowed.'
	
	;Level, Mode
	if min(level eq level[0]) eq 0 then message, 'All files must be the same data level.'
	if min(mode  eq mode[0])  eq 0 then tf_sort = 1 else tf_sort = keyword_set(tf_sort)
;	if min(mode  eq mode[0])  eq 0 then message, 'All files must have the same telemetry mode.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	end

;-----------------------------------------------------
; File and Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	get_time       = arg_present(time)
	get_b_dmpa     = arg_present(b_dmpa)
	get_b_dmpa_gsm = arg_present(b_dmpa_gsm)
	get_b_dmpa_gse = arg_present(b_dmpa_gse)
	get_b_gse      = arg_present(b_gse)
	get_b_gsm      = arg_present(b_gsm)

	;Create the variable names
	if level eq 'ql' then begin
		b_dmpa_name     = mms_construct_varname(sc, instr, mode,  'dmpa')
		b_gsm_dmpa_name = mms_construct_varname(sc, instr, mode,  'gsm_dmpa')
		pos_gse_name    = mms_construct_varname(sc, level, 'pos',   'gse')
		pos_gsm_name    = mms_construct_varname(sc, level, 'pos',   'gsm')
		radec_gse_name  = mms_construct_varname(sc, level, 'RADec', 'gse')
	endif else begin
		b_dmpa_name  = mms_construct_varname(sc, instr, mode,  level + '_dmpa')
		b_gse_name   = mms_construct_varname(sc, instr, mode,  level + '_gse')
		b_gsm_name   = mms_construct_varname(sc, instr, mode,  level + '_gsm')
		b_bcs_name   = mms_construct_varname(sc, instr, mode,  level + '_bcs')
		pos_gse_name = mms_construct_varname(sc, 'pos',   'gse')
		pos_gsm_name = mms_construct_varname(sc, 'pos',   'gsm')
	endelse

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Everything is ok
	status = 0

	;B_DMPA & TIME
	if status eq 0 && ( get_b_dmpa || get_time ) $
		then b_dmpa = MrCDF_nRead(theFiles, b_dmpa_name, $
		                          DEPEND_0 = time, $
		                          STATUS   = statue, $
		                          TSTART   = fstart, $
		                          TEND     = fend)

	;B_GSM_DMPA
	if status eq 0 && get_b_gsm_dmpa $
		then b_gsm_dmpa = MrCDF_nRead(theFiles, b_gsm_dmpa_name, $
		                              STATUS = status, $
		                              TSTART = fstart, $
		                              TEND   = fend)
		                          
	;B_GSE
	if status eq 0 && get_b_gse $
		then b_gse = MrCDF_nRead(theFiles, b_gse_name, $
		                         STATUS = status, $
		                         TSTART = fstart, $
		                         TEND   = fend)
		                          
	;B_GSM
	if status eq 0 && get_b_gsm $
		then b_gsm = MrCDF_nRead(theFiles, b_gsm_name, $
		                         STATUS = status, $
		                         TSTART = fstart, $
		                         TEND   = fend)
	
	;R_GSE & EPOCH_STATE
	if status eq 0 && ( get_r_gse || get_t_state ) $
		then pos_gse = MrCDF_nRead(theFiles, pos_gse_name, $
		                           DEPEND_0 = epoch_state, $
		                           STATUS = status, $
		                           TSTART = fstart, $
		                           TEND   = fend)
	
	;R_GSM
	if status eq 0 && get_r_gsm $
		then pos_gsm = MrCDF_nRead(theFiles, pos_gsm_name, $
		                           STATUS = status, $
		                           TSTART = fstart, $
		                           TEND   = fend)
	
	;RA & DEC
	if status eq 0 && arg_present(radec_gse) $
		then radec_gse = MrCDF_nRead(theFiles, radec_gse_name, $
		                             STATUS = status, $
		                             TSTART = fstart, $
		                             TEND   = fend)

	;Reissue the eror, if it occurred
	if status ne 0 then message, /REISSUE_LAST
;-----------------------------------------------------
; Minimum Variance Frame \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(lmn_frame) gt 0 then begin
		if arg_present(b_nml)     && n_elements(b_dmpa)     gt 0 then b_nml     = MrVector_Rotate(b_dmpa)
		if arg_present(b_gsm_nml) && n_elements(b_gsm_dmpa) gt 0 then b_gsm_nml = MrVector_Rotate(b_gsm_dmpa)
	endif
end
