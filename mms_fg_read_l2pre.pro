; docformat = 'rst'
;
; NAME:
;       mms_fg_read_ql
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
; :Categories:
;   MMS, DFG, AFG
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
;                               TT2000       - TT2000 epoch times for all B
;                               TT2000_STATE - TT2000 epoch times for all positions
;                               B_GSE        - 4xN (Bx, By, Bz, |B|) in GSE
;                               B_GSM        - 4xN (Bx, By, Bz, |B|) in GSM
;                               B_DMPA       - 4xN (Bx, By, Bz, |B|) in DMPA
;                               B_BCS        - 4xN (Bx, By, Bz, |B|) in BCS
;                               FLAG         - Quality flag
;                               POS_GSE      - (x, y, z) s/c position in GSE
;                               POS_GSM      - (x, y, z) s/c position in GSM
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
;       2015/10/22  -   Written by Matthew Argall
;-
function mms_fg_read_l2pre, files, tstart, tend
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
	
	;Ensure L1A EDI files were given
	if min(file_test(files, /READ)) eq 0 then message, 'Files must exist and be readable.'
	
	;AFG or DFG
	!Null = where( (instr ne 'afg') and (instr ne 'dfg'), count)
	if count gt 0 then message, 'Only AFG and DFG files are allowed.'
	
	;Level, Mode
	if min(level eq 'l2pre')    eq 0 then message, 'Only l2pre files are allowed.'
	if min(mode  eq mode[0]) eq 0 then message, 'All files must have the same telemetry mode.'

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

	;Create the variable names
	b_gse_name   = mms_construct_varname(sc, instr, mode, level + '_gse')
	b_gsm_name   = mms_construct_varname(sc, instr, mode, level + '_gsm')
	b_dmpa_name  = mms_construct_varname(sc, instr, mode, level + '_dmpa')
	b_bcs_name   = mms_construct_varname(sc, instr, mode, level + '_bcs')
	flag_name    = mms_construct_varname(sc, instr, mode, level + '_flag')
	pos_gsm_name = mms_construct_varname(sc, 'pos', 'gsm')
	pos_gse_name = mms_construct_varname(sc, 'pos', 'gse')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Read the data
	b_gse = MrCDF_nRead(files, b_gse_name, $
	                     DEPEND_0 = tt2000, $
	                     TSTART   = tstart, $
	                     TEND     = tend)
	b_gsm   = MrCDF_nRead(files, b_gsm_name,   TSTART=tstart, TEND=tend)
	b_dmpa  = MrCDF_nRead(files, b_dmpa_name,  TSTART=tstart, TEND=tend)
	b_bcs   = MrCDF_nRead(files, b_bcs_name,   TSTART=tstart, TEND=tend)
	flag    = MrCDF_nRead(files, flag_name,    TSTART=tstart, TEND=tend)
	pos_gse = MrCDF_nRead(files, pos_gse_name, TSTART=tstart, TEND=tend, DEPEND_0=epoch_state)
	pos_gsm = MrCDF_nRead(files, pos_gse_name, TSTART=tstart, TEND=tend)

	;Return a structure
	fg_ql = { tt2000:       tt2000, $
	          tt2000_state: epoch_state, $
	          b_gse:        b_gse, $
	          b_gsm:        b_gsm, $
	          b_dmpa:       b_dmpa, $
	          b_bcs:        b_bcs, $
	          flag:         flag, $
	          pos_gsm:      pos_gsm, $
	          pos_gse:      pos_gse }

	return, fg_ql
end
