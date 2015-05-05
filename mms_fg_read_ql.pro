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
;       SC:                 in, required, type=string
;                           MMS observatory/spacecraft number (e.g., 'mms1')
;       INSTR:              in, required, type=string
;                           The fluxgate instrument whos data is to be read. 'afg' or 'dfg'.
;       MODE:               in, required, type=string
;                           Data telemetry mode (e.g. 'srvy')
;       TSTART:             in, required, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, required, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;       FG_DIR:             in, required, type=string
;                           Directory in which to file digital sun sensor data.
;
; :Returns:
;       FG_QL_STRUCT:       Structure of fluxgate quicklook data. Tags are::
;                               'epoch'       - TT2000 epoch times
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
;       2015/05/03  -   Written by Matthew Argall
;-
function mms_fg_read_ql, sc, instr, mode, tstart, tend, fg_dir
	compile_opt idl2
	on_error, 2

	;Data directory
	if n_elements(fg_dir) eq 0 then cd, CURRENT=fg_dir
	
	;Required inputs -- TSTART and TEND would otherwise be ignored.
	if n_params() lt 5 then message, 'Not enough input parameters'
	
	;Correct instrument
	if max(strlowcase(instr) eq ['dfg', 'afg']) eq 0 $
		then message, 'Instrument (' + instr + ') must be {"AFG" | "DFG"}.'

;-----------------------------------------------------
; File and Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------	
	
	;Create the file name
	fname = mms_construct_filename(sc, instr, mode, 'ql', /TOKENS, DIRECTORY=fg_dir)

	;Create the variable names
	b_dmpa_name     = mms_construct_varname(sc, instr, 'srvy_dmpa')
	b_gsm_dmpa_name = mms_construct_varname(sc, instr, 'srvy_gsm_dmpa')
	pos_gse         = mms_construct_varname(sc, 'ql',  'pos_gse')
	pos_gsm         = mms_construct_varname(sc, 'ql',  'pos_gsm')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Search for files
	files = MrFile_Search(fname, /CLOSEST, $
	                      COUNT     = count, $
	                      TSTART    = tstart, $
	                      TEND      = tend, $
	                      TIMEORDER ='%Y%M%d')
	if count eq 0 then message, 'No fluxgate files found matching "' + fname + '".'

	;Read the data
	b_dmpa = MrCDF_nRead(files, b_dmpa_name, $
	                     DEPEND_0 = fg_epoch, $
	                     TSTART   = tstart, $
	                     TEND     = tend)
	b_gsm_dmpa = MrCDF_nRead(files, b_gsm_dmpa_name, TSTART=tstart, TEND=tend)
	pos_gse    = MrCDF_nRead(files, pos_gse,         TSTART=tstart, TEND=tend)
	pos_gsm    = MrCDF_nRead(files, pos_gsm,         TSTART=tstart, TEND=tend)

	;Return a structure
	fg_ql_struct = { epoch:      fg_epoch, $
	                 b_dmpa:     b_dmpa, $
	                 b_gsm_dmpa: b_gsm_dmpa, $
	                 pos_gse:    pos_gse, $
	                 pos_gsm:    pos_gsm }

	return, fg_ql_struct
end
