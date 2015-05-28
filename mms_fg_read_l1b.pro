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
;
; :Keywords:
;       TSTART:             in, optional, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, optional, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Returns:
;       FG_QL_STRUCT:       Structure of fluxgate quicklook data. Tags are::
;                               'epoch'  - TT2000 epoch times
;                               'b_bcs'  - 4xN (Bx, By, Bz, |B|) in BCS
;                               'b_omb'  - 4xN (Bx, By, Bz, |B|) in OMB
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
;       2015/05/19  -   Written by Matthew Argall
;-
function mms_fg_read_l1b, files, $
TSTART=tstart, $
TEND=tend
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
	
	;Level, Mode
	if ~(instr[0] eq 'dfg' || instr[0] eq 'afg') then message, 'Only AFG and DFG files are allowed.'
	if min(instr eq instr[0]) eq 0 then message, 'All files must be from the same instrument.'
	if min(level eq 'l1b')    eq 0 then message, 'Only L1B files are allowed.'
	if min(mode  eq mode[0])  eq 0 then message, 'All files must have the same telemetry mode.'

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
	b_bcs_name = mms_construct_varname(sc, instr, mode, OPTDESC='bcs')
	b_omb_name = mms_construct_varname(sc, instr, mode, OPTDESC='omb')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Read the data
	b_bcs = MrCDF_nRead(files, b_bcs_name, TSTART=tstart, TEND=tend, DEPEND_0=fg_epoch)
	b_omb = MrCDF_nRead(files, b_omb_name, TSTART=tstart, TEND=tend)

	;Return a structure
	fg_l1b = { epoch: fg_epoch, $
	           b_bcs: b_bcs, $
	           b_omb: b_omb  }

	return, fg_l1b
end
