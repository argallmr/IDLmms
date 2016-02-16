; docformat = 'rst'
;
; NAME:
;       mms_dsp_read_l2_bpsd
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
;   Read DSP level 2 b-field data.
;
; :Categories:
;   MMS, DSP
;
; :Params:
;       FILES:              in, required, type=string/strarr
;                           Name(s) of the DSP file(s) to be read.
;
; :Keywords:
;       TSTART:             in, optional, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, optional, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Returns:
;       BPSD_L2:            Structure of magnetometer power spectral density. Tags are::
;                               'tt2000' - TT2000 epoch times
;                               'freq'   - Frequencies of PSD bins
;                               'b1_psd' - Power spectral density of B1
;                               'b2_psd' - Power spectral density of B2
;                               'b3_psd' - Power spectral density of B3
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
;       2015/07/01  -   Written by Matthew Argall
;-
function mms_dsp_read_l2_bpsd, files, $
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
	
	;AFG or DFG
	!Null = where( (instr ne 'dsp') and (instr ne 'dsp'), count)
	if count gt 0 then message, 'Only AFG and DFG files are allowed.'
	
	;Level, Mode
	if min(instr   eq 'dsp')   eq 0 then message, 'Only DSP files are allowed.'
	if min(level   eq 'l2')    eq 0 then message, 'Only level 2 (L2) files are allowed.'
	if min(mode    eq mode[0]) eq 0 then message, 'All files must have the same telemetry mode.'
	if min(optdesc eq 'bpsd')  eq 0 then message, 'All files must be "bpsd" files.'

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
	b1_vname = mms_construct_varname(sc, instr, optdesc, 'scm1')
	b2_vname = mms_construct_varname(sc, instr, optdesc, 'scm1')
	b3_vname = mms_construct_varname(sc, instr, optdesc, 'scm1')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Read the data
	b1_psd = MrCDF_nRead(files, b1_vname, TSTART=tstart, TEND=tend, /COL_MAJOR, $
	                     DEPEND_0=dsp_epoch, DEPEND_1=freq)
	b2_psd = MrCDF_nRead(files, b2_vname, TSTART=tstart, TEND=tend, /COL_MAJOR)
	b3_psd = MrCDF_nRead(files, b3_vname, TSTART=tstart, TEND=tend, /COL_MAJOR)

	;Return a structure
	bpsd_l2 = { tt2000: dsp_epoch, $
	            freq:   freq, $
	            b1_psd: b1_psd, $
	            b2_psd: b2_psd, $
	            b3_psd: b3_psd $
	          }

	return, bpsd_l2
end
