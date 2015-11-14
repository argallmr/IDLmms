; docformat = 'rst'
;
; NAME:
;       mms_edi_ql_tritof_read
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
;   Read EDI triangulation and time-of-flight results.
;
; :Categories:
;   MMS, EDI
;
; :Params:
;       FILES:              in, required, type=string/strarr
;                           Name(s) of the EDP file(s) to read.
;
; :Keywords:
;       TSTART:             in, optional, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, optional, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Returns:
;       EDI_QL:             Structure of EDP quicklook data. Tags are::
;                               'TT2000'      - TT2000 epoch times
;                               'E_DMPA'      - 3xN electric field (Ex, Ey, Ez) in DMPA
;                               'B_DMPA'      - 3xN magnetic field (Bx, By, Bz) in DMPA
;                               'V_ExB_DMPA'  - 3xN ExB drift velocity (Vx, Vy, Vz) in DMPA
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
;       2015/11/11  -   Written by Matthew Argall
;-
function mms_edi_ql_tritof_read, files, tstart, tend
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(cdfIDs) gt 0 then $
			for i = 0, nFiles - 1 do if cdfIDs[i] ne 0 then cdf_close, cdfIDs[i]
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
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
	if min(instr   eq 'edi')   eq 0 then message, 'Only EDI files are allowed.'
	if min(level   eq 'ql')    eq 0 then message, 'Only Quick-Look (QL) files are allowed.'
	if min(mode    eq mode[0]) eq 0 then message, 'All files must have the same telemetry mode.'
	if min(stregex(optdesc, 'efield', /BOOLEAN)) eq 0 then message, 'All files must efield-mode files.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	endif

;-----------------------------------------------------
; File and Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------	

	;Create the variable names
	e_tof_vname = mms_construct_varname(sc, instr, 'E',     'tof')
	v_tof_vname = mms_construct_varname(sc, instr, 'v_ExB', 'tof')
	e_tri_vname = mms_construct_varname(sc, instr, 'E',     'tri')
	v_tri_vname = mms_construct_varname(sc, instr, 'v_ExB', 'tri')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])

	;Read the data
	e_dmpa = MrCDF_nRead(cdfIDs, e_tof_vname,  $
	                     STATUS   = status, $
	                     TSTART   = tstart, $
	                     TEND     = tend, $
	                     DEPEND_0 = tt2000)
	
	;Is it worth reading the rest?
	if status eq 0 then begin
		e_tof_dmpa = MrCDF_nRead(cdfIDs, e_tof_vname, TSTART=tstart, TEND=tend)
		v_tof_dmpa = MrCDF_nRead(cdfIDs, v_tof_vname, TSTART=tstart, TEND=tend)
		e_tri_dmpa = MrCDF_nRead(cdfIDs, e_tri_vname, TSTART=tstart, TEND=tend)
		v_tri_dmpa = MrCDF_nRead(cdfIDs, v_tri_vname, TSTART=tstart, TEND=tend)
	endif else begin
		message, /REISSUE_LAST
	endelse
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor

;-----------------------------------------------------
; Remove Fill Values \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	e_tof_dmpa = replace_fillval(e_tof_dmpa, -1e31)
	v_tof_dmpa = replace_fillval(v_tof_dmpa, -1e31)
	e_tri_dmpa = replace_fillval(e_tri_dmpa, -1e31)
	v_tri_dmpa = replace_fillval(v_tri_dmpa, -1e31)

;-----------------------------------------------------
; Return Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	edi_ql = { tt2000:     tt2000, $
	           e_tof_dmpa: e_tof_dmpa, $
	           v_tof_dmpa: v_tof_dmpa, $
	           e_tri_dmpa: e_tri_dmpa, $
	           v_tri_dmpa: v_tri_dmpa  $
	         }

	return, edi_ql
end
