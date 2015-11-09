; docformat = 'rst'
;
; NAME:
;       mms_edi_ql_efield_read
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
;   Read EDI E-field quicklook data.
;
; :Categories:
;   MMS, EDI
;
; :Params:
;       FILES:              in, required, type=string/strarr
;                           Name(s) of the EDPIfile(s) to read.
;       TSTART:             in, optional, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, optional, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       E_DMPA:             in, optional, type=3xN float
;                           Electric field via the bestarg method in DMPA coordinates.
;       E_BC_DMPA:          in, optional, type=3xN float
;                           Electric field via the beam convergence method in DMPA coordinates.
;       V_EXB_DMPA:         in, optional, type=3xN float
;                           ExB drift velocity via the bestarg method in DMPA coordinates.
;       V_EXB_BC_DMPA:      in, optional, type=3xN float
;                           ExB drift velocity via the beam convergence method in DMPA coordinates.
;       QUALITY_BC:         in, optional, type=bytarr
;                           Quality flag for beam convergence data.
;       TIME:               in, optional, type=lon64arr (cdf_time_tt2000)
;                           Time tags for all variables.
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
;       2015/09/12  -   Written by Matthew Argall
;-
pro mms_edi_ql_efield_read, files, tstart, tend, $
E_DMPA=E_dmpa, $
E_BC_DMPA=E_bc_dmpa, $
QUALITY_BC=quality_bc, $
V_EXB_DMPA=v_ExB_dmpa, $
V_EXB_BC_DMPA=v_ExB_bc_dmpa, $
TIME=time
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(cdfIDs) gt 0 then $
			for i = 0, nFiles - 1 do if cdfIDs[i] ne 0 then cdf_close, cdfIDs[i]
		void = cgErrorMSG(/QUIET)
		return
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
	if min(instr   eq 'edi')  eq 0 then message, 'Only EDP files are allowed.'
	if min(level   eq 'ql')   eq 0 then message, 'Only Quick-Look (QL) files are allowed.'
	if min(mode    eq 'srvy') eq 0 then message, 'All files must be survey mode.'
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
	e_vname           = mms_construct_varname(sc, instr, 'E',       'dmpa')
	v_vname           = mms_construct_varname(sc, instr, 'v_ExB',   'dmpa')
	e_bc_vname        = mms_construct_varname(sc, instr, 'E',       'bc_dmpa')
	v_bc_vname        = mms_construct_varname(sc, instr, 'v_ExB',   'bc_dmpa')
	q_bc_vname        = mms_construct_varname(sc, instr, 'quality', 'bc')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Everything is ok
	status = 0

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])

	;EFIELD
	if status eq 0 && ( arg_present(E_dmpa) || arg_present(time) ) $
		then E_dmpa = MrCDF_nRead(cdfIDs, e_vname,  $
	                              STATUS   = status, $
	                              TSTART   = tstart, $
	                              TEND     = tend, $
	                              DEPEND_0 = time)
	
	;V_EXB
	if status eq 0 && arg_present(v_ExB_dmpa) $
		then v_ExB_dmpa = MrCDF_nRead(cdfIDs, v_vname, $
		                              STATUS = status, $
		                              TSTART = tstart, $
		                              TEND   = tend)
	
	;E_BC
	if status eq 0 && arg_present(E_bc_dmpa) $
		then E_bc_dmpa = MrCDF_nRead(cdfIDs, e_bc_vname, $
		                             STATUS = status, $
		                             TSTART = tstart, $
		                             TEND   = tend)
	
	
	;V_ExB_BC
	if status eq 0 && arg_present(v_ExB_bc_dmpa) $
		then v_ExB_bc_dmpa = MrCDF_nRead(cdfIDs, v_bc_vname, $
		                                 STATUS = status, $
		                                 TSTART = tstart, $
		                                 TEND   = tend)
	
	
	;QUALITY BC
	if status eq 0 && arg_present(quality_bc) $
		then quality_bc = MrCDF_nRead(cdfIDs, q_bc_vname, $
		                              STATUS = status, $
		                              TSTART = tstart, $
		                              TEND   = tend)
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor

;-----------------------------------------------------
; Remove FillVals \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(E_dmpa)        gt 0 then E_dmpa        = replace_fillval(E_dmpa,        -1e31)
	if n_elements(E_bc_dmpa)     gt 0 then E_bc_dmpa     = replace_fillval(E_bc_dmpa,     -1e31)
	if n_elements(v_ExB_dmpa)    gt 0 then v_ExB_dmpa    = replace_fillval(v_ExB_dmpa,    -1e31)
	if n_elements(v_ExB_bc_dmpa) gt 0 then v_ExB_bc_dmpa = replace_fillval(v_ExB_bc_dmpa, -1e31)
end
