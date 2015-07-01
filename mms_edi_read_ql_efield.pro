; docformat = 'rst'
;
; NAME:
;       mms_edp_read_ql
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
;       2015/06/15  -   Written by Matthew Argall
;-
function mms_edi_read_ql_efield, files, $
TSTART=tstart, $
TEND=tend
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(cdfIDs) gt 0 then $
			for i = 0, nFiles - 1 do if cdfIDs[i] ne 0 then cdf_close, cdfIDs[i]
		void = cgErrorMSG(/QUIET)
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
	if min(instr   eq 'edi')   eq 0 then message, 'Only EDP files are allowed.'
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
	e_vname           = mms_construct_varname(sc, instr, 'E',            'dmpa')
	b_vname           = mms_construct_varname(sc, instr, 'B',            'dmpa')
	v_vname           = mms_construct_varname(sc, instr, 'v',            'ExB_dmpa')
	d_vname           = mms_construct_varname(sc, instr, 'd',            'dmpa')
	pos_vg1_vname     = mms_construct_varname(sc, instr, 'pos',          'virtual_gun1_dmpa')
	pos_vg2_vname     = mms_construct_varname(sc, instr, 'pos',          'virtual_gun2_dmpa')
	fv_gd12_vname     = mms_construct_varname(sc, instr, 'fv',           'gd12_dmpa')
	fv_gd21_vname     = mms_construct_varname(sc, instr, 'fv',           'gd21_dmpa')
	recnum_vname      = mms_construct_varname(sc, instr, 'recnum')
	recnum_gd12_vname = mms_construct_varname(sc, instr, 'recnum',       'gd12')
	recnum_gd21_vname = mms_construct_varname(sc, instr, 'recnum',       'gd21')
	q_gd12_vname      = mms_construct_varname(sc, instr, 'beam_quality', 'gd12')
	q_gd21_vname      = mms_construct_varname(sc, instr, 'beam_quality', 'gd21')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])

	;Read the data
	e_dmpa = MrCDF_nRead(cdfIDs, e_vname,  $
	                     STATUS   = status, $
	                     TSTART   = tstart, $
	                     TEND     = tend, $
	                     DEPEND_0 = tt2000)
	
	;Is it worth reading the rest?
	if status eq 0 then begin
		b_dmpa       = MrCDF_nRead(cdfIDs, b_vname,           TSTART=tstart, TEND=tend)
		v_dmpa       = MrCDF_nRead(cdfIDs, v_vname,           TSTART=tstart, TEND=tend)
		d_dmpa       = MrCDF_nRead(cdfIDs, d_vname,           TSTART=tstart, TEND=tend)
		pos_vg1_dmpa = MrCDF_nRead(cdfIDs, pos_vg1_vname,     TSTART=tstart, TEND=tend, DEPEND_0=tt2000_gd12)
		pos_vg2_dmpa = MrCDF_nRead(cdfIDs, pos_vg2_vname,     TSTART=tstart, TEND=tend, DEPEND_0=tt2000_gd21)
		fv_gd12_dmpa = MrCDF_nRead(cdfIDs, fv_gd12_vname,     TSTART=tstart, TEND=tend)
		fv_gd21_dmpa = MrCDF_nRead(cdfIDs, fv_gd21_vname,     TSTART=tstart, TEND=tend)
		recnum       = MrCDF_nRead(cdfIDs, recnum_vname,      TSTART=tstart, TEND=tend)
		recnum_gd12  = MrCDF_nRead(cdfIDs, recnum_gd12_vname, TSTART=tstart, TEND=tend)
		recnum_gd21  = MrCDF_nRead(cdfIDs, recnum_gd21_vname, TSTART=tstart, TEND=tend)
		q_gd12       = MrCDF_nRead(cdfIDs, q_gd12_vname,      TSTART=tstart, TEND=tend)
		q_gd21       = MrCDF_nRead(cdfIDs, q_gd21_vname,      TSTART=tstart, TEND=tend)
	endif else begin
		message, /REISSUE_LAST
	endelse
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor

;-----------------------------------------------------
; Return Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	edi_ql = { tt2000:       tt2000, $
	           tt2000_gd12:  tt2000_gd12, $
	           tt2000_gd21:  tt2000_gd21, $
	           e_dmpa:       e_dmpa, $
	           b_dmpa:       b_dmpa, $
	           v_ExB_dmpa:   v_dmpa, $
	           d_dmpa:       d_dmpa, $
	           pos_vg1_dmpa: pos_vg1_dmpa, $
	           pos_vg2_dmpa: pos_vg2_dmpa, $
	           fv_gd12_dmpa: fv_gd12_dmpa, $
	           fv_gd21_dmpa: fv_gd21_dmpa, $
	           recnum:       recnum, $
	           recnum_gd12:  recnum_gd12, $
	           recnum_gd21:  recnum_gd21, $
	           q_gd12:       q_gd12, $
	           q_gd21:       q_gd21 $
	         }

	return, edi_ql
end
