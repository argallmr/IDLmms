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
;       COUNTS_GD12:        in, optional, type=lonarr
;                           Quality 0 electron counts from GDU1.
;       COUNTS_GD21:        in, optional, type=lonarr
;                           Quality 0 electron counts from GDU2.
;       ENERGY_GD12:        in, optional, type=lonarr
;                           Energy (eV) of electrons detected by GDU1.
;       ENERGY_GD21:        in, optional, type=lonarr
;                           Energy (eV) of electrons detected by GDU2.
;       TIME_GD12:          in, optional, type=3xN float
;                           Time tags for "GD12" variables.
;       TIME_GD21:          in, optional, type=3xN float
;                           Time tags for "GD21" variables.
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
pro mms_edi_l2_q0_read, files, tstart, tend, $
COUNTS_GD12=counts_gd12, $
COUNTS_GD21=counts_gd21, $
ENERGY_GD12=energy_gd12, $
ENERGY_GD21=energy_gd21, $
TIME_GD12=time_gd12, $
TIME_GD21=time_gd21
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
	if min(instr   eq 'edi')  eq 0 then message, 'Only EDI files are allowed.'
	if min(level   eq 'l2')   eq 0 then message, 'Only L2 files are allowed.'
	if min(mode    eq 'srvy') eq 0 then message, 'All files must be survey mode.'
	if min(stregex(optdesc, 'q0', /BOOLEAN)) eq 0 then message, 'All files must Q0 files.'

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
	counts_gd12_vname = mms_construct_varname(sc, instr, 'counts', 'gd12')
	counts_gd21_vname = mms_construct_varname(sc, instr, 'counts', 'gd21')
	energy_gd12_vname = mms_construct_varname(sc, instr, 'energy', 'gd12')
	energy_gd21_vname = mms_construct_varname(sc, instr, 'energy', 'gd21')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Everything is ok
	status = 0

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])

	;COUNTS & TIME GDU1
	if status eq 0 && ( arg_present(counts_gd12) || arg_present(time_gd12) ) $
		then counts_gd12 = MrCDF_nRead(cdfIDs, counts_gd12_vname,  $
		                               STATUS   = status, $
		                               TSTART   = tstart, $
		                               TEND     = tend, $
		                               DEPEND_0 = time_gd12)

	;COUNTS & TIME GDU2
	if status eq 0 && ( arg_present(counts_gd21) || arg_present(time_gd21) ) $
		then counts_gd21 = MrCDF_nRead(cdfIDs, counts_gd21_vname,  $
		                               STATUS   = status, $
		                               TSTART   = tstart, $
		                               TEND     = tend, $
		                               DEPEND_0 = time_gd21)
	
	;ENERGY GDU1
	if status eq 0 && arg_present(energy_gd12) $
		then energy_gd12 = MrCDF_nRead(cdfIDs, energy_gd12_vname, $
		                              STATUS = status, $
		                              TSTART = tstart, $
		                              TEND   = tend)
	
	;ENERGY GDU2
	if status eq 0 && arg_present(energy_gd12) $
		then energy_gd21 = MrCDF_nRead(cdfIDs, energy_gd12_vname, $
		                               STATUS = status, $
		                               TSTART = tstart, $
		                               TEND   = tend)
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor
end
