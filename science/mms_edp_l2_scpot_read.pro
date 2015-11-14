; docformat = 'rst'
;
; NAME:
;       mms_edp_l2_scpot_read
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
;   Read EDP level 2 spacecraft potential data.
;
; :Categories:
;   MMS, EDP
;
; :Params:
;       FILES:          in, required, type=string/strarr
;                       Name of the EDI e-field mode file or files to be read.
;       TSTART:         in, optional, type=string
;                       Start time of the data interval to read, as an ISO-8601 string.
;       TEND:           in, optional, type=string
;                       End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       BITMASK         in, optional, type=bytarr
;                       Status bitmask.
;       DCV:            in, optional, type=6xN float
;                       Probe-to-spacecraft potential for all 6 probes.
;       PSP:            in, optional, type=fltarr
;                       Probe-to-spacecraft potential, average over for all 6 probes.
;       QUALITY:        in, optional, type=bytarr
;                       Data quality (3=good, 0=bad)
;       SCPOT:          in, optional, type=fltarr
;                       Spacecraft potential
;       TIME:           in, optional, type=lon64arr (cdf_time_tt2000)
;                       Time tags for all variables.
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
pro mms_edp_l2_scpot_read, files, tstart, tend, $
BITMASK=bitmask, $
DCV=dcv, $
PSP=psp, $
QUALITY=quality, $
SCPOT=scpot, $
SORT=tf_sort, $
TIME=time
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(cdfIDs) gt 0 then $
			for i = 0, nFiles - 1 do cdf_close, cdfIDs[i]
		void = cgErrorMSG(/QUIET)
		return
	endif
	
	;Defaults
	tf_sort = keyword_set(tf_sort)
	
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
	if min(instr   eq 'edp')   eq 0 then message, 'Only ED[ files are allowed.'
	if min(level   eq 'l2')    eq 0 then message, 'Only L2 files are allowed.'
	if min(optdesc eq 'scpot') eq 0 then message, 'Only spacecraft potential files are allowed.'
	if min(mode    eq mode[0]) eq 0 then tf_sort = 1 else tf_sort = keyword_set(tf_sort)
;	if min(mode    eq mode[0]) eq 0 then message, 'All files must have the same telemetry mode.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	endif

;-----------------------------------------------------
; Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Variable names for GDU1
	bitmask_vname = mms_construct_varname(sc, instr, optdesc, 'bitmask')
	dcv_vname     = mms_construct_varname(sc, instr, 'dcv')
	psp_vname     = mms_construct_varname(sc, instr, 'psp')
	scpot_vname   = mms_construct_varname(sc, instr, 'scpot')
	q_vname       = mms_construct_varname(sc, instr, optdesc, 'quality')

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Everything is ok
	status = 0

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])

	;BITMASK
	if status eq 0 && arg_present(bitmask) $
		then bitmask = MrCDF_nRead(cdfIDs, bitmask_vname, $
		                           STATUS = status, $
		                           TSTART = tstart, $
		                           TEND   = tend)

	;DCV
	if status eq 0 && arg_present(dcv) $
		then dcv = MrCDF_nRead(cdfIDs, dcv_vname, $
		                       STATUS = status, $
		                       TSTART = tstart, $
		                       TEND   = tend)

	;PSP
	if status eq 0 && arg_present(psp) $
		then psp = MrCDF_nRead(cdfIDs, psp_vname, $
		                       STATUS = status, $
		                       TSTART = tstart, $
		                       TEND   = tend)
	;QUALITY
	if status eq 0 && arg_present(quality) $
		then quality = MrCDF_nRead(cdfIDs, q_vname, $
		                           STATUS = status, $
		                           TSTART = tstart, $
		                           TEND   = tend)
	
	;SCPOT & TIME
	if status eq 0 && ( arg_present(scpos) || arg_present(time) ) $
		then scpot = MrCDF_nRead(cdfIDs, scpot_vname, $
		                          DEPEND_0 = time, $
		                          STATUS   = status, $
		                          TSTART   = tstart, $
		                          TEND     = tend)
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor

;-----------------------------------------------------
; Remove Fill Values \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(scpot) gt 0 then scpot = replace_fillval(scpot, -1e31)
	if n_elements(dcv)   gt 0 then dcv   = replace_fillval(dcv,   -1e31)
	if n_elements(psp)   gt 0 then psp   = replace_fillval(psp,   -1e31)

;-----------------------------------------------------
; Sort By Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_sort then begin
		it   = sort(time)
		time = time[it]
		if n_elements(bitmask) gt 0 then bitmask = bitmask[it]
		if n_elements(dcv)     gt 0 then dcv     = dcv[it]
		if n_elements(psp)     gt 0 then psp     = psp[*,it]
		if n_elements(quality) gt 0 then quality = quality[it]
		if n_elements(scpot)   gt 0 then scpot   = scpot[it]
	endif
end