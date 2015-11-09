; docformat = 'rst'
;
; NAME:
;       mms_fpi_sitl_read
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
;   Read FPI sitl data data.
;
;   Calling Sequenc::
;      mms_fpi_sitl_read, FILES, TSTART, TEND
;      mms_fpi_sitl_read, SC, MODE, TSTART, TEND
;
; :Categories:
;   MMS, FPI
;
; :Params:
;       FILES:              in, required, type=string/strarr
;                           Name(s) of the AFG or DFG file(s) to be read.
;       TSTART:             in, optional, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, optional, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       N_E:                out, optional, type=fltarr
;                           A named variable to receive the electron density.
;       N_I:                out, optional, type=fltarr
;                           A named variable to receive the ion density.
;       TIME:               out, optional, type=int64 (cdf_time_tt2000)
;                           A named variable to receive the time array. Time tags for
;                               `N_E`, `N_I`, `VI_DCS`, `VE_DCS`.
;       VE_DCS:             out, optional, type=3xN fltarr
;                           A named variable to receive the electron bulk velocity.
;       VI_DCS:             out, optional, type=3xN fltarr
;                           A named variable to receive the ion bulk velocity.
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
;       2015/11/04  -   Written by Matthew Argall
;-
pro mms_fpi_sitl_read, files, tstart, tend, arg4, $
VE_DSC=ve_dsc, $
VI_DSC=vi_dsc, $
N_I=n_i, $
N_E=n_e, $
TIME=time
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_sort = keyword_set(tf_sort)
	
	;Number of consecutive inputs with data
	nparams = n_elements(files)      eq 0 ? 0 : $
	              n_elements(tstart) eq 0 ? 1 : $
	              n_elements(tend)   eq 0 ? 2 : $
	              n_elements(arg4)   eq 0 ? 3 : $
	              n_elements(arg5)   eq 0 ? 4 : $
	              5
	
	;FGM QL SRVY file
	if nparams eq 4 then begin
		sc     = files
		mode   = tstart
		fstart = tend
		fend   = arg4
	
		;Grab the files
		theFiles = mms_find_file(files, 'fpi', mode, 'sitl', $
		                         COUNT     = nfiles, $
		                         OPTDESC   = optdesc, $
		                         SDC_ROOT  = sdc_dir, $
		                         SEARCHSTR = searchstr, $
		                         TIMEORDER = '%Y%M%d%H%m%S', $
		                         TSTART    = fstart, $
		                         TEND      = fend)
		if nfiles eq 0 then message, 'Unable to find FPI files: "' + searchstr + '".'
	endif else if nparams eq 3 then begin
		theFiles = files
	endif else begin
		message, 'Incorrect number of parameters.'
	endelse
;-----------------------------------------------------
; Check Input Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
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
	
	;Level, Mode
	if min(instr eq 'fpi')   eq 0 then message, 'Only FPI files are allowed.'
	if min(level eq 'sitl')  eq 0 then message, 'Only SITL files are allowed.'
	if min(mode  eq mode[0]) eq 0 then tf_sort = 1 else tf_sort = keyword_set(tf_sort)
;	if min(mode  eq mode[0]) eq 0 then message, 'All files must have the same telemetry mode.'

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

	t_name   = 'Epoch'
	
	;Ions
	ni_name  = mms_construct_varname(sc, instr, 'DISnumberDensity')
	vix_name = mms_construct_varname(sc, instr, 'iBulkV_X',  'DSC')
	viy_name = mms_construct_varname(sc, instr, 'iBulkV_Y',  'DSC')
	viz_name = mms_construct_varname(sc, instr, 'iBulkV_Z',  'DSC')
	
	;Electrons
	ne_name  = mms_construct_varname(sc, instr, 'DESnumberDensity')
	vex_name = mms_construct_varname(sc, instr, 'iBulkV_X',  'DSC')
	vex_name = mms_construct_varname(sc, instr, 'iBulkV_Y',  'DSC')
	vex_name = mms_construct_varname(sc, instr, 'iBulkV_Z',  'DSC')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Everything is ok
	status = 0

	;NI & TIME
	if status eq 0 && ( arg_present(n_i) || arg_present(time) ) $
		then n_i = MrCDF_nRead(theFiles, ni_name, $
		                       DEPEND_0 = time, $
		                       STATUS   = statue, $
		                       TSTART   = fstart, $
		                       TEND     = fend)
		                          
	;VI
	if status eq 0 && arg_present(vi_dsc) then begin
		vix = MrCDF_nRead(theFiles, vix_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		viy = MrCDF_nRead(theFiles, viy_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		viz = MrCDF_nRead(theFiles, viz_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		vi_dsc = [ temporary(vix), temporary(viy), temporary(viz) ]
	endif

	;NE
	if status eq 0 && arg_present(n_e) $
		then n_e = MrCDF_nRead(theFiles, ne_name, $
		                       STATUS   = statue, $
		                       TSTART   = fstart, $
		                       TEND     = fend)
		                          
	;VE
	if status eq 0 && arg_present(vi_dsc) then begin
		vex = MrCDF_nRead(files, vex_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		vey = MrCDF_nRead(files, vey_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		vez = MrCDF_nRead(files, vez_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		ve_dsc = transpose( [ [temporary(vex)], [temporary(vey)], [temporary(vez)] ] )
	endif

;-----------------------------------------------------
; Minimum Variance Frame \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(lmn_frame) gt 0 then begin
		if arg_present(b_nml)     && n_elements(b_dmpa)     gt 0 then b_nml     = MrVector_Rotate(b_dmpa)
		if arg_present(b_gsm_nml) && n_elements(b_gsm_dmpa) gt 0 then b_gsm_nml = MrVector_Rotate(b_gsm_dmpa)
	endif
end
