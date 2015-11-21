; docformat = 'rst'
;
; NAME:
;       mms_fpi_l1b_desmoms_read
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
;   Read FPI L1B burst moments data. Reads both ion (dis) and electron (des) moment
;   files.
;
;   Calling Sequenc::
;      mms_fpi_sitl_read, FILES, TSTART, TEND
;      mms_fpi_sitl_read, SC, OPTDESC, TSTART, TEND
;
; :Categories:
;   MMS, FPI
;
; :Params:
;       FILES:              in, optional, type=string/strarr
;                           Name(s) of the AFG or DFG file(s) to be read. Either `FILES`
;                               or `SC` is required. See "calling sequence" above.
;       SC:                 in, optional, type=string
;                           MMS spacecraft ID of the file to be read.
;       OPTDESC:            in, optional, type=string
;                           Optional descriptor of the file name. Required if `SC` is
;                               given. Options are 'des-moms' and 'dis-moms'
;       TSTART:             in, required, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, required, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       N:                  out, optional, type=fltarr
;                           A named variable to receive the electron density.
;       J:                  out, optional, type=fltarr
;                           A named variable to receive the current density of the given species.
;       TIME:               out, optional, type=int64 (cdf_time_tt2000)
;                           A named variable to receive the time array. Time tags for
;                               `N_E`, `N_I`, `VI_DCS`, `VE_DCS`.
;       V_GSE:              out, optional, type=3xN fltarr
;                           A named variable to receive the bulk velocity.
;       P_GSE:              out, optional, type=6xN fltarr
;                           A named variable to receive the symmetric pressure tensor.
;       T_GSE:              out, optional, type=6xN fltarr
;                           A named variable to receive the symmetric temperature tensor.
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
;       2015/11/18  -   Written by Matthew Argall
;-
pro mms_fpi_l1b_moms_read, arg1, arg2, arg3, arg4, $
J=J, $
N=n, $
P_GSE=P_gse, $
T_GSE=T_gse, $
TIME=time, $
V_GSE=v_gse
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_sort = keyword_set(tf_sort)
	
	;Number of consecutive inputs with data
	nparams = n_elements(arg1)      eq 0 ? 0 : $
	              n_elements(arg2) eq 0 ? 1 : $
	              n_elements(arg3)   eq 0 ? 2 : $
	              n_elements(arg4)   eq 0 ? 3 : $
	              4
	
	;FGM QL SRVY file
	if nparams eq 4 then begin
		sc      = arg1
		optdesc = arg2
		fstart  = arg3
		fend    = arg4
	
		;Grab the files
		theFiles = mms_find_file(sc, 'fpi', 'brst', 'l1b', $
		                         COUNT     = nfiles, $
		                         OPTDESC   = optdesc, $
		                         SDC_ROOT  = sdc_dir, $
		                         SEARCHSTR = searchstr, $
		                         TIMEORDER = '%Y%M%d%H%m%S', $
		                         TSTART    = fstart, $
		                         TEND      = fend)
		if nfiles eq 0 then message, 'Unable to find FPI files: "' + searchstr + '".'
	endif else if nparams eq 3 then begin
		theFiles = arg1
		fstart   = arg2
		fend     = arg3
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
	if min(level eq 'l1b')  eq 0 then message, 'Only L1B files are allowed.'
;	if min(mode  eq mode[0]) eq 0 then tf_sort = 1 else tf_sort = keyword_set(tf_sort)
	if min(mode  eq 'brst') eq 0 then message, 'All files must be brst mode.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	end

;-----------------------------------------------------
; Which Variables to Read \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	get_time = arg_present(time)
	get_j    = arg_present(j)
	get_n    = arg_present(n)      || get_j || get_time
	get_v    = arg_present(v_gse)  || get_j
	get_p    = arg_present(P_gse)
	get_t    = arg_present(T_gse)

;-----------------------------------------------------
; File and Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Electrons or ions?
	particle = (strsplit(optdesc, '-', /EXTRACT))[0]

	t_name   = 'Epoch'
	
	;Moments
	n_name   = mms_construct_varname(sc, particle, 'numberDensity')
	vx_name  = mms_construct_varname(sc, particle, 'bulkX')
	vy_name  = mms_construct_varname(sc, particle, 'bulkY')
	vz_name  = mms_construct_varname(sc, particle, 'bulkZ')
	Pxx_name = mms_construct_varname(sc, particle, 'PresXX')
	Pxy_name = mms_construct_varname(sc, particle, 'PresXY')
	Pxz_name = mms_construct_varname(sc, particle, 'PresXZ')
	Pyy_name = mms_construct_varname(sc, particle, 'PresYY')
	Pyz_name = mms_construct_varname(sc, particle, 'PresYZ')
	Pzz_name = mms_construct_varname(sc, particle, 'PresZZ')
	Txx_name = mms_construct_varname(sc, particle, 'TempXX')
	Txy_name = mms_construct_varname(sc, particle, 'TempXY')
	Txz_name = mms_construct_varname(sc, particle, 'TempXZ')
	Tyy_name = mms_construct_varname(sc, particle, 'TempYY')
	Tyz_name = mms_construct_varname(sc, particle, 'TempYZ')
	Tzz_name = mms_construct_varname(sc, particle, 'TempZZ')

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Everything is ok
	status = 0

	;NI & TIME
	if status eq 0 && (get_n || get_time) $
		then n = MrCDF_nRead(theFiles, n_name, $
		                     DEPEND_0 = time, $
		                     STATUS   = statue, $
		                     TSTART   = fstart, $
		                     TEND     = fend)
	
	;V
	if status eq 0 && get_v then begin
		vx = MrCDF_nRead(theFiles, vx_name, $
		                 STATUS = status, $
		                 TSTART = fstart, $
		                 TEND   = fend)
		vy = MrCDF_nRead(theFiles, vy_name, $
		                 STATUS = status, $
		                 TSTART = fstart, $
		                 TEND   = fend)
		vz = MrCDF_nRead(theFiles, vz_name, $
		                 STATUS = status, $
		                 TSTART = fstart, $
		                 TEND   = fend)
		v_gse = [ temporary(vx), temporary(vy), temporary(vz) ]
	endif
	
	;P
	if status eq 0 && get_P then begin
		Pxx = MrCDF_nRead(theFiles, Pxx_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		Pxy = MrCDF_nRead(theFiles, Pxy_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		Pxz = MrCDF_nRead(theFiles, Pxz_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		Pyy = MrCDF_nRead(theFiles, Pyy_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		Pyz = MrCDF_nRead(theFiles, Pyz_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		Pzz = MrCDF_nRead(theFiles, Pzz_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		P_gse = [ temporary(Pxx), temporary(Pxy), temporary(Pxz), $
		                          temporary(Pyy), temporary(Pyz), $
		                                          temporary(Pzz) ]
	endif
	
	;T
	if status eq 0 && get_T then begin
		Txx = MrCDF_nRead(theFiles, Txx_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		Txy = MrCDF_nRead(theFiles, Txy_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		Txz = MrCDF_nRead(theFiles, Txz_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		Tyy = MrCDF_nRead(theFiles, Tyy_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		Tyz = MrCDF_nRead(theFiles, Tyz_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		Tzz = MrCDF_nRead(theFiles, Tzz_name, $
		                  STATUS = status, $
		                  TSTART = fstart, $
		                  TEND   = fend)
		T_gse = [ temporary(Txx), temporary(Txy), temporary(Txz), $
		                          temporary(Tyy), temporary(Tyz), $
		                                          temporary(Tzz) ]
	endif

	;Rethrow the error message
	if status ne 0 then message, /REISSUE_LAST

;-----------------------------------------------------
; Derived Products \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	npts = n_elements(time)

	;Currents
	;   - 1e9 converts C km / (s cm^3) to C / m^2 s
	;   - 1e6 converts A / m^2 to uA / m^2
	if get_j then begin
		J = constants('q') * 1e15 *  rebin(n, 3, npts) * v_gse
		if particle eq 'des' then J = -J
	endif

;-----------------------------------------------------
; Minimum Variance Frame \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;TODO: Rotate everything to LMN

end
