; docformat = 'rst'
;
; NAME:
;       mms_rot_gse2gsm
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
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
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
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
;   Transform vectors from GSE coordinates to GSM coordinates via a coordinate system
;   transformation.
;
;   Calling Sequence:
;      mms_fgm_ql_read, FILES, TSTART, TEND
;      mms_fgm_ql_read, SC, OPTDESC, TSTART, TEND
;
; :Params:
;       FILES:              in, required, type=string/strarr
;                           Name(s) of the MEC file(s) to be read.
;       TSTART:             in, optional, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, optional, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Returns:
;       V_GSM:              `V_GSE` transformed to GSM coordinates.
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
;       2015-11-27  -   Written by Matthew Argall
;-
pro mms_mec_read, arg1, arg2, arg3, arg4, $
LVEC=lvec, $
LPHASE=lphase, $
QECI2BCS=qeci2bcs, $      ;GEI/J2000 -> BCS
QECI2GSM=qeci2gsm, $      ;GEI/J2000 -> GSM
QECI2GEO=qeci2geo, $      ;GEI/J2000 -> GEO
QECI2SM=qeci2sm, $        ;GEI/J2000 -> SM
QECI2GSE=qeci2gse, $      ;GEI/J2000 -> GSE
QECI22KGSE=qeci2gse2000, $  ;GEI/J2000 -> GSE2000
R_ECI=r_eci, $
R_GSM=r_gsm, $
R_GEO=r_geo, $
R_SM=r_sm, $
R_GSE=r_gse, $
R_2KGSE=r_gse2000, $
TIME=time, $
V_ECI=v_eci, $
V_GSM=v_gsm, $
V_GEO=v_geo, $
V_SM=v_sm, $
V_GSE=v_gse, $
V_2KGSE=v_gse2000
	compile_opt idl2
	on_error, 2

	;Number of consecutive defined parameters
	nparams = n_elements(arg1)     eq 0 ? 0 : $
	              n_elements(arg2) eq 0 ? 1 : $
	              n_elements(arg3) eq 0 ? 2 : $
	              n_elements(arg4) eq 0 ? 3 : $
	              4
	
	if nparams eq 4 then begin
		sc      = arg1
		optdesc = arg2
		fstart  = arg3
		fend    = arg4
	
		;Find the files
		theFiles = mms_find_file(sc, 'mec', 'srvy', 'l2', $
		                         COUNT     = nfiles, $
		                         OPTDESC   = optdesc, $
		                         SEARCHSTR = searchstr, $
		                         TSTART    = fstart, $
		                         TEND      = fend)
		if nfiles eq 0 then message, 'Unable to find EDP files: "' + searchstr[0] + '".'
	endif else if nparams eq 3 then begin
		theFiles = arg1
		tstart   = arg2
		tend     = arg3
	endif else begin
		message, 'Incorrect number of defined parameters.'
	endelse
	
;-----------------------------------------------------
; Check Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
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
	if min(instr eq 'mec')  eq 0 then message, 'All files must be MEC files.'
	if min(level eq 'l2')   eq 0 then message, 'All files must be L2.'
	if min(mode  eq 'srvy') eq 0 then message, 'All files must be srvy mode.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	end
	
;-----------------------------------------------------
; Variable Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	lvec_name       = mms_construct_varname(sc, instr, 'ang_mom', 'vec')
	lphase_name     = mms_construct_varname(sc, instr, 'ang_mom', 'phase')
	qeci2bcs_name   = mms_construct_varname(sc, instr, 'quat',    'eci_to_bcs')
	qeci2gsm_name   = mms_construct_varname(sc, instr, 'quat',    'eci_to_gsm')
	qeci2geo_name   = mms_construct_varname(sc, instr, 'quat',    'eci_to_geo')
	qeci2sm_name    = mms_construct_varname(sc, instr, 'quat',    'eci_to_sm')
	qeci2gse_name   = mms_construct_varname(sc, instr, 'quat',    'eci_to_gse')
	qeci2gse2k_name = mms_construct_varname(sc, instr, 'quat',    'eci_to_gse2000')
	reci_name       = mms_construct_varname(sc, instr, 'r',       'eci')
	rgsm_name       = mms_construct_varname(sc, instr, 'r',       'gsm')
	rgeo_name       = mms_construct_varname(sc, instr, 'r',       'geo')
	rsm_name        = mms_construct_varname(sc, instr, 'r',       'sm')
	rgse_name       = mms_construct_varname(sc, instr, 'r',       'gse')
	rgse2k_name     = mms_construct_varname(sc, instr, 'r',       'gse2000')
	veci_name       = mms_construct_varname(sc, instr, 'v',       'eci')
	vgsm_name       = mms_construct_varname(sc, instr, 'v',       'gsm')
	vgeo_name       = mms_construct_varname(sc, instr, 'v',       'geo')
	vsm_name        = mms_construct_varname(sc, instr, 'v',       'sm')
	vgse_name       = mms_construct_varname(sc, instr, 'v',       'gse')
	vgse2k_name     = mms_construct_varname(sc, instr, 'v',       'gse2000')
	
;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Everything is ok
	status = 0

	;LVEC
	if status eq 0 && arg_present(lvec) $
		then lvec = MrCDF_nRead(theFiles, lvec_name, $
		                        STATUS   = status, $
		                        TSTART   = fstart, $
		                        TEND     = fend)

	;LPHASE
	if status eq 0 && arg_present(lphase) $
		then lphase = MrCDF_nRead(theFiles, lphase_name, $
		                          STATUS   = status, $
		                          TSTART   = fstart, $
		                          TEND     = fend)

	;QECI2BCS & TIME
	if status eq 0 && ( arg_present(time) || arg_present(qeci2bcs) ) $
		then qeci2bcs = MrCDF_nRead(theFiles, qeci2bcs_name, $
		                            DEPEND_0 = time, $
		                            STATUS   = status, $
		                            TSTART   = fstart, $
		                            TEND     = fend)

	;QECI2GSM
	if status eq 0 && arg_present(qeci2gsm) $
		then qeci2gsm = MrCDF_nRead(theFiles, qeci2gsm_name, $
		                            STATUS   = status, $
		                            TSTART   = fstart, $
		                            TEND     = fend)

	;QECI2GEO
	if status eq 0 && arg_present(qeci2geo) $
		then qeci2geo = MrCDF_nRead(theFiles, qeci2geo_name, $
		                            STATUS   = status, $
		                            TSTART   = fstart, $
		                            TEND     = fend)

	;QECI2SM
	if status eq 0 && arg_present(qeci2sm) $
		then qeci2sm = MrCDF_nRead(theFiles, qeci2sm_name, $
		                           STATUS   = status, $
		                           TSTART   = fstart, $
		                           TEND     = fend)

	;QECI2GSE
	if status eq 0 && arg_present(qeci2gse) $
		then qeci2gse = MrCDF_nRead(theFiles, qeci2gse_name, $
		                            STATUS   = status, $
		                            TSTART   = fstart, $
		                            TEND     = fend)

	;QECI2GSE2000
	if status eq 0 && arg_present(qeci2gse2000) $
		then qeci2gse2000 = MrCDF_nRead(theFiles, qeci2gse2k_name, $
		                                STATUS   = status, $
		                                TSTART   = fstart, $
		                                TEND     = fend)

	;R_ECI
	if status eq 0 && arg_present(r_eci) $
		then r_eci = MrCDF_nRead(theFiles, reci_name, $
		                         STATUS   = status, $
		                         TSTART   = fstart, $
		                         TEND     = fend)

	;R_GSM
	if status eq 0 && arg_present(r_gsm) $
		then r_gsm = MrCDF_nRead(theFiles, rgsm_name, $
		                         STATUS   = status, $
		                         TSTART   = fstart, $
		                         TEND     = fend)

	;R_GEO
	if status eq 0 && arg_present(r_geo) $
		then r_geo = MrCDF_nRead(theFiles, rgeo_name, $
		                         STATUS   = status, $
		                         TSTART   = fstart, $
		                         TEND     = fend)

	;R_SM
	if status eq 0 && arg_present(r_sm) $
		then r_sm = MrCDF_nRead(theFiles, rsm_name, $
		                        STATUS   = status, $
		                        TSTART   = fstart, $
		                        TEND     = fend)

	;R_GSE
	if status eq 0 && arg_present(r_gse) $
		then r_gse = MrCDF_nRead(theFiles, rgse_name, $
		                         STATUS   = status, $
		                         TSTART   = fstart, $
		                         TEND     = fend)

	;R_GSE2K
	if status eq 0 && arg_present(r_gse2k) $
		then r_gse2k = MrCDF_nRead(theFiles, rgse2k_name, $
		                           STATUS   = status, $
		                           TSTART   = fstart, $
		                           TEND     = fend)

	;V_ECI
	if status eq 0 && arg_present(v_eci) $
		then v_eci = MrCDF_nRead(theFiles, veci_name, $
		                         STATUS   = status, $
		                         TSTART   = fstart, $
		                         TEND     = fend)

	;V_GSM
	if status eq 0 && arg_present(v_gsm) $
		then v_gsm = MrCDF_nRead(theFiles, vgsm_name, $
		                         STATUS   = status, $
		                         TSTART   = fstart, $
		                         TEND     = fend)

	;V_GEO
	if status eq 0 && arg_present(v_geo) $
		then v_geo = MrCDF_nRead(theFiles, vgeo_name, $
		                         STATUS   = status, $
		                         TSTART   = fstart, $
		                         TEND     = fend)

	;V_SM
	if status eq 0 && arg_present(v_sm) $
		then v_sm = MrCDF_nRead(theFiles, vsm_name, $
		                        STATUS   = status, $
		                        TSTART   = fstart, $
		                        TEND     = fend)

	;V_GSE
	if status eq 0 && arg_present(v_gse) $
		then v_gse = MrCDF_nRead(theFiles, vgse_name, $
		                         STATUS   = status, $
		                         TSTART   = fstart, $
		                         TEND     = fend)

	;V_GSE2K
	if status eq 0 && arg_present(v_gse2k) $
		then v_gse2k = MrCDF_nRead(theFiles, rgse2k_name, $
		                           STATUS   = status, $
		                           TSTART   = fstart, $
		                           TEND     = fend)

	;Reissue error
	if status ne 0 then message, /REISSUE_LAST
end