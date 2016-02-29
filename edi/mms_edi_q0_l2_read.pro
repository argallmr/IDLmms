; docformat = 'rst'
;
; NAME:
;       mms_edi_read_l1a_efield
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
;   Read EDI Q0 level 2 data.
;
; :Categories:
;   MMS, EDI, Bestarg
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
;       DIRECTORY:      in, optional, type=string, default=pwd
;                       Directory in which to find EDI data.
;       QUALITY:        in, optional, type=integer/intarr, default=pwd
;                       Quality of EDI beams to return. Can be a scalar or vector with
;                           values [0, 1, 2, 3].
;
; :Returns:
;       Q0_DATA:        Structure of q0 data. Fields are below.
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
;       2015/02/26  -   Written by Matthew Argall
;-
function mms_edi_q0_l2_read, files, tstart, tend
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
	nFiles    = n_elements(files)
	tf_sort   = 0B

	;Dissect the file name
	mms_dissect_filename, files, $
	                      INSTR   = instr, $
	                      LEVEL   = level, $
	                      MODE    = mode, $
	                      OPTDESC = optdesc, $
	                      SC      = sc, $
	                      VX      = vx, $
	                      VY      = vy, $
	                      VZ      = vz
	
	;Ensure L1A EDI files were given
	if min(file_test(files, /READ)) eq 0 then message, 'Files must exist and be readable.'
	if min(sc      eq sc[0])   eq 0 then message, 'All files must be from the same spacecraft.'
	if min(instr   eq 'edi')   eq 0 then message, 'Only EDI files are allowed.'
	if min(level   eq 'l2')    eq 0 then message, 'Only L1A files are allowed.'
	if min(optdesc eq 'q0')    eq 0 then message, 'Only EDI Q0 files are allowed.'
	if min(mode    eq mode[0]) eq 0 then message, 'All files must be the same mode.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	end

;-----------------------------------------------------
; Version Checking \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; No version constraints
	;

;-----------------------------------------------------
; Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	prefix = strjoin([sc, instr], '_') + '_'
	suffix = '_' + strjoin([mode, level], '_')
	
	;Variable Names
	epoch_vname            = 'Epoch'
	epoch_gdu1_vname       = 'epoch_gdu1'
	epoch_gdu2_vname       = 'epoch_gdu2'
	epoch_timetag_vname    = 'epoch_timetag'
	optics_vname           = prefix + 'optics_state'              + suffix
	e_gdu1_vname           = prefix + 'energy_gdu1'               + suffix
	e_gdu2_vname           = prefix + 'energy_gdu2'               + suffix
	counts_gdu1_vname      = prefix + 'counts_gdu1'               + suffix
	counts_gdu2_vname      = prefix + 'counts_gdu2'               + suffix
	
	traj_dbcs_gdu1_vname    = prefix + 'traj_dbcs_gdu1'             + suffix
	traj_dbcs_gdu1_lo_vname = prefix + 'traj_dbcs_gdu1_delta_minus' + suffix
	traj_dbcs_gdu1_hi_vname = prefix + 'traj_dbcs_gdu1_delta_plus'  + suffix
	traj_dbcs_gdu2_vname    = prefix + 'traj_dbcs_gdu2'             + suffix
	traj_dbcs_gdu2_lo_vname = prefix + 'traj_dbcs_gdu2_delta_minus' + suffix
	traj_dbcs_gdu2_hi_vname = prefix + 'traj_dbcs_gdu2_delta_plus'  + suffix
	
	traj_gse_gdu1_vname    = prefix + 'traj_gse_gdu1'             + suffix
	traj_gse_gdu1_lo_vname = prefix + 'traj_gse_gdu1_delta_minus' + suffix
	traj_gse_gdu1_hi_vname = prefix + 'traj_gse_gdu1_delta_plus'  + suffix
	traj_gse_gdu2_vname    = prefix + 'traj_gse_gdu2'             + suffix
	traj_gse_gdu2_lo_vname = prefix + 'traj_gse_gdu2_delta_minus' + suffix
	traj_gse_gdu2_hi_vname = prefix + 'traj_gse_gdu2_delta_plus'  + suffix
	
	traj_gsm_gdu1_vname    = prefix + 'traj_gsm_gdu1'             + suffix
	traj_gsm_gdu1_lo_vname = prefix + 'traj_gsm_gdu1_delta_minus' + suffix
	traj_gsm_gdu1_hi_vname = prefix + 'traj_gsm_gdu1_delta_plus'  + suffix
	traj_gsm_gdu2_vname    = prefix + 'traj_gsm_gdu2'             + suffix
	traj_gsm_gdu2_lo_vname = prefix + 'traj_gsm_gdu2_delta_minus' + suffix
	traj_gsm_gdu2_hi_vname = prefix + 'traj_gsm_gdu2_delta_plus'  + suffix

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])
	
	;Read the data
	counts_gdu1    = MrCDF_nRead(cdfIDs, counts_gdu1_vname,   TSTART=tstart, TEND=tend, DEPEND_0=epoch_gdu1)
	counts_gdu2    = MrCDF_nRead(cdfIDs, counts_gdu2_vname,   TSTART=tstart, TEND=tend, DEPEND_0=epoch_gdu2)
	traj_dbcs_gdu1 = MrCDF_nRead(cdfIDs, traj_dbcs_gdu1_vname, TSTART=tstart, TEND=tend)
	traj_dbcs_gdu2 = MrCDF_nRead(cdfIDs, traj_dbcs_gdu2_vname, TSTART=tstart, TEND=tend)
	traj_gse_gdu1  = MrCDF_nRead(cdfIDs, traj_gse_gdu1_vname, TSTART=tstart, TEND=tend)
	traj_gse_gdu2  = MrCDF_nRead(cdfIDs, traj_gse_gdu2_vname, TSTART=tstart, TEND=tend)
	traj_gsm_gdu1  = MrCDF_nRead(cdfIDs, traj_gsm_gdu1_vname, TSTART=tstart, TEND=tend)
	traj_gsm_gdu2  = MrCDF_nRead(cdfIDs, traj_gsm_gdu2_vname, TSTART=tstart, TEND=tend)
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor

;-----------------------------------------------------
; Output Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Initial structures
	q0_data = { tt2000_gdu1: temporary(epoch_gdu1), $
	            tt2000_gdu2: temporary(epoch_gdu2), $
	            counts_gdu1: temporary(counts_gdu1), $
	            counts_gdu2: temporary(counts_gdu2), $
	            traj_dbcs_gdu1: temporary(traj_dbcs_gdu1), $
	            traj_dbcs_gdu2: temporary(traj_dbcs_gdu2), $
	            traj_gse_gdu1: temporary(traj_gse_gdu1), $
	            traj_gse_gdu2: temporary(traj_gse_gdu2), $
	            traj_gsm_gdu1: temporary(traj_gsm_gdu1), $
	            traj_gsm_gdu2: temporary(traj_gsm_gdu2) $
	          }
	
	;Return the data
	return, q0_data
end