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
;   Read EDI electric field mode data.
;
;   Steps:
;       1) Read data from file
;       2) Filter by quality
;       3) Sort by time (only if "slow" and "fast" files were given)
;       4) Compute firing vectors from firing angles
;       5) Compute chip and code periods
;       6) Determine time of flight overflow
;       7) Expand GDU energies to have same time tags as COUNTS1
;       7) Return structure or array of structures
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
;       EDI:            Structure of EDI data. Fields are below. If zero beams are
;                           detected from a GD pair, its fields will be missing from
;                           the output structure. Use COUNT_GD12 and COUNT_GD21 to test.
;                             'COUNT_GD12'       -  Number of points returned
;                             'TT2000_GD12'      -  Time (cdf_time_tt2000)
;                             'AZIMUTH_GD12'     -  Azimuthal firing angle (degrees)
;                             'POLAR_GD12'       -  Polar firing angle (degrees)
;                             'FV_GD12'          -  Firing vectors
;                             'TOF_GD12'         -  Time of flight (micro-seconds)
;                             'QUALITY_GD12'     -  Quality flag
;                             'ENERGY_GD12'      -  Energy
;                             'CODE_LENGTH_GD12' -  Code length
;                             'M_GD12'           -  Correlator length
;                             'N_GD12'           -  Correlator length
;                             'MAX_ADDR_GD12'    -  Max beam hit address
;
;                             'COUNT_GD21'       -  Number of points returned
;                             'TT2000_GD21'      -  Time (cdf_time_tt2000)
;                             'AZIMUTH_GD21'     -  Azimuthal firing angle (degrees)
;                             'POLAR_GD21'       -  Polar firing angle (degrees)
;                             'FV_GD21'          -  Firing vectors
;                             'TOF_GD21'         -  Time of flight (micro-seconds)
;                             'QUALITY_GD21'     -  Quality flag
;                             'ENERGY_GD21'      -  Energy
;                             'CODE_LENGTH_GD21' -  Code length
;                             'M_GD21'           -  Correlator length
;                             'N_GD21'           -  Correlator length
;                             'MAX_ADDR_GD21'    -  Max beam hit address
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
;       2015/02/23  -   Written by Matthew Argall
;-
function mms_edi_q0_l1a_read, files, tstart, tend, $
STATUS=status
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(cdfIDs) gt 0 then $
			for i = 0, nFiles - 1 do if cdfIDs[i] ne 0 then cdf_close, cdfIDs[i]
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Everything starts off ok
	status = 0

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
	if min(sc      eq sc[0])    eq 0 then message, 'All files must be from the same spacecraft.'
	if min(instr   eq 'edi')    eq 0 then message, 'Only EDI files are allowed.'
	if min(level   eq 'l1a')    eq 0 then message, 'Only L1A files are allowed.'
	if min(optdesc eq 'efield') eq 0 then message, 'Only EDI eField-mode files are allowed.'
	if min(mode    eq mode[0])  eq 0 then begin
		if total( (mode eq 'fast') + (mode eq 'slow') ) eq n_elements(mode) $
			then tf_sort = 1 $
			else message, 'All files must have the same MODE.'
	endif

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
	;Version v0.10.z of survey files did not have the correct
	;time tags for the optics state variable.
	if mode eq 'fast' || mode eq 'slow' then begin
		ibad = where( (vx eq 0) and (vy le 10), nbad )
		if nbad gt 0 then begin
			status = 101
			message, 'Optics state has bad time tags.'
		endif
	endif
	
	;Version v0.1.0, v0.2.0 brst files had the optics state as cdf_uint2
	;instead of a cdf_uint1 value.
	tf_fix_optics_datatype = 0B
	if mode eq 'brst' then begin
		ibad = where( vx eq 0, nbad )
		if nbad gt 0 then begin
			MrPrintF, 'LogWarn', 'Fixing optics datatype to byte.'
			tf_fix_optics_datatype = 1B
		endif
	endif

;-----------------------------------------------------
; Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;General variable names
	optics_name = mms_construct_varname(sc, instr, 'optics')
	
	;Variable names for GD12
	;   - TOF is a detector quantity, the rest are gun quantities
	phi_gd12_name       = mms_construct_varname(sc, instr, 'phi_gd12')
	theta_gd12_name     = mms_construct_varname(sc, instr, 'theta_gd12')
	q_gd12_name         = mms_construct_varname(sc, instr, 'sq_gd12')
	e_gd12_name         = mms_construct_varname(sc, instr, 'e_gd12')
	word15_gd12_name    = mms_construct_varname(sc, instr, 'word15_gd12')
	
	;Variable names for GD21
	phi_gd21_name       = mms_construct_varname(sc, instr, 'phi_gd21')
	theta_gd21_name     = mms_construct_varname(sc, instr, 'theta_gd21')
	q_gd21_name         = mms_construct_varname(sc, instr, 'sq_gd21')
	e_gd21_name         = mms_construct_varname(sc, instr, 'e_gd21')
	word15_gd21_name    = mms_construct_varname(sc, instr, 'word15_gd21')

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])
	
	;Optics
	;   - OPTICS is not returned when /STRUCTARR is set -- it would have to be inflated.
	optics = MrCDF_nRead(cdfIDs, optics_name, TSTART=tstart, TEND=tend, DEPEND_0=epoch_timetag)

	;Read the data for GD12
	phi_gd12 = MrCDF_nRead(cdfIDs, phi_gd12_name, $
	                       DEPEND_0 = epoch_gd12, $
	                       TSTART   = tstart, $
	                       TEND     = tend)
	theta_gd12     = MrCDF_nRead(cdfIDs, theta_gd12_name,  TSTART=tstart, TEND=tend)
	q_gd12         = MrCDF_nRead(cdfIDs, q_gd12_name,      TSTART=tstart, TEND=tend)
	e_gd12         = MrCDF_nRead(cdfIDs, e_gd12_name,      TSTART=tstart, TEND=tend)
	word15_gd12    = MrCDF_nRead(cdfIDs, word15_gd12_name, TSTART=tstart, TEND=tend)

	;Read the data for GD21
	phi_gd21 = MrCDF_nRead(cdfIDs, phi_gd21_name, $
	                       DEPEND_0 = epoch_gd21, $
	                       TSTART   = tstart, $
	                       TEND     = tend)
	theta_gd21     = MrCDF_nRead(cdfIDs, theta_gd21_name,  TSTART=tstart, TEND=tend)
	q_gd21         = MrCDF_nRead(cdfIDs, q_gd21_name,      TSTART=tstart, TEND=tend)
	e_gd21         = MrCDF_nRead(cdfIDs, e_gd21_name,      TSTART=tstart, TEND=tend)
	word15_gd21    = MrCDF_nRead(cdfIDs, word15_gd21_name, TSTART=tstart, TEND=tend)
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor

;-----------------------------------------------------
; Version Corrections \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;
	; ENERGY is on a packet-time basis and was not expanded
	; up to beam-time basis until v0.1.0 in burst and v?.?.?
	; in survey. When writing time tags for DEPEND_0 data,
	; we will have to compare the number of elements in ENERGY
	; with those in EPOCH_TIMETAG and EPOCH_GD[12,21].
	;
	tf_timetag = n_elements(energy) eq n_elements(epoch_timetag)
	
	;Fix optics state?
	if tf_fix_optics_datatype then optics = byte(optics)

;-----------------------------------------------------
; Find Quality 0 Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	iq0_gd12 = where(q_gd12 eq 0, count_gd12, NCOMPLEMENT=nbad_gd12)
	iq0_gd21 = where(q_gd21 eq 0, count_gd21, NCOMPLEMENT=nbad_gd21)
	
	;GD12
	if count_gd12 eq 0 then begin
		MrPrintF, 'LogWarn', 'No beams of desired quality for GD12.'
	
	;Filter
	endif else if nbad_gd12 gt 0 then begin
		phi_gd12       = phi_gd12[iq0_gd12]
		theta_gd12     = theta_gd12[iq0_gd12]
		q_gd12         = q_gd12[iq0_gd12]
		e_gd12         = e_gd12[iq0_gd12]
		word15_gd12    = word15_gd12[iq0_gd12]
		
		;Energy may be on a different time base
		if ~tf_timetag then epoch_gd12 = epoch_gd12[iq0_gd12]
	endif
	
	;GD21
	if count_gd21 eq 0 then begin
		MrPrintF, 'LogWarn', 'No beams of desired quality for GD21.'
	
	;Filter
	endif else if nbad_gd21 gt 0 then begin
		epoch_gd21     = epoch_gd21[iq0_gd21]
		phi_gd21       = phi_gd21[iq0_gd21]
		theta_gd21     = theta_gd21[iq0_gd21]
		q_gd21         = q_gd21[iq0_gd21]
		word15_gd21    = word15_gd21[iq0_gd21]
		
		;Energy may be on a different time base
		if ~tf_timetag then e_gd21= e_gd21[iq0_gd21]
	endif

	;No quality 0 data
	if count_gd12 + count_gd21 eq 0 then begin
		status = 102
		message, 'No Q0 data found.'
	endif

;-----------------------------------------------------
; Survey Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Do we want to make srvy data from fast and slow?
	;   - Must sort in time.
	if tf_sort then begin

		;Packet
		itt           = sort(epoch_timetag)
		epoch_timetag = epoch_timetag[itt]
		optics        = optics[itt]
		if tf_timetag then begin
			e_gd12 = e_gd12[itt]
			e_gd21 = e_gd21[itt]
		endif
		
		;GD12
		if count_gd12 gt 0 then begin
			it          = sort(epoch_gd12)
			phi_gd12    = phi_gd12[it]
			theta_gd12  = theta_gd12[it]
			q_gd12      = q_gd12[it]
			e_gd12      = e_gd12[it]
			word15_gd12 = word15_gd12[it]
			if ~tf_timetag then e_gd12 = e_gd12[it]
		endif
		
		;GD21
		if count_gd21 gt 0 then begin
			;Data
			it          = sort(epoch_gd21)
			epoch_gd21  = epoch_gd21[it]
			phi_gd21    = phi_gd21[it]
			theta_gd21  = theta_gd21[it]
			q_gd21      = q_gd21[it]
			e_gd21      = e_gd21[it]
			word15_gd21 = word15_gd21[it]
			if ~tf_timetag then e_gd21 = e_gd21[it]
		endif
	endif

;-----------------------------------------------------
; Output Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Initial structures
	edi_gd12 = {count_gd12: count_gd12}
	edi_gd21 = {count_gd21: count_gd21}
	edi      = { tt2000_timetag: reform(temporary(epoch_timetag)), $
	             optics:         reform(temporary(optics)) $
	           }

	;PACKETS
	if tf_timetag then begin
		edi = create_struct(edi, $
		                    'energy_gd12', reform(temporary(e_gd12)), $
		                    'energy_gd21', reform(temporary(e_gd21)) )
	endif

	;GD12
	if count_gd12 gt 0 then begin
		edi_gd12 = create_struct( edi_gd12, $
		                          'tt2000_gd12',   temporary(epoch_gd12), $
		                          'azimuth_gd12',  temporary(phi_gd12), $
		                          'polar_gd12',    temporary(theta_gd12), $
		                          'word15_gd12',   temporary(word15_gd12) $
		                         )
		if ~tf_timetag then edi_gd12 = create_struct(edi_gd12, 'energy_gd12', temporary(e_gd12))
	endif 
	
	;GD21
	if count_gd21 gt 0 then begin
		edi_gd21 = create_struct( edi_gd21, $
		                          'tt2000_gd21',   temporary(epoch_gd21), $
		                          'azimuth_gd21',  temporary(phi_gd21), $
		                          'polar_gd21',    temporary(theta_gd21), $
		                          'word15_gd21',   temporary(word15_gd21) $
		                         )
		if ~tf_timetag then edi_gd21 = create_struct(edi_gd21, 'energy_gd21', temporary(e_gd21))
	endif 
	
	;Combine structures
	q0_l1a = create_struct(temporary(edi), temporary(edi_gd12), temporary(edi_gd21))
	
	;Return the data
	if n_elements(status) eq 0 then status = 0
	return, q0_l1a
end