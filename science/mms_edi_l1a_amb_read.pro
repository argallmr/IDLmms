; docformat = 'rst'
;
; NAME:
;       mms_edi_read_l1a_amb
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
;   Read EDI level 1A ambient mode data.
;
; :Categories:
;   MMS, EDI
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
;       COUNTS_0:       out, optional, type=lonarr
;                       Named variable to receive 0 degree pitch angle counts. The
;                           GDU associated with each value is given in `GDU_0`, and
;                           its time tags are `T_0`.
;       COUNTS_180:     out, optional, type=lonarr
;                       Named variable to receive 180 degree pitch angle counts. The
;                           GDU associated with each value is given in `GDU_180`, and
;                           its time tags are `T_180`.
;       CRSF_10MIN:     out, optional, type=bytarr
;                       Named variable to receive CRSF 10MIin data, whatever that is.
;                           Time tags are `EPOCH_CRSF`.
;       DWELL:          out, optional, type=intarr
;                       Named variable to receive dwell data. Time tags are `EPOCH_TIMETAG`.
;       DELTA_T:        out, optional, type=fltarr
;                       Named variable to receive delta_t data. Time tags are `EPOCH_TIMETAG`.
;       ENERGY1:        out, optional, type=integer
;                       Named variable to receive energy state of GDU1. Time tags are
;                           `EPOCH_TIMETAG`. Values are::
;                               2:    500eV
;       ENERGY2:        out, optional, type=integer
;                       Named variable to receive energy state of GDU2. Time tags are
;                           `EPOCH_TIMETAG`. Values are::
;                               2:    500eV
;       EPOCH_ANGLE:    out, optional, type=long64arr (cdf_time_tt2000)
;                       Named variable to receive epoch times for `PHI`, `THETA`, `PITCH1`, `PITCH2`.
;       EPOCH_CRSF:     out, optional, type=long64arr (cdf_time_tt2000)
;                       Named variable to receive epoch times for `CRSF_10MIN`.
;       EPOCH_TIMETAG:  out, optional, type=long64arr (cdf_time_tt2000)
;                       Named variable to receive epoch times for `DELTA_T`, `DWELL`,
;                           `ENERGY1`, `ENERGY2`, `PITCH_MODE`, `PACK_MODE`, `OPTICS`.
;       PHI:            out, optional, type=integer
;                       Named variable to receive azimuthal look direction. Time tags are
;                           `EPOCH_ANGLE`.
;       GDU1_COUNTS1:   out, optional, type=integer
;                       Named variable to receive the raw electron counts detected by
;                           GDU1. Time tags are `TIME`.
;       GDU2_COUNTS1:   out, optional, type=integer
;                       Named variable to receive the raw electron counts detected by
;                           GDU2. Time tags are `TIME`.
;       GDU_0:          out, optional, type=bytarr
;                       Indicates which gun was used when collecting `COUNTS_0` data.
;                           Only returned if `COUNTS_0` is present. Values are::
;                               1 = GDU1
;                               2 = GDU2
;       GDU_180:        out, optional, type=bytarr
;                       Indicates which gun was used when collecting `COUNTS_180` data.
;                           Only returned if `COUNTS_180` is present. Values are::
;                               1 = GDU1
;                               2 = GDU2
;       OPTICS:         out, optional, type=bytarr
;                       Named variable to receive the optics state.
;       PITCH_GDU1:     out, optional, type=bytarr
;                       Named variable to receive the pitch angle (deg) of GDU1 counts.
;       PITCH_GDU2:     out, optional, type=bytarr
;                       Named variable to receive the pitch angle (deg) of GDU2 counts.
;       PITCH_MODE:     out, optional, type=bytarr
;                       Named variable to receive the operating pitch angle mode. Values are::
;                           0:      0 & 180 degrees
;                           2:      90 degrees
;                           1 or 3: Alternate 90, 0 & 180 degrees
;       PACK_MODE:      out, optional, type=bytarr
;                       Named variable to receive the correlator length. Values are::
;                           0:      2 detector pads in use
;                           4:      4 detector pads in use
;       SORT:           in, optional, type=boolean, default=0
;                       If set, data will be sorted according to their time tags. If
;                           the given files have more than one mode between them, SORT
;                           is automatically set to true. 
;       T_0:            out, optional, type=lon64arr (cdf_time_tt2000)
;                       Named variable to receive the time tags for `GDU_0`
;       T_180:          out, optional, type=lon64arr (cdf_time_tt2000)
;                       Named variable to receive the time tags for `GDU_180`
;       THETA:          out, optional, type=integer
;                       Named variable to receive polar look direction. Time tags are
;                           `EPOCH_ANGLE`.
;       TIME:           out, optional, type=lon64arr (cdf_time_tt2000)
;                       Named variable to receive the time tags for `GDU1_COUNTS1`, $
;                           `GDU2_COUNTS1`, `PITCH_GDU1`, `PITCH_GDU2`, `PHI`, and `THETA`.
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
;       2015/09/04  -   Written by Matthew Argall
;-
pro mms_edi_l1a_amb_read, files, tstart, tend, $
ANISOTROPY=anisotropy, $
COUNTS_0=COUNTS_0, $
COUNTS_180=counts_180, $
CRSF_10MIN=crsf_10min, $
DWELL=dwell, $
DELTA_T=delta_t, $
ENERGY1=energy1, $
ENERGY2=energy2, $
EPOCH_ANGLE=epoch_angle, $
EPOCH_CRSF=epoch_crsf, $
EPOCH_TIMETAG=epoch_timetag, $
PHI=phi, $
GDU1_COUNTS1=gdu1_counts1, $
GDU2_COUNTS1=gdu2_counts1, $
GDU_0=gdu_0, $
GDU_180=gdu_180, $
OPTICS=optics, $
PITCH_GDU1=pitch1, $
PITCH_GDU2=pitch2, $
PITCH_MODE=pitch_mode, $
PACK_MODE=pack_mode, $
SORT=tf_sort, $
T_0=t_0, $
T_180=t_180, $
THETA=theta, $
TIME=epoch
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
	
	;Ensure L1A EDI files were given
	if min(file_test(files, /READ)) eq 0 then message, 'Files must exist and be readable.'
	if min(instr   eq 'edi')   eq 0 then message, 'Only EDI files are allowed.'
	if min(level   eq 'l1a')   eq 0 then message, 'Only L1A files are allowed.'
	if min(optdesc eq 'amb')   eq 0 then message, 'Only EDI ambient-mode files are allowed.'
	if min(mode    eq mode[0]) eq 0 then tf_sort = 1 else tf_sort = keyword_set(tf_sort) ;message, 'All files must have the same telemetry mode.'

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
	tf_anisotropy   = arg_present(anisotropy)
	tf_counts_0     = arg_present(counts_0)   || tf_anisotropy
	tf_counts_180   = arg_present(counts_180) || tf_anisotropy
	tf_gdu1_counts1 = arg_present(gdu1_raw_counts1)
	tf_gdu2_counts1 = arg_present(gdu2_raw_counts1)
	tf_pitch1       = arg_present(pitch1)
	tf_pitch2       = arg_present(pitch2)
	
	if tf_counts_0 || tf_counts_180 then begin
		tf_gdu1_counts1 = 1
		tf_gdu2_counts1 = 1
		tf_pitch1       = 1
		tf_pitch2       = 1
	endif

;-----------------------------------------------------
; Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Variable names for GDU1
	counts1_gdu1_name = mms_construct_varname(sc, instr, optdesc, 'gdu1_raw_counts1')
	energy_gdu1_name  = mms_construct_varname(sc, instr, optdesc, 'energy1')
	pitch_gdu1_name   = mms_construct_varname(sc, instr, 'pitch', 'gdu1')
	
	;Variable names for GDU2
	counts1_gdu2_name = mms_construct_varname(sc, instr, optdesc, 'gdu2_raw_counts1')
	energy_gdu2_name  = mms_construct_varname(sc, instr, optdesc, 'energy2')
	pitch_gdu2_name   = mms_construct_varname(sc, instr, 'pitch', 'gdu2')
	
	;Other variable names
	delta_t_name     = 'delta_t'
	crfs_10min_name  = 'delta_t'
	epoch_crsf       = 'epoch_crsf'
	phi_name         = mms_construct_varname(sc, instr, optdesc, 'phi')
	theta_name       = mms_construct_varname(sc, instr, optdesc, 'theta')
	dwell_name       = mms_construct_varname(sc, instr, optdesc, 'dwell')
	pitch_name       = mms_construct_varname(sc, instr, optdesc, 'pitchmode')
	pacmo_name       = mms_construct_varname(sc, instr, optdesc, 'pacmo')
	optics_name      = mms_construct_varname(sc, instr, optdesc, 'optics')

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Everything is ok
	status = 0

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])

	;CRSF_10MIN
	if arg_present(crsf_10min) || arg_present(epoch_crsf) $
		then crsf_10min = MrCDF_nRead(cdfIDs, crfs_10min_name, $
		                              STATUS   = status, $
		                              TSTART   = tstart, $
		                              TEND     = tend, $
		                              DEPEND_0 = epoch_crsf)

	;DELTA_T
	if status eq 0 && arg_present(delta_t) $
		then delta_t = MrCDF_nRead(cdfIDs, delta_t_name, $
		                           TSTART = tstart, $
		                           TEND   = tend, $
		                           STATUS = status)
	
	;DWELL
	if status eq 0 && arg_present(dwell) $
		then optics = MrCDF_nRead(cdfIDs, dwell_name, $
		                         TSTART   = tstart, $
		                         TEND     = tend, $
		                         STATUS   = status, $
		                         DEPEND_0 = epoch_timetag)
		
	;ENERGY1
	if status eq 0 && arg_present(energy1) $
		then energy_gdu1 = MrCDF_nRead(cdfIDs, energy_gdu1_name, $
		                               TSTART   = tstart, $
		                               TEND     = tend, $
		                               STATUS   = status, $
		                               DEPEND_0 = epoch_timetag)
	
	;ENERGY2
	if status eq 0 && arg_present(energy2) $
		then energy_gdu2 = MrCDF_nRead(cdfIDs, energy_gdu2_name, $
		                               TSTART   = tstart, $
		                               TEND     = tend, $
		                               STATUS   = status, $
		                               DEPEND_0 = epoch_timetag)

	;GDU1 COUNTS & TIME
	if status eq 0 && ( tf_gdu1_counts1 || arg_present(time) ) $
		then gdu1_counts1 = MrCDF_nRead(cdfIDs, counts1_gdu1_name, $
		                                STATUS   = status, $
		                                TSTART   = tstart, $
		                                TEND     = tend, $
		                                DEPEND_0 = time)
	
	;GDU2 COUNTS
	if status eq 0 && tf_gdu2_counts1 $
		then gdu2_counts1 = MrCDF_nRead(cdfIDs, counts1_gdu2_name, $
		                                TSTART   = tstart, $
		                                TEND     = tend, $
		                                STATUS   = status, $
		                                DEPEND_0 = time)

	;OPTICS
	if status eq 0 && arg_present(optics) $
		then optics = MrCDF_nRead(cdfIDs, optics_name, $
		                          TSTART   = tstart, $
		                          TEND     = tend, $
		                          STATUS   = status, $
		                          DEPEND_0 = epoch_timetag)
	
	;PHI & EPOCH_ANGLE
	if status eq 0 && ( arg_present(phi) || arg_present(epoch_angle) ) $
		then phi = MrCDF_nRead(cdfIDs, phi_name, $
		                       STATUS   = status, $
		                       TSTART   = tstart, $
		                       TEND     = tend, $
		                       DEPEND_0 = epoch_angle)
	
	;PACK_MODE
	if status eq 0 && arg_present(pack_mode) $
		then pack_mode = MrCDF_nRead(cdfIDs, pacmo_name, $
		                             TSTART   = tstart, $
		                             TEND     = tend, $
		                             STATUS   = status, $
		                             DEPEND_0 = epoch_timetag)

	;PITCH_MODE & EPOCH_TIMETAG
	if status eq 0 && ( arg_present(pitch_mode) || arg_present(epoch_timetag) ) $
		then pitch_mode = MrCDF_nRead(cdfIDs, pitch_name, $
		                              STATUS   = status, $
		                              TSTART   = tstart, $
		                              TEND     = tend, $
		                              DEPEND_0 = epoch_timetag)
	
	;GDU1 PITCH
	if status eq 0 && tf_pitch1 $
		then pitch1 = MrCDF_nRead(cdfIDs, pitch_gdu1_name, $
		                          TSTART   = tstart, $
		                          TEND     = tend, $
		                          STATUS   = status, $
		                          DEPEND_0 = epoch_angle)
	
	;GDU2 PITCH
	if status eq 0 && tf_pitch2 $
		then pitch2 = MrCDF_nRead(cdfIDs, pitch_gdu2_name, $
		                          TSTART   = tstart, $
		                          TEND     = tend, $
		                          STATUS   = status, $
		                          DEPEND_0 = epoch_angle)
	
	;THETA
	if status eq 0 && arg_present(theta) $
		then theta = MrCDF_nRead(cdfIDs, theta_name, $
		                         TSTART   = tstart, $
		                         TEND     = tend, $
		                         STATUS   = status, $
		                         DEPEND_0 = epoch_angle)
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor
	
	;Error?
	if status ne 0 then message, /REISSUE_LAST

;-----------------------------------------------------
; Sort Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_sort then begin
		;TIME
		if n_elements(time) gt 0 then begin
			isort = sort(time)
			time  = time[isort]
			if n_elements(gdu1_counts1) gt 0 then gdu1_counts1 = gdu1_counts1[isort]
			if n_elements(gdu2_counts1) gt 0 then gdu2_counts1 = gdu2_counts1[isort]
		endif
		
		;EPOCH_TIMETAG
		if n_elements(epoch_timetag) gt 0 then begin
			isort         = sort(epoch_timetag)
			epoch_timetag = epoch_timetag[isort]
			if n_elements(energy1)    gt 0 then energy1    = energy1[isort]
			if n_elements(energy2)    gt 0 then energy2    = energy2[isort]
			if n_elements(pitch_mode) gt 0 then pitch_mode = pitch_mode[isort]
			if n_elements(pack_mode)  gt 0 then pack_mode  = pack_mode[isort]
			if n_elements(optics)     gt 0 then optics     = optics[isort]
			if n_elements(dwell)      gt 0 then dwell      = dwell[isort]
			if n_elements(delta_t)    gt 0 then delta_t    = delta_t[isort]
		endif
		
		;EPOCH_ANGLE
		if n_elements(epoch_angle) gt 0 then begin
			isort       = sort(epoch_angle)
			epoch_angle = epoch_angle[isort]
			if n_elements(phi)    gt 0 then phi    = phi[isort]
			if n_elements(theta)  gt 0 then theta  = theta[isort]
			if n_elements(pitch1) gt 0 then pitch1 = pitch1[isort]
			if n_elements(pitch2) gt 0 then pitch2 = pitch2[isort]
		endif
		
		;EPOCH_CRSF
		if n_elements(epoch_crsf) gt 0 then begin
			isort      = sort(epoch_crsf)
			epoch_crsf = epoch_crsf[isort]
			if n_elements(crsf_10min) gt 0 then crsf_10min = phi[isort]
		endif
	endif

;-----------------------------------------------------
; Counts 0 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_counts_0 then begin
		;Find pitch angle 0 cases
		ipitch_0_gdu1 = where(pitch1 eq 0, npitch_0_gdu1)
		ipitch_0_gdu2 = where(pitch2 eq 0, npitch_0_gdu2)
		
		;Select 0 pitch angle
		if npitch_0_gdu1 gt 0 && npitch_0_gdu2 gt 0 then begin
			t_0      = [ time[ipitch_0_gdu1],         time[ipitch_0_gdu2]         ]
			counts_0 = [ gdu1_counts1[ipitch_0_gdu1], gdu2_counts1[ipitch_0_gdu2] ]
			
			;Sort times
			isort    = sort(t_0)
			t_0      = t_0[isort]
			counts_0 = counts_0[isort]
			
			;Mark GDU
			gdu_0                    = bytarr(npitch_0_gdu1 + npitch_0_gdu2)
			gdu_0[0:npitch_0_gdu1-1] = 1B
			gdu_0[npitch_0_gdu1:npitch_0_gdu1+npitch_0_gdu2-1] = 2B
			gdu_0                    = gdu_0[isort]

		;Only GDU1 data
		endif else if npitch_0_gdu1 gt 0 then begin
			t_0      = time[ipitch_0_gdu1]
			counts_0 = gdu1_counts1[ipitch_0_gdu1]
			gdu_0    = replicate(1B, npitch_0_gdu1)
		
		;Only GDU2 data
		endif else if npitch_0_gdu2 gt 0 then begin
			t_0      = time[ipitch_0_gdu2]
			counts_0 = gdu2_counts1
			gdu_0    = replicate(2B, npitch_0_gdu2)
		endif else begin
			message, 'No 0 degree pitch angle data.'
		endelse
	endif

;-----------------------------------------------------
; Counts 180 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_counts_180 then begin
		;Find pitch angle 180 cases
		ipitch_180_gdu1 = where(pitch1 eq 180, npitch_180_gdu1)
		ipitch_180_gdu2 = where(pitch2 eq 180, npitch_180_gdu2)
		
		;Select 180 pitch angle
		if npitch_180_gdu1 gt 0 && npitch_180_gdu2 gt 0 then begin
			t_180      = [ time[ipitch_180_gdu1],         time[ipitch_180_gdu2]         ]
			counts_180 = [ gdu1_counts1[ipitch_180_gdu1], gdu2_counts1[ipitch_180_gdu2] ]
			
			;Sort times
			isort      = sort(t_180)
			t_180      = t_180[isort]
			counts_180 = counts_180[isort]
			
			;Mark GDU
			gdu_180                      = bytarr(npitch_180_gdu1 + npitch_180_gdu2)
			gdu_180[0:npitch_180_gdu1-1] = 1B
			gdu_180[npitch_180_gdu1:npitch_180_gdu1+npitch_180_gdu2-1] = 2B
			gdu_180                      = gdu_180[isort]
		
		;Only GDU1 data
		endif else if npitch_180_gdu1 gt 0 then begin
			t_180      = time[ipitch_180_gdu1]
			counts_180 = gdu1_counts1[ipitch_180_gdu1]
			gdu_180    = replicate(1B, npitch_180_gdu1)
		
		;Only GDU2 data
		endif else if npitch_180_gdu2 gt 0 then begin
			t_180      = time[ipitch_180_gdu2]
			counts_180 = gdu2_counts1[ipitch_180_gdu2]
			gdu_180    = replicate(2B, npitch_180_gdu2)
		endif else begin
			message, 'No 180 degree pitch angle data.'
		endelse
	endif

;-----------------------------------------------------
; Anisotropy \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_anisotropy $
		then anisotropy = float(counts_0) / float(counts_180)


end