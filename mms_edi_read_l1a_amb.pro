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
;
; :Keywords:
;       DIRECTORY:      in, optional, type=string, default=pwd
;                       Directory in which to find EDI data.
;       QUALITY:        in, optional, type=integer/intarr, default=pwd
;                       Quality of EDI beams to return. Can be a scalar or vector with
;                           values [0, 1, 2, 3].
;       TSTART:         in, optional, type=string
;                       Start time of the data interval to read, as an ISO-8601 string.
;       TEND:           in, optional, type=string
;                       End time of the data interval to read, as an ISO-8601 string.
;
; :Returns:
;       EDI:            Structure of EDI data. Fields are below.
;                             'EPOCH'          -  TT2000 times
;                             'EPOCH_ANGLE'    -  TT2000 times for AZIMUTH and POLAR
;                             'EPOCH_TIMETAGS' -  TT2000 times for PITCH_MODE and PACK_MODE
;                             'COUNTS_GDU1'    -  Electron counts from GDU1
;                             'ENERGY_GDU1'    -  Energy mode of GDU1
;                                                   2:      500 eV
;                             'COUNTS_GDU2'    -  Electron counts from GDU2
;                             'ENERGY_GDU2'    -  Energy mode of GDU2
;                             'AZIMUTH'        -  Azimuthal look direction in GDU1 system
;                             'POLAR'          -  Polar look direction in GDU1 system
;                             'PITCH_MODE'     -  Pitch angle mode
;                                                   0:      0 & 180 degrees
;                                                   2:      90 degrees
;                                                   1 or 3: Alternate 90, 0 & 180 degrees
;                             'PACK_MODE'      -  Correlator length
;                                                   0:      2 detector pads in use
;                                                   4:      4 detector pads in use
;
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
;       2015/06/01  -   Written by Matthew Argall
;-
function mms_edi_read_l1a_amb, files, $
QUALITY=quality, $
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
	
	;Ensure L1A EDI files were given
	if min(file_test(files, /READ)) eq 0 then message, 'Files must exist and be readable.'
	if min(instr   eq 'edi')   eq 0 then message, 'Only EDI files are allowed.'
	if min(level   eq 'l1a')   eq 0 then message, 'Only L1A files are allowed.'
	if min(optdesc eq 'amb')   eq 0 then message, 'Only EDI ambient-mode files are allowed.'
	if min(mode    eq mode[0]) eq 0 then message, 'All files must have the same telemetry mode.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	end

;-----------------------------------------------------
; Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Variable names for GDU1
	counts1_gdu1_name = mms_construct_varname(sc, instr, optdesc, 'gdu1_raw_counts1')
	energy_gdu1_name  = mms_construct_varname(sc, instr, optdesc, 'energy1')
	
	;Variable names for GDU2
	counts1_gdu2_name = mms_construct_varname(sc, instr, optdesc, 'gdu2_raw_counts1')
	energy_gdu2_name  = mms_construct_varname(sc, instr, optdesc, 'energy2')
	
	;Other variable names
	phi_name   = mms_construct_varname(sc, instr, optdesc, 'phi')
	theta_name = mms_construct_varname(sc, instr, optdesc, 'theta')
	pitch_name = mms_construct_varname(sc, instr, optdesc, 'pitchmode')
	pacmo_name = mms_construct_varname(sc, instr, optdesc, 'pacmo')

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])

	;Read the data for GD12
	counts_gdu1 = MrCDF_nRead(cdfIDs, counts1_gdu1_name, $
	                          DEPEND_0 = epoch_gdu1, $
	                          STATUS   = status, $
	                          TSTART   = tstart, $
	                          TEND     = tend)
	
	;Read the rest of the variables?
	if status eq 0 then begin
		energy_gdu1 = MrCDF_nRead(cdfIDs, energy_gdu1_name,  TSTART=tstart, TEND=tend)

		;Read the data for GD21
		counts_gdu2 = MrCDF_nRead(cdfIDs, counts1_gdu2_name, $
		                          DEPEND_0 = epoch_gdu2, $
		                          TSTART   = tstart, $
		                          TEND     = tend)
		energy_gdu2  = MrCDF_nRead(cdfIDs, energy_gdu2_name,  TSTART=tstart, TEND=tend)
		
		;Read other data
		phi        = MrCDF_nRead(cdfIDs, phi_name,   TSTART=tstart, TEND=tend, DEPEND_0=epoch_angle)
		theta      = MrCDF_nRead(cdfIDs, theta_name, TSTART=tstart, TEND=tend)
		pitch_mode = MrCDF_nRead(cdfIDs, pitch_name, TSTART=tstart, TEND=tend, DEPEND_0=epoch_timetag)
		pack_mode  = MrCDF_nRead(cdfIDs, pacmo_name, TSTART=tstart, TEND=tend)
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
	;All data
	edi_amb = { epoch_gdu1:       epoch_gdu1, $
	            epoch_gdu2:       epoch_gdu2, $
	            epoch_angle:      epoch_angle, $
	            epoch_timetag:    epoch_timetag, $
	            counts_gdu1:      counts_gdu1, $
	            energy_gdu1:      energy_gdu1, $
	            counts_gdu2:      counts_gdu2, $
	            energy_gdu2:      energy_gdu2, $
	            azimuth:          phi, $
	            polar:            theta, $
	            pitch_mode:       pitch_mode, $
	            pack_mode:        pack_mode $
	          }
	
	;Return the data
	return, edi_amb
end