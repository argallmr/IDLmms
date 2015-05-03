; docformat = 'rst'
;
; NAME:
;       mms_edi_read_efieldmode
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
; :Categories:
;   MMS, EDI, Bestarg
;
; :Params:
;       SC:             in, required, type=string
;                       MMS observatory/spacecraft number (e.g., 'mms1')
;       MODE:           in, required, type=string
;                       Data telemetry mode.
;       LEVEL:          in, required, type=string
;                       Data level.
;       TSTART:         in, required, type=string
;                       Start time of the data interval to read, as an ISO-8601 string.
;       TEND:           in, required, type=string
;                       End time of the data interval to read, as an ISO-8601 string.
;       EDI_DIR:        in, required, type=string
;                       Directory in which to find EDI data.
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
;                           the output structure. Use N_GD12 and N_GD21 to test.
;                             'N_GD12'       -  Number of points returned
;                             'EPOCH_GD12'   -  Time (cdf_time_tt2000)
;                             'AZIMUTH_GD12' -  Azimuthal firing angle (degrees)
;                             'POLAR_GD12'   -  Polar firing angle (degrees)
;                             'FV_GD12'      -  Firing vectors
;                             'TOF_GD12'     -  Time of flight (micro-seconds)
;                             'Q_GD12'       -  Quality flag
;
;                             'N_GD21'       -  Number of points returned
;                             'EPOCH_GD21'   -  Time (cdf_time_tt2000)
;                             'AZIMUTH_GD21' -  Azimuthal firing angle (degrees)
;                             'POLAR_GD21'   -  Polar firing angle (degrees)
;                             'FV_GD21'      -  Firing vectors
;                             'TOF_GD21'     -  Time of flight (micro-seconds)
;                             'Q_GD21'       -  Quality flag
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
;       2015/05/01  -   Written by Matthew Argall
;-
function mms_edi_read_efieldmode, sc, mode, level, tstart, tend, $
DIRECTORY=edi_dir, $
QUALITY=quality
	compile_opt idl2
	on_error, 2
	
	if n_elements(edi_dir) eq 0 then cd, CURRENT=edi_dir

;-----------------------------------------------------
; File and Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create the file name
	fname = mms_construct_filename(sc, 'edi', mode, level, $
	                               OPTDESC   = 'efield', $
	                               /TOKENS, $
	                               DIRECTORY = edi_dir)
	
	;Variable names for GD12
	phi_gd12_name   = mms_construct_varname(sc, 'edi', 'phi_gd12')
	theta_gd12_name = mms_construct_varname(sc, 'edi', 'theta_gd12')
	tof_gd12_name   = mms_construct_varname(sc, 'edi', 'tof1_us')
	q_gd12_name     = mms_construct_varname(sc, 'edi', 'sq_gd12')
	
	;Variable names for GD21
	phi_gd21_name   = mms_construct_varname(sc, 'edi', 'phi_gd21')
	theta_gd21_name = mms_construct_varname(sc, 'edi', 'theta_gd21')
	tof_gd21_name   = mms_construct_varname(sc, 'edi', 'tof2_us')
	q_gd21_name     = mms_construct_varname(sc, 'edi', 'sq_gd21')

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Search for files
	files = MrFile_Search(fname, /CLOSEST, $
	                      COUNT     = count, $
	                      TSTART    = tstart, $
	                      TEND      = tend, $
	                      TIMEORDER ='%Y%M%d')
	if count eq 0 then message, 'No EDI files found in "' + edi_dir + '".'

	;Read the data for GD12
	phi_gd12 = MrCDF_nRead(files, phi_gd12_name, $
	                       DEPEND_0 = epoch_gd12, $
	                       TSTART   = tstart, $
	                       TEND     = tend)
	theta_gd12 = MrCDF_nRead(files, theta_gd12_name, TSTART=tstart, TEND=tend)
	tof_gd12   = MrCDF_nRead(files, tof_gd12_name,   TSTART=tstart, TEND=tend)
	q_gd12     = MrCDF_nRead(files, q_gd12_name,     TSTART=tstart, TEND=tend)

	;Read the data for GD21
	phi_gd21 = MrCDF_nRead(files, phi_gd21_name, $
	                       DEPEND_0 = epoch_gd21, $
	                       TSTART   = tstart, $
	                       TEND     = tend)
	theta_gd21 = MrCDF_nRead(files, theta_gd21_name, TSTART=tstart, TEND=tend)
	tof_gd21   = MrCDF_nRead(files, tof_gd21_name,   TSTART=tstart, TEND=tend)
	q_gd21     = MrCDF_nRead(files, q_gd21_name,     TSTART=tstart, TEND=tend)

;-----------------------------------------------------
; Filter by Quality? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(quality) gt 0 then begin
		void = MrIsMember(quality, q_gd12, iq_gd12, COUNT=n_gd12)
		void = MrIsMember(quality, q_gd21, iq_gd21, COUNT=n_gd21)
		
		;GD12
		if n_gd12 gt 0 then begin
			epoch_gd12 = epoch_gd12[iq_gd12]
			phi_gd12   = phi_gd12[iq_gd12]
			theta_gd12 = theta_gd12[iq_gd12]
			tof_gd12   = tof_gd12[iq_gd12]
			q_gd12     = q_gd12[iq_gd12]
		endif else begin
			message, 'No beams of desired quality for GD12.', /INFORMATIONAL
		endelse
		
		;GD21
		if n_gd21 gt 0 then begin
			epoch_gd21 = epoch_gd21[iq_gd21]
			phi_gd21   = phi_gd21[iq_gd21]
			theta_gd21 = theta_gd21[iq_gd21]
			tof_gd21   = tof_gd21[iq_gd21]
			q_gd21     = q_gd21[iq_gd21]
		endif else begin
			message, 'No beams of desired quality for GD21.', /INFORMATIONAL
		endelse
		
		if n_gd12 + n_gd21 eq 0 then $
			message, 'No beams found of desired quality.'
			
	;No filter
	endif else begin
		n_gd12 = n_elements(epoch_gd12)
		n_gd21 = n_elements(epoch_gd21)
	endelse

;-----------------------------------------------------
; Firing Vectors \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Constant: Degrees -> Radians
	deg2rad = !pi / 180.0D
	
	if n_gd12 gt 0 then begin
		;Convert to radians
		azimuth_gd12 = phi_gd12   * deg2rad
		polar_gd12   = theta_gd12 * deg2rad
	
		;Convert to cartesian coordinates
		fv_gd12      = fltarr(3, n_elements(azimuth_gd12))
		fv_gd12[0,*] = sin(polar_gd12) * cos(azimuth_gd12)
		fv_gd12[1,*] = sin(polar_gd12) * sin(azimuth_gd12)
		fv_gd12[2,*] = cos(polar_gd12)
	endif
	
	if n_gd21 gt 0 then begin
		;Convert to radians
		azimuth_gd21 = phi_gd21   * deg2rad
		polar_gd21   = theta_gd21 * deg2rad
		
		;Convert to cartesian coordinates
		fv_gd21      = fltarr(3, n_elements(azimuth_gd21))
		fv_gd21[0,*] = sin(polar_gd21) * cos(azimuth_gd21)
		fv_gd21[1,*] = sin(polar_gd21) * sin(azimuth_gd21)
		fv_gd21[2,*] = cos(polar_gd21)
	endif

;-----------------------------------------------------
; Return Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;All data
	if n_gd12 gt 0 then begin
		edi_gd12 = { n_gd12:       n_gd12, $
		             epoch_gd12:   epoch_gd12, $
		             azimuth_gd12: phi_gd12, $
		             polar_gd12:   theta_gd12, $
		             fv_gd12:      fv_gd12, $
		             tof_gd12:     tof_gd12, $
		             q_gd12:       q_gd12 $
		           }
	;Number of points found
	endif else edi_gd12 = {n_gd21: ngd21}
	
	;All data
	if n_gd21 gt 0 then begin
		edi_gd21 = { n_gd21:       n_gd21, $
		             epoch_gd21:   epoch_gd21, $
		             azimuth_gd21: phi_gd21, $
		             polar_gd21:   theta_gd21, $
		             fv_gd21:      fv_gd21, $
		             tof_gd21:     tof_gd21, $
		             q_gd21:       q_gd21 $
		           }
	;Number of points found
	endif else edi_gd12 = {n_gd21: ngd21}
	
	;Return the data
	return, create_struct(edi_gd12, edi_gd21)
end