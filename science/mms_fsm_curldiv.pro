; docformat = 'rst'
;
; NAME:
;       mms_fsm_curldiv
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
;   Calculate the current density as the curl of the magnetic field.
;       J = \frac{1}{\mu_{0}} \nabla \times \vec{B}
;
; :Params:
;       TSTART:             in, required, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, required, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;       MODE:               in, required, type=string, default='srvy'
;                           Data rate mode. Choices are 'brst' or 'srvy'. The empy string
;                               selects the default value.
;       LEVEL:              in, required, type=string, default='ql'
;                           Data level. Choices are 'l1b', 'ql', 'l2pre', 'l2'. The empty
;                               string selects the default value.
;
; :Keywords:
;       JCURL:          out, optional, type=3x3 float
;                       Current density (uJ / m^2) computed via the curlometer technique
;       TIME:           out, optional, type=long64arr (cdf_time_tt2000)
;                       Time stamps of `JRECIP` and `JCURL`.
;
; :Returns:
;       JRECIP:         Current density (uJ / m^2) compute via the reciprocal vector technique
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
;       2015/12/10  -   Written by Matthew Argall
;-
function mms_fsm_curldiv, tstart, tend, mode, level, $
JCURL=Jcurl, $
DIVB=divB, $
TIME=time
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	if n_elements(mode)  eq 0 || mode  eq '' then mode  = 'srvy'
	if n_elements(level) eq 0 || level eq '' then level = 'l2plus'

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Magnetic field
	;   - GSE Coordinates
	mms_fsm_l2plus_read, 'mms1', mode, tstart, tend, B_GSE=b1, TIME=t1
	mms_fsm_l2plus_read, 'mms2', mode, tstart, tend, B_GSE=b2, TIME=t2
	mms_fsm_l2plus_read, 'mms3', mode, tstart, tend, B_GSE=b3, TIME=t3
	mms_fsm_l2plus_read, 'mms4', mode, tstart, tend, B_GSE=b4, TIME=t4

;-----------------------------------------------------
; Interpolate Fields \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to float (seconds)
	t0     = t1[0]
	t1_ssm = MrCDF_epoch2ssm(t1, t0)
	t2_ssm = MrCDF_epoch2ssm(temporary(t2), t0)
	t3_ssm = MrCDF_epoch2ssm(temporary(t3), t0)
	t4_ssm = MrCDF_epoch2ssm(temporary(t4), t0)
	
	;Interpolate B-fields
	;   - Use MMS1 as reference spacecraft
	b2 = MrInterpol(b2, temporary(t2_ssm), t1_ssm)
	b3 = MrInterpol(b3, temporary(t3_ssm), t1_ssm)
	b4 = MrInterpol(b4, temporary(t4_ssm), t1_ssm)

;-----------------------------------------------------
; Positions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Using MEC is 100x faster.
	;
	
	;FDOA Spacecraft Position
;	r1 = mms_fdoa_scpos('mms1', tstart, tend, t1)
;	r2 = mms_fdoa_scpos('mms2', tstart, tend, t1)
;	r3 = mms_fdoa_scpos('mms3', tstart, tend, t1)
;	r4 = mms_fdoa_scpos('mms4', tstart, tend, t1)
	
	;MEC Spacecraft Position
	mms_mec_read, 'mms1', 'epht89d', tstart, tend, R_GSE=r1, TIME=t1_mec
	mms_mec_read, 'mms2', 'epht89d', tstart, tend, R_GSE=r2, TIME=t2_mec
	mms_mec_read, 'mms3', 'epht89d', tstart, tend, R_GSE=r3, TIME=t3_mec
	mms_mec_read, 'mms4', 'epht89d', tstart, tend, R_GSE=r4, TIME=t4_mec
	
	;Convert to seconds
	t1_mec = MrCDF_epoch2ssm(temporary(t1_mec), t0)
	t2_mec = MrCDF_epoch2ssm(temporary(t2_mec), t0)
	t3_mec = MrCDF_epoch2ssm(temporary(t3_mec), t0)
	t4_mec = MrCDF_epoch2ssm(temporary(t4_mec), t0)
	
	;Interpolate position
	r1 = MrInterpol(r1, temporary(t1_mec), t1_ssm)
	r2 = MrInterpol(r2, temporary(t2_mec), t1_ssm)
	r3 = MrInterpol(r3, temporary(t3_mec), t1_ssm)
	r4 = MrInterpol(r4, temporary(t4_mec), t1_ssm)

;-----------------------------------------------------
; Curl \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Curl via Reciprocal Vectors
	;   - 1e-12/mu0 converts 1/km * nT  --> A/m^2
	;   - 1e6 converts A/m^2 --> uA/m^2
	mu0 = constants('mu_0')
	Jrecip = (1e-6/mu0) * MrReciprocalCurl(r1, r2, r3, r4, b1, b2, b3, b4)

	;Curl Via Curlomter
	;   - Returns in units of uA/m^2
	if arg_present(Jcurl) then Jcurl = MrCurlometer(r1, r2, r3, r4, b1, b2, b3, b4)
	
	;Divergence via Reciprocal Vectors
	;   - 1e-3 converts 1/km nT --> A/m^2
	if arg_present(divB) then begin
		;Correct units
		;   - 1e-12/u0  converts 1/km nT --> A/m^2
		;   - 1e6 converts A/m^2 --> uA/m^2
		divB  = MrReciprocalDivergence(r1, r2, r3, r4, b1, b2, b3, b4)
		divB *= (1e-6/mu0)
	endif

	;Delete data
	r1 = (r2 = (r3 = (r4 = !Null)))
	b1 = (b2 = (b3 = (b4 = !Null)))

	time = temporary(t1)
	return, Jrecip
end