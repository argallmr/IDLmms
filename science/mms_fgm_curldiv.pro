; docformat = 'rst'
;
; NAME:
;       mms_fgm_curldiv
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
;       INSTR:              in, required, type=string, default='dfg'
;                           FGM instrument used for the calculation. Choices are 'dfg'
;                               or 'afg'. The empy string selects the default value.
;       MODE:               in, required, type=string, default='srvy'
;                           Data rate mode. Choices are 'brst' or 'srvy'. The empy string
;                               selects the default value.
;       LEVEL:              in, required, type=string, default='ql'
;                           Data level. Choices are 'l1b', 'ql', 'l2pre', 'l2'. The empy
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
;       2015/11/13  -   Written by Matthew Argall
;-
function mms_fgm_curldiv, tstart, tend, instr, mode, level, $
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
	
	if n_elements(instr) eq 0 || instr eq '' then instr = 'dfg'
	if n_elements(mode)  eq 0 || mode  eq '' then mode  = 'srvy'
	if n_elements(level) eq 0 || level eq '' then level = 'l2pre'

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Magnetic field
	;   - DMPA Coordinates
	mms_fgm_ql_read, 'mms1', instr, mode, level, tstart, tend, B_GSE=b1, TIME=t1
	mms_fgm_ql_read, 'mms2', instr, mode, level, tstart, tend, B_GSE=b2, TIME=t2
	mms_fgm_ql_read, 'mms3', instr, mode, level, tstart, tend, B_GSE=b3, TIME=t3
	mms_fgm_ql_read, 'mms4', instr, mode, level, tstart, tend, B_GSE=b4, TIME=t4
	
	;Remove the magntidue
	b1 = b1[0:2,*]
	b2 = b2[0:2,*]
	b3 = b3[0:2,*]
	b4 = b4[0:2,*]
	
	;FDOA Spacecraft Position
	;   - DMPA Coordinates
	r1 = mms_fdoa_scpos('mms1', tstart, tend, t1)
	r2 = mms_fdoa_scpos('mms2', tstart, tend, t1)
	r3 = mms_fdoa_scpos('mms3', tstart, tend, t1)
	r4 = mms_fdoa_scpos('mms4', tstart, tend, t1)

	;Convert time to seconds
	MrCDF_Epoch_Breakdown, t1[0], year, month, day
	t0 = MrCDF_Epoch_Compute(year, month, day, /TT2000)
	t1_sse = MrCDF_epoch2sse(t1, t0)
	t2_sse = MrCDF_epoch2sse(temporary(t2), t0)
	t3_sse = MrCDF_epoch2sse(temporary(t3), t0)
	t4_sse = MrCDF_epoch2sse(temporary(t4), t0)
	
	;interpolate B-fields
	b2 = MrInterpol(b2, temporary(t2_sse), t1_sse)
	b3 = MrInterpol(b3, temporary(t3_sse), t1_sse)
	b4 = MrInterpol(b4, temporary(t4_sse), temporary(t1_sse))

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