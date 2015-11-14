; docformat = 'rst'
;
; NAME:
;       mms_fpi_divP
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
;   Calculate the divergence of the pressure tensor using reciprocal vectors.
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
;                           Data level. Choices are 'l1b', 'ql', 'l2pre', 'l2'. The empy
;                               string selects the default value.
;
; :Keywords:
;       ION:            in, optional, type=boolean, default=0
;                       If set, the ion pressure tensor is used. The default is to
;                           use the electron pressure tensor.
;       TIME:           out, optional, type=long64arr (cdf_time_tt2000)
;                       Time tags for `DIVP`
;
; :Returns:
;       DIVP:           Divergence of the pressure tensor
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
function mms_fpi_divP, tstart, tend, mode, level, $
ION=ion, $
TIME=time
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	ion = keyword_set(ion)
	if n_elements(mode)  eq 0 || mode  eq '' then mode  = 'fast'
	if n_elements(level) eq 0 || level eq '' then level = 'sitl'

;-----------------------------------------------------
; FPI Pressure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;FPI moments
	if ion then begin
		mms_fpi_sitl_read, 'mms1', mode, tstart, tend, PI_DSC=pi1, TIME=t1
		mms_fpi_sitl_read, 'mms2', mode, tstart, tend, PI_DSC=pi2, TIME=t2
		mms_fpi_sitl_read, 'mms3', mode, tstart, tend, PI_DSC=pi3, TIME=t3
		mms_fpi_sitl_read, 'mms4', mode, tstart, tend, PI_DSC=pi4, TIME=t4
	endif else begin
		mms_fpi_sitl_read, 'mms1', mode, tstart, tend, PE_DSC=pe1, TIME=t1
		mms_fpi_sitl_read, 'mms2', mode, tstart, tend, PE_DSC=pe2, TIME=t2
		mms_fpi_sitl_read, 'mms3', mode, tstart, tend, PE_DSC=pe3, TIME=t3
		mms_fpi_sitl_read, 'mms4', mode, tstart, tend, PE_DSC=pe4, TIME=t4
	endelse

;-----------------------------------------------------
; S/C Position \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;FDOA Spacecraft Position
	r1_dmpa = mms_fdoa_scpos('mms1', tstart, tend, t1)
	r2_dmpa = mms_fdoa_scpos('mms2', tstart, tend, t1)
	r3_dmpa = mms_fdoa_scpos('mms3', tstart, tend, t1)
	r4_dmpa = mms_fdoa_scpos('mms4', tstart, tend, t1)
	r1_dmpa = transpose(r1_dmpa)
	r2_dmpa = transpose(r2_dmpa)
	r3_dmpa = transpose(r3_dmpa)
	r4_dmpa = transpose(r4_dmpa)

;-----------------------------------------------------
; Interpolate FPI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert time to seconds
	MrCDF_Epoch_Breakdown, t1[0], year, month, day
	MrCDF_Epoch_Compute, t0, year, month, day, /TT2000
	t1_sse = MrCDF_epoch2sse(t1, t0)
	t2_sse = MrCDF_epoch2sse(t2, t0)
	t3_sse = MrCDF_epoch2sse(t3, t0)
	t4_sse = MrCDF_epoch2sse(t4, t0)
	
	;Interpolate to reference spacecraft
	pe2 = MrInterpol(pe2, t2_sse, t1_sse)
	pe3 = MrInterpol(pe3, t3_sse, t1_sse)
	pe4 = MrInterpol(pe4, t4_sse, t1_sse)

;-----------------------------------------------------
; Divergence \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Curlometer
	divP = MrReciprocalDivergence(r1_dmpa, r2_dmpa, r3_dmpa, r4_dmpa, pe1, pe2, pe3, pe4)

	return, divP
end