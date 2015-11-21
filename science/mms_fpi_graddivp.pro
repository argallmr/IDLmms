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
function mms_fpi_graddivP, mode, tstart, tend, $
GRADP=gradP, $
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
	if mode ne 'srvy' && mode ne 'brst' then message, 'MODE must be "srvy" or "brst".'
	fpi_mode = mode eq 'srvy' ? 'fast' : 'brst'

;-----------------------------------------------------
; FPI Pressure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;BRST
	if fpi_mode eq 'brst' then begin
		if ion then begin
			mms_fpi_l1b_moms_read, 'mms1', 'dis-moms', tstart, tend, P_GSE=p1, TIME=t1
			mms_fpi_l1b_moms_read, 'mms2', 'dis-moms', tstart, tend, P_GSE=p2, TIME=t2
			mms_fpi_l1b_moms_read, 'mms3', 'dis-moms', tstart, tend, P_GSE=p3, TIME=t3
			mms_fpi_l1b_moms_read, 'mms4', 'dis-moms', tstart, tend, P_GSE=p4, TIME=t4
		endif else begin
			mms_fpi_l1b_moms_read, 'mms1', 'des-moms', tstart, tend, P_GSE=p1, TIME=t1
			mms_fpi_l1b_moms_read, 'mms2', 'des-moms', tstart, tend, P_GSE=p2, TIME=t2
			mms_fpi_l1b_moms_read, 'mms3', 'des-moms', tstart, tend, P_GSE=p3, TIME=t3
			mms_fpi_l1b_moms_read, 'mms4', 'des-moms', tstart, tend, P_GSE=p4, TIME=t4
		endelse
	;FAST
	endif else begin
		if ion then begin
			mms_fpi_sitl_read, 'mms1', mode, tstart, tend, PI_DSC=p1, TIME=t1
			mms_fpi_sitl_read, 'mms2', mode, tstart, tend, PI_DSC=p2, TIME=t2
			mms_fpi_sitl_read, 'mms3', mode, tstart, tend, PI_DSC=p3, TIME=t3
			mms_fpi_sitl_read, 'mms4', mode, tstart, tend, PI_DSC=p4, TIME=t4
		endif else begin
			mms_fpi_sitl_read, 'mms1', mode, tstart, tend, PE_DSC=p1, TIME=t1
			mms_fpi_sitl_read, 'mms2', mode, tstart, tend, PE_DSC=p2, TIME=t2
			mms_fpi_sitl_read, 'mms3', mode, tstart, tend, PE_DSC=p3, TIME=t3
			mms_fpi_sitl_read, 'mms4', mode, tstart, tend, PE_DSC=p4, TIME=t4
		endelse
	endelse

;-----------------------------------------------------
; S/C Position \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;FDOA Spacecraft Position
	r1 = mms_fdoa_scpos('mms1', tstart, tend, t1)
	r2 = mms_fdoa_scpos('mms2', tstart, tend, t1)
	r3 = mms_fdoa_scpos('mms3', tstart, tend, t1)
	r4 = mms_fdoa_scpos('mms4', tstart, tend, t1)

;-----------------------------------------------------
; Interpolate FPI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert time to seconds
	MrCDF_Epoch_Breakdown, t1[0], year, month, day
	t0 = MrCDF_Epoch_Compute(year, month, day, /TT2000)
	t1_sse = MrCDF_epoch2sse(t1, t0)
	t2_sse = MrCDF_epoch2sse(temporary(t2), t0)
	t3_sse = MrCDF_epoch2sse(temporary(t3), t0)
	t4_sse = MrCDF_epoch2sse(temporary(t4), t0)

	;Interpolate to reference spacecraft
	p2 = MrInterpol(p2, t2_sse, t1_sse)
	p3 = MrInterpol(p3, t3_sse, t1_sse)
	p4 = MrInterpol(p4, t4_sse, t1_sse)

;-----------------------------------------------------
; Divergence \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Divergence
	;   - 1e-18 converts cm^3/C 1/km^2 nP --> V/m
	;   - 1e-3 converts V/m --> mV/m
	divP = 1e-15 * MrReciprocalDivergence(r1, r2, r3, r4, p1, p2, p3, p4)
	
	;Gradient
	if arg_present(gradP) then begin
		scalarP1 = (p1[0,*] + p1[4,*] + p1[6,*]) / 3.0
		scalarP2 = (p2[0,*] + p2[4,*] + p2[6,*]) / 3.0
		scalarP3 = (p3[0,*] + p3[4,*] + p3[6,*]) / 3.0
		scalarP4 = (p4[0,*] + p4[4,*] + p4[6,*]) / 3.0
		gradP    = MrReciprocalGradient(r1, r2, r3, r4, scalarP1, scalarP2, scalarP3, scalarP4)
	endif

	if arg_present(time) then time = temporary(t1)
	return, divP
end