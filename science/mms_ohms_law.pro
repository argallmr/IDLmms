; docformat = 'rst'
;
; NAME:
;       mms_ohms_law
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
;   Calculate all terms in the Generalized Ohm's Law.
;
; :Categories:
;   MMS, FPI
;
; :Params:
;       FILES:              in, required, type=string/strarr
;                           Name(s) of the AFG or DFG file(s) to be read.
;       MODE:               in, optional, type=string, default='srvy'
;                           Data rate mode. Choices are 'srvy' and 'brst'.
;       TSTART:             in, optional, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, optional, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       E_C:                out, optional, type=3xN fltarr
;                           Electric field due to ion convection. Computed as::
;                               \vec{E} = -\vec{U}_{i} \time \vec{B}
;       E_HALL:             out, optional, type=3xN fltarr
;                           Hall electric field. Computed as::
;                               \vec{E} = \frac{1} {n e} \vec{J}_{tot} \time \vec{B}
;       E_DIVPE:            out, optional, type=3xN fltarr
;                           Electric field due to electron pressure divergence. Computed as::
;                               \vec{E} = \frac{1} {n e} \nabla \time \vec{P}_{e}
;       E_DJDT:             out, optional, type=3xN fltarr
;                           Electric field due electron inertia. Computed as::
;                               \vec{E} = \frac{m_{e}} {n e^{2}} \frac{\partial J_{e}} {\partial t}
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
pro mms_ohms_law, sc, mode, tstart, tend, $
E_C=E_C, $
E_HALL=E_Hall, $
E_DIVPE=E_divPe, $
E_DJDT=E_dJdt, $
TIME=time
	compile_opt idl2
;	on_error, 2
	
	;Constants
	me = constants('m_e')
	q  = constants('q')
	
	;Which quantities to caluclate?
	get_ec   = arg_present(E_C)
	get_eh   = arg_present(E_Hall)
	get_divp = arg_present(E_divPe)
	get_djdt = arg_present(E_dJdt)
	
;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Magnetic field
	mms_fgm_ql_read, sc, 'dfg', mode, 'l2pre', tstart, tend, $
	                 B_GSE = b_fgm, $
	                 TIME  = t_fgm
	
	;FPI moments
	;   - Only 'fast' and 'brst' available
	fpi_mode = mode eq 'srvy' ? 'fast' : mode
	if fpi_mode eq 'brst' then begin
		;Read DES
		mms_fpi_l1b_moms_read, sc, 'des-moms', tstart, tend, $
		                       N     = n_e, $
		                       V_GSE = ve, $
		                       TIME  = t_fpi
		;Read DIS
		mms_fpi_l1b_moms_read, sc, 'dis-moms', tstart, tend, $
		                       N     = n_i, $
		                       V_GSE = vi, $
		                       TIME  = time_i
		
		;Interpolate ions onto electron times
		n_i  = interpol(n_i, time_i, t_fpi)
		vi   = MrInterpol(vi, temporary(time_i), t_fpi)
		npts = n_elements(t_fpi)
		
		;Total current
		;   - 1e9 converts C km / (s cm^3) to C / m^2 s
		;   - 1e6 converts A / m^2 to uA / m^2
		J_total = q*1e15 * (rebin(n_i, 3, npts) * vi - rebin(n_e, 3, npts) * ve)
	
	endif else begin
		mms_fpi_sitl_read, sc, fpi_mode, tstart, tend, $
		                   VI_DSC  = vi, $
		                   VE_DSC  = ve, $
		                   N_I     = n_i, $
		                   N_E     = n_e, $
		                   J_TOTAL = J_total, $
		                   TIME    = t_fpi
	endelse
	;Number of points
	npts = n_elements(t_fpi)
	
;-----------------------------------------------------
; Interpolate FGM to FPI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;TODO: Average the magnetic field instead of interpolate.
	;      Does FIELDS mark the BEGIN or CENTER time?
	;      Does FPI    mark the BEGIN or CENTER time?
	b_fpi      = fltarr(3, n_elements(t_fpi))
	b_fpi[0,*] = interpol(b_fgm[0,*], t_fgm, t_fpi)
	b_fpi[1,*] = interpol(b_fgm[1,*], t_fgm, t_fpi)
	b_fpi[2,*] = interpol(b_fgm[2,*], t_fgm, t_fpi)
	
;-----------------------------------------------------
; Convective Electric Field \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Equation:
	;   - E_C = -(Ui x B)
	;

	;Unit conversion
	;   - 1e-6 converts km/s nT  --> V/m
	;   - 1e3  converts V/m --> mV/m
	if get_ec then E_C = -1e-3 * MrVector_Cross(vi, b_fpi)
	
;-----------------------------------------------------
; Hall Electric Field \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Equation:
	;   - E_Hall = 1/(qn) (J x B)
	;

	;Unit conversion
	;   - 1e-21 converts 1/C cm^3 uA/m^2 nT  --> V/m
	;   - 1e3   converts V/m --> mV/m
	if get_eh then E_Hall = rebin(1e-18 / (q * n_e), 3, npts) * MrVector_Cross(J_total, b_fpi)
	
;-----------------------------------------------------
; Pressure Divergence \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Equation:
	;   - E_divP = -1/(qn) \nabla \cdot P
	;
	
	if get_divp then begin
		;Divergence of the electron pressure
		;   - 1/km erg/cm^3
		divPe = mms_fpi_gradDivP(mode, tstart, tend)

		;Electric field
		;   - 1e-10 converts 1/C cm^3 1/km erg/cm^3  --> V/m
		;   - 1e3  converts V/m --> mV/m
		E_divPe = (-1e-7 / q) * (1.0 / rebin(n_e, 3, npts)) * divPe
	endif
	
;-----------------------------------------------------
; Electron Inertial Term \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Equation:
	;   - E_C = me / (n e^2) [ dJ/dt + div(JU + UJ) ]
	;
	
	if get_djdt then begin
		;Median sampling interval
		;   - s
		t_fpi_sse = MrCDF_epoch2sse(t_fpi)
		dt        = median(t_fpi_sse[1:*] - t_fpi_sse)
	
		;Time derivative of current
		;   - uA / m^2 s
		dJ_dt     = (J_total[*,1:*] - J_total[*,0:npts-2]) / dt
	
		;Unit conversion
		;   - 1e-12 converts uA/m^2 1/s --> V/m
		;   - 1e3 converts V/m --> mV/m
		E_dJdt = (me/q^2) * 1e-9/rebin(n_e, 3, npts) * temporary(dJ_dt)
	endif
	
;-----------------------------------------------------
; Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if arg_present(time) then time = temporary(t_fpi)
end
