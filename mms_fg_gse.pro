; docformat = 'rst'
;
; NAME:
;       mms_fg_gse
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
;   Read DFG or AFG magnetic field data and transform it into GSE coordinates.
;
; :Categories:
;   MMS, DFG, AFG
;
; :Params:
;       SC:             in, required, type=string
;                       MMS observatory/spacecraft number (e.g., 'mms1')
;       INSTR:          in, required, type=string
;                       Instrument name. Choices are 'dfg' or 'afg'.
;       MODE:           in, required, type=string
;                       Data telemetry mode.
;       TSTART:         in, required, type=string
;                       Start time of the data interval to read, as an ISO-8601 string.
;       TEND:           in, required, type=string
;                       End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       ATTITUDE_DIR:   in, optional, type=string, default=pwd
;                       Directory in which to find FDOA definitive attitude data.
;       B_BCS:          out, optional, type=3xN float
;                       A named variable to receive the magnetic field in BCS coordinates.
;       B_DMPA:         out, optional, type=3xN float
;                       A named variable to receive the magnetic field in DMPA coordinates.
;       B_OMB:          out, optional, type=3xN float
;                       A named variable to receive the magnetic field in OMB coordinates.
;       B_SMPA:         out, optional, type=3xN float
;                       A named variable to receive the magnetic field in SMPA coordinates.
;       EPOCH:          out, optional, type=int64arr (cdf_time_tt2000)
;                       Named variable to receive the epoch times associated with B
;       SUNPULSE_DIR:   in, optional, type=string, default=pwd
;                       Directory in which to find HK 0X101 sunpulse data.
;       _REF_EXTRA:     in, optional, type=string, default=pwd
;                       Any keyword accepted by mms_fg_bcs is also accepted via keyword
;                           inheritance.
;
; :Returns:
;       B_GSE:          3-component magnetic field in GSE coordinates.
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
;       2015/05/04  -   Written by Matthew Argall
;-
function mms_fg_gse, sc, instr, mode, tstart, tend, $
ATTITUDE_DIR=attitude_dir, $
B_BCS=b_bcs, $
B_OMB=b_omb, $
B_DMPA=b_dmpa, $
B_SMPA=b_smpa, $
EPOCH=epoch, $
SUNPULSE_DIR=sunpulse_dir, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Defaults
	bcs  = keyword_set(bcs)
	dmpa = keyword_set(dmpa)
	gse  = keyword_set(gse)
	smpa = keyword_set(smpa)
	if n_elements(attitude_dir) eq 0 then attitude_dir = ''
	if n_elements(sunpulse_dir) eq 0 then sunpulse_dir = ''
	
	;Make sure either attitude or sunpulse information is given
	if attitude_dir eq '' && sunpulse_dir eq '' then cd, CURRENT=attitude_dir
	
;-----------------------------------------------------
; Get the data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Read data
	b_bcs = mms_fg_bcs(sc, instr, mode, tstart, tend, $
	                   EPOCH         = epoch, $
	                   B_SMPA        = b_smpa, $
	                   B_OMB         = b_omb, $
	                   _STRICT_EXTRA = extra)
	if arg_present(b_bcs) eq 0 then b_bcs = !Null
	if arg_present(b_omb) eq 0 then b_omb = !Null

;-----------------------------------------------------
; Despin \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;
	; Assume the principle axis of inertia (z-MPA)
	; is the same as the angular momentum vector (L)
	;

	;Despin using definitive attitude
	if sunpulse_dir eq '' then begin
		message, 'Despinning with attitude data not implemented yet.'
	
		;Build matrix
		smpa2dmpa_gd12 = mms_fdoa_xdespin(attitude, t, 'L')
		smpa2dmpa_gd21 = mms_fdoa_xdespin(attitude, t, 'L')
	
	;Despin using sun pulse times.
	endif else begin
		;Read sun pulse data
		dss = mms_dss_read_sunpulse(sc, tstart, tend, sunpulse_dir, /UNIQ_PULSE)

		;Build matrix
		smpa2dmpa = mms_dss_xdespin( dss, epoch )
	endelse

	;Transform
	b_dmpa = mrvector_rotate( smpa2dmpa, b_smpa )
	if arg_present(b_dmpa) eq 0 then b_dmpa = !Null


;-----------------------------------------------------
; Rotate to GSE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(attitude) ne 0 then begin
		message, 'Rotate to GSE not implemented yet.'
		
		;dmpa2gei
		
		;CXForm
		b_gse = cxform(b_dmpa, 'GEI', 'GSE', sse)
	endif else begin
		message, 'No attitude data. Cannot rotate to GSE', /INFORMATIONAL
		b_gse = !Null
	endelse

;-----------------------------------------------------
; Return Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	return, b_gse
end