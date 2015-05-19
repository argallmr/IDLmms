; docformat = 'rst'
;
; NAME:
;       mms_edi_gse
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
;   Read EDI electric field mode data and transform it into the body coordinate
;   system (BCS).
;
; :Categories:
;   MMS, EDI
;
; :Params:
;       FILES:          in, required, type=string/strarr
;                       Name(s) of the L1A EDI efield-mode file(s) to read.
;
; :Keywords:
;       ATTITUDE_DIR:   in, optional, type=string, default=pwd
;                       Directory in which to find FDOA definitive attitude data.
;       BCS:            in, optional, type=boolean, default=0
;                       If set, data in BCS coordinates will be included in `EDI`.
;       DMPA:           in, optional, type=boolean, default=0
;                       If set, data in DMPA coordinates will be included in `EDI`.
;       EDI:            in, optional, type=boolean, default=0
;                       If set, data in EDI coordinates will be included in `EDI`.
;       GSE:            in, optional, type=boolean, default=1
;                       If set, data in GSE coordinates will be included in `EDI`.
;       SMPA:           in, optional, type=boolean, default=0
;                       If set, data in SMPA coordinates will be included in `EDI`.
;       SUNPULSE_DIR:   in, optional, type=string, default=pwd
;                       Directory in which to find HK 0X101 sunpulse data.
;       TSTART:         in, required, type=string
;                       Start time of the data interval to read, as an ISO-8601 string.
;       TEND:           in, required, type=string
;                       End time of the data interval to read, as an ISO-8601 string.
;       _REF_EXTRA:     in, optional, type=string, default=pwd
;                       Any keyword accepted by mms_fg_bcs is also accepted via keyword
;                           inheritance.
;
; :Returns:
;       EDI:            Structure array of EDI data. In addition to fields returned by
;                           mms_edi_read_efield, we have::
;                             'GUN_GD12_GSE'     -  Position of gun 1 (meters)
;                             'DET_GD12_GSE'     -  Position of detector 2 (meters)
;                             'VIRTUAL_GUN1_GSE' -  Position of gun 1 on virtual spacecraft (meters)
;                             'FV_GD12_GSE'      -  Firing vectors
;
;                             'GUN_GD21_GSE'     -  Position of gun 2 (meters)
;                             'DET_GD21_GSE'     -  Position of detector 1 (meters)
;                             'VIRTUAL_GUN2_GSE' -  Position of gun 2 on virtual spacecraft (meters)
;                             'FV_GD21_GSE'      -  Firing vectors
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
;       2015/05/18  -   Require file names instead of search for files. TSTART and TEND
;                           are keywords, not parameters. - MRA
;-
function mms_edi_gse, sc, mode, tstart, tend, $
GSE=gse, $
BCS=bcs, $
DMPA=dmpa, $
EDI=edi, $
SMPA=smpa, $
ATTITUDE_DIR = attitude_dir, $
SUNPULSE_DIR = sunpulse_dir, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Defaults
	bcs  = keyword_set(bcs)
	dmpa = keyword_set(dmpa)
	gse  = n_elements(gse) eq 0 ? 1 : keyword_set(gse)
	smpa = keyword_set(smpa)
	if n_elements(attitude_dir) eq 0 then attitude_dir = ''
	if n_elements(sunpulse_dir) eq 0 then sunpulse_dir = ''
	
	;Make sure either attitude or sunpulse information is given
	if attitude_dir eq '' && sunpulse_dir eq '' then cd, CURRENT=attitude_dir
	
	;Default to returning data in GSE
	if bcs + dmpa + gse + smpa eq 0 then gse = 1
	
;-----------------------------------------------------
; Get the data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get gun positions
	gun_gd12_bcs = mms_instr_origins_ocs('EDI1_GUN')
	det_gd12_bcs = mms_instr_origins_ocs('EDI1_DETECTOR')
	gun_gd21_bcs = mms_instr_origins_ocs('EDI2_GUN')
	det_gd21_bcs = mms_instr_origins_ocs('EDI2_DETECTOR')

	;Read data
	edi = mms_edi_bcs(files, EDI=edi, TSTART=tstart, TEND=tend, _STRICT_EXTRA=extra)

;-----------------------------------------------------
; Rotate to SMPA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if attitude_dir ne '' then begin
		message, 'Rotation to SMPA not implemented yet.'
	
	endif else begin
		;Warning message
		message, 'No Attitude data. Cannot rotate to SMPA.', /INFORMATIONAL
		
		;Copy data
		;   - Rotation is small -- assume negligible.
		if edi.count_gd12 gt 0 then begin
			fv_gd12_smpa      = edi.fv_gd12_bcs
			gun_gd12_smpa     = edi.gun_gd12_bcs
			det_gd12_smpa     = edi.det_gd12_bcs
			virtual_gun1_smpa = edi.virtual_gun1_bcs
		endif
		
		if edi.count_gd21 gt 0 then begin
			fv_gd21_smpa      = edi.fv_gd21_bcs
			gun_gd21_smpa     = edi.gun_gd21_bcs
			det_gd21_smpa     = edi.det_gd21_bcs
			virtual_gun2_smpa = edi.virtual_gun2_bcs
		endif
	endelse

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
		dss_sunpulse = mms_dss_read_sunpulse(sc, tstart, tend, sunpulse_dir, /UNIQ_PULSE)

		;Build matrix
		if edi.count_gd12 gt 0 then smpa2dmpa_gd12 = mms_dss_xdespin( dss_sunpulse, edi.epoch_gd12 )
		if edi.count_gd21 gt 0 then smpa2dmpa_gd21 = mms_dss_xdespin( dss_sunpulse, edi.epoch_gd21 )
	endelse

	;Transform
	if edi.count_gd12 gt 0 then begin
		fv_gd12_dmpa      = mrvector_rotate( smpa2dmpa_gd12, fv_gd12_smpa )
		gun_gd12_dmpa     = mrvector_rotate( smpa2dmpa_gd12, gun_gd12_smpa )
		det_gd12_dmpa     = mrvector_rotate( smpa2dmpa_gd12, det_gd12_smpa )
		virtual_gun1_dmpa = mrvector_rotate( smpa2dmpa_gd12, virtual_gun1_smpa )
	endif
	
	if edi.count_gd21 gt 0 then begin
		fv_gd21_dmpa      = mrvector_rotate( smpa2dmpa_gd21, fv_gd21_smpa )
		gun_gd21_dmpa     = mrvector_rotate( smpa2dmpa_gd21, gun_gd21_smpa )
		det_gd21_dmpa     = mrvector_rotate( smpa2dmpa_gd21, det_gd21_smpa )
		virtual_gun2_dmpa = mrvector_rotate( smpa2dmpa_gd21, virtual_gun2_smpa )
	endif


;-----------------------------------------------------
; Rotate to GSE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(attitude) ne 0 then begin
		message, 'Rotate to GSE not implemented yet.'
		
		;dmpa2gei
		
		;CXForm
		fv_gd12_gse = cxform(fv_gd12_gei, 'GEI', 'GSE', sse)
	endif else begin
		message, 'No attitude data. Cannot rotate to GSE', /INFORMATIONAL
	endelse

;-----------------------------------------------------
; Return Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Return data in BCS?
	if bcs eq 0 then begin
		edi = remove_tags(edi, ['gun_gd12_bcs',     'det_gd12_bcs', $
		                       'gun_gd21_bcs',     'det_gd21_bcs', $
		                       'virtual_gun1_bcs', 'virtual_gun2_bcs', $
		                       'fv_gd12_bcs',      'fv_gd21_bcs'])
	endif

	;Return data in SMPA?
	if smpa then begin
		if edi.count_gd12 gt 0 then begin
			edi = create_struct( edi, $
				                'fv_gd12_smpa', fv_gd12_smpa, $
				                'gun_gd12_smpa', gun_gd12_smpa, $
				                'det_gd12_smpa', det_gd12_smpa, $
				                'virtual_gun1_smpa', virtual_gun1_smpa )
		endif
		
		if edi.count_gd21 gt 0 then begin
			edi = create_struct( edi, $
			                    'fv_gd21_smpa', fv_gd21_smpa, $
			                    'gun_gd21_smpa', gun_gd21_smpa, $
			                    'det_gd21_smpa', det_gd21_smpa, $
			                    'virtual_gun2_smpa', virtual_gun2_smpa )
		endif
	endif
	
	;Return data in DMPA?
	if dmpa then begin
		if edi.count_gd12 gt 0 then begin
			edi = create_struct( edi, $
				                'fv_gd12_dmpa', fv_gd12_dmpa, $
				                'gun_gd12_dmpa', gun_gd12_dmpa, $
				                'det_gd12_dmpa', det_gd12_dmpa, $
				                'virtual_gun1_dmpa', virtual_gun1_dmpa )
		endif
		
		if edi.count_gd21 gt 0 then begin
			edi = create_struct( edi, $
			                    'fv_gd21_dmpa', fv_gd21_dmpa, $
			                    'gun_gd21_dmpa', gun_gd21_dmpa, $
			                    'det_gd21_dmpa', det_gd21_dmpa, $
			                    'virtual_gun2_dmpa', virtual_gun2_dmpa )
		endif
	endif
	
	;Return data in GSE
	if gse then begin
		if edi.count_gd12 gt 0 then begin
			edi = create_struct( edi, $
				                'fv_gd12_gse', fv_gd12_gse, $
				                'gun_gd12_gse', gun_gd12_gse, $
				                'det_gd12_gse', det_gd12_gse, $
				                'virtual_gun1_gse', virtual_gun1_gse )
		endif
		
		if edi.count_gd21 gt 0 then begin
			edi = create_struct( edi, $
			                    'fv_gd21_gse', fv_gd21_gse, $
			                    'gun_gd21_gse', gun_gd21_gse, $
			                    'det_gd21_gse', det_gd21_gse, $
			                    'virtual_gun2_gse', virtual_gun2_gse )
		endif
	endif

	;Return structure
	return, edi
end