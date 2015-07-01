; docformat = 'rst'
;
; NAME:
;       mms_edi_create_l2
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
;   Read EDI electric field mode level 1A data and turn it into level 2 data.
;   L2 implies calibrated data in despun spacecraft reference frame (no VxB removal)
;   and in the GSE coordinate system.
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
;       2015/06/22  -   renamed from mms_edi_gse to mms_edi_create_l2. - MRA
;-
function mms_edi_create_l2, filenames, $
ATTITUDE=attitude, $
CS_GSE=cs_gse, $
CS_BCS=cs_bcs, $
CS_DMPA=cs_dmpa, $
CS_SMPA=cs_smpa, $
SUNPULSE=sunpulse, $
TSTART=tstart, $
TEND=tend, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Defaults
	cs_bcs  = keyword_set(cs_bcs)
	cs_dmpa = keyword_set(cs_dmpa)
	cs_gse  = keyword_set(cs_gse)
	cs_smpa = keyword_set(cs_smpa)
	
	;Default to returning data in GSE
	if cs_bcs + cs_dmpa + cs_gse + cs_smpa eq 0 then cs_gse = 1
	
;-----------------------------------------------------
; Get the data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get gun positions
	gun_gd12_bcs = mms_instr_origins_ocs('EDI1_GUN')
	det_gd12_bcs = mms_instr_origins_ocs('EDI1_DETECTOR')
	gun_gd21_bcs = mms_instr_origins_ocs('EDI2_GUN')
	det_gd21_bcs = mms_instr_origins_ocs('EDI2_DETECTOR')

	;Read data
	edi = mms_edi_create_l1b(filenames, /CS_BCS, TSTART=tstart, TEND=tend, _STRICT_EXTRA=extra)

;-----------------------------------------------------
; Rotate to SMPA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(attitude) gt 0 then begin
		message, 'Rotation to SMPA not implemented yet.'
	
	endif else if n_elements(sunpulse) gt 0 then begin
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
	endif

;-----------------------------------------------------
; Despin \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;
	; Assume the principle axis of inertia (z-MPA)
	; is the same as the angular momentum vector (L)
	;

	;Despin using definitive attitude
	if n_elements(attitude) gt 0 then begin
		message, 'Despinning with attitude data not implemented yet.'
	
		;Build matrix
		smpa2dmpa_gd12 = mms_fdoa_xdespin(attitude, t, 'L')
		smpa2dmpa_gd21 = mms_fdoa_xdespin(attitude, t, 'L')
	
	;Despin using sun pulse times.
	endif else if n_elements(sunpulse) gt 0 then begin
		;Build matrix
		if edi.count_gd12 gt 0 then smpa2dmpa_gd12 = mms_dss_xdespin( sunpulse, edi.epoch_gd12 )
		if edi.count_gd21 gt 0 then smpa2dmpa_gd21 = mms_dss_xdespin( sunpulse, edi.epoch_gd21 )
	endif else begin
		message, 'Either ATTITUDE or SUNPULSE must be given.'
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
	if n_elements(attitude) gt 0 then begin
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
	if cs_bcs eq 0 then begin
		edi = remove_tags(edi, ['gun_gd12_bcs',     'det_gd12_bcs', $
		                        'gun_gd21_bcs',     'det_gd21_bcs', $
		                        'virtual_gun1_bcs', 'virtual_gun2_bcs', $
		                        'fv_gd12_bcs',      'fv_gd21_bcs'])
	endif

	;Return data in SMPA?
	if cs_smpa then begin
		if edi.count_gd12 gt 0 then begin
			edi = create_struct( edi, $
				                'fv_gd12_smpa',      fv_gd12_smpa, $
				                'gun_gd12_smpa',     gun_gd12_smpa, $
				                'det_gd12_smpa',     det_gd12_smpa, $
				                'virtual_gun1_smpa', virtual_gun1_smpa )
		endif
		
		if edi.count_gd21 gt 0 then begin
			edi = create_struct( edi, $
			                    'fv_gd21_smpa',      fv_gd21_smpa, $
			                    'gun_gd21_smpa',     gun_gd21_smpa, $
			                    'det_gd21_smpa',     det_gd21_smpa, $
			                    'virtual_gun2_smpa', virtual_gun2_smpa )
		endif
	endif
	
	;Return data in DMPA?
	if cs_dmpa then begin
		if edi.count_gd12 gt 0 then begin
			edi = create_struct( edi, $
				                'fv_gd12_dmpa',      fv_gd12_dmpa, $
				                'gun_gd12_dmpa',     gun_gd12_dmpa, $
				                'det_gd12_dmpa',     det_gd12_dmpa, $
				                'virtual_gun1_dmpa', virtual_gun1_dmpa )
		endif
		
		if edi.count_gd21 gt 0 then begin
			edi = create_struct( edi, $
			                    'fv_gd21_dmpa',      fv_gd21_dmpa, $
			                    'gun_gd21_dmpa',     gun_gd21_dmpa, $
			                    'det_gd21_dmpa',     det_gd21_dmpa, $
			                    'virtual_gun2_dmpa', virtual_gun2_dmpa )
		endif
	endif
	
	;Return data in GSE
	if cs_gse then begin
		if edi.count_gd12 gt 0 then begin
			edi = create_struct( edi, $
				                'fv_gd12_gse',      fv_gd12_gse, $
				                'gun_gd12_gse',     gun_gd12_gse, $
				                'det_gd12_gse',     det_gd12_gse, $
				                'virtual_gun1_gse', virtual_gun1_gse )
		endif
		
		if edi.count_gd21 gt 0 then begin
			edi = create_struct( edi, $
			                    'fv_gd21_gse',      fv_gd21_gse, $
			                    'gun_gd21_gse',     gun_gd21_gse, $
			                    'det_gd21_gse',     det_gd21_gse, $
			                    'virtual_gun2_gse', virtual_gun2_gse )
		endif
	endif

	;Return structure
	return, edi
end