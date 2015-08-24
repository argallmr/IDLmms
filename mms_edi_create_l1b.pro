; docformat = 'rst'
;
; NAME:
;       mms_edi_create_l1b
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
;   Read EDI electric field mode level 1A data and turn it into level 1B data. L1B
;   implies calibrated data in the spinning, spacecraft body coordinate system (BCS).
;
; :Categories:
;   MMS, EDI
;
; :Params:
;       FILES:          in, required, type=string/strarr
;                       Name(s) of the L1A EDI efield-mode file(s) to read.
;       TSTART:         in, required, type=string
;                       Start time of the data interval to read, as an ISO-8601 string.
;       TEND:           in, required, type=string
;                       End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       CS_123:         in, optional, type=boolean, default=0
;                       If set, data in EDI coordinates will be included in `EDI`.
;       CS_BCS:         in, optional, type=boolean, default=0
;                       If set, data in BCS coordinates will be included in `EDI`.
;       QUALITY:        in, optional, type=integer/intarr, default=pwd
;                       Quality of EDI beams to return. Can be a scalar or vector with
;                           values [0, 1, 2, 3].
;
; :Returns:
;       EDI:            Structure array of EDI data. In addition to fields returned by
;                           mms_edi_read_efield, we have::
;                             'GUN_GD12_BCS'     -  Position of gun 1 (meters)
;                             'DET_GD12_BCS'     -  Position of detector 2 (meters)
;                             'VIRTUAL_GUN1_BCS' -  Position of gun 1 on virtual spacecraft (meters)
;                             'FV_GD12_BCS'      -  Firing vectors
;
;                             'GUN_GD21_BCS'     -  Position of gun 2 (meters)
;                             'DET_GD21_BCS'     -  Position of detector 1 (meters)
;                             'VIRTUAL_GUN2_BCS' -  Position of gun 2 on virtual spacecraft (meters)
;                             'FV_GD21_BCS'      -  Firing vectors
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
;       2015/06/22  -   Renamed from mms_edi_bcs to mms_edi_create_l1b. - MRA
;       2015/08/22  -   TSTART and TEND are prameters, not keywords. - MRA
;-
function mms_edi_create_l1b, files, tstart, tend, $
CS_123=cs_123, $
CS_BCS=cs_bcs, $
QUALITY=quality
	compile_opt idl2
	on_error, 2
	
	cs_123 = keyword_set(cs_123)
	cs_bcs = n_elements(cs_bcs) eq 0 ? 1B : keyword_set(cs_bcs)
	
;-----------------------------------------------------
; Get the data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get gun positions
	gun_gd12_bcs = mms_instr_origins_ocs('EDI1_GUN')
	det_gd12_bcs = mms_instr_origins_ocs('EDI1_DETECTOR')
	gun_gd21_bcs = mms_instr_origins_ocs('EDI2_GUN')
	det_gd21_bcs = mms_instr_origins_ocs('EDI2_DETECTOR')
	
	;Virtual gun positions in BCS
	pos_vg1_bcs = gun_gd12_bcs - det_gd21_bcs
	pos_vg2_bcs = gun_gd21_bcs - det_gd12_bcs

	;Read data
	edi = mms_edi_read_l1a_efield(files, tstart, tend, $
	                              QUALITY = quality)

;-----------------------------------------------------
; Rotate 123 <-> BCS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Transformation matrices from 123 to bcs
	edi1_to_bcs = mms_instr_xxyz2ocs('EDI1')
	edi2_to_bcs = mms_instr_xxyz2ocs('EDI2')

	;Rotate 123 -> BCS
	if edi.count_gd12 gt 0 then fv_gd12_bcs = mrvector_rotate(edi1_to_bcs, edi.fv_gd12_123)
	if edi.count_gd21 gt 0 then fv_gd21_bcs = mrvector_rotate(edi2_to_bcs, edi.fv_gd21_123)
	
	;Rotate BCS -> 123
	bcs_to_edi1 = transpose(edi1_to_bcs)
	bcs_to_edi2 = transpose(edi2_to_bcs)
	gun_gd12_123 = mrvector_rotate( bcs_to_edi1, gun_gd12_bcs )
	det_gd12_123 = mrvector_rotate( bcs_to_edi1, gun_gd12_bcs )
	gun_gd21_123 = mrvector_rotate( bcs_to_edi2, gun_gd21_bcs )
	det_gd21_123 = mrvector_rotate( bcs_to_edi2, gun_gd21_bcs )
	
	;Virtual gun positions in 123
	pos_vg1_123 = gun_gd12_123 - det_gd21_123
	pos_vg2_123 = gun_gd21_123 - det_gd12_123

;-----------------------------------------------------
; Append Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Include data in EDI coordinates?
	if cs_123 then begin
		edi = create_struct( edi, $
		                     'gun_gd12_123',     gun_gd12_123, $
		                     'det_gd12_123',     det_gd12_123, $
		                     'gun_gd21_123',     gun_gd21_123, $
		                     'det_gd21_123',     det_gd21_123, $
		                     'virtual_gun1_123', pos_vg1_123, $
		                     'virtual_gun2_123', pos_vg2_123 $
		                   )
	endif else begin
		edi = remove_tags(edi, ['fv_gd12_123', 'fv_gd21_123'])
	endelse

	;Positions on real and virtual spacecraft
	if cs_bcs then begin
		edi = create_struct( edi, $
		                    'gun_gd12_bcs',     gun_gd12_bcs, $
		                    'det_gd12_bcs',     det_gd12_bcs, $
		                    'gun_gd21_bcs',     gun_gd21_bcs, $
		                    'det_gd21_bcs',     det_gd21_bcs, $
		                    'virtual_gun1_bcs', pos_vg1_bcs, $
		                    'virtual_gun2_bcs', pos_vg2_bcs )
		
		;Firing vectors
		if edi.count_gd12 gt 0 then edi = create_struct(edi, 'fv_gd12_bcs', fv_gd12_bcs)
		if edi.count_gd21 gt 0 then edi = create_struct(edi, 'fv_gd21_bcs', fv_gd21_bcs)
	endif
	
	;Return structure
	return, edi
end