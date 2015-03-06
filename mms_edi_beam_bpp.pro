; docformat = 'rst'
;
; NAME:
;       MMS_EDI_BEAM_BPP
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
;   Determine the firing angle of each beam in their own BPP.
;
;   Uses::
;       Digital Sun Sensor:
;           Time
;       Magnetometer:
;           Interpolated Magnetic Field
;       EDI:
;           Time
;           Analog Voltages
;
;   Creates:
;       EDI:
;           Gun positions in OCS
;           Gun positions in despun frame with detectors as origins
;           Gun positions in BPP with detectors as origins
;           Gun firing directions in EDI_GUN
;           Gun firing directions in OCS
;           Gun firing directions in BPP
;
; :Categories:
;   MMS, EDI, Bestarg
;
; :Params:
;       SRT:                in, required, type=lon64arr
;                           Sun pulse times.
;       T_BEAM1:            in, required, type=lon64arr
;                           Time tags from EDI GDU1 beam hits.
;       T_BEAM2:            in, required, type=lon64arr
;                           Time tags from EDI GDU2 beam hits.
;       B_BEAM1_DOCS:       in, required, type=lon64arr
;                           Magnetic field values interpolated onto `T_BEAM1`
;       B_BEAM2_DOCS:       in, required, type=lon64arr
;                           Magnetic field values interpolated onto `T_BEAM2`
;       VAX1:               in, required, type=fltarr
;                           EDI GDU1 firing analog voltage, x-component
;       VAY1:               in, required, type=fltarr
;                           EDI GDU1 firing analog voltage, y-component
;       VAX2:               in, required, type=fltarr
;                           EDI GDU2 firing analog voltage, x-component
;       VAY2:               in, required, type=fltarr
;                           EDI GDU2 firing analog voltage, y-component
;
; :Keywords:
;       GUN_INFO_DOCS:      out, optional, type=struct
;                           Structure of gun information in DOCS coordinates. Fields are::
;                               GUN1_POS_DOCS   -  Positiion of gun 1 with respect to detector 2
;                               GUN1_FIRE_DOCS  -  Firing direction of Gun1 (unit vector)
;                               GUN2_POS_DOCS   -  Positiion of gun 2 with respect to detector 1
;                               GUN2_FIRE_DOCS  -  Firing direction of Gun2 (unit vector)
;       SPIN_FREQ:          in, optional, type=float, default=mean(`SRT`)
;                           Spin frequency (radians / second) of the spacecraft.
;
; :Returns:
;       GUN_INFO_BPP:       Structure of gun information in the instantaneous BPP
;                           determined from `B_BEAM1` and `B_BEAM2` for each beam. Fields are::
;                               GUN1_POS_BPP   -  Positiion of gun 1 with respect to detector 2
;                               GUN1_FIRE_BPP  -  Firing direction of Gun1 (unit vector)
;                               GUN2_POS_BPP   -  Positiion of gun 2 with respect to detector 1
;                               GUN2_FIRE_BPP  -  Firing direction of Gun2 (unit vector)
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2015/02/15  -   Written by Matthew Argall
;       2015/03/06  -   Made into a stand-alone routine by accepting data, not file names.
;                          Removed the VIEW keyword. Return structure of data in BPP. - MRA
;-
function mms_edi_beam_bpp, srt, t_beam1, t_beam2, b_beam1_docs, b_beam2_docs, vax1, vay1, vax2, vay2, $
GUN_INFO_DOCS=gun_info_docs, $
SPIN_FREQ=omega
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Positions in DOCS (Apply Spin) \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; In the rotating frame, the gun positions are static while the field is rotating.
	; In the despun frame, the field is stationary, but the gun positions are rotating.
	;

	;Positions of Guns & Detectors in OCS
	;   Shift from the OCS origin to the optics (detector) origins. Detector 2 serves as
	;   the optics for Gun 1, so get the Gun1 position with respect to detector2.
	gun1_pos_ocs = mms_instr_origins_instr('EDI1_GUN', 'EDI2_DETECTOR')
	gun2_pos_ocs = mms_instr_origins_instr('EDI2_GUN', 'EDI1_DETECTOR')

	;Spin-up positions
	gun1_pos_docs = mms_dss_despin(srt, t_beam1, gun1_pos_ocs, OMEGA=omega)
	gun2_pos_docs = mms_dss_despin(srt, t_beam2, gun2_pos_ocs, OMEGA=omega)

;-----------------------------------------------------
; Firing Voltages as Angles in DOCS \\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert analog firing voltages to cartesian firing direction.
	gun1_fire_cart = mms_edi_aVoltage2Angle(vax1, vay1, /CARTESIAN)
	gun2_fire_cart = mms_edi_aVoltage2Angle(vax2, vay2, /CARTESIAN)

	;Matrix from EDI to OCS
	edi12ocs = mms_instr_xxyz2ocs('EDI1_GUN')
	edi22ocs = mms_instr_xxyz2ocs('EDI2_GUN')
	
	;Rotate to OCS
	gun1_fire_ocs = rotate_vector(edi12ocs, gun1_fire_cart)
	gun2_fire_ocs = rotate_vector(edi22ocs, gun2_fire_cart)

	;Despin firing directions to DOCS
	gun1_fire_docs = mms_dss_despin(srt, t_beam1, gun1_fire_ocs, OMEGA=omega)
	gun2_fire_docs = mms_dss_despin(srt, t_beam2, gun2_fire_ocs, OMEGA=omega)

;-----------------------------------------------------
; Project into BPP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Replace fillvals
	if n_elements(fillval) gt 0 then begin
		b_beam1_docs = replace_fillval(b_beam1_docs, fillval)
		b_beam2_docs = replace_fillval(b_beam2_docs, fillval)
	endif
	
	;Transform from OCS into BPP
	ocs2bpp       = mms_instr_xb2bpp(b_beam1_docs)
	gun1_pos_bpp  = rotate_vector(ocs2bpp, gun1_pos_docs)
	gun1_fire_bpp = rotate_vector(ocs2bpp, gun1_fire_docs)
	
	;Transformation matrix from OCS into BPP
	ocs2bpp       = mms_instr_xb2bpp(b_beam2_docs)
	gun2_pos_bpp  = rotate_vector(ocs2bpp, gun2_pos_docs)
	gun2_fire_bpp = rotate_vector(ocs2bpp, gun2_fire_docs)

;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if arg_present(gun_info_docs) then begin
		gun_info_docs = { gun1_pos_docs:  temporary(gun1_pos_docs), $
		                  gun1_fire_docs: temporary(gun1_fire_docs), $
		                  gun2_pos_docs:  temporary(gun2_pos_docs), $
		                  gun2_fire_docs: temporary(gun2_fire_docs) $
		                }
	endif

	gun_info_bpp = { gun1_pos_bpp:  temporary(gun1_pos_bpp), $
	                 gun1_fire_bpp: temporary(gun1_fire_bpp), $
	                 gun2_pos_bpp:  temporary(gun2_pos_bpp), $
	                 gun2_fire_bpp: temporary(gun2_fire_bpp) $
	               }
	
	return, gun_info_bpp
end