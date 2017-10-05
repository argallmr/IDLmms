; docformat = 'rst'
;
; NAME:
;       mms_fsm_config
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
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
;+
;   Establish the system variable !MMS_FSM that contains standardized directories
;   in the FSM data processing chain. Environment variables checked are:
;       FSM_IDL_ROOT        - Location of the IDL library
;       CONTIG_BRST_ROOT    - Location of the contiguous burst interval files
;
; :Categories:
;    MMS
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
;       2017/08/09  -   Written by Matthew Argall
;-
PRO mms_fsm_config
	Compile_Opt idl2
	On_Error, 2
	
	;Default locations
	mms_fsm = { fsm_idl_root:     '/home/argall/IDL/MMS/fsm/', $
	            contig_brst_root: '/home/argall/MATLAB/MMS/contig_brst/' }
	
	;Establish the system variable
	DefSysV, '!mms_fsm', EXIST=tf_exist
	IF ~tf_exist THEN DefSysV, '!mms_fsm', mms_fsm
	
	;Check environment variables
	fsm_idl_root = GetEnv('FSM_IDL_ROOT')
	IF fsm_idl_root NE '' THEN mms_fsm.fsm_idl_root = fsm_idl_root
	
	;Contiguous burst segments
	contig_brst_root = GetEnv('CONTIG_BRST_ROOT')
	IF contig_brst_root EQ '' THEN mms_fsm.contig_brst_root = contig_brst_root
END