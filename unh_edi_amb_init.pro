; docformat = 'rst'
;
; NAME:
;    mms_unh_init
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
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
;   Set parameters required for data processing.
;
;   Creates a system variable
;       !UNH_AMB_INIT:      A configuration structure with the following tags::
;                               DROPBOX    -  Location to which newly processed data is
;                                             initially saved.
;                               DATA_PATH  -  Location to which DROPBOX data is moved
;                                             after being processed. It is the root of
;                                             the SDC directory structure.
;                               LOG_PATH   -  Location to which log files are saved.
;                               STATUS     -  Status flag::
;                                                 0        - Everything OK
;                                                 1-99     - Warning
;                                                 100-255  - Error
;
; :Categories:
;    MMS
;
; :Returns:
;
; :Author:
;    Matthew Argall::
;        University of New Hampshire
;        Morse Hall Room 348
;        8 College Road
;        Durham, NH 03824
;        matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/10/26  -   Written by Matthew Argall
;       2015/11/19  -   Changed to procedure. Created system variable. - MRA
;       2016/01/27  -   Renamed from unh_edi_init to unh_edi_amb_init. - MRA
;-
pro unh_edi_amb_init, $
RESET=reset
	compile_opt idl2
	on_error, 2
	
	;Default locations
	edi_amb_init = { dropbox:   '/nfs/edi/temp', $
	                 data_path: '/nfs', $
	                 log_path:  '/nfs/edi/logs', $
	                 status:    0B $
	               }
	
	;Create or reset the system variable
	defsysv, '!edi_amb_init', EXISTS=exists
	if ~exists then begin
		defsysv, '!edi_amb_init', edi_amb_init
	endif else if keyword_set(reset) then begin
		!edi_amb_init = edi_amb_init
	endif

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Check environment variables
	dropbox_root = getenv('DROPBOX_ROOT')
	if dropbox_root ne '' then !edi_amb_init.dropbox = dropbox_root
	
	data_path = getenv('DATA_PATH_ROOT')
	if data_path ne '' then !edi_amb_init.data_path = data_path
	
	log_path = getenv('LOG_PATH_ROOT')
	if log_path ne '' then !edi_amb_init.log_path = log_path
end