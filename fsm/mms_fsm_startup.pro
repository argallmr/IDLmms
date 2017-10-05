; docformat = 'rst'
;
; NAME:
;       mms_fsm_startup
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
;   Startup utility for FSM data processing. Sets IDL path, among other things.
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
PRO mms_fsm_startup
	Compile_Opt idl2
	On_Error, 2
	
	;Root of the IDL library
	root_dir = Routine_Info('mms_fsm_startup', /SOURCE)
	root_dir = FilePath( '', $
	                     ROOT_DIR     = File_DirName(root_dir.path), $
	                     SUBDIRECTORY = ['..', '..'] )
	
	;Add the library elements to the path
	!Path = Expand_Path('<IDL_DEFAULT>')
	!Path = !Path + ':' + Expand_Path('+/' + FilePath( '', ROOT_DIR=root_dir, SUBDIRECTORY='brommund' ) )
	!Path = !Path + ':' + Expand_Path('+/' + FilePath( '', ROOT_DIR=root_dir, SUBDIRECTORY='CDF' ) )
	!Path = !Path + ':' + Expand_Path('+/' + FilePath( '', ROOT_DIR=root_dir, SUBDIRECTORY='coyote' ) )
	!Path = !Path + ':' + Expand_Path('+/' + FilePath( '', ROOT_DIR=root_dir, SUBDIRECTORY='fileIO' ) )
	!Path = !Path + ':' + Expand_Path('+/' + FilePath( '', ROOT_DIR=root_dir, SUBDIRECTORY='lib' ) )
	!Path = !Path + ':' + Expand_Path('+/' + FilePath( '', ROOT_DIR=root_dir, SUBDIRECTORY='mrvariable' ) )
	!Path = !Path + ':' + Expand_Path('+/' + FilePath( '', ROOT_DIR=root_dir, SUBDIRECTORY='timeparser' ) )
	!Path = !Path + ':' + Expand_Path('+/' + FilePath( '', ROOT_DIR=root_dir, SUBDIRECTORY=['MMS', 'fsm'] ) )
	!Path = !Path + ':' + Expand_Path('+/' + FilePath( '', ROOT_DIR=root_dir, SUBDIRECTORY=['MMS', 'utils'] ) )
	
	;So that windows can be pushed through
	PREF_SET, 'IDL_GR_X_RENDERER', 1, /COMMIT
END