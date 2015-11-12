; docformat = 'rst'
;
; NAME:
;    mms_mkdir
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
;   Create a directory structure consistent with the MMS SDC. Result is::
;       ROOT/SC/INSTR/MODE/LEVEL[/OPTDESC]/YEAR/MONTH[/DAY]
;
;   Where /DAY is include if MODE = 'brst'
;
; :Categories:
;    MMS
;
; :Params:
;       SC:         in, required, type=string
;                   Spacecraft for which to process data. Options are::
;                       'mms1', 'mms2', 'mms3', or 'mms4'
;       INSTR:      in, required, type=string
;                   Instrument ID
;       MODE:       in, required, type=string
;                   Telemetry mode.
;       LEVEL:      in, required, type=string
;                   Data quality level
;       OPTDESC:    in, required, type=string
;                   Optional descriptor
;       TSTART:     in, required, type=string
;                   Start time. Formatted as either 'YYYYMMDD' or 'YYYYMMDDhhmmss'.
;
; :Keywords:
;       OUT:        out, optional, type=string
;                   Resulting directory path.
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
;       2015/11/11  -   Written by Matthew Argall
;-
pro mms_mkdir, root, sc, instr, mode, level, optdesc, tstart, $
OUT=outdir
	compile_opt idl2
	on_error, 2
	
	;Is it possible to create directories?
	if ~file_test(root, /DIRECTORY, /WRITE) then message, 'ROOT is not writable "' + root + '".'
	
	;Time stamp
	tlen = strlen(tstart)
	case tlen of
		8: begin
			yrdir  = strmid(tstart, 0, 4)
			modir  = strmid(tstart, 4, 2)
			daydir = ''
		endcase
		
		14: begin
			yrdir  = strmid(tstart, 0, 4)
			modir  = strmid(tstart, 4, 2)
			daydir = strmid(tstart, 6, 2)
		endcase
		
		else: message, 'Unexpected file start time: "' + tstart + '".'
	endcase
	
	;Final output destination
	if mode eq 'brst' $
		then subdirs = [sc, instr, mode, level, optdesc, yrdir, modir, daydir] $
		else subdirs = [sc, instr, mode, level, optdesc, yrdir, modir]
	
	;Make the directory if it does not exist
	outdir = filepath('', ROOT_DIR=root, SUBDIRECTORY=subdirs)
	if ~file_test(outdir) then begin
		;Test each subdirectory
		for i = 0, n_elements(subdirs) - 1 do begin
			outdir = filepath('', ROOT_DIR=root, SUBDIRECTORY=subdirs[0:i])
			if ~file_test(outdir) then file_mkdir, outdir
		endfor
	endif
end