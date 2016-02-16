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
;    MMS, Utility
;
; :Params:
;       ROOT:       in, required, type=string
;                   Root directory in which to save/retrieve files. If the empty
;                       string is given, ROOT will become the current directory.
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
;       VERSION:    in, required, type=string
;                   Version number formatted as "X.Y.Z".
;       SUFFIX:     in, optional, type=string, default=''
;                   Suffix of file name. This encompasses the file extension.
;
; :Keywords:
;       DEPTH:      in, optional, type=string, default='mm'
;                   Depth of the SDC directory structure. A complete directory tree is::
;                           `ROOT`/sc/instr/mode/level/optdesc/yyyy/mm/dd
;                       As an example, if DEPTH='mm', then the output path will be::
;                           `ROOT`/sc/instr/mode/level/optdesc/yyyy/mm/
;       DIRS:       in, optional, type=string, default='/sc/instr/mode/level/optdesc/yyyy/mm'
;                   Path elements and their order, joined by the system path separator.
;                       May contain any of these values: 'sc', 'instr', 'mode', 'level', 
;                       'optdesc', 'yyyy', 'mm', 'dd'.
;       NOMKDIR:    in, optional, type=boolean, default=0
;                   If set, do not attempt to make the directory. By default, the
;                       resulting path will be created.
;       NOBASE:     in, optional, type=boolean, default=0
;                   If set, return only the path.
;       NOPATH:     in, optional, type=boolean, defualt=0
;                   If set, return only the file base name.
;
; :Returns:
;       OUT:        The resulting directory path and/or file name.
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
;       2015/11/19  -   Written by Matthew Argall
;-
function mms_build_path, root, sc, instr, mode, level, optdesc, tstart, version, suffix, $
DIRS=dirs, $
DEPTH=depth, $
NOMKDIR=nomkdir, $
NOBASE=nobase, $
NOPATH=nopath
	compile_opt idl2
	on_error, 2
	
	;Defaults
	mkdir = ~keyword_set(nomkdir)
	if n_elements(suffix) eq 0  then suffix = ''
	if root               eq '' then cd, CURRENT=root
	
	;Depth
	if n_elements(depth) gt 0 then begin
		subdirs = ['/', 'sc', 'instr', 'mode', 'level', 'optdesc', 'yyyy', 'mm', 'dd']
		idepth  = where(subdirs eq depth, ndepth)
		if ndepth eq 0 $
			then message, 'Invalid value for DEPTH: "' + depth + '".' $
			else dirs = strjoin(subdirs[0:idepth], path_sep())
	endif

	;Path
	if n_elements(dirs) eq 0 then begin
		case mode of
			'brst':       dirs = strjoin(['sc', 'instr', 'mode', 'level', 'optdesc', 'yyyy', 'mm', 'dd'], path_sep())
			'hirangecal': dirs = strjoin(['sc', 'instr', 'mode', 'level'], path_sep())
			'lorangecal': dirs = strjoin(['sc', 'instr', 'mode', 'level'], path_sep())
			else:         dirs = strjoin(['sc', 'instr', 'mode', 'level', 'optdesc', 'yyyy', 'mm'], path_sep())
		endcase
	endif
	
	;Time stamp
	mms_parse_time, tstart, year, month, day

	;Build the path
	;   - Allows user to create path in whatever order they want
	dirs    = strsplit(dirs, path_sep(), COUNT=ndirs, /EXTRACT)
	path    = root
	pathsep = path_sep()
	for i = 0, ndirs - 1 do begin
		case dirs[i] of
			'sc':      path += pathsep + sc
			'instr':   path += pathsep + instr
			'mode':    path += pathsep + mode
			'level':   path += pathsep + level
			'optdesc': path += pathsep + optdesc
			'yyyy':    path += pathsep + year
			'mm':      path += pathsep + month
			'dd':      path += pathsep + day
			'':        ;do nothing
			else: message, 'Invalid directory element: "' + dirs[i] + '".'
		endcase
	endfor

	;filepath
	if mkdir && ~file_test(path, /DIRECTORY) then file_mkdir, path
	
	;Output file name
	base = sc      + '_'  + $
	       instr   + '_'  + $
	       mode    + '_'  + $
	       level   + '_'  + $
	       optdesc + (optdesc[0] eq '' ? '' : '_') + $
	       tstart  + '_v' + $
	       version        + $
	       suffix
	
	;Return which part?
	case 1 of
		keyword_set(nobase): return, path
		keyword_set(nopath): return, fname
		else:                return, filepath(base, ROOT_DIR=path)
	endcase
end