; docformat = 'rst'
;
; NAME:
;    mms_sdc_ql_BEfields
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
;   Create a quick-look plot of the magnetic and electric fields.
;
; :Categories:
;    MMS, QL
;
; :Params:
;
; :Keywords:
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 348
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/03/15  -   Written by Matthew Argall
;-
pro mms_ql_edi_efield_script
	compile_opt strictarr
	on_error, 2
	
	;Constants
	sdc_dir  = '/nfs/mmsa/sdc/'
	edi_dir  = '/nfs/mmsa/matt/'
	save_dir = '/home/argall/mms_figures/'

;-------------------------------------------------------
; Search For EDI E-Field Files /////////////////////////
;-------------------------------------------------------
	sc      = 'mms%([1-4]%)'
	instr   = 'edi'
	mode    = 'slow'
	level   = 'ql'
	optdesc = 'efield'
	
	;Search for file
	files_edi = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles_edi, $
	                          DIRECTORY = edi_dir, $
	                          OPTDESC   = optdesc, $
;	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr)
	if nfiles_edi eq 0 then $
		message, 'No EDI files found: "' + searchstr + '".'

;-------------------------------------------------------
; Extract Data Intervals ///////////////////////////////
;-------------------------------------------------------
	;Dissect the file names
	mms_dissect_filename, files_edi, SC=sc, TSTART=fstart

	;Turn to ISO format
	MrTimeParser, reform(fstart), '%Y%M%d', '%Y-%M-%d', date_temp
	tstart = date_temp + 'T00:00:00Z'
	tend   = date_temp + 'T24:00:00Z'
	
	;Plot the data
	for i = 0, nfiles_edi - 1 do begin
		win = mms_sdc_ql_edi_efield(sc[i], tstart[i], tend[i], SAVE_DIR=save_dir)
		obj_destroy, win
	endfor
	
end