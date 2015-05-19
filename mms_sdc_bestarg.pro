; docformat = 'rst'
;
; NAME:
;       mms_edi_bestarg
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
;   Prepare EDI data for input into bestarg.pro
;
; :Categories:
;   MMS, EDI, Bestarg
;
; :Returns:
;       WIN:        Graphics window containing the plot of EDI beams.
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
;       2015/05/19  -   Written by Matthew Argall
;-
function mms_sdc_bestarg
	compile_opt idl2
	on_error, 2

	sc           = 'mms4'
	tstart       = '2015-04-22T17:03:15Z'
	tend         = '2015-04-22T17:03:30Z'
	quality      = 3

;-----------------------------------------------------
; Find Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;DFG L1B
	instr = 'dfg'
	mode  = 'srvy'
	level = 'l1b'
	fg_l1b_files = mms_sdc_find_file(sc, instr, mode, level, $
	                                 COUNT     = count, $
	                                 SEARCHSTR = searchstr, $
	                                 TSTART    = tstart, $
	                                 TEND      = tend)
	if count eq 0 then message, 'DFG L1B files not found: "' + searchstr + '".'

	;DFG QUICK-LOOK
	mode  = 'srvy'
	level = 'ql'
	fg_ql_files = mms_sdc_find_file(sc, instr, mode, level, $
	                                COUNT     = count, $
	                                SEARCHSTR = searchstr, $
	                                TSTART    = tstart, $
	                                TEND      = tend)
	if count eq 0 then message, 'DFG Quick-Look (QL) files not found: "' + searchstr + '".'

	;EDI EFIELD
	instr = 'edi'
	mode  = 'srvy'
	level = 'ql'
	fg_ql_files = mms_sdc_find_file(sc, instr, mode, level, $
	                                COUNT     = count, $
	                                OPTDESC   = 'efield', $
	                                SEARCHSTR = searchstr, $
	                                TSTART    = tstart, $
	                                TEND      = tend)
	if count eq 0 then message, 'EDI efield-mode files not found: "' + searchstr + '".'

;-----------------------------------------------------
; Call Bestarg \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	result = mms_edi_bestarg(edi_files, fg_l1b_files, fg_ql_files, $
	                         TSTART = tstart, $
	                         TEND   = tend)
	
	return, result
end
