; docformat = 'rst'
;
; NAME:
;       mms_edi_combine_srvy
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
;   Combine flow and fast survey structures returned by mms_edi_read_*.
;
; :Categories:
;   MMS, EDI
;
; :Params:
;       SLOW:               in, required, type=struct
;                           Data structure containing EDI slow survey data.
;       FAST:               in, required, type=struct
;                           Data structure containing EDI fast survey data.
;
; :Returns:
;       SRVY:               Structure that is a combination of `SLOW` and `FAST`.
;                             Fields are concatenated.
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
;       2015/08/22  -   Written by Matthew Argall
;-
function mms_edi_combine_srvy, slow, fast
	compile_opt idl2
;	on_error, 2
	
;-----------------------------------------------------
; Check Input Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	nslow = n_elements(slow)
	nfast = n_elements(fast)
	if nslow + nfast eq 0 then message, 'Usage: srvy = mms_edi_combine_srvy(slow, fast)'
	
	;No fast survey data
	if nfast eq 0 then begin
		srvy = slow
		
	;No slow survey data
	endif else if nslow eq 0 then begin
		srvy = fast
		
	;Combine fast and slow
	endif else begin
		srvy = []
	
		;Get the time stamps so data can be sorted
		tt2000_gd12 = [slow.tt2000_gd12, fast.tt2000_gd12]
		tt2000_gd21 = [slow.tt2000_gd21, fast.tt2000_gd21]
		
		;Sort data
		isort_gd12 = sort(tt2000_gd12)
		isort_gd21 = sort(tt2000_gd21)
		
		;Find unique elements
		iuniq_gd12 = uniq(tt2000_gd12, isort_gd12)
		iuniq_gd21 = uniq(tt2000_gd21, isort_gd21)
		
		;Proper order
		iorder_gd12 = isort_gd12[iuniq_gd12]
		iorder_gd21 = isort_gd12[iuniq_gd21]
	
		;Get the names of all fields
		;   - SLOW nad FAST should have the same fields
		slow_fields = tag_names(slow)
		fast_fields = tag_names(fast)
		nfields     = n_elements(slow_fields)
		
		;Separate by guns
		;   - Each gun should have the same fields
		igd12 = where( stregex(slow_fields, '(gd12|gun1)', /BOOLEAN), ngd12, $
		               COMPLEMENT=igd21, NCOMPLEMENT=ngd21)
		
		;Step through each field
		for i = 0, nfields / 2 do begin
			;
			; Start with Gun1
			;
			
			;Pick out the index of the common field
			theField = slow_fields[igd12[i]]
			islow    = where(slow_fields eq theField)
			ifast    = where(fast_fields eq theField)
			
			;Combine the data
			tmp_data = [[slow.(islow)], [fast.(ifast)]]
			srvy     = create_struct(srvy, theField, temporary(tmp_data[*,iorder_gd12]))
			
			;
			; Repeat for Gun2
			;
			
			;Pick out the index of the common field
			theField = slow_fields[igd21[i]]
			islow    = where(slow_fields eq theField)
			ifast    = where(fast_fields eq theField)
			
			;Combine the data
			tmp_data = [[slow.(islow)], [fast.(ifast)]]
			srvy     = create_struct(srvy, theField, (temporary(tmp_data))[*,iorder_gd121])
		endfor
	endelse
	
	return, srvy
end
