; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_error_codes
;
; PURPOSE:
;+
;   MMS EDI Ambient mode error codes and definitions.
;
; :Params:
;       CODE:       in, optional, type=integer
;                   The error code for which the corresponding error message is returned.
;                       If not given, a hash is returned. Its keys are the error codes
;                       and its values are the corresponding messages.
;
; :Keywords:
;       PRINT:      in, optional, type=boolean, default=0
;                   If set, error codes and their definitions will be printed to
;                       the terminal window. This keyword automatically sets the
;                       `TXT` keyword.
;       TXT:        in, optional, type=boolean, default=0
;                   If set, error codes and their definitions will be returned as
;                       an array of strings. `CODE` is ignored.
;
; :Returns:
;       OUTPUT:     The error message. See keywords also.
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
;       2015/02/26  -   Written by Matthew Argall
;-
function mms_edi_amb_error_codes, code, $
PRINT=print, $
TXT=txt
	compile_opt idl2
	on_error, 2
	
	;Output to terminal or log file?
	tf_print = keyword_set(print)
	tf_txt   = keyword_set(txt) || tf_print
	
	;Error codes and their messages
	error_codes = orderedhash(   0B, 'Everything is ok.', $
	                             1B, 'One or more files with zero records.', $
	                             2B, 'Empty file created.', $
	                           100B, 'Unexpected trapped error.', $
	                           101B, 'Error reading file.', $
	                           102B, 'No data in file.', $
	                           103B, 'No file found.', $
	                           104B, 'Incompatible file versions.' $
	                         )

	;Return the message associated with an error code
	if n_elements(code) gt 0 then begin
		output = error_codes[code]
	
	;Output codes
	endif else if tf_txt then begin
		output    = strarr(n_elements(error_codes)+1)
		output[0] = string('CODE', 'MESSAGE', FORMAT='(1x, a4, 3x, a7)')

		;Loop over each element
		count = 0
		foreach msg, error_codes, code do begin
			;Make the output string
			output[count+1] = string(code, msg, FORMAT='(2x, i3, 3x, a0)')
			count       += 1
		endforeach
		
		;Output
		if tf_print then for i = 0, count-1 do print, output[i]
	
	;Return error codes
	endif else begin
		output = error_codes
	endelse
	
	return, output
end