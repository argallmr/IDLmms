; docformat = 'rst'
;
; NAME:
;
;       MMS_DISSECT_FILENAME
;
; PURPOSE:
;+
;       The purpose of this program is to dissect the filename of a data file from the
;       MMS mission.
;
;       scId_instrumentId_paramName_optionalDescriptor
;
; :Categories:
;       MMS
;
; :Params:
;       VARNAME:            in, required, type=string
;                           Name of the MMS variable to be dissected.
;
; :Keywords:
;       DESCRIPTOR:         out, optional, type=string
;                           Optional descriptor for variable's data.
;       INSTRUMENT:         out, optional, type=string
;                           Instrument ID. Possible values are::
;                               'hpca'              'edi'
;                               'aspoc'             'adp'
;                               'epd'               'sdp'
;                               'epd-eis'           'adp-sdp'
;                               'epd-feeps'         'afg'
;                               'fpi'               'dfg'
;                               'des'               'afg-dfg'
;                               'dis'               'scm'
;                               'des-dis'           'fields'
;       PARAM_NAME:         out, optional, type=string
;                           Data parameter.
;       SPACECRAFT:         out, optional, type=string
;                           The spacecraft ID. Possible values are::
;                               'mms1'
;                               'mms2'
;                               'mms3'
;                               'mms4'
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2013
;
; :History::
;   Modification History::
;       2015/02/06  -   Written by Matthew Argall
;-
pro mms_dissect_varname, varname, $
DESCRIPTOR=descriptor, $
INSTRUMENT=instrument, $
PARAM_NAME=param_name, $
SPACECRAFT=spacecraft
	compile_opt strictarr
	on_error, 2

	;Check that a filename was provided.
	if n_elements(filename) eq 0 then message, 'A variable name must be given.'

;-----------------------------------------------------
;DISSECT FILENAMES \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	str = stregex(varname, '(mms[1-4])_'       + $                ;Spacecraft ID
	                        '([a-z-]+)_'        + $                ;Instrument ID
	                        '([a-z0-4]+)_'      + $                ;Parameter Name
	                        '([a-zA-Z0-9-]*)_?', $                 ;Optional Descriptor
	                        /EXTRACT, /SUBEXP)

	;Find non-matches
	iFail = where(str[0,*] eq '', nFail, COMPLEMENT=iPass, NCOMPLEMENT=nPass)
	if nFail gt 0 then begin
		message, 'Cannot dissect variable names: ', /INFORMATIONAL
		print, '    "' + transpose(filename[iFail]) + '"'
	endif
	if nPass eq 0 then return

	;Extract the subexpressions
	spacecraft = str[1,iPass]
	instrument = str[2,iPass]
	param_name = str[3,iPass]
	descriptor = str[5,iPass]
	
	;Return scalars?
	if nPass eq 1 then begin
		spacecraft = spacecraft[0]
		instrument = instrument[0]
		param_name = param_name[0]
		descriptor = descriptor[0]
	endif
end