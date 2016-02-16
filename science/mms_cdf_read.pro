; docformat = 'rst'
;
; NAME:
;    mms_cdf_read
;
; PURPOSE:
;+
;   Read data from a single variable in any official MMS data product.
;
; :Categories:
;    MMS, Utility
;
; :See Also:
;   mms_cdf_variables.pro
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
;       2015/11/21  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Return CDF variable names.
;
; :Examples:
;   Read MMS1 DFG SRVY L2PRE magnetic field data in GSE coordinates on 2015-08-16
;       IDL> data = mms_cdf_read('gse', 'mms1', 'dfg', 'srvy', 'l2pre', '2015-08-16T00:00:00Z', '2015-08-16T24:00:00Z', DEPEND_0=t)
;       IDL> help, data, t
;           DATA            FLOAT     = Array[4, 1098281]
;           T               LONG64    = Array[1, 1098281]
;
; :Params:
;       VARNAMES:       in, required, type=string/strarr
;                       Requested variable names. These are the CDF variable names, but
;                           with `SC`, `INSTR`, `MODE`, `LEVEL` stripped from the
;                           beginning of the name.
;       SC:             in, required, type=string
;                       Spacecraft identifier.
;       INSTR:          in, required, type=string
;                       Instrument identifier.
;       MODE:           in, required, type=string
;                       Data rate mode.
;       LEVEL:          in, required, type=string
;                       Data level.
;       TSTART:         in, required, type=string
;                       Start time of data interval. Format: 'YYYY-MM-DDThh:mm:ssZ'
;       TEND:           in, required, type=string
;                       End time of data interval. Format: 'YYYY-MM-DDThh:mm:ssZ'
;       OPTDESC:        in, optional, type=string
;                       Optional variable name descriptor.
;
; :Keywords:
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrCDF_nRead is also accepted via
;                           keyword inheritance.
;
; :Returns:
;       DATA:           Data corresponding to the intput parameters.
;-
function mms_cdf_read, varname, sc, instr, mode, level, tstart, tend, optdesc, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Can read only one variable at a time.
	if n_elements(varname) ne 1 then message, 'VARNAME must be a scalar string.'

;-----------------------------------------------------
; Find the File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find the file(s)
	theFile = mms_find_file(sc, instr, mode, level, $
	                        COUNT     = count, $
	                        OPTDESC   = optdesc, $
	                        SEARCHSTR = srchstr, $
	                        TSTART    = tstart, $
	                        TEND      = tend)
	if count eq 0 then begin
		MrPrintF, 'LogText', strjoin(['sc', 'instr', 'mode', 'level', 'optdesc'], '_') + '".'
		MrPrintF, 'LogText', 'Between ' + tstart + ' and ' + tend
		message, 'Could not find MMS file: "' + srchstr + '".'
	endif else if count gt 3 then begin
		MrPrintF, 'LogWarn', count, FORMAT='(%"Reading %i files...")'
	endif

	;Find the variable name
	variable = mms_cdf_variables(varname, sc, instr, mode, level, optdesc)
	if n_elements(variable) eq 0 then return, !Null

;-----------------------------------------------------
; Read the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	data = MrCDF_nRead(theFile, variable, $
	                   TSTART        = tstart, $
	                   TEND          = tend, $
	                   _STRICT_EXTRA = extra)

	return, data
end
