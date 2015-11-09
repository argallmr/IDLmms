; docformat = 'rst'
;
; NAME:
;       ep_prep_order
;
;+
;   Calculate equivalent single runner time of flight
;
; :Params:
;       MTOF:           in, required, type=float/fltarr
;                       Measured time of flight as reported in telemetry.
;       ORDER:          in, optional, type=integer/intarr
;                       Assumed runner order.
;       TGS:            in, required, type=integer/intarr
;                       Single gyration gyrotime.
;       TCODE:          in, required, type=integer/intarr
;                       Code length.
;
; :Returns:
;       STATUS:         Structure with the following tags::
;                           STATUS - 0 = ok, 1 = error
;                           MSG    - Contains error message if status = 1
;                           ESTOF  - Equivalent single runner time of flight.
;                                    Only present if status = 0.
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
;       2015-10-06  -   Written by Matthew Argall. Adapted from EquivSrToF written
;                           Cluster's bestarg.
;-
FUNCTION EquivSrToF, mtof, order, tgs, tcode
	compile_opt idl2
	on_error, 2

	;Initialize return structure
	ret = { status:0, msg:'' }

;---------------------------------------------------------------------
; Check Inputs ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Number of parameters.
	if n_params() ne 4 then begin
		ret.status = 1
		ret.msg = 'EquivSrTof(): need 4 parameters, but there are ' + $
		          strtrim(n_params(), 2)
		return, ret
	endif

	ndim = n_elements(mtof)

	;ORDER must be scalar or same size as MTOF
	if n_elements(order) eq 1 then begin
		l_order = intarr(ndim) + order
	endif else if n_elements(order) ne ndim then begin
		ret.status = 1
		ret.msg = 'EquivSrToF(): bad array size of parameter <order>'
		return, ret
	endif else begin
		l_order = order
	endelse

	;TGS must be scalar or same size as MTOF
	if n_elements(tgs) eq 1 then begin
		l_tgs = intarr(ndim) + tgs
	endif else if n_elements(tgs) ne ndim then begin
		ret.status = 1
		ret.msg = 'EquivSrToF(): bad array size of parameter <tgs>'
		return, ret
	endif else begin
		l_tgs = tgs
	endelse

	;TCODE must be scalar or same size as MTOF
	if n_elements(tcode) eq 1 then begin
		l_tcode = intarr(ndim) + tcode
	endif else if n_elements(tcode) ne ndim then begin
		ret.status = 1
		ret.msg = 'EquivSrToF(): bad array size of parameter <tcode>'
		return, ret
	endif else begin
		l_tcode = tcode
	endelse

	;Re-define output structure with ESTOF tag.
	ret = { status:0, msg:'', estof: fltarr(ndim) }

;---------------------------------------------------------------------
; Determine Time of Flight ///////////////////////////////////////////
;---------------------------------------------------------------------
	;
	; Find a suitable multiplier i that fits the equation
	; (I) mtof + i * tcode ~= order * tgs
	; left side : true multi-runner time of flight
	; right side: n-fold gyro-time
	;
	; The equation can more exactly be written as
	; (1) mtof + i * tcode < order * tgs + 0.5 * tcode
	; (2) mtof + i * tcode > order * tgs - 0.5 * tcode
	;
	;                           order * tgs - mtof
	; this yields : i = floor ( ------------------  + 0.5 )
	;                                 tcode
	;

	;Determine I
	i = floor( (l_order*l_tgs - mtof)/l_tcode  + 0.5 )

	;Compute single runner time of flight
	ret.estof = (mtof + i * l_tcode) / l_order

	return, ret
END