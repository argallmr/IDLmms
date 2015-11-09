; ***************************************************************************
; MMS FIELDS -- MMSEDI_GetCdfFillValue()
;
; Return fill value used for floating point data in MMS CDF files
;
; Created: June 1, 2015
; Author:  Hans Vaith, University of New Hampshire, Space Science Center
; ***************************************************************************

; ===========================================================================
  FUNCTION MMSEDI_GetCdfFillValue, epsilon=epsilon
; ===========================================================================
; Keyword epsilon can be used to return a value that is slightly higher than
; the (negative) fill value. This can be used in case there is doubt about
; the validity of using a comparison for equal with floating point numbers
; (as there should be, even with constants, when crossing between machines
; with different standards for implementing the floating point standard)
; ***************************************************************************

    cdf_fill_value = -1.0e+31

    if keyword_set(epsilon) then ret_val = cdf_fill_value + 1.e26 $
    else                         ret_val = cdf_fill_value

    return, ret_val

END
