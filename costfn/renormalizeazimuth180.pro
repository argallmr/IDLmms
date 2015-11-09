; ***************************************************************************
; MMS FIELDS -- ReNormalizeAzimuth180
;
; Transform EDI azimuth angles to range [-180,180] degrees
;
; Created: September 21, 2015
; Author:  Hans Vaith, University of New Hampshire, Space Science Center
;
; This file is a renamed copy of la_util.pro from the Cluster IDL
; directory
; ***************************************************************************


; =============================================================================
  FUNCTION ReNormalizeAzimuth180, phasearr, rad=rad
; =============================================================================
; normalize azimuth angles in phasearr to range [-180, 180]
; *****************************************************************************

    compile_opt strictarr,hidden

    outphase = phasearr

    if keyword_set(rad) then begin
        pi = !PI
    endif else begin
        pi = 180
    endelse


    while 1 eq 1 do begin
        x = where( outphase lt -pi)
        if x[0] eq -1 then break $
        else outphase[x] = outphase[x] + 2*pi
    endwhile

    while 1 eq 1 do begin
        x = where( outphase ge pi)
        if x[0] eq -1 then break $
        else outphase[x] = outphase[x] - 2*pi
    endwhile

    return, outphase

END

