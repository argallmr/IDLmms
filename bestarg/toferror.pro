; ============================================================================
  FUNCTION TofError, tofarr1, tofarr2
; ============================================================================
; Project: Cluster-II EDI
; Author : Hans Vaith, MPE Garching
; Creation Date: 31-July-2001
;
; TofError: calculate error in the difference of two Time-of-Flight
;           distributions
;           If tofarr2 is not present, it is assumed that tofarr1 is
;           an array of delta tof's (from nearly simultaneous times-of-flight)
;           for which the error is to be calculated. The formula from the
;           Numerical Recipes book is not used in this case.
;
; ============================================================================
; Id: toferror.pro,v 1.3 2001/09/19 08:08:16 hav Exp hav
; Log: toferror.pro,v
; Revision 1.3  2001/09/19  08:08:16  hav
; extended for use with nearly simultaneous times-of-flight
;
; Revision 1.2  2001/09/13 09:30:48  hav
; corrected some factors of student-t distribution according to mfo's program
; added 90 percent confidence error
;
; Revision 1.1  2001/07/31 14:29:11  hav
; Initial revision
;
; ============================================================================

  ret   = { status:0, $
            msg:'', $
            dtof:   0.0, $  ; difference of ToF averages
            err_t68:0.0, $  ; t-scaled, 68.3% confidence
            err_t90:0.0, $  ; t-scaled, 90% confidence
            err_t95:0.0, $  ; t-scaled, 95% confidence
            err_nr: 0.0 $   ; Numerical Recipes Formula
           }


  ; check arguments
  ; ---------------
  if n_params() eq 1 then begin
     tofarr2 = [ 0.0, 0.0 ] ; make up a dummy array with average=0 and stdev=0
  endif else if n_params() ne 2 then begin
     ret.status = 1
     ret.msg    = 'TofError(): need one or two parameters'
     return, ret
  endif

  n1 = n_elements(tofarr1)
  n2 = n_elements(tofarr2)

  if (n1 lt 2 or n2 lt 2) then begin
     ret.status = 1
     ret.msg    = 'TofError(): not enough array elements: ' + $
                   strtrim(n1,2) + ' / ' + strtrim(n2,2)
     return, ret
  endif

  ; now do the calculations
  ; -----------------------
  avg1 = total(tofarr1) / n1
  avg2 = total(tofarr2) / n2

  ret.dtof = avg1-avg2

  sqsum1 = total( (tofarr1-avg1)^2 )
  sqsum2 = total( (tofarr2-avg2)^2 )

  n1factor = sqrt( 1./(n1-1) * sqsum1 ) / sqrt(n1) ; stdev(tofarr1) / sqrt(n1)
  n2factor = sqrt( 1./(n2-1) * sqsum2 ) / sqrt(n2) ; stdev(tofarr2) / sqrt(n2)


  narr   = [ 2,     3,     4,     5,     6,     8,     10,    20,    30 ]
  t68arr = [ 1.839, 1.322, 1.198, 1.142, 1.111, 1.077, 1.059, 1.028, 1.018]
  t90arr = [ 6.314, 2.920, 2.353, 2.132, 2.015, 1.895, 1.833, 1.729, 1.699]
  t95arr = [ 12.71, 4.303, 3.182, 2.776, 2.571, 2.365, 2.262, 2.093, 2.045]


  err_t68_1 = interpol(t68arr, narr, n1 ) * n1factor
  err_t68_2 = interpol(t68arr, narr, n2 ) * n2factor
  ret.err_t68 = sqrt(err_t68_1^2 + err_t68_2^2)


  err_t90_1 = interpol(t90arr, narr, n1 ) * n1factor
  err_t90_2 = interpol(t90arr, narr, n2 ) * n2factor
  ret.err_t90 = sqrt(err_t90_1^2 + err_t90_2^2)


  err_t95_1 = interpol(t95arr, narr, n1 ) * n1factor
  err_t95_2 = interpol(t95arr, narr, n2 ) * n2factor
  ret.err_t95 = sqrt(err_t95_1^2 + err_t95_2^2)

  if (n_params() eq 2) then $
     ret.err_nr = sqrt ( (sqsum1+sqsum2)/(n1+n2-2) * (1./n1 + 1./n2) )


  return, ret

END
