; ***************************************************************************
; MMS FIELDS -- ToAw_GroupBeamDirections
;
; Sort EDI beams into two groups (towards/away from the target)
;
; Created: September 21, 2015
; Author:  Hans Vaith, University of New Hampshire, Space Science Center
;
; This file is a renamed copy of towardsawaysort2.pro from the Cluster IDL
; directory
; ***************************************************************************


; ============================================================================
  FUNCTION ToAw_BeamPhaseSort, phasearr, refphase, $ ; IN
                               avg, x1, x2           ; OUT
; ============================================================================
; Sort the gun firing angles in the Bperp plane into two groups
;
; Return
;    Combined standard deviation of the firing directions around their
;    respective average directions
; ****************************************************************************

    compile_opt strictarr,hidden

   tmpphase = ReNormalizeAzimuth180( phasearr )
   refphase = ReNormalizeAzimuth180( refphase )

   ; renormalize tmpphase to range [ refphase-270 ; refphase + 90 ]
   ; --------------------------------------------------------------
   x = where(tmpphase gt refphase+90)
   if x[0] ne -1 then tmpphase[x] = tmpphase[x] - 360

   x = where(tmpphase lt refphase-270)
   if x[0] ne -1 then tmpphase[x] = tmpphase[x] + 360

   sqsum = 0.0

   ; group 2
   ; -------
   x2 = where(tmpphase lt refphase-90, cnt2)
   if cnt2 gt 0 then begin
      ph2 = tmpphase[x2]
      avg = total(ph2)/cnt2
      sqsum =sqsum + total((ph2-avg)^2)
      if avg lt -180 then avg = avg + 360
      if avg gt 180  then avg = avg - 360
   endif

   ; group 1
   ; -------
   x1 = where(tmpphase ge refphase-90, cnt1) 
   if cnt1 gt 0 then begin
      ph1 = tmpphase[x1]
      avg = total(ph1)/cnt1
      sqsum = sqsum + total((ph1-avg)^2)
      if avg lt -180 then avg = avg + 360
      if avg gt 180  then avg = avg - 360     
   endif

   stdev = sqrt(sqsum/(cnt1+cnt2))

   return, stdev

END


; ============================================================================
  PRO ToAw_DetermineBeamGroups, phasearr, $   ; IN
                                x1, x2, $     ; OUT
                                step = step, debug = debug
; ============================================================================

    compile_opt strictarr,hidden

   if not keyword_set(step) then step = 0
   if not keyword_set(debug) then debug = 0

   if step then print, 'beam group sorting.'

   avga = 0 & x1 = 0 & x2 = 0
   refa = 0.0 & refb = 120.0 & refc = 240.0

   stdeva = ToAw_BeamPhaseSort(phasearr, refa, avga, x1, x2) 
   stdevb = ToAw_BeamPhaseSort(phasearr, refb, avgb, x1, x2)
   stdevc = ToAw_BeamPhaseSort(phasearr, refc, avgc, x1, x2)
   if step and debug then begin
      print, '1 refa=', refa, '   stdev_a=', stdeva, '  avg=', avga
      print, '1 refb=', refb, '   stdev_b=', stdevb, '  avg=', avgb
      print, '1 refc=', refc, '   stdev_c=', stdevc, '  avg=', avgc
      print, '-----------------------------------------------------'
   endif
    
   for icnt=2,6 do begin

      refa = avga & refb=avgb & refc = avgc

      stdeva = ToAw_BeamPhaseSort(phasearr, refa, avga, x1, x2)
      stdevb = ToAw_BeamPhaseSort(phasearr, refb, avgb, x1, x2)
      stdevc = ToAw_BeamPhaseSort(phasearr, refc, avgc, x1, x2)
      if step and debug then begin
         print, icnt, ' refa=', refa, '   stdev_a=', stdeva, '  avg=', avga
         print, icnt, ' refb=', refb, '   stdev_b=', stdevb, '  avg=', avgb
         print, icnt, ' refc=', refc, '   stdev_c=', stdevc, '  avg=', avgc
         print, '-----------------------------------------------------'
      endif
   endfor

   if avga gt avgb then ref = avga else ref = avgb
   if avgc gt ref  then ref = avgc
         
   stdev = ToAw_BeamPhaseSort(phasearr, ref, avg, x1, x2)
   if step and debug then begin
      print, 'final     ref=', ref, '   stdev=', stdev, '   avg=', avg
      print, '-----------------------------------------------------'
   endif

END


; ============================================================================
  PRO ToAw_CalcRefDirections, $
      phasearr, x1, x2, $ ; IN
      ref_1, ref_2, ref_c ; OUT
; ============================================================================
; ****************************************************************************

    compile_opt strictarr,hidden

    tmpphase = ReNormalizeAzimuth180( phasearr )

    ; reconstruct reference direction 1
    ; ---------------------------------
    if x1[0] eq -1 then ref1 = 0.0 $
    else begin
        phmin = min( tmpphase[x1] )
        phmax = max( tmpphase[x1] )
        if phmax-phmin gt 180 then begin
            ref_1 = total( ReNormalizeAzimuth180(tmpphase[x1]+180) ) / $
                    n_elements(x1) $
                    - 180
            ref_1 = ReNormalizeAzimuth180( ref_1 ) 
        endif else begin
            ref_1 = total( tmpphase[x1] ) / n_elements(x1)
        endelse
    endelse

    ; reconstruct reference direction 2
    ; ---------------------------------
    ;help, tmpphase, x2
    ;print, x2
    if x2[0] eq -1 then ref2 = 0.0 $
    else begin
        phmin = min( tmpphase[x2] )
        phmax = max( tmpphase[x2] )
        if phmax-phmin gt 180 then begin
            ref_2 = total( ReNormalizeAzimuth180(tmpphase[x2]+180) ) / $
                    n_elements(x2) $
                    - 180
            ref_2 = ReNormalizeAzimuth180( ref_2 ) 
        endif else begin
            ref_2 = total( tmpphase[x2] ) / n_elements(x2)
        endelse
    endelse

    ; calculate common reference direction
    ; ------------------------------------
    if x1[0] eq -1 and x2[0] eq -1 then begin
    ref_c = 0.0
    endif else if x1[0] eq -1 and x2[0] ne -1 then begin
    ref_c = ReNormalizeAzimuth180( ref_2 + 180 )
    endif else if x1[0] ne -1 and x2[0] eq -1 then begin
    ref_c = ref_1
    endif else begin
    ref_2i = ReNormalizeAzimuth180( ref_2 + 180 ) ; invert ref_2
    ref_c = (ref_1 + ref_2i) / 2
    endelse


END

; ============================================================================
  PRO ToAw_CalcStats, phasearr, x1, x2, ref_1, ref_2, ref_c, $ ; IN
                      sq_1, sq_2, stdev_c                      ; OUT  
; ============================================================================

    compile_opt strictarr,hidden

  ; calculate contributions to variance and standard deviation of 
  ; reference direction 1
  ; ----------------------------------------------------------------------
  if x1[0] eq -1 then sq_1 = -1.0 $
  else begin
      diff_1 = phasearr[x1] - ref_1

      x = where( diff_1 gt 180)
      if x[0] ne -1 then diff_1[x] = diff_1[x] - 180

      x = where( diff_1 lt -180)
      if x[0] ne -1 then diff_1[x] = diff_1[x] + 180

      sq_1 = diff_1^2           ; contributions to variance
      var_1 = total(sq_1) / n_elements(sq_1)
      stdev_1 = sqrt ( var_1 )
  endelse

  ; calculate contributions to variance and standard deviation of 
  ; reference direction 2
  ; ----------------------------------------------------------------------
  if x2[0] eq -1 then sq_2 = -1.0 $
  else begin
      diff_2 = phasearr[x2] - ref_2

      x = where( diff_2 gt 180)
      if x[0] ne -1 then diff_2[x] = diff_2[x] - 180

      x = where( diff_2 lt -180)
      if x[0] ne -1 then diff_2[x] = diff_2[x] + 180

      sq_2 = diff_2^2           ; contributions to variance
      var_2 = total(sq_2) / n_elements(sq_2)
      stdev_2 = sqrt ( var_2 )
  endelse

  ; calculate contributions to variance and standard deviation
  ; ----------------------------------------------------------
  phase_c = phasearr
  if x2[0] ne -1 then $
     phase_c[x2] = ReNormalizeAzimuth180( phasearr[x2] + 180 ) 

  diff_c = phase_c - ref_c
  x = where( diff_c gt 180)  & if x[0] ne -1 then diff_c[x] = diff_c[x] - 180
  x = where( diff_c lt -180) & if x[0] ne -1 then diff_c[x] = diff_c[x] + 180
  sq_c = diff_c^2               ; contributions to variance
  stdev_c = sqrt ( total(sq_c) / n_elements(sq_c) )



;  print, 'ToAw_CalcStats: stdevs are : '
;  print, 'ref 1 : ', ref_1, stdev_1
;  print, 'ref_2 : ', ref_2, stdev_2
;  print, 'ref_c : ', ref_c, stdev_c


END

; ============================================================================
  FUNCTION ToAw_RemoveOutliers, sq_1, sq_2, phasearr, $ ; IN
                                x1, x2 , $              ; IN/OUT
                                debug = debug
; ============================================================================

    compile_opt strictarr,hidden

  if keyword_set(debug) then debug = 1 else debug = 0

  fac = 3.0

  if sq_1[0] ge 0 then begin
      var_1 = total(sq_1) / n_elements(sq_1)
      x = where( sq_1  gt fac*var_1, out_1_cnt)
      if debug then $
        print, 'ref_1 : ' + strtrim(out_1_cnt,2) + ' outliers detected'
      x1i = where( sq_1 le fac*var_1 )
      N1i = n_elements(x1i)
  endif else begin
      out_1_cnt = 0
      N1i = 0
  endelse
      

  if sq_2[0] ge 0 then begin
      var_2 = total(sq_2) / n_elements(sq_2)
      x = where( sq_2  gt fac*var_2, out_2_cnt)
      if debug then $
        print, 'ref_2 : ' + strtrim(out_2_cnt,2) + ' outliers detected'
      x2i = where( sq_2 le fac*var_2 )
      N2i = n_elements(x2i)
  endif else begin
      out_2_cnt = 0
      N2i = 0
  endelse


  if out_1_cnt eq 0 and out_2_cnt eq 0 then begin
      if debug then print, 'No outliers.'
      return, 0
  endif else if N1i lt 2 or N2i lt 2 then begin
      if debug then begin
         print, 'less than 2 beams in one or both groups after ' + $
                'outlier removal.'
         print, 'Outlier removal not performed.' 
      endif
      return, 0
  endif else begin
      x1 = x1[ x1i ]
      x2 = x2[ x2i ]
      return, out_1_cnt + out_2_cnt
  endelse    
      

END


; ============================================================================
  PRO ToAw_GroupBeamDirections, $
                phasearr, $    ; IN
                x1, x2,   $    ; OUT
                ref_phase, $   ; OUT
                outlier_sum, $ ; OUT
                tof_regime, $  ; OUT
                remove_outliers=remove_outliers, $       ; remove outliers
                debug=debug    ; print debugging messages
; ============================================================================

    compile_opt strictarr,hidden

  if keyword_set(debug) then debug = 1 else debug = 0

  outlier_sum = 0
  tof_regime  = 0

  NN = n_elements(phasearr)

  inlier_arr = intarr(NN) + 1 ; all beams are inliers initially

  ; This loop will be executed more than once only if keyword remove_outliers
  ; has been set and if at least one outlier was subsequently identified 
  ; (and removed)
  ; -------------------------------------------------------------------------
  while 1 eq 1 do begin

      inlier_idx = where( inlier_arr eq 1 )
      if inlier_idx[0] eq -1 then begin
          message, 'no inliers left!!!'
      endif

      cur_phasearr = ReNormalizeAzimuth180( phasearr[inlier_idx] )

      ; Sort the beams into two groups
      ToAw_DetermineBeamGroups, cur_phasearr, $  ; IN
                                cur_x1, cur_x2   ; OUT

      ; calculate averaged reference directions for the two groups
      ; and also the common average direction
      ToAw_CalcRefDirections, cur_phasearr, cur_x1, cur_x2, $ ; IN
                              ref_1, ref_2, ref_c             ; OUT

      ; calculate some statistical parameters
      ToAw_CalcStats, cur_phasearr, cur_x1, cur_x2, $  ; IN
                      ref_1, ref_2, ref_c, $           ; IN
                      sq_1, sq_2, stdev_c              ; OUT
     
      if stdev_c lt 10. then tof_regime = 1 else tof_regime = 0


      if debug then begin
          if cur_x1[0] ne -1 then begin
              print, 'ref phase = ' + strtrim(ref_c,2)
              print, 'group 1 statistical output --------'
              var_1 = total(sq_1) / n_elements(sq_1)
              for i=0,n_elements(cur_x1)-1 do begin
                  print, format='(F7.1,"    ",F15.2)', $
                    cur_phasearr[cur_x1[i]], sq_1[i]
              endfor
              print, format='(F7.1,"    ",F15.2,"  <-- ref , var")', $
                     ref_1, var_1 
      
          endif
          if cur_x2[0] ne -1 then begin
              print, 'group 2 statistical output --------'
              var_2 = total(sq_2) / n_elements(sq_2)
              for i=0,n_elements(cur_x2)-1 do begin
                  print, format='(F7.1,"    ",F15.2)', $
                    cur_phasearr[cur_x2[i]], sq_2[i]
              endfor
              print, format='(F7.1,"    ",F15.2,"  <-- ref , var")', $
                     ref_2, var_2 
          endif
          print, 'stdev_c = ', stdev_c
          print, '_______________________'
      endif

          
      if not keyword_set(remove_outliers) then begin

          outlier_cnt = 0

      endif else begin

          outlier_cnt = $
                ToAw_RemoveOutliers(sq_1, sq_2, cur_phasearr, $ ; IN
                                    cur_x1, cur_x2,           $ ; IN/OUT
                                    debug=debug ) 
      endelse

      if cur_x1[0] eq -1 then x1 = -1L $ 
      else                    x1 = inlier_idx[cur_x1]

      if cur_x2[0] eq -1 then x2 = -1L $
      else                    x2 = inlier_idx[cur_x2]

      ; set up inlier array for next round of outlier removal
      inlier_arr = intarr(NN)   ; all beams cleared initially
      if x1[0] ne -1 then inlier_arr[ x1 ] = 1 ; set direction 1 inliers
      if x2[0] ne -1 then inlier_arr[ x2 ] = 1 ; set direction 2 inliers

      if outlier_cnt eq 0 then break ; done if not outliers found this time    

      outlier_sum = outlier_sum + outlier_cnt ; sum up total number of outliers

  endwhile

  print, 'ToAw_GroupBeamDirections'
  print, '   number of beams    : ', n_elements(phasearr)
  print, '   number in group 1  : ', n_elements(x1)
  print, '   number in group 2  : ', n_elements(x2)
  if n_elements(ref_1) gt 0 then $
    print, '   avg dir in group 1 : ', ref_1 $
  else $
    print, '   avg dir in group 1 : no ref_dir 1 beams'
  if n_elements(ref_2) gt 0 then $
    print, '   avg dir in group 2 : ', ref_2 $
  else $
    print, '   avg dir in group 1 : no ref_dir 2 beams'
  if n_elements(ref_c) gt 0 then $
    print, '   common avg dir     : ', ref_c $
  else $
    print, '   common avg dir     : not defined'

  ref_phase = ref_c
  
END
