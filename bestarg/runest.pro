; ============================================================================
; Project: Cluster-II EDI
; Author : Hans Vaith, MPE Garching
; Creation Date: 08-May-2001
; 
; RunEst: Estimation of runner order
;
; This function is intended to support the BESTTARG program by providing 
; information about the runner order of beam hits.
; The information about the runner order is obtained by looking at long 
; stretches of data, typically 1 hour. First, the gyro time Tg, obtained from 
; B data, is fitted to the ToF data. An offset for B is derived 
; in order to minimize the average deviation of ToF from the Tg curve. Second,
; probabilities are calculated for each ToF, for being a single- or 
; multi-runner, up to the order which is passed in keyword MAXORDER(default 6).
; For a description of the returned information see the return structure 
; definition in the code below.
; ============================================================================
; Id: runest.pro,v 1.20 2002/03/22 14:34:53 hav Exp hav 
; Log: runest.pro,v 
; Revision 1.20  2002/03/22 14:34:53  hav
; bug fix: see 1.19 + -1L vs [ -1L ] in B offset determination function !!!
;
; Revision 1.18  2002/03/03 17:02:26  hav
; added B data gap detection and handling (make EDI points within gaps 
; class C beams).
;
; Revision 1.17  2002/02/04 11:27:56  hav
; new keywords smooth and pl_reduce, more plotsets, rearrangement of
; input section
;
; Revision 1.16  2001/10/26  12:40:00  hav
; changed interface: q is passed (ppq); /useq1 (hav) determines if q=1 
; points are used
; Determination of B offset and sfac is done with q gt 2 points only
;
; Revision 1.15  2001/10/26 11:27:15  hav
; added support for runner order estimation of q=1 data
;
; Revision 1.14  2001/09/27  15:45:51  hav
; bug fix in format of error message
;
; Revision 1.13  2001/09/25 14:43:19  hav
; bug fix: position of RE_DetermineBoffset, call axlabel() only if plotset gt 0
;
; Revision 1.12  2001/09/21 12:16:23  hav
; sfac determined automatically, but can be overridden via keyword sfac=...
;
; Revision 1.11  2001/09/17  12:35:21  hav
; bug fix: call axlabel only if plotset gt 0
;
; Revision 1.10  2001/09/17 07:57:42  hav
; keyword RELAX allows specification of value for B time begin/end condition
;
; Revision 1.9  2001/09/17 07:51:30  hav
; changed back to default SFAC=0.25; new keyword RELAX for B timedition
; text may have been screwed up, so here it is again:
; changed back to default SFAC=0.25; new keyword RELAX for B time begin/end
; condition
;
; Revision 1.8  2001/09/14  15:11:39  hav
; changed probability width factor from 0.25 to 0.5; changeable via keyword SFAC
;
; Revision 1.7  2001/09/14  10:25:24  hav
; checkin prior to possibly major changes
;
; Revision 1.6  2001/08/07 11:32:32  hav
; added equivalent single runner times-of-flight to return structure
;
; Revision 1.5  2001/07/05 14:21:03  hav
; second pline replaced by interpol
;
; Revision 1.4  2001/07/05 13:54:51  hav
; spline -> linear interpolation of B
;
; Revision 1.3  2001/06/19  13:42:56  hav
; changed keyword st to stime, use bb_sr and st_sr rather than bs.avg...
;
; Revision 1.2  2001/05/17  14:32:12  hav
; only changes which affect plotting
;
; Revision 1.1  2001/05/15  16:15:26  hav
; Initial revision
;
; ============================================================================


; ============================================================================
  PRO CheckForGaps,  b_ct,  edi_ct,  x_no_gap, x_in_gap,  debug = debug
; ============================================================================
; This procedure checks for gaps in the magnetic field data and
; determines if any EDI data are located within these gaps. Two index
; arrays are being returned which allow to separate the EDI data into 
; groups of points which are outside of gaps and those which are
; inside the gaps and should not be used for multi-runner analysis 
;
; Parameters:
;   b_ct      IN     array of B data times (ct = seconds since 1-Jan-1970)
;   edi_ct    IN     array of EDI data times ("beam times")
;   x_no_gap  OUT    array of indices for EDI data outside of gaps
;   x_in_gap  OUT    array of indices for EDI data inside of gaps
;
; side effects: none (i.e. b_ct and edi_ct are not being modified)
; ****************************************************************************

  if not keyword_set(debug) then debug = 0 else debug = 1

  nm_spacing =  0.04466d           ; spacing of FGM samples in NM1
  max_spacing =  3 * nm_spacing    ; define max allowed spacing
  NB   =  n_elements(b_ct)         ; number of B data points
  NEDI =  n_elements(edi_ct)       ; number of EDI data points

  actual_spacing =  b_ct(1:NB-1) - b_ct(0:NB-2)
  avg_spacing    =  total(actual_spacing) / (NB-1)
  x_gap =  where( actual_spacing gt max_spacing, gap_cnt)

  if debug ne 0 then begin
     print, 'avg B data spacing    : ', avg_spacing
     print, 'number of gaps : ', gap_cnt
  endif

  x_in_gap =      -1L ; initialize array which holds the indices of EDI
                      ; points which will not be included in the runner
                      ; order analysis because they fall into a gap of
                      ; the B data

  x_no_gap =  lindgen(NEDI)

  if gap_cnt gt 0 then begin

     x_no_gap = -1L

     if debug ne 0 then begin   
        print, 'smallest gap   : ', min(actual_spacing(x_gap))
        print, 'largest gap    : ', max(actual_spacing(x_gap))
        print, 'avg gap width  : ', total(actual_spacing(x_gap)) / gap_cnt
     endif


     ; loop through gaps and see if there
     ; are any EDI points located inside
     ; -----------------------------------
     for i = 0, gap_cnt-1 do begin
        
        ct_left = b_ct(x_gap(i))     ; left border of gap
        ct_right = b_ct(x_gap(i)+1)  ; right border of gap

        ; for first round through loop set the right border of 
        ; the previous (i.e. nonexisting) gap to some arbitrary value
        ; lower than edi_ct(0)
        if i eq 0 then ct_right_prev = edi_ct(0)-1.0 $
        else           ct_right_prev = b_ct(x_gap(i-1)+1)

        if debug ne 0 then begin
           print, clutimeval2str(ct_left)
           print, clutimeval2str(ct_right)
        endif   

        x_outside = where( edi_ct ge ct_right_prev and edi_ct le ct_left, $
                           cnt_outside)         
        x_inside =  where( edi_ct gt ct_left and edi_ct lt ct_right, $
                           cnt_inside)
        if cnt_outside gt 0 then x_no_gap = [ x_no_gap, x_outside ]
        if cnt_inside  gt 0 then x_in_gap = [ x_in_gap, x_inside ]

        if debug ne 0 then begin
           print, '   number of EDI points within gap ' + $
                  strtrim(i, 2) + ' : ',  cnt_inside
           for j = 0, cnt_inside-1 do begin
              print, '   ', clutimeval2str(edi_ct(x_inside(j)))
           endfor
        endif
     endfor

     ; finally add points right of last gap to x_no_gap
     x_outside = where( edi_ct ge ct_right, cnt_outside )
     if cnt_outside gt 0 then x_no_gap = [ x_no_gap, x_outside ]

     N_IN_GAP =  n_elements(x_in_gap)
     if N_IN_GAP gt 1 then x_in_gap = x_in_gap(1:N_IN_GAP-1)
     
     N_NO_GAP = n_elements(x_no_gap)
     if N_NO_GAP gt 1 then x_no_gap = x_no_gap(1:N_NO_GAP-1)

  endif

END

; ===========================================================================
  FUNCTION RE_DetermineBoffset, rtof, tgi, width, debug=debug
; ===========================================================================
; Parameters:
;    rtof     dealiased time of flight
;    tgi      gyro time as calculated from initial bmag, interpolated to 
;             times of EDI hits
;    width    half width of acceptance window around tgyro
;    
; Try to find offset for bmag data
;   - calculate mean difference between tgi and rtof and use this difference
;     to correct tgi -> tgi1
;   - now consider only points closer to tgi1 than +/- width 
;     correct tgi1 again using only these points : --> tgi2
;   - use the total correction tgi -> tgi2 to calculate correction for bmag
; =========================================================================

@constants.inc

ret = { status:0, msg:'Ok', db:0.0 } 

NN =  n_elements(rtof)

if NN lt 1 then begin
   ret.status =  1
   ret.msg = 'No points of quality>=2 available for B offset determination.'
   return, ret
endif

; ---------------------
; find first correction
; ---------------------
delta1 = total(rtof-tgi) / NN
tgi1   = tgi+delta1
if debug gt 0 then print, '<rtof - tgi>                  : ', delta1, ' us'

; ---------------------------------------------------------------------
; now select only points close to tgi1 and calculate another correction
; ---------------------------------------------------------------------
xclose = where( abs(rtof-tgi1) lt width, NCLOSE )
if NCLOSE eq 0 then begin
   ret.status = 1
   ret.msg = 'Error: no points close to Tgyro found. Giving up!'
   return, ret
endif

delta2 = total(rtof(xclose)-tgi1(xclose)) / NCLOSE
if debug gt 0 then print, '<rtof(xclose) - tgi1(xclose)> : ', delta2, ' us'

delta = delta1+delta2 ; combined correction

; -----------------------------------
; use this delta to find B correction
; -----------------------------------

avg_tof = total(rtof(xclose))/NCLOSE
ret.db  = -BTG_CONV/avg_tof^2 * delta
print, 'delta1, NCLOSE, delta2, ret.db', delta1, NCLOSE, delta2, ret.db
return, ret

END


; ============================================================================
  FUNCTION RunEst, $
           d, bs, $     ; WW data and B data (hav passing mechanism)
           tof=tof, $      ; WW data and B data (ppq passing mechanism)
           stime=ct, $
           ctype=ctype, $
           nc = nc, mc = mc, $
           gdu = gdu, q = q, $
           st_sr=st_sr, bb_sr=bb_sr,  $
           MAXORDER=MAXORDER, $ ; configuration keywords
           CONF_LIM=CONF_LIM, $
           P_LIM=P_LIM, $
           SFAC=SFAC, $
           RELAX=RELAX, $
           smooth = smooth, $
           plotset=plotset, yr=yr, xr=xr, $ ; plotting specs
           srb=srb, $     ; used only with (d,bs) passing mechanism
           useq1=useq1, $ ; ditto
           pl_reduce = pl_reduce,  $
           debug = debug, $    ; print debugging messages
           usedb = usedb, $
           db = db, $          ; use this delta B instead of determining it !
           nop=nop, $
           dots=dots           ; plot multi-runner lines as dots
; ============================================================================
; parameters:
;    d   : WW data structure as returned by GetWWData() in wwconv2.pro
;    bs  : a structure containing a two arrays
;          hr:  { ct, bx, by, bz, bb } high res B data
;          avg: { ct, bx, by, bz, bb } spin res B data 
;          The structure bs is the result of function
;          GetMagData() in module WWCONV.PRO
;          
; replace these parameters in the function call as needed and
; make the appropriate changes in the input section below
; ============================================================================

forward_function axlabel
@constants.inc

; -----------------------
; first evaluate keywords
; -----------------------
if not keyword_set(MAXORDER) then MAXORDER=6
if not keyword_set(CONF_LIM) then CONF_LIM=20.0d
if not keyword_set(P_LIM) then P_LIM=exp(-2.0) ; gt p(2*sigma)
if not keyword_set(smooth) then smooth = 1

if not keyword_set(plotset) then plotset=0
if not keyword_set(yr) then yr = [0,0]
if not keyword_set(srb) then srb = 0
if not keyword_set(debug) then debug=0
if not keyword_set(RELAX) then relax = 4.0 
if not keyword_set(XR) then xr=[0,1]
if not keyword_set(useq1) then useq1 = 0
if not keyword_set(pl_reduce) then pl_reduce = 1
if not keyword_set(usedb) then usedb = 0
if not keyword_set(nop) then nop=0
if not keyword_set(dots) then dots=0

if keyword_set(tof) then hav_call=0 else hav_call=1


; ============================================================================
; THIS IS THE INPUT SECTION
; =============================================================================

; all of the following are arrays with as many elements as there 
; are PACMO1/5 structures
; --------------------------------------------------------------
; Note, that for data prior to 27 February, m is not part of the 
; PACMO1/5 structures, but the code below requires (or better assumes)
; m to be available at this time resolution. To solve this problem
; when working with the pick library take the (raw) m from the second 
; header (word 1, bits 4,5), convert it ( [0,1,2,3] -> [16,8,4,2] ) and 
; create an m array with PACMO1/5 resolution   
; --------------------------------------------------------------

if not hav_call then begin    ; This is the initialization branch
                              ; for the ppq passing mechanism.
                              ; Derive the size of the arrays from
                              ; tof

   if debug gt 0 then print, 'ppq passing mechanism.' 

   N_ALL = n_elements(tof)

   ph = fltarr(N_ALL)  ; define dummies for data which are 
   th = fltarr(N_ALL)  ; available only in case of the d,bs 
   en = intarr(N_ALL)  ; (hav) passing mechanism

   bd_bmag = bb_sr ; get B magnitude and associated time tags
   bd_ct   = st_sr

   sc_id =  0      ; dummy 

endif else begin              ; This is the initialization branch 
                              ; if parameters have been passed via 
                              ; d,bs (hav passing mechanism)


   sc_id = d.hdr.sc_id
   dd = d.data

   if debug gt 0 then print, 'hav passing mechanism.' 

   ; select data according to quality
   ; --------------------------------
   if useq1 eq 0 then begin
      myq1  = where(dd.q1 ge 2 and dd.mch1 eq 7 and dd.ovfl_t1 eq 0, cnt1)
      myq2  = where(dd.q2 ge 2 and dd.mch2 eq 7 and dd.ovfl_t2 eq 0, cnt2)
   endif else begin
      myq1  = where( (dd.q1 eq 1 or (dd.q1 ge 2 and dd.mch1 eq 7)) and $
                     dd.ovfl_t1 eq 0, cnt1)
      myq2  = where( (dd.q2 eq 1 or (dd.q2 ge 2 and dd.mch2 eq 7)) and $
                     dd.ovfl_t2 eq 0, cnt2)
   endelse
   N_ALL = cnt1+cnt2


   if N_ALL eq 0 then $
      return, { status:1,  msg:'Error: no data of selected quality!' }

   if myq1(0) ne -1 then begin
      tof   = dd(myq1).tof1       ; time of flight in micro-s
      ct    = dd(myq1).ct1        ; beamtime in seconds since 1.1.1970
      ctype = dd(myq1).ctype      ; correlator code type (0=short)
      nc    = dd(myq1).nc         ; correlator n (1,2,4,8,16,32,64)
      mc    = dd(myq1).mc         ; correlator m (16,8,4,2 )  
      gdu   = intarr(n_elements(myq1)) + 1
      q     = dd(myq1).q1
      ph    = 360. - dd(myq1).ph2 ; transform into GDU1 system
      th    = 180. - dd(myq1).th2 ; transform into GDU1 system
      en    = dd(myq1).en         ; energy flag
   endif

   if myq2(0) ne -1 then begin
      if myq1(0) ne -1 then begin
         tof   = [tof,   dd(myq2).tof2]  ; time of flight in micro-s
         ct    = [ct,    dd(myq2).ct2]   ; beamtime in seconds since 1.1.1970
         ctype = [ctype, dd(myq2).ctype] ; correlator code type (0=short)
         nc    = [nc,    dd(myq2).nc]    ; correlator n (1,2,4,8,16,32,64)
         mc    = [mc,    dd(myq2).mc]    ; correlator m (16,8,4,2)
         gdu   = [gdu,   intarr(n_elements(myq2)) + 2]
         q     = [q,     dd(myq2).q2]
         ph    = [ph,    dd(myq2).ph1]
         th    = [th,    dd(myq2).th1]
         en    = [en,    dd(myq2).en]    ; energy flag
      endif else begin
         tof   = dd(myq2).tof2  ; time of flight in micro-s
         ct    = dd(myq2).ct2   ; beamtime in seconds since 1.1.1970
         ctype = dd(myq2).ctype ; correlator code type (0=short)
         nc    = dd(myq2).nc    ; correlator n (1,2,4,8,16,32,64)
         mc    = dd(myq2).mc    ; correlator m (16,8,4,2)
         gdu   = intarr(n_elements(myq2)) + 2
         q     = dd(myq2).q2
         ph    = dd(myq2).ph1
         th    = dd(myq2).th1
         en    = dd(myq2).en    ; energy flag
      endelse
   endif

   ; these are Bmag data spanning the same time interval
   ; (or a little more) as the EDI PACMO1/5 data above 
   ; ----------------------------------------------------------------------
   if srb ne 0 then begin
      bd_bmag = bs.avg.bb ; spin-averaged Bmag data
      bd_ct   = bs.avg.ct ; associated spin-center times
   endif else begin
      bd_bmag = bs.hr.bb ; high resolution Bmag data
      bd_ct   = bs.hr.ct ; associated times
   endelse

endelse

; save, /VARIABLES, filename='bestarg_mms_C3_2001-06-08t053000_054000_@20150306_runest_entry_variables.sav'
; restore, filename='bestarg_mms_C3_2001-06-08t053000_054000_@20150306_runest_entry_variables.sav'
; stop
; print, bd_bmag(0), bd_ct(0), 15.0*0.119209289551*mc(0)*nc(0), tof(0)
; 241.255 nT 9.9197634e+008      114.441 us     147.104 us                    gyroPeriod = 148 us
; correlator window possibly 61.0352-167.847 us, 
; but I don't understand the ~30 us offset... is Ch 7 delayed? If so, where is delay reported?
; print, bd_bmag(0:3), bd_ct(0:3), mc(0:3), nc(0:3), 15.0*0.119209289551*mc(0:3)*nc(0:3), tof(0:3)
;       241.255      241.256      241.295      241.261
;   9.9197634e+008  9.9197634e+008  9.9197634e+008  9.9197634e+008
;           16          16          16          16
;        4       4       4       4
;       114.441      114.441      114.441      114.441   <<< these are consistent w chipPeriod = 7.62939
;       147.104      146.150      145.912      145.912

; smoothing of magnetic field data
; --------------------------------
if smooth gt 1 then bd_bmag = smooth(bd_bmag, smooth)


; ============================================================================
; END OF INPUT SECTION
; do not change anything below this line (unless you know what you're doing)
; ============================================================================

; -------------------------------------------------------------------------
; return structure definition
; ---------------------------
  data = { RE_DATA,  $
           beamtime:0.0d, $           ; seconds since 1.1.1970
           gdu:0, $                   ; 1 or 2
           runorder:0, $              ; 1..MAXORDER, 0 means check flags
           flag:intarr(MAXORDER), $   ; 1 if runner probability gt P_LIM
           prob:dblarr(MAXORDER), $   ; runner probabilities
           estof:fltarr(MAXORDER), $  ; equivalent single runner ToFs
           sortindex:0L, $
           tg:0.0, $                  ; gyro time
           tof:0.0, $                 ; time of flight
           q:0, $                     ; quality
              ph:0.0, $ ; phi     these three contain meaningful data
              th:0.0, $ ; theta   only when using the d,bs (hav) 
              en:0 $    ; energy  passing mechanism
         }           
           
  data = replicate(data, N_ALL)

  ret = { data:data, $ ; runner data array
          status:0, $  ; return status: 0=ok, other = error
          msg:'', $    ; return message (error details)
          sfac:0.0,$
          db:0.0, $
          sc_id:0, $
          smooth:0 }
; --------------------------------------------------------------------------


; sort in time
; ------------
sortindex = sort(ct)

tof_all   =   tof(sortindex)
ct_all    =    ct(sortindex)
ctype_all = ctype(sortindex)
nc_all    =    nc(sortindex)
mc_all    =    mc(sortindex)
gdu_all   =   gdu(sortindex)
th_all    =    th(sortindex)
ph_all    =    ph(sortindex)
q_all     =     q(sortindex)
en_all    =    en(sortindex)

; check number of data points
; ---------------------------
if debug gt 0 then print, 'number of EDI data points: ', N_ALL
if N_ALL lt 2 then begin
   ret.status = 1
   ret.msg = 'Error: less than two data points of selected quality!'
   return, ret
endif

; check the B data for gaps
; =========================
CheckForGaps, bd_ct, ct_all, x_no_gap, x_in_gap,  debug = debug
if debug ne 0 then begin
   help, ct_all, x_no_gap, x_in_gap
   for i=0L,n_elements(x_in_gap)-1 do print, x_in_gap(i)
   print,  'checking integrity & listing EDI points in gaps'
   if x_in_gap(0) eq -1 then begin
      print,  '   nothing to do!'
   endif else begin
      for i = 0, n_elements(x_in_gap)-1 do begin
         x = where(x_no_gap eq x_in_gap(i),  cnt)
         print, '  index ',  x_in_gap(i), ' : ',  cnt, '   ', $
                clutimeval2str(ct_all(x_in_gap(i)))
      endfor
   endelse
endif

; return if there are no EDI data outside of gaps
; -----------------------------------------------
if x_no_gap(0) eq -1 then begin
   ret.status = 1
   ret.msg = 'RunEst B-gap detection : no EDI data outside of gaps'
   return, ret
endif


tof   =   tof_all(x_no_gap)
ct    =    ct_all(x_no_gap)
ctype = ctype_all(x_no_gap)
nc    =    nc_all(x_no_gap)
mc    =    mc_all(x_no_gap)
gdu   =   gdu_all(x_no_gap)
th    =    th_all(x_no_gap)
ph    =    ph_all(x_no_gap)
q     =     q_all(x_no_gap)
en    =    en_all(x_no_gap)

N_NO_GAP =  n_elements(x_no_gap)


; make sure that B data close enough to start and end are available
; (No longer to be done, since gap detection is inplemented)
; -----------------------------------------------------------------
;NB = n_elements(bd_ct)
;if bd_ct(0) gt (ct(0)+relax) or bd_ct(NB-1) lt (ct(NN-1)-relax) then begin
;   ret.status = 1
;   ret.msg = 'Error: B data must enclose EDI data'
;
;   print, format='("EDI data start/end        : ", f17.6, " / ", f17.6)', $
;          ct(0), ct(NN-1)
;   print, format='("relative B data start/end : ", f17.6, " / ", f17.6)', $
;          bd_ct(0)-ct(0), bd_ct(NB-1)-ct(NN-1)
;
;   return, ret
;endif  

; redefine time as seconds since first beam time
; ----------------------------------------------
ta = ct - ct(0)

; calculate tchip and tcode (assuming short code: 15 chips)
; ---------------------------------------------------------
nchips = intarr(N_NO_GAP) + 15 ; number of chips in Pseudo Noise Code
x = where(ctype eq 1) ; see where long code has been used
if x(0) ne -1 then nchips(x) = 127 ; set number of chips for long code
tchip = 2.0d^(-24)*2*mc*nc * 1.0d6 ; chip length in micro-s
tcode = nchips*tchip               ; code length in micro-s

; interpolate tg_sr to q=3 data times : --> tgi
; ---------------------------------------------
if debug gt 0 then print, 'Interpolating Tg data to times of EDI hits...'
;tgi = spline(bd_ct-ct(0), BTG_CONV/bd_bmag, ta)
tgi = interpol(BTG_CONV/bd_bmag, bd_ct-ct(0), ta)

rtof = tof ; copy tof in order not to overwrite original data
f = 1.0    ; use only points closer to tgi than f*tchip 

; move data into tube +/- 0.5 tCode around gyro time (dealiasing)
; ---------------------------------------------------------------
if debug gt 0 then print, 'Moving data into tube +/- 0.5*tcode around tgi'
n = floor( (tgi-rtof)/tCode + 0.5 )
rtof = rtof + n*tCode

; Determine B offset
; ------------------
q_ge2 = where( q ge 2, Q_GE2_NN)
if Q_GE2_NN eq 0 then begin
   ret.status = 1
   ret.msg = 'Zero data points of q>=2 for B offset determination.'
   goto, undo_mods
   ;return,  ret
endif

if usedb eq 0 then begin

   dbs = RE_DetermineBoffset(rtof(q_ge2), tgi(q_ge2), $
                             f*tchip(q_ge2), debug=debug)
   if dbs.status ne 0 then begin
      ret.status = dbs.status
      ret.msg = dbs.msg
      goto, undo_mods
      ; return, ret
   endif
   if debug gt 0 then print, 'derived B offset : ', dbs.db, ' nT'
   db = dbs.db
endif else begin
   print,'using passed B offset : ' , db
endelse

; recalculate Tgyro with new B offset
; -----------------------------------
;tgi3 = spline(bd_ct-ct(0), BTG_CONV/(bd_bmag+db), ta)
tgi3 = interpol(BTG_CONV/(bd_bmag+db), bd_ct-ct(0), ta)

; test
; ----
if usedb eq 0 and debug gt 0 then $
   dummy = RE_DetermineBoffset(rtof(q_ge2), tgi3(q_ge2), $
                               f*tchip(q_ge2), debug=debug)

; now move data into tube around tgi3
; -----------------------------------
if debug gt 0 then print, 'Moving data into tube +/- 0.5*tcode around tgi3'
n = floor( (tgi3-rtof)/tCode  +  0.5 )
rtof_tgi = rtof ; tracking code - disable
rtof = rtof + n*tCode

; calculate standard deviation of tof around tgi3
; -----------------------------------------------
xclose = where( abs(rtof(q_ge2)-tgi3(q_ge2)) lt f*tchip(q_ge2) , NCLOSE )
if NCLOSE gt 0 then begin
   stdev = sqrt( total((rtof(q_ge2(xclose))-tgi3(q_ge2(xclose)))^2) / NCLOSE )
   avg_tchip = total(tchip(q_ge2(xclose))) / NCLOSE
   if debug gt 0 then begin
      print, 'analysis of q>=2 data points:'
      print, 'standard deviation (us) : ', stdev
      print, 'average chip width (us) : ', avg_tchip
      print, 'ratio stdev/tchip       : ', stdev / avg_tchip
   endif
endif else begin
   ret.status = 1
   ret.msg = 'No q>=2 points close to tgi3 found. Giving up!'
   goto, undo_mods
   ; return, ret
endelse

if not keyword_set(SFAC) then begin
   sfac = stdev / avg_tchip
   if debug gt 0 then print, 'using sfac of ', sfac
endif


; ======================================================================
; Calculate distance of tof from n * tgyro (multiples of tgi3)
; ======================================================================
dist = fltarr(MAXORDER, N_NO_GAP)
for i=0,MAXORDER-1 do begin
   dist(i,*) = (rtof - (i+1)*tgi3) mod tCode
   x = where(dist(i,*) gt 0.5*tCode)
   if x(0) ne -1 then dist(i,x) = dist(i,x) - tCode(x)
   x = where(dist(i,*) lt -0.5*tCode)
   if x(0) ne -1 then dist(i,x) = dist(i,x) + tCode(x)
endfor
; save, /VARIABLES, filename='bestarg_mms_C3_2001-06-08t053000_054000_@20150306_runest_L671_variables.sav'
; restore, filename='bestarg_mms_C3_2001-06-08t053000_054000_@20150306_runest_entry_variables.sav'
; stop

; ======================================================================
; assign order assuming gaussian probability distributions
; and use p_max^2/sum(others) as a confidence criterion
; ======================================================================
stdev = SFAC*tchip ; this is the stdev of the gaussian distributions

p_run = fltarr(MAXORDER, N_NO_GAP) ; define some arrays
p_tot = fltarr(N_NO_GAP)
p_max = p_tot
order = intarr(N_NO_GAP)

; calculate probabilities
; -----------------------
for i=0,MAXORDER-1 do begin
   p_run(i,*) = exp(-0.5*(dist(i,*)/stdev)^2)

   x = where(abs(dist(i,*)) gt tchip)  ; is this reasonable to do ?
   if x(0) ne -1 then p_run(i,x) = 0.0

   p_tot = p_tot + p_run(i,*)
   x = where(p_run(i,*) gt p_max)
   if x(0) ne -1 then begin
      p_max(x) = p_run(i,x)
      order(x) = i+1
   endif
endfor 

x = where(p_max lt P_LIM)    ; set order to 'unknown' if p is below threshold 
if x(0) ne -1 then order(x) = 0
; save, /VARIABLES, filename='bestarg_mms_C3_2001-06-08t053000_054000_@20150306_runest_L704_variables.sav'
; restore, filename='bestarg_mms_C3_2001-06-08t053000_054000_@20150306_runest_entry_variables.sav'
; stop

; --------------------------------------------------------
; now calculate a confidence level for the runner order which 
; has the highest probability (some sort of SNR^2)
; set runner order to 'undetermined' (0) where the confidence level is 
; below the threshold
; --------------------------------------------------------
p_other = p_tot - p_max
x = where(p_other lt 1.d-10)
if x(0) ne -1 then p_other(x) = 1.0d-10 ; avoid numerical problems
conf = p_max^2 / p_other ; confidence level

x = where(conf lt CONF_LIM)  ; set order to 'unknown' if c is below threshold
if x(0) ne -1 then order(x) = 0

; -------------------------------------------------------------------------
; Here is what the function returns in case everything is ok
; -------------------------------------------------------------------------

  ret.status = 0 ; Ok
  ret.msg    = 'OK'
  ret.sc_id  = sc_id
  ret.sfac   = sfac
  ret.db     = db
  ret.smooth = smooth

  ret.data.beamtime = ct_all
  ret.data.gdu      = gdu_all
  ret.data.q        = q_all
  ret.data.en       = en_all
  ret.data.ph       = ph_all
  ret.data.th       = th_all
  ret.data.sortindex = sortindex

  ret.data(x_no_gap).tg  = tgi3
  ret.data(x_no_gap).tof = rtof

  ret.data(x_no_gap).runorder = order
  for i=0,MAXORDER-1 do begin
     x = where(p_run(i,*) ge P_LIM)
     if x(0) ne -1 then ret.data(x_no_gap(x)).flag(i) = 1
     tmp = dblarr(N_NO_GAP)
     tmp(*) = p_run(i,*)
     ret.data(x_no_gap).prob(i) = tmp
     esr_ret =  EquivSrToF(rtof, i+1, tgi3, tcode)
     if esr_ret.status ne 0 then begin
        ret.status = esr_ret.status
        ret.msg    = esr_ret.msg
        goto, undo_mods
        ;return, ret
     endif
     ret.data(x_no_gap).estof(i) = esr_ret.estof
  endfor

  ; fill in the data for points in gaps (make them class C beams)
  ; -------------------------------------------------------------

;  print, 'x_in_gap : ', x_in_gap

  if x_in_gap(0) ne -1 then begin
     ret.data(x_in_gap).runorder = 0
     ret.data(x_in_gap).flag(*) = 0
     ret.data(x_in_gap).prob(*) = 0.0
     ret.data(x_in_gap).tg  = -1.0
     ret.data(x_in_gap).tof = -1.0
     ret.data(x_in_gap).estof(*) = -1.0
  endif

; --------------------------------------------------------------------------



; ==================================================================
; Plotting starts here
; ==================================================================

if plotset gt 0 then begin
   loadct, 39
   !x.style=1
   !y.style=1
   !p.psym=0
   @ct39.inc

   if nop eq 0 then begin
      !p.multi=[0,0,3,0,0]
      !p.charsize=2.0
   endif else begin
      !p.multi=[0,0,1,0,0]
      !p.charsize=1.0
   endelse

   dt = ct(n_elements(ct)-1) - ct(0)
   xrange = dt * xr + ct(0)
   if debug gt 0 then begin
      print, CluTimeVal2Str(xrange(0))
      print, CluTimeVal2Str(xrange(1))
   endif
   dummy=where(ct gt xrange(0) and ct lt xrange(1),npts)
   ;print, 'number of points to plot: ', npts
   ax = axlabel( xrange(0), xrange(1) )
   if ax.status ne 0 then begin
      print, 'axlabel() error:'
      print, ax.msg
      ret.status = 1
      ret.msg = ax.msg
      goto, undo_mods
      ;return, ret
   endif

   tax = ct - ax.ct0 ; seconds since beginning of day

   ; define colors for multirunners
   ; ------------------------------
   col = intarr(MAXORDER)
   for i=0,MAXORDER-1 do begin
      col(i) = 250 - 1.0*i/(MAXORDER-1) * (250-65)
   endfor
endif

; ---------------------------------------------------------------------------
; Plotset 1 : raw ToF values, GDUs distinguished by plot symbol, 
;             qualities distinguished by colors
;             (violet=0,lightblue=1,yellow=2,red=3)
;             out-of-range points are plotted in orange and 3 x larger 
; ---------------------------------------------------------------------------
if plotset eq 1 then begin
   if not hav_call then $
      print, 'plotset 1 not available with ppq passing mechanism' $
   else begin
      print, 'Plotset 1: uncorrected ToF, all qualities, color coded'

      if keyword_set(yr) then begin
          yr_min = yr(0)
          yr_max = yr(1)
      endif else begin
          yr_min = min([dd.tof1,dd.tof2])
          yr_max = max([dd.tof1,dd.tof2])
      endelse
      plot, /nodata, tax, rtof, yrange=[yr_min,yr_max], $
            ytitle='Raw ToF'

      qcol = [ violet, lightblue, yellow, red ]
 
      for i=0,3 do begin
         x = where(dd.q1 eq i and dd.ovfl_t1 eq 0)
         if x(0) ne -1 then $
            oplot, dd(x).ct1-ax.ct0, dd(x).tof1, $
                   symsize=0.7, psym=4, color=qcol(i)
      endfor
      xf = where(dd.ovfl_t1 eq 1)
      if xf(0) ne -1 then $
         oplot, dd(xf).ct1-ax.ct0, dd(xf).tof1, $
                symsize=2.0, psym=4, color=darkblue


      for i=0,3 do begin
         x = where(dd.q2 eq i and dd.ovfl_t2 eq 0)
         if x(0) ne -1 then $
            oplot, dd(x).ct2-ax.ct0, dd(x).tof2, $
                   symsize=0.7, psym=2, color=qcol(i)
      endfor
      xf = where(dd.ovfl_t2 eq 1)
      if xf(0) ne -1 then $
         oplot, dd(xf).ct2-ax.ct0, dd(xf).tof2, $
                symsize=2.0, psym=2, color=darkblue

   endelse
endif

if plotset eq 2 then begin
   if not hav_call then $
      print, 'plotset 2 not available with ppq passing mechanism' $
   else begin
      print, 'plotset 2 not yet implemented'
   endelse
endif

if plotset ge 3 then begin
   print, 'Plotset >= 3: dealiased ToF, quality >= 2'

   plot, /nodata, tax, rtof, yrange=yr, ytitle='ToF1,2 [usec]'

   x = where(gdu eq 1)
   if x(0) ne -1 then oplot, tax(x), rtof(x), psym=4, symsize=0.6, $
                             nsum = pl_reduce
   x = where(gdu eq 2)
   if x(0) ne -1 then oplot, tax(x), rtof(x), psym=2, symsize=0.6, $
                             nsum = pl_reduce
endif

if plotset eq 3 then begin
   oplot, tax, tgi3, color=250, nsum = pl_reduce
endif

if plotset eq 4 then begin
   print, 'Plotset 4 : raw + fitted Tgyro, +/- tChip, +/-tCode'

   oplot, tax, tgi, color=65
   oplot, tax, tgi3, color=250, nsum = pl_reduce

   oplot, tax, tgi3 + tchip, color=190, nsum = pl_reduce
   oplot, tax, tgi3 - tchip, color=190, nsum = pl_reduce

   oplot, tax, tgi3 + tcode, color=220, nsum = pl_reduce
   oplot, tax, tgi3 - tcode, color=220, nsum = pl_reduce

endif
; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------
; Plotset 5 : plot single and multi runner curves over data points
; ---------------------------------------------------------------------------
if plotset eq 5 then begin
   print, 'Plotset 5: nominal single + multirunner lines'
   for i=0,MAXORDER-1 do begin
      MultiRunnerPlot, tax, tgi3, i+1, tCode, col(i),  $
                       dots=dots, pl_reduce = pl_reduce
   endfor
endif
; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------
; Plotset 6 : show to which runner order each data point has been assigned
; ---------------------------------------------------------------------------
if plotset eq 6 then begin
   print, 'Plotset 6: actually assigned single and multi-runners'

   !p.symsize=0.6

   for i=0,MAXORDER-1 do begin
      x = where(order eq (i+1) and gdu eq 1)
      if x(0) ne -1 then oplot, tax(x), rtof(x), color=col(i), psym=4, $
                                nsum = pl_reduce
      x = where(order eq (i+1) and gdu eq 2)
      if x(0) ne -1 then oplot, tax(x), rtof(x), color=col(i), psym=2, $
                                nsum = pl_reduce

   endfor

   for i=0,MAXORDER-1 do begin
      MultiRunnerPlot, tax, tgi3, i+1, tCode, col(i),  $
                       dots=dots, pl_reduce = pl_reduce
   endfor


   !p.symsize=0.0
endif



; ---------------------------------------------------------------------------
; Plotset 7 : show the equivalent single runner times of flight
; ---------------------------------------------------------------------------
if plotset eq 7 then begin
   print, 'Plotset 7: equivalent single runner times of flight'

   !p.symsize=0.6

   oplot, tax, tgi3, color=250, nsum = pl_reduce

   for i=0,MAXORDER-1 do begin

      x = where(order eq (i+1) and gdu eq 1)
      if x(0) ne -1 then $
         oplot, [tax(x)], [ret.data(x).estof(i)], color=col(i), psym=4, $
                nsum = pl_reduce

      x = where(order eq (i+1) and gdu eq 2)
      if x(0) ne -1 then $
         oplot, [tax(x)], [ret.data(x).estof(i)], color=col(i), psym=2, $
                nsum = pl_reduce

   endfor

   !p.symsize=0.0

endif

; ---------------------------------------------------------------
if plotset gt 0 and nop eq 0 then begin 

   ; plot runner probabilities
   ; -------------------------
   plot_io, /nodata, tax, p_run(0,*), yrange=[1e-10, 2e0], $
            ytitle='runner probability'

   for i=0,MAXORDER-1 do begin
      oplot, tax, p_run(i,*), psym=-4, color=col(i),  nsum = pl_reduce
   endfor

   oplot, tax, intarr(n_elements(tax)) + P_LIM, linestyle=2,  $
          nsum = pl_reduce ; dashed

   ; plot confidence level
   ; ---------------------
   x = where(conf gt 1e4*CONF_LIM)
   if x(0) ne -1 then conf(x) = 1e4*CONF_LIM

   x = where(conf lt 1e-4*CONF_LIM)
   if x(0) ne -1 then conf(x) = 1e-4*CONF_LIM

   clim = intarr(n_elements(tax)) + CONF_LIM
   plot, tax, alog10(conf), yrange=[-3,6], ytitle='Log(confidence)', $
         nsum = pl_reduce
   oplot, tax, alog10(clim), linestyle=2, nsum = pl_reduce ; dashed

   for i=0,MAXORDER-1 do begin
      x = where(order eq (i+1))
      if x(0) ne -1 then oplot, tax(x), alog10(conf(x)), psym=4, $
                                color=col(i), nsum = pl_reduce
   endfor

   dummy=axlabel(/reset)
endif


; ---------------------------------------------------------------

  if debug gt 0 then begin
     ntotal = n_elements(ret.data)
     x = where(ret.data.gdu eq 1, cnt) & ngdu1 = cnt 
     x = where(ret.data.gdu eq 2, cnt) & ngdu2 = cnt 
     print, format='("data points      total/det1/det2 : ",' + $
                   'I6, "           ",' + $
                   'I6, "           ",' + $
                   'I6)', ntotal, ngdu1, ngdu2
     x = where(ret.data.runorder gt 0, acnt)
     x = where(ret.data.runorder gt 0 and ret.data.gdu eq 1, acnt1) 
     x = where(ret.data.runorder gt 0 and ret.data.gdu eq 2, acnt2) 
     print, format='("assignment total/det1/det2 : ",' + $
                   'I6, " (", (F5.1), "%)  ",' + $
                   'I6, " (", F5.1, "%)  ",' + $
                   'I6, " (", F5.1, "%)")', $
                    acnt, 100.0*acnt/ntotal, $
                    acnt1, 100.0*acnt1/ngdu1, $
                    acnt2, 100.0*acnt2/ngdu2
     for i=0,MAXORDER-1 do begin
        x = where(ret.data.runorder eq (i+1), cnt)
        x = where((ret.data.runorder eq (i+1)) and ret.data.gdu eq 1, cnt1)
        x = where((ret.data.runorder eq (i+1)) and ret.data.gdu eq 2, cnt2)
       
        print, format='("order ", I2, "   total/det1/det2 : ", ' + $
                     'I6, " (", F5.1, "%)  ",' + $
                     'I6, " (", F5.1, "%)  ",' + $
                     'I6, " (", F5.1, "%)")', $ 
               i+1, $
               cnt, 100.0*cnt/acnt, $
               cnt1, 100.0*cnt1/acnt1, $
               cnt2, 100.0*cnt2/acnt2
            
     endfor 

     if debug eq 2 then begin
     ; ----------------------------------------------------------
     ; calculate and plot the distribution around tgi3(xclose)
     ; create 20 bins to cover the interval +/-tChip around tgi3
     ; ----------------------------------------------------------
     window, 1

     bincnt = lonarr(20)
     for i=0,19 do begin
        factor = -0.9 + i*0.1
        x = where( (rtof(xclose)-tgi3(xclose)) lt (factor*tchip(xclose)), cnt)
        if i eq 0 then bincnt(i) = cnt $
        else           bincnt(i) = cnt - total(bincnt(0:i-1))
     endfor
     xbin = indgen(20) - 9.5
     plot, xbin, bincnt, psym=-4, ystyle=0, title='close points'

     ; ------------------------------------------------------
     ; now plot the distribution with single runners only
     ; ------------------------------------------------------
     bincnt = lonarr(20)
     for i=0,19 do begin
        factor = -0.9 + i*0.1
        xs = where(ret.data.runorder eq 1)
        x = where( (ret.data(xs).tof-ret.data(xs).tg) lt $
                    (factor*tchip(xs)), cnt)
        if i eq 0 then bincnt(i) = cnt $
        else           bincnt(i) = cnt - total(bincnt(0:i-1))
     endfor
     xbin = indgen(20) - 9.5
     plot, xbin, bincnt, psym=-4, ystyle=0

     ; ---------------------------------------------------------------------
     ; plot the distribution for equivalent single runners of double runners
     ; ---------------------------------------------------------------------
     bincnt = lonarr(20)
     for i=0,19 do begin
        factor = -0.9 + i*0.1
        xs = where(ret.data.runorder eq 2)
        x = where( (ret.data(xs).estof(1)-ret.data(xs).tg) lt $
                    (factor*tchip(xs)), cnt)
        if i eq 0 then bincnt(i) = cnt $
        else           bincnt(i) = cnt - total(bincnt(0:i-1))
     endfor
     xbin = indgen(20) - 9.5
     plot, xbin, bincnt, psym=-4, ystyle=0
     endif


  endif

undo_mods: ; undo modification of keywords if ppq call mechanism was used

  if not hav_call then begin
      tof   = tof_all
      ct    = ct_all
      ctype = ctype_all
      nc    = nc_all
      mc    = mc_all
      gdu   = gdu_all
      q     = q_all
      th    = th_all ; these three are actually not part of the keywords
      ph    = ph_all ; but nevermind
      en    = en_all ;
  endif

  return, ret

; -------------------------------------------------------------------------

END


; ===========================================================================
  PRO MultiRunnerPlot, x, sr, n, tcode, color, dots=dots, $
                       pl_reduce = pl_reduce
; ===========================================================================
; x             x array for plotting
; sr            single runner curve (Tgyro)
; n             order of multirunner to be plotted
; tcode         code period
; color         plot color specification
; ***************************************************************************

  if not keyword_set(pl_reduce) then pl_reduce = 1
  if not keyword_set(dots) then mypsym=0 else mypsym=3


  s = floor( (sr-n*sr+0.5*tcode)/tcode )
  mr = n*sr + s*tcode 
  lt0 = where(mr lt 0)
  if lt0(0) ne -1 then mr(lt0) = mr(lt0) + tCode(lt0)
  oplot, x, mr, color=color, $
         psym = mypsym, nsum = pl_reduce

;   mr = n * sr ; multi-runner
;   endloop = 0
;   plotted=0
;   while not endloop do begin
;       xtmp = where(mr ge (sr-0.5*tcode) and mr le (sr+0.5*tcode), cnt)
;       if cnt gt 0 then begin
;          plotted=1
;          oplot, x, mr, color=color, nsum=pl_reduce
;       endif else $
;          if plotted then endloop = 1
;
;       mr = mr - tcode
;   endwhile

END

