; common blocks that might be needed - maybe we don't need In and Out; can combine
cb_constants
cb_runEstIn
cb_runEstOut
cb_driftStepIn
cb_driftStepOut
cb_E_field
                                                 ??? clarify operation of the correlators ???
                                                 ??? how much data do we process at a pass ???
; indenting
; aim for 1 entry, 1 exit

EDI information needed:
	tof   =       ; time of flight in micro-s
	beamTime    =         ; beamtime in seconds since 1.1.1970
	beamQuality     =
	PNC_codeLengthBit =       ; correlator code type (0=short)
	correlatorN    =         ; correlator n (1,2,4,8,16,32,64)
	correlatorM    =          ; correlator m (16,8,4,2 )
	gdu   =

Mag information needed
  spinAvgB  ; spin-averaged B 2norm data ??? can we assume 2norm, comment it, drop the 2n ???
	spinAvgBtime; Unix Epoch
	possibly HiRes data?

; ============================================================================
FUNCTION RunEst, $
         d, bs, $     ; WW data and B data (hav passing mechanism)
         tof=tof, $      ; WW data and B data (ppq passing mechanism)
         stime=beamTime, $
         PNC_codeLengthBit=PNC_codeLengthBit, $
         correlatorN = correlatorN, correlatorM = correlatorM, $
         gdu = gdu, q = q, $
         st_sr=st_sr, bb_sr=bb_sr,  $
         MAXORDER=MAXORDER, $ ; configuration keywords
         CONF_LIM=CONF_LIM, $
         P_LIM=P_LIM, $
         SFAC=SFAC, $
         RELAX=RELAX, $
         smooth = smooth, $
         srb=srb, $     ; used only with (d,bs) passing mechanism
         useq1=useq1, $ ; ditto
         pl_reduce = pl_reduce,  $
         usedb = usedb, $
         db = db, $          ; use this delta B instead of determining it !
	; ============================================================================
	; parameters:
	;    d   : WW data structure as returned by GetWWData() in wwconv2.pro
	;    bs  : a structure containing a two arrays
	;          hr:  { beamTime, bx, by, bz, bb } high res B data
	;          avg: { beamTime, bx, by, bz, bb } spin res B data
	;          The structure bs is the result of function
	;          GetMagData() in module WWCONV.PRO
	;
	; replace these parameters in the function call as needed and
	; make the appropriate changes in the input section below
	; ============================================================================

	; first evaluate keywords
	if not keyword_set (MAXORDER) then MAXORDER=6                            ??? why would this change ???
	if not keyword_set (CONF_LIM) then CONF_LIM=20.0d                        ??? why would this change ???
	if not keyword_set (P_LIM) then P_LIM=exp(-2.0) ; gt p(2*sigma)          ??? why would this change ???
	if not keyword_set (smooth) then smooth = 1

	if not keyword_set (srb) then srb = 0                            ??? which do we usually use: HR or avg ???
	if not keyword_set (RELAX) then relax = 4.0
	if not keyword_set (XR) then xr=[0,1]
	if not keyword_set (useq1) then useq1 = 0                      ??? when would we use Q = 1 ???
	if not keyword_set (pl_reduce) then pl_reduce = 1
	if not keyword_set (usedb) then usedb = 0                      ??? when would we use a defined db ???

	if keyword_set(tof) then hav_call=0 else hav_call=1 ;          ??? diff twixt hav_call ppq_call ???

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

		nBeams       = n_elements(tof)
		spinAvgB   = bb_sr ; get B magnitude and associated time tags
		spinAvgBtime = st_sr
	endif else begin              ; This is the initialization branch
                              ; if parameters have been passed via
                              ; d,bs (hav passing mechanism)
		dd = d.data

   ; select data according to quality           ??? why use Q=1 data? ???
   ; --------------------------------
   if useq1 eq 0 then begin
      iGDU1Q1ge2MaxCh7  = where(dd.q1 ge 2 and dd.mch1 eq 7 and dd.ovfl_t1 eq 0, cnt1) ??? what is overflow? ???
      iGDU2Q1ge2MaxCh7  = where(dd.q2 ge 2 and dd.mch2 eq 7 and dd.ovfl_t2 eq 0, cnt2)
   endif else begin
      iGDU1Q1ge2MaxCh7  = where( (dd.q1 eq 1 or (dd.q1 ge 2 and dd.mch1 eq 7)) and $
                     dd.ovfl_t1 eq 0, cnt1)
      iGDU2Q1ge2MaxCh7  = where( (dd.q2 eq 1 or (dd.q2 ge 2 and dd.mch2 eq 7)) and $
                     dd.ovfl_t2 eq 0, cnt2)
   endelse
   nBeams = cnt1+cnt2

   if nBeams eq 0 then $
      return, { status:1,  msg:'Error: no data of selected quality!' }
??? Matt: Can we pre-alloc w nBeams here, then use direct insertion rather than concat below? Save logic, time ???
   if iGDU1Q1ge2MaxCh7(0) ne -1 then begin
      tof   = dd(iGDU1Q1ge2MaxCh7).tof1       ; time of flight in micro-s
      beamTime    = dd(iGDU1Q1ge2MaxCh7).ct1        ; beamtime in seconds since 1.1.1970
      PNC_codeLengthBit = dd(iGDU1Q1ge2MaxCh7).PNC_codeLengthBit      ; correlator code type (0=short)
      correlatorN    = dd(iGDU1Q1ge2MaxCh7).correlatorN         ; correlator n (1,2,4,8,16,32,64)
      correlatorM    = dd(iGDU1Q1ge2MaxCh7).correlatorM         ; correlator m (16,8,4,2 )
      gdu   = intarr(n_elements(iGDU1Q1ge2MaxCh7)) + 1
      quality     = dd(iGDU1Q1ge2MaxCh7).q1
   endif

   if iGDU2Q1ge2MaxCh7(0) ne -1 then begin
      if iGDU1Q1ge2MaxCh7(0) ne -1 then begin
         tof   = [tof,   dd(iGDU2Q1ge2MaxCh7).tof2]  ; time of flight in micro-s
         beamTime    = [beamTime,    dd(iGDU2Q1ge2MaxCh7).ct2]   ; beamtime in seconds since 1.1.1970
         PNC_codeLengthBit = [PNC_codeLengthBit, dd(iGDU2Q1ge2MaxCh7).PNC_codeLengthBit] ; correlator code type (0=short)
         correlatorN    = [correlatorN,    dd(iGDU2Q1ge2MaxCh7).correlatorN]    ; correlator n (1,2,4,8,16,32,64)
         correlatorM    = [correlatorM,    dd(iGDU2Q1ge2MaxCh7).correlatorM]    ; correlator m (16,8,4,2)
         gdu   = [gdu,   intarr(n_elements(iGDU2Q1ge2MaxCh7)) + 2]
         quality     = [quality,     dd(iGDU2Q1ge2MaxCh7).q2]
      endif else begin
         tof   = dd(iGDU2Q1ge2MaxCh7).tof2  ; time of flight in micro-s
         beamTime    = dd(iGDU2Q1ge2MaxCh7).ct2   ; beamtime in seconds since 1.1.1970
         PNC_codeLengthBit = dd(iGDU2Q1ge2MaxCh7).PNC_codeLengthBit ; correlator code type (0=short)
         correlatorN    = dd(iGDU2Q1ge2MaxCh7).correlatorN    ; correlator n (1,2,4,8,16,32,64)
         correlatorM    = dd(iGDU2Q1ge2MaxCh7).correlatorM    ; correlator m (16,8,4,2)
         gdu   = intarr(n_elements(iGDU2Q1ge2MaxCh7)) + 2
         quality     = dd(iGDU2Q1ge2MaxCh7).q2
      endelse
   endif

   ; these are Bmag data spanning the same time interval
   ; (or a little more) as the EDI PACMO1/5 data above
   ; ----------------------------------------------------------------------
   if srb ne 0 then begin
      spinAvgB = bs.avg.bb ; spin-averaged Bmag data
      spinAvgBtime   = bs.avg.spinAvgBtime ; associated spin-center times
   endif else begin
      spinAvgB = bs.hr.bb ; high resolution Bmag data
      spinAvgBtime   = bs.HiRes.spinAvgBtime ; associated times
   endelse
endelse


; smoothing of magnetic field data
; --------------------------------
if smooth gt 1 then spinAvgB = smooth(spinAvgB, smooth)

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
           quality:0, $                     ; quality
         }

  data = replicate(data, nBeams)

  ret = { data:data, $ ; runner data array
          status:0, $  ; return status: 0=ok, other = error
          msg:'', $    ; return message (error details)
          StdDevPerChipPeriod:0.0,$
          db:0.0, $
          smooth:0 }
; --------------------------------------------------------------------------


; sort in time
; ------------
sortindex = sort(beamTime)

tof_all   =   tof(sortindex) ; no filter, just sort
ct_all    =    beamTime(sortindex)
ctype_all = PNC_codeLengthBit(sortindex)
nc_all    =    correlatorN(sortindex)
mc_all    =    correlatorM(sortindex)
gdu_all   =   gdu(sortindex)
q_all     =     quality(sortindex)

; check number of data points
; ---------------------------
if nBeams lt 2 then begin
   ret.status = 1
   ret.msg = 'Error: less than two data points of selected quality!'
   return, ret
endif

; check the B data for gaps - who will decide if there are gaps?
??? what happens if the first Bavg is later than the first beam, but that beam should be used ???
??? will this be a problem on MMS if the time base is at the beginning of each time seg (1/4 spin) ???
CheckForGaps, spinAvgBtime, beamTime, x_no_gap, x_in_gap,  debug = debug
; create new inline check for data gaps

; return from runest if there are no EDI data outside of gaps
; -----------------------------------------------
if x_no_gap(0) eq -1 then begin
   ret.status = 1
   ret.msg = 'RunEst B-gap detection : no EDI data outside of gaps'
   return, ret
endif

tof   =   tof_all(x_no_gap) ; use tof, etc for these, not tof_all?
beamTime    =    ct_all(x_no_gap)
PNC_codeLengthBit = ctype_all(x_no_gap)
correlatorN    =    nc_all(x_no_gap)
correlatorM    =    mc_all(x_no_gap)
gdu   =   gdu_all(x_no_gap)
quality     =     q_all(x_no_gap)

N_NO_GAP =  n_elements(x_no_gap)

; redefine time as seconds since first beam time
; ----------------------------------------------
ssFirstBeam = beamTime - beamTime(0)

; calculate chipPeriod and codePeriod (assuming short code: 15 chips)
; ---------------------------------------------------------
nchips = intarr (N_NO_GAP) + 15 ; number of chips in Pseudo Noise Code
x      = where (PNC_codeLengthBit eq 1) ; see where long code has been used

if x (0) ne -1 then $
	nchips (x) = 127 ; set number of chips for long code

??? if we doc these are microsec, drop _us ???
chipPeriod = 0.119209289551 * correlatorM * correlatorN; 2.0d^ (-24)* 2.0 * 1.0d6 = 0.11921
codePeriod = nchips * chipPeriod               ; code length in microsec

??? above q > 1  --- see q=3 comment below ???
; interpolate tg_sr to q=3 data times : --> interpGyroPeriod ;'Interpolating Tg data to times of EDI hits...'
; convert Bavg to gyroPeriod, interpolate from
; spinAvgBtime-beamTime(0) time base to ssFirstBeam
; we may not need intrp for MMS because spinAvgB5s applies to whole beams5s
; we use spinAvgB2n several times, so save it?
interpGyroPeriod = interpol (B2Tg_nTus/spinAvgB, spinAvgBtime-beamTime(0), ssFirstBeam)

rtof = tof ; copy tof in order not to overwrite original data
f = 1.0    ; use only points closer to interpGyroPeriod than f*chipPeriod

; move data into tube +/- 0.5 codePeriod around gyro time (dealiasing)   ??? why +- 0.5 here and n below ???
;'Moving data into tube +/- 0.5*codePeriod around interpGyroPeriod'
; originally, this was interpGyroPeriod - tof, and appeared to assume that
; ??? every beam is some [nearly] integer multiple of codePeriod ???
; The gyroPeriod is greater than the tof, by an amount that varies according
; to several constraints, such as runner order.
; For a single runner, it is ToF that is some multipleof codePeriod, and
; in the best of times, it is 1 codePeriod.
; (interpGyroPeriod - tof) and (tof + n*codePeriod) can be thought of as
; moving the tof to the beginning of a time period, and appending
; an integral number of codePeriods to the end.
n = floor ( (interpGyroPeriod-tof)/codePeriod + 0.5 ) v0200b:L260
; this is just round ((interpGyroPeriod-tof)/codePeriod)
codePeriodRoundedToF = tof + n*codePeriod                        ??? what do we really expect here ??? delta-t twixt Tg and ToF ???

; Determine B offset
; ------------------
iQualityGT1 = where (quality gt 1, nQualityGT1)
if nQualityGT1 eq 0 then begin           ///// this is another exit, and should be avoided ////
   ret.status = 1
   ret.msg = 'Zero data points of quality>=2 for B offset determination.'
   goto, undo_mods
   ;return,  ret
endif

if usedb eq 0 then begin

   dbs = RE_DetermineBoffset (roundedToF(iQualityGT1), interpGyroPeriod(iQualityGT1), f*chipPeriod(iQualityGT1), debug=debug)
; make inline? ---------------------------------------------------------------

We come here with Q>1: what are the data integrity risks here? I.e., how do GyroPeriods, ToFs, and codePeriods
relate at this point?

; make inline? ---------------------------------------------------------------
  FUNCTION RE_DetermineBoffset, roundedToF, interpGyroPeriod, width, debug=debug
; Parameters:
;    roundedToF     dealiased time of flight
;    interpGyroPeriod      gyro time as calculated from initial bmag, interpolated to
;             times of EDI hits
;    width    half width of acceptance window around tgyro
;             Note that if this is f*chipPeriod, where f=1,
;             then there is a +- f*chipPeriod window on both sides
;
; Try to find offset for bmag data
;   - calculate mean difference between interpGyroPeriod and rtof and use this difference
;     to correct interpGyroPeriod -> firstAdjustedGyroPeriod
;   - now consider only points closer to firstAdjustedGyroPeriod than +/- width
;     correct firstAdjustedGyroPeriod again using only these points : --> tgi2
;   - use the total correction interpGyroPeriod -> tgi2 to calculate correction for bmag

@constants.inc

ret = { status:0, msg:'Ok', db:0.0 }

NN =  n_elements(roundedToF)

if NN lt 1 then begin                                   ??? seriously? if NN = 2, this works? ???
   ret.status =  1
   ret.msg = 'No points of quality>=2 available for B offset determination.'
   return, ret
endif

; ---------------------
; find first correction
; ---------------------
; save, roundedToF, interpGyroPeriod, filename='bestarg_mms_C3_2001-06-08t053000_054000_@20150306_RE_DetermineBoffset.sav'
??? is this the mean ???
firstAvg_ToF_Tg_diff = total (roundedToF - interpGyroPeriod) / NN
firstAdjustedGyroPeriod = interpGyroPeriod + firstAvg_ToF_Tg_diff

; ---------------------------------------------------------------------
; now select only points within +- codePeriod of firstAdjustedGyroPeriod and calculate another correction
; ---------------------------------------------------------------------
??? isn't this the same as asking which (abs (firstAvg_ToF_Tg_diff) < codePeriod) -> save doing the calcs???
xclose = where (abs (roundedToF - firstAdjustedGyroPeriod) lt width, NCLOSE )
if NCLOSE eq 0 then begin
   ret.status = 1
   ret.msg = 'Error: no points close to Tgyro found. Giving up!'
   return, ret
endif

delta2 = total (roundedToF (xclose) - firstAdjustedGyroPeriod (xclose)) / NCLOSE

delta = firstAvg_ToF_Tg_diff + delta2 ; combined correction

; -----------------------------------
; use this delta to find B correction
; From the equation of gyroPeriod:
; Tg = 2 pi m / |q| B, where m = electron mass, |q| is |electron charge|
; We have Tg = K / B, B = K / Tg, where K = 2 pi m / |q|
; B = (2 pi m / |q|) Tg -> dB/dTg = -2 pi m / |q| Tg^2
; But nT2micros = 2 pi m / |q| * 1e15, and we want dB(nT)/dTg(µs) =>
; B_offset = nT2micros / Tg^2 * delta_Tg
; -----------------------------------

avg_tof = total (rtof (xclose)) / NCLOSE
ret.dBdTg  = -B2Tg_nTus/avg_tof^2 * delta

return, ret

END
; make inline? ---------------------------------------------------------------
; make inline? ---------------------------------------------------------------

We get here with a +- adjustment for B


   if dbs.status ne 0 then begin
      ret.status = dbs.status
      ret.msg = dbs.msg
      goto, undo_mods
      ; return, ret
   endif
   db = dbs.db <<< db is dB is B_AdjustFactor, ' nT'
endif else begin
   print,'using passed B offset : ' , db
endelse

; recalculate Tgyro with new B offset
tgi3 = interpol(B2Tg_nTus / (spinAvgB+db), spinAvgBtime-beamTime(0), ssFirstBeam)

; 'Moving data into tube +/- 0.5*codePeriod around tgi3'
n = floor( (tgi3-roundedToF)/codePeriod  +  0.5 )           just rounding --- again --- but we are using already
rtof = rtof + n*codePeriod                                  rounded ToFs, so what is the effect?

; calculate standard deviation of tof around tgi3
xclose = where( abs(rtof(iQualityGT1)-tgi3(iQualityGT1)) lt f*chipPeriod(iQualityGT1) , NCLOSE ) ??? whose QualityGT1 ???
if NCLOSE gt 0 then begin
   stdev = sqrt( total((rtof(iQualityGT1(xclose))-tgi3(iQualityGT1(xclose)))^2) / NCLOSE )
   avg_tchip = total(chipPeriod(iQualityGT1(xclose))) / NCLOSE
;    if debug gt 0 then begin
;       print, 'analysis of quality>=2 data points:'
;       print, 'standard deviation (us) : ', stdev
;       print, 'average chip width (us) : ', avg_tchip
;       print, 'ratio stdev/chipPeriod       : ', stdev / avg_tchip
;    endif
endif else begin
   ret.status = 1
   ret.msg = 'No quality>=2 points close to tgi3 found. Giving up!'
   goto, undo_mods
endelse

if not keyword_set(SFAC) then $
	StdDevPerChipPeriod = stdev / avg_tchip $
else $
	StdDevPerChipPeriod = SFAC
endif


; Calculate distance of tof from n * tgyro (in multiples of tgi3)
; These calculations seem to assume that dist will be close to an
; integral number of gyroPeriods. dist is adjusted to keep it in the
; range of +- 0.5 codePeriods
dist = fltarr (MAXORDER, N_NO_GAP)
for i=0, MAXORDER-1 do begin
	dist(i,*) = (rtof - (i+1) * tgi3) mod codePeriod

	x = where (dist (i,*) gt 0.5 * codePeriod)
	if x(0) ne -1 then $
		dist (i,x) = dist (i,x) - codePeriod (x)

	x = where(dist(i,*) lt -0.5*codePeriod)
	if x(0) ne -1 then $
		dist (i,x) = dist (i,x) + codePeriod (x)
endfor

; assign order assuming gaussian probability distributions
; and use p_max^2/sum(others) as a confidence criterion
chipPeriodStdDev = StdDevPerChipPeriod * chipPeriod ; this is the stdev of the gaussian distributions

p_run = fltarr(MAXORDER, N_NO_GAP) ; define some arrays
p_tot = fltarr(N_NO_GAP)
p_max = p_tot
order = intarr(N_NO_GAP)

; calculate probabilities
for i=0, MAXORDER-1 do begin
	; the prob for each beam for this runner order
	p_run (i,*) = exp ( ((dist(i,*) / chipPeriodStdDev) / -2.0) ^2)

	x = where (abs (dist(i,*)) gt chipPeriod)  ; is this reasonable to do ?
	; I don't think it can ever be; see above... but the check is a simple one
	; and if something went very wrong, then it is possible that dist could
	; still be gt chipPeriod, even after the +- 0.5 adjustment.
	; Setting these Probs to zero effectively removes them from Pmax searches: Pvalid is always > 0
	if x(0) ne -1 then $
		p_run (i,x) = 0.0

	p_tot = p_tot + p_run (i,*) ; p_tot (beams), so we are summing all the P for each beam

	x = where (p_run (i,*) gt p_max) ; p_max initially zero
	if x(0) ne -1 then begin ; here is where we track the max order for each beam
		p_max (x) = p_run(i,x) ; p_max (beams)
		order (x) = i+1
	endif
endfor

x = where (p_max lt P_LIM)    ; set order to 'unknown' if p is below threshold
if x (0) ne -1 then $
	order (x) = 0

; now calculate a confidence level for the runner order which
; has the highest probability (some sort of SNR^2)
; set runner order to 'undetermined' (0) where the confidence level is
; below the threshold
p_other = p_tot - p_max ;

x = where (p_other lt 1.d-10) ; effectively 0.0
if x(0) ne -1 then $
	p_other (x) = 1.0d-10 ; avoid numerical problems
conf = p_max^2 / p_other ; confidence level

; check for CONF_LIM in pp_...
x = where(conf lt CONF_LIM)  ; set order to 'unknown' if c is below threshold
if x(0) ne -1 then order(x) = 0

; Here is what the function returns in case everything is ok
  ret.status = 0 ; Ok
  ret.msg    = 'OK'
  ret.StdDevPerChipPeriod   = StdDevPerChipPeriod
  ret.db     = db
  ret.smooth = smooth

  ret.data.beamtime = ct_all
  ret.data.gdu      = gdu_all
  ret.data.quality        = q_all
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
     esr_ret =  EquivSrToF(rtof, i+1, tgi3, codePeriod)
     if esr_ret.status ne 0 then begin
        ret.status = esr_ret.status
        ret.msg    = esr_ret.msg
        goto, undo_mods
        ;return, ret
     endif
     ret.data(x_no_gap).estof(i) = esr_ret.estof
  endfor

  ; fill in the data for points in gaps (make them class C beams)
  if x_in_gap(0) ne -1 then begin
     ret.data(x_in_gap).runorder = 0
     ret.data(x_in_gap).flag(*) = 0
     ret.data(x_in_gap).prob(*) = 0.0
     ret.data(x_in_gap).tg  = -1.0
     ret.data(x_in_gap).tof = -1.0
     ret.data(x_in_gap).estof(*) = -1.0
  endif

undo_mods: ; undo modification of keywords if ppq call mechanism was used

  if not hav_call then begin ??? why do this ???
      tof   = tof_all
      beamTime    = ct_all
      PNC_codeLengthBit = ctype_all
      correlatorN    = nc_all
      correlatorM    = mc_all
      gdu   = gdu_all
      quality     = q_all
  endif

  return, ret
END
