pro ep_calc_chisurf, $
	status, status_renorm,                         $ ; Output status
	robust, poc, use_bestord,                      $ ; Control, IN
	gyrorad, gyroper, flip,                        $ ; Curved traj. Info, IN
	nr, np, rsurf, psurf, xsurf, ysurf, resop,     $ ; Grid, IN
	beam,                                          $ ; Beam Info, IN
	$
	ndegfree, nclimb, chi_stat,                    $ ; OUT
	ndegfree_unpen, nclimb_unpen,                  $ ; OUT
	$
	chisurf,                                       $ ; Chi2-surface info, OUT
	xd, yd, rd, pd,                                $ ; Target info, OUT
	rchi2, chi2, rchi2_unpen,                      $ ; OUT
	rderr_min, pderr_min,                          $ ; OUT
	$
	chisurf_renorm,                                $ ; Chi2-surface using fa_error_renorm
	xd_renorm, yd_renorm, rd_renorm, pd_renorm,    $
	rchi2_renorm, chi2_renorm, rchi2_renorm_unpen, $
	rderr_min_renorm, pderr_min_renorm,            $
	$
	finegrid=finegrid

print, 'ep_calc_chisurf starting'
	common ep_envar_con_cb

	status = 0
	status_renorm = 0
	chi_stat = 0

	n_posord = beam.n_posord        ; # of possible "legal" orders for each beam
	posord = beam.posord            ; Array of these legal orders

	; Use information in beam.bestord, i.e., only 1 "legal" order for each beam,
	; which has been previously determined
	if (use_bestord) then begin
		n_posord = make_array(size=size(beam.n_posord),value=1)
		posord = make_array(size=size(beam.posord),value=0)
		posord(0,*) = beam.bestord
	endif

	; July 28th, 2003 (Maggie's Birthday)
	; The error associated with the beam firing angle is now a function of
	; beam order in the following way:
	; fa_error = beam_width + (beam_order-1) * (0.9 degrees) * (T_gyro microsec/3900 microsec)
	; The flight software on Cluster adjusts the beam firing direction within the Bperp plane
	; by 0.9 degrees every 3.9 milliseconds. The numbers are approximate, but good enough
	; for the purpose of adjusting the beam angular uncertainty for multi-runners.
	; - actual stepping size: arctan (1/64) =~ 0.895 degrees
	; - processing loop period: 1/256 seconds =~ 3.906 ms

	; The numbers will be different for MMS:
	; - stepping size: arctan(1/128) = ~0.448 degrees
	; - processing loop period: 1/512 seconds =~ 1.953 ms

	; Since the processing loop period on MMS might change to 1/256 seconds
	; (in which case the stepping size would be changed to 0.9 degrees),
	; it is probably best to make these two numbers constants that can easily be changed
	; without having to dive deep into an IDL function.

	fa_error        = make_array (beam.maxorder, beam.mm, /float)
	fa_error_renorm = make_array (beam.maxorder, beam.mm, /float)

	; Nominal firing angle errors
	for i=0, n_elements (fa_error(*,0))-1 do begin ; why not beam.maxorder-1?
		fa_error (i, *) = beam.sigfir (*) + (posord (i,*)-1) * (0.9 * !dtor) * beam.runest_estg (*) / 3900.
	endfor

	; Renormalized firing angle errors (not a function of beam order,
	; because they've been evaluated at the best order - see bestarg.pro)
	; The output which uses this quantity is only used when the
	; use_bestord keyword is set, so this is consistent.
	fa_error_renorm (0,*) = beam.fa_error_renorm (*)

	;******************************************************************
	;for each measurement im:
	;  assign angle with respect to GDU_locX,GDU_locY(im) to each x,y --> dalp
	;  compute dalp also for multiples order*(x,y)
	;  subtract gun angle alpha(im) --> dalp(x,y,order)
	;  dalp is the deviation from perfect firing, make it less than 90 degrees
	;  find the order for which dalp has its lowest value f0(x,y)
	;sum of f0 from im=0 to im=mm-1 --> likelihood function farr
	;******************************************************************

	; Local arrays
	farr        = fltarr (nr,np)
	farr_renorm = fltarr (nr,np)
	nsum_f      = 0.
	penalty_sum = 0.

	delx  = fltarr (nr, np, beam.maxorder) + 1. ; why is delx larger than dely?
	dely  = fltarr (nr, np, beam.maxorder)
	faerr = fltarr (nr, np, beam.maxorder) ; Firing angle error
	faerr_renorm = fltarr (nr, np, beam.maxorder)

	ntot = long (float(nr) * float(np)) ; n_elements (xsurf)?
	xsurf_1D = reform (xsurf, ntot)
	ysurf_1D = reform (ysurf, ntot)

	;========================
	; Speed Test
	;ST ysurf_st = reform(ysurf_1D#make_array(beam.maxorder,/float,value=1.), nr,np,beam.maxorder)
	;ST xsurf_st = reform(xsurf_1D#make_array(beam.maxorder,/float,value=1.), nr,np,beam.maxorder)
	;ST delx_st  = fltarr(nr,np,beam.maxorder) + 1.
	;ST dely_st  = fltarr(nr,np,beam.maxorder)
	; Speed Test
	;========================

	;ST ct_whole_beg = systime(1)
	for im = 0, beam.mm-1 do begin
		ordermin = posord (0,im)
		iordermin = 0

		if (beam.tri_ok (im) eq 1) then begin
			;ST ct_beg = systime(1)
			for iorder = 0, n_posord(im)-1 do begin
				order = posord (iorder,im)
				; this is huge - the entire (xsurf - GDU_locX) - 1 copy for each order of runner
				; vectors from gun to possible targets
				delx (*,*,iorder) = order*xsurf -beam.GDU_locX(im)
				dely (*,*,iorder) = order*ysurf -beam.GDU_locY(im)

				faerr (*,*,iorder) = fa_error (iorder,im)               ; Firing angle error on grid
				faerr_renorm (*,*,iorder) = fa_error_renorm (iorder,im)
			endfor

			;ST         ct_end = systime(1)
			;ST         print, 'Loop 1: ', ct_end-ct_beg

			;===========
			; Speed Test
			;ST         ct_beg = systime(1)
			;ST         ordvec = reform(make_array(ntot,/float,value=1.)# $
			;ST                         posord(0:n_posord(im)-1,im), $
			;ST                         nr,np,n_posord(im)) ; [nr,np,no]
			;ST         dely_st(*,*,0:n_posord(im)-1) = $
			;ST           ordvec*ysurf_st(*,*,0:n_posord(im)-1) - beam.GDU_locY(im)
			;ST         delx_st(*,*,0:n_posord(im)-1) = $
			;ST           ordvec*xsurf_st(*,*,0:n_posord(im)-1) - beam.GDU_locX(im)
			;ST         ct_end = systime(1)
			;ST         print, 'Loop 1 Vectorized: ', ct_end-ct_beg
			; Speed Test
			;===========

			dalp = atan (dely,delx) ; matrix: Angles of possible targets, straight-line traj

			if (pp_curve_traj eq 1) then begin

				; Need a dot product for the curved trajectory calculation
				; What is the dot product between the possible target vectors (as seen
				; from the gun) and the gun firing direction vectors?
				; dot must have same dimensions as delx, dely and dalp
				;ST         ct_beg = systime(1)
				that = [ [xsurf_1D-beam.GDU_locX(im)], [ysurf_1D-beam.GDU_locY(im)] ]
				tmag = sqrt(total(that^2,2)) # make_array (2, /float, value=1.) ; [ntot,2] ; make_array? seriously? not [ 1. 1. ]?
				that = that/tmag    ; [ntot,2], Unit length
				; use that = that / norm (that) instead; defaults to L2 (Euclidean) norm for vectors

				; next up looks like another outer prduct - rebin

				ghat = make_array (ntot, /float, value=1.) # [cos(beam.alpha(im)),sin(beam.alpha(im))]
				dot = total(that*ghat,2) ; [ntot], dot>0 towards, dot<0 away

				; dot gives that same value regardless of target order, so we can cast
				; this last dimension as:

				dot = dot # make_array(beam.maxorder,/float,value=1.) ; another outer product
				dot = reform (dot,nr,np,beam.maxorder) ; [nr,np,maxorder]

				;ST         ct_end = systime(1)
				;ST         print, 'Vector 1: ', ct_end-ct_beg

				;-------------------------------------------------------
				; window, 0, xsize=800, ysize=800
				; edi_setcolors, cs
				; xx = xsurf
				; yy = ysurf
				; dd = dot(*,*,0)
				; plot, xx, yy, psym=2, symsize=.25
				; red = where(dd ge 0)
				; if (red(0) ne -1) then oplot, xx(red), yy(red),  psym=2, symsize=.25, color=cs.red
				; blue = where(dd lt 0)
				; if (blue(0) ne -1) then oplot, xx(blue), yy(blue), psym=2, symsize=.25, color=cs.blue
				; arrow, beam.GDU_locX(im),beam.GDU_locY(im),10.*cos(beam.alpha(im))+beam.GDU_locX(im), $
				;   10.*sin(beam.alpha(im))+beam.GDU_locY(im), color=cs.green, /data
				; wait, .5
				;-------------------------------------------------------

				dalp = ep_calc_ctraj(dalp,dely,delx,gyrorad,flip,dot)
			endif

			; Differences between angles of possible targets and of measured beam
			dalp = abs (dalp - beam.alpha(im))
			delx = sqrt(delx^2+dely^2) ; distances from gun to possible targets --- huh? reuse delx?

			; intrinsic errors of firing angles
			dely = faerr^2 + atan (beam.siggdu, delx)^2
			dely_renorm = faerr_renorm^2 + atan (beam.siggdu,delx)^2

			; Force angle difference to be between 0 and 90 degrees
			dalp  = dalp * (dalp lt !pi)   +(2*!pi-dalp) * (dalp ge !pi)
			dalp0 = dalp * (dalp lt !pi/2) +(!pi-dalp)   * (dalp ge !pi/2)

			; difference over error
			dalp = dalp0^2 / dely
			dalp_renorm = dalp0^2 / dely_renorm

			f0 = dalp(*,*,iordermin) * ordermin^poc
			f0_renorm = dalp_renorm(*,*,iordermin) * ordermin^poc
			if robust then begin
				f0 = alog(1 + 0.5*f0) ; Cauchy distribution
				f0_renorm = alog(1 + 0.5*f0_renorm)
			endif

			;ST         ct_beg = systime(1)
			for iorder=1,n_posord(im)-1 do begin
				order = posord(iorder,im)

				f1 = dalp(*,*,iorder) * order^poc
				if robust then f1 = alog(1 + 0.5*f1)
				higher=where(f1 lt f0)
				if total(higher) ge 0 then f0(higher)=f1(higher)

				f1_renorm = dalp_renorm(*,*,iorder) * order^poc
				if robust then f1_renorm = alog(1 + 0.5*f1_renorm)
				higher=where(f1_renorm lt f0_renorm)
				if total(higher) ge 0 then f0_renorm(higher)=f1_renorm(higher)
			endfor                  ;best order
			;ST         ct_end = systime(1)
			;ST         print, '2nd Loop: ', ct_end-ct_beg

			farr = farr + f0
			farr_renorm = farr_renorm + f0_renorm
			nsum_f = nsum_f + 1
			penalty_sum = penalty_sum + beam.runest_penalty(im)
		endif
	endfor
	;ST ct_whole_end = systime(1)
	;ST print, 'Whole Loop: ', ct_whole_end-ct_whole_beg

	nchi = nsum_f
	surfdim = 2                     ; Two fit parameters: r and phi

	ndegfree_unpen = nchi - surfdim ; Will always be at least 2
	nclimb_unpen = surfdim

	ndegfree = nchi - surfdim - (penalty_sum/float(nchi)-1)
	nclimb = surfdim + (penalty_sum/float(nchi)-1)

	chisurf = farr
	chisurfmin = min(chisurf,ibest)
	rd= rsurf(ibest)
	pd= psurf(ibest)
	xd= rd * cos(pd)
	yd= rd * sin(pd)
	; If ibest is on the boundary of the surface, then we need to abort
	; because this isn't guaranteed to be a minimum
	rmin= rsurf(0,0)
	rmax= rsurf(nr-1,0)

	if rd eq rmin or rd eq rmax then begin
		status = 0
		goto, calc_renorm
	endif

	if (keyword_set(finegrid)) then begin ; Not necessary for coarse grid
		pmin= psurf(0,0)
		pmax= psurf(0,np-1)
		if pd eq pmin or pd eq pmax then begin
			status = 0
			goto, calc_renorm
		endif
	endif

	; Define the minimum errors on rd, pd (these will most likely
	; be increased by a subsequent call to ep_param_errors.pro)
	rderr_min = rsurf(ibest+1) - rd ; Minimum
	pderr_min = resop               ; Minimum

	; Define the number of degrees of freedom, and normalize the
	; chisurfmin values
	chi2 = chisurfmin

	; No runner penalty
	rchi2_unpen = chi2/float(ndegfree_unpen)

	; With runner penalty
	if (ndegfree le 0) then begin
		chi_stat = 0
		rchi2 = pp_rfill
	endif else begin
		chi_stat = 1
		rchi2 = chi2/float(ndegfree)
	endelse
	status = 1

	calc_renorm:
	;============ RENORM QUANTITIES=====================
	chisurf_renorm = farr_renorm
	chisurfmin_renorm = min(chisurf_renorm,ibest_renorm)
	rd_renorm= rsurf(ibest_renorm)
	pd_renorm= psurf(ibest_renorm)
	xd_renorm= rd_renorm * cos(pd_renorm)
	yd_renorm= rd_renorm * sin(pd_renorm)
	rmin= rsurf(0,0)
	rmax= rsurf(nr-1,0)

	if rd_renorm eq rmin or rd_renorm eq rmax then begin
		status_renorm = 0
		return
	endif

	if (keyword_set(finegrid)) then begin ; Not necessary for coarse grid
		pmin= psurf(0,0)
		pmax= psurf(0,np-1)
		if pd_renorm eq pmin or pd_renorm eq pmax then begin
			status_renorm = 0
			return
		endif
	endif

	rderr_min_renorm = rsurf(ibest_renorm+1) - rd_renorm
	pderr_min_renorm = resop
	chi2_renorm = chisurfmin_renorm
	rchi2_renorm_unpen = chi2_renorm/float(ndegfree_unpen)

	if (ndegfree le 0) then   $
		rchi2_renorm = pp_rfill $
	else $
		rchi2_renorm = chi2_renorm / float(ndegfree)

	status_renorm = 1

	return
end
