pro ep_assign_order, $
	beam, xd, yd, gyrorad, flip, $ ; IN
	sigratio_nopen, fdoe_nopen, $ ; OUT
	rchi2, chi2, ndegfree, nclimb, chi_stat, $ ; OUT
	rchi2_unpen, ndegfree_unpen, nclimb_unpen, $ ; OUT
	bumpdown=bumpdown, perr3=perr3

	common ep_envar_con_cb

	;******************************************************************
	; By default, the 1st iteration has up to now used robust estimation.
	; Now, the chi^2 for the best target [xd,yd] (not for the entire target
	; space) is calculated.
	; Keep the information of what order each beam has in bestord.
	; Keep the chi2 and rchi2 and ndegfree values!!!!
	;******************************************************************

	; NO! sigratio = fltarr(beam.mm)
	sigratio_nopen = fltarr(beam.mm)

	; NO! fdoe = fltarr(beam.mm)
	fdoe_nopen = fltarr(beam.mm)

	chi_sum = 0.0
	nchi_sum = 0L
	penalty_sum = 0.0
	perr3 = [-1.]

	; NO HIGHER-ORDER-SUPRESSION-PENALTY HERE!
	poc_pen = 0

	for im = 0, beam.mm-1 do begin
		if (beam.tri_ok(im) eq 1) then begin
			iordermin = 0
			iordermax = beam.n_posord(im)-1
			ordermin = beam.posord(iordermin,im)
			ordermax = beam.posord(iordermax,im)

			; ordvec could be any monotonically increasing (not necessarily by 1)
			; sequence of numbers ranging between 1 and beam.maxorder.
			; It represents the most likely (in a statistical sense) orders for
			; this beam.
			ordvec = beam.posord(iordermin:iordermax,im)

			; Firing angle error
			fa_error = beam.sigfir(im) + (ordvec-1)*(0.9*!dtor) * beam.runest_estg(im)/3900.

			dely = ordvec*yd -beam.GDU_locY(im)
			delx = ordvec*xd -beam.GDU_locX(im) ; vectors from gun to possible targets
			dalp = atan(dely,delx)  ; Angle of pos. target, straight-line traj.

			if (pp_curve_traj eq 1 or keyword_set(bumpdown)) then begin ; Curved trajectories (poorman's)

				; What is the dot product between the possible target vectors (as seen
				; from the gun) and the gun firing direction vectors?
				; dot must have same dimensions as delx, dely and dalp
				nord = n_elements(ordvec)
				that = [[ordvec*xd-beam.GDU_locX(im)],$ ; [nord,2]
						    [ordvec*yd-beam.GDU_locY(im)]]
				tmag = sqrt(total(that^2,2))#make_array(2,/float,value=1.) ; [nord,2]
				that = that/tmag    ; [nord,2], Unit length
				ghat = make_array(nord,/float,value=1.) # $ ; [nord,2], Unit length
					[cos(beam.alpha(im)),sin(beam.alpha(im))]
				dot = total(that*ghat,2) ; [nord], dot>0 towards, dot<0 away
				angle = acos(dot)   ; [nord]
				;---------------------------------------------------------------
				; window, 0, xsize=800, ysize=800
				; edi_setcolors, cs
				; xx = xd
				; yy = yd
				; dd = dot(0)
				; drange = max([abs(xx),abs(yy)])
				; xrange = [-drange,drange]*2.
				; yrange = xrange
				; plot, [xx], [yy], psym=2, symsize=.25, xrange=xrange, yrange=yrange
				; if (dd ge 0) then oplot, [xx], [yy], psym=2, symsize=1, color=cs.red
				; if (dd lt 0) then oplot, [xx], [yy], psym=2, symsize=1, color=cs.blue
				; arrow, beam.GDU_locX(im),beam.GDU_locY(im),10.*cos(beam.alpha(im))+beam.GDU_locX(im), $
				;   10.*sin(beam.alpha(im))+beam.GDU_locY(im), color=cs.green, /data
				; wait, 1
				;---------------------------------------------------------------

				dalp = ep_calc_ctraj(dalp,dely,delx,gyrorad,flip,dot)

				;        print, im, gyrorad, flip, beam.alpha(im)*!radeg, dot(0), dely(0), delx(0), dalp1_save*!radeg, dalp(0)*!radeg, $
				;          format='(i2,2x,e8.2,2x,i1,2x,f7.2,2x,f6.3,2x,f6.2,2x,f6.2,2x,f7.2,2x,f7.2)'
			endif

			; Differences between angles of possible targets and of measured beam
			dalp = abs(dalp-beam.alpha(im))
			; Must check direction (given that we might have away beams)
			dalp = dalp*(dalp lt !pi)+(2*!pi-dalp)*(dalp ge !pi)
			dalp = dalp*(dalp lt !pi/2)+(!pi-dalp)*(dalp ge !pi/2)

			; Save this array for perr3 calculation
			dalp_save = dalp        ; Vector of same length as ordvec

			delx = sqrt(delx^2+dely^2) ; distances from gun to possible targets
			dely = fa_error^2 + atan(beam.siggdu,delx)^2 ;intrinsic errors of firing angles
			dalp = dalp^2 / dely    ;difference over error
			dalp_nopen = dalp       ;No penalty

			; NO!        dalp = dalp * ordvec^poc_pen ;With higher-order-suppression penalty
			; NO!        f0 = min (dalp, iorderbest)

			f0_nopen = min (dalp_nopen, iorderbest_nopen)

			f0_nopen_b = f0_nopen
			iorderbest_nopen_b = iorderbest_nopen

			; Do we keep this iorderbest_nopen?  Or do we bump it down if a minimum
			; angle requirement is met?
			if (keyword_set(bumpdown) and $
				iorderbest_nopen ne 0 and $ ; Not at lowest legal order
				n_elements(ordvec) gt 1) then begin ; Have more than 1 legal order

				for itry=iorderbest_nopen,0,-1 do begin
					if (angle(itry) gt pp_reorder_anglim_rad or itry eq 0) then begin
						iorderbest_bump = itry
						goto, bumpout
					endif
				endfor

bumpout:
				if (iorderbest_bump ne iorderbest_nopen) then begin
					iorderbest_nopen = iorderbest_bump
					f0_nopen = dalp_nopen(iorderbest_nopen)
					beam.class(im) = 'R'
				endif
			endif

			; NO!        orderbest = ordvec(iorderbest)

			orderbest = ordvec(iorderbest_nopen)
			beam.bestord(im) = orderbest

			;===========================================================
			; Bookkeeping for perr3
			perr3 = [perr3,dalp_save(iorderbest_nopen)]

			;===========================================================
			; Bookkeeping for the chi2 and rchi2 and ndegfree values
			;        print, 'ep_assign_order: ', beam.class_init(im), beam.class(im), iorderbest_nopen_b, iorderbest_nopen, f0_nopen_b, f0_nopen

			chi_sum = chi_sum + f0_nopen
			nchi_sum = nchi_sum + 1
			penalty_sum = penalty_sum + beam.runest_penalty(im)

			;===========================================================
			; Bookkeeping, non-robust, using Target [xd,yd]
			; Both non-penalized, and penalized ('nopen' variables)
			; chi2 contributions
			; NO!        fdoe(im) = f0

			fdoe_nopen(im) = f0_nopen
			; NO!        sigratio(im) = beam.sigfir(im)^2 / dely(iorderbest)
			sigratio_nopen (im) = fa_error (iorderbest_nopen)^2 / dely (iorderbest_nopen)
			;===========================================================

		endif
	endfor

	; perr3 calculation
	perr3 = perr3(1:nchi_sum)
	perr3 = stddev(perr3)           ; radians

	surfdim = 2                     ; Two fit parameters: r and phi
	chi2 = chi_sum

	; Without runner penalty
	ndegfree_unpen = nchi_sum - surfdim
	nclimb_unpen = surfdim
	rchi2_unpen = chi2/float(ndegfree_unpen)

	; With runner penalty
	ndegfree = nchi_sum - surfdim - (penalty_sum/float(nchi_sum)-1)
	nclimb = surfdim + (penalty_sum/float(nchi_sum)-1)
	if (ndegfree le 0) then begin
		chi_stat = 0
		rchi2 = pp_rfill
	endif $
	else begin
		chi_stat = 1
		rchi2 = chi2 / float(ndegfree)
	endelse

	return
end
