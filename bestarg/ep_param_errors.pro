;	ep_param_errors.pro,v 1.20 2010/05/03 14:51:08 ppq Exp

pro ep_param_errors, $
	chisurf, $ ; Chi2 surface info, IN
	xsurf, ysurf, rsurf, psurf, $ ; Grid info, IN
	xd, yd, rd, pd, $ ; Target info, IN
	rchi2, chi2, ndegfree, nclimb, chi_stat, $ ; Goodness info, IN
	rderr_min, pderr_min, $ ; Minimum errors to start with, IN
	rderr, pderr, $ ; Error info, IN/OUT
	ambig_180=ambig_180, $
	coarse=coarse, fine=fine, $ ; IN
	derr2=derr2, perr2=perr2, $ ; OUT
	keyplot3=keyplot3, beam=beam, kp3_title=kp3_title, $ ; IN
	oplot_x=oplot_x, oplot_y=oplot_y, $ ; IN
	edge_error=edge_error

	; If we're in the ToF regime (virtually parallel beams):
	;      The triangulation surface will only narrow down the drift step
	;      angle (not the magnitude) and then only to within 180 degrees.
	;      The drift step magnitude is not well-determined at all.

	common ep_envar_con_cb

	coarse = keyword_set(coarse)
	keyplot3 = keyword_set(keyplot3)
	edge_error = keyword_set(edge_error)

	goto, skip_v2_32

	; Choose the confidence level
	; nclimb should never be more than 5
	; nclimb= 1     2     3     4     5     6
	con99 = [6.63, 9.21, 11.3, 13.3, 15.1, 16.8]
	con90 = [2.71, 4.61, 6.25, 7.78, 9.24, 10.6]
	; Round nclimb to the nearest integer
	nclimb_round = long(nclimb + .5)
	if (coarse) then clev = con99(nclimb_round-1) else clev = con90(nclimb_round-1)
	; OLD if (coarse) then clev = 9.21 else clev = 4.61
	bowl_error_scale_factor = chi2/float(ndegfree)

skip_v2_32:

	if (coarse) then $
		clev = cak_fchiprob(.990,nclimb) else $
		clev = cak_fchiprob(.900,nclimb)
	bowl_error_scale_factor = chi2/float(ndegfree) > 1.

	if (edge_error) then $
		bowl_error_scale_factor = 1.0

	if (coarse) then $
		conlevel = [clev] $
	else $
		conlevel = [clev,clev*bowl_error_scale_factor]
		; 1.   = 68% confidence, 1 fit parameter
		; 2.71 = 90% confidence, 1 fit parameter
		; 6.63 = 99% confidence, 1 fit parameter
		; 2.30 = 68% confidence, 2 fit parameters
		; 4.61 = 90% confidence, 2 fit parameters
		; 9.21 = 99% confidence, 2 fit parameters
		; 3.53 = 68% confidence, 3 fit parameters
		; 6.25 = 90% confidence, 3 fit parameters
		; 11.3 = 99% confidence, 3 fit parameters
		; See "Numerical Recipes" by Press, 1986
		; page 536 in the 'Modeling of Data' chpt.

	if (edge_error) then $
		conlevel = [clev]

	for ii=0, n_elements(conlevel)-1 do begin

		if (edge_error) then $
			goto, skip_inside

		inside = where(chisurf-chi2 le conlevel(ii))
		;    if (inside(0) eq -1) then begin
		;        message, 'Caught it', /cont
		;        stop
		;    endif
		ninside = n_elements(inside)

		;----------------------------------------------------------------
		rderr0 = max([max(rsurf(inside))-rd, rd-min(rsurf(inside)), rderr_min])
		;----------------------------------------------------------------
		; Must look at the dot products!!!  Duh!
		; All of the vectors with heads within region defined by 'inside'
		nin = n_elements(inside)
		invecs = fltarr(2,nin)
		invecs(0,*) = cos(psurf(inside)) ; invecs are unit vectors!!!!
		invecs(1,*) = sin(psurf(inside))

		; Best target vector (cast into same dimension as invecs)
		tvec = [cos(pd),sin(pd)]#make_array(nin,/float,value=1.) ; unit vectors
		dotp = total(invecs*tvec,1)
		dotp = dotp*((dotp ge -1.) and (dotp le 1.)) + 1.*(dotp gt 1.) + (-1.)*(dotp lt -1.) ; Precision problems make this necessary
		angles_from_target = acos(dotp) ; Ranges from 0 to !pi by def.
		pderr0 = max([max(angles_from_target),pderr_min])

		; If we're on the coarse grid, we want to limit how large pderr0 can
		; be because this is used to define the azimuthal extent of the fine
		; grid.  So, if pderr0 is > pi/2 AND all points within the
		; 'inside' array are sufficiently far away from the origin, then just
		; ask who's within pi/2 degrees from the target, use these points to
		; redefine pderr0, and set the ambig_180 flag which will be output.
		if (coarse) then begin

			;        message, 'Here in coarse grid', /cont
			;        print, 'pderr0: ',pderr0*!radeg
			;        stop

			ambig_180 = 0
			izero = where(rsurf(inside) le 2.*pp_cg_minresr_m)
			if (pderr0 gt !pi/2 and izero(0) eq -1) then begin
				iss = where(angles_from_target lt !pi/2.)
				pderr0 = max([max(angles_from_target(iss)),pderr_min])
				ambig_180 = 1
			endif
		endif

		if (ii eq 0) then begin
			rderr = rderr0
			pderr = pderr0
		endif else begin
			derr2 = rderr0
			perr2 = pderr0
		endelse

skip_inside:

;----------------------------------------------------------------
		if keyplot3 then begin
			if (coarse) then begin
				title = 'COARSE GRID, Rchi2 = ' +strtrim(rchi2,2) +', Nominal 99% ConLevel!c' +kp3_title
				conlevel_color='green'
				last = 0
				init = 1
				siz=1.5
				rsiz = 0.35
			endif $
			else begin
				if (ii eq 0) then $
					title = 'FINE GRID, Rchi2 = '+$
					strtrim(rchi2,2)+', Nominal 90% ConLevel!c'+kp3_title $
				else $
					title = 'FINE GRID, Rchi2 = '+$
					strtrim(rchi2,2)+', Scaled 90% ConLevel!c'+kp3_title

				if (edge_error) then $
					title='FINE GRID Failure: Target on Edge!c'+kp3_title

				conlevel_color='red'
				last = 1
				init = 0
				siz=1.5
				rsiz = 0.35
			endelse

			if (not edge_error) then begin
				chisurf_plot, $
					beam.mm, beam.GDU_locX, beam.GDU_locY, beam.alpha, $
					beam.out, beam.gunid, beam.maxorder, beam.bestord, $
					beam.tri_ok, $
					chisurf, xsurf, ysurf, $
					xd, yd, rd, pd, $
					rchi2, $
					conlevel(ii), inside, rderr0, pderr0, $
					conlevel_info, conlevel_xy, $
					conlevel_color=conlevel_color, last=last, init=init, siz=siz, $
					title=title, oplot_x=oplot_x, oplot_y=oplot_y, rsiz=rsiz, ninside=ninside, $
					coarse=coarse, ps=0

				res = ''
				read, res, prompt = 'Harcopy? [y or n] : '
				if (res eq 'y') then begin
					read, res, prompt = 'File name: '
					set_plot, 'PS'
					device, file=res, /landscape, xs=10, ys=5, /inches, /color
					chisurf_plot, $
						beam.mm, beam.GDU_locX, beam.GDU_locY, beam.alpha, $
						beam.out, beam.gunid, beam.maxorder, beam.bestord, $
						beam.tri_ok, $
						chisurf, xsurf, ysurf, $
						xd, yd, rd, pd, $
						rchi2, $
						conlevel(ii), inside, rderr0, pderr0, $
						conlevel_info, conlevel_xy, $
						conlevel_color=conlevel_color, last=last, init=init, siz=siz, $
						title=title, oplot_x=oplot_x, oplot_y=oplot_y, rsiz=rsiz, ninside=ninside, $
						ps=1
					device, /close
					set_plot, 'X'
				endif
			endif $
			else begin
				chisurf_plot, $
					beam.mm, beam.GDU_locX, beam.GDU_locY, beam.alpha, $
					beam.out, beam.gunid, beam.maxorder, beam.bestord, $
					beam.tri_ok, $
					chisurf, xsurf, ysurf, $
					xd, yd, rd, pd, $
					rchi2, -1.e+31, $
					last=last, init=init, siz=siz, $
					title=title, oplot_x=oplot_x, oplot_y=oplot_y, $
					rsiz=rsiz, coarse=coarse, /edge_error, ps=0

				res = ''
				read, res, prompt = 'Harcopy? [y or n] : '
				if (res eq 'y') then begin
					read, res, prompt = 'File name: '
					set_plot, 'PS'
					device, file=res, /landscape, xs=10, ys=5, /inches, /color
					chisurf_plot, $
						beam.mm, beam.GDU_locX, beam.GDU_locY, beam.alpha, $
						beam.out, beam.gunid, beam.maxorder, beam.bestord, $
						beam.tri_ok, $
						chisurf, xsurf, ysurf, $
						xd, yd, rd, pd, $
						rchi2, -1.e+31, $
						last=last, init=init, siz=siz, $
						title=title, oplot_x=oplot_x, oplot_y=oplot_y, $
						rsiz=rsiz, coarse=coarse, /edge_error, ps=1
					device, /close
					set_plot, 'X'
				endif
			endelse
		endif
	endfor

	return
end
