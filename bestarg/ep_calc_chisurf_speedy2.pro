pro ep_calc_chisurf_speedy2, $
	status, status_renorm, $ ; Output status
	gyrorad, gyroper, flip, $ ; Curved traj. Info, IN
	nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid, IN
	beam, $ ; Beam Info, IN
	$
	ndegfree, nclimb, chi_stat, $ ; OUT
	ndegfree_unpen, nclimb_unpen, $ ; OUT
	$
	chisurf, $ ; Chi2-surface info, OUT
	xd, yd, rd, pd, $ ; Target info, OUT
	rchi2, chi2, rchi2_unpen, $ ; OUT
	rderr_min, pderr_min, $ ; OUT
	$
	chisurf_renorm, $ ; Chi2-surface using fa_error_renorm
	xd_renorm, yd_renorm, rd_renorm, pd_renorm, $
	rchi2_renorm, chi2_renorm, rchi2_renorm_unpen, $
	rderr_min_renorm, pderr_min_renorm, $
	$
	finegrid=finegrid

print, 'ep_calc_chisurf_speedy2 starting'
	common ep_envar_con_cb
	
	status = 0
	status_renorm = 0
	chi_stat = 0

	fa_error = beam.sigfir(*) + (beam.bestord(*)-1)*(0.9*!dtor) * beam.runest_estg(*)/3900.
	
	ntot = long(float(nr)*float(np))
	ntot_v = long(float(nr)*float(np)*float(beam.mm))
	dum_ntot = make_array(ntot,/float,value=1.)
	dum_mm = make_array(beam.mm,/float,value=1.)
	
	;===============================================================
	; Memory requirement considerations:
	; dely, delx, dalp, dely_renorm, dalp0 all have [nr,np,nbeam]
	; Possible memory requirement:
	;    nrmax_finegrid (100m) = 562
	;    npmax_finegrid = 180
	;    nbeam_max = 64 (nominal mode)
	;    5 huge matricies
	;    float type (4-byte)
	;    562*180*64*5*4 = 129.485 Megabytes
	; Default IDL dynamic memory limit:  Somewhere between 130 and 140
	; Megabytes; this is controlled by using the 'limit' command in the
	; csh and setting the datasize to be unlimited, which I did in the csh
	; script ep_run.csh
	;===============================================================

dely = reform(dum_ntot#beam.bestord,nr,np,beam.mm)*reform(reform(ysurf,ntot)#dum_mm,nr,np,beam.mm) - reform(dum_ntot#beam.GDU_locY,nr,np,beam.mm) ; [nr,np,nbeam]
delx = reform(dum_ntot#beam.bestord,nr,np,beam.mm)*reform(reform(xsurf,ntot)#dum_mm,nr,np,beam.mm) - reform(dum_ntot#beam.GDU_locX,nr,np,beam.mm) ; [nr,np,nbeam]
dalp = atan(dely,delx)          ; [nr,np,nbeam]
if (pp_curve_traj eq 1) then begin
    dot = [[reform(reform(xsurf,ntot)#dum_mm,ntot_v)-$
            reform(dum_ntot#beam.GDU_locX,ntot_v)],$
           [reform(reform(ysurf,ntot)#dum_mm,ntot_v)-$
            reform(dum_ntot#beam.GDU_locY,ntot_v)]] ; [ntot_v,2]
    tmag = sqrt(total(dot^2,2))#make_array(2,/float,value=1.) ; [ntot_v,2]
    dot = dot/tmag              ; [ntot_v,2], Unit length
    tmag=0
    ghat = [[cos(reform(dum_ntot#beam.alpha,ntot_v))],$
            [sin(reform(dum_ntot#beam.alpha,ntot_v))]] ; [ntot_v,2], Unit length
    dot = total(dot*ghat,2)     ; [ntot_v], dot>0 towards, dot<0 away
    ghat=0
    dot = reform(dot,nr,np,beam.mm) ; [nr,np,nbeam]

; Put code inline:    dalp = ep_calc_ctraj(dalp,dely,delx,gyrorad,flip,dot)

; Toward beams
    id = where(dot ge 0.)
    if (id(0) ne -1) then begin
        if (flip eq 0) then $
          dalp(id) = dalp(id) - sqrt(delx(id)^2+dely(id)^2)/2./gyrorad else $
          dalp(id) = dalp(id) + sqrt(delx(id)^2+dely(id)^2)/2./gyrorad
    endif

; Away beams
    id = where(dot lt 0.)
    if (id(0) ne -1) then begin
        if (flip eq 0) then $
          dalp(id) = dalp(id) + sqrt(delx(id)^2+dely(id)^2)/2./gyrorad else $
          dalp(id) = dalp(id) - sqrt(delx(id)^2+dely(id)^2)/2./gyrorad
    endif

    id = 0                      ; de-allocate memory
    dot = 0

endif

; Differences between angles of possible targets and of measured beam
dalp = abs(dalp-reform(dum_ntot#beam.alpha,nr,np,beam.mm))
delx = sqrt(delx^2+dely^2)      ; distances from gun to possible targets

; intrinsic errors of firing angles
dely = reform(dum_ntot#fa_error,nr,np,beam.mm)^2 + atan(beam.siggdu,delx)^2
dely_renorm = reform(dum_ntot#beam.fa_error_renorm,nr,np,beam.mm)^2 + $
  atan(beam.siggdu,delx)^2

; Deallocate memory
delx = 0

; Force angle difference to be between 0 and 90 degrees
dalp = dalp*(dalp lt !pi)+(2*!pi-dalp)*(dalp ge !pi)
dalp0 = dalp*(dalp lt !pi/2)+(!pi-dalp)*(dalp ge !pi/2)

; Diagnostics Oct. 2009
;diag_out = 1
;if (diag_out) then $
;  save, file='chisurf_components.idlsav', gyrorad, gyroper, flip, $
;  nr, np, rsurf, psurf, xsurf, ysurf, resop, beam, $
;  dalp0, dely, dely_renorm

; difference over error
dalp = dalp0^2 / dely
dalp0 = dalp0^2 / dely_renorm

; Deallocate memory
dely = 0
dely_renorm = 0

igood = where(beam.tri_ok eq 1)
farr = total(dalp(*,*,igood),3)
farr_renorm = total(dalp0(*,*,igood),3)
nsum_f = n_elements(igood)
penalty_sum = total(beam.runest_penalty(igood))

;===================================================================
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
pderr_min = 2.*resop ; Doubled this because we were having "target on edge of fine grid" problems
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
pderr_min_renorm = 2.*resop     ; Doubled this because we were having "target on edge of fine grid" problems
chi2_renorm = chisurfmin_renorm
rchi2_renorm_unpen = chi2_renorm/float(ndegfree_unpen)
if (ndegfree le 0) then rchi2_renorm = pp_rfill else $
  rchi2_renorm = chi2_renorm/float(ndegfree)
status_renorm = 1

return
end
