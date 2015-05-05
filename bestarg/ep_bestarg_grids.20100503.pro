;	ep_bestarg_grids.pro,v 1.8 2002/02/19 14:26:56 ppq Exp	
; pro ep_bestarg_grids, nr, np, rarr, parr, resop, siz, $ ; OUT
                      xarr, yarr, $ ; OUT
                      coarse=coarse, fine=fine, $ ; IN
                      rmax=rmax_in, $ ; IN
                      pd=pd_in, err_pd=pderr_in, $ ; IN
                      rd=rd_in, err_rd=rderr_in ; IN

common ep_envar_con_cb

if not keyword_set(fine) then begin
;===========================================================
;====================COARSE GRID============================
;===========================================================
    resop = pp_cg_phires_rad    ; Resolution of phi-grid
    pcen = 0.                   ; Center of phi-grid
    pwid = 2.*!pi               ; Width of phi-grid 
    
    resmin = pp_cg_minresr_m    ; Minimal radial resolution (linear portion)
    logp = pp_cg_logpr_f        ; (Bin width)/(Bin center) for the
                                ;   logarithmic portion of the grid
    rmin = 0.
    rmax = rmax_in
    
endif else begin
;===========================================================
;==================== FINE GRID ============================
;===========================================================

    
    resop = pp_fg_phires_rad    ; Resolution of phi-grid
    pcen = pd_in                ; Center of phi-grid
    pwid = 2.*pderr_in          ; Width of phi-grid
    
    resmin = pp_fg_minresr_m    ; Minimum radial resolution, meters
    logp = pp_fg_logpr_f        ; (Bin width)/(Bin center) for the
                                ;   logarithmic portion of the grid
    rmin = rd_in - rderr_in > 0.
    rmax = rd_in + rderr_in
    
endelse

;==================MISC. OUTPUT===========================
siz = rmax

;==================PHI GRID===============================
np = long(pwid/resop + .5)
pmin = pcen - float(np)/2.*resop
pmax = pcen + float(np)/2.*resop

;==================RADIAL GRID============================
; Cross-over point = point where the logarithmic spacing would equal resmin
fac = (1.+logp/2.)/(1.-logp/2.)
rc = resmin/(fac-1.)

if (rc gt rmin+resmin) then begin

; Linear portion of grid necessary
    nbin = long( (rc-rmin)/resmin ) + 1
    rc = rmin + nbin*resmin     ; Adjusted rc lies on a bin boundary
    rlin = findgen(nbin+1)/float(nbin)*(rc-rmin) + rmin
    rlin = rlin(0:nbin-1)
    
; Logarithmic portion of grid
    rlog = [rc]
    rr = rc
    while (rr le rmax) do begin
        rlog = [rlog,fac*rr]
        rr = fac*rr
    endwhile

; Concatonate the two portions of the grid
    rarr = [rlin,rlog]
    
endif else begin                ; Linear portion of grid not necessary
    rlog = [rmin]
    rr = rmin
    while (rr le rmax) do begin
        rlog = [rlog,fac*rr]
        rr = fac*rr
    endwhile
    rarr = rlog
endelse
nr = n_elements(rarr)


;==================DEFINE GRIDS===========================
rarr = rarr#make_array(np,/float,value=1.)

parr= resop * (findgen(nr*np) mod np) + (pmin+resop/2.)
parr= reform(rotate(reform(parr,np,nr),1),nr*np)	
parr= reform(parr,nr,np)        ;azimuth samples

xarr = rarr * cos(parr)
yarr = rarr * sin(parr)

return
end
