;	ep_bestarg_grids.pro,v 1.8 2002/02/19 14:26:56 ppq Exp
pro ep_bestarg_grids, $
	nGridCols, nPhiGridRows, radialGrid, phiGrid, phiGridRes, maxRadialOut, $ ; OUT
  xarr, yarr, $ ; OUT
  coarse=coarse, fine=fine, $ ; IN
  rmax=maxRadialIn, $ ; IN
  pd=pd_in, err_pd=pderr_in, $ ; IN
  rd=rd_in, err_rd=rderr_in ; IN

print, 'ep_bestarg_grids starting'
; if n_elements (maxRadialIn) gt 0 then $
; print, 'maxRadialIn ', maxRadialIn

	common ep_envar_con_cb ; see IDL_bestarg_3D_grids_0100 for a test of this procedure

	if not keyword_set (fine) then begin
; print, 'coarse ', coarse
		;====================COARSE GRID============================

		phiGridRes = pp_cg_phires_rad   ; Resolution of coarse phi-grid; 3°
		phiGridCtr = 0.                 ; Center of phi-grid
		phiGridWidth = 2.*!pi           ; Width of phi-grid

		minRadialRes = pp_cg_minresr_m  ; Minimal radial resolution (linear portion); default 5
		logp = pp_cg_logpr_f            ; (Bin width)/(Bin center) for the logarithmic portion of the grid; default 30
		                                ; Not clear what this does, but it is NOT a log yet
		minRadialStep = 0.
		maxRadial = maxRadialIn         ; maxRadial = maximum drift step in meters (floating point); range, meaning uncertain; passed into bestarg
; print, 'phiGridRes, phiGridCtr, phiGridWidth, minRadialRes, logp'
; print,  phiGridRes, phiGridCtr, phiGridWidth, minRadialRes, logp
	endif $

	else begin
; print, 'fine ', fine
		;==================== FINE GRID ============================
		phiGridRes   = pp_fg_phires_rad ; Resolution of fine phi-grid; 1°
		phiGridCtr   = pd_in            ; Center of phi-grid
		phiGridWidth = 2. * pderr_in    ; Width of phi-grid

		minRadialRes = pp_fg_minresr_m  ; Minimum radial resolution, meters
		logp = pp_fg_logpr_f            ; (Bin width)/(Bin center) for the logarithmic portion of the grid; default 1
		                                ; Not clear what this does, but it is NOT a log yet
		minRadialStep = rd_in - rderr_in > 0.    ; ">" is the max() function
		maxRadial = rd_in + rderr_in
	endelse

	;==================MISC. OUTPUT===========================
	maxRadialOut = maxRadial

	;==================PHI GRID===============================
	nPhiGridRows  = long (phiGridWidth / phiGridRes + .5)           ; ex: for a sample run, IDL=120, MATLAB=121
	PhiGridMin = phiGridCtr - float (nPhiGridRows)/2. * phiGridRes  ; ex: -pi
	PhiGridMax = phiGridCtr + float (nPhiGridRows)/2. * phiGridRes  ; why not just -PhiGridMin, UNLESS phiGridCtr is not 0 in the fine grid case

	;==================RADIAL GRID============================
	; Cross-over point = point where the logarithmic spacing would equal minRadialRes
	fac = (1. + logp/2.) / (1. - logp/2.)       ; logp=1, fac=3; logp=30, fac=-1.1429
	radialLinLogXovr = minRadialRes / (fac-1.)  ; -2.3333 for logp=30, fac=-1.1429

; print, 'logp, minRadialRes, radialLinLogXovr, minRadialStep, minRadialRes'
; print,  logp, minRadialRes, radialLinLogXovr, minRadialStep, minRadialRes

if (radialLinLogXovr gt (minRadialStep + minRadialRes) ) then begin
	; Linear portion of radial grid necessary
	nbin = long ( (radialLinLogXovr-minRadialStep) / minRadialRes) + 1
	radialLinLogXovr = minRadialStep + nbin * minRadialRes             ; Adjusted radialLinLogXovr lies on a bin boundary
	linearRadialSteps = findgen(nbin+1) / float(nbin) * (radialLinLogXovr - minRadialStep) + minRadialStep ; creates array
	linearRadialSteps = linearRadialSteps (0: nbin-1)                                   ; trims linearRadialSteps to nbin

	; Logarithmic portion of grid
	logRadialSteps = [radialLinLogXovr]
	rr = radialLinLogXovr
	while (rr le maxRadial) do begin
	  logRadialSteps = [logRadialSteps, fac * rr]
	  rr = fac * rr
	endwhile

	; Concatenate the two portions of the grid
	radialSteps = [linearRadialSteps, logRadialSteps]

	endif else begin                ; Linear portion of grid not necessary
		logRadialSteps = [minRadialStep]
		rr = minRadialStep
		while (rr le maxRadial) do begin
			logRadialSteps = [logRadialSteps, fac * rr]
			rr = fac * rr
		endwhile
		radialSteps = logRadialSteps
	endelse

	nGridCols = n_elements (radialSteps)
; print, 'radialSteps'
; print, radialSteps

	;==================DEFINE GRIDS=========================== ; use rebin instead of outer product?
	; radialGrid - this is 120 row copies of radialSteps, 120 x radialGrid, and should be built that way, not as outer product.
	; note that it may comprise a linear and a log portion
	radialGrid = radialSteps # make_array (nPhiGridRows, /float, value=1.) ; outer product: (radialGrid' * m_a)' OR m_a' * radialGrid

	; phiGrid - this is 120 row copies of ???, 120 x ??, and should be built that way, not as a series of matrix ops.
	; The next line defines the row?
	phiGrid = phiGridRes * (findgen (nGridCols * nPhiGridRows) mod nPhiGridRows) + (PhiGridMin + phiGridRes/2.)
	phiGrid = reform (rotate (reform (phiGrid, nPhiGridRows, nGridCols), 1), nGridCols * nPhiGridRows)
	phiGrid = reform (phiGrid, nGridCols, nPhiGridRows)                    ;azimuth samples

	xarr = radialGrid * cos (phiGrid)
	; 120 x 18 * 120 x 18 => 120 x 18, element-wise multiply
	; possible speed up? each row of phiGrid contains the same value, but here we take the sin of each value
	; after that, it's a matter of multiplying each row of radialGrid by that value. radialGrid row values
	; are unique, so there is no further shortcut available
	yarr = radialGrid * sin (phiGrid)

print, 'nGridCols, nPhiGridRows', nGridCols, nPhiGridRows
print, 'phiGridRes, maxRadialOut', phiGridRes, maxRadialOut

; print, 'radialGrid'
; print, radialGrid (*, 1) ; just the first row, for verification

; print, 'phiGrid'
; print, phiGrid

; print, 'xarr, yarr'
; print, xarr, yarr

;stop
	return
end
