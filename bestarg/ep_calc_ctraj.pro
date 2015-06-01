function ep_calc_ctraj, $
	phi, dy, dx, gyrorad, flip, dot

	; phi, dy, dx and dot must all have the same dimensions
	; phi = array of angles to possible targets assuming straight-line trajectories
	;     = atan (dy,dx)
	; dy  = target_y - gun_y (meters)
	; dx  = target_x - gun_x (meters)
	; gyrorad = gyroradius in meters
	; flip = 0 for B coming OUT of the B-perp plane
	;        (xhat_bpp cross yhat_bpp = Bhat)
	;      = 1 for B going INTO the B-perp plane
	;        (xhat_bpp cross yhat_bpp = -Bhat)
	; dot = dot product between the possible target unit vectors (as seen
	; from the gun) and the gun firing direction unit vectors.

	; Toward beams
	id = where (dot ge 0.)
	if (id(0) ne -1) then begin
		if (flip eq 0) then $
			phi(id) = phi(id) - sqrt (dx(id)^2 + dy(id)^2) /2. /gyrorad $
		else $
			phi(id) = phi(id) + sqrt (dx(id)^2 + dy(id)^2) /2. /gyrorad
	endif

	; Away beams
	id = where (dot lt 0.)
	if (id(0) ne -1) then begin
		if (flip eq 0) then $
			phi(id) = phi(id) + sqrt (dx(id)^2 + dy(id)^2) /2. /gyrorad $
		else $
			phi(id) = phi(id) - sqrt (dx(id)^2 + dy(id)^2) /2. /gyrorad
	endif

	return, phi
end
