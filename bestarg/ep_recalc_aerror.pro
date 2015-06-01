pro ep_recalc_aerror, $
	target_mag, target_phi, $
	used_beam, bestord_beam, xg_beam, yg_beam, alpha_beam, $
	aerror    ; OUT

	yd = target_mag * sin(target_phi*!dtor)
	xd = target_mag * cos(target_phi*!dtor)

	id = where(used_beam eq 1)
	order = bestord_beam(id)
	GDU_locY = yg_beam(id)
	GDU_locX = xg_beam(id)
	alpha = alpha_beam(id)          ; radians

	dely = order*yd - GDU_locY
	delx = order*xd - GDU_locX
	dalp = atan (dely,delx)
	dalp = abs (dalp - alpha)

	; Must check direction (given that we might have away beams)
	dalp = dalp * (dalp lt !pi)   + (2*!pi-dalp) * (dalp ge !pi)
	dalp = dalp * (dalp lt !pi/2) + (!pi-dalp)   * (dalp ge !pi/2)

	aerror = stddev(dalp)*!radeg

	return
end
