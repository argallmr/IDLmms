function mms_test_q0_pa
	compile_opt idl2
;	on_error, 2
	
	;File names to use
	q0_file  = '/nfs/edi/temp/mms2_edi_srvy_l2_q0_20151016_v2.0.30.cdf'
	fgm_file = '/nfs/mms2/dfg/srvy/l2pre/2015/10/mms2_dfg_srvy_l2pre_20151016_v3.14.0.cdf'

	;Read data
	q0  = mms_edi_q0_l2_read(q0_file)
	fgm = mms_fg_read_l2pre(fgm_file)

	;Convert to radians
	q0.traj_dbcs_gdu1 *= !dtor
	q0.traj_dbcs_gdu2 *= !dtor
	q0.traj_gse_gdu1 *= !dtor
	q0.traj_gse_gdu2 *= !dtor
	q0.traj_gsm_gdu1 *= !dtor
	q0.traj_gsm_gdu2 *= !dtor

	;Allocate memory to Trajectory unit vectors
	traj_gse_gdu1 = fltarr(3, n_elements(q0.tt2000_gdu1))
	traj_gse_gdu2 = fltarr(3, n_elements(q0.tt2000_gdu2))
	traj_dbcs_gdu1 = traj_gse_gdu1
	traj_dbcs_gdu2 = traj_gse_gdu2
	traj_gsm_gdu1 = traj_gse_gdu1
	traj_gsm_gdu2 = traj_gse_gdu2
	
	;Trajectory unit vectors
	traj_dbcs_gdu1[0,*] = sin(q0.traj_dbcs_gdu1[1,*]) * cos(q0.traj_dbcs_gdu1[0,*])
	traj_dbcs_gdu1[1,*] = sin(q0.traj_dbcs_gdu1[1,*]) * sin(q0.traj_dbcs_gdu1[0,*])
	traj_dbcs_gdu1[2,*] = cos(q0.traj_dbcs_gdu1[1,*])
	
	traj_dbcs_gdu2[0,*] = sin(q0.traj_dbcs_gdu2[1,*]) * cos(q0.traj_dbcs_gdu2[0,*])
	traj_dbcs_gdu2[1,*] = sin(q0.traj_dbcs_gdu2[1,*]) * sin(q0.traj_dbcs_gdu2[0,*])
	traj_dbcs_gdu2[2,*] = cos(q0.traj_dbcs_gdu2[1,*])
	
	traj_gse_gdu1[0,*] = sin(q0.traj_gse_gdu1[1,*]) * cos(q0.traj_gse_gdu1[0,*])
	traj_gse_gdu1[1,*] = sin(q0.traj_gse_gdu1[1,*]) * sin(q0.traj_gse_gdu1[0,*])
	traj_gse_gdu1[2,*] = cos(q0.traj_gse_gdu1[1,*])
	
	traj_gse_gdu2[0,*] = sin(q0.traj_gse_gdu2[1,*]) * cos(q0.traj_gse_gdu2[0,*])
	traj_gse_gdu2[1,*] = sin(q0.traj_gse_gdu2[1,*]) * sin(q0.traj_gse_gdu2[0,*])
	traj_gse_gdu2[2,*] = cos(q0.traj_gse_gdu2[1,*])
	
	traj_gsm_gdu1[0,*] = sin(q0.traj_gsm_gdu1[1,*]) * cos(q0.traj_gsm_gdu1[0,*])
	traj_gsm_gdu1[1,*] = sin(q0.traj_gsm_gdu1[1,*]) * sin(q0.traj_gsm_gdu1[0,*])
	traj_gsm_gdu1[2,*] = cos(q0.traj_gsm_gdu1[1,*])
	
	traj_gsm_gdu2[0,*] = sin(q0.traj_gsm_gdu2[1,*]) * cos(q0.traj_gsm_gdu2[0,*])
	traj_gsm_gdu2[1,*] = sin(q0.traj_gsm_gdu2[1,*]) * sin(q0.traj_gsm_gdu2[0,*])
	traj_gsm_gdu2[2,*] = cos(q0.traj_gsm_gdu2[1,*])

	;Time to seconds
	t_fgm_ssm  = MrCDF_epoch2ssm(fgm.tt2000)
	t_gdu1_ssm = MrCDF_epoch2ssm(q0.tt2000_gdu1)
	t_gdu2_ssm = MrCDF_epoch2ssm(q0.tt2000_gdu2)
	
	;Interpolate B onto Q0 times
	b_dbcs_gdu1 = MrInterpol(fgm.b_dmpa[0:2,*], t_fgm_ssm, t_gdu1_ssm)
	b_dbcs_gdu2 = MrInterpol(fgm.b_dmpa[0:2,*], t_fgm_ssm, t_gdu2_ssm)
	b_gse_gdu1 = MrInterpol(fgm.b_gse[0:2,*], t_fgm_ssm, t_gdu1_ssm)
	b_gse_gdu2 = MrInterpol(fgm.b_gse[0:2,*], t_fgm_ssm, t_gdu2_ssm)
	b_gsm_gdu1 = MrInterpol(fgm.b_gsm[0:2,*], t_fgm_ssm, t_gdu1_ssm)
	b_gsm_gdu2 = MrInterpol(fgm.b_gsm[0:2,*], t_fgm_ssm, t_gdu2_ssm)
	
	;Magnetic field unit vectors
	b_dmpa_gdu1 = MrVector_Normalize(b_dbcs_gdu1[0:2,*])
	b_dmpa_gdu2 = MrVector_Normalize(b_dbcs_gdu2[0:2,*])
	b_gse_gdu1  = MrVector_Normalize(b_gse_gdu1[0:2,*])
	b_gse_gdu2  = MrVector_Normalize(b_gse_gdu2[0:2,*])
	b_gsm_gdu1  = MrVector_Normalize(b_gsm_gdu1[0:2,*])
	b_gsm_gdu2  = MrVector_Normalize(b_gsm_gdu2[0:2,*])

	;Compute pitch angle
	pa_dbcs_gdu1 = acos( MrVector_Dot(traj_dbcs_gdu1, b_dbcs_gdu1) )
	pa_dbcs_gdu2 = acos( MrVector_Dot(traj_dbcs_gdu2, b_dbcs_gdu2) )
	pa_gse_gdu1 = acos( MrVector_Dot(traj_gse_gdu1, b_gse_gdu1) )
	pa_gse_gdu2 = acos( MrVector_Dot(traj_gse_gdu2, b_gse_gdu2) )
	pa_gsm_gdu1 = acos( MrVector_Dot(traj_gsm_gdu1, b_gsm_gdu1) )
	pa_gsm_gdu2 = acos( MrVector_Dot(traj_gsm_gdu2, b_gsm_gdu2) )

	print, 'Time', 'gx', 'gy', 'gz', 'phi', 'theta', FORMAT='(a-12, 3x, 5(a-9, 2x))'
	for i = 0, 4 do begin
		print, FORMAT='(a12, 1x, 3(2x, f9.6), 1x, 2(2x, f9.4))', $
		       strmid(MrCDF_Epoch_Encode(q0.tt2000_gdu1[i]), 12, 12), $
		       traj_gse_gdu1[*,i], q0.traj_gse_gdu1[*,i]*!radeg
	endfor
	
	;window
	win = MrWindow(YGAP=0.5, REFRESH=0)
	
	;Plot
	p1 = MrPlot(t_gdu1_ssm, pa_gse_gdu1*!radeg, $
	            /CURRENT, $
	            TITLE       = 'EDI Pitch Angles GDU1 & GDU2 in GSE', $
	            XTICKFORMAT = '(a1)', $
	            YTITLE      = 'PA!c(deg)')
	p2 = MrPlot(t_gdu2_ssm, pa_gse_gdu2*!radeg, $
	            /CURRENT, $
	            XTICKFORMAT = 'time_labels', $
	            YTITLE = 'PA!c(deg)')
	
	win -> Refresh
	return, win
end