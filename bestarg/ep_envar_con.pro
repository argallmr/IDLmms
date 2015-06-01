pro ep_envar_con

common ep_envar_con_cb, $
	pp_eV2joule, pp_eV2erg, pp_emass_mks, pp_deg2rad, $
	pp_clight_cgs, pp_svpcm2mvpm, pp_nt2gauss, pp_cm2m, pp_p2f, pp_echarge_mks, $
	pp_rfill, pp_bfill, pp_i4fill, pp_i2fill, $
	$
	pp_phase0, pp_sc_base, pp_eindex2keV, pp_ptime_atime_offset, $
	pp_hour_delta, pp_checkmsg_array, pp_tri_ang_thresh_1, pp_tri_ang_thresh_2, $
	pp_tri_ang_thresh_3, pp_tof_tdiff_thresh, pp_rmax_con, $
	$
	pp_nvar, pp_varnames, pp_varformat, pp_vartypes, pp_varvalues, $
	$
	$                             ; ADDING A NEW CONTROL PARAMETER?  HERE!
	pp_nchunk, pp_method, pp_curve_traj, pp_order_assign, pp_acfir_assign, $
	pp_acfir, pp_acfir_rad, pp_gunid, pp_nbeam_min, pp_qual_min, $
	pp_maxorder, pp_maxchan_min, $
	pp_maxchan_max, pp_outlier_removal, pp_outlier_maxperc, $
	pp_outlier_nbeam_min, pp_cg_phires, pp_cg_phires_rad, $
	pp_fg_phires, pp_fg_phires_rad, pp_cg_minresr, pp_cg_minresr_m, $
	pp_fg_minresr, pp_fg_minresr_m, pp_cg_acgdu, pp_cg_acgdu_m, $
	pp_fg_acgdu, pp_fg_acgdu_m, pp_cg_logpr, pp_cg_logpr_f, $
	pp_fg_logpr, pp_fg_logpr_f, $
	pp_actof_con, pp_acgdu, pp_acgdu_m, $
	pp_order_use_unrun1, pp_order_use_unrun2, pp_order_use_hrb, $
	pp_maxorder_runest, pp_para_anglim, pp_para_anglim_rad, $
	pp_dtof_ambiguity_factor, $
	pp_qstat_minperc, pp_tofclass_nbeam_min, pp_rchi2_abortlim, $
	pp_rchi2_outlim, pp_bmag_toflim, pp_reorder_anglim, pp_reorder_anglim_rad, $
	pp_noclassc_pmt, pp_pmt_dtof_error, pp_noclassc_tri, pp_runest_sfac, $
	pp_dt_smt, pp_dt_smt_sec, pp_npair_min_smt, pp_smt_dtof_error, pp_rmax, $
	pp_noclassc_rmt, pp_rmt_dtof_error, pp_runest_smooth, pp_bmag_trilim, $
	pp_ediefw_out, pp_runner_penalty_meth, $
	pp_qual_assign_meth, pp_qa_nbeam_min, pp_qa_drat_cb, pp_qa_drat_gc, $
	pp_qa_dlim, pp_qa_dlim_m, pp_qa_deld_cb, pp_qa_deld_cb_m, $
	pp_qa_deld_gc, pp_qa_deld_gc_m, pp_edidsi_out, pp_edidsi_inertial_out, $
	$
	$                             ; ADDING A NEW CONTROL PARAMETER?  HERE TOO!
	pp_nchunk_def, pp_method_def, pp_curve_traj_def, $
	pp_order_assign_def, pp_acfir_assign_def, $
	pp_acfir_def, pp_gunid_def, pp_nbeam_min_def, $
	pp_qual_min_def, pp_maxorder_def, pp_maxchan_min_def, $
	pp_maxchan_max_def, pp_outlier_removal_def, pp_outlier_maxperc_def, $
	pp_outlier_nbeam_min_def, pp_cg_phires_def, pp_fg_phires_def, $
	pp_cg_minresr_def, $
	pp_fg_minresr_def, pp_cg_acgdu_def, pp_fg_acgdu_def, $
	pp_cg_logpr_def, pp_fg_logpr_def, $
	pp_actof_con_def, pp_acgdu_def, $
	pp_order_use_unrun1_def, pp_order_use_unrun2_def, pp_order_use_hrb_def, $
	pp_maxorder_runest_def, pp_para_anglim_def, pp_dtof_ambiguity_factor_def, $
	pp_qstat_minperc_def, pp_tofclass_nbeam_min_def, pp_rchi2_abortlim_def, $
	pp_rchi2_outlim_def, pp_bmag_toflim_def, pp_reorder_anglim_def, $
	pp_noclassc_pmt_def, pp_pmt_dtof_error_def, pp_noclassc_tri_def, $
	pp_runest_sfac_def, $
	pp_dt_smt_def, pp_npair_min_smt_def, pp_smt_dtof_error_def, pp_rmax_def, $
	pp_noclassc_rmt_def, pp_rmt_dtof_error_def, pp_runest_smooth_def, $
	pp_bmag_trilim_def, pp_ediefw_out_def, pp_runner_penalty_meth_def, $
	pp_qual_assign_meth_def, pp_qa_nbeam_min_def, pp_qa_drat_cb_def, $
	pp_qa_drat_gc_def, $
	pp_qa_dlim_def, pp_qa_deld_cb_def, pp_qa_deld_gc_def, $
	pp_edidsi_out_def, pp_edidsi_inertial_out_def, $
	$
	pp_spinstart_angular_offset, pp_att_orb_hpath, $
	pp_rcs_snapshot_name, $
	$
	pp_edi_piso_fgm_out, pp_edi_piso_fgm_save, $
	pp_edi_piso_fgm_root, pp_spinfgm_file, pp_hrfgm_file, $
	$
	pp_edi_piso_att_orb_path, $
	pp_gdc_rdda_root, $
	pp_gdc_rdda_root2, $
	pp_edi_piso_bash, pp_ep_code_path, pp_ep_tempdata_path, $
	pp_write_aux, pp_dmdef_file, pp_qstat_file, $
	pp_keyplot3, pp_keyplot4, pp_long_iffname, pp_long_ppplusname, $
	$
	pp_pstat, $
	pp_edi_cdfbin_path, pp_write_cdf, $
	$
	pp_iff_hourly

	; Define the pstat array used for book-keeping the successes
	; and failures of the various analysis methods
	; Be sure to update plot_bestarg.pro if you add a new stat message!

	pp_pstat = [ $
		'TRI Success', $    ; 0 RESERVED FOR SUCCESS ONLY!
		'PMT Success', $    ; 1 RESERVED FOR SUCCESS ONLY!
		'SMT Success', $    ; 2 RESERVED FOR SUCCESS ONLY!
		'RMT Success', $    ; 3 RESERVED FOR SUCCESS ONLY!
		'', $               ; 4 RESERVED FOR SUCCESS ONLY!
		'', $               ; 5 RESERVED FOR SUCCESS ONLY!
		'', $               ; 6 RESERVED FOR SUCCESS ONLY!
		'', $               ; 7 RESERVED FOR SUCCESS ONLY!
		'', $               ; 8 RESERVED FOR SUCCESS ONLY!
		'', $               ; 9 RESERVED FOR SUCCESS ONLY!
		$
		'TRI Failure: Coarse grid target on grid edge', $ ; 10
		'TRI Failure: Coarse grid rchi2 > pp_rchi2_abortlim', $ ; 11
		'TRI Failure: Fine grid target on grid edge', $ ; 12
		'TRI Failure: Fine grid rchi2 > pp_rchi2_outlim', $ ; 13
		'TRI/RMT Failure: Not enough beams in each to/away class for RMT, too parallel for TRI', $ ; 14
		'TRI Failure: Bmag lt pp_bmag_trilim', $ ; 15
		'TRI Failure: N_degrees_freedom <= 0', $ ; 16
		'TRI/RMT Failure: Not enough beams to start with for RMT; beams too parallel for TRI', $ ; 17
		'', $               ; 18
		'', $               ; 19
		$
		'PMT Failure: Not enough beams to start with', $ ; 20
		'PMT Failure: TofError.pro failure', $ ; 21
		'PMT Failure: Early return, only SMT desired', $ ; 22
		'', $               ; 23
		'', $               ; 24
		'RMT Failure: Not enough beams to start with', $ ; 25
		'RMT Failure: Not enough beams in each to/away class', $ ; 26
		'RMT Failure: TofError.pro failure', $ ; 27
		'RMT Failure: Towards/Away Indexing Problem', $ ; 28
		'', $               ; 29
		$
		'PMT/SMT Failure: Not enough beams to start with', $ ; 30
		'PMT/SMT Failure: Not enough beams in each to/away class', $ ; 31
		'PMT/SMT Failure: Both attempted, both failed', $ ; 32
		'', $               ; 33
		'', $               ; 34
		'', $               ; 35
		'', $               ; 36
		'', $               ; 37
		'', $               ; 38
		'', $               ; 39
		$
		'SMT Failure: Not enough beams to start with', $ ; 40
		'SMT Failure: Not enough simultaneous pairs', $ ; 41
		'SMT Failure: TofError.pro failure', $ ; 42
		'', $               ; 43
		'', $               ; 44
		'', $               ; 45
		'', $               ; 46
		'', $               ; 47
		'', $               ; 48
		'', $               ; 49
		$
		'Generic Failure: Not enough beams after order/class prep', $ ; 50
		'Generic Failure: Not enough beams after outlier removal', $ ; 51
		'Generic Failure: Value of pp_method not valid', $ ; 52
		'Generic Failure: Mean/Stdev Angle=NaN Error', $ ; 53
		'Generic Failure: Not enough beams after tri_ok assessment', $ ; 54
		'', $               ; 55
		'', $               ; 56
		'', $               ; 57
		'', $               ; 58
		'']                 ; 59
	; Key plot switches
	pp_keyplot3 = long(getenv('EDI_PISO_KEYPLOT3'))
	pp_keyplot4 = long(getenv('EDI_PISO_KEYPLOT4'))

	; Get some of the environment variables that the routines
	; ep_get_spinfgm.pro and ep_get_hrfgm_oh.pro will need.
	pp_edi_piso_fgm_out = getenv('EDI_PISO_FGM_OUT')
	pp_edi_piso_fgm_save = long(getenv('EDI_PISO_FGM_SAVE'))
	pp_iff_hourly = long(getenv('EDI_PISO_IFF_HOURLY'))
	pp_edi_piso_att_orb_path = getenv('EDI_PISO_ATT_ORB_TPATH')
	pp_gdc_rdda_root = getenv('GDC_RDDA_ROOT')
	pp_gdc_rdda_root2 = getenv('GDC_RDDA_ROOT2')
	pp_edi_piso_fgm_root = getenv('EDI_PISO_FGM_ROOT')
	pp_edi_piso_bash = getenv('EDI_PISO_BASH')
	pp_ep_code_path = getenv('EP_CODE_PATH')
	pp_ep_tempdata_path = getenv('EP_TEMPDATA_PATH')

	; Set whether or not to write the aux file
	pp_write_aux = long(getenv('EDI_PISO_WRITE_AUX'))
	pp_long_iffname = long(getenv('EDI_PISO_LONG_IFFNAME'))
	pp_long_ppplusname = long(getenv('EDI_PISO_LONG_PPPLUSNAME'))

	pp_write_cdf = long(getenv('EDI_PISO_WRITE_CDF'))
	pp_edi_cdfbin_path = getenv('EDI_PISO_CDFBIN_PATH')

	pp_eV2joule = 1.60219e-19       ; Electron volts to joules, multiplicative
	pp_eV2erg = 1.60219e-12         ; Electron volts to ergs, multiplicative
	pp_emass_mks = 9.10953e-31      ; Electron mass, kg
	pp_echarge_mks = 1.6e-19        ; Electron charge, Coulombs
	pp_deg2rad = !pi/180.           ; Degrees to radians, multiplicative
	pp_clight_cgs = 3.e+10
	pp_svpcm2mvpm = 3.e+4*1.e+3     ; statvolt/cm to mV/m, multiplicative
	pp_nt2gauss = 1e-9*1e+4         ; nanotesla to gauss, multiplicative
	pp_cm2m = 1e-2
	pp_p2f = 1e-2

	pp_rfill = -1.e+31
	pp_bfill = 255
	pp_i4fill = 2147483647          ; 2^31 - 1
	pp_i2fill = 32767

	;eqs phase0 = 135.d0*!dpi/180.d0     ; 135 deg angle from GDU1 to
	;eqs                                 ; the See-Sun axis (SSa),
	;eqs                                 ; measured counterclockwise in the
	;eqs                                 ; spin plane (i.e. in the direction of
	;eqs                                 ; the spacecraft rotation) for EQS

	; 296.2 deg angle from GDU1 to the See-Sun axis (SSa),
	; measured counterclockwise in the spin plane 
	; (i.e. in the direction of the spacecraft rotation) for Cluster
	pp_phase0 = 296.2d0*!dpi/180.d0 

	; Gun/Det. spacing of Cluster (approx)
	; Note that this ISN'T the radius of Cluster; it's the diameter. We use this just
	; like you would a radius when we construct the gun positions
	; because the triangulation software works with a virtual spacecraft
	; which is twice as big as Cluster.
	pp_sc_base = 3.20d0             

	; Energy in keV of the e_index=0 and e_index=1 energy levels
	pp_eindex2keV = [1.d0, 0.5d0]    

	;============== PID DEFINITIONS (FYI) ====================
	;pid = 1L                        ; EDI housekeeping data
	;pid = 10L                       ; ALL Nominal Mode 1 EDI data
	;                                ; (i.e. S/C modes 11,12,13 or 22)
	;pid = 21L                       ; Burst Mode 1 EDI data
	;pid = 23L                       ; Burst Mode 3 EDI data
	;pid = 99L                       ; S/C housekeeping data
	;=========================================================

	; Expected value of packet_time - atime(1st subpacket of packet):
	;         ptime - atime ~ 5.2s
	pp_ptime_atime_offset = 5.2d0   ; Seconds
	pp_hour_delta = 2.d0*pp_ptime_atime_offset ; Seconds, used to buffer
										              ; the hour time range

	pp_checkmsg_array = [ $
		'Valid Science Mode', $
		'Science mode not equal to 5', $
		'Sub-mode inconsistent with TM-mode', $
		'RAM dump', $
		'sn.pacmoN.data_count = 0 (not expected)', $
		'NNN/COTYPE/SAMTYPE inconsistent', $
		'7th bit of SOB inconsistent with e-switching', $
		'Packet/sub-packet time anomoly', $
		'SRT = 0', $
		'TSPIN is unreasonable', $
		'NM1, PACMO1 not yet handled by PICK/EDI_PISO']

	;===============================================================
	; Working and default environment variables set in 'ep_run.csh':

	pp_dmdef_file = 'ep_'+getenv('EDI_PISO_DM')+'_defaults.txt'
	ep_extract_envar_info, pp_nvar, pp_varnames, pp_varformat, pp_vartypes, pp_varvalues, pp_dmdef_file, path=getenv('EP_CODE_PATH')

	for i=0,pp_nvar-1 do begin
		var = pp_varnames(i)
		type = pp_vartypes(i)
		vardef = pp_varnames(i)+'_def'
	
		istat = execute(var+" = "+type+"(getenv('"+var+"'))")
		istat = execute(vardef+" = "+type+"(getenv('"+vardef+"'))")

		; Check that the environment variables haven't been corrupted en route to this routine...
		istat = execute("if ("+vardef+" ne "+type+"("+pp_varvalues(i)+")) then message, 'ep_envar_con: Something wrong with env vars.'")
	endfor

	; Special units
	pp_acfir_rad = pp_acfir*pp_deg2rad ; Radians
	pp_cg_phires_rad = pp_cg_phires*pp_deg2rad ; Radians
	pp_fg_phires_rad = pp_fg_phires*pp_deg2rad ; Radians
	pp_cg_minresr_m = pp_cg_minresr*pp_cm2m ; Meters
	pp_fg_minresr_m = pp_fg_minresr*pp_cm2m ; Meters
	pp_cg_acgdu_m = pp_cg_acgdu*pp_cm2m ; Meters
	pp_fg_acgdu_m = pp_fg_acgdu*pp_cm2m ; Meters
	pp_cg_logpr_f = pp_cg_logpr*pp_p2f ; Fraction
	pp_fg_logpr_f = pp_fg_logpr*pp_p2f ; Fraction
	pp_acgdu_m = pp_acgdu*pp_cm2m   ; Meters
	pp_para_anglim_rad = pp_para_anglim*pp_deg2rad ; Radians
	pp_reorder_anglim_rad = pp_reorder_anglim*pp_deg2rad ; Radians
	pp_dt_smt_sec = pp_dt_smt/1e3   ; Seconds
	pp_qa_dlim_m = pp_qa_dlim*pp_cm2m ; Meters
	pp_qa_deld_cb_m = pp_qa_deld_cb*pp_cm2m ; Meters
	pp_qa_deld_gc_m = pp_qa_deld_gc*pp_cm2m ; Meters

	;========IMPORTANT NOTE!!! IMPORTANT NOTE!!! IMPORTANT NOTE!!!======
	; The following parameters are used in whatech.pro.
	; PLEASE note that there are parameters in bestarg.pro
	; THAT MIGHT NEED TO CHANGE IF THE PARAMETERS BELOW ARE CHANGED!!!
	; I didn't have time to completely 'pp_control'-ize the whatech.pro.
	; So, DON'T CHANGE THE PARAMETERS BELOW UNLESS YOU FULLY UNDERSTAND
	; ALL OF THE OTHER PARAMETERS STILL HARDWIRED IN THE BESTARG CODE!!!
	; PPQ, 14-Feb-2001, (Happy Valentine's Day).
	;===================================================================

	; At least one beam pair must have an angle difference exceeding pp_tri_ang_thresh_1 in order
	; for triangulation to be well-behaved.  This ensures that, at least for this pair,
	; the triangulation point has a relative error in distance of 1/3.
	pp_tri_ang_thresh_1 = 4.2 * pp_acfir_rad ; radians

	; In addition to the condition stated above, the average angle difference of all
	; of the beams must exceed pp_tri_ang_thresh_2 in order for triangulation to be
	; well-behaved.
	pp_tri_ang_thresh_2 = 2.1*pp_acfir_rad ; radians

	; If the average angle difference of all of the beams exceeds pp_tri_ang_thresh_3,
	; and both ToF and Tri are well-behaved, then pure triangulation is preferred.
	pp_tri_ang_thresh_3 = 6.5*pp_acfir_rad ; radians

	; If the average ToF difference of all of the beam pairs exceeds pp_tof_tdiff_thresh * actof,
	; then ToF analysis is considered well-behaved.
	pp_tof_tdiff_thresh = sqrt (2.)

	pp_rmax_con = 3                 ; Used by whatech to determine appropriate grid size.
	;===================================================================

	; ========== OFFICIAL SPIN-START OFFSET ===========
	; Extract the official spin-start angular offset from the environment
	; variable set by the GCDC script that runs this EDI_PISO software.
	pp_spinstart_angular_offset = float(getenv('EDI_PISO_OFFSET')) ; Degrees

	; ========== ATT/ORB history file path for the SCS-->GSE ==========
	; ========== transformation                              ==========
	pp_att_orb_hpath = getenv('EDI_PISO_ATT_ORB_TPATH')

	; THIS currently doesn't work on the mpecl:  rcs_snapshot_name = 'v3_01'
	pp_rcs_snapshot_name = get_rcs_snapshot_name (path=getenv('EP_CODE_PATH'))

	; Check
; 	for i=0, pp_nvar-1 do begin
; 		istat = execute ('print, ' +pp_varnames(i) +',' +pp_varnames(i) +'_def')
; 	endfor
; 	print, pp_acfir_rad
; 	print, pp_cg_phires_rad
; 	print, pp_fg_phires_rad
; 	print, pp_cg_minresr_m
; 	print, pp_fg_minresr_m
; 	print, pp_cg_acgdu_m
; 	print, pp_fg_acgdu_m
; 	print, pp_cg_logpr_f
; 	print, pp_fg_logpr_f
; 	print, pp_acgdu_m
; 	print, pp_para_anglim_rad
; 	print, pp_reorder_anglim_rad
; 	print, pp_dt_smt_sec
; 	print, pp_qa_dlim_m
; 	print, pp_qa_deld_cb_m
; 	print, pp_qa_deld_gc_m

	return
end
