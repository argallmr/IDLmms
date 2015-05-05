; pro mms_bestarg_wrapper_0100

; .compile bestarg

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

bestarg_mms_local

end
