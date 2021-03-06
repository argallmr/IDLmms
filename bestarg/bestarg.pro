;	bestarg.pro,v 1.36 2010/05/03 15:53:33 ppq Exp

pro bestarg, $
	nBeams, beam_gunid, beam_out, beam_xg, beam_yg, $ ; IN
	beam_alpha, beam_tof, beam_code_type, beam_qual, $ ; IN
	beam_bwidth, beam_runstat, $ ; IN
	beam_maxchan, beam_btime, $ ; IN
	beam_tchip, beam_tcode, $
	erg,rmax,gyrorad,gyroper,flip, $ ; IN
	bmag=bmag, $       ; IN
	keyplot3=keyplot3, $ ; IN
	poc=poc, $         ; IN
	plot_title=kp3_title, $ ; IN
	$
	rmax_out=rmax_out, $ ; OUT
	beam_class=beam_class_out, $ ; OUT
	bclass_init=beam_class_init_out, $ ; OUT
	beam_penalty=beam_penalty_out, $ ; OUT
	$
	$                  ; Results Output
	status_out=status_out, $
	msg_out=msg_out, $
	method_out=method_out, $
	edi6_out=edi6_out, $
	ambig_180_out=ambig_180_out, $
	mean_angle_out=mean_angle_out, $
	stdev_angle_out=stdev_angle_out, $
	out_out=out_out, $
	bestord_out=bestord_out, $
	cat=classA_towards, $
	nat=nonA_towards, $
	caa=classA_away, $
	naa=nonA_away, $
	beam_used_out=beam_used_out, $
	perr3_out=perr3_out, $
	$
	$                  ; TRI-specific output
	rchi2_CG=rchi2_CG, chi2_CG=chi2_CG, ndegfree_CG=ndegfree_CG, $
	rchi2_unpen_CG=rchi2_unpen_CG, ndegfree_unpen_CG=ndegfree_unpen_CG, $
	rchi2_FG=rchi2_FG, chi2_FG=chi2_FG, ndegfree_FG=ndegfree_FG, $
	rchi2_unpen_FG=rchi2_unpen_FG, ndegfree_unpen_FG=ndegfree_unpen_FG, $
	derr2=derr2_out, perr2=perr2_out, tgerr2=tgerr2_out, $
	$
	$                  ; Additional Results output for pp_method = 9
	status_meth9 = status_meth9, $
	msg_meth9 = msg_meth9, $
	method_meth9 = method_meth9, $
	edi6_meth9 = edi6_meth9, $
	ambig_180_meth9 = ambig_180_meth9, $
	mean_angle_meth9 = mean_angle_meth9, $
	stdev_angle_meth9 = stdev_angle_meth9, $
	out_meth9 = out_meth9, $
	bestord_meth9 = bestord_meth9, $
	classA_towards_meth9 = classA_towards_meth9, $
	nonA_towards_meth9 = nonA_towards_meth9, $
	classA_away_meth9 = classA_away_meth9, $
	nonA_away_meth9 = nonA_away_meth9, $
	beam_used_meth9 = beam_used_meth9, $
	perr3_meth9 = perr3_meth9

; print, nBeams
; This routine uses a set of EDI beam measurements to compute the best target,
; i.e., the drift vector and gyrotime that provide the best fit to the EDI data.

; **************** Input parameters:
;   beam_gunid = gun ID (1 or 2) for beam k
;   beam_out   = 0       qual = 2,3   Use for TRI and TOF OK
;              = 1       qual = 1     Only use for TRI
;      NOTE: qual=0 beams are not allowed in this code!!!
;            Must filter them out beforehand!!!
;   beam_xg    = xg(k)            = gun X position for beam k in meters
;   beam_yg    = yg(k)            = gun Y position for beam k in meters
;   beam_alpha = alpha(k)*180/!pi = firing angle of beam k in degrees
;   beam_tof   = tof(k)           = time of flight of beam k in microseconds
;   beam_code_type = code_type(k)  =0 for short (correlator) code, 1 for

; ERG = beam energy in keV (floating point, the same for all beams)
; RMAX = maximum drift step in meters (floating point)
; **************** Output parameters:
; EDI6 = fltarr(6) gives the best target and its error
;   edi6(0) = rd           = drift step in meters
;   edi6(1) = derr         = error of drift step (68% confidence)
;   edi6(2) = pd*180/!pi   = drift azimuth in degrees
;   edi6(3) = perr*180/!pi = error of drift azimuth (68% confidence)
;   edi6(4) = tg           = gyrotime in microseconds
;   edi6(5) = tgerr        = error of gyrotime (68% confidence)
; **************** Keywords:
; KEYPLOT switches on plotting.
; POC (Power of Order Correction) shall avoid faked multirunners (default: 2.).
;   The contribution of a beam to chi^2 is multiplied with order^POC.
; ****************

; The data analyzed by BESTARG is a set of nBeams
; fired from gun positions xg,yg at angles, alpha,
; and returning to the spacecraft after times of flight, tof.
; The task of BESTARG is to find the target that gives the best fit to the data.
; A target consists of a 2-dimensional drift vector (in the plane
; perpendicular to the magnetic field) and a gyrotime.
; The drift vector is either displaced in Cartesian coordinates x,y
; or in polar coordinates (drift step, drift azimuth).
; In order to find the best fit, the 3-dimensional target space
; is represented by a 3-dimensional grid. The coordinates of this grid
; are given by the arrays rarr,parr,tarr (drift step, azimuth, gyrotime).
; The coordinates of the best target are called rd,pd,tg.
; For each point of the grid (i.e., each possible target),
; BESTARG computes the likelihood functions farr and garr.

; The contribution of a beam to farr, the likelihood function for triangulation,
; is given by the difference between its firing angle, alpha,
; and the angle dalp from its gun position to the possible drift vector x,y.
; This angular difference dalp-alpha is divided by its intrinsic error
; and squared. This square is called f0.
; In order to get farr, the contributions f0 of all beams must be summed up.
; The summing up is different for the 1st and 2nd iteration of BESTARG.
; In the 1st iteration, which is done with a coarse grid, a more robust fitting
; is done by computing farr as the sum of the terms log (1+0.5*f0) of all beams.
; Robust estimation mea	ns estimation for broad distributions with significant
; numbers of “outlier” points.
; In the 2nd iteration, which is done with a fine grid, farr is computed
; as the sum of the f0 of all beams. This corresponds to usual chi^2 fitting.

; The intrinsic errors of dalp-alpha are assumed to consist of
; an uncertainty of 1 degree in firing direction and
; an uncertainty of 0.05 meters in the positions of gun and detector.
; The angle dalp is not only computed for the possible drift vectors x,y, but
; also for multiples order*(x,y). This gives dalp and f0 as a function of order.
; If f0(order) is lowest for order=bestord then the respective beam
; is interpreted as a multirunner of order=bestord.
; In this case the contribution of the beam is f0(bestord).
; Since the algorithm has a tendency to interpret single runners
; as multirunners, the terms f0(order) are multiplied by order^POC
; before their minimum order=bestord is searched.
; The default value, POC=2., can be justified quasi-theoretically,
; and was also found empirically to be a good choice.
; If EDI is operated with the long correlator code the order of the beams
; will be known a priori.
; The target with the lowest farr is the best triangulation target.

; The errors of the 3 (or 2) components of the best target are computed
; after the 2nd iteration. Using chi^2 statistics, the error bars are given
; by farr+garr -farr(rd,pd,tg)-garr(rd,pd,tg) = chi^2 -chi^2(rd,pd,tg) < 1.

; After the 1st iteration, the intrinsic errors, sigfir (firing direction)
; are replaced by their effective errors derived from the chi^2 values at the best target.

; In the 1st iteration, robust estimation is preferred, because it is
; less sensitive to outliers. Before the chi^2 fitting of the 2nd iteration,
; outliers must be sorted out. Hereby, a measurement is considered as an outlier
; if its difference dalp-alpha evaluated
; at the best target of the robust estimation is large.
; If a beam is a triangulation outlier then it must not be used
; for ToF analysis.

; Further information on BESTARG can be found in the LaTeX document (FITDOC.TEX)
; "Fitting a time series of drift vector and gyrotime to EDI measurements"
;
; draft version for triangulation of single runners only:  J. Semeter 20/06/1999
; debugged version:                                       T. M. Bauer 25/08/1999
; version for multirunners with error analysis:           T. M. Bauer 02/09/1999
; version with correction for faked multirunners:         T. M. Bauer 05/09/1999
; version accounting for uncertainty in gun position:     T. M. Bauer 07/09/1999
; completely revised version including ToF analysis, with two iterations,
;   robust estimation, and improved error analysis:       T. M. Bauer 16/08/2000

; ******************************************************************

common ep_envar_con_cb, $
  ;CONSTANTS & UNIT CONVERSION
  pp_eV2joule,    $            ;Scalar float: convert eV to Joules
  pp_eV2erg,      $            ;Scalar float: convert eV to erg
  pp_emass_mks,   $            ;Scalar float: electron mass in MKS system
  pp_deg2rad,     $            ;Scalar float: convert degrees to radians
  pp_clight_cgs,  $            ;Scalar float: Seed of light in CGS system
  pp_svpcm2mvpm,  $            ;
  pp_nt2gauss,    $            ;Scalar float: convert nT to gauss
  pp_cm2m,        $            ;Scalar float: convert cm to m
  pp_p2f,         $            ;
  pp_echarge_mks, $            ;Scalar float: electron charge in MKS system
  
  ;FILL VALUES
  pp_rfill,       $            ;Scalar float: fill value used below
  pp_bfill,       $            ;Scalar short: fill value used below and in ep_richmans_tof_sa
  pp_i4fill,      $
  pp_i2fill,      $
  $
  pp_phase0, $         ;Angular dispacement (degrees) of sun sensor from X-axis: edi_piso_onechunk
  pp_sc_base, $        ;Diameter of spacecraft (meters): edi_piso_onechunk, edi_piso (whatech)
  pp_eindex2keV, $     ;2x1 float; Beam energy in keV - 0=0.5keV, 1=1.0keV: edi_piso (whatech)
  pp_ptime_atime_offset, $
  pp_hour_delta, pp_checkmsg_array, pp_tri_ang_thresh_1, pp_tri_ang_thresh_2, $
  pp_tri_ang_thresh_3, pp_tof_tdiff_thresh, pp_rmax_con, $
  $
  pp_nvar, pp_varnames, pp_varformat, pp_vartypes, pp_varvalues, $
  $
  $                             ; ADDING A NEW CONTROL PARAMETER?  HERE!
  pp_nchunk, pp_method, pp_curve_traj, pp_order_assign, pp_acfir_assign, $
  pp_acfir, $                   ;Nominal beam uncertainty (1.0 degrees): ep_envarcon_defaults.txt
  pp_acfir_rad, $               ;PP_ACFIR in radians: ep_prep_order.pro
  pp_gunid, $                   ;0=GDU1&2, 1=GDU1, 2=GDU2: edi_piso_onechunk
  pp_nbeam_min, $               ;Minimum number of beams for a single chunk: edi_piso_onechunk
  pp_qual_min, $                ;Minimum beam quality to use: edi_piso_onechunk
  pp_maxorder, $                ;Maximum allowed runner order: ep_prep_order
  pp_maxchan_min, $             ;Minimum MAXCHAN value to use: edi_piso
  pp_maxchan_max, $             ;Maximum MAXCHAN value to use: edi_piso
  pp_outlier_removal, pp_outlier_maxperc, $
  pp_outlier_nbeam_min, pp_cg_phires, pp_cg_phires_rad, $
  pp_fg_phires, pp_fg_phires_rad, pp_cg_minresr, pp_cg_minresr_m, $
  pp_fg_minresr, pp_fg_minresr_m, pp_cg_acgdu, pp_cg_acgdu_m, $
  pp_fg_acgdu, pp_fg_acgdu_m, pp_cg_logpr, pp_cg_logpr_f, $
  pp_fg_logpr, pp_fg_logpr_f, $
  pp_actof_con, pp_acgdu, pp_acgdu_m, $
  pp_order_use_unrun1, pp_order_use_unrun2, pp_order_use_hrb, $
  pp_maxorder_runest, $         ;Maximum allowed runner order: ep_prep_order
  pp_para_anglim, $
  pp_para_anglim_rad, $         ;Maximum angle (radians) still considered parallel: ep_method_logic_rmt_sa
  pp_dtof_ambiguity_factor, $
  pp_qstat_minperc, pp_tofclass_nbeam_min, pp_rchi2_abortlim, $
  pp_rchi2_outlim, pp_bmag_toflim, pp_reorder_anglim, pp_reorder_anglim_rad, $
  pp_noclassc_pmt, pp_pmt_dtof_error, pp_noclassc_tri, pp_runest_sfac, $
  pp_dt_smt, pp_dt_smt_sec, pp_npair_min_smt, pp_smt_dtof_error, pp_rmax, $
  pp_noclassc_rmt, $
  pp_rmt_dtof_error, $         ;Confidence interval to use with ToF results: ep_richmans_tof_sa
  pp_runest_smooth, pp_bmag_trilim, $
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
  pp_qstat_minperc_def, $
  pp_tofclass_nbeam_min_def, $        ;Minimum number of beams from a single gun for which we can still use ToF
  pp_rchi2_abortlim_def, $
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
  pp_pstat, $           ;Pre-defined status messages. 14,17,25,26 Defined in ep_method_logic_rmt_sa
  pp_edi_cdfbin_path, pp_write_cdf, $
  $
  pp_iff_hourly

; 	save, /COMM, /VARIABLES, /SYSTEM_VARIABLES, filename='bestarg_20150223a.sav'
	save, /ALL, filename='bestarg_mms_C3_2001-06-08t053000_054000_@20150306.sav'

; 	restore, ''
; 	restore, /verbose, 'bestarg_mms_C3_2001-06-08t053000_054000.sav'

; stop
;  .compile bestarg, ep_bestarg_grids
; http://michaelgalloy.com/page/2

; 	common bestarg_mms_cb

	keyplot3 = keyword_set(keyplot3)
	if n_elements(poc) eq 0 then $
		poc = 2.

	; Predefined in case of early return before being filled.
	rmax_out            = rmax
	beam_class_out      = make_array(nBeams,/string,value='F') ; 'F'ill
	beam_class_init_out = make_array(nBeams,/string,value='F') ; 'F'ill
	beam_penalty_out    = make_array(nBeams,/float,value=pp_rfill)

	method_out      = 0
	edi6_out        = make_array(6,/float,value=pp_rfill)
	ambig_180_out   = -1
	mean_angle_out  = pp_rfill
	stdev_angle_out = pp_rfill
	out_out         = make_array(nBeams,/int,value=pp_bfill)
	bestord_out     = make_array(nBeams,/int,value=pp_bfill)
	classA_towards  = pp_bfill
	nonA_towards    = pp_bfill
	classA_away     = pp_bfill
	nonA_away       = pp_bfill
	beam_used_out   = make_array(nBeams,/int,value=pp_bfill)
	perr3_out       = pp_rfill

	method_meth9         = 3
	edi6_meth9           = make_array(6,/float,value=pp_rfill)
	ambig_180_meth9      = -1
	mean_angle_meth9     = pp_rfill
	stdev_angle_meth9    = pp_rfill
	out_meth9            = make_array(nBeams,/int,value=pp_bfill)
	bestord_meth9        = make_array(nBeams,/int,value=pp_bfill)
	classA_towards_meth9 = pp_bfill
	nonA_towards_meth9   = pp_bfill
	classA_away_meth9    = pp_bfill
	nonA_away_meth9      = pp_bfill
	beam_used_meth9      = make_array(nBeams,/int,value=pp_bfill)
	perr3_meth9          = pp_rfill

	rchi2_CG          = pp_rfill
	chi2_CG           = pp_rfill
	ndegfree_CG       = pp_bfill
	rchi2_unpen_CG    = pp_rfill
	ndegfree_unpen_CG = pp_bfill
	rchi2_FG          = pp_rfill
	chi2_FG           = pp_rfill
	ndegfree_FG       = pp_bfill
	rchi2_unpen_FG    = pp_rfill
	ndegfree_unpen_FG = pp_bfill
	derr2_out         = pp_rfill
	perr2_out         = pp_rfill
	tgerr2_out        = pp_rfill

	; Prep the beam order information (if any)
	ep_prep_order, $
		nBeams, beam_runstat, status, beam_out, beam_code_type, beam_tchip, beam_tcode, $ ; IN
		n_posord, posord, maxorder_pos, beam_class_init, beam_penalty ; OUT

	; Calculate the beam velocity
	vb = sqrt (erg *2. *1.0e3 *pp_eV2joule /pp_emass_mks) * 1.0e-6 ; 18.755300 m/microsec
	;******************************************************************
	; Define the 'beam' structure --- MUST be not sooner in code, because relies on values not then present
	;******************************************************************
	beam = { $
		; beams, $
		vb:              vb, $                ; beam velocity: meters/microsec
		mm:              nBeams, $
		maxorder:        maxorder_pos, $
		siggdu:          pp_acgdu_m, $    ; Meters
		class:           beam_class_init, $
		class_init:      beam_class_init, $
		gunid:           beam_gunid, $
		maxchan:         beam_maxchan, $
		code_type:       beam_code_type, $
		out:             beam_out, $
		pmt_ok:          make_array(nBeams,/int,value=0), $ ; Use Poor Man's ToF: Defined later
		tri_ok:          make_array(nBeams,/int,value=0), $ ; Use triangulation:  Defined later
		xg:              beam_xg, $
		yg:              beam_yg, $
		alpha:           beam_alpha*!dtor, $                ; Beam angle in BPP
		qual:            beam_qual, $                       ; Beam quality
		tof:             beam_tof, $                        ; Beam ToF
		bestord:         make_array(nBeams,/int,value=0), $ ; ???: Defined later
		toaw_init:       make_array(nBeams,/int,value=0), $ ; Initial guess of toward/away beams: 1=to, -1=aw, RMT
		toaw_final:      make_array(nBeams,/int,value=0), $ ; Final determination of toward/away: 1=to, -1=aw, RMT
		n_posord:        n_posord, $
		posord:          posord, $
		btime:           beam_btime, $                      ; Times beams were fired
		sigfir:          beam_bwidth, $                     ; Width of beam in BPP
		fa_error_renorm: make_array(nBeams,/float,value=pp_rfill), $ ; Def. later
		$
		$                       ; RunEst.pro information
		$ ; Not used anywhere   runest_order:reform(beam_runstat(0,*)), $
		$ ; Not used anywhere   runest_flag:beam_runstat(1:pp_maxorder_runest,*), $
		runest_estof:    beam_runstat(pp_maxorder_runest+1:2*pp_maxorder_runest,*), $ ; Used in ep_poormans_tof.pro and ep_richmans_tof_sa.pro
		runest_estg:     reform(beam_runstat(2*pp_maxorder_runest+1,*)), $ ; Used several places
		$ ;Not used anywhere    runest_prob:beam_runstat(2*pp_maxorder_runest+2: $
		$ ;                                           3*pp_maxorder_runest+1,*), $
		runest_penalty:  beam_penalty, $ ; Used several places
		$
		correlatorChipPeriod: beam_tchip, $     ; Correlator chip length, microsecs
		correlatorCodePeriod: beam_tcode}       ; Correlator code length, microsecs

	out_out = beam_out
	beam_class_out = beam_class_init
	beam_class_init_out = beam_class_init
	beam_penalty_out = beam_penalty
	if (not status) then begin
		status_out = 50             ; Not enough beams after order/class prep
		msg_out = pp_pstat(status_out)

		status_meth9 = 50
		msg_meth9 = pp_pstat(status_out)

		return
	endif

	; Calculate the beam velocity
	vb = sqrt (erg *2. *1.0e3 *pp_eV2joule /pp_emass_mks) * 1.0e-6 ; meters/microsec

	; Calculate the gyro-time using the magnitude of the average magnetic
	; field for this interval
	bmag_cgs = bmag *1.0e-9 *1.0e+4
	tg = 2. *!pi *9.1e-28 *3.0e+10 /(4.8e-10 *bmag_cgs) *1.0e6 ; microsecs
	tgerr = 0.
	tgerr2 = 0.

	skip_cases = 0
	bestord_out = beam.bestord
	out_out = beam.out
	beam_class_out = beam.class
	edi6_out = make_array(6,/float,value=pp_rfill)
	case pp_method of
		7: begin
			icont = ep_method_logic_rmt_sa ( $
				checkpar=1, beam, $ ;IN
				mean_angle_out, stdev_angle_out, $ ; OUT
				out_out, bestord_out, $ ; OUT
				status_out, msg_out, method_out, $ ; OUT
				edi6_out, ambig_180_out, $ ; OUT
				classA_towards, nonA_towards, $ ; OUT
				classA_away, nonA_away) ; OUT
			if (icont) then  $
				skip_cases = 1 $
			else             $
				return
		end
		8: begin
			idm = ep_method_logic_rmt_sa ( $
				checkpar=0, beam, $ ;IN
				mean_angle_out, stdev_angle_out, $ ; OUT
				out_out, bestord_out, $ ; OUT
				status_out, msg_out, method_out, $ ; OUT
				edi6_out, ambig_180_out, $ ; OUT
				classA_towards, nonA_towards, $ ; OUT
				classA_away, nonA_away) ; OUT
			return
		end
		9: begin
			idm = ep_method_logic_rmt_sa ( $
				checkpar=0, beam, $ ;IN
				mean_angle_meth9, stdev_angle_meth9, $ ; OUT
				out_meth9, bestord_meth9, $ ; OUT
				status_meth9, msg_meth9, method_meth9, $ ;OUT
				edi6_meth9, ambig_180_meth9, $ ; OUT
				classA_towards_meth9, nonA_towards_meth9, $ ; OUT
				classA_away_meth9, nonA_away_meth9, $
				beam_used_meth9, perr3_meth9) ; OUT
			skip_cases = 1
			; goto, bestarg_continue
		end
		else: begin
			; goto, bestarg_continue
			message, 'bestarg: pp_method not found.'
		end
	endcase

;bestarg_continue:

	;******************************************************************
	; Define which beams are suitable for triangulation
	; beam.tri_ok updated here
	; Note: if beam.out updated, then this needs to be called again
	;******************************************************************
	ep_choose_tri_beams, $
		beam, TRI_OK

	if (not TRI_OK) then begin ; Not enough beams after TRI_OK assessment
		status_out = 54
		msg_out = pp_pstat(status_out)
		return
	endif

	;******************************************************************
	; Define the radial and azimuthal coarse grids
	;******************************************************************

	; Test
	;rmax=100.

	ep_bestarg_grids, $
		coarse=1, rmax=rmax, $ ; IN
		nr, np, rsurf, psurf, resop, siz, xsurf, ysurf ; OUT


	;******************************************************************
	; Calculate the chi2-surface on the coarse grid using
	; 1) robust estimation
	; 2) Use all RunEst info (searches for all lowest chi2 contributions)
	; 3) NO HIGHER-ORDER-SUPPRESSION PENALTY TO THE CHI2 CONTRIBUTIONS!
	;    (i.e., no order^poc higher-order-suppression penalty...
	;    runner penalty is applied at the end, however, to normaliztion of chi2)
	; 4) Use only 'chosen' beams defined by beam.tri_ok
	;******************************************************************
	robust = 1
	use_bestord = 0
	poc_in = 0                      ; poc_in = poc

	ep_calc_chisurf, $
		status, status_renorm, $
		robust, poc_in, use_bestord, $ ; Control, IN
		gyrorad, gyroper, flip, $      ; Curved traj. Info, IN
		nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid Info, IN
		beam, $                       ; Beam Info, IN
		$
		ndegfree, nclimb, chi_stat, $ ; OUT
		ndegfree_unpen, nclimb_unpen, $ ; OUT
		$
		chisurf, $                    ; Chi2-surface info, OUT
		xd, yd, rd, pd, $             ; Target info, OUT
		rchi2, chi2, rchi2_unpen, $   ; OUT
		derr_min, perr_min          ; OUT

	; Do more passes with rmax = rmax * 2 < 100m if rmax being too small
	; was a problem
	while (not status and rmax_out le 100.) do begin
		rmax_out = rmax_out*2.
		ep_bestarg_grids, $
			coarse=1, rmax=rmax_out, $ ; IN
			nr, np, rsurf, psurf, resop, siz, xsurf, ysurf ; OUT
		ep_calc_chisurf, $
			status, status_renorm, $
			robust, poc_in, use_bestord, $ ; Control, IN
			gyrorad, gyroper, flip, $ ; Curved traj. Info, IN
			nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid Info, IN
			beam, $
			$
			ndegfree, nclimb, chi_stat, $ ; OUT
			ndegfree_unpen, nclimb_unpen, $ ; OUT
			$
			chisurf, $                ; Chi2-surface info, OUT
			xd, yd, rd, pd, $         ; Target info, OUT
			rchi2, chi2, rchi2_unpen, $ ; OUT
			derr_min, perr_min      ; OUT
	endwhile

	; Check some statii, and even if fail keep going (because there are
	; old ToF branches below this), but TRI not possible
	; BUT, force it through anyway if pp_method = 9
	NO_TRI = 0
	if (not status) then $
		NO_TRI = 1 ; Target might lie on grid edge

	NO_TRI_2 = 0
	if (not chi_stat) then $
		NO_TRI_2 = 1 ; rchi2 calculation not possible (ndegfree le 0)

	;******************************************************************
	; Using the above 'Target 1' information, assign the order based on lowest
	; chi2 contribution - beam.bestord defined here
	;******************************************************************
	ep_assign_order, $
		beam, xd, yd, gyrorad, flip, $ ; IN
		sigratio_nopen, fdoe_nopen, $ ; OUT
		rchi2, chi2, ndegfree, nclimb, chi_stat, $ ; OUT
		rchi2_unpen, ndegfree_unpen, nclimb_unpen ; OUT

	if (not chi_stat) then $
		NO_TRI_2 = 1 ; rchi2 calculation not possible (ndegfree le 0)

	;******************************************************************
	; Remove outliers - beam.out, fdoe_nopen, sigratio_nopen are updated here
	;******************************************************************
	ep_outlier_removal, $
		status, beam, $ ; IN/OUT
		fdoe_nopen, sigratio_nopen

	out_out = beam.out
	if (not status) then begin
		bestord_out = beam.bestord
		out_out = beam.out
		beam_class_out = beam.class
		status_out = 51             ; After outlier removal, not enough beams
		msg_out = pp_pstat(status_out)
		return
	endif

	;******************************************************************
	; Update which beams are suitable for triangulation because
	; beam.out has been updated and we possibly have outliers
	; beam.tri_ok updated here
	;******************************************************************
	ep_choose_tri_beams, $
		beam, TRI_OK
	if (not TRI_OK) then begin
			status_out = 54             ; Not enough beams after TRI_OK assessment
			msg_out = pp_pstat(status_out)
			return
	endif

	; beam.tri_ok should not change from here on
	beam_used_out = beam.tri_ok

	;******************************************************************
	; Define effective measurement errors based on the quality of the fit.
	; Put the renormalized errors into a new variable so that you can
	; use them when necessary (like when using the coarse grid chi-squared
	; surface for determining the fine grid extent)
	; Only use those beams that are 'OK'
	;******************************************************************
	iok = where(beam.tri_ok eq 1)
	beam.fa_error_renorm(iok) =  $
		(beam.sigfir(iok) + (beam.bestord(iok)-1) *(0.9*!dtor) * beam.runest_estg(iok)/3900.) $
		* sqrt (total (fdoe_nopen(iok)) /total (sigratio_nopen(iok)))

	;******************************************************************
	; Recalculate the coarse grid chi2-surface using:
	; 1) Non-robust estimation
	; 2) Beam order information contained in beam.bestord
	; 3) Renormalized firing angle errors (beam.fa_error_renorm)
	;    (these errors are used so that rchi2_min is of order 1, so
	;     that when this surface is used to define the extent of the
	;     fine grid, we can use the standard method, and crawl the
	;     standard increment up the sides of the 'bowl')
	; 4) NO HIGHER-ORDER-SUPPRESSION PENALTY (i.e. order^poc)
	;    TO THE CHI2 CONTRIBUTIONS!
	; 5) Use only 'chosen' beams defined by beam.tri_ok
	;******************************************************************
	robust = 0
	use_bestord = 1
	poc_in = 0

	;ep_calc_chisurf, status, status_renorm, $
	;  robust, poc_in, use_bestord, $ ; Control, IN
	;  gyrorad, gyroper, flip, $     ; Curved traj. Info, IN
	;  nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid Info, IN
	;  beam, $
	;  $
	;  ndegfree, nclimb, chi_stat, $ ; OUT
	;  ndegfree_unpen, nclimb_unpen, $ ; OUT
	;  $
	;  chisurf, $                    ; Chi2-surface info, OUT
	;  xd, yd, rd, pd, $             ; Target info, OUT
	;  rchi2, chi2, rchi2_unpen, $   ; OUT
	;  derr_min, perr_min, $       ; OUT
	;  $
	;  chisurf_renorm, $             ; Chi2-surface using fa_error_renorm
	;  xd_renorm, yd_renorm, rd_renorm, pd_renorm, $
	;  rchi2_renorm, chi2_renorm, rchi2_renorm_unpen, $
	;  derr_min_renorm, perr_min_renorm

	ep_calc_chisurf_speedy2, $
		status, status_renorm, $
		gyrorad, gyroper, flip, $     ; Curved traj. Info, IN
		nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid Info, IN
		beam, $
		$
		ndegfree, nclimb, chi_stat, $ ; OUT
		ndegfree_unpen, nclimb_unpen, $ ; OUT
		$
		chisurf, $                    ; Chi2-surface info, OUT
		xd, yd, rd, pd, $             ; Target info, OUT
		rchi2, chi2, rchi2_unpen, $   ; OUT
		derr_min, perr_min, $         ; OUT
		$
		chisurf_renorm, $             ; Chi2-surface using fa_error_renorm
		xd_renorm, yd_renorm, rd_renorm, pd_renorm, $
		rchi2_renorm, chi2_renorm, rchi2_renorm_unpen, $
		derr_min_renorm, perr_min_renorm

	;===============SPEED TESTING============================
	;===============SPEED TESTING============================
	;ep_calc_chisurf_speedy2, status_st, status_renorm_st, $
	;  gyrorad, gyroper, flip, $     ; Curved traj. Info, IN
	;  nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid Info, IN
	;  beam, $
	;  $
	;  ndegfree_st, nclimb_st, chi_stat_st, $ ; OUT
	;  ndegfree_unpen_st, nclimb_unpen_st, $ ; OUT
	;  $
	;  chisurf_st, $                    ; Chi2-surface info, OUT
	;  xd_st, yd_st, rd_st, pd_st, $             ; Target info, OUT
	;  rchi2_st, chi2_st, rchi2_unpen_st, $   ; OUT
	;  derr_min_st, perr_min_st, $       ; OUT
	;  $
	;  chisurf_renorm_st, $             ; Chi2-surface using fa_error_renorm
	;  xd_renorm_st, yd_renorm_st, rd_renorm_st, pd_renorm_st, $
	;  rchi2_renorm_st, chi2_renorm_st, rchi2_renorm_unpen_st, $
	;  derr_min_renorm_st, perr_min_renorm_st
	;stop
	;===============SPEED TESTING============================
	;===============SPEED TESTING============================



	if (not status_renorm) then $
		NO_TRI = 1 ; Target might lie on grid edge
	if (not chi_stat) then $
		NO_TRI_2 = 1    ; rchi2 calculation not possible  (ndegfree le 0)
	oplot_x = [xd_renorm]
	oplot_y = [yd_renorm]

	;******************************************************************
	; Now some logic pertaining to which method to use and output.
	; Since this seems to be changing quite often, these various
	; logical chains have been modularized.
	;******************************************************************
	bestord_out = beam.bestord
	out_out = beam.out
	beam_class_out = beam.class
	edi6_out = make_array(6,/float,value=pp_rfill)

	; Might have already done RMT above, in which case skip all of the
	; pp_method checks below and continue with TRI (if necessary).
	if (skip_cases and NO_TRI) then begin
		status_out = 10             ; Coarse grid target on grid edge
		msg_out = pp_pstat(status_out)
		return
	endif $
	else $
		if (skip_cases and NO_TRI_2) then begin
			status_out = 16             ; N_degrees_freedom <= 0
			msg_out = pp_pstat(status_out)
			return
		endif $
	else $
		goto, tri_continue

	;******************************************************************
	; Define the PMT-worthy beams here
	; beam.pmt_ok defined
	;******************************************************************
	ep_define_worthiness, $
		beam

	icont = 0
	case pp_method of
		0: begin
			if (NO_TRI) then begin
				status_out = 10     ; Coarse grid target on grid edge
				msg_out = pp_pstat(status_out)
				return
			endif $
			else  $
				if (NO_TRI_2) then begin
					status_out = 16     ; N_degrees_freedom <= 0
					msg_out = pp_pstat(status_out)
					return
				endif $
				else  $
					goto, tri_continue
		end
		1: begin
			icont = ep_method_logic1 ( $
				xd_renorm,yd_renorm, $
				tg,beam,bmag,NO_TRI, $ ; IN
				mean_angle_out, stdev_angle_out, $ ; OUT
				out_out, $ ; OUT
				status_out, msg_out, method_out, $ ; OUT
				edi6_out, classA_towards, nonA_towards, $ ;OUT
				classA_away, nonA_away, $ ; OUT
				ambig_180_out) ; OUT

			if (icont) then $
				goto, tri_continue
			return
		end
		2: begin
			idm = ep_method_logic_pmt ( $
				xd_renorm,yd_renorm,tg, $
				beam,bmag,NO_TRI, $ ;IN
				mean_angle_out, stdev_angle_out, $ ; OUT
				out_out, $ ; OUT
				status_out, msg_out, method_out, $ ; OUT
				edi6_out, classA_towards, nonA_towards, $ ;OUT
				classA_away, nonA_away, $ ; OUT
				ambig_180_out) ; OUT
			return
		end
		3: begin
			icont = ep_method_logic2 ( $
				xd_renorm,yd_renorm,tg,$
				beam,bmag,NO_TRI, $ ; IN
				mean_angle_out, stdev_angle_out, $ ; OUT
				out_out, $ ; OUT
				status_out, msg_out, method_out, $ ; OUT
				edi6_out, classA_towards, nonA_towards, $ ;OUT
				classA_away, nonA_away, $ ; OUT
				ambig_180_out)
			if (icont) then $
				goto, tri_continue
			return
		end
		4: begin
			icont = ep_method_logic3 ( $
				xd_renorm,yd_renorm,tg,$
				beam,bmag,NO_TRI, $ ;IN
				mean_angle_out, stdev_angle_out, $ ; OUT
				out_out, $ ; OUT
				status_out, msg_out, method_out, $ ; OUT
				edi6_out, classA_towards, nonA_towards, $ ;OUT
				classA_away, nonA_away, $ ; OUT
				ambig_180_out)
			if (icont) then $
				goto, tri_continue
			return
		end
		5: begin
			idm = ep_method_logic_smt ( $
				xd_renorm,yd_renorm,$
				tg,beam,bmag,NO_TRI, $ ;IN
				mean_angle_out, stdev_angle_out, $ ; OUT
				out_out, $ ; OUT
				status_out, msg_out, method_out, $ ; OUT
				edi6_out, classA_towards, nonA_towards, $ ;OUT
				classA_away, nonA_away, $ ; OUT
				ambig_180_out) ; OUT
			return
		end
		6: begin
			idm = ep_method_logic_pmtsmt ( $
				xd_renorm,yd_renorm,$
				tg,beam,bmag,NO_TRI, $ ;IN
				mean_angle_out, stdev_angle_out, $ ; OUT
				out_out, $ ; OUT
				status_out, msg_out, method_out, $ ; OUT
				edi6_out, classA_towards, nonA_towards, $ ;OUT
				classA_away, nonA_away, $ ; OUT
				ambig_180_out) ; OUT
			return
		end
		else: begin ; Value of pp_method not valid
			status_out = 52
			msg_out = pp_pstat (status_out) +': ' +strtrim (pp_method,2)
			return
		end
	endcase

tri_continue:

	method_out = 0
	;******************************************************************
	; Test rchi2 on coarse grid...Continue to fine grid?
	; NOTE: This is rchi2 from the 'normal' surface, using nominal firing
	; angle errors, NOT the renormalized firing angle errors...See most
	; recent call above to ep_calc_chisurf.pro
	;******************************************************************

	chi2_CG = chi2
	ndegfree_CG = ndegfree
	rchi2_CG = rchi2
	ndegfree_unpen_CG = ndegfree_unpen
	rchi2_unpen_CG = rchi2_unpen

	if (rchi2_CG gt pp_rchi2_abortlim) then begin ; Coarse grid rchi2 > pp_rchi2_abortlim
		status_out = 11
		msg_out = pp_pstat(status_out)
		bestord_out = beam.bestord
		out_out = beam.out
		beam_class_out = beam.class
		return
	endif

	if (not chi_stat) then begin    ; Can't continue if ndegfree <= 0
		status_out = 16
		bestord_out = beam.bestord
		out_out = beam.out
		beam_class_out = beam.class
		msg_out = pp_pstat(status_out)
		return
	endif

	;******************************************************************
	; From the improved coarse grid surface, use the shape of the
	; 'bowl' near the minimum to define the extent of the fine grid
	;******************************************************************
	ep_param_errors, $
		chisurf_renorm, $ ; Chi2 surface info, IN
		xsurf, ysurf, rsurf, psurf, $ ; Grid info, IN
		xd_renorm, yd_renorm, rd_renorm, pd_renorm, $ ; Target info, IN
		rchi2_renorm, chi2_renorm, ndegfree, nclimb, chi_stat, $ ; Goodness info, IN
		derr_min_renorm, perr_min_renorm, $ ; Minimum errors to start with, IN
		derr, perr, $               ; Error info, OUT
		ambig_180=ambig_180_out, $
		/coarse, $
		keyplot3=keyplot3, beam=beam, kp3_title=kp3_title ; Plotting info, IN

	;******************************************************************
	; Define the radial and azimuthal fine grids
	;******************************************************************
	ep_bestarg_grids, $
		fine=1, pd=pd_renorm, err_pd=perr, $ ; IN
		rd=rd_renorm, err_rd=derr, $ ; IN
		nr, np, rsurf, psurf, resop, siz, xsurf, ysurf ; OUT

	;******************************************************************
	; Calculate the chi2-surface on the fine grid using
	; 1) Non-robust estimation
	; 2) All RunEst Information!  (Examine all "legal" orders)
	; 3) Nominal firing angle errors
	; 4) NO HIGHER-ORDER-SUPPRESSION penalty
	; 5) Beam usage contained in beam.tri_ok
	;******************************************************************
	robust = 0
	use_bestord = 0
	poc_in = 0

	ep_calc_chisurf, $
		status, status_renorm, $
		robust, poc_in, use_bestord, $ ; Control, IN
		gyrorad, gyroper, flip, $     ; Curved traj. Info, IN
		nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid Info, IN
		beam, $                       ; Beam Info, IN
		$
		ndegfree_fp, nclimb_fp, chi_stat_fp, $ ; OUT, 'fp' = 'f'isrt 'p'ass
		ndegfree_unpen_fp, nclimb_unpen_fp, $ ; OUT
		$
		chisurf_fp, $                 ; Chi2-surface info, OUT
		xd_fp, yd_fp, rd_fp, pd_fp, $ ; Target info, OUT
		rchi2_fp, chi2_fp, rchi2_unpen_fp, $ ; OUT
		derr_min_fp, perr_min_fp, $   ; OUT
		/finegrid

	if (not status) then begin      ; Target lies on fine grid edge

		; Double the size of the fine grid and try again (exactly the chisurf you tried above)
		derr = derr*2.
		perr = perr*2. < 45.*!dtor ; Don't allow angular extent greater 90 degrees (note: angular extent of fine grid = 2.*perr)

		ep_bestarg_grids, $
			fine=1, pd=pd_renorm, err_pd=perr, $ ; IN
			rd=rd_renorm, err_rd=derr, $ ; IN
			nr, np, rsurf, psurf, resop, siz, xsurf, ysurf ; OUT

		robust = 0
		use_bestord = 0
		poc_in = 0
		ep_calc_chisurf, $
			status_second, status_renorm, $ ; OUT
			robust, poc_in, use_bestord, $ ; Control, IN
			gyrorad, gyroper, flip, $ ; Curved traj. Info, IN
			nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid Info, IN
			beam, $                   ; Beam Info, IN
			$
			ndegfree_fp, nclimb_fp, chi_stat_fp, $ ; OUT, 'fp' = 'f'isrt 'p'ass
			ndegfree_unpen_fp, nclimb_unpen_fp, $ ; OUT
			$
			chisurf_fp, $             ; Chi2-surface info, OUT
			xd_fp, yd_fp, rd_fp, pd_fp, $ ; Target info, OUT
			rchi2_fp, chi2_fp, rchi2_unpen_fp, $ ; OUT
			derr_min_fp, perr_min_fp, $ ; OUT
			/finegrid

		if (not status_second) then begin ; Failure
			bestord_out = beam.bestord
			out_out = beam.out
			beam_class_out = beam.class
			status_out = 12         ; Fine grid target on grid edge
			msg_out = pp_pstat(status_out)

			if keyword_set(keyplot3) then begin ; Show the fine grid on the edge with plot
				ep_param_errors, $
					chisurf_fp, $ ; Chi2 surface info, IN
					xsurf, ysurf, rsurf, psurf, $ ; Grid info, IN
					xd_fp, yd_fp, rd_fp, pd_fp, $ ; Target info, IN
					-1.e+31, -1.e+31, ndegfree_fp, nclimb_fp, chi_stat_fp, $ ; Goodness info, IN
					-1.e+31, -1.e+31, $ ; Minimum errors to start with, IN
					/fine, $
					keyplot3=keyplot3, beam=beam, kp3_title=kp3_title, $ ; Plotting info, IN
					oplot_x=oplot_x, oplot_y=oplot_y, /edge_error
				endif
			return
		endif
	endif

	;******************************************************************
	; Using the above 'Target 3' information, assign the order based on lowest
	; chi2 contribution - beam.bestord defined here
	; ALSO:  Bump down the order if minimum angle threshhold met

	; Disable Bump thing until I investigate!  Sometimes, bumping down adds HUGE
	; contributions to the chi2, so the minimization surface then becomes
	; totally off!  Case in point:  SC3, 20010321, start at 22:24:11

	; ALSO:  Calculate the chi2, rchi2 and ndegfree for this Target 3 and
	;        this definitive order assignment
	; ALSO:  Calculate perr3, which is an alternate measure of the phi
	;        angle error
	;******************************************************************
	ep_assign_order, $
		beam, xd_fp, yd_fp, gyrorad, flip, $ ; IN
		sigratio_nopen_ao, fdoe_nopen_ao, $ ; OUT
		rchi2_ao, chi2_ao, ndegfree_ao, nclimb_ao, chi_stat_ao, $ ; OUT
		rchi2_unpen_ao, ndegfree_unpen_ao, nclimb_unpen_ao, $ ; OUT
		perr3=perr3  ;, $                ; OUT
		;  /bumpdown                     ; IN

	if (not chi_stat_ao) then begin
		status_out = 16             ; N_degrees_freedom <= 0
		bestord_out = beam.bestord
		out_out = beam.out
		beam_class_out = beam.class
		msg_out = pp_pstat (status_out)
		return
	endif

	;******************************************************************
	; I have quite possibly changed the order assignment (bumping down)
	; which renders chisurf_fa (from previous call to ep_calc_chisurf)
	; inconsistent with the new chi2_ao returned from ep_assign_order.  The
	; chisurf_fp and chi2_fp returned from ep_calc_chisurf are related in that
	; chi2_fp = min(chisurf_fp).  But we've recalculated chi2 in
	; ep_assign_order, so now this isn't necessarily the case anymore that
	; chi2_ao = min(chisurf_fp).
	; It's a problem because in the call below to ep_param_errors it must
	; be that chi2 = min(chisurf).  So, I'm going to recalc the chisurf
	; using now, finally, (hopefully!) the definitive orders so that chi2
	; and chisurf are consistent.
	; Calculate the chi2-surface on the fine grid using
	; 1) Non-robust estimation
	; 2) Runner information in beam.bestord
	; 3) Nominal firing angle errors
	; 4) NO HIGHER-ORDER-SUPPRESSION penalty
	; 5) Beam usage contained in beam.tri_ok
	;******************************************************************

	robust = 0
	use_bestord = 1
	poc_in = 0

	;ep_calc_chisurf, status, status_renorm, $
	;  robust, poc_in, use_bestord, $ ; Control, IN
	;  gyrorad, gyroper, flip, $     ; Curved traj. Info, IN
	;  nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid Info, IN
	;  beam, $                       ; Beam Info, IN
	;  $
	;  ndegfree, nclimb, chi_stat, $ ; OUT
	;  ndegfree_unpen, nclimb_unpen, $ ; OUT
	;  $
	;  chisurf, $                    ; Chi2-surface info, OUT
	;  xd, yd, rd, pd, $             ; Target info, OUT
	;  rchi2, chi2, rchi2_unpen, $   ; OUT
	;  derr_min, perr_min, $       ; OUT
	;  /finegrid

	ep_calc_chisurf_speedy2, $
		status, status_renorm, $
		gyrorad, gyroper, flip, $     ; Curved traj. Info, IN
		nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid Info, IN
		beam, $                       ; Beam Info, IN
		$
		ndegfree, nclimb, chi_stat, $ ; OUT
		ndegfree_unpen, nclimb_unpen, $ ; OUT
		$
		chisurf, $                    ; Chi2-surface info, OUT
		xd, yd, rd, pd, $             ; Target info, OUT
		rchi2, chi2, rchi2_unpen, $   ; OUT
		derr_min, perr_min, $       ; OUT
		/finegrid

	;===============SPEED TESTING============================
	;===============SPEED TESTING============================
	;ep_calc_chisurf_speedy2, status_st, status_renorm_st, $
	;  gyrorad, gyroper, flip, $     ; Curved traj. Info, IN
	;  nr, np, rsurf, psurf, xsurf, ysurf, resop, $ ; Grid Info, IN
	;  beam, $
	;  $
	;  ndegfree_st, nclimb_st, chi_stat_st, $ ; OUT
	;  ndegfree_unpen_st, nclimb_unpen_st, $ ; OUT
	;  $
	;  chisurf_st, $                 ; Chi2-surface info, OUT
	;  xd_st, yd_st, rd_st, pd_st, $ ; Target info, OUT
	;  rchi2_st, chi2_st, rchi2_unpen_st, $ ; OUT
	;  derr_min_st, perr_min_st, $   ; OUT
	;  /finegrid
	;stop
	;===============SPEED TESTING============================
	;===============SPEED TESTING============================

	if (not status) then begin
		; Target lies on fine grid edge (which shouldn't happen here since we handled this error branch
		; above by doubling the size of the fine grid on the first pass)
		status_out = 12             ; Fine grid target on grid edge
		bestord_out = beam.bestord
		out_out = beam.out
		beam_class_out = beam.class
		msg_out = pp_pstat(status_out)

		if keyword_set(keyplot3) then begin ; Show the fine grid on the edge with plot
			ep_param_errors, $
				chisurf, $ ; Chi2 surface info, IN
				xsurf, ysurf, rsurf, psurf, $ ; Grid info, IN
				xd, yd, rd, pd, $     ; Target info, IN
				-1.e+31, -1.e+31, ndegfree, nclimb, chi_stat, $ ; Goodness info, IN
				-1.e+31, -1.e+31, $   ; Minimum errors to start with, IN
				/fine, $
				keyplot3=keyplot3, beam=beam, kp3_title=kp3_title, $ ; Plotting info, IN
				oplot_x=oplot_x, oplot_y=oplot_y, /edge_error
		endif
		return
	endif

	;******************************************************************
	; Fine grid reduced chi-squared for output
	;******************************************************************
	chi2_FG = chi2
	ndegfree_FG = ndegfree
	rchi2_FG = rchi2
	ndegfree_unpen_FG = ndegfree_unpen
	rchi2_unpen_FG = rchi2_unpen

	if (rchi2_FG gt pp_rchi2_outlim) then begin
		status_out = 13             ; Fine grid rchi2 > pp_rchi2_outlim
		bestord_out = beam.bestord
		out_out = beam.out
		beam_class_out = beam.class
		msg_out = pp_pstat(status_out)
		return
	endif

	;******************************************************************
	; Determine the radial and azimuthal errors on the target from the
	; fine grid chi2-surface
	;******************************************************************
	ep_param_errors, $
		chisurf, $ ; Chi2 surface info, IN
		xsurf, ysurf, rsurf, psurf, $ ; Grid info, IN
		xd, yd, rd, pd, $             ; Target info, IN
		rchi2, chi2, ndegfree, nclimb, chi_stat, $ ; Goodness info, IN
		derr_min, perr_min, $       ; Minimum errors to start with, IN
		derr, perr, $               ; Error info, OUT
		/fine, $
		derr2=derr2, perr2=perr2, $
		keyplot3=keyplot3, beam=beam, kp3_title=kp3_title, $ ; Plotting info, IN
		oplot_x=oplot_x, oplot_y=oplot_y

	; Diagnostics Oct. 2009
	;save, file='chisurf_results.idlsav', chisurf, xd, yd, rd, pd, rchi2, chi2, ndegfree, nclimb, chi_stat, derr, perr, derr2, perr2
	;stop

	;******************************************************************
	; THE ANSWER:
	;******************************************************************
	edi6_out = [rd,derr,pd*!radeg,perr*!radeg,tg,tgerr]

	derr2_out = derr2
	perr2_out = perr2*!radeg
	perr3_out = perr3*!radeg

	tgerr2_out = tgerr2

	status_out = 0                  ; TRI Success
	msg_out = pp_pstat(status_out)
	bestord_out = beam.bestord
	out_out = beam.out
	beam_class_out = beam.class

; 	print, 'edi6_out = [rd, derr, pd*!radeg, perr*!radeg, tg, tgerr]'
; 	print, edi6_out
	return
end
