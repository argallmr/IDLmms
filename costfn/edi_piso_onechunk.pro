; docformat = 'rst'
;
; NAME:
;       edi_piso_onechunk
;
;+
;   Process "one chunck" of data.
;      1) Calculate beam width in BPP
;      2) Despin and transform gun positions and beams into BPP
;      3) Output sorted information
;           * [GD21, GD12]
;           * Quality & MaxChan filter
;           * RunEst & other beam information
;
; :Params:
;       ATIME_OUT:      out, required, type=dblarr
;                       Time in seconds since 1970-01-01. Takes ATIME from
;                           EDI_PISO_ONECHUK_COMMON and sorts it with rest of data.
;       BTIME_OUT:      out, optional, type=dblarr
;                       Beam time in seconds since 1970-01-01.  Takes GTIME from
;                           EDI_PISO_ONECHUK_COMMON and sorts it with rest of data.
;       BWIDTH_OUT:     out, required, type=fltarr
;                       Width of firing beam in BPP.
;       DATA_OUT:       out, required, type=7xM fltarr
;                       Array with first dimension ordered as::
;                           DATA_OUT[0,*] - Gun ID
;                           DATA_OUT[1,*] - Quality
;                           DATA_OUT[2,*] - X Gun Position in BPP
;                           DATA_OUT[3,*] - Y Gun Position in BPP
;                           DATA_OUT[4,*] - Firing angle in BPP
;                           DATA_OUT[5,*] - Time of flight
;                           DATA_OUT[6,*] - Code type [short/long]
;       NNN_OUT:        out, required, type=long
;                       Correlator n (1,2,4,8,16,32,64) of first beam. Takes NNN from
;                           EDI_PISO_ONECHUNK_COMMON and sorts it with rest of data.
;       MM_OUT:         out, required, type=long
;                       Correlator m (16,8,4,2 ) of first beam. Takes MM from
;                           EDI_PISO_ONECHUNK_COMMON and sorts it with rest of data.
;       CT_OUT:         out, required, type=long
;                       Code type (short/long).  Takes CODE_TYPE from
;                           EDI_PISO_ONECHUNK_COMMON and sorts it with rest of data.
;                           Should be same as `DATA_OUT`[6,*]
;       MXCH_OUT:       out, required, type=bytarr
;                       Max-channel for both GDUs. Takes MXCH1 and MXCH2 from
;                           EDI_PISO_ONECHUNK_COMMON and sorts it with rest of data.
;       GDOTB_OUT:      out, required, type=fltarr
;                       Angle (degrees) between beam firing vectors and B.
;       RUNSTAT_OUT:    out, required, type=intarr
;                       Beam status array with the first dimension ordered as::
;                           RUNSTAT_OUT[0]                       - Runner order
;                           RUNSTAT_OUT[1:MAXORDER]              - Flag
;                           RUNSTAT_OUT[MAXORDER+1:2*MAXORDER]   - Estimated single runner ToF
;                           RUNSTAT_OUT[2*MAXORDER+1]            - Gyrotime
;                           RUNSTAT_OUT[2*MAXORDER+2]            - Probability
;       SCS2BPP:        out, required, type=3x3 fltarr
;                       Transformation from spinning s/c coordinates to BPP
;                       Status flag: 0=fail, 1=pass
;       BPP2SCS:        out, required, type=3x3 fltarr
;                       Transformation from BPP to spinning s/c coordinates
;       CHUNK_STATUS:   out, required, type=integer
;                       Return status flag: 0=fail, 1=pass
;       ATIME_T70:      out, optional, type=integer
;                       Not used.
;    SPIN_SRT_T70_PCK:  out, optional, type=integer
;                       Not used.
;    SPIN_SRT_T70_FGM:  out, optional, type=integer
;                       Not used.
;       UNITOUT:        out, optional, type=integer
;                       Not used.
;       UNITOUT2:       out, optional, type=integer
;                       Not used.
;       RUNSTRUC1_OC:   in, required, type=structarr
;                       Runner order structure for GD21 returned by RunEst
;       RUNSTRUC2_OC:   in, required, type=structarr
;                       Runner order structure for GD12 returned by RunEst
;
; :Keywords:
;       PHASE0_OFFSET:  in, optional, type=integer
;                       Not used.
;       GDOTB_ERROR:    out, optional, type=fltarr
;                       Not used.
;       SCS2GSE:        out, optional, type=3x3 dblarr
;                       Transformation from spinning spacecraft to GSE coordinates
;       FLIP:           out, required, type=bytarr
;                       Flip flag indicating that initially bhat[2] < 0
;       SCS2BPP_REFVEC: out, optional, type=3x1 fltarr
;                       Reference fector used to construct `SCS2BPP`.
;       NNN2:           out, optional, type=lonarr
;                       Correlator n for all beams.
;       MM2:            out, optional, type=lonarr
;                       Correlator m for all beams.
;       CT2:            out, optional, type=lonarr
;                       Code type for all beams.
;
; :Common Blocks:
;   EP_ENVAR_CON_CB
;       pp_nbeam_min       - Minimum number of beams for a valid single chunk
;       pp_maxorder_runest - Maximum runner order to use
;       pp_gunid           - 0=use both guns, 1=GDU1, 2=GDU2
;       pp_maxchan_max     - Maximum value for maxchan
;       pp_maxchan_min     - Minimum value for maxchan
;       pp_qual_min        - Minimum quality value
;       pp_sc_base         - Spacecraft diameter
;       pp_phase0          - Angular offset from sun sensor to SCS X-axis.
;   EDI_PISO_ONESPIN_COMMON
;   EDI_PISO_ONECHUNK_COMMON
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015-10-07  -   Written by Matthew Argall. Adapted from edi_piso_onechunk written
;                           for Cluster's bestarg.
;-
pro edi_piso_onechunk, atime_out, btime_out, bwidth_out, $
                       data_out, nnn_out, mm_out, ct_out, mxch_out, $
                       gdotb_out, runstat_out, $
                       scs2bpp, bpp2scs, chunk_status, $
                       atime_t70, $
                       spin_srt_t70_pck, spin_srt_t70_fgm, $
                       spin_srt_ssm_pck, spin_srt_ssm_fgm, $
                       unitout, unitout2, $
                       runstruc1_oc, runstruc2_oc, $
                       PHASE0_OFFSET=phase0_offset, $
                       GDOTB_ERROR=gdotb_error, $
                       SCS2GSE=scs2gse, FLIP=flip, $
                       SCS2BPP_REFVEC=scs2bpp_refvec, $
                       NNN2=nnn_out_all, MM2=mm_out_all, CT2=ct_out_all

common ep_envar_con_cb
common edi_piso_onespin_common, spin_start_ssm, spin_width_sec, spin_srt_ssm
common edi_piso_onechunk_common, bx, by, bz, atime_ssm, btime1_ssm, btime2_ssm, t1, t1m2, VAX1, VAY1, VAX2, VAY2, I1, I2, SQ1, SQ2, NNN, mm, code_type, mxch1, mxch2

	;Define number of points and spin frequency
	ndata = n_elements(atime_ssm)
	omega = 2.D0*!dpi / spin_width_sec ; Hz

	;Temporal displacement from beginning of spin.
	;   - USE THE BEAM TIME FROM THE PICK LIBRARY INSTEAD OF CORRECTING THE
	;     ATIMES YOURSELF!!!!
	ddtime1 = btime2_ssm - spin_srt_ssm ; Timing for GUN1/DET2
	ddtime2 = btime1_ssm - spin_srt_ssm ; Timing for GUN2/DET1

	;Compute firing angles from gun voltages.
	vx1  = vax1 - 12800.            ; vax1 has already been X by 8.
	vy1  = vay1 - 12800.
	vv1  = sqrt(vx1*vx1+vy1*vy1)
	vx2  = vax2 - 12800.
	vy2  = vay2 - 12800.
	vv2  = sqrt(vx2*vx2+vy2*vy2)
	g1ph = !radeg * atan(vy1,vx1)
	g2ph = !radeg * atan(vy2,vx2)
	g1th = 79.0 * asin(vv1/13336.)
	g2th = 79.0 * asin(vv2/13336.)

	;Calculate the beam widths
	ep_calc_beamwidth, g1ph, g1th, g2ph, g2th, omega, ddtime1, ddtime2, $
	                   bx, by, bz, $
	                   bwidth1, bwidth2

	;Compute times of flight
	tof_scale = 1.e3/59.6046       ; convert (by dividing) tof in 59.6-ns to tof in mics (NOS)
	tof1 = t1/tof_scale            ; [mics]
	tof2 = (t1 - t1m2)/tof_scale   ; [mics]

;---------------------------------------------------------------------
; Firing Vectors in GDU1 Coordinates /////////////////////////////////
;---------------------------------------------------------------------

	;GD12 firing vetors in GDU1 coordinates
	;   - Involves converting from spherical to cartesian coordinates
	v1xg1 = sin(g1th*!dtor) * cos(g1ph*!dtor) ; beam 1 firing direction in gun1 coord...g1
	v1yg1 = sin(g1th*!dtor) * sin(g1ph*!dtor) ; x=spin axis, z=radial outward through gun1
	v1zg1 = cos(g1th*!dtor)         ; y=z ^ x

	;
	;GD21 firing vectors in GDU1 coordinates
	;   First, change from spherical to cartesian coordinates
	;   Second, rotation from GDU2 to GDU1 involves a 180 degree rotation about X
	;                |  1      0           0      |            | 1  0  0 |
	;       v_GDU1 = |  0  cos(theta)  sin(theta) | v_GDU2  =  | 0 -1  0 | v_GDU2
	;                |  0 -sin(theta)  cos(theta) |            | 0  0 -1 |
	;
	v2xg1 =  sin(g2th*!dtor) * cos(g2ph*!dtor) ; beam 2 firing direction in gun1 coord.!
	v2yg1 = -sin(g2th*!dtor) * sin(g2ph*!dtor) ;
	v2zg1 = -cos(g2th*!dtor)                   ;

	;Spin phase
	;   - Angular displacement from beginning of spin for each gun & vector
	;   - Correct for displacement of sun sensor from x-axis (want to align
	;     [data, x-axis sun] not [data, sun sensor, sun])
	phase1 = omega*(ddtime1[*])
	phase2 = omega*(ddtime2[*])
	cos_wt1 = cos(phase1[*] - pp_phase0)
	sin_wt1 = sin(phase1[*] - pp_phase0)
	cos_wt2 = cos(phase2[*] - pp_phase0)
	sin_wt2 = sin(phase2[*] - pp_phase0)

;---------------------------------------------------------------------
; Gun Positions in Inertial Coordinates //////////////////////////////
;---------------------------------------------------------------------

	;
	;Despin gun positions (fixed in s/c frame -> spinning in inertial frame)
	;   - Gun positions in inertial frame (no spin-axis component!)
	;   - Z_intertial is towards the sun!!!!
	;   - wt is the displacement angle
	;
	
	;GD12
	g1posy = -(pp_sc_base + 0.D0) * sin_wt1 ; Remeber: pp_sc_base = diameter of Cluster = radius of 'virtual' triangulation spacecraft (which is twice as big as Cluster)
	g1posz =  (pp_sc_base + 0.D0) * cos_wt1
	
	;GD21
	g2posy =  (pp_sc_base + 0.D0) * sin_wt2
	g2posz = -(pp_sc_base + 0.D0) * cos_wt2

	;
	;Switch to coordinate system with Z along spin axis:
	;        | 0  0  1 |
	;   v' = | 0 -1  0 | v
	;        | 1  0  0 |
	;

	;GD12
	g1posx =  g1posz
	g1posy = -g1posy

	;GD21
	g2posx =  g2posz
	g2posy = -g2posy

	;Z-component is zero
	g1posz = replicate(0.D0,ndata)
	g2posz = replicate(0.D0,ndata)

;---------------------------------------------------------------------
; Firing Vectors in Inertial Coordinates /////////////////////////////
;---------------------------------------------------------------------

	;Firing vectors in inertial frame
	;   - Despin; same as with gun positions
	
	;GD12
	g1y =  v1yg1 * cos_wt1 - v1zg1 * sin_wt1
	g1z =  v1yg1 * sin_wt1 + v1zg1 * cos_wt1

	;GD21
	g2y =  v2yg1 * cos_wt2 - v2zg1 * sin_wt2
	g2z =  v2yg1 * sin_wt2 + v2zg1 * cos_wt2

	;
	;Switch to coordinate system with Z along spin axis:
	;        | 0  0  1 |
	;   v' = | 0 -1  0 | v
	;        | 1  0  0 |
	;

	;GD12
	g1y = -g1y
	g1x =  g1z
	g1z =  v1xg1

	;GD21
	g2y = -g2y
	g2x =  g2z
	g2z =  v2xg1

;---------------------------------------------------------------------
; Angle between B and Firing Vectors /////////////////////////////////
;---------------------------------------------------------------------
	; Now have B in SCS and the gun firing directions in SCS
	; ([g1x,g1y,g1z] and [g2x,g2y,g2z]).  Take the dot products and return
	; the angle between firing vectors and the magnetic field direction.
	bmag   = sqrt(bx^2  + by^2  + bz^2)
	g1mag  = sqrt(g1x^2 + g1y^2 + g1z^2)
	g2mag  = sqrt(g2x^2 + g2y^2 + g2z^2)
	gdotb1 = acos((bx*g1x + by*g1y + bz*g1z) / bmag / g1mag) * 180d0/!dpi ; Degrees
	gdotb2 = acos((bx*g2x + by*g2y + bz*g2z) / bmag / g2mag) * 180d0/!dpi ; Degrees

;---------------------------------------------------------------------
; Transform to BPP ///////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Use B in SCS to construct scs2bpp. Then use scs2gse to construct
	;transformations between SCS, GSE, and BPP
	bhat_scs = [bx[0],by[0],bz[0]] / sqrt(bx[0]^2 + by[0]^2 + bz[0]^2)
	ep_construct_scs2bpp, bhat_scs, scs2gse, $ ; IN
	                      flip, scs2bpp_refvec, scs2bpp, bpp2scs ; OUT


	; Gun1 position
	gunx1 = scs2bpp[0,0]*g1posx + scs2bpp[0,1]*g1posy + scs2bpp[0,2]*g1posz
	guny1 = scs2bpp[1,0]*g1posx + scs2bpp[1,1]*g1posy + scs2bpp[1,2]*g1posz
	gunz1 = scs2bpp[2,0]*g1posx + scs2bpp[2,1]*g1posy + scs2bpp[2,2]*g1posz

	; Gun2 position
	gunx2 = scs2bpp[0,0]*g2posx + scs2bpp[0,1]*g2posy + scs2bpp[0,2]*g2posz
	guny2 = scs2bpp[1,0]*g2posx + scs2bpp[1,1]*g2posy + scs2bpp[1,2]*g2posz
	gunz2 = scs2bpp[2,0]*g2posx + scs2bpp[2,1]*g2posy + scs2bpp[2,2]*g2posz

	; Gun1 firing direction/angle
	gun1firx_bpp = scs2bpp[0,0]*g1x + scs2bpp[0,1]*g1y + scs2bpp[0,2]*g1z
	gun1firy_bpp = scs2bpp[1,0]*g1x + scs2bpp[1,1]*g1y + scs2bpp[1,2]*g1z
	gun1firz_bpp = scs2bpp[2,0]*g1x + scs2bpp[2,1]*g1y + scs2bpp[2,2]*g1z

	; Gun2 firing direction
	gun2firx_bpp = scs2bpp[0,0]*g2x + scs2bpp[0,1]*g2y + scs2bpp[0,2]*g2z
	gun2firy_bpp = scs2bpp[1,0]*g2x + scs2bpp[1,1]*g2y + scs2bpp[1,2]*g2z
	gun2firz_bpp = scs2bpp[2,0]*g2x + scs2bpp[2,1]*g2y + scs2bpp[2,2]*g2z
	
	;Firing angles in BPP
	firaz1 = atan(gun1firy_bpp, gun1firx_bpp) * !radeg
	firaz2 = atan(gun2firy_bpp, gun2firx_bpp) * !radeg

;---------------------------------------------------------------------
; Output /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Allocate memory
	;   - Data from GDU1 and GDU2 will be concatenated
	dimout      = long(float(ndata)*2.)
	data_out    = fltarr(7,dimout)
	atime_out   = dblarr(dimout)
	btime_out   = dblarr(dimout)
	bwidth_out  = fltarr(dimout)
	mxch_out    = bytarr(dimout)
	gdotb_out   = fltarr(dimout)
	dimout2     = long(3.*float(pp_maxorder_runest) + 2.)
	runstat_out = dblarr(dimout2,dimout)
	; Here is the structure when pp_maxorder_runest = 6
	; runstat_out(0,*)     = runorder(*)
	; runstat_out(1:6,*)   = flag(0:5,*)
	; runstat_out(7:12,*)  = estof(0:5,*)
	; runstat_out(13,*)    = tg(*)
	; runstat_out(14:19,*) = prob(0:5,*)

	nnn_keep = lonarr(dimout)
	mm_keep  = lonarr(dimout)
	ct_keep  = lonarr(dimout)
	istart = 0
	nout   = 0
	
	;Find GD12
	gd1      = where((sq2   ge pp_qual_min)    and $
	                 (mxch2 ge pp_maxchan_min) and $
	                 (mxch2 le pp_maxchan_max)) ; Gun1/Det2 remember...
	
	;Find GD21
	gd2 = where((sq1 ge pp_qual_min)      and $
	            (mxch1 ge pp_maxchan_min) and $
	            (mxch1 le pp_maxchan_max)) ; Gun2/Det1
	
	;GD12
	; Beginning of array is for GD12, end for GD21
	;   DATA_OUT[0] - Gun ID
	;   DATA_OUT[1] - Quality
	;   DATA_OUT[2] - X Gun Position in BPP
	;   DATA_OUT[3] - Y Gun Position in BPP
	;   DATA_OUT[4] - Firing angle in BPP
	;   DATA_OUT[5] - Time of flight
	;   DATA_OUT[6] - Code type [short/long]
	;
	;   RUNSTAT_OUT[0]                       - Runner order
	;   RUNSTAT_OUT[1:MAXORDER]              - Flag
	;   RUNSTAT_OUT[MAXORDER+1:2*MAXORDER]   - Estimated single runner ToF
	;   RUNSTAT_OUT[2*MAXORDER+1]            - Gyrotime
	;   RUNSTAT_OUT[2*MAXORDER+2]            - Probability
	if [[gd1[0] ne -1] and [pp_gunid le 1]] then begin
		ngd1 = n_elements(gd1)
		data_out[0,0:ngd1-1] = 1.0             ; Gun 1
		data_out[1,0:ngd1-1] = float(sq2[gd1]) ; Detector 2
		data_out[2,0:ngd1-1] = gunx1[gd1]      ; Gun 1
		data_out[3,0:ngd1-1] = guny1[gd1]      ; Gun 1
		data_out[4,0:ngd1-1] = firaz1[gd1]     ; Gun 1
		data_out[5,0:ngd1-1] = tof2[gd1]       ; Detector 2
		data_out[6,0:ngd1-1] = code_type[gd1]
		btime_out[0:ngd1-1]  = btime2_ssm[gd1] ; Detector 2
		bwidth_out[0:ngd1-1] = bwidth1[gd1]    ; Gun 1
		atime_out[0:ngd1-1]  = atime_ssm[gd1]
		mxch_out[0:ngd1-1]   = mxch2[gd1]      ; Detector 2
		gdotb_out[0:ngd1-1]  = gdotb1[gd1]     ; Gun 1

		;Runner Order
		runstat_out[0,0:ngd1-1] = runstruc2_oc[gd1].runorder ; DET2

		;Flags
		runstat_out[1:pp_maxorder_runest,0:ngd1-1] = $
			runstruc2_oc[gd1].flag[0:pp_maxorder_runest-1]   ; DET2

		;Estof
		runstat_out[pp_maxorder_runest+1:2*pp_maxorder_runest,0:ngd1-1] = $
			runstruc2_oc[gd1].estof[0:pp_maxorder_runest-1]  ; DET2

		;Gyrotime
		runstat_out[2*pp_maxorder_runest+1,0:ngd1-1] = $
			runstruc2_oc[gd1].tg

		;Probability
		runstat_out[2*pp_maxorder_runest+2:3*pp_maxorder_runest+1,0:ngd1-1] = $
			runstruc2_oc[gd1].prob[0:pp_maxorder_runest-1]   ; DET2

		nnn_keep[0:ngd1-1] = nnn[gd1]
		mm_keep[0:ngd1-1]  = mm[gd1]
		ct_keep[0:ngd1-1]  = code_type[gd1]
		istart             = ngd1
		nout               = nout + ngd1
	endif

	;GD21 -- same process
	if [[gd2[0] ne -1] and [[pp_gunid eq 0] or [pp_gunid eq 2]]] then begin
		ngd2 = n_elements(gd2)
		data_out[0,istart:istart+ngd2-1] = 2.0             ; Gun2
		data_out[1,istart:istart+ngd2-1] = float(sq1[gd2]) ; Detector 1
		data_out[2,istart:istart+ngd2-1] = gunx2[gd2]      ; Gun2
		data_out[3,istart:istart+ngd2-1] = guny2[gd2]      ; Gun2
		data_out[4,istart:istart+ngd2-1] = firaz2[gd2]     ; Gun2
		data_out[5,istart:istart+ngd2-1] = tof1[gd2]       ; Detector 1
		data_out[6,istart:istart+ngd2-1] = code_type[gd2]
		btime_out[istart:istart+ngd2-1]  = btime1_ssm[gd2] ; Detector 1
		bwidth_out[istart:istart+ngd2-1] = bwidth2[gd2]    ; Gun2
		atime_out[istart:istart+ngd2-1]  = atime_ssm[gd2]
		mxch_out[istart:istart+ngd2-1]   = mxch1[gd2]      ; Detector 1
		gdotb_out[istart:istart+ngd2-1]  = gdotb2[gd2]     ; Gun2

		runstat_out[0,istart:istart+ngd2-1] = runstruc1_oc[gd2].runorder ; DET1
		runstat_out[1:pp_maxorder_runest,istart:istart+ngd2-1] = $
			runstruc1_oc[gd2].flag[0:pp_maxorder_runest-1] ; DET1
		runstat_out[pp_maxorder_runest+1:2*pp_maxorder_runest, istart:istart+ngd2-1] = $
			runstruc1_oc[gd2].estof[0:pp_maxorder_runest-1] ; DET1
		runstat_out[2*pp_maxorder_runest+1,istart:istart+ngd2-1] = $
			runstruc1_oc[gd2].tg
		runstat_out[2*pp_maxorder_runest+2:3*pp_maxorder_runest+1, istart:istart+ngd2-1] = $
			runstruc1_oc[gd2].prob[0:pp_maxorder_runest-1] ; DET1

		nnn_keep[istart:istart+ngd2-1] = nnn[gd2]
		mm_keep[istart:istart+ngd2-1]  = mm[gd2]
		ct_keep[istart:istart+ngd2-1]  = code_type[gd2]
		nout = nout + ngd2
	endif

;---------------------------------------------------------------------
; Trim ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Do we have enough beams to continue?
	if (nout ge pp_nbeam_min) then begin

		;Trim results
		data_out    = data_out[0:6,0:nout-1]
		atime_out   = atime_out[0:nout-1]
		btime_out   = btime_out[0:nout-1]
		bwidth_out  = bwidth_out[0:nout-1]
		mxch_out    = mxch_out[0:nout-1]
		gdotb_out   = gdotb_out[0:nout-1]
		runstat_out = runstat_out[0:3*pp_maxorder_runest+1,0:nout-1]
		nnn_keep    = nnn_keep[0:nout-1]
		mm_keep     = mm_keep[0:nout-1]
		ct_keep     = ct_keep[0:nout-1]

	; Decided we want these quantities at the beamtime resolution afterall
		nnn_out_all = nnn_keep
		mm_out_all  = mm_keep
		ct_out_all  = ct_keep

	; Paschmann says this check on whether or not nnn or mm changes within
	; this chunk interval isn't necessary...Just take the nnn and mm
	; values for the 1st beam...This goes for the code type too...

	;decom ; nnn and mm check
	;decom     if [min[nnn_keep] ne max[nnn_keep]] then message, $
	;decom       'nnn changes during this chunk interval...problem for bestarg.pro'
	;decom     if [min[mm_keep] ne max[mm_keep]] then message, $
	;decom       'mm changes during this chunk interval...problem for bestarg.pro'

		nnn_out = nnn_keep[0]
		mm_out  = mm_keep[0]
		ct_out  = ct_keep[0]

		chunk_status = 1
		
	;Not enough beams pass criteria
	endif else begin
		chunk_status = 0
	endif
end


