;+
;   View the results of rotating beams into their bpp.
;
; :Params:
;       DATA_STRUCT:        in, required, type=structure
;                           Results of the averaging and BPP-finding process.
;
; :Keywords:
;       DOCS:               in, optional, type=boolean, default=0
;                           If set, data is given in despun OCS instead of BPP.
;-
pro mms_test_costFn_update, win, t, B, d_bpp, fv_bpp, pos_bpp, gun_id, $
FILENAME=filename, $
TITLE=title
	compile_opt idl2
	on_error, 2

	;Save the file?
	if n_elements(filename) eq 0 then filename = ''
	
	;Create a window if necessary
	if n_elements(win) eq 0 || ~obj_valid(win) then begin
		buffer = filename eq '' ? 0 : 1
		win = mms_test_costFn_win(B, BUFFER=buffer)
	endif
	
	;Default title
	if n_elements(title) eq 0 then begin
		;Breakdown timestamp
		MrCDF_Epoch_Breakdown, t, yr0, mo0, day0, hr0, min0, sec0
		title = 'Beam intersections in BPP'                                                   + '!C' + $
		        string(FORMAT='(%"%04i-%02i-%02i %02i:%02i:%02i")',yr0, mo0, day0, hr0, min0) + '!C' + $
		        string(FORMAT='(%"d = [%0.2f, %0.2f, %0.2f]")', d_bpp)                        + '!C' + $
		        string(FORMAT='(%"B = [%0.2f, %0.2f, %0.2f]")', B)
	endif

	;Turn refresh off immediately
	win -> refresh, /DISABLE

;-----------------------------------------------------
; Create S/C \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Spacecraft outline
	;   - Create a circle with a radius that extends from optics to gun.
	nVerts           = 200
	radius           = mms_instr_origins_instr('EDI1_GUN', 'EDI2_DETECTOR')
	sc_sphr_ocs      = fltarr(3, nVerts)
	sc_sphr_ocs[2,*] = sqrt(total(radius^2))
	sc_sphr_ocs[0,*] = 2.0 * !pi * findgen(nVerts)/(nVerts-1.0)
	sc_xyz_ocs       = cv_coord(FROM_SPHERE=sc_sphr_ocs, /TO_RECT)
	
	;Rotate into BPP
	xyz2bpp    = mms_edi_xxyz2bpp(B)
	sc_xyz_bpp = mrvector_rotate(xyz2bpp, sc_xyz_ocs)
	
	;Plot range
	range = 1.75 * [-sc_sphr_ocs[2,0], sc_sphr_ocs[2,0]]

;-----------------------------------------------------
; Setup the Axes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	gAxes = win['Beam Plot']
	gAxes.title = title

;-----------------------------------------------------
; Draw the Spacecraft \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	gSC = win['SC Outline']
	gSC -> SetData, reform(sc_xyz_bpp[0,*]), reform(sc_xyz_bpp[1,*])

;-----------------------------------------------------
; Draw Gun Positions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Get graphics and associated indices
	gGun1 = win['Gun1']
	gGun2 = win['Gun2']
	igun1 = where(gun_id eq 1, ngun1, COMPLEMENT=igun2, NCOMPLEMENT=ngun2)

	;Update gun positions
	if ngun1 gt 0 then begin
		gGun1.sym_size = 2.0
		gGun1 -> SetData, reform(pos_bpp[0,igun1]), reform(pos_bpp[1,igun1])
	endif else begin
		gGun1.sym_size = 0.0
	endelse
		
	if ngun2 gt 0 then begin
		gGun2.sym_size = 2.0
		gGun2 -> SetData, reform(pos_bpp[0,igun2]), reform(pos_bpp[1,igun2])
	endif else begin
		gGun2.sym_size = 0.0
	endelse

;-----------------------------------------------------
; Firing Vectors \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	gFire1 = win['Firing Vectors GD12']
	gFire2 = win['Firing Vectors GD21']

	;GUN1
	if ngun1 gt 0 then begin
		;Beam slope, y-intercept, (x1,x2) and (y1,y2)
		;   - slope (m)       = rise / run
		;   - y-intercept (b) = y1 - m * x1
		;   - (x1,x2)         = range
		;   - (y1,y2)         = m*x + b
		m     = reform( fv_bpp[1,igun1] / fv_bpp[0,igun1] )
		b     = reform( pos_bpp[1,igun1] - pos_bpp[0,igun1] * m )
		x_bpp = rebin( [range[0], range[1]], 2, ngun1 )
		y_bpp = transpose( [[m * x_bpp[0,*] + b], [m * x_bpp[1,*] + b]] )

		;Define connectivity
		;   - Make (x1,x2) and (y1,y2) pairs adjacent
		;   - Indicate connectivity: [2,       2,       2,
		;                                0, 1,    2, 3,    4, 5, ...]
		g1_fv_xbpp = reform(x_bpp, 2 * ngun1)
		g1_fv_ybpp = reform(y_bpp, 2 * ngun1)
		g1_fv_conn = reform([replicate(2,1,ngun1), lindgen(2,ngun1)], 3*ngun1)
		
		;Draw the firing vectors
		gFire1.hide = 0
		gFire1 -> SetData, g1_fv_xbpp, g1_fv_ybpp, CONNECTIVITY=g1_fv_conn
	endif else begin
		gFire1.hide = 1
	endelse
	
	;GUN2
	if ngun2 gt 0 then begin
		;Beam slope, y-intercept, (x1,x2) and (y1,y2)
		m     = reform( fv_bpp[1,igun2] / fv_bpp[0,igun2] )
		b     = reform( pos_bpp[1,igun2] - pos_bpp[0,igun2] * m )
		x_bpp = rebin( [range[0], range[1]], 2, ngun2)
		y_bpp = transpose( [[m * x_bpp[0,*] + b], [m * x_bpp[1,*] + b]] )
		
		;Connectivity
		g2_fv_xbpp = reform(x_bpp, 2 * ngun2)
		g2_fv_ybpp = reform(y_bpp, 2 * ngun2)
		g2_fv_conn = reform([replicate(2,1,ngun2), lindgen(2,ngun2)], 3*ngun2)
		
		;Draw the firing vectors
		gFire2.hide = 0
		gFire2 -> SetData, g2_fv_xbpp, g2_fv_ybpp, CONNECTIVITY=g2_fv_conn
	endif else begin
		gFire2.hide = 1
	endelse

;-----------------------------------------------------
; Virtual Source \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Draw drift step
	gTarget  = win['Target']
	gTarget -> SetData, -d_bpp[0], -d_bpp[1]

;-----------------------------------------------------
; Save \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	win -> Refresh
	
	;Save?
	if filename ne '' then begin
		gAxes -> Save, filename
		print, 'Image saved to "' + filename + '".'
	endif
end




;+
;   View the results of rotating beams into their bpp.
;
; :Params:
;       DATA_STRUCT:        in, required, type=structure
;                           Results of the averaging and BPP-finding process.
;
; :Keywords:
;       DOCS:               in, optional, type=boolean, default=0
;                           If set, data is given in despun OCS instead of BPP.
;-
function mms_test_costFn_win, B, $
BUFFER=buffer
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Create Window \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Dimensions of 2:1 window
	wdims   = get_screen_size()
	waspect = wdims[0] / wdims[1]
	winy    = 500
	winx    = fix(winy * waspect)
	
	;Center it on the screen
	center = wdims / 2
	corner = center + [-winx/2, -winy/2]
	
	;Create window
	win = window(DIMENSIONS=[winx, winy], LOCATION=corner, BUFFER=buffer)
	win -> Refresh, /DISABLE

;-----------------------------------------------------
; Create S/C \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Spacecraft outline
	;   - Create a circle with a radius that extends from optics to gun.
	nVerts           = 200
	radius           = mms_instr_origins_instr('EDI1_GUN', 'EDI2_DETECTOR')
	sc_sphr_ocs      = fltarr(3, nVerts)
	sc_sphr_ocs[2,*] = sqrt(total(radius^2))
	sc_sphr_ocs[0,*] = 2.0 * !pi * findgen(nVerts)/(nVerts-1.0)
	sc_xyz_ocs       = cv_coord(FROM_SPHERE=sc_sphr_ocs, /TO_RECT)
	
	;Rotate into BPP
	xyz2bpp    = mms_edi_xxyz2bpp(B)
	sc_xyz_bpp = mrvector_rotate(xyz2bpp, sc_xyz_ocs)

;-----------------------------------------------------
; Setup the Axes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Draw a set of axes 1.5 times bigger than the s/c
	range = 1.75 * [-sc_sphr_ocs[2,0], sc_sphr_ocs[2,0]]
	gAxes = plot(range, range, /NODATA, /CURRENT, $
	             ASPECT_RATIO = 1.0, $
	             MARGIN       = [0.13, 0.1, 0.04, 0.2], $
	             NAME         = 'Beam Plot', $
	             TITLE        = 'Beam intersections in $B_{Avg}$ BPP', $
	             XRANGE       = range, $
	             XSTYLE       = 1, $
	             XTITLE       = 'Distance (m)', $
	             YRANGE       = range, $
	             YSTYLE       = 1, $
	             YTITLE       = 'Distance (m)')

;-----------------------------------------------------
; Draw the Spacecraft \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	gSC = polygon(reform(sc_xyz_bpp[0,*]), reform(sc_xyz_bpp[1,*]), $
	                /DATA, $
	                NAME   = 'SC Outline', $
	                TARGET = gAxes)

;-----------------------------------------------------
; Draw Gun Positions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	gGun1 = Symbol(sc_xyz_bpp[0,0], sc_xyz_bpp[1,0], 'circle', $
	               /DATA, $
	               NAME      = 'Gun1', $
	               SYM_COLOR = 'Blue', $
	               SYM_SIZE  = 2.0, $
	               TARGET    = gAxes)
	
	gGun2 = Symbol(sc_xyz_bpp[0,0], sc_xyz_bpp[1,0], 'circle', $
	               /DATA, $
	               NAME      = 'Gun2', $
	               SYM_COLOR = 'Red', $
	               SYM_SIZE  = 2.0, $
	               TARGET    = gAxes)

;-----------------------------------------------------
; Firing Vectors \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Draw the firing vectors
	gFire1 = Polyline(range, range, $
	                  COLOR        = 'Blue', $
	                  /CLIP, $
	                  /DATA, $
	                  TARGET       = gAxes, $
	                  NAME         = 'Firing Vectors GD12')
	
	;Draw the firing vectors
	gFire2 = Polyline(range, range, $
	                  COLOR        = 'red', $
	                  /CLIP, $
	                  /DATA, $
	                  TARGET       = gAxes, $
	                  NAME         = 'Firing Vectors GD21')

;-----------------------------------------------------
; Virtual Source \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Draw drift step
	gTarget = symbol( 0, 0, 'X', /DATA, $
	                 NAME      = 'Target', $
	                 TARGET    = gAxes, $
	                 SYM_COLOR = 'Black', $
	                 SYM_THICK = 2, $
	                 SYM_SIZE  = 3.0)

;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	win -> Refresh
	return, win
end


;+
;
;-
function mms_edi_test_costFn, sc, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		void = cgErrorMsg()
		return, !Null
	endif

	sc       = 'mms2'                 ;Slow                            ;Fast
	tstart   = '2015-05-09T15:30:00Z' ;01-Aug-2015 05:08:46.030963219  01-Aug-2015 00:41:57.83222159
	tend     = '2015-05-09T16:30:00Z' ;01-Aug-2015 13:19:18.340386093  01-Aug-2015 05:07:01.822075718
	edi_dir  = '/nfs/edi/temp/'
	sdc_dir  = '/nfs/'
	hk_dir   = '/nfs/hk/'
	att_dir  = filepath('', ROOT_DIR='/nfs', SUBDIRECTORY=['ancillary', sc, 'defatt'])
	filename = ''
	quality  = 3
	method   = 1
	dt       = 5
	save_dir = '/nfs/edi/temp/'
	view_bpp = 1
;	png_dir  = ''
	png_dir  = '/nfs/edi/beam_plots/'

;-------------------------------------------------------
; Search for Files /////////////////////////////////////
;-------------------------------------------------------
	
	;EDI QL FAST file
	instr   = 'edi'
	mode    = 'fast'
	level   = 'l1a'
	optdesc = 'efield'
	ffast_edi = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfast_edi, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	;Pretend we have no fast files
	ffast_edi = ''
	nfast_edi = 0
	
	;EDI QL SLOW file
	instr   = 'edi'
	mode    = 'slow'
	level   = 'l1a'
	optdesc = 'efield'
	fslow_edi = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nslow_edi, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	if nfast_edi + nslow_edi eq 0 then message, 'No EDI fast or slow survey files found.'
	
	;FG L1B file
	instr   = 'dfg'
	mode    = 'srvy'
	level   = 'l1b'
	optdesc = ''
	files_fg = mms_find_file(sc, instr, mode, level, $
	                         COUNT     = nfiles_fg, $
	                         OPTDESC   = optdesc, $
	                         SDC_ROOT  = sdc_dir, $
	                         SEARCHSTR = searchstr, $
	                         TSTART    = tstart, $
	                         TEND      = tend)
	if nfiles_fg eq 0 then message, 'No FG files found: "' + searchstr + '".'
	
	;DSS file
	instr   = 'fields'
	mode    = 'hk'
	level   = 'l1b'
	optdesc = '101'
	files_dss = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles_dss, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = HK_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	if nfiles_dss eq 0 then message, 'No FG files found: "' + searchstr + '".'

	; Attitude file
	;   - Do not throw errors for attitude files. They are used only
	;     to rotate from BCS to SMPA, which is very nearly a unitary
	;     transformation.
	;   - Sunpulse times are used to despin data.
	str = filepath( ROOT_DIR=att_dir, strupcase(sc) + '_DEFATT_%Y%D_%Y%D.V*' );
	files_att = MrFile_Search( str, $
	                           /CLOSEST, $
	                           COUNT     = nfiles_att, $
	                           TSTART    = tstart, $
	                           TEND      = tend, $
	                           TIMEORDER = '%Y%D', $
	                           VREGEX    = 'V([0-9]{2})' )

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Attitude
	if nfiles_att gt 0 then begin
		defatt = mms_fdoa_read_defatt(files_att, tstart, tend, HEADER=att_hdr)
		zmpa   = att_hdr.zmpa[*,0]
	endif
	
	;Sunpulse
	sunpulse = mms_dss_read_sunpulse(files_dss, tstart, tend, /UNIQ_PULSE)
	
	;FG
	fg_l1b = mms_fg_read_l1b(files_fg, tstart, tend)
	
	;EDI Slow
	if nslow_edi gt 0 then begin
		edi_slow_l1b = mms_edi_create_l1b(fslow_edi, tstart, tend, $
		                                  /CS_123, $
		                                  /CS_BCS, $
		                                  QUALITY = quality)
	endif
	
	;EDI Fast
	if nfast_edi gt 0 then begin
		edi_fast_l1b = mms_edi_create_l1b(ffast_edi, tstart, tend, $
		                                  /CS_123, $
		                                  /CS_BCS, $
		                                  QUALITY = quality)
	endif
	
	;Comine slow and fast into survey
	edi_srvy_l1b = mms_edi_combine_srvy(edi_slow_l1b, edi_fast_l1b)

;-----------------------------------------------------
; Compute Average B \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Time range that we have data
	if edi_srvy_l1b.count_gd12 eq 0 && edi_srvy_l1b.count_gd21 eq 0 then begin
		message, 'No EDI E-field data available.'
	endif else if edi_srvy_l1b.count_gd12 eq 0 then begin
		t0 = edi_srvy_l1b.tt2000_gd21[0]
		t1 = edi_srvy_l1b.tt2000_gd21[-1]
	endif else if edi_srvy_l1b.count_gd21 eq 0 then begin
		t0 = edi_srvy_l1b.tt2000_gd12[0]
		t1 = edi_srvy_l1b.tt2000_gd12[-1]
	endif else begin
		t0 = min( [ edi_srvy_l1b.tt2000_gd12[0],  edi_srvy_l1b.tt2000_gd21[0]  ] )
		t1 = max( [ edi_srvy_l1b.tt2000_gd12[-1], edi_srvy_l1b.tt2000_gd21[-1] ] )
	end
	
	;Breakdown into time vectors
	MrCDF_Epoch_Breakdown, [t0, t1], yr, mo, day, hr, mnt, sec
	
	;Round down to the nearest DT seconds and recompute
	sec = sec - (sec mod dt)
	MrCDF_Epoch_Compute, tedge, yr, mo, day, hr, mnt, sec, /TT2000
	t0  = tedge[0]
	t1  = tedge[1] + long64(dt * 1d9)

	;Find FGM data within this time interval
	irange   = MrIndexRange( fg_l1b.tt2000, [t0, t1] )
	t_fg     = fg_l1b.tt2000[ irange[0]:irange[1] ]
	b_fg_bcs = fg_l1b.b_bcs[0:2, irange[0]:irange[1] ]

	;Compute the averaged magnetic field
	b_avg_bcs = mms_edi_bavg(t_fg, b_fg_bcs, edi_srvy_l1b.tt2000_gd12, edi_srvy_l1b.tt2000_gd21, DT=dt)

;-----------------------------------------------------
; Beam Width \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Number of beam hits per gun
	n12 = edi_srvy_l1b.count_gd12
	n21 = edi_srvy_l1b.count_gd21

	;Rotate interpolate B-field into the EDI1 coordinate system
	bcs2edi1     = mms_instr_xxyz2instr('BCS',  'EDI1')
	b_gd12_edi1  = mrvector_rotate( bcs2edi1,  b_avg_bcs.b_gd12 )
	b_gd21_edi1  = mrvector_rotate( bcs2edi1,  b_avg_bcs.b_gd21 )
	
	;Combine data
	bx     = [ reform(b_gd12_edi1[0,*]),  reform(b_gd21_edi1[0,*])  ]
	by     = [ reform(b_gd12_edi1[1,*]),  reform(b_gd21_edi1[1,*])  ]
	bz     = [ reform(b_gd12_edi1[2,*]),  reform(b_gd21_edi1[2,*])  ]
	fa_az  = [ edi_srvy_l1b.azimuth_gd12, edi_srvy_l1b.azimuth_gd21 ]
	fa_pol = [ edi_srvy_l1b.polar_gd12,   edi_srvy_l1b.polar_gd21   ]
	gun_id = [ bytarr(n12) + 1B,          bytarr(n21) + 2B          ]
	
	;Beam width
	beam_width = mmsedi_beamwidth( fa_az, fa_pol, bx, by, bz, gun_id )
	
	;Delete variables
	bcs2edi1    = !Null
	b_gd12_edi1 = !Null
	b_gd21_edi1 = !Null
	bx          = !Null
	by          = !Null
	bz          = !Null
	fa_az       = !Null
	fa_pol      = !Null
;	gun_id      = !Null

;-----------------------------------------------------
; Rotate BCS to SMPA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;BCS -> SMPA
	bcs2smpa = mms_fg_xbcs2smpa(zmpa)
	
	;Averaged data
	t_avg       = b_avg_bcs.t_avg
	b_avg_smpa  = mrvector_rotate(bcs2smpa, b_avg_bcs.b_avg)
	b_gd12_smpa = mrvector_rotate(bcs2smpa, b_avg_bcs.b_gd12)
	b_gd21_smpa = mrvector_rotate(bcs2smpa, b_avg_bcs.b_gd21)
	recnum      = b_avg_bcs.recnum
	recnum_gd12 = b_avg_bcs.recnum_gd12
	recnum_gd21 = b_avg_bcs.recnum_gd21
	b_avg_bcs   = !Null
	
	;Beam data
	t_gd12       = edi_srvy_l1b.tt2000_gd12
	t_gd21       = edi_srvy_l1b.tt2000_gd21
	pos_vg1_smpa = mrvector_rotate(bcs2smpa, edi_srvy_l1b.virtual_gun1_bcs)
	pos_vg2_smpa = mrvector_rotate(bcs2smpa, edi_srvy_l1b.virtual_gun2_bcs)
	fv_gd12_smpa = mrvector_rotate(bcs2smpa, edi_srvy_l1b.fv_gd12_bcs)
	fv_gd21_smpa = mrvector_rotate(bcs2smpa, edi_srvy_l1b.fv_gd21_bcs)
	edi_srvy_l1b = !Null

;-----------------------------------------------------
; Despin SMPA -> DMPA & Combine \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;SMPA -> DMPA
	smpa2dmpa_avg   = mms_dss_xdespin(sunpulse, t_avg)
	smpa2dmpa_beam1 = mms_dss_xdespin(sunpulse, t_gd12)
	smpa2dmpa_beam2 = mms_dss_xdespin(sunpulse, t_gd21)
	
	;Averaged data
	b_avg_dmpa =     mrvector_rotate( smpa2dmpa_avg,   temporary(b_avg_smpa) )
	b_gdu_dmpa = [ [ mrvector_rotate( smpa2dmpa_beam1, temporary(b_gd12_smpa) ) ], $
	               [ mrvector_rotate( smpa2dmpa_beam2, temporary(b_gd21_smpa) ) ] ]
	recnum_gdu = [ temporary(recnum_gd12), temporary(recnum_gd21) ]
	
	;Beam data
	pos_vg_dmpa = [ [ mrvector_rotate( smpa2dmpa_beam1, temporary(pos_vg1_smpa) ) ], $
	                [ mrvector_rotate( smpa2dmpa_beam2, temporary(pos_vg2_smpa) ) ] ]
	fv_gdu_dmpa = [ [ mrvector_rotate( smpa2dmpa_beam1, temporary(fv_gd12_smpa) ) ], $
	                [ mrvector_rotate( smpa2dmpa_beam2, temporary(fv_gd21_smpa) ) ] ]
	
	;Delete data
	smpa2dmpa_avg   = !Null
	smpa2dmpa_beam1 = !Null
	smpa2dmpa_beam2 = !Null

;-----------------------------------------------------
; Grid of Targets \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	rmin    = 0.0
	rmax    = 5.0
	rstep   = 0.1
	phimin  = 0.0
	phistep = 0.1
	phimax  = 2.0 * !pi - phistep
	grid    = mmsedi_polargrid(phimin, phimax, phistep, rmin, rmax, rstep)

;-----------------------------------------------------
; Cost Function \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Allocate memory
	N       = n_elements(t_avg)
	d_bpp   = fltarr(3, N)
	d_delta = fltarr(3, N)

	;Step through each analysis interval
	for i = 0, N - 1 do begin
		;Time and magnetic field for current interval
		t = t_avg[i]
		b = b_avg_dmpa[*,i]
		
		;Indices associated with the current interval
		inds = where(recnum_gdu eq recnum[i], ngdu)

	;-----------------------------------------------------
	; Rotate into BPP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AVERAGE FIELD
		if method eq 1 then begin
			xyz2bpp = mms_edi_xxyz2bpp(B)
		;INSTANTANEOUS FIELD
		endif else if method eq 2 then begin
			xyz2bpp = mms_edi_xyz2bpp(b_gdu_dmpa[inds])
		endif else begin
			message, 'METHOD must be 1 or 2.'
		endelse
		
		;Rotate into BPP
		fv_bpp  = mrvector_rotate( xyz2bpp, fv_gdu_dmpa[*,inds] )
		pos_bpp = mrvector_rotate( xyz2bpp, pos_vg_dmpa[*,inds] ) 
		bw      = beam_width[inds]

	;-----------------------------------------------------
	; Cost Function & Drift Step \\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		costFn = mmsedi_costfunction( fv_bpp[0,*],  fv_bpp[1,*], $
		                              pos_bpp[0,*], pos_bpp[1,*], bw, grid)

		;
		; The virtual source point is located at the grid point
		; where the minimum of the cost function occurs. The drift
		; step points from the virtual source point to the detector.
		;
		delta        =  min(costFn, idelta)
		d_bpp[0,i]   = -grid[idelta].x
		d_bpp[1,i]   = -grid[idelta].y
		d_delta[1,i] =  delta

	;-----------------------------------------------------
	; View Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if view_bpp then begin
			;Breakdown timestamp
			MrCDF_Epoch_Breakdown, t,                yr0, mo0, day0, hr0, min0, sec0
			MrCDF_Epoch_Breakdown, t+long64(dt*1d9), yr1, mo1, day1, hr1, min1, sec1
			tstart = string(FORMAT='(%"%04i%02i%02i%02i%02i%02i")', yr0, mo0, day0, hr0, min0, sec0)
			
			;Filename
			if png_dir ne '' then begin
				;Output do a daily file
				png_out = filepath(ROOT_DIR=png_dir, string(FORMAT='(%"%04i%02i%02i")', yr0, mo0, day0))
				if ~file_test(png_out, /DIRECTORY) then file_mkdir, png_out
			
				;Create the file name
				filename = mms_construct_filename(sc, 'edi', 'srvy', 'ql', $
				                                  DIRECTORY = png_out, $
				                                  OPTDESC   = 'driftstep-idl', $
				                                  TSTART    = tstart )
				filename = strmid(filename, 0, strpos(filename, '_v*', /REVERSE_SEARCH)) + '.png'
			endif
			
			;Create title
			title = 'Beam intersections in $B_{Avg}$ BPP'                       + '!C' + $
			        string(FORMAT='(%"%s %04i-%02i-%02i %02i:%02i:%02i - %02i:%02i:%02i")', $
			        sc, yr0, mo0, day0, hr0, min0, sec0, hr1, min1, sec1)       + '!C' + $
			        string(FORMAT='(%"d = [%0.2f, %0.2f, %0.2f]")', d_bpp[*,i]) + '!C' + $
			        string(FORMAT='(%"B = [%0.2f, %0.2f, %0.2f]")', B)

			;Create the window
			mms_test_costFn_update, win, t, b, d_bpp[*,i], fv_bpp, pos_bpp, gun_id[inds], $
			                        FILENAME = filename, $
			                        TITLE    = title
		end
	endfor

;-----------------------------------------------------
; BPP -> DMPA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Data must be rotated out of BPP with a magnetic field
	;common to the analysis window -- this is the average
	;B-field
	xyz2bpp = mms_edi_xxyz2bpp(b_avg_dmpa)
	bpp2xyz = transpose(xyz2bpp, [1,0,2])
	
	;Transform data
	d_dmpa = mrvector_rotate(bpp2xyz, d_bpp)

;-----------------------------------------------------
; Drift Velocity & Electric Field \\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Calculate gyro-period
	me     = 9.10938291d-31
	q      = 1.60217657d-19
	b_mag  = mrvector_magnitude( b_avg_dmpa )
	T_gyro = rebin( reform( 2.0*!dpi*me*q / B_mag, 1, N), 3, N )
	
	;Drift velocity
	;   - Convert m/s -> km/s
	v_drift_dmpa = 1e-3 * (d_dmpa / T_gyro)
	
	;Electric Field
	;   - Convert from V/m to mV/m with 1e-3
	E_dmpa = mrvector_cross( b_avg_dmpa, d_dmpa ) / (1e3 * T_gyro)

;-----------------------------------------------------
; Gather Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Pick out the gun
	igd12 = where(gun_id eq 1, COMPLEMENT=igd21)

	meta = { sc:        sc, $
	         instr:     'edi', $
	         mode:      'srvy', $
	         level:     'ql', $
	         optdesc:   'efield', $
	         tstart:    tstart, $
	         tend:      tend, $
	         directory: save_dir, $
	         parents:   [ffast_edi, fslow_edi, files_fg, files_dss, files_att] $
	       }

	beam = { tt2000_gd12:    temporary(t_gd12), $
	         tt2000_gd21:    temporary(t_gd21), $
	         fv_gd12_dmpa:   fv_gdu_dmpa[*,igd12], $
	         fv_gd21_dmpa:   (temporary(fv_gdu_dmpa))[*,igd21], $
	         pos_vg1_dmpa:   pos_vg_dmpa[*,igd12], $
	         pos_vg2_dmpa:   (temporary(pos_vg_dmpa))[*,igd21], $
	         recnum_gd12:    recnum_gdu[igd12], $
	         recnum_gd21:    (temporary(recnum_gdu))[igd21], $
	         beamwidth_gd12: beam_width[igd12], $
	         beamwidth_gd21: (temporary(beam_width))[igd21] $
	       }
	
	b_avg = { tt2000:      t_avg, $
	          dt:          long64(dt * 1d9), $
	          b_avg_dmpa:  temporary(b_avg_dmpa), $
	          recnum:      temporary(recnum), $
	          b_gd12_dmpa: b_gdu_dmpa[*,igd12], $
	          b_gd21_dmpa: (temporary(b_gdu_dmpa))[*,igd21] $
	        }
	
	efield = { tt2000:     temporary(t_avg), $
	           d_dmpa:     temporary(d_dmpa), $
	           d_delta:    temporary(d_delta), $
	           v_ExB_dmpa: temporary(v_drift_dmpa), $
	           E_dmpa:     temporary(E_dmpa) $
	         }

	return, efield
end
