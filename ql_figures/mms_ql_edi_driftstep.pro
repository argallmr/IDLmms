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
function mms_ql_edi_driftstep, sc, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		void = cgErrorMsg()
		return, obj_new()
	endif

	sc       = 'mms2'
	tstart   = '2015-05-09T16:08:00Z'
	tend     = '2015-05-09T16:13:00Z'
	edi_dir  = '/nfs/edi/temp/'
	sdc_dir  = '/nfs/'
	dt       = 5
	save_dir = '/nfs/edi/beam_plots/'

;-------------------------------------------------------
; Find and Read EDI Files //////////////////////////////
;-------------------------------------------------------
	instr   = 'edi'
	mode    = 'srvy'
	level   = 'ql'
	optdesc = 'efield'
	
	;Search for file
	files_edi = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles_edi, $
	                          DIRECTORY = edi_dir, $
	                          OPTDESC   = optdesc, $
;	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)

	;If files exist
	if nfiles_edi eq 0 then message, 'No EDI files found: "' + searchstr + '".'
	
	;Read files
	edi_ql = mms_edi_read_ql_efield(files_edi, tstart, tend)

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
	win = window(DIMENSIONS=[winx, winy], LOCATION=corner, BUFFER=save_dir ne '')
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

;-----------------------------------------------------
; Draw S/C, Guns, Fire Vectors, & Targets \\\\\\\\\\\\
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
	
	;Draw the s/c
	gSC = polygon(reform(sc_xyz_ocs[0,*]), reform(sc_xyz_ocs[1,*]), $
	                /DATA, $
	                NAME   = 'S\C Outline', $
	                TARGET = gAxes)
	
	;Firing directions
	gFire1 = Polyline(range, range, COLOR='Blue', /CLIP, /DATA, TARGET=gAxes, NAME='Firing Vectors GD12')
	gFire2 = Polyline(range, range, COLOR='Red',  /CLIP, /DATA, TARGET=gAxes, NAME='Firing Vectors GD21')
	
	;Guns
	gGun1 = Symbol(sc_xyz_ocs[0,0], sc_xyz_ocs[1,0], 'circle', $
	               /DATA, $
	               NAME      = 'Gun1', $
	               SYM_COLOR = 'Blue', $
	               SYM_SIZE  = 2.0, $
	               TARGET    = gAxes)
	gGun2 = Symbol(sc_xyz_ocs[0,0], sc_xyz_ocs[1,0], 'circle', $
	               /DATA, $
	               NAME      = 'Gun2', $
	               SYM_COLOR = 'Red', $
	               SYM_SIZE  = 2.0, $
	               TARGET    = gAxes)
	
	;Draw drift step from CF
	gTargetCF = symbol(edi_ql.d_dmpa[0], edi_ql.d_dmpa[1], 'X', /DATA, $
	                   NAME      = 'Target', $
	                   TARGET    = gAxes, $
	                   SYM_COLOR = 'Black', $
	                   SYM_THICK = 2, $
	                   SYM_SIZE  = 3.0)
	
	;Draw drift step from BC
	gTargetBC = symbol(edi_ql.d_dmpa[0], edi_ql.d_dmpa[1], 'o', /DATA, $
	                   NAME      = 'Target', $
	                   TARGET    = gAxes, $
	                   SYM_COLOR = 'Green', $
	                   SYM_THICK = 2, $
	                   SYM_SIZE  = 3.0)
	
;-----------------------------------------------------
; Draw Each Bavg Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	nPts = n_elements(edi_ql.recnum)
	for i = 0, nPts - 2 do begin
		recnum = edi_ql.recnum[i]

		;Number of beams used to compute B_avg
		inds1 = where(edi_ql.recnum_gd12 eq recnum, n1)
		inds2 = where(edi_ql.recnum_gd21 eq recnum, n2)

		;Are there beams associated with this time
		if n1 eq 0 && n2 eq 0 then continue
		
	;-----------------------------------------------------
	; Drift Step and BPP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Virtual Source Point
		;   - Negative of the drift step
		t     =  edi_ql.tt2000[i]
		d_cf  = -edi_ql.d_dmpa[*,i]
		d_bc  = -edi_ql.d_bc_dmpa[*,i]
		b_avg =  edi_ql.b_dmpa[*,i]

		;Rotate the s/c into B_AVG bpp
		xyz2bpp    = mms_instr_xb2bpp(b_avg)
		sc_xyz_bpp = MrVector_Rotate(xyz2bpp, sc_xyz_ocs)
		
		;Rotate drift step into average BPP
		d_cf_bpp = MrVector_Rotate(xyz2bpp, d_cf)
		d_bc_bpp = MrVector_Rotate(xyz2bpp, d_bc)

	;-----------------------------------------------------
	; GD12 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if n1 gt 0 then begin
			;Gun positions and firing vectors
			g1_pos = edi_ql.pos_vg1_dmpa[*,inds1]
			g1_fv  = edi_ql.fv_gd12_dmpa[*,inds1]

			;Rotate to BPP
			g1_pos_bpp = MrVector_Rotate(xyz2bpp, g1_pos)
			g1_fv_bpp  = MrVector_Rotate(xyz2bpp, g1_fv)
			
			;Beam slope, y-intercept, (x1,x2) and (y1,y2)
			;   - slope (m)       = rise / run
			;   - y-intercept (b) = y1 - m * x1
			;   - (x1,x2)         = range
			;   - (y1,y2)         = m*x + b
			m     = reform( g1_fv_bpp[1,*] / g1_fv_bpp[0,*] )
			b     = reform( g1_pos_bpp[1,*] - g1_pos_bpp[0,*] * m )
			x_bpp = rebin( [range[0], range[1]], 2, n1 )
			y_bpp = transpose( [[m * x_bpp[0,*] + b], [m * x_bpp[1,*] + b]] )

			;Define connectivity
			;   - Make (x1,x2) and (y1,y2) pairs adjacent
			;   - Indicate connectivity: [2,       2,       2,
			;                                0, 1,    2, 3,    4, 5, ...]
			g1_fv_xbpp = reform(x_bpp, 2 * n1)
			g1_fv_ybpp = reform(y_bpp, 2 * n1)
			g1_fv_conn = reform([replicate(2,1,n1), lindgen(2,n1)], 3*n1)
		endif
		
	;-----------------------------------------------------
	; GD21 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if n2 gt 0 then begin
			;Gun positions and firing vectors
			g2_pos = edi_ql.pos_vg2_dmpa[*,inds2]
			g2_fv  = edi_ql.fv_gd21_dmpa[*,inds2]
			
			;Rotate to BPP
			g2_pos_bpp = MrVector_Rotate(xyz2bpp, g2_pos)
			g2_fv_bpp  = MrVector_Rotate(xyz2bpp, g2_fv)
		
			;Beam slope, y-intercept, (x1,x2) and (y1,y2)
			m     = reform( g2_fv_bpp[1,*] / g2_fv_bpp[0,*] )
			b     = reform( g2_pos_bpp[1,*] - g2_pos_bpp[0,*] * m )
			x_bpp = rebin( [range[0], range[1]], 2, n2)
			y_bpp = transpose( [[m * x_bpp[0,*] + b], [m * x_bpp[1,*] + b]] )
			
			;Connectivity
			g2_fv_xbpp = reform(x_bpp, 2 * n2)
			g2_fv_ybpp = reform(y_bpp, 2 * n2)
			g2_fv_conn = reform([replicate(2,1,n2), lindgen(2,n2)], 3*n2)
		endif

	;-----------------------------------------------------
	; Update Graphics \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		MrCDF_Epoch, t,                yr0, mo0, day0, hr0, min0, sec0, /BREAKDOWN_EPOCH
		MrCDF_Epoch, t+long64(dt*1e9), yr1, mo1, day1, hr1, min1, sec1, /BREAKDOWN_EPOCH

		;Title
		title = 'Beam intersections in $B_{Avg}$ BPP!C' + $
		        string(FORMAT='(%"%s %04i-%02i-%02i %02i:%02i:%02i - %02i:%02i:%02i")', $
		               sc, yr0, mo0, day0, hr0, min0, sec0, hr1, min1, sec1) + '!C' + $
		        string(FORMAT='(%"d_cf = [%0.2f, %0.2f, %0.2f]")', d_cf_bpp) + '!C' + $
		        string(FORMAT='(%"d_bc = [%0.2f, %0.2f, %0.2f]")', d_bc_bpp) + '!C' + $
		        string(FORMAT='(%"B = [%0.2f, %0.2f, %0.2f]")', b_avg)
		gAxes.title = title
		
		;Draw the spacecraft
		gSC -> SetData, reform(sc_xyz_bpp[0,*]), reform(sc_xyz_bpp[1,*])

		;Draw the gun positions
		if n1 gt 0 then begin
			;Un-hide graphics
			gGun1.sym_size = 2.0
			gFire1.hide    = 0
			
			;SetData
			gGun1  -> SetData, reform(g1_pos_bpp[0,*]), reform(g1_pos_bpp[1,*])
			gFire1 -> SetData, g1_fv_xbpp, g1_fv_ybpp, CONNECTIVITY=g1_fv_conn
		endif else begin
			gGun1.sym_size = 0.0
			gFire1.hide    = 1
		endelse
		
		if n2 gt 0 then begin
			;Un-hide data
			gGun2.sym_size = 2.0
			gFire2.hide    = 0
			
			;Set Data
			gGun2  -> SetData, reform(g2_pos_bpp[0,*]), reform(g2_pos_bpp[1,*])
			gFire2 -> SetData, g2_fv_xbpp, g2_fv_ybpp, CONNECTIVITY=g2_fv_conn
		endif else begin
			gGun2.sym_size = 0.0
			gFire2.hide    = 1
		endelse
		
		;Draw the target
		gTargetCF -> SetData, d_cf_bpp[0], d_cf_bpp[1]
		gTargetCF.sym_size = 3.0
		gTargetCF.sym_size = 2.0
		
		;Draw the target
		gTargetBC -> SetData, d_bc_bpp[0], d_bc_bpp[1]
		gTargetBC.sym_size = 3.0
		gTargetBC.sym_size = 2.0

	;-----------------------------------------------------
	; Next Iteration\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Refresh to draw graphic
		win -> Refresh

		;Save?
		if save_dir ne '' then begin
			if i eq 0 then begin
				;Get the file version
				mms_dissect_filename, files_edi, VERSION=version

				;Create a date directory if it does not exist
				MrTimeParser, tstart, '%Y-%M-%d', '%Y%M%d', date
				out_dir = filepath(ROOT_DIR=save_dir, date + '_v' + version)
				if ~file_test(out_dir) then file_mkdir, out_dir
			endif
		
			;Create the file name
			filename = filepath( ROOT_DIR=out_dir, $
			                     sc + '_' + instr + '_' + mode + '_' + level + '_' + $
			                     'driftstep-' + string(dt, format='(i1)') + 's_' + $
			                     string(FORMAT='(%"%04i%02i%02i%02i%02i%02i")', $
			                            yr0, mo0, day0, hr0, min0, sec0) + $
			                     '.png' )

			gAxes -> Save, filename
			print, 'Image saved to "' + filename + '".'
		endif else begin
			wait, 0.5
		endelse
	
		win -> Refresh, /DISABLE
	endfor
	win -> Refresh
	return, win
end
