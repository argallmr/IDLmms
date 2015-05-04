


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
function mms_edi_view, t_dfg, b_dfg, t_avg, b_avg, t_err, b_stdev, $
                                  gun1_pos, gun2_pos, gun1_fire, gun2_fire, $
                                  edi1_beam_inds, edi2_beam_inds, drift_step, $
DOCS=docs, $
FILENAME=filename
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		void = cgErrorMsg()
		return, obj_new()
	endif

;-----------------------------------------------------
; Get Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	edi_dir      = '/Users/argall/Documents/Work/Data/MMS/EDI/'
	dfg_dir      = '/Users/argall/Documents/Work/Data/MMS/DFG/'
	sunpulse_dir = '/Users/argall/Documents/Work/Data/MMS/HK/'
	attitude_dir = '/Users/argall/Documents/Work/Data/MMS/Ephemeris/'
	sc           = 'mms4'
	tstart       = '2015-04-22T16:00:00Z'
	tend         = '2015-04-22T16:45:00Z'
	
	;Get FG data
	fg = mms_fg_read_ql(sc, 'dfg', 'srvy', tstart, tend, dfg_dir)

	;Get EDI data
	edi = mms_edi_gse(sc, 'slow', 'l1a', tstart, tend, /DMPA, $
	                  DIRECTORY    = edi_dir, $
	                  SUNPULSE_DIR = sunpulse_dir)

	;Average magnetic field
	avg = mms_edi_bavg(fg.epoch, fg.b_dmpa, edi.epoch_gd12, edi.epoch_gd21)

;-----------------------------------------------------
; Extract Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Extract required data
	t_fg   = fg.epoch
	b_fg   = fg.b_dmpa
	fg     = !Null

	t_gd12   = edi.epoch_gd12
	t_gd21   = edi.epoch_gd21
	fv_gd12  = edi.fv_gd12_dmpa
	fv_gd21  = edi.fv_gd21_dmpa
	gun1_pos = edi.virtual_gun1_dmpa
	gun2_pos = edi.virtual_gun2_dmpa
	edi      = !Null
	
	t_avg     = avg.t_avg
	b_avg     = avg.b_avg
	t_err     = rebin(avg.t_err, 2, n_elements(t_avg))
	b_std     = avg.b_std
	inds_gd12 = avg.inds_gd12
	inds_gd21 = avg.inds_gd21
	avg       = !Null

;-----------------------------------------------------
; Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	dmpa     = 1
	tf_drift = 0
	filename = ''

;-----------------------------------------------------
; Create Window \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Dimensions of 2:1 window
	wdims   = get_screen_size()
	waspect = wdims[0] / wdims[1]
	winy    = 500
	winx    = 2 * fix(winy * waspect)
	
	;Center it on the screen
	center = wdims / 2
	corner = center + [-winx/2, -winy/2]
	
	;Create window
	win    = window(DIMENSIONS=[winx, winy], LOCATION=corner)
	win   -> Refresh, /DISABLE

;-----------------------------------------------------
; Plot Bx, By, and Bz \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to seconds
	t_min   = t_fg[0] < t_avg[0]
	t_mean  = (t_avg - t_min) * 1d-9
	t_mag   = (t_fg - t_min) * 1d-9

	;YRange
	ymin      = min(b_fg, DIMENSION=2, MAX=ymax)
	margin    = [0.17, 0.27, 0.08, 0.15]
	font_size = 12

	;Plot the average magnetic field
	px_fg = Plot(t_mag, b_fg[0,*], /CURRENT, $
	             COLOR       = 'Grey', $
	             FONT_SIZE   = font_size, $
	             MARGIN      = margin, $
	             NAME        = 'Bx', $
	             POSITION    = [0.1, 0.64, 0.45, 0.93], $
	             TITLE       = 'Beam-Averaged Magnetic Field', $
	             XTICKFORMAT = '(a1)', $
	             YRANGE      = [ymin[0] * (ymin[0] gt 0 ? 0.9 : 1.1), ymax[0] * (ymax[0] gt 0 ? 1.1 : 0.8)], $
	             YSTYLE      = 1, $
	             YTITLE      = 'Bx!C(nT)')
	
	py_fg = Plot(t_mag, b_fg[1,*], /CURRENT, $
	             COLOR       = 'Grey', $
	             FONT_SIZE   = font_size, $
	             LAYOUT      = [1,3,2], $
	             MARGIN      = margin, $
	             NAME        = 'By', $
	             POSITION    = [0.1, 0.37, 0.45, 0.64], $
	             XTICKFORMAT = '(a1)', $
	             YRANGE      = [ymin[1] * (ymin[1] gt 0 ? 0.9 : 1.1), ymax[1] * (ymax[1] gt 0 ? 1.1 : 0.8)], $
	             YSTYLE      = 1, $
	             YTITLE      = 'By!C(nT)')
	
	pz_fg = Plot(t_mag, b_fg[2,*], /CURRENT, $
	             COLOR       = 'Grey', $
	             FONT_SIZE   = font_size, $
	             LAYOUT      = [1,3,3], $
	             MARGIN      = margin, $
	             NAME        = 'Bz', $
	             POSITION    = [0.1, 0.1, 0.45, 0.37], $
	             XTITLE      = 'Time (s)', $
	             YRANGE      = [ymin[2] * (ymin[2] gt 0 ? 0.9 : 1.1), ymax[2] * (ymax[2] gt 0 ? 1.1 : 0.8)], $
	             YSTYLE      = 1, $
	             YTITLE      = 'Bz!C(nT)')

;-----------------------------------------------------
; Plot B_AVG with Error Bars \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Make T_ERR the proper size
	t_error = rebin(t_err, 2, n_elements(t_mean))

	;Plot the measured magnetic field
	opx_avg = ErrorPlot(t_mean, b_avg[0,*], t_err, b_std[0,*], /CURRENT, $
	                    OVERPLOT=px_fg, NAME='Bx Avg', SYMBOL='Diamond', $
	                    ERRORBAR_COLOR='Red')
	opy_avg = ErrorPlot(t_mean, b_avg[1,*], t_err, b_std[1,*], /CURRENT, $
	                    OVERPLOT=py_fg, NAME='By Avg', SYMBOL='Diamond', $
	                    ERRORBAR_COLOR='Red')
	opz_avg = ErrorPlot(t_mean, b_avg[2,*], t_err, b_std[2,*], /CURRENT, $
	                    OVERPLOT=pz_fg, NAME='Bz Avg', SYMBOL='Diamond', $
	                    ERRORBAR_COLOR='Red')

	;Create a legend
	leg_b = Legend(HORIZONTAL_ALIGNMENT = 'RIGHT', $
	               LABEL                = 'Average', $
	               NAME                 = 'Averaged Data Legend', $
	               POSITION             = [0.53, 0.97], $
	               /NORMAL, $
	               TARGET               = opx_avg, $
	               TEXT_COLOR           = 'Black', $
	               VERTICAL_ALIGNMENT   = 'TOP')
	leg_b -> Add, px_fg, LABEL='Measured', TEXT_COLOR='Grey'

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
; Associate Beams with B_avg \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Look at each averaging interval
	navg = n_elements(t_avg)

	;Figure out which beams correspond to which average value
	hist1 = histogram(inds_gd12, MIN=0, MAX=navg-1, BINSIZE=1, REVERSE_INDICES=ri1)
	hist2 = histogram(inds_gd21, MIN=0, MAX=navg-1, BINSIZE=1, REVERSE_INDICES=ri2)

;-----------------------------------------------------
; Draw S/C, Guns, Fire Vectors, & Target \\\\\\\\\\\\\
;-----------------------------------------------------
	;Draw a set of axes 1.5 times bigger than the s/c
	range = 1.25 * [-sc_sphr_ocs[2,0], sc_sphr_ocs[2,0]]
	gAxes = plot(range, range, /NODATA, /CURRENT, $
	             ASPECT_RATIO = 1.0, $
	             MARGIN       = [0.5, 0.1, 0.05, 0.1], $
	             TITLE        = 'Beam intersections in ' + (dmpa ? '$B_{Avg}$ BPP' : '$B_{Interp}$ BPP'), $
	             XRANGE       = range, $
	             XSTYLE       = 1, $
	             XTITLE       = 'Distance (m)', $
	             YRANGE       = range, $
	             YSTYLE       = 1, $
	             YTITLE       = 'Distance (m)')
	
	;Draw the s/c
	gSC = polygon(reform(sc_xyz_ocs[0,*]), reform(sc_xyz_ocs[1,*]), /DATA, TARGET=gAxes)
	
	;Firing directions
	gFire1 = Polyline(range, range, COLOR='Blue', /DATA, TARGET=gAxes)
	gFire2 = Polyline(range, range, COLOR='Red',  /DATA, TARGET=gAxes)
		
	;Draw drift step
	if tf_drift then begin
		gTarget = symbol(drift_step[0], drift_step[1], 'X', /DATA, $
		                 TARGET    = gAxes, $
		                 SYM_THICK = 2, $
		                 SYM_SIZE  = 2.0)
	endif

;-----------------------------------------------------
; Draw Each Bavg Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	for i = 0, navg - 2 do begin
		;Number of beams used to compute B_avg
		n1 = ri1[ri1[i+1]] - ri1[ri1[i]]
		n2 = ri2[ri2[i+1]] - ri2[ri1[i]]
		
		;Are there beams associated with this time
		if n1 eq 0 && n2 eq 0 then continue
		
	;-----------------------------------------------------
	; Find Beams \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Get the beam indices
		inds1 = ri1[ri1[i]:ri1[i+1]-1]
		inds2 = ri2[ri2[i]:ri2[i+1]-1]
		
		;Gun positions
		g1_pos = gun1_pos[*,inds1]
		g2_pos = gun2_pos[*,inds2]

		;Gun firing directions
		fv_g1 = fv_gd12[*,inds1]
		fv_g2 = fv_gd21[*,inds2]
		
		;Drift step
		if tf_drift then d = drift_step[*,inds1]
		
	;-----------------------------------------------------
	; Rotate into B_avg BPP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Rotate the s/c into B_AVG bpp
		xyz2bpp    = mms_instr_xb2bpp(b_avg[*,i])
		sc_xyz_bpp = MrVector_Rotate(xyz2bpp, sc_xyz_ocs)
		
		;Rotate the positions and firing directions into average BPP
		if dmpa then begin
			g1_pos = MrVector_Rotate(xyz2bpp, g1_pos)
			g2_pos = MrVector_Rotate(xyz2bpp, g2_pos)
			fv_g1  = MrVector_Rotate(xyz2bpp, fv_g1)
			fv_g2  = MrVector_Rotate(xyz2bpp, fv_g2)
			if tf_drift then d = MrVector_Rotate(xyz2bpp, d)
		endif
		
	;-----------------------------------------------------
	; Two Points to Define the Beams \\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Beam slope, y-intercept, (x1,x2) and (y1,y2)
		;   - slope (m)       = rise / run
		;   - y-intercept (b) = y1 - m * x1
		;   - (x1,x2)         = range
		;   - (y1,y2)         = m*x + b
		m   = reform( fv_g1[1,*] / fv_g1[0,*] )
		b   = reform( g1_pos[1,*] - g1_pos[0,*] * m )
		g1x = rebin( [range[0], range[1]], 2, n1 )
		g1y = transpose( [[m * g1x[0,*] + b], [m * g1x[1,*] + b]] )
		
		;Beam slope, y-intercept, (x1,x2) and (y1,y2)
		m   = reform( fv_g2[1,*] / fv_g2[0,*] )
		b   = reform( g2_pos[1,*] - g2_pos[0,*] * m )
		g2x = rebin( [range[0], range[1]], 2, n2)
		g2y = transpose( [[m * g2x[0,*] + b], [m * g2x[1,*] + b]] )
		
		;Define connectivity
		;   - Make (x1,x2) and (y1,y2) pairs adjacent
		;   - Indicate connectivity: [2,       2,       2,
		;                                0, 1,    2, 3,    4, 5, ...]
		g1x   = reform(g1x, 2 * n1)
		g1y   = reform(g1y, 2 * n1)
		g2x   = reform(g2x, 2 * n2)
		g2y   = reform(g2y, 2 * n2)
		conn1 = reform([replicate(2,1,n1), lindgen(2,n1)], 3*n1)
		conn2 = reform([replicate(2,1,n2), lindgen(2,n2)], 3*n2)
		
	;-----------------------------------------------------
	; Update Graphics \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		
		;Draw the spacecraft
		gSC -> SetData, reform(sc_xyz_bpp[0,*]), reform(sc_xyz_bpp[1,*])

		;Draw the gun positions
		gGuns = symbol(reform([[g1_pos[0,*]], [g2_pos[0,*]]]), $
		               reform([[g1_pos[1,*]], [g2_pos[1,*]]]), 'circle', $
		               /DATA, TARGET=gAxes)

		;Draw the beams
		gFire1 -> SetData, g1x, g1y, CONNECTIVITY=conn1
		gFire2 -> SetData, g2x, g2y, CONNECTIVITY=conn2
		
		;Draw the target
		if tf_drift then gTarget -> SetData, d[0,*], d[1,*]
		
	;-----------------------------------------------------
	; Next Iteration\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Save?
		if filename ne '' then begin
			if nPts le 2 then begin
				gAxes -> Save, filename
			endif else begin
				if i eq nPts - 2 then close = 1 else close = 0
				gAxes -> Save, filename, /APPEND, CLOSE=close
			endelse
		endif
	
		;Let me see what is happening
		win -> Refresh
		wait, 0.5
		win -> Refresh, /DISABLE
		
		;Delete the guns
		if i ne navg - 2 then gGuns -> Delete
	endfor

	win -> Refresh
	return, win
end
