; docformat = 'rst'
;
; NAME:
;       mms_edi_read_l1a_efield
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may  be used to endorse or promote products derived from this     ;
;         software without specific prior written permission.                            ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Read EDI electric field mode data.
;
;   Steps:
;       1) Find files and read data
;       2) Filter out EDI data that are too far from FGM data points
;       3) Interpolate B and |B| to beam times
;       4) Runner Order Estimate
;       5) Compute B_avg
;       6) Beam Width
;       7) Rotate beams and B to DMPA
;       8) Time of flight
;       9) Compute cost function
;         .
;         .
;         .
;
; :Categories:
;   MMS, EDI, Bestarg
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
;       2015/05/01  -   Written by Matthew Argall
;-
;*****************************************************************************************
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
pro mms_test_costFn_plot_update, win, sc, t, dt, B, beams, d_tri, d_tof, $
OUTPUT_DIR=output_dir
	compile_opt idl2
	on_error, 2
	
	;Draw the target off the grid
	if n_elements(d_bpp) eq 0 then d_bpp = [100.0, 100.0, 0.0]

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	
	;Breakdown timestamp
	;   - They are needed for TITLE and FILENAME
	MrCDF_Epoch_Breakdown, t,                yr0, mo0, day0, hr0, min0, sec0
	MrCDF_Epoch_Breakdown, t+long64(dt*1d9), yr1, mo1, day1, hr1, min1, sec1
	
	;Output
	if n_elements(output_dir) eq 0 then output_dir = ''
	
	;Create a window if necessary
	if n_elements(win) eq 0 || ~obj_valid(win) then begin
		buffer = output_dir eq '' ? 0 : 1
		win = mms_test_costfn_plot_init(B, BUFFER=buffer)
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

;-----------------------------------------------------
; Triangulation Axes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Create a Title
	MrCDF_Epoch_Breakdown, t,                yr0, mo0, day0, hr0, min0, sec0
	MrCDF_Epoch_Breakdown, t+long64(dt*1d9), yr1, mo1, day1, hr1, min1, sec1
	title = 'Beam intersections in $B_{Avg}$ BPP'                       + '!C' + $
	        string(FORMAT='(%"%s %04i-%02i-%02i %02i:%02i:%02i - %02i:%02i:%02i")', $
	        sc, yr0, mo0, day0, hr0, min0, sec0, hr1, min1, sec1);       + '!C' + $
;	        string(FORMAT='(%"d = [%0.2f, %0.2f, %0.2f]")', d_bpp)      + '!C' + $
;	        string(FORMAT='(%"B = [%0.2f, %0.2f, %0.2f]")', B)

	;Grab the axes.
	gTri  = win['Tri Plot']
	range = gTri.xrange
;	gTri.title = title
;	if ~obj_isa(win, 'GraphicsBuffer') then gAxes.title = title

;-----------------------------------------------------
; ToF Axes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Time and time range
	xrange  = MrCDF_epoch2ssm([t, t+long64(dt*1d9)])
	tof_ssm = MrCDF_epoch2ssm(beams.tt2000)
	isort   = sort(tof_ssm)
	
	yrange = [min(beams.rtof, MAX=ymax)]
	yrange += yrange * [-0.3, 0.3]
	
	;Update plot
	gToF    = win['ToF Plot']
	gToF   -> SetData, tof_ssm[isort], beams[isort].tof
	gToF   -> SetProperty, XRANGE=xrange, YRANGE=yrange

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
	igun1 = where(beams.gdu eq 1, ngun1, COMPLEMENT=igun2, NCOMPLEMENT=ngun2)

	;Update gun positions
	if ngun1 gt 0 then begin
		gGun1.sym_size = 2.0
		gGun1 -> SetData, reform(beams[igun1].vg_bpp[0]), reform(beams[igun1].vg_bpp[1])
	endif else begin
		gGun1.sym_size = 0.0
	endelse
		
	if ngun2 gt 0 then begin
		gGun2.sym_size = 2.0
		gGun2 -> SetData, reform(beams[igun2].vg_bpp[0]), reform(beams[igun2].vg_bpp[1])
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
		m     = reform( beams[igun1].fv_bpp[1] / beams[igun1].fv_bpp[0] )
		b     = reform( beams[igun1].vg_bpp[1] - beams[igun1].vg_bpp[0] * m )
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
		m     = reform( beams[igun2].fv_bpp[1] / beams[igun2].fv_bpp[0] )
		b     = reform( beams[igun2].vg_bpp[1] - beams[igun2].vg_bpp[0] * m )
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
	gTarget  = win['Target TRI']
	gTarget -> SetData, -d_tri[0], -d_tri[1]
	gTarget  = win['Target TOF']
	gTarget -> SetData, -d_tof[0], -d_tof[1]

;-----------------------------------------------------
; Save \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	win -> Refresh

	;Save the file?
	if output_dir ne '' then begin
		;Output do a daily file
		png_out = filepath(ROOT_DIR=output_dir, string(FORMAT='(%"%04i%02i%02i-idl")', yr0, mo0, day0))
		if ~file_test(png_out, /DIRECTORY) then file_mkdir, png_out
	
		;Start time for the file name
		tstart = string(FORMAT='(%"%04i%02i%02i%02i%02i%02i")', yr0, mo0, day0, hr0, min0, sec0)
		
		;Create the file name
		filename = strjoin([sc, 'edi', 'srvy', 'ql', 'driftstep-idl', tstart], '_')
		filename = filepath(filename, ROOT_DIR=output_dir) + '.png'

		;Save the image
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
function mms_test_costFn_plot_init, B, $
BUFFER=buffer
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Create Window \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Dimensions of 2:1 window
	wdims   = get_screen_size()
	waspect = wdims[0] / wdims[1]
	winy    = 600
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
; Triangulation Axes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Plot cananot set TITLE if window is /BUFFER
	if ~buffer then title = 'Beam intersections in $B_{Avg}$ BPP'

	;Draw a set of axes 1.5 times bigger than the s/c
	range = [-10,10] ;1.75 * [-sc_sphr_ocs[2,0], sc_sphr_ocs[2,0]]
	gTri = plot(range, range, /NODATA, /CURRENT, $
;	            ASPECT_RATIO = 1.0, $
	            POSITION     = [0.25, 0.4, 0.75, 0.9], $
	            NAME         = 'Tri Plot', $
;	            TITLE        = title, $
	            XRANGE       = range, $
	            XSTYLE       = 1, $
	            XTITLE       = 'Distance (m)', $
	            YRANGE       = range, $
	            YSTYLE       = 1, $
	            YTITLE       = 'Distance (m)')

;-----------------------------------------------------
; Time-of-Flight Axes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	yrange = [350, 450]
	gToF = plot([0], [0], /CURRENT, $
	            POSITION     = [0.13, 0.05, 0.9, 0.3], $
	            NAME         = 'ToF Plot', $
	            SYMBOL       = 'D', $
;	            TITLE        = title, $
;	            XRANGE       = range, $
	            XSTYLE       = 1, $
	            XTICKFORMAT  = 'time_labels', $
	            XTITLE       = 'Time (UT)', $
;	            YRANGE       = range, $
	            YSTYLE       = 1, $
	            YTITLE       = 'ToF ($\mu$s)')

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
	;Triangulation drift step
	gTarget = symbol( 0, 0, 'X', /DATA, $
	                 NAME      = 'Target TRI', $
	                 TARGET    = gAxes, $
	                 SYM_COLOR = 'Black', $
	                 SYM_THICK = 2, $
	                 SYM_SIZE  = 3.0)
	;Time-of-Flight drift step
	gTarget = symbol( 0, 0, 'X', /DATA, $
	                 NAME      = 'Target TOF', $
	                 TARGET    = gAxes, $
	                 SYM_COLOR = 'Purple', $
	                 SYM_THICK = 2, $
	                 SYM_SIZE  = 3.0)

;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	win -> Refresh
	return, win
end


;+
;   Find files required to process EDI E-field mode data.
;
; :Params:
;       SC:         in, required, type=string
;                   ID of the spacecraft from which files are to be found.
;                       CHoices are: 'mms1', 'mms2', 'mms3', 'mms4'.
;       TSTART:     in, required, type=string
;                   Start time, as an ISO-8601 string, of the data interval
;                       to be processed.
;       TEND:       in, required, type=string
;                   End time, as an ISO-8601 string, of the data interval
;                       to be processed.
;
; :Returns:
;       FILES:      out, required, type=struct
;                   Structure with file names and counts. Fields are:
;                       NFAST_EDI - Number of EDI fast survey files found.
;                       NSLOW_EDI - Number of EDI slow survey files found.
;                       NFGM      - Number of FGM survey files found.
;                       NDSS      - Number of DSS (HK101) files found.
;                       NATT      - Number of definitive attitude files found.
;                       FAST_EDI  - Names of the fast survey EDI files.
;                       SLOW_EDI  - Names of the slow survey EDI files.
;                       FGM       - Names of the FGM files.
;                       DSS       - Names of the DSS files.
;                       ATT       - Names of the definitive attitude files.
;-
function mms_edi_costfn_pickfiles, sc, tstart, tend
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Search for Files /////////////////////////////////////
;-------------------------------------------------------
	
	;EDI QL FAST file
	edi_l1a = mms_find_file(sc, 'edi', ['slow', 'fast'], 'l1a', $
	                        COUNT     = nedi_l1a, $
	                        OPTDESC   = 'efield', $
	                        SDC_ROOT  = sdc_dir, $
	                        SEARCHSTR = searchstr, $
	                        TSTART    = tstart, $
	                        TEND      = tend)
	if nedi_l1a eq 0 then message, 'No EDI L1A fast or slow survey files found.'
	
	;FG L1B file
	fgm_l1b = mms_find_file(sc, 'dfg', 'srvy', 'l1b', $
	                         COUNT     = nfgm_l1b, $
	                         OPTDESC   = '', $
	                         SDC_ROOT  = sdc_dir, $
	                         SEARCHSTR = searchstr, $
	                         TSTART    = tstart, $
	                         TEND      = tend)
	if nfgm_l1b eq 0 then message, 'No FG files found: "' + searchstr + '".'
	
	;FGM L2PRE file
	fgm_l2pre = mms_find_file(sc, 'dfg', 'srvy', 'l2pre', $
	                         COUNT     = nfgm_l2pre, $
	                         OPTDESC   = '', $
	                         SDC_ROOT  = sdc_dir, $
	                         SEARCHSTR = searchstr, $
	                         TSTART    = tstart, $
	                         TEND      = tend)
	if nfgm_l2pre eq 0 then message, 'No FGM L2PRE files found: "' + searchstr + '".'
	
	;DSS file
	files_dss = mms_find_file(sc, 'fields', 'hk', 'l1b', $
	                          COUNT     = nfiles_dss, $
	                          OPTDESC   = '101', $
	                          SDC_ROOT  = '/nfs/hk/', $
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

;-------------------------------------------------------
; Output ///////////////////////////////////////////////
;-------------------------------------------------------

	files = { natt:       nfiles_att, $
	          ndss:       nfiles_dss, $
	          nedi_l1a:   nedi_l1a, $
	          nfgm_l1b:   nfgm_l1b, $
	          nfgm_l2pre: nfgm_l2pre, $
	          defatt:     files_att, $
	          dss:        files_dss, $
	          edi_l1a:    edi_l1a, $
	          fgm_l1b:    fgm_l1b, $
	          fgm_l2pre:  fgm_l2pre $
	        }
	
	return, files
end


;+
;   Find files required to process EDI E-field mode data.
;
; :Params:
;       FILES:      in, required, type=string
;                   Files and file counts returned by 'edi_efield_pickfiles'.
;       TSTART:     in, required, type=string
;                   Start time, as an ISO-8601 string, of the data interval
;                       to be processed.
;       TEND:       in, required, type=string
;                   End time, as an ISO-8601 string, of the data interval
;                       to be processed.
;       EDI:        out, optional, type=structure
;                   Structure of EDI survey data for the requested time interval.
;                       Slow and fast survey data structures returned by 
;                       "mms_edi_create_l1b.pro" are combined into a single
;                       structure.
;       FGM:        out, optional, type=structure
;                   Structure of FGM survey data for the requested time interval.
;                       See "mms_fg_read_l1b.pro"
;       DSS:        out, optional, type=structure
;                   Structure of Digial Sun Pulse (HK101) data for the requested time
;                       interval. See "mms_dss_read_sunpulse.pro"
;       DEFATT:     out, optional, type=structure
;                   Structure of definitive attitude data for the requested time
;                       interval. See "mms_fdoa_read_defatt.pro"
;       ZMPA:       out, optional, type=3x1 float
;                   Unit vector of the z-MPA axis in BCS coordinates.
;-
pro mms_edi_costfn_readfiles, files, tstart, tend, $
EDI_L1A   = edi_l1a, $
EDI_L1B   = edi_l1b, $
FGM_L1B   = fgm_l1b, $
FGM_L2PRE = fgm_l2pre, $
DSS       = dss, $
DEFATT    = defatt, $
ZMPA      = zmpa
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Attitude
	if (arg_present(defatt) || arg_present(zmpa)) && files.natt gt 0 then begin
		defatt = mms_fdoa_read_defatt(files.defatt, tstart, tend, HEADER=att_hdr)
		zmpa   = att_hdr.zmpa[*,0]
	endif
	
	;Sunpulse
	if arg_present(dss) $
		then dss = mms_dss_read_sunpulse(files.dss, tstart, tend, /UNIQ_PULSE)
	
	;FGM L1B
	if arg_present(fgm_l1b) $
		then fgm_l1b = mms_fg_read_l1b(files.fgm_l1b, tstart, tend)
	
	;FGM L2PRE
	if arg_present(fgm_l2pre) $
		then fgm_l2pre = mms_fg_read_l2pre(files.fgm_l2pre, tstart, tend)
	
	;EDI L1A
	if arg_present(edi_l1a) then begin
		edi_l1a = mms_edi_read_efield_l1a(files.edi_l1a, tstart, tend, $
		                                  QUALITY = quality, $
		                                  /STRUCTARR)
	endif
	
	;EDI L1B
	if arg_present(edi_l1b) then begin
		edi_l1b = mms_edi_create_l1b(files.edi_l1a, tstart, tend, $
		                             /CS_123, $
		                             /CS_BCS, $
		                             QUALITY = quality, $
		                             /STRUCTARR)
	endif
end


;+
;   Interpolate via cubic spline the magnetic field data onto EDI beam times.
;
; :Params:
;       B:          in, required, type=3xN or 4xN float
;                   3-components, or 3-components plus the magnitude of the magnetic field.
;       T_FGM:      in, required, type=long64 (cdf_time_tt2000)
;                   TT2000 time stamps of each vector in `B`
;       T_EDI:      in, required, type=long64 (cdf_time_tt2000)
;                   TT2000 time stamps of each EDI beam
;       GDU:        in, required, type=long64 (cdf_time_tt2000)
;                   GDU ID (1 or 2) of each  `T_EDI` value.
;
; :Returns:
;       B_OUT:      `B` interpolated onto the EDI time stamps.
;-
function mms_edi_costfn_binterp, b, t_fgm, t_edi, gdu
	compile_opt idl2
	on_error, 2
	
	;Allocate mem
	dims  = size(b, /DIMENSIONS)
	n     = n_elements(t_edi)
	b_out = fltarr(dims[0], n)

	;Convert to double (seconds since midnight)
	t_fgm_ssm = MrCDF_epoch2ssm(t_fgm, t_edi[0])
	t_edi_ssm = MrCDF_epoch2ssm(t_edi, t_edi[0])

	;Interpolation is time dependent, so must be done separately for each gun
	igd12 = where(gdu eq 1, ngd12, NCOMPLEMENT=ngd21, COMPLEMENT=igd21)

	;Interpolate each component
	for i = 0, dims[0] - 1 do begin
		if ngd12 gt 0 then b_out[i,igd12] = spline(t_fgm_ssm, b[i,*], t_edi_ssm[igd12], 0.01)
		if ngd21 gt 0 then b_out[i,igd21] = spline(t_fgm_ssm, b[i,*], t_edi_ssm[igd21], 0.01)
	endfor

	return, b_out
end


;+
;   Average the vector magnetic field within each analysis interval.
;
; :Params:
;       EDI:        in, required, type=structarr
;                   Structure array of EDI beam data. Required fields are:
;                       TT2000  -   TT2000 time tags of each beam.
;       FGM:        in, required, type=structarr
;                   Structure of FGM magnetic field data. Required fields are:
;                       TT2000  -   TT2000 time tags of each magnetic field vector.
;                       B_DMPA  -   Full resolution vector magnetic field in DMPA coordinates
;
; :Keywords:
;       DT:         in, optional, type=float, default=5.0
;                   Duration of each analysis interval in seconds.
;
; :Returns:
;       AVG:        Array of averaged data. Fields are::
;                       T_AVG       -   TT2000 time stamp of the resulting averaged data.
;                       B_MAG       -   Average magnetic field magnitude
;                       B_AVG       -   Average magnetic field vector
;                       B_STD       -   Standard deviation of each component of B_AVG.
;                       RECNUM      -   Record number of each averaged value/vector
;                       RECNUM_EDI  -   Record number cross-reference to pair EDI beams
;                                       with the averaged value/vector with which it is
;                                       associated.
;-
function mms_edi_costfn_bavg, edi, fgm, $
DT = dt
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Setup Time Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Default
	if n_elements(dt) eq 0 then dt = 5.0
	dt_tt = long64(dt * 1d9)

	;Data interval
	t0 = edi[0].tt2000
	t1 = edi[-1].tt2000
	
	;Breakdown into time vectors
	MrCDF_Epoch_Breakdown, [t0, t1], yr, mo, day, hr, mnt, sec
	
	;Round down to the nearest DT seconds and recompute
	sec   = sec - (sec mod dt)
	tedge = MrCDF_Epoch_Compute(yr, mo, day, hr, mnt, sec, /TT2000)
	ti    = tedge[0]
	tf    = tedge[1] + long64(dt * 1d9)

	;Find FGM data within this time interval
	irange = MrIndexRange( fgm.tt2000, [ti, tf] )
	t_fgm  = fgm.tt2000[ irange[0]:irange[1] ]
	b_dmpa = fgm.b_dmpa[0:2, irange[0]:irange[1] ]

;-----------------------------------------------------
; Compute Average B \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Output time
	t_avg = MrMake_Array(START=ti, LAST=tf, INCREMENT=dt_tt, TNAME='LONG64')

	;Locate points in averaging interval
	iavg  = value_locate(t_avg, t_fgm)
	ihist = histogram(reform(iavg), REVERSE_INDICES=ri)
	nhist = n_elements(ihist)
	
	;We have designed T_AVG so that zero points are associated with TF.
	;   - Averaging occurs over the interval [0,5)
	;   - All points >= TF have been discarded
	;As a result, T_AVG has one more point than NHIST and it must be removed.
	t_avg = t_avg[0:nhist-1]

	;Compute average and standard deviation of field
	b_avg  = fltarr(3, nhist)
	b_std  = fltarr(3, nhist)
	recnum = lindgen(nhist)
	for i = 0, nhist - 1 do begin
		;No b-field found in this interval
		if ri[i] eq ri[i+1] then begin
			b_avg[*,i] = -1e31
			b_std[*,i] = -1e31
		;Average the b-field
		endif else begin
			i0 = ri[ri[i]]
			i1 = ri[ri[i+1]-1]
			b_avg[*,i] = mean(b_dmpa[*,iavg[i0:i1]],   DIMENSION=2)
			b_std[*,i] = stddev(b_dmpa[*,iavg[i0:i1]], DIMENSION=2)
		endelse
	endfor

;-----------------------------------------------------
; Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Find EDI points that map to the averaged field
	recnum_edi = value_locate(t_avg, edi.tt2000)
	
	avg = { t_avg:      t_avg, $
	        b_mag:      MrVector_Magnitude(b_avg), $
	        b_avg:      b_avg, $
	        b_std:      b_std, $
	        recnum:     recnum, $
	        recnum_edi: recnum_edi $
	      }
	
	return, avg
end


;+
;   Remove EDI beams that fall within FGM data gaps.
;
; :Params:
;       EDI:        in, required, type=structarr
;                   Structure array of EDI beam data. Required fields are:
;                       TT2000  -   TT2000 time tags of each beam.
;       FGM:        in, required, type=structarr
;                   Structure of FGM magnetic field data. Required fields are:
;                       TT2000  -   TT2000 time tags of each magnetic field vector.
;                       B_DMPA  -   Full resolution vector magnetic field in DMPA coordinates
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of beams remaining.
;       MAX_TT_DISTANCE:    in, optional, type=float, default=0.25
;                           Gap size (seconds). If beams are located farther than this
;                               amount of time away from `FGM` data points, they are
;                               removed from `EDI`.
;
; :Returns:
;       EDI_OUT     `EDI` structure array with beams removed.
;-
function mms_edi_costfn_bGaps, edi, fgm, $
COUNT=nkeep, $
MAX_TT_DISTANCE=max_tt_distance
	compile_opt idl2
	on_error, 2

	;Defaults
	if max_tt_distance eq 0 then max_tt_distanct = 0.25

	max_tt = long64(max_tt_distance*1d9)

;-----------------------------------------------------
; Check Data Gaps \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Number of points
	nedi = n_elements(edi)
	nfgm = n_elements(fgm.tt2000)

	;
	; Check for gaps in magnetic field data: for each EDI data point
	; calculate which B data point is closest in time
	;
	;     Y    is the index of T_FG.
	;     YOUT is the fractional index within T_FG at which T_EDI is located.
	;
	; This is similar to using Value_Locate, except we will use the Round()
	; where as Value_Locate would use Floor().
	;
	t0    = fgm.tt2000[0]
	B_idx = round( interpol( dindgen(nfgm), fgm.tt2000-t0, edi.tt2000-t0 ) )

	;If there is no magnetic field data within MAX_TT_DISTANCE nanoseconds
	;from an EDI data point, then remove the EDI data point.
	delta_tt = abs(edi.tt2000 - fgm.tt2000[B_idx])
	ikeep    = where(delta_tt lt max_tt, nkeep, $
	                 COMPLEMENT=iremove, NCOMPLEMENT=nremove)

	;Remove points
	if nremove gt 0 then begin
		MrPrintF, 'LogText', nremove, FORMAT='(%"Removing %i EDI data points with no B data close-by.")'
	endif

	;Too few points to continue
	if nkeep lt 2 then begin
		msg = string(nkeep, FORMAT='(%"Fewer than 2 (%i) EDI data points after B gap removal.")')
		message, msg
	endif
	
	;Remove data within data gaps
	if nremove gt 0 $
		then return, edi[ikeep] $
		else return, edi
end


;+
;   Sort beams into "toward" and "away" categories. "Toward" beams are those fired
;   toward the target and "away" beams are those fired away from the target. Because
;   we do not know where the target is, there is an ambiguity associated with the
;   terms "toward" and "away".
;
;   This program is intended for use with a single analysis interval (5 seconds for
;   1/4-spin resolution data).
;
; :Params:
;       BEAMS:      in, required, type=structarr
;                   Structure of EDI beam data. Required fields are:
;                       ALPHA  -   Azimuth angle of the firing vectors in BPP.
;       AVG:        out, required, type=float
;                   Average azimuth angle of all firing vectors in BPP. This is
;                       associated with the anglular position of the drift step in BPP.
;       SDEV:       out, required, type=float
;                   Standard deviation associated with `AVG`.
;-
pro mms_edi_costfn_tofsort, beams, avg, sdev
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Standard Deviation of Firing Vectors \\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Calculate the mean firing vector and its standard deviation
	;   - As a side-effect, beams are classified into TOWARD and AWAY categoreis

;	mms_edi_tof_sort, beams.alpha, ito, iaw, stdev
;	nto = n_elements(ito)
;	naw = n_elements(iaw)

	ep_hav_sorter, beams.alpha, ito, iaw, STDEV=stdev
	nto = n_elements(ito)
	naw = n_elements(iaw)

;	toaw_groupbeamdirections, beams.alpha, ito, iaw
;	nto = n_elements(ito)
;	naw = n_elements(iaw)

	;
	; Note that the standard deviation includes beams fired both
	; toward and away from the target, which makes it larger than
	; we want. This is because beams fired away from the target
	; diverge from one another going out, and converge when
	; returning.
	;
	; Below, we rotate "away" beams by 180 degrees and recompute
	; the average and standard deviation.
	; 

;-----------------------------------------------------
; Add Fields \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	beams = MrStruct_AddTags(beams, 'toaw', 0S)
	if nto gt 0 then beams[ito].toaw = 1
	if naw gt 0 then beams[iaw].toaw = -1

;	;Plot the beams
;	p1 = plot([0], [0], XRANGE=[-1.5,1.5], YRANGE=[-1.5,1.5], /NODATA)
;	for i = 0, nto-1 do !Null = arrow([0, cos(beams[ito[i]].alpha*!dtor)], [0, sin(beams[ito[i]].alpha*!dtor)], TARGET=p1, /DATA, COLOR='Blue')
;	for i = 0, naw-1 do !Null = arrow([0, cos(beams[iaw[i]].alpha*!dtor)], [0, sin(beams[iaw[i]].alpha*!dtor)], TARGET=p1, /DATA, COLOR='Black')

;	stop
;	p1 -> Close


;-----------------------------------------------------
; Average Toward & Away \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; The mean firing angle is not the same as the
	; angle of the mean firing vector:
	;
	;    ;Firing vectors and firing angles
	;    nbeams = 5
	;    x      = 3.0*randomu(3, nbeams)
	;    y      = 3.0*randomu(4, nbeams)
	;    v_hat  = [x, y] / sqrt(x^2 + y^2)
	;    alpha  = atan(y, x)
	;
	;    ;Mean firing vector
	;    x_avg = mean(x)
	;    y_avg = mean(y)
	;    v_avg = [x_avg, y_avg] / sqrt(x_avg^2 + y_avg^2)        ;Mean firing vector
	;
	;    ;Mean firing angles
	;    phi1 = atan(v_avg[0], v_avg[1])                         ;Angle of mean firing vector
	;    phi2 = mean(alpha)                                      ;Mean firing angle
	;
	;    ;Angular differnce between to mean firing vector
	;    adiff1 = acos( v_hat[0]*v_avg[0] + v_hat[1]*v_avg[1]    ;Angular separation from mean firing vector
	;    adiff2 = alpha - phi2                                   ;Angular separation from mean firing angle
	;    
	;    ;Standard deviations
	;    sdev1 = sqrt( total(adiff1^2) / (nbeams - 1) )
	;    sdev2 = sqrt( total(adiff2^2) / (nbeams - 1) )
	;
	;    ;Print resutls
	;    print, FORMAT='(%"alpha1 = %0.4f +/- %0.4f")', [phi1, sdev1] * !radeg
	;    print, FORMAT='(%"alpha2 = %0.4f +/- %0.4f")', [phi2, sdev2] * !radeg
	;    
	;    ;Results
	;    alpha1 = 34.6486 +/- 17.4425
	;    alpha2 = 53.7017 +/- 29.4491
	;
	; Below, we take the angular separation from the mean firing vector.
	;
	
	
	;Average away beams
	if naw gt 0 then begin
		;Rotate away beams into toward direction
		alpha = beams.alpha[iaw] * (!dpi / 180.0D)

		;Components of firing vectors
		;   - Rotate by 180 degrees into the "toward" direction
		x = -cos(alpha)
		y = -sin(alpha)
		
		;Components of mean firing vector
		xavg = mean(x)
		yavg = mean(y)
		v_hat = [xavg, yavg] / sqrt(xavg^2 + yavg^2)
		
		;Angular difference as a dot product
		phi_aw   = atan(v_hat[1], v_hat[0])
		adiff_aw = acos(x*v_hat[0] + y*v_hat[1])
	endif
	
	;Average toward beams
	if nto gt 0 then begin
		;Rotate away beams into toward direction
		alpha = beams.alpha[ito] * (!dpi / 180.0D)
		
		;Components of firing vectors
		x = cos(alpha)
		y = sin(alpha)
		
		;Components of mean firing vector
		xavg  = mean(x)
		yavg  = mean(y)
		v_hat = [xavg, yavg] / sqrt(xavg^2 + yavg^2)
		
		;Angular difference as a dot product
		phi_to   = atan(v_hat[1], v_hat[0])
		adiff_to = acos(x*v_hat[0] + y*v_hat[1])
	endif
;-----------------------------------------------------
; Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	case 1 of
		nto gt 0 && naw gt 0: begin
			avg  = (phi_to + phi_aw) / 2.0
			sdev = sqrt( total( [adiff_to, adiff_aw]^2 ) / (nto + naw - 1) )
		endcase
		
		nto gt 0: begin
			avg  = phi_to
			sdev = sqrt( total(adiff_to^2) / (nto - 1) )
		endcase
		
		naw gt 0: begin
			avg  = phi_aw
			sdev = sqrt( total(adiff_aw^2) / (naw - 1) )
		endcase
		
		else: message, 'No toward or away beams.'
	endcase
end


;+
;   Calculate the relativistic electron velocity associated with each beam.
;
;   Side effects:
;       Adds the following tags `BEAMS`.
;           V      -   Electron velocity (m/s)
;           RGYRO  -   Electron gyroradius (meters)
;
; :Params:
;       BEAMS:      in, required, type=structarr
;                   Structure of EDI beam data. Required fields are:
;                       ENERGY   -   Position of the guns on the virtual spacecraft in DMPA.
;       B_MAG:      in, required, type=3xN float
;                   Magnetic field magnitude interpolated to beam in `BEAMS`.
;-
pro mms_edi_costfn_vBeam, beams, b_mag
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Constants \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Some constants
	c        = 299792485.0        ;speed of light (m/s)
	eV2joule = 1.60217646e-19     ;converts eV to J
	q        = 1.60217646e-19     ;charge constant (C)
	me       = 9.10938188e-31     ;electron mass (kg)
	nT2T     = 1e-9               ;converts nano-Tesla to Tesla

;-----------------------------------------------------
; Relativistic Velcoity \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Velocity of the beam: knowing
	;   KE    = m c^2 * (gamma - 1)
	;   gamma = 1 / sqrt( 1 - v^2/c^2)
	;   E0    = m c^2
	;solve for v
	E0     = me * c^2
	v_beam = c * sqrt( 1 - ( E0 / (beams.energy*eV2joule + E0) )^2 )   ;meters / second

;-----------------------------------------------------
; Gyroradius \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Gyroradius
	;   w = q B / gamma m
	;   r = v / w
	gam   = 1.0 / sqrt( 1.0 - v_beam^2 / c^2 )   ;unitless
	wGyro = B_mag *  ((nT2T * q) / (gam * me))   ;radians / second
	rGyro = v_beam / temporary(wGyro)            ;meters

;-----------------------------------------------------
; Add to Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Add velocity and gyroradius to beam structure
	beams       = MrStruct_AddTags(beams, 'v', 0.0, 'rGyro', 0.0)
	beams.v     = temporary(v_beam)
	beams.rGyro = temporary(rGyro)
end


;+
;   Despin EDI beam data in BCS coordinates, then transform it into DMPA coordinates
;   where the magntic field lives.
;
;   Side Effects:
;       Adds the following tags to `EDI`.
;           VG_DMPA   -   Position of the guns on the virtual spacecraft in DMPA.
;           GUN_DMPA  -   Position of the guns in DMPA.
;           DET_DMPA  -   Position of the detectors DMPA.
;           FV_DMPA   -   Firing vectors in DMPA.
;       All fields with "_BCS" listed under the `EDI` parameter are removed.
;
; :Params:
;       EDI:        in, required, type=structarr
;                   Structure of EDI beam data. Fields are:
;                       VG_BCS   -   Position of the guns on the virtual spacecraft in BCS.
;                       GUN_BCS  -   Position of the guns in BCS.
;                       DET_BCS  -   Position of the detectors BCS.
;                       FV_BCS   -   Firing vectors in BCS.
;                       GDU      -   GDU (1 or 2) associated with each element.
;       DSS:        in, required, type=struct
;                   Structure of digital sun sensor housekeeping data. The structure
;                       should be produced by mms_dss_read_sunpulse.pro. It is used to
;                       despin from BCS coordinates.
;       ZMPA:       in, optional, type=3x1 float
;                   Z-axis of the Major Principle Axis (MPA) coordinate system. The value
;                       should originate form the FDOA Definitive Attiude file header
;                       and is valid for an entire orbit of data. Use to rotate from
;                       DBCS to DMPA coordinates. If not present (i.e. DEFATT data is
;                       unavailable), DBCS and DMPA are assumed to be identical (typically
;                       this is true to within a hundredth of a degree).
;-
pro mms_edi_costfn_bcs2dmpa, edi, dss, zmpa
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; BCS -> SMPA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;BCS -> SMPA
	if n_elements(zmpa) eq 0 then begin
		MrPrintF, 'LogWarn', 'No DEFATT data. Cannot transform BCS -> SMPA.'
		gun_smpa = edi.gun_bcs
		det_smpa = edi.det_bcs
		vg_smpa  = edi.vg_bcs
		fv_smpa  = edi.fv_bcs
	endif else begin
		;Transformation matrix to SMPA
		bcs2smpa = mms_fg_xbcs2smpa(zmpa)
	
		;Transform
		gun_smpa = MrVector_Rotate(bcs2smpa, edi.gun_bcs)
		det_smpa = MrVector_Rotate(bcs2smpa, edi.det_bcs)
		vg_smpa  = MrVector_Rotate(bcs2smpa, edi.vg_bcs)
		fv_smpa  = MrVector_Rotate(bcs2smpa, edi.fv_bcs)
	endelse

;-----------------------------------------------------
; Despin SMPA -> DMPA & Combine \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Despinning is time dependent, so the two guns must be
	;despun independently.
	igd12 = where(edi.gdu eq 1, ngd12, COMPLEMENT=igd21, NCOMPLEMENT=ngd21)

	;GD12
	if ngd12 gt 0 then begin
		;SMPA -> DMPA
		smpa2dmpa = mms_dss_xdespin(dss, edi[igd12].tt2000)
	
		;Beam data
		gun_dmpa_gd12 = MrVector_Rotate( smpa2dmpa, gun_smpa[*,igd12] )
		det_dmpa_gd12 = MrVector_Rotate( smpa2dmpa, det_smpa[*,igd12] )
		vg_dmpa_gd12  = MrVector_Rotate( smpa2dmpa, vg_smpa[*,igd12]  )
		fv_dmpa_gd12  = MrVector_Rotate( smpa2dmpa, fv_smpa[*,igd12]  )
	endif
	
	;GD21
	if ngd21 gt 0 then begin
		;SMPA -> DMPA
		smpa2dmpa = mms_dss_xdespin(dss, edi[igd21].tt2000)
	
		;Beam data
		gun_dmpa_gd21 = MrVector_Rotate( smpa2dmpa, gun_smpa[*,igd21] )
		det_dmpa_gd21 = MrVector_Rotate( smpa2dmpa, det_smpa[*,igd21] )
		vg_dmpa_gd21  = MrVector_Rotate( smpa2dmpa, vg_smpa[*,igd21]  )
		fv_dmpa_gd21  = MrVector_Rotate( smpa2dmpa, fv_smpa[*,igd21]  )
	endif
	
	;Delete data
	gun_smpa = !Null
	det_smpa = !Null
	vg_smpa  = !Null
	fv_smpa  = !Null

;-----------------------------------------------------
; Add to EDI structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create output structure
	;   - Remove unwanted tags
	;   - Add position and firing vectors in DMPA
	edi = MrStruct_RemoveTags(edi, ['gun_123', 'det_123', 'vg_123', 'fv_123', $
	                                'gun_bcs', 'det_bcs', 'vg_bcs', 'fv_bcs'])
	edi = MrStruct_AddTags(edi, 'gun_dmpa', fltarr(3), $
	                            'det_dmpa', fltarr(3), $
	                            'vg_dmpa',  fltarr(3), $
	                            'fv_dmpa',  fltarr(3))

	;GD12
	if ngd12 gt 0 then begin
		edi[igd12].gun_dmpa = temporary(gun_dmpa_gd12)
		edi[igd12].det_dmpa = temporary(det_dmpa_gd12)
		edi[igd12].vg_dmpa  = temporary(vg_dmpa_gd12)
		edi[igd12].fv_dmpa  = temporary(fv_dmpa_gd12)
	endif
	
	;GD21
	if ngd21 gt 0 then begin
		edi[igd21].gun_dmpa = temporary(gun_dmpa_gd21)
		edi[igd21].det_dmpa = temporary(det_dmpa_gd21)
		edi[igd21].vg_dmpa  = temporary(vg_dmpa_gd21)
		edi[igd21].fv_dmpa  = temporary(fv_dmpa_gd21)
	endif
end


;+
;   Transform E-field beam data from DMPA to BPP coordinates. The program is designed
;   for a single analysis interval (5 seconds for 1/4-spin fit data).
;
;   Side effects:
;       Adds the following tags to both `TRI` and `TOF`.
;           VG_BPP   -   Position of the guns on the virtual spacecraft in BPP.
;           GUN_BPP  -   Position of the guns in BPP.
;           DET_BPP  -   Position of the detectors BPP.
;           FV_BPP   -   Firing vectors in BPP.
;           ALPHA    -   Azimimuthal firing angle in BPP.
;
; :Params:
;       TRI:        in, required, type=structarr
;                   Structure of EDI beam data. Fields are:
;                       VG_DMPA   -   Position of the guns on the virtual spacecraft in DMPA.
;                       GUN_DMPA  -   Position of the guns in DMPA.
;                       DET_DMPA  -   Position of the detectors DMPA.
;                       FV_DMPA   -   Firing vectors in DMPA.
;       B:          in, required, type=3x1 float
;                   A single magnetic field vector used to transform `EDI` beam data
;                       into BPP.
;-
pro mms_edi_costfn_dmpa2bpp, edi, b
	compile_opt idl2
	on_error, 2
	
	;Determine BPP
	dmpa2bpp = mms_edi_xxyz2bpp(b)

	;Rotate beams
	vg_bpp  = MrVector_Rotate(dmpa2bpp, edi.vg_dmpa)
	fv_bpp  = MrVector_Rotate(dmpa2bpp, edi.fv_dmpa)
	gun_bpp = MrVector_Rotate(dmpa2bpp, edi.gun_dmpa)
	det_bpp = MrVector_Rotate(dmpa2bpp, edi.det_dmpa)
	
	;Calculate firing angle in BPP
	alpha = reform( atan( fv_bpp[1,*], fv_bpp[0,*] ) )

	;Create the output structure
	edi_bpp = edi[0]
	edi_out = MrStruct_AddTags(edi, 'vg_bpp',  fltarr(3), $
	                                'gun_bpp', fltarr(3), $
	                                'det_bpp', fltarr(3), $
	                                'fv_bpp',  fltarr(3), $
	                                'alpha',   0.0)

	;Fill with new results
	edi_out.vg_bpp  = temporary(vg_bpp)
	edi_out.gun_bpp = temporary(gun_bpp)
	edi_out.det_bpp = temporary(det_bpp)
	edi_out.fv_bpp  = temporary(fv_bpp)
	edi_out.alpha   = temporary(alpha) * (180.0 / !pi)

;Plot firing vectors
;n = n_elements(edi_out)
;p1 = plot([0], [0], XRANGE=[-1.5,1.5], YRANGE=[-1.5,1.5], /NODATA)
;for i = 0, n-1 do !Null = arrow([0, edi[i].fv_bcs[0]], [0, edi_out[i].fv_bcs[1]], TARGET=p1, /DATA, COLOR='Grey')
;for i = 0, n-1 do !Null = arrow([0, edi[i].fv_dmpa[0]], [0, edi_out[i].fv_dmpa[1]], TARGET=p1, /DATA, COLOR='Red')
;for i = 0, n-1 do !Null = arrow([0, edi_out[i].fv_bpp[0]], [0, edi_out[i].fv_bpp[1]], TARGET=p1, /DATA, COLOR='Blue')
;wait, 0.25
;for i = 0, n-1 do !Null = arrow([0, cos(edi_out[i].alpha*!dtor)], [0, sin(edi_out[i].alpha*!dtor)], TARGET=p1, /DATA, COLOR='Black')
;stop
;p1 -> close

	;Swap input for output
	edi = temporary(edi_out)
end


;+
;   Transform E-field results from BPP to DMPA coordinates.
;
;   Side effects:
;       Adds the following tag to both `TRI` and `TOF`.
;           D_DMPA  -   Drift step in DMPA coordinates.
;
; :Params:
;       B:          in, required, type=3xN float
;                   Magnetic field vectors associated with each element in `TRI` and `TOF`.
;       TRI:        in, required, type=structarr
;                   Triangulation reslults, one element per analysis interval. Fields are:
;                       'D_BPP'  -   Drift step in BPP (meters)
;       TOF:        in, required, type=structarr
;                   Time-of-Flight reslults, one element per analysis interval. Fields are:
;                       'DMAG'   -   Magnitude of the drift step (meters)
;                       'PHI'    -   Polar angle of the dirft step location in BPP (radians)
;-
pro mms_edi_costfn_bpp2dmpa, b, tri, tof
	compile_opt idl2
	on_error, 2
	
	;Rotation matrix from BPP to DMPA
	dmpa2bpp = mms_edi_xxyz2bpp(b)
	bpp2dmpa = transpose(temporary(dmpa2bpp), [1,0,2])

;-----------------------------------------------------
; Triangulation \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Rotate to DMPA
	d_tri_dmpa = MrVector_Rotate(bpp2dmpa, tri.d_bpp)
	
	;Add to structure
	tri = MrStruct_AddTags(tri, 'd_dmpa', fltarr(3))
	tri.d_dmpa = temporary(d_tri_dmpa)

;-----------------------------------------------------
; Time of Flight \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Rotate to DMPA
	d_bpp = transpose( [ [ tof.dmag * cos(tof.phi) ], $
	                     [ tof.dmag * sin(tof.phi) ], $
	                     [ fltarr(n_elements(tof)) ] ] )
	d_tof_dmpa = MrVector_Rotate(bpp2dmpa, temporary(d_bpp))
	
	;Add to structure
	tof = MrStruct_AddTags(tof, 'd_dmpa', fltarr(3))
	tof.d_dmpa = temporary(d_tof_dmpa)
end


;+
;   Replace bad data represented by NaNs in the E-field results with the official CDF
;   fill value of -1e31.
;
; :Params:
;       STRUCT:     in, requied, type=structarr
;                   Array of structure containing results from either the triangulation
;                       or time-of-flight methods. Required fields are::
;                           D_DMPA      -   Drift step in DMPA coordinates.
;                           V_EXB_DMPA  -   Drift velocity (m/s) in DMPA coordinates.
;                           E_DMPA      -   Electric field (mV/m) in DMPA coordinates.
;-
pro mms_edi_costfn_fillvals, struct
	compile_opt idl2
	on_error, 2
	
	;Change fill values
	struct.d_dmpa     = replace_fillval(struct.d_dmpa, !values.f_nan, REPLACE_VALUE=-1e31)
	struct.v_ExB_dmpa = replace_fillval(struct.d_dmpa, !values.f_nan, REPLACE_VALUE=-1e31)
	struct.E_dmpa     = replace_fillval(struct.d_dmpa, !values.f_nan, REPLACE_VALUE=-1e31)
end


;+
;   Calculate the electric field and drift velocity from drift step measurements.
;
;   Side Effects::
;       Adds tags to `STRUCT`
;           V_EXB_DMPA  -   Drift velocity (m/s) in DMPA coordinates.
;           E_DMPA      -   Electric field (mV/m) in DMPA coordinates.
;
; :Params:
;       STRUCT:     in, requied, type=structarr
;                   Array of structure containing results from either the triangulation
;                       or time-of-flight methods. Required fields are::
;                           D_DMPA  -    Drift step in DMPA coordinates.
;                           TGYRO   -    Gyrotime associated with `B_DMPA`
;       B_DMPA:     in, requied, type=3xN float
;                   Averaged mangetic field associated with each element in `STRUCT`
;-
pro mms_edi_costfn_VECalc, struct, b_dmpa
	compile_opt idl2
	on_error, 2
	
	;Number of points
	npts = n_elements(struct)
	
	;Drift velocity
	;   - Convert m/s -> km/s
	tGyro        = rebin(reform(struct.tGyro, 1, npts), 3, npts)
	v_drift_dmpa = 1e-3 * (struct.d_dmpa / tGyro)
	
	;Electric Field
	;   - Convert from V/m to mV/m with 1e-3
	E_dmpa = MrVector_Cross( b_dmpa, struct.d_dmpa ) / (1e3 * temporary(tGyro))
	
	;Add results to struct
	struct = MrStruct_AddTags(struct, 'v_ExB_dmpa', fltarr(3), $
	                                  'E_dmpa',     fltarr(3))
	struct.v_ExB_dmpa = temporary(v_drift_dmpa)
	struct.E_dmpa     = temporary(E_dmpa)
end


;+
; Process L1A EDI electric field mode data to produce an electric field
; and associated error.
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft identifier. Choices are: 'mms1', 'mms2', 'mms3', 'mms4'
;       TSTART:     in, required, type=string
;                   Start time of interval to be processed. Must be an ISO-8601 string.
;                       Format is 'YYYY-MM-DDThh:mm:ssZ'
;       END:        in, required, type=string
;                   End time of interval to be processed. Must be an ISO-8601 string.
;                       Format is 'YYYY-MM-DDThh:mm:ssZ'
;
; :Returns:
;       EFIELD_FILE:    Name of the electric field CDF file produced.
;-
function mms_edi_test_costfn, sc, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif

	sc       = 'mms1'
	tstart   = '2015-08-01T15:30:00Z' ;01-Aug-2015 05:08:46.030963219  01-Aug-2015 00:41:57.83222159
	tend     = '2015-08-01T16:30:00Z' ;01-Aug-2015 13:19:18.340386093  01-Aug-2015 05:07:01.822075718
	edi_dir  = '/nfs/edi/temp/'
	sdc_dir  = '/nfs/'
	hk_dir   = '/nfs/hk/'
	att_dir  = filepath('', ROOT_DIR='/nfs', SUBDIRECTORY=['ancillary', sc, 'defatt'])
	filename = ''
	quality  = 3
	method   = 1
	dt       = 5
	save_dir = '/nfs/edi/temp/'
	view_bpp = 0
	png_dir  = ''
;	png_dir  = '/nfs/edi/beam_plots/'
	
	;Output error messages to a log file
;	olog = MrStdLog('/nfs/edi/temp/temp_log.txt', /NOTRACEBACK)
	olog = MrStdLog(-2, NOTRACEBACK=1)
	
	
	max_tt_distance = long64(0.25 * 1d9) ;mms_edi_costfn_bGaps
	min_quality     = 3     ;Beam filter
	nbeam_min       = 2     ;mms_edi_beam_class / mms_edi_test_costfn
	maxRunOrder     = !Null ;mms_edi_beam_class
	maxorder_runest = !Null ;mms_edi_beam_class / runest
	penalty_method  = !Null ;mms_edi_beam_class
	sfac            = !Null ;runest
	acfir           = !Null ;mms_edi_tri_rmax
	thresh1         = !Null ;mms_edi_tri_rmax
	thresh2         = !Null ;mms_edi_tri_rmax
	para_ang_lim    = 5     ;mms_edi_test_costfn

;-------------------------------------------------------
; Get Data /////////////////////////////////////////////
;-------------------------------------------------------
	
	;Read data files
	files = mms_edi_costfn_pickfiles(sc, tstart, tend)
	mms_edi_costfn_readfiles, files, tstart, tend, $
	                          EDI_L1B   = edi, $
	                          FGM_L2PRE = fgm_l2pre

	;Remove data that falls within a B data gap
	edi = mms_edi_costfn_bGaps(edi, fgm_l2pre, $
	                           MAX_TT_DISTANCE=max_tt_distance)

;-----------------------------------------------------
; Beam Class & Runner Estimate \\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;1) Cubic Spline interpolation
	b_edi = mms_edi_costfn_binterp(fgm_l2pre.b_bcs, fgm_l2pre.tt2000, edi.tt2000, edi.gdu)
	b_mag = b_edi[3,*]
	b_edi = b_edi[0:2,*]
	
	;Choose which data is passed to RunEst
	ifilter = where(edi.quality  ge min_quality and $ ; minimum quality
	                edi.tof_ovfl eq 0           and $ ; no time-of-flight overflow
	                (edi.max_addr ge 30   and       $ ; Auto tracking
	                 edi.max_addr le 32)        and $
	                (edi.energy   eq 1000 or        $ ; 1keV
	                 edi.energy   eq 500),          $ ; 500eV
	                N_DATA, NCOMPLEMENT=nremove)

	if nremove gt 0 then MrPrintF, 'LogText', nremove, FORMAT='(%"%i beams removed after Quality, Max_Addr, and Energy filter.")'
	edi   = edi[ifilter]
	b_mag = b_mag[ifilter]
	b_edi = b_edi[*,ifilter]

	;Determine the multi-runner order
	;   Beams obtain additional attributes:
	;      RUNORDER - Runner order
	;      RTOF     - Dealiased time of flight
	;      TGYRO    - Gyroperiod
	;      PROB     - Probability of a runner order being true
	;      FLAG     - 1 if PROB passes threshold, 0 if not
	;      ESTOF    - Equivalent single runner time of flight
	;
	;   Error handling for RunEst is redirected to the Catch statement above
	;
	runest = mms_edi_runest(edi, B_mag, SFAC=sfac, /VERBOSE)
	if runest.status ne 0 then return, !Null

;-----------------------------------------------------
; Beam Velocity \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Calculate beam velcoity
	;   Beams obtain additional attributes:
	;      V      - Relativistic beam velocity
	;      RGYRO  - Relativistic gyroradius
	mms_edi_costfn_vBeam, edi, b_mag

;-----------------------------------------------------
; Beam Width \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	beam_width = mmsedi_beamwidth( edi.azimuth, edi.polar, b_edi[0,*], b_edi[1,*], b_edi[2,*], edi.gdu )
	
	;We no longer need this data.
	fgm_l2pre = MrStruct_RemoveTags(fgm_l2pre, ['b_gsm', 'b_gse', 'b_bcs', 'flag', $
	                                            'epoch_state', 'pos_gse', 'pos_gsm'])
	b_mag   = !Null
	b_edi   = !Null

;-----------------------------------------------------
; Rotate EDI to DMPA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Read attitude and sunpulse data
	mms_edi_costfn_readfiles, files, tstart, tend, $
	                          DEFATT    = defatt, $
	                          DSS       = dss, $
	                          ZMPA      = zmpa

	;Rotate from BCS to DMPA
	;  Beams obtain additional attributes
	;     GUN_DMPA - Gun position in DMPA coordinates
	;     DET_DMPA - Detector position in DMPA coordinates
	;     VG_DMPA  - Virtual gun position in DMPA coordinates
	;     FV_DMPA  - Firing vector in DMPA coordinates
	;  And lose others:
	;     GUN_123, DET_123, VG_123, FV_123, GUN_BCS, DET_BCS, VG_BCS, FV_BCS
	mms_edi_costfn_bcs2dmpa, edi, temporary(dss), $
	                         temporary(defatt), temporary(zmpa)

;-----------------------------------------------------
; Compute Average B \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Average magnetic field
	avg = mms_edi_costfn_bavg(edi, fgm_l2pre, DT=dt)
	
	;
	; In cluster, whatech (here, "mms_edi_tri_rmax") redefined the
	; quality flag like so
	;
	;     out = 0 if quality >= 2
	;     out = 1 if quality <  2
	;
	; It was then modified by ep_prep_order (here, "mms_edi_beam_class")
	; and used as a flag for indicating if triangulation or time-of-flight
	; methods should be used. Here, we define the attribute TRITOF_FLAG
	; for this purpose.
	;
	; Note that above, we have already filtered by beam quality, so this
	; should not be necessary. It is not clear yet, however, how Bestarg
	; uses this flag. More digging required.
	;
	edi = MrStruct_AddTags(temporary(edi), 'tritof_flag', 0B)
	edi.tritof_flag = edi.quality lt 2

	;Assign beam class
	;  Beams obtain additional attributes
	;     N_POSORD    - Number of possible runner orders to test later.
	;     POSORD      - Contains the possible runner orders of the beam.
	;     CLASS       - The assigned beam class (see below)
	;     PENALTY     - Runner order penalty
	;     MAXORD      - Maximum runner order.
	status = mms_edi_beam_class( edi, $
	                             NBEAM_MIN       = nbeam_min, $
	                             MAXRUNORDER     = maxrunorder, $
	                             MAXORDER_RUNEST = maxorder_runest, $
	                             PENALTY_METHOD  = penalty_method )
	nbeam = total(edi.tritof_flag gt 1)
	if nbeam gt 0 then begin
		MrPrintF, 'LogText', nbeam, ninds, FORMAT='(%"%i of %i unusable beam classes.")'
	endif
	
	;Find A and B beams
	igood = where(edi.class eq 'A' or edi.class eq 'B', ngood, $
	             COMPLEMENT=ibad, NCOMPLEMENT=nbad)
	if nbad gt 0 then begin
		MrPrintF, 'LogText', nbad, nbad+ngood, FORMAT='(%"%i of %i non-A or B Class beams.")'
		avg        = MrStruct_ReplaceValue(avg, {recnum_edi: avg.recnum_edi[igood]})
		edi        = edi[igood]
		beam_width = beam_width[igood]
	endif else begin
		message, 'No class A or B beams found.'
	endelse

;-----------------------------------------------------
; Step Through Each Interval \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	ntof = 0
	ntri = 0
	npts = n_elements(avg.t_avg)
	for i = 0, npts - 1 do begin
		;Find the beams to use
		inds = where(avg.recnum_edi eq avg.recnum[i], ninds)
		if ninds lt nbeam_min then begin
			MrPrintF, 'LogText', ninds, nbeam_min, FORMAT='(%"Number of beams < NBEAM_MIN (%i < %i)")'
			continue
		endif

		;Data for this interval
		t     = avg.t_avg[i]
		b     = avg.b_avg[*,i]
		beams = edi[inds]
		
		;Rotate to BPP
		;  Beams obtain additional attributes
		;     VG_BPP - Virtual gun positions in BPP
		;     FV_BPP - Firing vector in BPP
		;     ALPHA  - Firing angle in BPP
		mms_edi_costfn_dmpa2bpp, beams, b

		;Sort into toward and away beams
		;  Beams obtain additional attributes
		;     TOAW    - 1 if "toward", -1 if "away"
		mms_edi_costfn_tofsort, beams, phito, sdev
;		ep_toaw_sort_sa, beams, indgen(ninds), phito, sdev

	;-----------------------------------------------------
	; Triangulation \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;TRIANGULATION
		;   - Check for parallel beams
		;   - Pass parallel beam test (SDEV > PARA_ANG_LIM)
		;if checkpar && sdev gt para_ang_lim then begin
			tri_temp = mms_edi_tri(beams, beam_width[inds])
	
	;-----------------------------------------------------
	; Time-of-Flight \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;TIME-OF-FLIGHT
		;   - Forced TOF (CHECKPAR = 0)
		;   - Parallel beams (SDEV < para_ang_lim)
;		endif else begin
			;Time-of-Flight Results
			rmt_temp = mms_edi_rmt( beams,   phito,     sdev, $
			                        bestord, ambig_180, stats, aerror, $
			                        CHECKPAR           = checkpar, $
			                        PARA_ANG_LIM       = para_ang_lim, $
			                        TOFCLASS_NBEAM_MIN = tofclass_nbeam_min )
;		endelse

	;-----------------------------------------------------
	; View/Output Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if view_bpp then begin
			d_tof = rmt.dmag * [cos(rmt.phi), sin(rmt.phi)]
			mms_test_costfn_plot_update, win, sc, t, dt, b, beams, tri.d_bpp, d_tof, $
			                             OUTPUT_DIR=png_dir
		endif

	;-----------------------------------------------------
	; Save Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Allocate memory.
		if i eq 0 then begin
			tri = replicate({edi_tri}, npts)
			tof = replicate({edi_tof}, npts)
		endif
		
		;Save the data
		tri[i] = temporary(tri_temp)
		tof[i] = temporary(rmt_temp)
	endfor

;-----------------------------------------------------
; BPP -> DMPA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Data must be rotated out of BPP with a magnetic field
	;common to the analysis window -- this is the average
	;B-field
	;  Results obtain new attributes
	;     D_DMPA - Drift step in DMPA coordinates (meters)
	mms_edi_costfn_bpp2dmpa, avg.b_avg, tri, tof
	
	;Compute drift velocity and electric field
	;  Results obtain new attributes
	;     V_DMPA - Drift velocity in DMPA coordinates (m/s)
	;     E_DMPA - Electric field in DMPA coordinates (mV/m)
	mms_edi_costfn_VECalc, tri, avg.b_avg
	mms_edi_costfn_VECalc, tof, avg.b_avg

;-----------------------------------------------------
; Gather Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Pick out the gun
	parents = [ files.defatt,  $
	            files.dss,     $
	            files.edi_l1a, $
	            files.fgm_l2pre ]
	ifull = where(parents ne '', nfull)
	if nfull gt 0 then parents = parents[ifull]
	
	;Replace NaNs with FillVals
	mms_edi_costfn_fillvals, tri
	mms_edi_costfn_fillvals, tof

	;Gather the metadata
	meta = { sc:        sc, $
	         instr:     'edi', $
	         mode:      'srvy', $
	         level:     'ql', $
	         optdesc:   'efield', $
	         tstart:    tstart, $
	         tend:      tend, $
	         directory: save_dir, $
	         parents:   temporary(parents) $
	       }

	;Write to file
	efield_file = mms_edi_ql_efield_write(meta, avg.t_avg, tri, tof)
	return, efield_file
end
