; docformat = 'rst'
;
; NAME:
;       mms_edi_view_bpp
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
;   Visualize EDI beam in the plane perpendicular to B (BPP).
;
; :Categories:
;   MMS, EDI
;
; :Returns:
;       WIN:        Graphics window containing the plot of EDI beams.
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
;       2015/05/05  -   Written by Matthew Argall
;-
function mms_edi_view_bpp
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Get Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	edi_dir      = '/Users/argall/Documents/Work/Data/MMS/EDI/'
	dfg_dir      = '/Users/argall/Documents/Work/Data/MMS/DFG/'
	fg_cal_dir   = '/Users/argall/Documents/Work/Data/MMS/FG_Cal/'
	sunpulse_dir = '/Users/argall/Documents/Work/Data/MMS/HK/'
	attitude_dir = '/Users/argall/Documents/Work/Data/MMS/Ephemeris/'
	sc           = 'mms4'
	tstart       = '2015-04-22T17:03:15Z'
	tend         = '2015-04-22T17:03:30Z'
	quality      = 3
	
	;Get FG data
	!Null = mms_fg_gse(sc, 'dfg', 'f128', tstart, tend, $
	                  B_DMPA       = b_fg, $
	                  CAL_DIR      = fg_cal_dir, $
	                  DATA_DIR     = dfg_dir, $
	                  EPOCH        = t_fg, $
	                  SUNPULSE_DIR = sunpulse_dir)

	;Get EDI data
	edi = mms_edi_gse(sc, 'slow', tstart, tend, /DMPA, $
	                  DIRECTORY    = edi_dir, $
	                  QUALITY      = quality, $
	                  SUNPULSE_DIR = sunpulse_dir)

;-----------------------------------------------------
; Extract Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	t_gd12   = edi.epoch_gd12
	t_gd21   = edi.epoch_gd21
	fv_gd12  = edi.fv_gd12_dmpa
	fv_gd21  = edi.fv_gd21_dmpa
	gun1_pos = edi.virtual_gun1_dmpa
	gun2_pos = edi.virtual_gun2_dmpa
	edi      = !Null

	b_avg = mean(b_fg,  DIMENSION=2)
	b_std = stddev(b_fg, DIMENSION=2)

;-----------------------------------------------------
; Rotate to BPP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Transformation to BPP
	xyz2bpp = mms_edi_xxyz2bpp(b_avg)
	
	;Rotate firing vectors and gun positions
	fv_gd12_bpp  = MrVector_Rotate(xyz2bpp, fv_gd12)
	fv_gd21_bpp  = MrVector_Rotate(xyz2bpp, fv_gd21)
	gun1_pos_bpp = MrVector_Rotate(xyz2bpp, gun1_pos)
	gun2_pos_bpp = MrVector_Rotate(xyz2bpp, gun2_pos)

;-----------------------------------------------------
; Create S/C \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Spacecraft outline
	;   - Create a circle with a radius that extends from optics to gun.
	nVerts       = 200
	radius       = mms_instr_origins_instr('EDI1_GUN', 'EDI2_DETECTOR')
	sc_sphr      = fltarr(3, nVerts)
	sc_sphr[2,*] = sqrt(total(radius^2))
	sc_sphr[0,*] = 2.0 * !pi * findgen(nVerts)/(nVerts-1.0)
	sc_xyz       = cv_coord(FROM_SPHERE=sc_sphr, /TO_RECT)
	
	;Rotate to BPP
	sc_xyz_bpp = MrVector_Rotate(xyz2bpp, sc_xyz)

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
	win  = window(DIMENSIONS=[winx, winy], LOCATION=corner)
	win -> Refresh, /DISABLE

;-----------------------------------------------------
; Draw S/C, Guns & Fire Vectors \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Draw a set of axes 1.5 times bigger than the s/c
	range = 1.25 * [-sc_sphr[2,0], sc_sphr[2,0]]
	gAxes = plot(range, range, /NODATA, /CURRENT, $
	             ASPECT_RATIO = 1.0, $
	             TITLE        = 'Beam intersections in $B_{Avg}$ BPP', $
	             XRANGE       = range, $
	             XSTYLE       = 1, $
	             XTITLE       = 'Distance (m)', $
	             YRANGE       = range, $
	             YSTYLE       = 1, $
	             YTITLE       = 'Distance (m)')
	
	;Draw the s/c
	gSC = polygon(reform(sc_xyz_bpp[0,*]), reform(sc_xyz_bpp[1,*]), /DATA, TARGET=gAxes)
	
	;Firing directions
	gFire1 = Polyline(range, range, COLOR='Blue', /DATA, TARGET=gAxes)
	gFire2 = Polyline(range, range, COLOR='Red',  /DATA, TARGET=gAxes)
		
;-----------------------------------------------------
; Two Points to Define the Beams \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of points
	n1 = n_elements(t_gd12)
	n2 = n_elements(t_gd21)

	;Beam slope, y-intercept, (x1,x2) and (y1,y2)
	;   - slope (m)       = rise / run
	;   - y-intercept (b) = y1 - m * x1
	;   - (x1,x2)         = range
	;   - (y1,y2)         = m*x + b
	m   = reform( fv_gd12_bpp[1,*] / fv_gd12_bpp[0,*] )
	b   = reform( gun1_pos_bpp[1,*] - gun1_pos_bpp[0,*] * m )
	g1x = rebin( [range[0], range[1]], 2, n1 )
	g1y = transpose( [[m * g1x[0,*] + b], [m * g1x[1,*] + b]] )
	
	;Beam slope, y-intercept, (x1,x2) and (y1,y2)
	m   = reform( fv_gd21_bpp[1,*] / fv_gd21_bpp[0,*] )
	b   = reform( gun2_pos_bpp[1,*] - gun2_pos_bpp[0,*] * m )
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
	;Draw the gun positions
	gGuns = symbol(reform([[gun1_pos_bpp[0,*]], [gun2_pos_bpp[0,*]]]), $
	               reform([[gun1_pos_bpp[1,*]], [gun2_pos_bpp[1,*]]]), 'circle', $
	               /DATA, TARGET=gAxes)

	;Draw the beams
	gFire1 -> SetData, g1x, g1y, CONNECTIVITY=conn1
	gFire2 -> SetData, g2x, g2y, CONNECTIVITY=conn2

	win -> Refresh
	return, win
end
