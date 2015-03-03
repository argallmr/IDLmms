; docformat = 'rst'
;
; NAME:
;       MMS_EDI_BEAM_BPP
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
;   Rotate beam firing vectors into their instantaneous BPP.
;
; :Categories:
;   MMS, EDI
;
; :Uses:
;   Uses the following external programs::
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2015/02/15  -   Written by Matthew Argall
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
function mms_edi_beam_bpp_view, b_avg, gun1_pos, gun2_pos, gun1_fire, gun2_fire, $
                                edi1_beam_inds, edi2_beam_inds, drift_step_docs, $
DOCS=docs
	compile_opt idl2
	on_error, 2

	docs = keyword_set(docs)

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
	;Figure out which beams correspond to which average value
	hist1 = histogram(edi1_beam_inds, MIN=0, BINSIZE=1, REVERSE_INDICES=ri1)
	hist2 = histogram(edi2_beam_inds, MIN=0, BINSIZE=1, REVERSE_INDICES=ri2)

;-----------------------------------------------------
; Create Window \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Dimensions of square window
	dims   = get_screen_size()
	aspect = dims[0] / dims[1]
	winx   = 500
	winy   = fix(winx / aspect)
	
	;Center it on the screen
	center = dims / 2
	corner = center + [-winx/2, -winy/2]
	
	;Create window
	win    = window(DIMENSIONS=[winx, winy], LOCATION=corner)
	win   -> Refresh, /DISABLE

;-----------------------------------------------------
; Draw S/C, Guns, Fire Vectors, & Target \\\\\\\\\\\\\
;-----------------------------------------------------
	;Draw a set of axes 1.5 times bigger than the s/c
	range = 1.25 * [-sc_sphr_ocs[2,0], sc_sphr_ocs[2,0]]
	gAxes = plot(range, range, /NODATA, /CURRENT, $
	             ASPECT_RATIO=1.0, MARGIN=[0.2, 0.1, 0.1, 0.1], $
	             TITLE='Beam intersections in ' + (docs ? '$B_{Avg}$ BPP' : '$B_{Interp}$ BPP'), $
	             XRANGE=range, XSTYLE=1, XTITLE='Distance (m)', $
	             YRANGE=range, YSTYLE=1, YTITLE='Distance (m)')
	
	;Draw the s/c
	gSC   = polygon(reform(sc_xyz_ocs[0,*]), reform(sc_xyz_ocs[1,*]), /DATA, TARGET=p1)
	
	;Firing directions
	gFire1 = Polyline(range, range, COLOR='Blue', /DATA)
	gFire2 = Polyline(range, range, COLOR='Red',  /DATA)
		
	;Draw drift step
	gTarget = symbol(drift_step_docs[0], drift_step_docs[1], 'X', $
	                 /DATA, TARGET=gAxes, SYM_THICK=2, SYM_SIZE=2.0)
	
	;Refresh the window
	win -> Refresh
	win -> Refresh, /DISABLE

;-----------------------------------------------------
; Draw Each Bavg Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	nPts = n_elements(hist1)
	for i = 0, nPts - 2 do begin
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
		g1_fire = gun1_fire[*,inds1]
		g2_fire = gun2_fire[*,inds2]
		
	;-----------------------------------------------------
	; Rotate into B_avg BPP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Rotate the s/c and drift step into bpp
		xyz2bpp     = mms_instr_xb2bpp(b_avg[*,i])
		sc_xyz_bpp  = rotate_vector(xyz2bpp, sc_xyz_ocs)
		d_bpp       = rotate_vector(xyz2bpp, drift_step_docs)
		
		;Rotate the positions and firing directions into BPP
		if docs then begin
			g1_pos  = rotate_vector(xyz2bpp, g1_pos)
			g2_pos  = rotate_vector(xyz2bpp, g2_pos)
			g1_fire = rotate_vector(xyz2bpp, g1_fire)
			g2_fire = rotate_vector(xyz2bpp, g2_fire)
		endif
	;-----------------------------------------------------
	; Two Points to Define the Beams \\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Beam slope, y-intercept, (x1,x2) and (y1,y2)
		;   - slope (m)       = rise / run
		;   - y-intercept (b) = y1 - m * x1
		;   - (x1,x2)         = range
		;   - (y1,y2)         = m*x + b
		m   = reform( g1_fire[1,*] / g1_fire[0,*] )
		b   = reform( g1_pos[1,*] - g1_pos[0,*] * m )
		g1x = rebin( [range[0], range[1]], 2, n1 )
		g1y = transpose( [[m * g1x[0,*] + b], [m * g1x[1,*] + b]] )
		
		;Beam slope, y-intercept, (x1,x2) and (y1,y2)
		m   = reform( g2_fire[1,*] / g2_fire[0,*] )
		b   = reform( g2_pos[1,*] - g2_pos[0,*] * m )
		g2x = rebin( [range[0], range[1]], 2, n2)
		g2y = transpose( [[m * g2x[0,*] + b], [m * g2x[1,*] + b]] )
		
		;Define connectivity
		;   - Make (x1,x2) and (y1,y2) pairs adjacent
		;   - Indicate connectivity: [2,       2,       2,
		;                                0, 1,    2, 3,    4, 5, ...]
		g1x = reform(g1x, 2 * n1)
		g1y = reform(g1y, 2 * n1)
		g2x = reform(g2x, 2 * n2)
		g2y = reform(g2y, 2 * n2)
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
		gTarget -> SetData, d_bpp[0], d_bpp[1]
		
	;-----------------------------------------------------
	; Next Iteration\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Let me see what is happening
		win -> Refresh
		wait, 0.5
		win -> Refresh, /DISABLE
		
		;Delete the guns
		if i ne nPts - 2 then gGuns -> Delete
	endfor

	win -> Refresh
	return, win
end

;+
;   Determine the firing angle of each beam in their own BPP.
;
;   Uses::
;       Digital Sun Sensor:
;           Time
;       Magnetometer:
;           Interpolated Magnetic Field
;       EDI:
;           Time
;           Analog Voltages
;
;   Creates:
;       EDI:
;           Gun positions in OCS
;           Gun positions in despun frame with detectors as origins
;           Gun positions in BPP with detectors as origins
;           Gun firing directions in EDI_GUN
;           Gun firing directions in OCS
;           Gun firing directions in BPP
;
; :Params:
;       B_AVG_FILE:         in, required, type=string
;                           CDF file containing 5-s averaged DFG magnetic field vectors.
;
; :Keywords:
;       TEST_DATA:          in, optional, type=struct/boolean, default=0
;                           Either a test data structure returned by mms_edi_bavg.pro or
;                               a boolean value indicating that test data should be used.
;       VIEW:               in, optional, type=integer, default=0
;                           If set, data will be displayed in a graphics window. Options are::
;                               0   -   Do not display data.
;                               1   -   Data is projected into BPP defined by the average B.
;                               2   -   Data is projected into BPP defined by the interpolated B.
;-
function mms_edi_beam_bpp, b_avg_file, $
TEST_DATA=test_data, $
VIEW=view
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Get the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(test_data) eq 0 then begin
		;Open the file
		oCDF = MrCDF_File(b_avg_file)
	
		;Get the dependent EDI file
		parents   = oCDF -> GetGlobalAttrVal('PARENTS')
		edi_fname = strsplit(parents[1], '>', /EXTRACT)
		edi_fname = edi_fname[1]
	
		;Get the SRT file
		srt_file = '/Users/argall/Documents/IDL/MMS/test_data/mms3_srt_20010704_v0.2.0.cdf'
		oSRT     = MrCDF_File(srt_file)
	
		;Open the EDI file
		root      = '/Users/argall/Documents/IDL/MMS/test_data/'
		edi_fname = filepath(edi_fname, ROOT_DIR=root)
		oEDI      = MrCDF_File(edi_fname)
	
		;Read the data
		b_beam1 = oCDF -> Read('mms3_dfg_b_beam1')
		b_beam2 = oCDF -> Read('mms3_dfg_b_beam2')
		srt     = oSRT -> Read('tspin')
		vax1    = oEDI -> Read('mms3_edi_vax1', DEPEND_0=t_beam1)
		vay1    = oEDI -> Read('mms3_edi_vay1')
		vax2    = oEDI -> Read('mms3_edi_vax2', DEPEND_1=t_beam2)
		vay2    = oEDI -> Read('mms3_edi_vay2')
	
		;Get B's fill value
		b_fillval = oCDF -> GetVarAttrValue('mms3_dfg_b_beam1', 'FILLVAL')
	
		;Close the EDI file
		obj_destroy, oEDI
	endif else begin
		if size(test_data, /TNAME) ne 'STRUCT' then begin
			test_data = mms_edi_test_data()
			test_data = mms_edi_bavg(TEST_DATA=test_data)
		endif
		srt       = test_data.srt
		omega     = test_data.spin_freq
		vax1      = reform(test_data.edi1_avolt[0,*])
		vay1      = reform(test_data.edi1_avolt[1,*])
		vax2      = reform(test_data.edi2_avolt[0,*])
		vay2      = reform(test_data.edi2_avolt[1,*])
		t_beam1   = test_data.edi_time
		t_beam2   = test_data.edi_time
		b_beam1   = test_data.b_beam1
		b_beam2   = test_data.b_beam2
		b_fillval = 1e-31
	endelse
	view = n_elements(view) eq 0 ? 0 : 0 > view < 2

;-----------------------------------------------------
; Positions in DOCS (Apply Spin) \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; In the rotating frame, the gun positions are static while the field is rotating.
	; In the despun frame, the field is stationary, but the gun positions are rotating.
	;

	;Positions of Guns & Detectors in OCS
	;   Shift from the OCS origin to the optics (detector) origins. Detector 2 serves as
	;   the optics for Gun 1, so get the Gun1 position with respect to detector2.
	gun1_pos_ocs = mms_instr_origins_instr('EDI1_GUN', 'EDI2_DETECTOR')
	gun2_pos_ocs = mms_instr_origins_instr('EDI2_GUN', 'EDI1_DETECTOR')

	;Spin-up positions
	gun1_pos_docs = mms_dss_despin(srt, t_beam1, gun1_pos_ocs, OMEGA=omega)
	gun2_pos_docs = mms_dss_despin(srt, t_beam2, gun2_pos_ocs, OMEGA=omega)

;-----------------------------------------------------
; Firing Voltages as Angles in DOCS \\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Gun firing directions do not need to be despun. Instead, the gun
	; positions are spun.
	;
	
	;Convert analog firing voltages to cartesian firing direction.
	gun1_fire_cart = mms_edi_aVoltage2Angle(vax1, vay1, /CARTESIAN)
	gun2_fire_cart = mms_edi_aVoltage2Angle(vax2, vay2, /CARTESIAN)
	
	;Matrix from EDI to OCS
	edi12ocs = mms_instr_xxyz2ocs('EDI1_GUN')
	edi22ocs = mms_instr_xxyz2ocs('EDI2_GUN')
	
	;Rotate to OCS
	gun1_fire_ocs = rotate_vector(edi12ocs, gun1_fire_cart)
	gun2_fire_ocs = rotate_vector(edi22ocs, gun2_fire_cart)

	;Despin firing directions to DOCS
	gun1_fire_docs = mms_dss_despin(srt, t_beam1, gun1_fire_ocs, OMEGA=omega)
	gun2_fire_docs = mms_dss_despin(srt, t_beam2, gun2_fire_ocs, OMEGA=omega)

;-----------------------------------------------------
; Project into BPP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find the beams
	iBeam1 = where(b_beam1[0,*] ne b_fillval and b_beam1[1,*] ne b_fillval and b_beam1[2,*] ne b_fillval, nBeam1)
	iBeam2 = where(b_beam2[0,*] ne b_fillval and b_beam2[1,*] ne b_fillval and b_beam2[2,*] ne b_fillval, nBeam2)
	
	;Beam1
	if nBeam1 gt 0 then begin
		;Throw away data
		b_beam1       = b_beam1[*,iBeam1]
		gun1_fire_ocs = gun1_fire_ocs[*,iBeam1]
		
		;Transform from OCS into BPP
		ocs2bpp       = mms_instr_xb2bpp(b_beam1)
		gun1_pos_bpp  = rotate_vector(ocs2bpp, gun1_pos_docs)
		gun1_fire_bpp = rotate_vector(ocs2bpp, gun1_fire_docs)
	endif
	
	;Beam2
	if nBeam2 gt 0 then begin
		;Throw away data
		b_beam2       = b_beam2[*,iBeam2]
		gun2_fire_ocs = gun2_fire_ocs[*,iBeam2]
		
		;Transformation matrix from OCS into BPP
		ocs2bpp       = mms_instr_xb2bpp(b_beam2)
		gun2_pos_bpp  = rotate_vector(ocs2bpp, gun2_pos_docs)
		gun2_fire_bpp = rotate_vector(ocs2bpp, gun2_fire_docs)
	endif

;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	test_data = create_struct( test_data, $
	                           'gun1_pos_bpp',  gun1_pos_bpp, $
	                           'gun1_fire_bpp', gun1_fire_bpp, $
	                           'gun2_pos_bpp',  gun2_pos_bpp, $
	                           'gun2_fire_bpp', gun2_fire_bpp $
	                         )
	
	if view gt 0 then begin
		if view eq 1 $
			then win = mms_edi_beam_bpp_view(test_data.b_avg, gun1_pos_docs, gun2_pos_docs, $
			                                 gun1_fire_docs, gun2_fire_docs, $
			                                 test_data.edi1_beam_inds, test_data.edi2_beam_inds, $
			                                 test_data.drift_step_docs, /DOCS) $
			else win = mms_edi_beam_bpp_view(test_data.b_avg, gun1_pos_bpp, gun2_pos_bpp, $
			                                 gun1_fire_bpp, gun2_fire_bpp, $
			                                 test_data.edi1_beam_inds, test_data.edi2_beam_inds, $
			                                 test_data.drift_step_docs)
	    return, win
	endif
	
	return, test_data
end