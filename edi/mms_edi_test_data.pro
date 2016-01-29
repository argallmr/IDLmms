; docformat = 'rst'
;
; NAME:
;       MMS_EDI_Test_Data
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
;         contributors may be used to endorse or promote products derived from this      ;
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
;   Create a set of test data for the EDI bestarg process.
;
; :Examples:
;   View the test data::
;       IDL> data = mms_edi_test_data(/VIEW)
;
; :Categories:
;       MMS, EDI, Bestarg
;
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/02/25  -   Written by Matthew Argall
;       2015/03/04  -   Force at least one sun pulse time. - MRA
;-
;*****************************************************************************************
;+
;   Create time tags for a data set. All data must be in the despun-ocs coordinate system.
;
; :Params:
;       BMAG:           in, required, type=float, default=40.0
;                       Magnitude of the magnetic field to create, in nano-Tesla
;       EMAG:           in, required, type=float, default=0.5
;                       Magnitude of the electric field to create, in milli-Volts/meter
;       GUN1_POS_DOCS:  in, required, type=3xN float
;                       Position of Gun1 with respect to Detector2, in millimeters.
;       GUN2_POS_DOCS:  in, required, type=3xN float
;                       Position of Gun2 with respect to Detector1, in millimeters.
;
; :KEYWORDS:
;       VERBOSE:        in, optional, type=boolean, default=0
;                       Print test values.
;       VIEW:           in, optional, type=boolean, default=0
;                       If set, a graphic of the test data will be created.
;-
function mms_edi_test_fa, B_docs, E_docs, gun1_pos_docs, gun2_pos_docs, $
VIEW=view
	compile_opt idl2
	on_error, 2

	;Defaults
	view    = keyword_set(view)
	nPts    = n_elements(B_docs[0,*])
	
;---------------------------------------------------------------------
; Rotate into BPP ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Determine the B-perp plane
	xyz2bpp = mms_instr_xb2bpp(B_docs)
	
	;Fields
	;   - Should be (0, 0, |B|)
	B_BPP = rotate_vector(xyz2bpp, B_docs)
	E_BPP = rotate_vector(xyz2bpp, E_docs)

	;Gun positions
	gun1_pos_bpp = rotate_vector(xyz2bpp, gun1_pos_docs)
	gun2_pos_bpp = rotate_vector(xyz2bpp, gun2_pos_docs)

;---------------------------------------------------------------------
; Drift Step & Firing Angle //////////////////////////////////////////
;---------------------------------------------------------------------
	;Drift Step from Electric Field
	;   - Given B = 40 nT and E = 0.5 mV/m
	;       - T_g = 893.00 micro-s  = 2*!pi*m/(qB)
	;       - v_D =  12.50 km/s     = ExB/|B|^2
	;       - d   = 156.25 m        = v_D*T_g
	;   - Scaling
	;       - T_g: kg/C-nT --> kg/C-T * 1e9 --> s   * 1e9 => Must multiply result by 1e9
	;       - v_D: mV/m-nT --> V/m-T  * 1e6 --> m/s * 1e6 => Must multiply result by 1e6
	Bmag           = magnitude_vec(B_docs)
	gyro_period    = 1e9 * constants('m_e', /DOUBLE) / (constants('q', /DOUBLE) * Bmag)
	drift_vel_bpp  = 1e6 * cross_product(E_BPP, B_BPP) / rebin(reform(Bmag^2, 1, nPts), 3, nPts)
	drift_step_bpp =       drift_vel_bpp * rebin(reform(gyro_period, 1, nPts), 3, nPts)

	;Electric Field from Drift Step
	;   - Given B = 40 nT and d = 5m
	;       - T_g = 893.00 micro-s  = 2*!pi*m/(qB)
	;       - v_D = 1.680  km/s     = d / T_g
	;       - E   = 0.223  mV/m     = v_D * B
	;   - Scaling
	;       - v_D: m/s    => No scaling
	;       - E:   m-nT/s --> m-T/s * 1e-9 --> mV/m * 1e-6 => Must multiply result by 1e-6
;	Bmag           = reform(B_bpp[2,*])
;	gyro_period    = 1e9 * constants('m_e', /DOUBLE) / (constants('q', /DOUBLE) * Bmag)
;	drift_vel_bpp  = d_bpp / gyro_period
;	E_bpp          = drift_vel_bpp * Bmag * 1e-6

;---------------------------------------------------------------------
; Angles from Guns to Drift Step /////////////////////////////////////
;---------------------------------------------------------------------
	;Vector pointing from drift step to guns
	;   - Should be (vx, vy, 0)
	;   - One gun fires toward and one away from the drift step.
	d_to_g1_bpp =   drift_step_bpp - gun1_pos_bpp
	d_to_g2_bpp = -(drift_step_bpp - gun2_pos_bpp)

;---------------------------------------------------------------------
; Rotate Back to DOCS ////////////////////////////////////////////////
;---------------------------------------------------------------------
	bpp2xyz         = transpose(xyz2bpp, [1, 0, 2])
	gun1_fire_docs  = rotate_vector(bpp2xyz, d_to_g1_bpp)
	gun2_fire_docs  = rotate_vector(bpp2xyz, d_to_g2_bpp)
	drift_step_docs = rotate_vector(bpp2xyz, drift_step_bpp)
	
	;Normalize firing directions
	gun1_fire_docs = normalize(gun1_fire_docs)
	gun2_fire_docs = normalize(gun2_fire_docs)

;---------------------------------------------------------------------
; Data to Output /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Keep the data
	data = { B_docs:          B_docs, $
	         E_docs:          E_docs, $
	         gun1_fire_docs:  gun1_fire_docs, $
	         gun2_fire_docs:  gun2_fire_docs, $
	         drift_step_docs: drift_step_docs $
	       }

;---------------------------------------------------------------------
; View ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if view then begin
		win = mms_edi_test_view(B_BPP, E_BPP, drift_step_bpp, xyz2bpp, $
		                        gun1_pos_bpp, gun2_pos_bpp, $
		                        d_to_g1_bpp, d_to_g2_bpp)
	endif

	return, data
end


;+
;   Create time tags in CDF_TIME_TT2000 format.
;
; :Params:
;       TSTART:         in, required, type=string
;                       Start time, formatted as `YYYY-MM-DDThh:mm:ss.mmmuuunnn', with
;                           the year, month, day, hour, minute seconds, milli-, micro-
;                           and nano-seconds.
;       DURATION:       in, required, type=double
;                       Length of signal, in seconds.
;       RATE:           in, required, type=double
;                       Sampling rate, in samples per second.
;
; :Returns:
;       TIME:           Time tags in CDF_TIME_TT2000 format.
;-
function mms_edi_test_fields, test_case, sample_rate, duration, spin_freq
	compile_opt idl2
	on_error, 2

	;Default to the first test case
	if n_elements(test_case) eq 0 then begin
		cases = [ ['CASE', 'DESCRIPTION'], $
		          ['  1 ', 'Constant B & E vector, reproducible.'], $
		          ['  2 ', 'Constant B & E vector, not reproducible.'], $
		          ['  3 ', '|B| varies as ~8nT per spin, E is constant.'], $
		          ['  4 ', '|B| is constant, b rotates opposite s/c spin. Constant E field.'] $
		        ]
		print, FORMAT='(a4, 4x, a0)', cases
		return, !Null
	endif
	
	;Number of samples
	nPts   = sample_rate * duration
	nSpins = floor(duration * spin_freq / (2.0 * !pi)) > 1
	n_spin = floor(nPts / nSpins)
	
	;Build the test data
	case test_case of
	;-----------------------------------------------------
	; Constant, Reproducible B & E \\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		1: begin
			;Initial seed to provide reproducability
			seed = 3
			Bmag = 40
			Emag = 0.5
			
			;Create a random B unit vector, then multiply by magnitude
			B_hat = randomu(seed, 3) - 0.5
			B_hat = B_hat / sqrt(total(B_hat^2))
			B     = B_hat * Bmag
			
			;Create another random unit vector
			;   - Cross it with B
			;   - Multiply by E magnitude to get E vector
			unit   = randomu(seed, 3) - 0.5
			unit  /= sqrt(total(unit^2))
			E_hat  = [ unit[1]*B[2] - unit[2]*B[1], $
			           unit[2]*B[0] - unit[0]*B[2], $
			           unit[0]*B[1] - unit[1]*B[0] ]
			E_hat /= sqrt(total(E_hat^2))
			E      = Emag * E_hat
			
			;Produce the correct number of points
			B = rebin(B, 3, nPts)
			E = rebin(E, 3, nPts)
		endcase
		
	;-----------------------------------------------------
	; Constant, Random B & E \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		2: begin
			;Do not provide seed, so that it is initialized by time stamp.
			Bmag = 40.0    ;nT
			Emag =  0.5    ;mV/m
			
			;Create a random B unit vector, then multiply by magnitude
			B_hat = randomu(seed, 3) - 0.5
			B_hat = B_hat / sqrt(total(B_hat^2))
			B     = B_hat * Bmag
			
			;Create another random unit vector
			;   - Cross it with B
			;   - Multiply by E magnitude to get E vector
			unit   = randomu(seed, 3) - 0.5
			unit  /= sqrt(total(unit^2))
			E_hat  = [ unit[1]*B[2] - unit[2]*B[1], $
			           unit[2]*B[0] - unit[0]*B[2], $
			           unit[0]*B[1] - unit[1]*B[0] ]
			E_hat /= sqrt(total(E_hat^2))
			E      = Emag * E_hat
			
			;Produce the correct number of points
			B = rebin(B, 3, nPts)
			E = rebin(E, 3, nPts)
		endcase
		
	;-----------------------------------------------------
	; |B| Varies by ~8nT Per Spin \\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		3: begin
			;Base magnitudes.
			seed = 2
			Bmag = 40    ;nT
			Emag = 0.5   ;mV/m
			
			;|B| increases at a constant rate.
			;   - Increase 8nT every spin
			;   - Reach Bmag half way through the sample interval
			slope = 20 * spin_freq / (sample_rate * 2.0 * !pi)     ;nT / s
			start = -(slope * nPts/2.0) + Bmag
			Bmag  = MrMake_Array(nPts, START=start, INCREMENT=slope)

			;Create B
			;   - Start with a unit vector of constant direction
			;   - Multiply by |B|
			B      = fltarr(3, nPts)
			B_hat  = randomu(seed, 3) - 0.5
			B_hat  = B_hat / sqrt(total(B_hat^2))
			B[0,*] = B_hat[0] * Bmag
			B[1,*] = B_hat[1] * Bmag
			B[2,*] = B_hat[2] * Bmag

			;Create another random unit vector
			;   - Cross it with B
			;   - Multiply by E magnitude to get E vector
			unit   = randomu(seed, 3) - 0.5
			unit  /= sqrt(total(unit^2))
			E_hat  = [ unit[1]*B[2,0] - unit[2]*B[1,0], $
			           unit[2]*B[0,0] - unit[0]*B[2,0], $
			           unit[0]*B[1,0] - unit[1]*B[0,0] ]
			E_hat /= sqrt(total(E_hat^2))
			E      = E_hat * Emag
			E      = rebin(E, 3, nPts)
		endcase
		
	;-----------------------------------------------------
	; |B| Consant, b Rotates, E Constant \\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		4: begin
			;Base magnitudes
			Bmag = 40    ;nT
			Emag = 0.5   ;mV/m
			
			;|B| is constant, but B is circularly polarized in Bx and By
			;   - Frequency is 1/4 spin
			signal = MrSigGen(sample_rate, duration, $
			                  AMP_A       = Bmag, $
			                  DC          = 0, $
			                  FREQUENCIES = 2.0 * spin_freq / (2.0 * !pi))

			;Store the in-phase and in-quadrature parts as Bx and By
			B      = fltarr(3, nPts)
			B[0,*] = real_part(signal[0:-2])
			B[1,*] = imaginary(temporary(signal[0:-2]))
			
			;Create E perpendicular to B
			E = [0.0, 0.0, Emag]
			
			;Rotate into an arbirary direction
			rotmat = MrEulerMatrix(35.0, 105.0, 0.0, ORDER=[3,2,1], /ANGLES)
			B      = rotate_vector(rotmat, B)
			E      = rotate_vector(rotmat, E)

			;Replicate E
			E = rebin(E, 3, nPts)
		endcase
	endcase

;-----------------------------------------------------
; Return the Fields \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	fields = { B_docs: B, $
	           E_docs: E }

	return, fields
end


;+
;   Create time tags in CDF_TIME_TT2000 format.
;
; :Params:
;       TSTART:         in, required, type=string
;                       Start time, formatted as `YYYY-MM-DDThh:mm:ss.mmmuuunnn', with
;                           the year, month, day, hour, minute seconds, milli-, micro-
;                           and nano-seconds.
;       DURATION:       in, required, type=double
;                       Length of signal, in seconds.
;       RATE:           in, required, type=double
;                       Sampling rate, in samples per second.
;
; :Returns:
;       TIME:           Time tags in CDF_TIME_TT2000 format.
;-
function mms_edi_test_times, tstart, duration, rate
	compile_opt idl2
	on_error, 2

	;Turn the start time into a TT2000 time
	t_tt2000 = cdf_parse_tt2000(tstart)

	;Number of data points to create
	nSamples  = (duration * rate) > 1

	;Time stamps, in seconds from tstart
	t_temp = dindgen(nSamples) / nSamples * duration

	;Time stamps in TT2000
	time = long64(t_temp * 1D9) + t_tt2000

	return, time
end


;+
;   Create a scalar with random magnitude or unit vector pointing in a random direction.
;
; :Params:
;       MAGNITUDE:          in, optional, type=float, default=1.0
;                           Scale factor for the random data.
;
; :Keywords:
;       SCALAR:             in, optional, type=boolean, defualt=0
;                           Create a scalar instead of a vector.
;       SEED:               in, optional, type=integer, default=1
;                           The seed used to generate random numbers.
;
; :Returns:
;       VALUE:              A scalar or 3-element vector.
;-
function mms_edi_test_view, B_BPP, E_BPP, D_BPP, xyz2bpp, $
                            gun1_pos_bpp, gun2_pos_bpp, $
                            gun1_fire_bpp, gun2_fire_bpp
	compile_opt idl2
	on_error, 2

	nPts   = n_elements(gun1_pos_bpp[0,*])
;-----------------------------------------------------
; Create S/C \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Spacecraft outline
	;   - Create a circle with a radius that extends from optics to gun.
	nVerts           = 200
	gun1_pos         = mms_instr_origins_instr('EDI1_GUN', 'EDI2_DETECTOR')
	sc_sphr_ocs      = fltarr(3, nVerts)
	sc_sphr_ocs[2,*] = sqrt(total(gun1_pos^2))
	sc_sphr_ocs[0,*] = 2.0 * !pi * findgen(nVerts)/(nVerts-1.0)
	sc_xyz_ocs       = cv_coord(FROM_SPHERE=sc_sphr_ocs, /TO_RECT)

;-----------------------------------------------------
; Draw S/C \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Create a square window
	dims   = get_screen_size()
	aspect = dims[0] / dims[1]
	winx   = 500
	winy   = fix(winx / aspect)
	win    = window(DIMENSIONS=[winx, winy], WINDOW_TITLE='Bestarg Test Data')
	win   -> Refresh, /DISABLE
	
	;Draw a set of axes 1.5 times bigger than the s/c, then draw the s/c
	range = 1.5 * [-sc_sphr_ocs[2,0,0], sc_sphr_ocs[2,0,0]]
	gAxes = plot(range, range, /NODATA, /CURRENT, $
	             TITLE  = 'Test Data', $
	             XRANGE = range, $
	             XSTYLE = 1, $
	             XTITLE = 'Distance (m)', $
	             YRANGE = range, $
	             YSTYLE = 1, $
	             YTITLE = 'Distance (m)')
	gSC   = polygon(reform(sc_xyz_ocs[0,*]), reform(sc_xyz_ocs[1,*]), /DATA, TARGET=gAxes)
	
	;Firing directions
	gFire1 = Polyline(range, range, COLOR='Blue', /DATA, TARGET=gAxes)
	gFire2 = Polyline(range, range, COLOR='Red',  /DATA, TARGET=gAxes)
	
	;Drift Step
	gDriftStep = Symbol(D_BPP[0,0], D_BPP[1,0], 'Star', /DATA, TARGET=gAxes)

;-----------------------------------------------------
; Draw Each Test Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	iStart = 0
	nStep  = 80
	iStop  = iStart + nStep - 1
	while iStart lt nPts do begin
		b       = B_BPP[*,iStart:iStop]
		g1_pos  = gun1_pos_bpp[*,iStart:iStop]
		g2_pos  = gun2_pos_bpp[*,iStart:iStop]
		g1_fire = gun1_fire_bpp[*,iStart:iStop]
		g2_fire = gun2_fire_bpp[*,iStart:iStop]
		d       = D_bpp[*,iStart:iStop]

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
		g1x = rebin( [range[0], range[1]], 2, nStep )
		g1y = transpose( [[m * g1x[0,*] + b], [m * g1x[1,*] + b]] )
		
		;Beam slope, y-intercept, (x1,x2) and (y1,y2)
		m   = reform( g2_fire[1,*] / g2_fire[0,*] )
		b   = reform( g2_pos[1,*] - g2_pos[0,*] * m )
		g2x = rebin( [range[0], range[1]], 2, nStep)
		g2y = transpose( [[m * g2x[0] + b], [m * g2x[1] + b]] )
		
		;Define connectivity
		;   - Make (x1,x2) and (y1,y2) pairs adjacent
		;   - Indicate connectivity: [2,       2,       2,
		;                                0, 1,    2, 3,    4, 5, ...]
		g1x = reform(g1x, 2 * nStep)
		g1y = reform(g1y, 2 * nStep)
		g2x = reform(g2x, 2 * nStep)
		g2y = reform(g2y, 2 * nStep)
		conn1 = reform([replicate(2,1,nStep), lindgen(2,nStep)], 3*nStep)
		conn2 = reform([replicate(2,1,nStep), lindgen(2,nStep)], 3*nStep)
		
	;-----------------------------------------------------
	; Update Graphics \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Rotate S/C into average BPP
		b_avg   = mean(B_BPP[*,iStart:iStop], DIMENSION=2)
		ocs2bpp = xyz2bpp[*, *, floor( iStart + (iStop - iStart) / 2 )]
		sc_pos_bpp = rotate_vector(ocs2bpp, sc_xyz_ocs)
		gSC -> SetData, reform(sc_pos_bpp[0,*]), reform(sc_pos_bpp[1,*])
	
		;Update the gun positions
		gGuns = symbol(reform([[g1_pos[0,*]], [g2_pos[0,*]]]), $
		               reform([[g1_pos[1,*]], [g2_pos[1,*]]]), 'Circle', $
		               /DATA, TARGET=gAxes)

		;Draw the beams
		gFire1 -> SetData, g1x, g1y, CONNECTIVITY=conn1
		gFire2 -> SetData, g2x, g2y, CONNECTIVITY=conn2
		
		;Drift step
		gDriftStep = symbol(d[0,*], d[1,*], 'Star', /DATA, TARGET=gAxes)
		
	;-----------------------------------------------------
	; Next Iteration\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		win -> Refresh
	
		;Let me see what is happening
		wait, 0.5
		win -> Refresh, /DISABLE
		
		;Delete the guns
		gGuns      -> Delete
		gDriftStep -> Delete
		
		;Advance
		iStart = iStop + 1
		iStop  = (iStop + nStep) < nPts
	endwhile

	return, win
end


;+
;   Create test data.
;-
function mms_edi_test_data, test_case, $
VIEW=view
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		void = cgErrorMSG(/QUIET)
		return, !Null
	endif
	
	;Show which tests are available?
	if n_elements(test_case) eq 0 then begin
		void = mms_edi_test_fields()
		return, !Null
	endif

	;Experimental Setup
	;   - Spin is right-handed (counter-clockwise) about the z-axis.
	;   - Instrument CS are aligned with the s/c CS, except for a single rotation about z by OFFSET.
	;   - All datasets are initially sampled at SAMPLE_RATE, then are down sampled
	tstart          = '2015-03-12T00:00:00.000000000'
	duration        = 10.0                                      ;seconds
	sample_rate     = 64.0                                      ;Samples / second
	spin_rate       = 3.0 / 60.0                                ;# rev / sec
	spin_freq       = 2.0 * !pi * spin_rate                     ;rad / sec
	
	;Set instrument-specific sampling rates
	B_sample_rate   = sample_rate                               ;Samples / second
	E_sample_rate   = sample_rate                               ;Samples / second
	edi_sample_rate = 16.0                                      ;Samples / second

	;Create times in tt2000 format
	;   - Generic time array, then instrument time arrays.
	;   - Down sampling occurs after data creation.
	time     = mms_edi_test_times(tstart, duration, sample_rate)
	srt      = mms_edi_test_times(tstart, duration, spin_rate)
	B_time   = mms_edi_test_times(tstart, duration, b_sample_rate)
	E_time   = mms_edi_test_times(tstart, duration, e_sample_rate)
	edi_time = mms_edi_test_times(tstart, duration, edi_sample_rate)
	
	;Create fields
	fields = mms_edi_test_fields(test_case, sample_rate, duration, spin_freq)
	
	;Create SC positions in despun-ocs
	;   - For EDI triangulation method, the detectors are at the center of the s/c
	;     and the guns are on the fringes.
	;   - Required to calculate firing directions in BPP
	gun1_pos_det2_ocs  = mms_instr_origins_instr('EDI1_GUN', 'EDI2_DETECTOR')
	gun2_pos_det1_ocs  = mms_instr_origins_instr('EDI2_GUN', 'EDI1_DETECTOR')
	gun1_pos_det2_docs = mms_dss_despin(srt, time, gun1_pos_det2_ocs, OMEGA=spin_freq)
	gun2_pos_det1_docs = mms_dss_despin(srt, time, gun2_pos_det1_ocs, OMEGA=spin_freq)

	;Create EDI test data
	data  = mms_edi_test_fa(fields.B_docs, fields.E_docs, gun1_pos_det2_docs, gun2_pos_det1_docs, VIEW=view)
	field = !Null
	
	;Down-sample the data
	gun1_fire_docs  = MrInterpol(data.gun1_fire_docs,  time, edi_time, /SPLINE)
	gun2_fire_docs  = MrInterpol(data.gun2_fire_docs,  time, edi_time, /SPLINE)
	drift_step_docs = MrInterpol(data.drift_step_docs, time, edi_time, /SPLINE)
	if E_sample_rate ne sample_rate $
		then E_docs = MrInterpol(data.E_docs, time, E_time, /SPLINE) $
		else E_docs = data.E_docs
	if B_sample_rate ne sample_rate $
		then B_docs = MrInterpol(data.B_docs, time, B_time, /SPLINE) $
		else B_docs = data.B_docs
	data = !Null

	;Spin up the data
	;   - In a coordinate system spinning ccw with the s/c, the fields spin cw.
	;   - Data will still be in the OCS coordinate system.
	B_ocs         = mms_dss_despin(srt, B_time,   temporary(B_docs),         OMEGA=-spin_freq)
	E_ocs         = mms_dss_despin(srt, E_time,   temporary(E_docs),         OMEGA=-spin_freq)
	gun1_fire_ocs = mms_dss_despin(srt, edi_time, temporary(gun1_fire_docs), OMEGA=-spin_freq)
	gun2_fire_ocs = mms_dss_despin(srt, edi_time, temporary(gun2_fire_docs), OMEGA=-spin_freq)

	;Rotation matrices from OCS to native CS
	dfg2ocs  = mms_instr_xxyz2ocs('DFG_123')
	sdp2ocs  = mms_instr_xxyz2ocs('SDP1')
	edi12ocs = mms_instr_xxyz2ocs('EDI1_GUN')
	edi22ocs = mms_instr_xxyz2ocs('EDI2_GUN')

	;Rotate into native CS
	B_123     = rotate_vector(transpose(dfg2ocs),  temporary(B_ocs))
	E_edp     = rotate_vector(transpose(sdp2ocs),  temporary(E_ocs))
	gun1_fire = rotate_vector(transpose(edi12ocs), temporary(gun1_fire_ocs))
	gun2_fire = rotate_vector(transpose(edi22ocs), temporary(gun2_fire_ocs))
	
	;Convert firing angles to analog voltages
	;   - Convert first to spherical coordinates
	fa1_sphr = cv_coord(FROM_RECT=gun1_fire, /TO_SPHERE)
	fa2_sphr = cv_coord(FROM_RECT=gun2_fire, /TO_SPHERE)
	avolt1   = mms_edi_angle2avoltage(fa1_sphr[[0,1],*])
	avolt2   = mms_edi_angle2avoltage(fa2_sphr[[0,1],*])
	
	;Add noise
	B_123 += randomu(4, size(B_123, /DIMENSIONS)) - 0.5
	E_edp += randomu(7, size(E_edp, /DIMENSIONS)) - 0.5

	;Data in the spinning reference frame system and native coordinate system.
	data = { srt:             srt, $
	         spin_freq:       spin_freq, $
	         B_time:          B_time, $
	         B_123:           B_123, $
	         E_time:          E_time, $
	         E_edp:           E_edp, $
	         edi_time:        edi_time, $
	         edi1_avolt:      avolt1, $
	         edi2_avolt:      avolt2, $
	         edi1_pos_ocs:    gun1_pos_det2_ocs, $
	         edi2_pos_ocs:    gun2_pos_det1_ocs, $
	         drift_step_docs: drift_step_docs $
	       }

	return, data
end
