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
;-
;*****************************************************************************************
;+
;   Create time tags for a data set. All data must be in the despun-ocs coordinate system.
;
; :Params:
;       B_MAG:          in, required, type=float, default=40.0
;                       Magnitude of the magnetic field to create, in nano-Tesla
;       E_MAG:          in, required, type=float, default=0.5
;                       Magnitude of the electric field to create, in milli-Volts/meter
;       GUN1_POS:       in, required, type=3xN float
;                       Position of Gun1 with respect to Detector2, in millimeters.
;       GUN2_POS:       in, required, type=3xN float
;                       Position of Gun2 with respect to Detector1, in millimeters.
;
; :KEYWORDS:
;       VERBOSE:        in, optional, type=boolean, default=0
;                       Print test values.
;-
function mms_edi_test_fa, B, d, gun1_pos_docs, gun2_pos_docs, $
VIEW=view, $
VERBOSE=verbose
	compile_opt idl2
	on_error, 2

	;Default magnitudes
	verbose = keyword_set(verbose)
	view    = keyword_set(view)
	
;---------------------------------------------------------------------
; Rotate into BPP ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Determine the B-perp plane
	xyz2bpp = mms_instr_xb2bpp(B_docs)
	
	;Magnetic Field
	;   - Should be (0, 0, |B|)
	B_BPP = rotate_vector(xyz2bpp, B_docs)

	;Electric field.
	;   - Rotate into BPP
	;   - Make it (Ex, Ey, 0), perpendicular to B
	;   - Transform back to OCS
	;   - Rescale
	E_BPP    = rotate_vector(xyz2bpp, E_docs)
	E_BPP[2] = 0
	E_docs   = rotate_vector(transpose(xyz2bpp), E_BPP)
	E_docs   = normalize(E_docs) * E_mag
	E_BPP    = rotate_vector(xyz2bpp, E_docs)
	
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
	drift_vel_bpp  = 1e6 * cross_product(E_BPP, B_BPP) / Bmag^2
	drift_step_bpp =       drift_vel_bpp * gyro_period


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
	d_to_g1_bpp      = fltarr(size(gun1_pos_bpp, /DIMENSIONS))
	d_to_g2_bpp      = fltarr(size(gun1_pos_bpp, /DIMENSIONS))
	d_to_g1_bpp[0,*] =   drift_step_bpp[0] - gun1_pos_bpp[0,*]
	d_to_g1_bpp[1,*] =   drift_step_bpp[1] - gun1_pos_bpp[1,*]
	d_to_g1_bpp[2,*] =   drift_step_bpp[2] - gun1_pos_bpp[2,*]
	d_to_g2_bpp[0,*] = -(drift_step_bpp[0] - gun2_pos_bpp[0,*])
	d_to_g2_bpp[1,*] = -(drift_step_bpp[1] - gun2_pos_bpp[1,*])
	d_to_g2_bpp[2,*] = -(drift_step_bpp[2] - gun2_pos_bpp[2,*])

;---------------------------------------------------------------------
; Rotate Back to DOCS ////////////////////////////////////////////////
;---------------------------------------------------------------------
	bpp2xyz         = transpose(xyz2bpp)
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

	;Show results
	if verbose then begin
		print, FORMAT='(%"  B_DOCS = [%8.2f, %8.2f, %8.2f]")', B_docs
		print, FORMAT='(%"  E_DOCS = [%8.2f, %8.2f, %8.2f]")', E_docs
		print, FORMAT='(%"  d_DOCS = [%8.2f, %8.2f, %8.2f]")', drift_step_docs
	endif

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
function mms_edi_test_times, tstart, duration, rate
	compile_opt idl2
	on_error, 2

	;Turn the start time into a TT2000 time
	t_tt2000 = cdf_parse_tt2000(tstart)

	;Number of data points to create
	nSamples  = duration * rate

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
function mms_edi_test_value, magnitude, $
SCALAR=scalar, $
SEED=seed
	compile_opt idl2
	on_error, 2

	;Defaults
	scalar = keyword_set(scalar)
	if n_elements(magnitude) eq 0 then magnitude = 1.0
	if n_elements(seed)      eq 0 then seed      = 1
	if n_elements(spin_freq) eq 0 then spin_freq = 0.0

	;Create the data
	if scalar then begin
		value = (randomu(seed, 1))[0] - 0.5
	endif else begin
		value  = randomu(seed, 3) - 0.5
		value /= sqrt(total(value^2))
	endelse

	;Scale the data
	value *= magnitude

	return, value
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
function mms_edi_test_signal, sample_rate, duration, $
AMPLITUDES=amplitudes, $
FREQUENCIES=frequencies, $
MAGNITUDE=magnitude, $
NOISE_LEVEL=noise_level, $
NOISE_SEED=noise_seed, $
PHASES=phases, $
SCALAR=scalar, $
SLOPE=slope, $
SEED=seed, $
T0=t0, $
TIME=time
	compile_opt idl2
	on_error, 2

	;Defaults
	scalar = keyword_set(scalar)
	if n_elements(magnitude) eq 0 then magnitude = 0.0
	if n_elements(slope)     eq 0 then slope     = 0.0

	;Start by creating the C
	signal = mrsiggen(sample_rate, duration, $
	                  AMP_A       = amplitudes, $
	                  DC          = magnitude, $
	                  NOISE_LEVEL = noise_level, $
	                  NOISE_SEED  = noise_seed, $
	                  PHASE_A     = phases, $
	                  SEED        = seed, $
	                  TIME        = time)
	
	;Create the Y and Z components of the vector
	if scalar eq 0 then begin
		nPts   = n_elements(time)
		signal = rebin(reform(signal, 1, nPts), 3, nPts)
		
		;Y-component
		signal[1,*] = mrsiggen(sample_rate, duration, $
		                       AMP_A       = amplitudes, $
		                       DC          = magnitude, $
		                       NOISE_LEVEL = noise_level, $
		                       NOISE_SEED  = noise_seed, $
		                       PHASE_A     = phases, $
		                       SEED        = seed, $
		                       TIME        = time)
		
		;Z-component
		signal[2,*] = mrsiggen(sample_rate, duration, $
		                       AMP_A       = amplitudes, $
		                       DC          = magnitude, $
		                       NOISE_LEVEL = noise_level, $
		                       NOISE_SEED  = noise_seed, $
		                       PHASE_A     = phases, $
		                       SEED        = seed, $
		                       TIME        = time)
	endif

	;Add a slope
	if slope ne 0 then begin
		y = slope * (findgen(npts) - npts/2)
		signal += y
	endif
	
	;Convert time to CDF_TIME_TT2000 and add T0
	time = long64(time * 1d9)
	if n_elements(t0) gt 0 then time += cdf_parse_tt2000(t0)

	return, value
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
	
	;Rotate the s/c into bpp
	sc_xyz_bpp  = rotate_vector(xyz2bpp, sc_xyz_ocs)

;-----------------------------------------------------
; Draw S/C \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Create a square window
	dims   = get_screen_size()
	aspect = dims[0] / dims[1]
	winx   = 500
	winy   = fix(winx / aspect)
	win    = window(DIMENSIONS=[winx, winy])
	win   -> Refresh, /DISABLE
	
	;Draw a set of axes 1.5 times bigger than the s/c, then draw the s/c
	range = 1.5 * [-sc_sphr_ocs[2,0], sc_sphr_ocs[2,0]]
	gAxes = plot(range, range, /NODATA, /CURRENT, XRANGE=range, XSTYLE=1, YRANGE=range, YSTYLE=1)
	gSC   = polygon(reform(sc_xyz_bpp[0,*]), reform(sc_xyz_bpp[1,*]), /DATA, TARGET=gAxes)
	
	;Firing directions
	gFire1 = Polyline(range, range, COLOR='Blue', /DATA, TARGET=gAxes)
	gFire2 = Polyline(range, range, COLOR='Red',  /DATA, TARGET=gAxes)
	
	;Drift Step
	gDriftStep = Symbol(D_BPP[0], D_BPP[1], 'Star', /DATA, TARGET=gAxes)
	win -> Refresh
	win -> Refresh, /DISABLE

;-----------------------------------------------------
; Draw Each Test Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	nPts   = n_elements(gun1_pos_bpp[0,*])
	iStart = 0
	nStep  = 10
	iStop  = iStart + nStep - 1
	while iStart lt nPts do begin
		g1_pos  = gun1_pos_bpp[*,iStart:iStop]
		g2_pos  = gun2_pos_bpp[*,iStart:iStop]
		g1_fire = gun1_fire_bpp[*,iStart:iStop]
		g2_fire = gun2_fire_bpp[*,iStart:iStop]

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
		;Update the gun positions
		gGuns = symbol(reform([[g1_pos[0,*]], [g2_pos[0,*]]]), $
		               reform([[g1_pos[1,*]], [g2_pos[1,*]]]), 'circle', $
		               /DATA, TARGET=gAxes)

		;Draw the beams
		gFire1 -> SetData, g1x, g1y, CONNECTIVITY=conn1
		gFire2 -> SetData, g2x, g2y, CONNECTIVITY=conn2
		
	;-----------------------------------------------------
	; Next Iteration\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		win -> Refresh
	
		;Let me see what is happening
		wait, 0.5
		win -> Refresh, /DISABLE
		
		;Delete the guns
		gGuns -> Delete
		
		;Advance
		iStart = iStop + 1
		iStop  = (iStop + nStep) < nPts
	endwhile

	return, win
end


;+
;   Create test data.
;-
function mms_edi_test_data, $
VIEW=view
	compile_opt idl2
	on_error, 2
	
	;Experimental parameters
	;   - Firing angles derived for 
	;       B = ( 0,  0, Bz)
	;       E = (Ex,  0,  0)
    ;   - Spin is right-handed (counter-clockwise) about the z-axis.
	;   - Instrument CS are aligned with the s/c CS, except for a single rotation about z by OFFSET.
	tstart          = '2015-03-12T00:00:00.000000000'
	duration        = 1.0D * 60.0D                              ;minutes * seconds/minute
	B_sample_rate   = 64.0                                      ;Samples / second
	E_sample_rate   = 64.0                                      ;Samples / second
	edi_sample_rate = 16.0                                      ;Samples / second
	spin_rate       = 3.0 / 60.0                                ;# rev / sec
	spin_freq       = 2.0 * !pi * spin_rate                     ;rad / sec

	;Create times in tt2000 format
	srt      = mms_edi_test_times(tstart, duration, spin_rate)
	B_time   = mms_edi_test_times(tstart, duration, b_sample_rate)
	E_time   = mms_edi_test_times(tstart, duration, e_sample_rate)
	edi_time = mms_edi_test_times(tstart, duration, edi_sample_rate)

	;Create magnetic field
	seed   = 4
	B_docs      = fltarr(3, B_sample_rate * duration)
	B_docs[0,*] = mms_edi_test_signal(B_sample_rate, duration, $
	                                  AMPLITUDES  = 23.0, $
	                                  FREQUENCIES =  0.0)
	B_docs[1,*] = mms_edi_test_signal(B_sample_rate, duration, $
	                                  AMPLITUDES  = -8.0, $
	                                  FREQUENCIES =  0.0)
	B_docs[2,*] = mms_edi_test_signal(B_sample_rate, duration, $
	                                  AMPLITUDES  = -11.0, $
	                                  FREQUENCIES =   0.0, $
	                                  T0=tstart, $
	                                  TIME=time)
	
	;Create the drift step
	d_docs = mms_edi_test_signal(B_sample_rate, duration, $
	                             AMPLITUDES  =  1.5, $
	                             FREQUENCIES =  0.0)
	
	;Create SC positions in despun-ocs
	;   - For EDI triangulation method, the detectors are at the center of the s/c
	;     and the guns are on the fringes.
	;   - Required to calculate firing directions in BPP
	gun1_pos_det2_ocs  = mms_instr_origins_instr('EDI1_GUN', 'EDI2_DETECTOR')
	gun2_pos_det1_ocs  = mms_instr_origins_instr('EDI2_GUN', 'EDI1_DETECTOR')
	gun1_pos_det2_docs = mms_dss_despin(srt, edi_time, gun1_pos_det2_ocs, OMEGA=spin_freq)
	gun2_pos_det1_docs = mms_dss_despin(srt, edi_time, gun2_pos_det1_ocs, OMEGA=spin_freq)

	;Create EDI test data
	data = mms_edi_test_fa(B_docs, d_docs, gun1_pos_det2_docs, gun2_pos_det1_docs, /VERBOSE, VIEW=view)

	;Spin up the data
	;   - In a coordinate system spinning ccw with the s/c, the fields spin cw.
	;   - Fields need to be offset from DSS toward their native CS.
	;   - Data will still be in the OCS coordinate system.
	B_ocs         = mms_dss_despin(srt, B_time,   data.B_docs,         OMEGA=-spin_freq)
	E_ocs         = mms_dss_despin(srt, E_time,   data.E_docs,         OMEGA=-spin_freq)
	gun1_fire_ocs = mms_dss_despin(srt, edi_time, data.gun1_fire_docs, OMEGA=-spin_freq)
	gun2_fire_ocs = mms_dss_despin(srt, edi_time, data.gun2_fire_docs, OMEGA=-spin_freq)

	;Rotation matrices from OCS to native CS
	dfg2ocs  = mms_instr_xxyz2ocs('DFG_123')
	sdp2ocs  = mms_instr_xxyz2ocs('SDP1')
	edi12ocs = mms_instr_xxyz2ocs('EDI1_GUN')
	edi22ocs = mms_instr_xxyz2ocs('EDI2_GUN')

	;Rotate into native CS
	B_123     = rotate_vector(transpose(dfg2ocs),  B_ocs)
	E_edp     = rotate_vector(transpose(sdp2ocs),  E_ocs)
	gun1_fire = rotate_vector(transpose(edi12ocs), gun1_fire_ocs)
	gun2_fire = rotate_vector(transpose(edi22ocs), gun2_fire_ocs)
	
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
	         drift_step_docs: data.drift_step_docs $
	       }

	return, data
end
