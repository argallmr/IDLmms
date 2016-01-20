; ***************************************************************************
; MMS FIELDS -- mms_edi_runest()
;
; Determine runner orders (number of gyrations) of EDI beams from measured
; times of flight and gyro-times (from B Magnitude)
;
; Created: June 1, 2015
; Author:  Hans Vaith, University of New Hampshire, Space Science Center
;          Removed B_TT input parameter and MIN_QUALITY keyword, as data
;             will be interpolated and filtered outside of this program.
;             Also separated the RET and PARAMS structures from the returned
;             data structure. Renamed from MMSEDI_RunEst to mms_edi_runest
;             to distinguish from original program. - Matthew Argall
; ***************************************************************************

; Parameters
;   edi             EDI eletric field data as returned by
;                   mms_edi_read_l1a_efield
;
;   B_mag           Time series of magnetic field magnitude values in nT
;
;   edi_out         The input EDI structure array with RUNEST results appended:
;                       runorder    runner order (0 if not assigned)
;                       rtof        time-of-flight [us]
;                                   The difference between tof and rtof is that tof is
;                                   potentially altered by an integer multiple of code
;                                   periods to bring it closer to the gyro time (for the
;                                   overwhelming majority of data points there should be
;                                   no difference between tof and rtof)
;                       prob        probability for each runner order
;                       flag        1 if probability of runner order is above
;                                   threshold, 0 otherwise
;                       estof       equivalent singler runner tof for each
;                                   runner order
;                       
;
; Keywords
;   conf_lim        in, optional, type=float, default=20.0D
;                   Confidence limit assuming some gaussian probability
;                       distribution and p_max^2/sum(others) confidence
;                       threshold (p_max is the maximum probability of
;                       all runner orders tested)
;   maxorder        in, optional, type=integer, default=6
;                   Maximum runner order to test.
;   p_lim           in, optional, type=float, default=exp(-2.0D)
;                   Probability limit assuming a gaussian distribution.
;   params          out, optional, type=structure
;                   Analysis parameters. Fields are:
;                       sfac        Same as the input SFAC keyword.
;                       p_lim       Minimum probability threshold limit.
;                       conf_lim    Minimum confidence threshold limit.
;                       maxorder    Maximum runner order tested
;                       meddev      Median deviation of RTOF from TGRYO in EDI_OUT
;   sfac            in, optional, type=float
;                   Multiplied with the code chip width, sfac determines the
;                   1-sigma width of gaussian probability function that is
;                   used to calculate runner order probabilities. The default
;                   for this number is determined from the distribution of
;                   the deviation between gyro times and times-of-flight.
;   verbose         in, optional, type=boolean, default=1
;                   Set to 1 for informational messages to be printed.
;
; Return
;   The return structure contains these fields
;       status      0 if successful, 1 in case of an error
;       msg         A string indicating the kind of error in case status is 1
; ***************************************************************************
function mms_edi_runest, edi, B_mag, $
CONF_LIM = conf_lim, $
MAXORDER = maxorder, $
P_LIM    = p_lim, $
PARAMS   = params, $
SFAC     = sfac,   $
VERBOSE  = verbose
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Defaults
	n_data  = n_elements(edi)
	verbose = keyword_set(verbose)
	if n_elements(CONF_LIM) eq 0 then CONF_LIM = 20.0D
	if n_elements(P_LIM)    eq 0 then P_LIM    = exp(-2.0D)
	if n_elements(MAXORDER) eq 0 then MAXORDER = 6

	;Input verification
	nB = n_elements(B_mag)
	if min(edi.quality)   lt 2  then message, 'Quality must be >= 2.'
	if n_data             ne nB then message, 'Inputs must have the same number of elements.'
	if n_data             lt 2  then message, 'Must provide at least 2 data points.'

;-----------------------------------------------------
; Define Constants \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Gyroperiod
	;   gamma * w = 2 pi / Tg
	;   gamma     = 1 / sqrt( 1 - v^2/c^2)
	;
	;Velocity of the beam: knowing
	;   KE    = m c^2 * (gamma - 1)
	;   E0    = m c^2
	;solve for v
	;
	; To convert nano-Tesla to micro-seconds
	;
	;    nT2us = (2 * pi * gamma * me / q) * 1e6 us/s * 1e9 nT/T
	;
	;    Tg = nT2us / B
	;
	BTG_CONV_1KEV   = 35793.785399d ; convert between Bmag [nT] and gyro time [us]
	BTG_CONV_500EV  = 35758.830466d ; convert between Bmag [nT] and gyro time [us]

;-----------------------------------------------------
; ToF Correction \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;
	; Convert B magnitude to gyro time
	;
	btg_conv_arr = (edi.energy eq 1000US) * BTG_CONV_1KEV + $
	               (edi.energy eq 500US)  * BTG_CONV_500EV

	tGyro = temporary(btg_conv_arr) / B_mag ; in micro-seconds

	;
	; Move EDI ToF data into tube +/- 0.5*tCode around gyro time
	;   - Difference between Tg from B-field and Tg from EDI, rounded
	;     to nearest integer multiple of the code period.
	;        0.0 to 0.4999 ==> 0
	;        0.5 to 1.4999 ==> 1
	;        1.5 to 2.4999 ==> 2
	;   - For single runners, n should be 0.
	;   - Move EDI gyrotime closer to B gyrotime by integer multiples of the code period
	;
	n_code = round( (tGyro - edi.tof)/ edi.tCode )
	rtof   = edi.tof + n_code * edi.tCode

	;
	; For better matching tweak gyro time with median of deviation
	; from times-of-flight
	;
	; Compute the difference between Tg from EDI and FGM
	;   - Adjust EDI Tg by the median difference.
	;
	meddev = median(rtof - tGyro, /EVEN)
	if verbose then MrPrintF, 'logtext', meddev, FORMAT='(%"Median dev of rtof from tGyro [us]: %0.4f.")'
	tGyroCor = tGyro + meddev

	; Move adjusted EDI ToF into tube +/- 0.5*tCode around gyro time
	;   - Same method as above.
	n_code = round( (tGyroCor - edi.tof)/ edi.tCode )
	rtof   = edi.tof + n_code * edi.tCode

	;
	; Calculate standard deviation of (ToF-tGyro)
	;
	if n_elements(sfac) eq 0 then begin
		diff     = rtof - tGyro
		xclose   = where(abs(diff) lt 3*stddev(diff))
		sfac     = stddev(diff[xclose]) / mean(edi[xclose].tChip)
		if verbose then MrPrintF, 'logtext', sfac, FORMAT='(%"Auto-determined sfac: %0.4f")'
	endif else begin
		if verbose then MrPrintF, 'logtext', sfac, FORMAT='(%"User provided sfac: %0.4f")'
	endelse

;-----------------------------------------------------
; Runner Order \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Assign order assuming gaussian probability distributions
	; and use p_max^2/sum(others) as a confidence criterion
	;
	
	; This is the standard deviation of the gaussian distributions
	stdev = sfac * edi.tChip

	;
	; Calculate probabilities (set to zero where distance is too large)
	;
	
	p_run = fltarr(MAXORDER, N_DATA) ; runner order assignment probability - per order
	for i=0,MAXORDER-1 do begin
		; Distance between ToF and n*tgyro
		;   - range [-tCode;+tCode]
		dist  = ( rtof - (i+1)*tGyroCor ) mod edi.tCode
	
		; Normalize to range [-0.5*tCode;+0.5*tCode]
		;   [-1.0, -0.5] --> [ 0.0, -0.5]
		;   [-0.5,  0.0] --> [-0.5,  0.5]
		;   [ 0.0,  0.5] --> [ 0.0,  0.5]
		;   [ 0.5,  1.0] --> [ 0.5,  1.0]
		dist = ((dist + 1.5*edi.tCode) mod edi.tCode) - 0.5*edi.tCode

		;Compute the probability of this being the runner order
		;   - If the distance between ToF and Tg is too large, set
		;     the probability to zero.
		p_run[i,*] = (abs(dist) le stdev) * exp(-0.5*(dist/stdev)^2)
	endfor

	p_tot = total(p_run, 1)              ; Total probability of all runner orders
	p_max = max(p_run, dimension=1, idx) ; maximum probability
	idx2d = array_indices(p_run, idx)
	order = fix(reform(idx2d[0,*])+1)    ; runner order with max probability

;-----------------------------------------------------
; Confidence \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Now calculate a confidence level for the runner order which
	; has the highest probability (some sort of SNR^2)
	; Set runner order to 'undetermined' (0) where the confidence level is
	; below the threshold
	;
	p_other = p_tot - p_max
	p_low   = 1.d-10
	p_other = p_other * (p_other gt p_low) + p_low*(p_other le p_low)
	conf    = p_max^2 / p_other

	;
	; Set runner order to 0 (unknown) where probability is too low
	; or confidence is too low
	;
	order = order * (p_max ge P_LIM and conf ge CONF_LIM)

;-----------------------------------------------------
; Return Data Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Add more information to each beam.
	edi = MrStruct_AddTags( edi, 'runorder', 0S, $
	                             'rtof',     0.0, $
	                             'tGyro',    0.0, $
	                             'prob',     dblarr(MAXORDER), $
	                             'flag',     bytarr(MAXORDER), $
	                             'estof',    fltarr(MAXORDER)  $
	                       )

	;Data generated by runest
	;   - PROB, ESTOF, and FLAG are saved in the next step
	edi.runorder = order
	edi.rtof     = rtof
	edi.tGyro    = tGyro
	edi.prob     = p_run
	edi.flag     = edi.prob ge P_LIM
	
	;
	; Calculate equivalent single runner times of flight
	; Condition for multi-runner tof: rtof + k*tCode ~= order*tGyro
	; This boils down to:
	;   rtof + k*tCode < order*tGyro + 0.5*tCode
	;   rtof + k*tCode > order*tGyro - 0.5*tCode
	; where the left side is the true multi-runner tof
	; Then: single_runner_tof = multi_runner_tof / order
	;
	for i=0,MAXORDER-1 do begin
		order = i+1
		k            = round( (order*tGyroCor - rtof) / edi.tCode )
		edi.estof[i] = (rtof + k*edi.tCode) / order
	endfor

;-----------------------------------------------------
; Save Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Structure of analysis parameters
	params = { sfac:     sfac, $
	           p_lim:    P_LIM, $
	           conf_lim: CONF_LIM, $
	           maxorder: MAXORDER, $
	           meddev:   meddev $
	         }

	;Return status structure
	ret = { status  : 0,   $
	        msg     : 'OK' $
	      }

	; We are done
	return, ret
end
