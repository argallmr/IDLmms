; ***************************************************************************
; MMS FIELDS -- MMSEDI_RunEst()
;
; Determine runner orders (number of gyrations) of EDI beams from measured
; times of flight and gyro-times (from B Magnitude)
;
; Created: June 1, 2015
; Author:  Hans Vaith, University of New Hampshire, Space Science Center
; ***************************************************************************

; ===========================================================================
  FUNCTION MMSEDI_RunEst,                   $
                EDI_EFieldData,             $ ; mandatory input
                B_mag,                      $ ; mandatory input
                B_tt,                       $ ; mandatory input
                sfac = sfac,                $ ; optional
                min_quality = min_quality,  $ ; optional
                verbose = verbose             ; optional
; ===========================================================================
; Parameters
;   EDI_EFieldData  EDI eletric field data as returned by
;                   MMSEDI_Read_L1A_Efield(L1A_EDI_EFIELD_CDF_FILE) or
;                   GetElfData(L0_EDI_EFIELD_FILE)
;
;   B_mag           Time series of magnetic field magnitude values in nT
;
;   B_tt            Time tags associated with magnetic field time series
;                   in nanoseconds since 1958-01-01t00:00:00 UTC
;
; Keywords
;   sfac            Multiplied with the code chip width, sfac determines the
;                   1-sigma width of gaussian probability function that is
;                   used to calculate runner order probabilities. The default
;                   for this number is determined from the distribution of
;                   the deviation between gyro times and times-of-flight.
;   min_quality     Minimum EDI beam quality. Allowed values are 2 and 3.
;                   2 = time-of-flight measured, SN-ratio above low limit
;                   3 = time-of-flight measured, SN-ratio above high limit
;                   The default is 3.
;
; Return
;   The return structure contains these fields
;       status      0 if successful, 1 in case of an error
;       msg         A string indicating the kind of error in case status is 1
;       obs         Observatory identifier ('MMS1', etc)
;       version     RunEst version and date string
;       params      A substructure containing processing parameters
;       data        An array of structures with the actual data. These are
;                   a superset of the data that are being passed into RunEst
;                   via parameter 'EDI_EFieldData' (with the 'tof' field name
;                   changed to 'tof_ori'). The additional structure
;                   fields are:
;                       runorder    runner order (0 if not assigned)
;                       tof         time-of-flight [us]
;                       tChip       code chip width [us]
;                       tCode       code period [us]
;                       prob        probability for each runner order
;                       flag        1 if probability of runner order is above
;                                   threshold, 0 otherwise
;                       estof       equivalent singler runner tof for each
;                                   runner order
;
;                   The difference between tof_ori and tof is that tof is
;                   potentially altered by an integer multiple of code
;                   periods to bring it closer to the gyro time (for the
;                   overwhelming majority of data points there should be
;                   no difference between tof_ori and tof)
; ***************************************************************************

    MMSEDI_RUNEST_VERSION = 'V0.2'
    MMSEDI_RUNEST_DATE    = '2015-09-11'

    ;
    ; Some constants
    ;
    BTG_CONV_1KEV   = 35793.785399d ; convert between Bmag [nT] and gyro time [us]
    BTG_CONV_500EV  = 35758.830466d ; convert between Bmag [nT] and gyro time [us]
    MAXORDER        = 6             ; maximum runner order to be evaluated
    CONF_LIM        = 20.0d         ; confidence threshold
    P_LIM           = exp(-2.0d)    ; p(2*sigma) probability threshold
    MAX_TT_DISTANCE = long64(0.25 * 1.d9) ; 0.25 sec in units nanoseconds

    ;
    ; Evaluate keywords and set defaults where needed
    ;
    if keyword_set(verbose) then verbose = 1 else verbose = 0

    if not keyword_set(min_quality) then begin
        min_quality = 3
        if verbose then message, 'Using default minimum quality = 3', /cont
    endif else begin
        if min_quality ne 2 and min_quality ne 3 then begin
            msg = 'min_quality must be 2 or 3'
            if verbose then message, msg, /cont
            return, { status:1, msg:msg }
        endif
    endelse

    ;
    ; Sanity checks
    ;
    if n_elements(B_tt) ne n_elements(B_mag) then begin
        msg = 'Expecting same number of elements in B_mag and B_tt'
        if verbose then message, msg, /cont
        return, { status:1, msg:msg }
    endif

    ;
    ; Shortcut (electric field mode is also known as
    ; 'windshield wiper' mode, hence 'ww')
    ;
    ww = EDI_EFieldData.data

    ;
    ; Select data with time-of-flight measurement
    ;
    x = where(ww.q ge min_quality and $ ; minimum quality
              ww.ovfl eq 0 and        $ ; no time-of-flight overflow
              (ww.gun_en eq 1 or      $ ; 1keV
               ww.gun_en eq 2),       $ ; 500eV
              N_DATA)

    ;
    ; Check number of data points
    ;
    if N_DATA lt 2 then begin
        msg = 'Less than 2 EDI data points after down selection.'
        if verbose then message, msg, /cont
        return, { status:1, msg:msg }
    endif

    ww = ww[x]

    ;
    ; Check for gaps in magnetic field data: for each EDI data point
    ; calculate which B data point is closest in time
    ;
    tt0      = min(B_tt)
    ww_idx   = interpol( dindgen(n_elements(B_tt)), $
                         B_tt - tt0, ww.tt2000 - tt0 )
    B_idx    = round(ww_idx) ; round to nearest integer index

    delta_tt = abs(ww.tt2000 - B_tt[B_idx])

    x  = where(delta_tt lt MAX_TT_DISTANCE, $
               cnt, complement=x_in_gap, ncomplement=cnt_in_gap)

    if cnt_in_gap gt 0 and verbose then begin
        message, 'Removing EDI data points with no B data close-by: ' + $
            strtrim(cnt_in_gap,2), /cont
    endif

    if cnt lt 2 then begin
        msg = 'Less than 2 EDI data points after eliminating data in B gaps.'
        if verbose then message, msg, /cont
        return, { status:1, msg:msg }
    endif

    ww     = ww[x]
    N_DATA = cnt

    ;
    ; Calculate chip width and code length in micro-seconds
    ;
    tChip = 2.0d^(-23) * ww.cor_m * ww.cor_n * 1.d6
    tCode = tChip * ww.cor_len

    ;
    ; Interpolate B magnitude to EDI beam times
    ;
    bmag_int = interpol( B_mag, B_tt - tt0, ww.tt2000 - tt0 )

    ;
    ; Convert B magnitude to gyro time
    ;
    btg_conv_arr = (ww.gun_en eq 1)*BTG_CONV_1KEV + $
                   (ww.gun_en eq 2)*BTG_CONV_500EV

    tGyro = btg_conv_arr / bmag_int ; in micro-seconds

    ;
    ; Move EDI ToF data into tube +/- 0.5*tCode around gyro time by adding
    ; proper multiple of tCode
    ;
    n_code = round( (tGyro - ww.tof)/tCode )
    rtof   = ww.tof + n_code*tCode

    ;
    ; For better matching tweak gyro time with median of deviation
    ; from times-of-flight
    ;
    meddev = median(rtof - tGyro, /even)
    if verbose then message, 'median dev of rtof from tGyro [us]: ' + $
                        strtrim(meddev,2), /cont
    tGyroCor = tGyro + meddev

    n_code = round( (tGyroCor - ww.tof)/tCode )
    rtof   = ww.tof + n_code*tCode

    ;
    ; Calculate standard deviation of (ToF-tGyro)
    ;
    if not keyword_set(sfac) then begin
        diff     = rtof - tGyro
        xclose   = where(abs(diff) lt 3*stddev(diff))
        sfac     = stddev(diff[xclose]) / mean(tChip[xclose])
        if verbose then $
            message, 'Auto-determined sfac: ' + $
                strtrim(sfac,2), /cont
    endif else begin
        if verbose then $
            message, 'Using provided sfac: ' + $
                strtrim(sfac,2), /cont
    endelse

    ;
    ; Assign order assuming gaussian probability distributions
    ; and use p_max^2/sum(others) as a confidence criterion
    ;
    stdev = sfac * tChip ; this is the standard deviation of the gaussian distributions

    p_run = fltarr(MAXORDER, N_DATA) ; runner order assignment probability - per order
;     p_tot = fltarr(N_DATA)           ; sum of probabilities over all runner orders
;     p_max = p_tot                    ; max probability
;     order = intarr(N_DATA)           ; runner order of max probability

    ;
    ; Calculate probabilities (set to zero where distance is too large)
    ;
    for i=0,MAXORDER-1 do begin

        ; distance between ToF and n*tgyro
        dist  = ( rtof - (i+1)*tGyroCor ) mod tCode ; range [-tCode;+tCode]
        ; normalize to range [-0.5*tCode;+0.5*tCode]
        dist = ((dist + 1.5*tCode) mod tCode) - 0.5*tCode

        p_run[i,*] = (abs(dist) le stdev) * exp(-0.5*(dist/stdev)^2)

    endfor

    p_tot = total(p_run, 1)              ; sum over runner orders
    p_max = max(p_run, dimension=1, idx) ; maximum probability
    idx2d = array_indices(p_run, idx)
    order = fix(reform(idx2d[0,*])+1)    ; first dim is order index (order-1)
                                         ; of run order with max probability

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

    ;
    ; Set up return structure
    ;
    dummy = $
    {   MMS_RUNEST_T, $
        runorder: 0,            $ ; number of gyrations
        tof     : 0.0,          $ ; Time-of-flight [us]
        tChip   : 0.0,          $ ; code chip width [us]
        tCode   : 0.0,          $ ; code period [us]
        tGyro   : 0.0,          $ ; gyro period [us]
        prob    : dblarr(13),   $ ; runner order probability
        flag    : intarr(13),   $ ; 1 if probability is above threshold, 0 otherwise
        estof   : fltarr(13),   $ ; equivalent single runner tof [us]
        ; the items below are carried over from the L1A beam data
        ct      : 0.0d,         $ ; time tag in sec since 1970-01-01T00:00:00 UTC
        tt2000  :long64(0),     $ ; time tag in ns since 1958-01-01T00:00:00 UTC
        gd      : 0,            $ ; gun-detector pair identifier
        vax     : 0,            $ ; analytic voltage (X-direction)
        vay     : 0,            $ ; analytic voltage (Y-direction)
        theta   : 0.0,          $ ; polar angle of firing direction in degrees
        phi     : 0.0,          $ ; azimuth of firing direction in degrees
        tof_ori : 0.0,          $ ; original tof from L1A [us]
        ovfl    : 0,            $ ; tof overflow flag
        gun_en  : 0,            $ ; gun energy flag
        cor_n   : 0,            $ ; code divider 'n'
        cor_m   : 0,            $ ; code divider 'm'
        cor_len : 0,            $ ; number of chips in the code
        q       : 0,            $ ; quality flag
        maxch   : 0,            $ ; correlator max channel address
        data14  : 0,            $ ; correlator data word 14 (signal)
        data15  : uint(0)       $ ; correlator data word 15 (signal+background)
    }

    ret = $
    { $
        status  : 0,    $
        msg     : '',   $
        obs     : '',   $
        version : MMSEDI_RUNEST_VERSION + '   ' + MMSEDI_RUNEST_DATE, $
        params  : { sfac:0.d, p_lim:0.d, conf_lim:0.d, maxorder:0, $
                    meddev:0.0d }, $
        data    : replicate( { MMS_RUNEST_T}, N_DATA ) $
    }


    ret.status   = 0        ; Ok
    ret.msg      = 'OK'
    ret.obs      = 'MMS' + strtrim(EDI_EFieldData.sc_num,2)

    ; save processing configuration parameters
    ret.params.sfac     = sfac
    ret.params.p_lim    = P_LIM
    ret.params.conf_lim = CONF_LIM
    ret.params.maxorder = MAXORDER
    ret.params.meddev   = meddev

    ; data generated by runest
    ret.data.runorder = order
    ret.data.tof      = rtof
    ret.data.tChip    = tChip
    ret.data.tCode    = tCode
    ret.data.tGyro    = tGyro

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

        ret.data.prob[i] = reform(p_run[i,*])
        ret.data.flag[i] = ret.data.prob[i] ge P_LIM

        order = i+1
        k = round( (order*tGyroCor - rtof)/tCode )
        ret.data.estof[i] = (rtof + k*tCode) / order

    endfor

    ;
    ; These are copied over from the EDI input structure
    ;
    ret.data.ct       = ww.ct
    ret.data.tt2000   = ww.tt2000
    ret.data.gd       = ww.gd
    ret.data.vax      = ww.vax
    ret.data.vay      = ww.vay
    ret.data.theta    = ww.theta
    ret.data.phi      = ww.phi
    ret.data.tof_ori  = ww.tof
    ret.data.ovfl     = ww.ovfl
    ret.data.gun_en   = ww.gun_en
    ret.data.cor_n    = ww.cor_n
    ret.data.cor_m    = ww.cor_m
    ret.data.cor_len  = ww.cor_len
    ret.data.q        = ww.q
    ret.data.maxch    = ww.maxch
    ret.data.data14   = ww.data14
    ret.data.data15   = ww.data15

    ;
    ; We are done
    ;
    return, ret

END
