; ***************************************************************************
; MMS FIELDS -- MMSEDI_Read_L1A_Efield()
;
; Read EDI electric field data from Level 1A CDF files
;
; Created: September 4, 2015
; Author:  Hans Vaith, University of New Hampshire, Space Science Center
; ***************************************************************************


; ===========================================================================
  FUNCTION MMSEDI_Read_L1A_Efield, filename, ctr=ctr, verbose=verbose
; ===========================================================================
; Parameters
;   filename    fully qualified file name of an MMS EDI Level 1A Survey 
;               or Burst EField CDF file.
; 
; Keywords
;   ctr         dblarr(2): time tag range in seconds since 
;               1958-01-01t00:00:00 UTC
;   verbose     print out some informational messages
;
; Return
;   EDI electric field mode data structure array, as defined in 
;   GetElfStruct()
; ***************************************************************************

    ;
    ; Evaluate keywords
    ;
    if keyword_set(verbose) then verbose = 1 else verbose = 0

    ;
    ; Time tag variable names are special in that they don't use the
    ; "mmsN_edi_" prefix (N = observatory number)
    ;
    tt_gd12_name = 'Epoch_beam_gd12'
    tt_gd21_name = 'Epoch_beam_gd21'

    ;
    ; Make sure the file exists
    ;
    if not File_Test(filename) then begin
        msg = filename + 'does not exist.'
        if verbose then message, msg, /cont
        return, { status:1, msg:msg }
    endif

    ;
    ; Open the file and get basic information
    ;
    cdf_id   = CDF_Open(filename)
    cdf_info = CDF_Inquire(cdf_id)

    ;
    ; We do not expect to have any rVariables
    ;
    if cdf_info.nvars gt 0 then begin
        msg = 'Non-zero rVariable count. This is unexpected!'
        if verbose then message, msg, /cont
        return, { status:1, msg:msg }
    endif

    ;
    ; Read information about all zVariables
    ;
    first_var = CDF_VarInq(cdf_id, 0, /zVariable)
    var_info  = replicate(first_var, cdf_info.nzvars)

    for var_id = 1L,cdf_info.nzvars - 1 do begin
        var_info[var_id] = CDF_VarInq(cdf_id, var_id, /zVariable)
    endfor

    ;
    ; Determine variable name prefix from 'Source_name' global attribute
    ;
    att_found = 0
    for att_id=0L,cdf_info.natts-1 do begin
        CDF_AttInq, cdf_id, att_id, att_name, scope, max_entry, max_zentry
        if att_name eq 'Source_name' then begin
            CDF_Control, cdf_id, attribute=att_name, get_attr_info=att_info
            CDF_AttGet, cdf_id, att_id, att_info.maxgentry, att_value
            var_name_prefix = strlowcase(strmid(att_value, 0, 4)) + '_edi_'
            sc_num = fix(strmid(att_value, 3, 1))
            att_found = 1
            break
        endif
    endfor

    if not att_found then begin
        msg = "Did not find 'Source_name' global attribute"
        message, msg, /cont
        return, { status:1, msg:msg }
    endif

    ;
    ; Determine number of records from time tag
    ;
    CDF_Control, cdf_id, Variable=tt_gd12_name, Get_Var_Info=tt12_info
    N_RECS = tt12_info.maxrec

    ;
    ; Set up data structure
    ;
    elf_data = replicate( MMSEDI_GetElfStruct(/cnv), 2*N_RECS )
    x12      = lindgen(N_RECS)
    x21      = x12 + N_RECS

    ;
    ; Define names of all independent variables (without prefix)
    ;
    time_tag_list = [ tt_gd12_name, tt_gd21_name ]

    var_name_list = $
    [ $
        'tof2_us',       'max_addr_gd12',   $
        'vax_gd12',      'vay_gd12',        $
        'theta_gd12',    'phi_gd12',        $
        'word14_gd12',   'word15_gd12',     $
        'numchips_gd12', 'sq_gd12',         $
        'm_gd12',        'n_gd12',          $
        'e_gd12',                           $
        $
        'tof1_us',       'max_addr_gd21',   $
        'vax_gd21',      'vay_gd21',        $
        'theta_gd21',    'phi_gd21',        $
        'word14_gd21',   'word15_gd21',     $
        'numchips_gd21', 'sq_gd21',         $
        'm_gd21',        'n_gd21',          $
        'e_gd21'                            $
    ]

    all_list          = [ time_tag_list, var_name_list ]
    all_prefixed_list = [ time_tag_list, var_name_prefix+var_name_list ]

    ;
    ; Read the data for all variables in the list
    ;
    for i=0L,n_elements(all_list)-1 do $
    begin
        var_name = all_prefixed_list[i]

        ;
        ; Make sure the actual number of elements is what we expect
        ;
        CDF_Control, cdf_id, Variable=var_name, Get_Var_Info=var_info
        if var_info.maxrec ne N_RECS then $
        begin
            msg = 'Unexpected number of elements'
            message, msg, /cont
            return, { status:1, msg:msg }
        endif

        ;
        ; Read the data and populate the data structure array
        ;
        CDF_VarGet, cdf_id, var_name, values, Rec_Count=N_RECS

        case all_list[i] of
            'Epoch_beam_gd12':elf_data[x12].tt2000   = reform(values)
            'tof2_us'       : elf_data[x12].tof      = reform(values)
            'max_addr_gd12' : elf_data[x12].maxch    = reform(values)
            'vax_gd12'      : elf_data[x12].vax      = reform(values)
            'vay_gd12'      : elf_data[x12].vay      = reform(values)
            'theta_gd12'    : elf_data[x12].theta    = reform(values)
            'phi_gd12'      : elf_data[x12].phi      = reform(values)
            'word14_gd12'   : elf_data[x12].data14   = reform(values)
            'word15_gd12'   : elf_data[x12].data15   = reform(values)
            'numchips_gd12' : elf_data[x12].cor_len  = reform(values)
            'sq_gd12'       : elf_data[x12].q        = reform(values)
            'n_gd12'        : elf_data[x12].cor_n    = reform(values)
            'm_gd12'        : elf_data[x12].cor_m    = reform(values)
            'e_gd12'        : begin
                ; convert to energy encoding used by EDI FSW
                values = reform(values)
                elf_data[x12].gun_en =  (values eq 0)    * 0  + $
                                        (values eq 250)  * 3  + $
                                        (values eq 500)  * 2  + $
                                        (values eq 1000) * 1
            end
            'Epoch_beam_gd21':elf_data[x21].tt2000   = reform(values)
            'tof1_us'       : elf_data[x21].tof      = reform(values)
            'max_addr_gd21' : elf_data[x21].maxch    = reform(values)
            'vax_gd21'      : elf_data[x21].vax      = reform(values)
            'vay_gd21'      : elf_data[x21].vay      = reform(values)
            'theta_gd21'    : elf_data[x21].theta    = reform(values)
            'phi_gd21'      : elf_data[x21].phi      = reform(values)
            'word14_gd21'   : elf_data[x21].data14   = reform(values)
            'word15_gd21'   : elf_data[x21].data15   = reform(values)
            'numchips_gd21' : elf_data[x21].cor_len  = reform(values)
            'sq_gd21'       : elf_data[x21].q        = reform(values)
            'n_gd21'        : elf_data[x21].cor_n    = reform(values)
            'm_gd21'        : elf_data[x21].cor_m    = reform(values)
            'e_gd21'        : begin
                ; convert to energy encoding used by EDI FSW
                values = reform(values)
                elf_data[x21].gun_en =  (values eq 0)    * 0  + $
                                        (values eq 250)  * 3  + $
                                        (values eq 500)  * 2  + $
                                        (values eq 1000) * 1
            end
        endcase

    endfor

    CDF_close, cdf_id

    ;
    ; Set gun-detector identifiers
    ;
    elf_data[x12].gd = 12
    elf_data[x21].gd = 21

    ;
    ; Calculate alternate 'ct' time tags (seconds since 1970-01-01T00:00:00 UTC)
    ;
    mct_epoch   = cdf_parse_tt2000('1958-01-01T00:00:00.000000000')
    elf_data.ct = (elf_data.tt2000 - mct_epoch) / 1.d9

    ;
    ; Set time-of-flight overflow flag from CDF fill values
    ;
    elf_data.ovfl = 0
    fill_value_plus_eps = MMSEDI_GetCdfFillValue(/epsilon)
    x = where(elf_data.tof lt fill_value_plus_eps, cnt)
    if cnt gt 0 then elf_data[x].ovfl = fill_value

    ;
    ; Sort the data by beam time
    ;
    elf_data = elf_data[ sort(elf_data.tt2000) ]

    ;
    ; Narrow down time range as requested
    ;
    if keyword_set(ctr) then begin

        x = where(elf_data.ct ge ctr[0] and $
                  elf_data.ct le ctr[1], $
                  cnt)

        if cnt eq 0 then begin
            msg = 'no data left after constraining to specified time range'
            if verbose then message, msg, /cont
            return, { status:1, msg:msg }
        endif else begin
            elf_data = elf_data[x]
        endelse

    endif


    return, $
    { $
        status  : 0,    $
        msg     : '',   $
        sc_num  : sc_num, $
        data    : elf_data $
    }

END
