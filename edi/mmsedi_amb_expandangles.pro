; ===========================================================================
  FUNCTION MMSEDI_AMB_ExpandAngles, $
        pitch_gdu1, $
        phi, $
        theta, $
        msg=msg, $
        quiet=quiet
; ===========================================================================
; Synopsis
;   This routine expands EDI ambient mode look directions (phi,theta) from 
;   L1A burst files to the time resolution of the raw counts data in a way
;   that ensures consistency between pitch angle flags and look directions.
;
; Additional information
;
;
; Parameters
;   pitch_gdu1  IN, integer array, mandatory
;       Data from L1A brst CDF variable mmsN_edi_pitch_gdu1
;       The number of elements in this array must be a multiple of eight
;
;   phi         IN, float array, mandatory
;       Data from L1A brst CDF variable mmsS_edi_amb_phi
;       The number of elements in this array must be one eighth of the 
;       number of elements in pitch_gdu1

;   theta       IN, float array, mandatory
;       Data from L1A brst CDF variable mmsS_edi_amb_theta
;       The number of elements in this array must be one eighth of the 
;       number of elements in pitch_gdu1
;
; Keywords
;   msg         optional named variable
;       Will contain a string with an error message in case this routine
;       returns a value of !Null
;
;   quiet       IN, 0/1, optional
;       If set, no error messages will be printed to the screen
;
; Return
;   In case of an error the value !Null is returned and msg will be set to
;   a string containing details on the error.
;   In case of no error, an array of structures with tags 'phi' and 'theta'
;   will be returned. The number of elements in the array will match the 
;   number of elements in the input parameter 'pitch_gdu1'
; 
; ***************************************************************************

    quiet = Keyword_Set(quiet)
    msg   = ''

    MULTIPLIER = 8L

    ;
    ; Run a number of sanity checks
    ;
    if N_Elements(phi) ne N_Elements(theta) then begin
        msg = 'Number of elements must be the same for phi and theta'
        goto, error
    endif

    if N_Elements(pitch_gdu1) ne MULTIPLIER * N_Elements(phi) then begin
        msg = 'pitch_gdu1 must have eight times the number of elements in phi'
        goto, error
    endif

    if N_Elements(pitch_gdu1) eq 0 then begin
        msg = 'N_Elements(pitch_gdu1) is zero'
        goto, error
    endif

    ; Coming from the CDF files, the pitch angle flag (pitch_gdu1) is
    ; of type CDF_UINT1 which translates into an IDL byte (unsigned).
    ; Since we want to do differences below we propagate this variable
    ; to a signed data type
    pitch_signed = fix(pitch_gdu1)

    N_PHI   = N_Elements(phi)
    N_PITCH = N_Elements(pitch_signed)

    proto = { phi:0.0, theta:0.0 }
    out_data = replicate( proto, N_PITCH )

    x0 = lindgen(N_PHI) * MULTIPLIER

    for i=0L,MULTIPLIER-1 do begin
        x = x0 + i
        if i eq 0 then begin
            ; The first out of eight elements is always assumed to be a match
            ; between reported pitch angle flag and look direction
            out_data[x].phi   = phi
            out_data[x].theta = theta
        endif else begin
            ; For the other seven elements detect "midstream" pitch angle flag
            ; changes. If there is such a change we need to modify the look angle
            ; instead of just propagating it.
            ; Note that pitch angle flag changes are only expected on even-numbered
            ; indices i (but we don't check for that here). This is because the pitch
            ; angle flags are themselves data that have been propagated to the cadence
            ; of the counts data.
            delta_pitch = pitch_signed[x] - pitch_signed[previous_x]
            y_same = Where( delta_pitch eq 0, complement=y_change, /Null)

            ; Just copy over all angle settings at indices where the 
            ; pitch angle flag has not changed
            out_data[x[y_same]] = out_data[previous_x[y_same]]

            if y_change ne !Null then begin

                ; Change azimuth angle by 180 degrees and renormalize 
                ; to range [-180, 180]
;                 out_data[x[y_change]].phi   = ((phi[y_change] + 360) mod 360) - 180
;                 out_data[x[y_change]].theta = theta[y_change]
                idx_arr = x[y_change]
                out_data[idx_arr].phi   = ((out_data[idx_arr-1].phi + 360) mod 360) - 180
                out_data[idx_arr].theta = out_data[idx_arr-1].theta

                ; Sanity check
                tmp = Where(delta_pitch[y_change] ne 180 and $
                            delta_pitch[y_change] ne -180, cnt)
                if cnt gt 0 then begin
                    Message, 'Encountered midstream pitch angle change that is different from 180!', /cont
                    Stop
                    Retall
                endif
            endif

        endelse
        previous_x = x ; save
    endfor

    return, out_data

error:
    if not quiet then Message, msg, /cont
    return, !Null

END

