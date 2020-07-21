; Project: MMS EDI
; Autor: Hans Vaith
; Date: 2019-08-23
;
; Modification History
;   None

; ===========================================================================
  FUNCTION MMSEDI_AmbBurst_DetermineFlips, pitch_gdu1, test=test
; ===========================================================================
; Synopsis
;   Determine flags for rapid look direction changes of the EDI detectors
;   in Ambient Mode from L1A pitch angle data
;
; Parameters
;   pitch_gdu1  in, integer array, required
;       Data from L1A brst CDF variable mmsN_edi_pitch_gdu1
;
; Keywords
;   test        in, 0/1, optional
;       If set the output data structure will have additional tags that 
;       are useful when testing the result against flip flags in the
;       L1A burst data files
;
; Return
;   If pitch_gdu1 has only one element, changes cannot be determined.
;   A value of !Null will be returned in that case.
;   Otherwise a structure with the following tags will be returned (each
;   tag is an array with the same number of elements as the input variable
;   pitch_gdu1):
;
;       FLIP_0_180      flips between pitch angles 0 and 180
;       FLIP_FA_90      flips between pitch angles 90 and 0/180
;       COMBINED_FLIP   FLIP_0_180 in bit 0, FLIP_FA_90 in bit 1
;       TEST_FLIP       OR of FLIP_0_180 and FLIP_FA_90 (only in case
;                       keyword /test is set). This data can be used
;                       for comparison with the L1A flip flag variable
;
;   The data held in structure tag COMBINED_FLIP can be used
;   for the flip flag variable in the L2 brst file.
; ***************************************************************************

    NN = N_Elements(pitch_gdu1)

    if NN eq 0 then begin
        ; This is an error
        Print, 'N_Elements(pitch_gdu1) is zero'
        Stop
        Retall
    endif

    if NN eq 1 then begin
        return, !Null
    endif

    ; Coming from the CDF files, the pitch angle flag (pitch_gdu1) is
    ; of type CDF_UINT1 which translates into an IDL byte (unsigned).
    ; Since we want to calculate differences we propagate this variable
    ; to a signed data type
    pitch_int = fix(pitch_gdu1)

    abs_delta_pa = Abs(pitch_int[1:-1] - pitch_int[0:-2])

    ; Determine FA/90 flips and 0/180 flips
    x_fa_90 = Where(abs_delta_pa eq 90, /Null)
    x_0_180 = Where(abs_delta_pa eq 180, /Null)

    ; Adjust the indices as we want to flag the first sample AFTER the look
    ; direction switch
    if x_fa_90 ne !Null then x_fa_90 += 1
    if x_0_180 ne !Null then x_0_180 += 1

    ; The very first pitch angle sample could theoretically be a sample right
    ; after a flip, so we would want to flag it. Using pitch angle deltas does
    ; not help us with that. For 90/FA flips which occur on a strictly regular
    ; basis this is possible by looking at the spacing between these flips and
    ; determining of the very first sample should be flagged as well.
    if N_Elements(x_fa_90) ge 2 then begin
        flip_spacing = x_fa_90[1] - x_fa_90[0]
        if x_fa_90[0] eq flip_spacing then x_fa_90 = [ 0, x_fa_90 ]
    endif

    ; Now place the flags into arrays of the same size as the pitch angle
    ; variable:
    ;   - For FA/90 flips we want to flag three consecutive samples to take
    ;     the HV settling times of the optics into account
    ;   - For 0/180 flips we want to flag two consecutive samples
    ; To simplify this we add two elements to avoid indexing beyond the
    ; end of the array. The elements are removed afterwards.
    flip_fa_90 = intarr(NN+2)
    flip_0_180 = intarr(NN+2)

    ; 3 consecutive flags for FA/90 flips
    if x_fa_90 ne !Null then begin
        flip_fa_90[x_fa_90]   = 1
        flip_fa_90[x_fa_90+1] = 1
        flip_fa_90[x_fa_90+2] = 1
    endif

    ; 2 consecutive flags for 0/180 flips
    if x_0_180 ne !Null then begin
        flip_0_180[x_0_180]   = 1
        flip_0_180[x_0_180+1] = 1
    endif

    ; For the test flag we mimick what is in the flip variable
    ; in the L1A files: 2 consecutive flags regardless of the
    ; kind of flip
    if Keyword_Set(test) then begin
        flip_fa_90_test = intarr(NN+2)
        if x_fa_90 ne !Null then begin
            flip_fa_90_test[x_fa_90]   = 1
            flip_fa_90_test[x_fa_90+1] = 1
        endif
    endif

    ; Strip off extra elements at the end
    flip_fa_90 = flip_fa_90[0:-3]
    flip_0_180 = flip_0_180[0:-3]

    ; For the L2 telemetry we want to combine the flags into
    ; a single variable
    ; Bit 0: 0/180 flips   Bit 1: FA/90 flips
    combined_flip = flip_0_180 or 2*flip_fa_90

    if Keyword_Set(test) then begin
        return, $
        { $
            flip_0_180:flip_0_180, $
            flip_fa_90:flip_fa_90, $
            combined_flip: combined_flip, $
            test_flip: flip_0_180 or flip_fa_90_test $
        }
    endif else begin
        return, $
        { $
            flip_0_180:flip_0_180, $
            flip_fa_90:flip_fa_90, $
            combined_flip: combined_flip $
        }
    endelse

END
