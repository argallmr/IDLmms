; ***************************************************************************
; MMS FIELDS -- MMSEDI_BeamWidth: calculate the electron beam width in
;               the plane perpendicular to the magnetic field, assuming
;               an elliptically shaped beam profile with either predefined
;               polar angle dependence of the semi-axes or setable semi-axes
;               values via keywords.
;
; Created: 06/24/15 by Hans Vaith
; ***************************************************************************

; ===========================================================================
  FUNCTION MMSEDI_BeamWidth, gun_phi_deg, gun_theta_deg, $
                             bx, by, bz, $
                             gun_id, $
                             dph_model=dph_model, $
                             dth_model=dth_model
; ===========================================================================
; Parameters
;
;   gun_phi_deg, gun_theta_deg
;       Array of gun firing azimuth angles (in degrees) in GDU coodinates
;       (GDU1 coordinates for gun1 firing directions, GDU2 coordinates for
;        gun2 firing directions)
;
;   bx, by, bz
;       Magnetic field vector cartesian components in GDU1 coordinates
;       (units may be arbitrary, because only the direction is needed)
;       The magnetic field data need to have the same number of elements as
;       the gun firing directions (that is, it is required that the magnetic
;       field data have been interpolated to the times associated with the
;       EDI firing directions)
;
;   gun_id
;       Integer array of gun identifiers (1 or 2) with the same number of
;       elements as the firing angles and magnetic field vector components.
;       If all firing directions are from a single gun, this argument is
;       permitted to be passed as a single integer rather than as an array.
;
;   dph_model, dth_model
;       Optional keywords to allow setting the semi-axes of the beam profile
;       ellipse
;
; Return
;   Angular beam width in the plane perpendicular to B in units of radians
; ***************************************************************************

    ;
    ; Make sure all arrays have the same dimension
    ;
    NN = n_elements(gun_phi_deg)

    if n_elements(gun_theta_deg) ne NN or $
       n_elements(bx) ne NN or $
       n_elements(by) ne NN or $
       n_elements(bz) ne NN then begin
        message, 'Need to have same number of elements in the data passed.', /cont
        help, gun_phi_deg, gun_theta_deg, bx, by, bz
        retall
    endif

    N_GUN = n_elements(gun_id)

    if N_GUN eq 1 then begin
        gun = intarr(NN) + gun_id
    endif else if N_GUN ne NN then begin
        message, 'bad number of elements in parameter <gun_id>', /cont
        help, NN, N_GUN
        retall
    endif else begin
        gun = gun_id
    endelse

    ;
    ; Sanity check
    ;
    dummy = where(gun ne 1 and gun ne 2, cnt)
    if cnt gt 0 then begin
        message, 'Invalid gun_id values!', /cont
        message, 'Stopping for inspection', /cont
        stop
        retall
    endif

    ;
    ; Calculate cartesian components of gun firing directions
    ;
    ph_rad = gun_phi_deg   * !dtor
    th_rad = gun_theta_deg * !dtor

    cos_th = cos(th_rad)
    sin_th = sin(th_rad)

    cos_ph = cos(ph_rad)
    sin_ph = sin(ph_rad)

    gx = sin_th * cos_ph
    gy = sin_th * sin_ph
    gz = cos_th

    ;
    ; Transform gun2 firing directions to GDU1 coordinates
    ;
    gy = gy * (gun eq 1) - gy * (gun eq 2)
    gz = gz * (gun eq 1) - gz * (gun eq 2)

    ;
    ; Normalize the magnetic field vector
    ; 
    bn = sqrt(double(bx)^2 + double(by)^2 + double(bz)^2)
    bxn = bx / bn
    byn = by / bn
    bzn = bz / bn

    ;
    ; Set semi-axes in azimuth (phi) direction from keyword or (default) let
    ; it vary from 0.5 to 0.25 degrees as theta goes from 0 to 90 degrees
    ;
    if keyword_set(dph_model) then $
        dph_2 = dblarr(NN) + dph_model $
    else $
        dph_2 = 0.5 * ( 1 - gun_theta_deg/180. )

    ;
    ; Set semi-axis in polar direction (theta) from keyword, or (default) let
    ; it vary from 0.5 to 2.0 as theta goes from 0 to 90 degrees
    ; 
    if keyword_set(dth_model) then $
        dth_2 = dblarr(NN) + dth_model $
    else $
        dth_2 = 0.5 * ( 1 + 6.0 * gun_theta_deg/180. )


    ;
    ; Calculate ep = (g x b); this is a tangent vector along the Bperp line
    ; (gives the orientation of the dth semi-axis of the beam profile ellipse)
    ;
    epx = gy*bz - gz*by
    epy = gz*bx - gx*bz
    epz = gx*by - gy*bx
    epn = sqrt(epx^2 + epy^2 + epz^2)
    epx = epx/epn & epy = epy/epn & epz = epz/epn

    ;
    ; Calculate eth (a unit vector that is locally oriented along theta)
    ;
    ethx =  cos_th * cos_ph
    ethy =  cos_th * sin_ph
    ethz = -sin_th

    ;
    ; Calculate angle between ep and eth
    ;
    alpha = acos(epx*ethx + epy*ethy + epz*ethz)

    ;
    ; Calculate the (full) width of the beam along the Bperp line that
    ; cuts through the beam profile [in radians]
    ;
    enum  = 1 + tan(alpha)^2
    denom = (1/dth_2)^2  + (tan(alpha)/dph_2)^2

    sigma = !dtor * 2 * sqrt( enum / denom )

    return, sigma

END
