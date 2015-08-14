; ***************************************************************************
; MMS FIELDS -- MMSEDI_PolarGrid: calculate cartesian coordinates of the
;               grid points for a specified 2D polar grid
;
; Created: 06/23/15 by Hans Vaith
; ***************************************************************************

; ===========================================================================
  FUNCTION MMSEDI_PolarGrid, phi_min, phi_max, phi_step, $
                        r_min,   r_max,   r_step
; ===========================================================================
; This function returns the cartesian coordinates (x,y) of the grid points
; of a 2D polar (azimuth,radius) grid
;
; Parameters
;   phi_min     minimum azimuth angle of grid in degrees
;   phi_max     maximum azimuth angle of grid in degrees
;   phi_step    angular grid resolution in degrees
;
;   r_min       minimum radius in meters
;   r_max       maximum radius in meters
;   r_step      radial grid resolution in meters
;
; Return
;   2D structure array of x,y grid point coordinates 
; ***************************************************************************

    N_PHI    = ulong( (phi_max - phi_min) / double(phi_step) )  +  1
    N_RADIUS = ulong( (r_max   - r_min  ) / double(r_step  ) )  +  1

    angle_arr  = phi_min + lindgen(N_PHI)    * phi_step
    radius_arr = r_min   + lindgen(N_RADIUS) * r_step

    grid = replicate( {MMSEDI_GRID, x:0.0, y:0.0} , N_PHI, N_RADIUS)

    grid.x = cos(angle_arr*!dtor) # radius_arr
    grid.y = sin(angle_arr*!dtor) # radius_arr

    return, grid

END
