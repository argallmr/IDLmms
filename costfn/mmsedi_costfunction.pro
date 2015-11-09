; ***************************************************************************
; MMS FIELDS -- Cost function for triangulation of EDI beam firing directions
; Created: 06/24/15 by Hans Vaith
;
; ***************************************************************************

; ===========================================================================
FUNCTION MMSEDI_CostFunction, gfx, gfy, gpx, gpy, beamwidth, grid
	compile_opt idl2
	on_error, 2
; ===========================================================================
; Parameters
;   gfx         array   gun firing direction x-component in BPP (Bperp plane)
;   gfx         array   gun firing direction y-component in BPP
;   gpx         array   gun position x-component in BPP [in meters]
;   gpy         array   gun position y-component in BPP [in meters]
;   beamwidth   array   beam width in plane perpendicular to B
;                       in units of radians, as returned by
;                       MMSEDI_BeamWidth()
;   grid        2D structure array of cartesian grid point positions (x,y)
;               in meters, as returned by MMSEDI_PolarGrid()
;
; Return
;   cost function array, which has the same dimensions as the 'grid' argument
; ***************************************************************************

    N_BEAMS = n_elements(gfx)

    ;
    ; Sanity checks
    ;
    if n_elements(gfy)       ne N_BEAMS or $
       n_elements(gpx)       ne N_BEAMS or $
       n_elements(gpy)       ne N_BEAMS or $
       n_elements(beamwidth) ne N_BEAMS then begin

        msg = 'Inconsistent number of elememts (gfx,gfy,gpx,gpy,beamwidth)'
        message, msg
    endif

    ;
    ; Get grid dimensions
    ; First dimension is azimuth (phi), second dimension is radius
    ;
    dimensions   = size(grid,/dimensions)
    costfunction = dblarr(dimensions[0],dimensions[1])
    ;
    ; Note: do not use /nozero in the dblarr() call above. We rely on the
    ; inital state of the array being all zeroes.


    ;
    ; Calculate all firing angles in a single call (vectorized=more efficient)
    ;
    firing_angle = atan(gfy, gfx)

    ;
    ; Loop through beams, calculate each beam's contribution to the
    ; costfunction at all grid points
    ;
    for i=0L,N_BEAMS-1 do begin
        ;
        ; Calculate angles of directions from current gun position
        ; to each grid point
        ;
        grid_angle   = atan(grid.y - gpy[i], grid.x - gpx[i])


        angle_diff   = grid_angle - firing_angle[i]
        ;
        ; Re-normalize angular difference
        ; Reasons (numbers are given in degrees not radians, for clarity)
        ; 1) the input range of -360 to +360 degrees needs to be reduced
        ;    to -180 to 180 (add 360 if below -180, subtract 360 if above 180)
        ; 2) the angular deviation between two directions cannot be larger
        ;    than 90 degrees (in other words: no distinction between parallel
        ;    and anti-parallel, since we are only interested in orientation,
        ;    not in direction)
        ; The two transformations combined are summarized in the following
        ; table:
        ;       IN                  OUT
        ;   -360 ... -270         0 ... 90
        ;   -270 ... -180       -90 ...  0
        ;   -180 ...  -90         0 ... 90
        ;    -90 ...    0       -90 ...  0
        ;      0 ...   90         0 ... 90
        ;     90 ...  180       -90 ...  0
        ;    180 ...  270         0 ... 90
        ;    270 ...  360       -90 ...  0
        ;
        ; The formula below achieves this transformation
        ;
        angle_diff = ((angle_diff + 2.5*!DPI) mod !DPI) - !DPI/2

        ;
        ; Add contribution of current beam to cost function
        ;
        costfunction = costfunction + (angle_diff/beamwidth[i])^2

    endfor

    return, costfunction

END
