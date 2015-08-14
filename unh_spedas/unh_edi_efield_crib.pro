; docformat = 'rst'
;
; NAME:
;    unh_edi_amb_crib
;
; PURPOSE:
;+
;   Plot EDI electric field data
;
; :Categories:
;    MMS, SITL
;
; :Examples:
;   To use::
;       IDL> .r unh_edi_efield_crib
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 348
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/08/03  -   Written by Matthew Argall
;-
;*****************************************************************************************
;
;Set local data directory.
;   - Note that the mms_load_data and mms_data_fetch routines alter
;     how SPEDAS looks for data. If you have my versions of those files,
;     SPEDAS will not check the web for the most recent available versions.
;     Instead, it will check only your local cache (e.g. the UNH SDC mirror).
;   - If you want to use the SDC instead, remove my versions of the two
;     programs from your IDL path and do not set LOCAL_DATA_DIR.

;Check if mms_init has been run.
;   - If not, run it and set the local data directory.
defsysv, '!mms', EXISTS=exists
if ~exists then mms_init, LOCAL_DATA_DIR='/nfs/'

;
; Note:
;   - In order to overplot -VxB and E_EDI onto E_EDP, I had to use tplot_panels,
;     which works only temporarily.
;   - If you resize the window or use tlimit to zoom in, you will need to copy and
;     paste the tplot_panels from the bottom of the program into IDL in order for
;     them to appear again.
;


;Set the time range and spacecraft ID
timespan,'2015-07-23/04:30', 3, /hour
sc_id      = 'mms1'
load_data  = 1
edotb_eq_0 = 1

; Set the plot window size for your display... 
; consider reading display size, and setting plot at, say, 90% of height, width
plot_window_height = 700
plot_window_width  = 700

;Load Data into TPlot
if load_data then begin
	mms_load_edi_efield, SC_ID = sc_id
	mms_sitl_get_dfg,    SC_ID = sc_id
	mms_load_edp,        SC        = sc_id, $
	                     MODE      = 'slow', $
	                     LEVEL     = 'ql', $
	                     DATA_TYPE = 'dce2d'
endif

;Load colors into color table. MMS Colors:
;   - MMS1:  BLACK (change to white if using dark background)
;   - MMS2:  RED
;   - MMS3:  GREEN
;   - MMS4:  BLUE
;   - X,Y,Z is BLUE, GREEN, RED, solid, dashed, dotted  
;
;   - Red = RGB [213, 94, 0] 
;   - Green = RGB [0, 158, 115]   
;   - Blue = RGB [86, 180, 233]
tvlct, r, g, b, /GET
red   = [[213], [ 94], [  0]]
green = [[  0], [158], [115]]
blue  = [[ 86], [180], [233]]

ired   = 1
igreen = 2
iblue  = 3
tvlct, red,   ired
tvlct, green, igreen
tvlct, blue,  iblue

;-----------------------------------------------------
; DFG \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
options, sc_id + '_dfg_srvy_gsm_dmpa', 'colors',  [iblue, igreen, ired]
options, sc_id + '_dfg_srvy_gsm_dmpa', 'labels',  ['Bx', 'By', 'Bz']
options, sc_id + '_dfg_srvy_gsm_dmpa', 'labflag', -1
ylim,    sc_id + '_dfg_srvy_gsm_dmpa', [-300, 300]

;-----------------------------------------------------
; Elevation Angle \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Determine the angle between B and the spin-plane
get_data, sc_id + '_dfg_srvy_gsm_dmpa', DATA = dfg_data

;Create unit vector and get the angle from Bz
b_mag = sqrt(total(dfg_data.y^2, 2))
angle = 90.0 - acos(dfg_data.y[*,2] / b_mag) * !radeg

;Store the angle in degrees
store_data, 'B_angle', DATA={x: dfg_data.x, y: angle}

;Set Properties
options, 'B_angle', 'ytitle', 'Elevation Angle!C(Deg)'
ylim, 'B_angle', [min(angle, MAX=amax), amax]

;Delete data
b_mag = !Null

;-----------------------------------------------------
; EDI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Split EDI E-field data into components
get_data, sc_id + '_edi_E_dmpa',     DATA = edi_e_data
get_data, sc_id + '_edi_v_ExB_dmpa', DATA = edi_v_data

;Remove fill values
igood = where(edi_e_data.y[*,0] ne -1e31, ngood)
if ngood gt 0 then edi_e_data.y = edi_e_data.y[igood, *]
if ngood gt 0 then edi_v_data.y = edi_v_data.y[igood, *]

;Store components into TPlot
store_data, 'EDI_Ex', DATA = { x: edi_e_data.x, y: edi_e_data.y[*,0] }
store_data, 'EDI_Ey', DATA = { x: edi_e_data.x, y: edi_e_data.y[*,1] }
store_data, 'EDI_Ez', DATA = { x: edi_e_data.x, y: edi_e_data.y[*,2] }
store_data, 'EDI_Vx', DATA = { x: edi_v_data.x, y: edi_v_data.y[*,0] }
store_data, 'EDI_Vy', DATA = { x: edi_v_data.x, y: edi_v_data.y[*,1] }
store_data, 'EDI_Vz', DATA = { x: edi_v_data.x, y: edi_v_data.y[*,2] }

;Set properties
options, 'EDI_Ex', 'colors', ired
options, 'EDI_Ey', 'colors', ired
options, 'EDI_Ez', 'colors', ired
options, 'EDI_Ex', 'psym',  2
options, 'EDI_Ey', 'psym',  2
options, 'EDI_Ez', 'psym',  2
options, 'EDI_Ex', 'symsize',  1
options, 'EDI_Ey', 'symsize',  1
options, 'EDI_Ez', 'symsize',  1
options, 'EDI_Ex', 'labels',  'Ex_EDI'
options, 'EDI_Ey', 'labels',  'Ey_EDI'
options, 'EDI_Ez', 'labels',  'Ez_EDI'
options, 'EDI_Ex', 'labflag', 3
options, 'EDI_Ey', 'labflag', 3
options, 'EDI_Ez', 'labflag', 3
options, 'EDI_Ex', 'labpos', 0.001
options, 'EDI_Ey', 'labpos', 0.001
options, 'EDI_Ez', 'labpos', 0.001

options, 'EDI_Vx', 'colors', ired
options, 'EDI_Vy', 'colors', ired
options, 'EDI_Vz', 'colors', ired
options, 'EDI_Vx', 'psym',  2
options, 'EDI_Vy', 'psym',  2
options, 'EDI_Vz', 'psym',  2
options, 'EDI_Vx', 'symsize',  1
options, 'EDI_Vy', 'symsize',  1
options, 'EDI_Vz', 'symsize',  1
options, 'EDI_Vx', 'labels',  'Vx_EDI'
options, 'EDI_Vy', 'labels',  'Vy_EDI'
options, 'EDI_Vz', 'labels',  'Vz_EDI'
options, 'EDI_Vx', 'labflag', 3
options, 'EDI_Vy', 'labflag', 3
options, 'EDI_Vz', 'labflag', 3
options, 'EDI_Vx', 'labpos', -50
options, 'EDI_Vy', 'labpos', -50
options, 'EDI_Vz', 'labpos', -50

;-----------------------------------------------------
; EDP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Separate EDP into components
get_data, sc_id + '_edp_slow_dce_dsl', DATA = edp_data

;Remove fill values
igood = where(edp_data.y[*,0] ne -1e31, ngood)
if ngood gt 0 then edp_data.y = edp_data.y[igood, *]

;Create data structures
EDPx = { x: edp_data.x, $
         y: edp_data.y[*,0] $
       }
EDPy = { x: edp_data.x, $
         y: edp_data.y[*,1] $
       }
EDPz = { x: edp_data.x, $
         y: edp_data.y[*,2] $
       }

;Store components into TPlot
store_data, 'EDP_Ex', DATA=EDPx
store_data, 'EDP_Ey', DATA=EDPy
store_data, 'EDP_Ez', DATA=EDPz

;Set Properties
options, 'EDP_Ex', 'labels', 'Ex_EDP'
options, 'EDP_Ey', 'labels', 'Ey_EDP'
options, 'EDP_Ez', 'labels', 'Ez_EDP'
options, 'EDP_Ex', 'labflag', 3
options, 'EDP_Ey', 'labflag', 3
options, 'EDP_Ez', 'labflag', 3
options, 'EDP_Ex', 'labpos', 10
options, 'EDP_Ey', 'labpos', 10
options, 'EDP_Ez', 'labpos', 10
ylim, 'EDP_Ex', [-15, 15]
ylim, 'EDP_Ey', [-15, 15]
ylim, 'EDP_Ez', [-15, 15]

;-----------------------------------------------------
; VxB Convective Field \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
get_data, sc_id + '_ql_pos_gsm', DATA=pos_data
if n_elements(pos_data) gt 0 then begin
	;Position
	t = dfg_data.x
	x = interpol(pos_data.y[*,0], pos_data.x, t)
	y = interpol(pos_data.y[*,1], pos_data.x, t)
	z = interpol(pos_data.y[*,2], pos_data.x, t)
	
	;Velocity
	dt = t[1:*] - t
	vx = (x[1:*] - x) / dt
	vy = (y[1:*] - y) / dt
	vz = (z[1:*] - z) / dt
	
	;VxB
	Ec_x = vy * dfg_data.y[1:*, 2] - vz * dfg_data.y[1:*, 1]
	Ec_y = vz * dfg_data.y[1:*, 0] - vx * dfg_data.y[1:*, 2]
	Ec_z = vx * dfg_data.y[1:*, 1] - vy * dfg_data.y[1:*, 0]
	
	;Store the data
	;   - E = -1.0 * (V x B)
	;   - Convert: km/s * nT  -->  m/s * T * 1e6  -->  V / m * 1e6  -->  mV/m * 1e-3
	store_data, 'V_sc', DATA = { x: t[1:*], y: [[vx], [vy], [vz]] }
	store_data, 'Ec_x', DATA = { x: t[1:*], y: -1e-3 * Ec_x }
	store_data, 'Ec_y', DATA = { x: t[1:*], y: -1e-3 * Ec_y }
	store_data, 'Ec_z', DATA = { x: t[1:*], y: -1e-3 * Ec_z }
	
	;Change properties
	options, 'Ec_x', 'colors', igreen
	options, 'Ec_y', 'colors', igreen
	options, 'Ec_z', 'colors', igreen
	options, 'Ec_x', 'labels', 'Ex_sc'
	options, 'Ec_y', 'labels', 'Ey_sc'
	options, 'Ec_z', 'labels', 'Ez_sc'
	options, 'Ec_x', 'labflag', 3
	options, 'Ec_y', 'labflag', 3
	options, 'Ec_z', 'labflag', 3
	options, 'Ec_x', 'labpos', -10
	options, 'Ec_y', 'labpos', -10
	options, 'Ec_z', 'labpos', -10
	
	;Clear data
	t        = !Null
	dt       = !Null
	vx       = !Null
	vy       = !Null
	vz       = !Null
	pos_data = !Null
endif

;-----------------------------------------------------
; ExB drift Velocity \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Interpolate the electric field onto the magnetic field
Ex = interpol( edp_data.y[*,0], edp_data.x, dfg_data.x )
Ey = interpol( edp_data.y[*,1], edp_data.x, dfg_data.x )
Ez = interpol( edp_data.y[*,2], edp_data.x, dfg_data.x )

;Ez from E dot B = 0?
if edotb_eq_0 $
	then Ez = -(dfg_data.y[*,0] * Ex + dfg_data.y[*,1] * Ey) / dfg_data.y[*,2]

;B squared magnitude: |B|^2
Bmag_sqr = total(dfg_data.y^2, 2)

;Drift Velocity
;   - v = ExB/|B|^2
;   - Scale: mV/m * nT / (nT)^2  -->  V/m * T / (T)^2 * 1e-6  -->  km/s * 1e3
v_ExB       = fltarr( size( dfg_data.y, /DIMENSIONS ) )
v_ExB[*,0]  = (Ey * dfg_data.y[*,2] - Ez * dfg_data.y[*,1]) / Bmag_sqr
v_ExB[*,1]  = (Ez * dfg_data.y[*,0] - Ex * dfg_data.y[*,2]) / Bmag_sqr
v_ExB[*,2]  = (Ex * dfg_data.y[*,1] - Ey * dfg_data.y[*,0]) / Bmag_sqr
v_ExB      *= 1e3

;Store the data
store_data, 'v_ExB_x', DATA = { x: dfg_data.x, y: v_ExB[*,0] }
store_data, 'v_ExB_y', DATA = { x: dfg_data.x, y: v_ExB[*,1] }
store_data, 'v_ExB_z', DATA = { x: dfg_data.x, y: v_ExB[*,2] }

;Set properties
options, 'v_ExB_x', 'labels', 'Vx'
options, 'v_ExB_y', 'labels', 'Vy'
options, 'v_ExB_z', 'labels', 'Vz'
options, 'v_ExB_x', 'labflag', 3
options, 'v_ExB_y', 'labflag', 3
options, 'v_ExB_z', 'labflag', 3
options, 'v_ExB_x', 'labpos', 50
options, 'v_ExB_y', 'labpos', 50
options, 'v_ExB_z', 'labpos', 50

;Delete data
v_ExB    = !Null
Bmag_sqr = !Null
Ex       = !Null
Ey       = !Null
Ez       = !Null

;-----------------------------------------------------
; Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Create a window
window, 0, XSIZE=plot_window_width, YSIZE=plot_window_height

;Plot the data
;   1. Bxyz
;   2. Ex
;   3. Ey
;   4. Ez
tplot, [sc_id + '_dfg_srvy_gsm_dmpa', $
        'B_angle', $
        'EDP_Ex', $
        'EDP_Ey', $
        'EDP_Ez', $
        'v_ExB_x', $
        'v_ExB_y', $
        'v_ExB_z']
        
;Overplot EDI onto EDP
tplot_panel, V='EDP_Ex', O='EDI_Ex', PSYM=1
tplot_panel, V='EDP_Ey', O='EDI_Ey', PSYM=1
tplot_panel, V='EDP_Ez', O='EDI_Ez', PSYM=1
        
;Overplot Ec onto EDP
tplot_panel, V='EDP_Ex', O='Ec_x'
tplot_panel, V='EDP_Ey', O='Ec_y'
tplot_panel, V='EDP_Ez', O='Ec_z'

;Overplot EDI onto v_ExB
tplot_panel, V='v_ExB_x', O='EDI_Vx', PSYM=1
tplot_panel, V='v_ExB_y', O='EDI_Vy', PSYM=1
tplot_panel, V='v_ExB_z', O='EDI_Vz', PSYM=1

;-----------------------------------------------------
; Clean Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;Restore the original color table
tvlct, r, g, b
end

