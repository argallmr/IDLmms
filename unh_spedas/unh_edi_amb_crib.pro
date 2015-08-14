; docformat = 'rst'
;
; NAME:
;    unh_edi_amb_crib
;
; PURPOSE:
;+
;   Plot EDI ambient data
;
; :Categories:
;    MMS, SITL
;
; :Examples:
;   To use::
;       IDL> .r unh_edi_amb_crib
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
;   - Not that the mms_load_data and mms_data_fetch routines alter
;     how SPEDAS looks for data. If you have my versions of those files,
;     SPEDAS will not check the web for the most recent available versions.
;     Instead, it will check only your local cache (e.g. the UNH SDC mirror).
;   - If you want to use the SDC instead, remove my versions of the two
;     programs from your IDL path and do not set LOCAL_DATA_DIR.
;
mms_init, LOCAL_DATA_DIR='/nfs/'

;Set the time range and spacecraft ID
timespan,'2015-06-23/00:00', 6, /hour
sc_id = 'mms3'

;Load Data into TPlot
mms_sitl_get_dfg, SC_ID=sc_id
mms_load_edi_amb, SC_ID=sc_id

;-----------------------------------------------------
; MMS Colors \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

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
;Set plot properties
options, sc_id + '_dfg_srvy_gsm_dmpa', 'colors', [iblue, igreen, ired]
options, sc_id + '_dfg_srvy_gsm_dmpa', 'labels', ['Bx', 'By', 'Bz']
options, sc_id + '_dfg_srvy_gsm_dmpa', 'labflag', -1

;-----------------------------------------------------
; EDI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;-----------------------------------------------------
; Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;Plot the data
;   1. B GSM-DMPA
;   2. Counts GDU1
;   3. Counts GDU2
tplot, [sc_id + '_dfg_srvy_gsm_dmpa', $
        sc_id + '_edi_amb_gdu1_raw_counts1', $
        sc_id + '_edi_amb_gdu2_raw_counts1']

;Restore the old color table
tvlct, r, b, b

end

