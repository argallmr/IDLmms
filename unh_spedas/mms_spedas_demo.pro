; docformat = 'rst'
;
; NAME:
;    mms_spedas_demo
;
; PURPOSE:
;+
;   Demo basics of SPEDAS for MMS.
;
; :Categories:
;    MMS, SPEDAS
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
;       2015/03/15  -   Written by Matthew Argall
;-
;*****************************************************************************************
;Initialize tplot
;   - TPlot required IDL to be in 8-bit color mode (DECOMPOSED=0)
;   - Load the rainbow color table
;   - Put the color white rgb = [255,255,255] at the top of the color table
;     so that the axes are white, not red.
tplot
device, DECOMPOSED=0
loadct, 13
tvlct, 255, 255, 255, 255

;Define where data will be downloaded (ROOT_DATA_DIR)
setenv,'ROOT_DATA_DIR=' + getenv('IDL_TMPDIR') + 'data/'
mms_init, LOCAL_DATA_DIR = getenv('IDL_TMPDIR') + 'data/'

;See where the SPEDAS root data directory is
print, root_data_dir()

;-----------------------------------------------------
; Load CDF Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;Pick a time interval
timespan, '2015-05-06/23:10:00', 12, /hour

;Get data
;   - Will ask for username and password
;   - "root_data_dir" to see that all the data is there.
mms_load_dfg, sc = 'mms1'

;Check which variables were loaded into tplot
tplot_names

;Plot the magnetic field
tplot, 'mms1_dfg_srvy_gsm_dmpa'

;-----------------------------------------------------
; Load More Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;
;The programs that exist so far:
;   mms_load_dfg.pro
;   mms_load_afg.pro
;   mms_load_epd_eis.pro
;   mms_load_epd_feeps.pro
;   mms_load_dsp.pro
;   .../spedas/idl/projects/mms/sitl/sitl_data_fetch
;

;
;Taken from "dsp_sitl_crib", but does not work!
;
;No other load_dsp programs work
;

mms_load_dsp, SC='mms1', DATA_TYPE='epsd'


;-----------------------------------------------------
; Load Data from a Local File \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

dir  = '/home/argall/data/'
dsp_epsd_file = filepath('mms1_dsp_fast_l2_epsd_20150704_v0.6.2.cdf',        ROOT_DIR=dir)
dfg_file      = filepath('mms1_dfg_srvy_ql_20150704_v0.0.2.cdf',             ROOT_DIR=dir)
edp_file      = filepath('mms1_edp_comm_ql_dce2d_20150704000000_v0.1.0.cdf', ROOT_DIR=dir)

;Set the time range
timespan, '2015-07-04/00:00:00', 6, /hour

;Initialize leap second table
;   - Supposedly unnecessary with CDF v6.0+
cdf_leap_second_init
cdf2tplot, dfg_file
cdf2tplot, edp_file
cdf2tplot, dsp_epsd_file

;See which variables were loaded
tplot_names

;Plot GDU1 and GDU2 raw counts on different axes
tplot, ['mms1_dfg_srvy_dmpa', 'mms1_edp_dce_xyz_dsl', 'mms1_dsp_epsd_x']
ylim, 'mms1_edp_dce_xyz_dsl', [-20,20]
ylim, 'mms1_dsp_epsd_x', 3e1, 1e5
zlim, 'mms1_dsp_epsd_x', 1e-16, 1e-10
options, 'mms1_dsp_epsd_x', 'ylog', 1
options, 'mms1_dsp_epsd_x', 'zlog', 1
tplot
