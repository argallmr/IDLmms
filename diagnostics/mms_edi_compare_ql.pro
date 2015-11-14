; docformat = 'rst'
;
; NAME:
;    mms_edi_compare_ql
;
; PURPOSE:
;+
;   Compare EDI and EDP electric fields.
;
; :Categories:
;    MMS, EDI, EDP, QL
;
; :Params:
;
; :Keywords:
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
;       2015/11/11  -   Written by Matthew Argall
;-
function mms_edi_compare_ql, sc, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif

;Test intervals
;mms4 2015-10-01: 15:30 - 16:30
;mms3 2015-10-12: 14:00 - 15:30
;mms3 2015-10-13: 15:30 - 16:30
;mms3 2015-10-18: 12:45 - 15:00
;mms3 2015-10-30: 15:00 - 15:30

	sc       = 'mms4'
	mode     = 'srvy'
	tstart   = '2015-10-01T15:30:00Z'
	tend     = '2015-10-01T16:30:00Z'

;-------------------------------------------------------
; Find Files ///////////////////////////////////////////
;-------------------------------------------------------
	edi_files = mms_find_file(sc, 'edi', 'srvy', 'ql', $
	                          DIRECTORY = '/nfs/edi/temp', $
	                          OPTDESC   = 'efield', $
	                          SEARCHSTR = str, $
	                          TSTART    = tstart, $
	                          TEND      = tend)

	edp_files = mms_find_file(sc, 'edp', ['slow', 'fast'], 'ql', $
	                          OPTDESC   = 'dce', $
	                          TSTART    = tstart, $
	                          TEND      = tend)

;-------------------------------------------------------
; Read Files ///////////////////////////////////////////
;-------------------------------------------------------
	edi_ql = mms_edi_ql_tritof_read(edi_files, tstart, tend)
	edp_ql = mms_edp_read_ql(edp_files, tstart, tend)

;-------------------------------------------------------
; Plot Data ////////////////////////////////////////////
;-------------------------------------------------------
	t_edp_ssm = MrCDF_epoch2ssm(edp_ql.tt2000, edi_ql.tt2000[0])
	t_edi_ssm = MrCDF_epoch2ssm(edi_ql.tt2000, edi_ql.tt2000[0])
	
	Exrange = [min(edp_ql.E_dsl[0,*], MAX=ymax), ymax]
	Eyrange = [min(edp_ql.E_dsl[1,*], MAX=ymax), ymax]
	Ezrange = [min(edp_ql.E_dsl[2,*], MAX=ymax), ymax]
	
	;Ex
	pEx = Plot(t_edp_ssm, edp_ql.E_dsl[0,*], $
	           LAYOUT      = [1,3,1], $
	           NAME        = '$E_{DCE}$', $
	           TITLE       = strupcase(sc) + ' ' + strmid(tstart, 0, 10), $
	           XSTYLE      = 1, $
	           XTICKFORMAT = '(a1)', $
	           YRANGE      = Exrange, $
	           YSTYLE      = 1, $
	           YTITLE      = '$E_{x}$!C(mV/m)')
	oEx = Plot(t_edi_ssm, edi_ql.E_tri_dmpa[0,*], $
	           COLOR       = 'Blue', $
	           NAME        = '$E_{TRI}$', $
	           OVERPLOT = pEx)
	qEx = Plot(t_edi_ssm, edi_ql.E_tof_dmpa[0,*], $
	           COLOR       = 'Red', $
	           NAME        = '$E_{TOF}$', $
	           OVERPLOT = pEx)
	
	;Ey
	pEy = Plot(t_edp_ssm, edp_ql.E_dsl[1,*], $
	           /CURRENT, $
	           LAYOUT      = [1,3,2], $
	           TITLE       = strupcase(sc) + ' ' + strmid(tstart, 0, 10), $
	           XSTYLE      = 1, $
	           XTICKFORMAT = '(a1)', $
	           YRANGE      = Eyrange, $
	           YSTYLE      = 1, $
	           YTITLE      = '$E_{y}$!C(mV/m)')
	oEy = Plot(t_edi_ssm, edi_ql.E_tri_dmpa[1,*], $
	           COLOR       = 'Blue', $
	            OVERPLOT = pEy)
	qEz = Plot(t_edi_ssm, edi_ql.E_tof_dmpa[1,*], $
	           COLOR       = 'Red', $
	           OVERPLOT = pEy)
	
	;Ex
	pEz = Plot(t_edp_ssm, edp_ql.E_dsl[2,*], $
	           /CURRENT, $
	           LAYOUT      = [1,3,3], $
	           TITLE       = strupcase(sc) + ' ' + strmid(tstart, 0, 10), $
	           XSTYLE      = 1, $
	           XTICKFORMAT = 'time_labels', $
	           XTITLE      = 'Time (UT)', $
	           YRANGE      = Ezrange, $
	           YSTYLE      = 1, $
	           YTITLE      = '$E_{z}$!C(mV/m)')
	oEz = Plot(t_edi_ssm, edi_ql.E_tri_dmpa[2,*], $
	           COLOR       = 'Blue', $
	           OVERPLOT = pEz)
	qEz = Plot(t_edi_ssm, edi_ql.E_tof_dmpa[2,*], $
	           COLOR       = 'Red', $
	           OVERPLOT = pEz)
	
	;Legend
	gL = Legend(POSITION = [1.0, 1.0], $
	            /RELATIVE, $
	            TARGET   = [pEx, oEx, qEx])
	
	return, pEx.window
end
