; docformat = 'rst'
;
; NAME:
;    mms_test_rotations
;
; PURPOSE:
;+
;   Test despinning and coordinate system transformations using DSS and FDOA.
;
; :Categories:
;    MMS, Diagnostics
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
;       2015/10/19  -   Written by Matthew Argall
;       2015/11/27  -   Renamed from mms_test_despin to mms_test_rotations. Added
;                           rotations to GSE and GSM via different methods. - MRA
;-
pro mms_test_rotations, sc, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return
	endif

	sc       = 'mms3'
	tstart   = '2015-08-16T12:45:00Z'
	tend     = '2015-08-16T13:45:00Z'
	sdc_dir  = '/nfs'
	hk_dir   = filepath('', ROOT_DIR=sdc_dir, SUBDIRECTORY='hk')
	att_dir  = filepath('', ROOT_DIR=sdc_dir, SUBDIRECTORY=['ancillary', sc, 'defatt'])

;-------------------------------------------------------
; Find Files ///////////////////////////////////////////
;-------------------------------------------------------
	
	;FGM SRVY L2PRE file
	instr   = 'dfg'
	fgm_file = mms_find_file(sc, instr, 'srvy', 'l2pre', $
	                         COUNT     = nfgm, $
	                         SDC_ROOT  = sdc_dir, $
	                         SEARCHSTR = searchstr, $
	                         TSTART    = tstart, $
	                         TEND      = tend)
	if nfgm eq 0 then message, 'FGM file not found: "' + searchstr + '".'
	
	;DSS file
	instr   = 'dfg'
	dss_file = mms_find_file(sc, 'fields', 'hk', 'l1b', $
	                         COUNT     = ndss, $
	                         OPTDESC   = '101', $
	                         SDC_ROOT  = hk_dir, $
	                         SEARCHSTR = searchstr, $
	                         TSTART    = tstart, $
	                         TEND      = tend)
	if ndss eq 0 then message, 'DSS file not found: "' + searchstr + '".'
	
	;DEFATT
	str = filepath(ROOT_DIR=att_dir, strupcase(sc) + '_DEFATT_%Y%D_%Y%D.V*' )
	att_file = MrFile_Search( str, $
	                          /CLOSEST, $
	                          COUNT     = nfiles_att, $
	                          TSTART    = tstart, $
	                          TEND      = tend, $
	                          TIMEORDER = '%Y%D', $
	                          VREGEX    = 'V([0-9]{2})' )
	if nfiles_att eq 0 then message, 'No attitude files found: "' + str + '".'

;-------------------------------------------------------
; Read Data ////////////////////////////////////////////
;-------------------------------------------------------
	
	;Read all data
	fgm_l2pre = mms_fg_read_l2pre(fgm_file, tstart, tend)
	dss       = mms_dss_read_sunpulse(dss_file, tstart, tend, /UNIQ_PULSE)
	atthdr    = mms_fdoa_read_defatt_header(att_file[0])
	
	;Alternative method for obtaining data AND zmpa axis
	defatt    = mms_fdoa_read_defatt(att_file, tstart, tend, HEADER=atthdr)
	zmpa      = atthdr.zmpa[*,0]

	;Extract magnetic field data of interest
	t      = fgm_l2pre.tt2000
	b_bcs  = fgm_l2pre.b_bcs[0:2,*]
	b_dmpa = fgm_l2pre.b_dmpa[0:2,*]
	b_gse  = fgm_l2pre.b_gse[0:2,*]
	b_gsm  = fgm_l2pre.b_gsm[0:2,*]
	
	;Delete leftovers
	fgm_l2pre = !Null

;-------------------------------------------------------
; BCS --> SMPA /////////////////////////////////////////
;-------------------------------------------------------
	
	;Rotate BCS --> SMPA
	bcs2smpa   = mms_fg_xbcs2smpa(zmpa)
	b_unh_smpa = MrVector_Rotate(bcs2smpa, b_bcs)

;-------------------------------------------------------
; SMPA --> DMPA ////////////////////////////////////////
;-------------------------------------------------------
	
	;Method 1: Sunpulse
	smpa2dmpa      = mms_dss_xdespin(dss, t)
	b_unh_dmpa_dss = MrVector_Rotate(smpa2dmpa, b_unh_smpa)
	
	;Method 2: Attitude
	smpa2dmpa       = mms_fdoa_xdespin(defatt, t)
	b_unh_dmpa_fdoa = MrVector_Rotate(smpa2dmpa, b_unh_smpa)

;-------------------------------------------------------
; BCS --> GEI //////////////////////////////////////////
;-------------------------------------------------------

	;Method 1: BCS --> GEI
	bcs2gei     = mms_fdoa_xgei2bcs(defatt.q, defatt.tt2000, t, /INVERSE)
	b_unh_gei_q = qtvrot(b_bcs, bcs2gei)

	;Method 2: DMPA --> GEI
	gei2dmpa    = mms_fdoa_xgei2despun(defatt, t, TYPE='P')
	dmpa2gei    = transpose(temporary(gei2dmpa), [1,0,2])
	b_unh_gei_p = MrVector_Rotate(dmpa2gei, b_dmpa)

;-------------------------------------------------------
; DMPA --> GSE /////////////////////////////////////////
;-------------------------------------------------------
	;Now we need the cotrans library
	cotrans_lib
	
	;Breakdown time
	MrCDF_Epoch_Breakdown, reform(t), yr, mo, day, hr, mnt, sec, milli, micro, nano
	doy = MrDate2DOY(mo, day, yr)
	sec = sec + milli*1d-3 + micro*1d-6 + nano*1d-9
	
	;GEI --> GSE
	tgeigse_vect, yr, doy, hr, mnt, sec, $
	              reform(b_unh_gei_q[0,*]), reform(b_unh_gei_q[1,*]), reform(b_unh_gei_q[2,*]), $
	              xqgse, yqgse, zqgse


	;GEI --> GSE
	tgeigse_vect, yr, doy, hr, mnt, sec, $
	              reform(b_unh_gei_p[0,*]), reform(b_unh_gei_p[1,*]), reform(b_unh_gei_p[2,*]), $
	              xpgse, ypgse, zpgse

	;Combine components to 3xN array
;	b_unh_gse_q = transpose( [ [temporary(xqgse)], [temporary(yqgse)], [temporary(zqgse)] ] )
	b_unh_gse_p = transpose( [ [temporary(xpgse)], [temporary(ypgse)], [temporary(zpgse)] ] )

;-------------------------------------------------------
; GSE --> GSM //////////////////////////////////////////
;-------------------------------------------------------
	;GSE --> GSM
	tgsegsm_vect, yr, doy, hr, mnt, sec, $
	              reform(b_gse[0,*]), reform(b_gse[1,*]), reform(b_gse[2,*]), $
	              xgsm, ygsm, zgsm
	
	;Combine components into 3xN array
	b_unh_gsm = transpose( [ [temporary(xgsm)], [temporary(ygsm)], [temporary(zgsm)] ] )

;-------------------------------------------------------
; DMPA Results /////////////////////////////////////////
;-------------------------------------------------------
	;Convert time to seconds
	t_ssm = MrCDF_epoch2ssm(t)
	
	;Get plot positions
	p = cgLayout([1,3], YGAP=0.5)
	
	;Create a window
	cgWindow, WOBJECT=win_dmpa
	win_dmpa -> SetProperty, /NOEXECUTECOMMANDS
	
	;BX Component
	cgPlot, t_ssm, b_dmpa[0,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,0], $
	        TITLE       = 'Despin Transformation Comparision', $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'B$\downX$!C(nT)'
	cgOPlot, t_ssm, b_unh_dmpa_dss[0,*],  /ADDCMD, COLOR = 'blue'
	cgOPlot, t_ssm, b_unh_dmpa_fdoa[0,*], /ADDCMD, COLOR = 'red'
	
	;BY Component
	cgPlot, t_ssm, b_dmpa[1,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,1], $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'B$\downY$!C(nT)'
	cgOPlot, t_ssm, b_unh_dmpa_dss[1,*],  /ADDCMD, COLOR = 'blue'
	cgOPlot, t_ssm, b_unh_dmpa_fdoa[1,*], /ADDCMD, COLOR = 'red'
	
	;BZ Component
	cgPlot, t_ssm, b_dmpa[2,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,2], $
	        XTICKFORMAT = 'time_labels', $
	        YTITLE      = 'B$\downZ$!C(nT)'
	cgOPlot, t_ssm, b_unh_dmpa_dss[2,*],  /ADDCMD, COLOR = 'blue'
	cgOPlot, t_ssm, b_unh_dmpa_fdoa[2,*], /ADDCMD, COLOR = 'red'

	;Create a legend
	cgLegend, /ADDCMD, $
	          ALIGNMENT = 1, $
	          TITLES    = ['FGM', 'UNH DSS', 'UNH DEFATT'], $
	          TCOLORS   = ['black', 'blue', 'red'], $
	          LENGTH    = 0, $
	          LOCATION  = [p[2,0], p[3,0]]
	
	;Execute
	win_dmpa -> SetProperty, NOEXECUTECOMMANDS=0, /UPDATE

;-------------------------------------------------------
; GEI Results //////////////////////////////////////////
;-------------------------------------------------------
	;Create a window
	cgWindow, WOBJECT=win_gei
	win_gei -> SetProperty, /NOEXECUTECOMMANDS
	
	;BX Component
	cgPlot, t_ssm, b_unh_gei_q[0,*], $
	        /ADDCMD, $
	        COLOR       = 'blue', $
	        /NOERASE, $
	        POSITION    = p[*,0], $
	        TITLE       = 'GEI Transformation Comparision', $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'B$\downX$!C(nT)'
	cgOPlot, t_ssm, b_unh_gei_p[0,*], /ADDCMD, COLOR = 'red'
	
	;BY Component
	cgPlot, t_ssm, b_unh_gei_q[1,*], $
	        /ADDCMD, $
	        COLOR       = 'blue', $
	        /NOERASE, $
	        POSITION    = p[*,1], $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'B$\downY$!C(nT)'
	cgOPlot, t_ssm, b_unh_gei_p[1,*], /ADDCMD, COLOR = 'red'
	
	;BZ Component
	cgPlot, t_ssm, b_unh_gei_q[2,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,2], $
	        XTICKFORMAT = 'time_labels', $
	        YTITLE      = 'B$\downZ$!C(nT)'
	cgOPlot, t_ssm, b_unh_gei_p[2,*], /ADDCMD, COLOR = 'red'

	;Create a legend
	cgLegend, /ADDCMD, $
	          ALIGNMENT = 1, $
	          TITLES    = ['UNH QT', 'UNH P'], $
	          TCOLORS   = ['blue', 'red'], $
	          LENGTH    = 0, $
	          LOCATION  = [p[2,0], p[3,0]]
	
	;Update the windwo
	win_gei -> SetProperty, NOEXECUTECOMMANDS=0, /UPDATE

;-------------------------------------------------------
; GSE Results //////////////////////////////////////////
;-------------------------------------------------------
	;Create a window
	cgWindow, WOBJECT=win_gse
	win_gse -> SetProperty, /NOEXECUTECOMMANDS
	
	;BX Component
	cgPlot, t_ssm, b_gse[0,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,0], $
	        TITLE       = 'GSE Transformation Comparision', $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'B$\downX$!C(nT)'
	cgOPlot, t_ssm, b_unh_gse_q[0,*], /ADDCMD, COLOR = 'blue'
	cgOPlot, t_ssm, b_unh_gse_p[0,*], /ADDCMD, COLOR = 'red'
	
	;BY Component
	cgPlot, t_ssm, b_gse[1,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,1], $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'B$\downY$!C(nT)'
	cgOPlot, t_ssm, b_unh_gse_q[1,*], /ADDCMD, COLOR = 'blue'
	cgOPlot, t_ssm, b_unh_gse_p[1,*], /ADDCMD, COLOR = 'red'
	
	;BZ Component
	cgPlot, t_ssm, b_gse[2,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,2], $
	        XTICKFORMAT = 'time_labels', $
	        YTITLE      = 'B$\downZ$!C(nT)'
	cgOPlot, t_ssm, b_unh_gse_q[2,*], /ADDCMD, COLOR = 'blue'
	cgOPlot, t_ssm, b_unh_gse_p[2,*], /ADDCMD, COLOR = 'red'

	;Create a legend
	cgLegend, /ADDCMD, $
	          ALIGNMENT = 1, $
	          TITLES    = ['FGM', 'UNH QT', 'UNH P'], $
	          TCOLORS   = ['black', 'blue', 'red'], $
	          LENGTH    = 0, $
	          LOCATION  = [p[2,0], p[3,0]]
	
	;Update the windwo
	win_gse -> SetProperty, NOEXECUTECOMMANDS=0, /UPDATE

;-------------------------------------------------------
; GSM Results //////////////////////////////////////////
;-------------------------------------------------------
	;Create a window
	cgWindow, WOBJECT=win_gsm
	win_gsm -> SetProperty, /NOEXECUTECOMMANDS
	
	;BX Component
	cgPlot, t_ssm, b_gsm[0,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,0], $
	        TITLE       = 'GSM Transformation Comparision', $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'B$\downX$!C(nT)'
	cgOPlot, t_ssm, b_unh_gsm[0,*], /ADDCMD, COLOR = 'blue'
	
	;BY Component
	cgPlot, t_ssm, b_gsm[1,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,1], $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'B$\downY$!C(nT)'
	cgOPlot, t_ssm, b_unh_gsm[1,*], /ADDCMD, COLOR = 'blue'
	
	;BZ Component
	cgPlot, t_ssm, b_gsm[2,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,2], $
	        XTICKFORMAT = 'time_labels', $
	        YTITLE      = 'B$\downZ$!C(nT)'
	cgOPlot, t_ssm, b_unh_gsm[2,*], /ADDCMD, COLOR = 'blue'

	;Create a legend
	cgLegend, /ADDCMD, $
	          ALIGNMENT = 1, $
	          TITLES    = ['FGM', 'UNH'], $
	          TCOLORS   = ['black', 'blue'], $
	          LENGTH    = 0, $
	          LOCATION  = [p[2,0], p[3,0]]
	
	;Update the windwo
	win_gsm -> SetProperty, NOEXECUTECOMMANDS=0, /UPDATE

end
