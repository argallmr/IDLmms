; docformat = 'rst'
;
; NAME:
;    mms_test_despin
;
; PURPOSE:
;+
;   Despin data using DSS and FDOA.
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
;-
pro mms_test_despin, sc, tstart, tend
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
	tstart   = '2015-08-15T11:00:00Z'
	tend     = '2015-08-15T16:00:00Z'
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
	zmpa      = atthdr.zmpa
	
	;Alternative method for obtaining data AND zmpa axis
;	defatt    = mms_fdoa_read_defatt(att_file, tstart, tend, HEADER=atthdr)
;	zmpa      = atthdr.zmpa[*,0]

	;Extract magnetic field data of interest
	t      = fgm_l2pre.tt2000
	b_bcs  = fgm_l2pre.b_bcs[0:2,*]
	b_dmpa = fgm_l2pre.b_dmpa[0:2,*]
	
	;Delete leftovers
	fgm_l2pre = !Null

;-------------------------------------------------------
; BCS --> DMPA /////////////////////////////////////////
;-------------------------------------------------------
	
	;Rotate BCS --> SMPA
	bcs2smpa   = mms_fg_xbcs2smpa(zmpa)
	b_unh_smpa = MrVector_Rotate(bcs2smpa, b_bcs)
	
	;Rotate SMPA --> DMPA via DSS
	smpa2dmpa      = mms_dss_xdespin(dss, t)
	b_unh_dmpa_dss = MrVector_Rotate(smpa2dmpa, b_unh_smpa)

;-------------------------------------------------------
; Plot the Results /////////////////////////////////////
;-------------------------------------------------------
	;Convert time to seconds
	t_ssm = MrCDF_epoch2ssm(t)
	
	;Get plot positions
	p = cgLayout([1,3], YGAP=0.5)
	
	;Create a window
	cgWindow
	
	;BX Component
	cgPlot, t_ssm, b_dmpa[0,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,0], $
	        TITLE       = 'B Despun via DSS', $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'B$\downX$!C(nT)'
	cgOPlot, t_ssm, b_unh_dmpa_dss[0,*], /ADDCMD, $
	         COLOR = 'blue'
	
	;BY Component
	cgPlot, t_ssm, b_dmpa[1,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,1], $
	        XTICKFORMAT = '(a1)', $
	        YTITLE      = 'B$\downY$!C(nT)'
	cgOPlot, t_ssm, b_unh_dmpa_dss[1,*], /ADDCMD, $
	         COLOR = 'blue'
	
	;BZ Component
	cgPlot, t_ssm, b_dmpa[2,*], $
	        /ADDCMD, $
	        /NOERASE, $
	        POSITION    = p[*,2], $
	        XTICKFORMAT = 'time_labels', $
	        YTITLE      = 'B$\downZ$!C(nT)'
	cgOPlot, t_ssm, b_unh_dmpa_dss[2,*], /ADDCMD, $
	         COLOR = 'blue'

	;Create a legend
	cgLegend, /ADDCMD, $
	          ALIGNMENT = 1, $
	          TITLES    = ['FGM', 'UNH'], $
	          TCOLORS   = ['black', 'blue'], $
	          LENGTH    = 0, $
	          LOCATION  = [p[2,0], p[3,0]]

end
