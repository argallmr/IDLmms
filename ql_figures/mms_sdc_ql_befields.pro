; docformat = 'rst'
;
; NAME:
;    mms_sdc_ql_BEfields
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Create a quick-look plot of the magnetic and electric fields.
;
; :Categories:
;    MMS, QL
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
;       2015/03/15  -   Written by Matthew Argall
;-
function mms_sdc_ql_BEfields, sc, tstart, tend, $
BUFFER=buffer, $
SAVE_DIR=save_dir
	compile_opt strictarr
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 && obj_valid(win) then obj_destroy, win
		void = cgErrorMSG(/QUIET)
		return, !Null
	endif
	
	;Directories
	if n_elements(save_dir) eq 0 then save_dir = ''
;	if n_elements(buffer)   eq 0 then buffer   = save_dir ne ''
	sdc_dir = '/nfs/'
	edi_dir = '/nfs/edi/'

;-------------------------------------------------------
; Create Plotting Window ///////////////////////////////
;-------------------------------------------------------
	;Create a window
	win = MrWindow(REFRESH=0, YSIZE=650, YGAP=0.5, BUFFER=buffer)
	nplots = 0
	
	;Midnight on TSTART
	MrTimeParser, [tstart, tend], '%Y-%M-%dT%H:%m:%S%z', $
	              YEAR = year, MONTH  = month,  DAY    = day, $
	              HOUR = hour, MINUTE = minute, SECOND = second
	MrCDF_Epoch, trange, fix(year), fix(month), fix(day), $
	                     fix(hour), fix(minute), fix(second), /TT2000, /COMPUTE_EPOCH
	MrCDF_Epoch, t0, fix(year), fix(month), fix(day), /TT2000, /COMPUTE_EPOCH
	
	;Create a title
	title = strupcase(sc) + ' ' + strmid(tstart, 0, 10)
	xrange = MrCDF_epoch2sse(trange, t0)

;-------------------------------------------------------
; DFG //////////////////////////////////////////////////
;-------------------------------------------------------
	instr = 'dfg'
	mode  = 'srvy'
	level = 'ql'

	;Search for files
	files_dfg = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	
	;Read data files
	if nfiles gt 0 then begin
		nodata = 0
		fg_ql = mms_fg_read_ql(files_dfg, TSTART=tstart, TEND=tend)
	endif

	;Use filler data?
	if nfiles eq 0 || fg_ql eq !Null then begin
		nodata = 1
		fg_ql = { tt2000: trange, $
		          B_dmpa: rebin(findgen(1,2), 4, 2) }
	endif

	;Convert to seconds
	t_fg  = MrCDF_epoch2sse(fg_ql.tt2000, trange[0])
	
	;Limit range
	fg_ql.B_dmpa = fg_ql.B_dmpa[[3,0,1,2], *]
	yrange = [min(fg_ql.B_dmpa, MAX=ymax, /NAN), ymax]
	yrange = -70 > yrange < 70

	;Plot the data
	pDFG = MrPlot(t_fg, fg_ql.B_dmpa, /CURRENT, $
	              DIMENSION     = 2, $
	              COLOR         = ['Black', 'Red', 'Forest Green', 'Blue'], $
	              NAME          = 'B DFG', $
	              NODATA        = nodata, $
	              TITLE         = title, $
	              XRANGE        = xrange, $
	              XTICKFORMAT   = '(a1)', $
	              YRANGE        = yrange, $
	              YTITLE        = 'B$\downDFG$!C(nT)')
	
	;Create a legend
	lDFG = MrLegend(ALIGNMENT        = 'NE', $
	                LABEL            = ['|B|', 'B$\downX$', 'B$\downY$', 'B$\downZ$'], $
	                NAME             = 'FG Legend', $
	                POSITION         = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH     = 0.0, $
	                TARGET           = pDFG, $
	                TEXT_COLOR       = ['Black', 'Red', 'Forest Green', 'Blue'], $
	                VERTICAL_SPACING = 1.5)

;-------------------------------------------------------
; Spacecraft Potential /////////////////////////////////
;-------------------------------------------------------
	instr   = 'edp'
	mode    = 'comm'
	level   = 'l2'
	optdesc = 'scpot'
	
	;Search for file
	files_scp = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TIMEORDER = '%Y%M%d%H%m%S', $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	
	;Read data files
	if nfiles gt 0 then begin
		nodata = 0
		scp_l2 = mms_edp_read_l2_scpot(files_scp, TSTART=tstart, TEND=tend)
	endif

	;Filler data
	if nFiles eq 0 || scp_l2 eq !Null then begin
		nodata = 1
		scp_l2 = { tt2000: trange, $
		           scpot:  [0.0, 1.0] }
	endif

	;Remove fill values
	scp_l2.scpot = replace_fillval(scp_l2.scpot, -1e31)

	;Convert to seconds
	t_scp = MrCDF_epoch2sse(scp_l2.tt2000, trange[0])

	;Plot the data
	pSCP = MrPlot(t_scp, scp_l2.scpot, /CURRENT, $
	              NAME          = 'SC Potential', $
	              NODATA        = nodata, $
	              XRANGE        = xrange, $
	              XTICKFORMAT   = '(a1)', $
	              YTITLE        = 'S/C Pot!C(V)')

;-------------------------------------------------------
; EDI Ambient Mode /////////////////////////////////////
;-------------------------------------------------------
	instr   = 'edi'
	mode    = 'slow'
	level   = 'l1a'
	optdesc = 'amb'
	
	;Search for file
	files_edi = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TIMEORDER = '%Y%M%d', $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	                          
	;Read data files
	if nfiles gt 0 then begin
		nodata  = 0
		edi_amb = mms_edi_read_l1a_amb(files_edi, TSTART=tstart, TEND=tend)
	endif

	;Filler data
	if nFiles eq 0 || edi_amb eq !Null then begin
		nodata = 1
		edi_amb = { epoch_gdu1:  trange, $
		            epoch_gdu2:  trange, $
		            counts_gdu1: [0, 1], $
		            counts_gdu2: [0, 1] }
	endif
		

	;Convert to seconds
	t_gdu1 = MrCDF_epoch2sse(edi_amb.epoch_gdu1, trange[0])
	t_gdu2 = MrCDF_epoch2sse(edi_amb.epoch_gdu2, trange[0])

	;Plot the data
	pEDI1 = MrPlot(t_gdu1, edi_amb.counts_gdu1, /CURRENT, $
	              COLOR         = 'Purple', $
	              NAME          = 'GDU1 Counts', $
	              NODATA        = nodata, $
	              XRANGE        = xrange, $
	              XTICKFORMAT   = '(a1)', $
	              YTITLE        = 'Counts')

	;Plot the data
	pEDI2 = MrPlot(t_gdu2, edi_amb.counts_gdu2, /CURRENT, $
	              COLOR         = 'Orange', $
	              OVERPLOT      = pEDI1, $
	              NAME          = 'GDU2 Counts', $
	              NODATA        = nodata, $
	              XRANGE        = xrange, $
	              XTICKFORMAT   = '(a1)', $
	              YTITLE        = 'Counts')
	
	;Create a legend
	lEDI = MrLegend(ALIGNMENT        = 'NW', $
	                LABEL            = ['GDU1', 'GDU2'], $
	                NAME             = 'EDI Amb Legend', $
	                POSITION         = [0.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH     = 0.0, $
	                TARGET           = [pEDI1, pEDI2], $
	                TEXT_COLOR       = ['Purple', 'Orange'], $
	                VERTICAL_SPACING = 1.5)

;-------------------------------------------------------
; EDI E-Field Mode /////////////////////////////////////
;-------------------------------------------------------
	instr   = 'edi'
	mode    = 'slow'
	level   = 'ql'
	optdesc = 'efield'
	
	;Search for file
	files_edi = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles, $
	                          DIRECTORY = edi_dir, $
	                          OPTDESC   = optdesc, $
;	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TIMEORDER = '%Y%M%d', $
	                          TSTART    = tstart, $
	                          TEND      = tend)

	;Read data files
	if nFiles gt 0 then begin
		nodata = 0
		edi_ql = mms_edi_read_ql_efield(files_edi, TSTART=tstart, TEND=tend)
	endif
	
	;Filler data
	if nFiles eq 0 || edi_ql eq !null then begin
		nodata = 1
		edi_ql = { tt2000:     trange, $
		           E_dmpa:     rebin(findgen(1,2), 3, 2), $
		           v_ExB_dmpa: rebin(findgen(1,2), 3, 2) }
	endif

	;Remove fill values
	edi_ql.e_dmpa     = replace_fillval(edi_ql.e_dmpa,     -1e31)
	edi_ql.v_ExB_dmpa = replace_fillval(edi_ql.v_ExB_dmpa, -1e31)

	;Convert to seconds
	t_edi = MrCDF_epoch2sse(edi_ql.tt2000, trange[0])

	;Overlay on top of Ambient-mode data if possible
	if obj_valid(pEDI1) then begin
		position = pEDI1.position
		xstyle   = 5
		ystyle   = 5
	endif

	;Plot the data
	pEDI_E = MrPlot(t_edi, edi_ql.E_dmpa, /CURRENT, $
	                DIMENSION     = 2, $
	                COLOR         = ['Red', 'Forest Green', 'Blue'], $
	                NAME          = 'E EDI', $
	                NODATA        = nodata, $
	                POSITION      = position, $
	                XRANGE        = xrange, $
	                XSTYLE        = xstyle, $
	                XTICKFORMAT   = '(a1)', $
	                YSTYLE        = ystyle, $
	                YTITLE        = 'E$\downEDI$!C(mV/m)')
	
	;Plot drift velocity data
	pEDI_V = MrPlot(t_edi, edi_ql.v_ExB_dmpa, /CURRENT, $
	                DIMENSION     = 2, $
	                COLOR         = ['Red', 'Forest Green', 'Blue'], $
	                NAME          = 'V_ExB EDI', $
	                NODATA        = nodata, $
	                XRANGE        = xrange, $
	                XTICKFORMAT   = '(a1)', $
	                YTITLE        = 'V$\downExB$!C(km/s)')
	
	;Create an axis
	axEDI = MrAxis( 'Y', $
	                LOCATION = 'Right', $
	                NAME     = 'Axis: EDI E-field', $
	                TARGET   = pEDI_E, $
	                TICKDIR  = 1, $
	                TITLE    = 'E$\downEDI$!C(mV/m)' )
	
	;Create a legend
	lEDI = MrLegend(ALIGNMENT        = 'NE', $
	                LABEL            = ['V$\downX$', 'V$\downY$', 'V$\downZ$'], $
	                NAME             = 'EDI ExB drift Legend', $
	                ORIENTATION      = 1, $
	                POSITION         = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH     = 0.0, $
	                TARGET           = pEDI_V, $
	                TEXT_COLOR       = ['Red', 'Forest Green', 'Blue'], $
	                VERTICAL_SPACING = 1.5)
	
	;Create a legend
	lEDI = MrLegend(ALIGNMENT        = 'NE', $
	                LABEL            = ['E$\downX$', 'E$\downY$', 'E$\downZ$'], $
	                NAME             = 'EDI E-field Legend', $
	                ORIENTATION      = 1, $
	                POSITION         = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH     = 0.0, $
	                TARGET           = pEDI_E, $
	                TEXT_COLOR       = ['Red', 'Forest Green', 'Blue'], $
	                VERTICAL_SPACING = 1.5)

;-------------------------------------------------------
; EDP Electric Field ///////////////////////////////////
;-------------------------------------------------------
	instr   = 'edp'
	mode    = 'comm'
	level   = 'ql'
	optdesc = 'dce2d'

	;Search for the file
	files_edp = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TIMEORDER = '%Y%M%d%H%m%S', $
	                          TSTART    = tstart, $
	                          TEND      = tend)

	;Read data files
	if nfiles gt 0 then begin
		nodata = 0
		edp_ql = mms_edp_read_ql(files_edp, TSTART=tstart, TEND=tend)
	endif
	
	;Use filler data?
	if nfiles eq 0 || edp_ql eq !Null then begin
		nodata = 1
		edp_ql = { tt2000: trange, $
		           E_dsl:  rebin(findgen(1,2), 3, 2) }
	endif
		
	;Remove fill values
	edp_ql.e_dsl = replace_fillval(edp_ql.e_dsl, -1e31)

	;Convert to seconds
	t_edp = MrCDF_epoch2sse(edp_ql.tt2000, trange[0])
	
	;Limit range
	edp_ql.E_dsl = reverse(edp_ql.E_dsl, 1)
	yrange = [min(edp_ql.E_dsl, MAX=ymax, /NAN), ymax]
	yrange = -15 > yrange < 15

	;Plot the data
	pEDP = MrPlot(t_edp, edp_ql.E_dsl, /CURRENT, $
	              DIMENSION     = 2, $
	              COLOR         = ['Red', 'Forest Green', 'Blue'], $
	              NAME          = 'E EDP', $
	              NODATA        = nodata, $
	              XRANGE        = xrange, $
	              XTITLE        = 'Time (UT)', $
	              XTICKFORMAT   = 'time_labels', $
	              YRANGE        = yrange, $
	              YTITLE        = 'E$\downEDP$!C(mV/m)')
	
	;Create a legend
	lEDP = MrLegend(ALIGNMENT        = 'NE', $
	                LABEL            = ['E$\downX$', 'E$\downY$', 'E$\downZ$'], $
	                NAME             = 'EDP Legend', $
	                POSITION         = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH     = 0.0, $
	                TARGET           = pEDP, $
	                TEXT_COLOR       = reverse(['Red', 'Forest Green', 'Blue']), $
	                VERTICAL_SPACING = 1.5)

;-------------------------------------------------------
; Prettify /////////////////////////////////////////////
;-------------------------------------------------------
	
	;Put the EDI E-field plot in the correct position
	win.OXMARGIN = [10,10]
	win -> Refresh
	win -> Refresh, /DISABLE
	pEDI_E.position = pEDI1.position
	
	;Save the file?
	if save_dir ne '' then begin
		fname = sc + '_ql_BEfields_' + year[0] + month[0] + day[0] + hour[0] + minute[0] + second[0] + '.png'
		fname = filepath(fname, ROOT_DIR=save_dir)
		win.SaveAs -> SetProperty, IM_RASTER = 0
		win   -> Refresh
		win   -> Save, fname
	endif else begin
		win -> Refresh
	endelse
	
	return, win
end
