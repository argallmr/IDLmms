; docformat = 'rst'
;
; NAME:
;    mms_sdc_ql_BEfields
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
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
function mms_ql_edi_efield, sc, tstart, tend, $
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
	sdc_dir = '/nfs/'
	edi_dir = '/nfs/edi/'
	
	;Create a window
	win = MrWindow(OXMARGIN=[10,6], YSIZE=600, YGAP=0.5, REFRESH=0, BUFFER=save_dir ne '')
	nplots = 0
	
	;Midnight on TSTART
	MrTimeParser, [tstart, tend], '%Y-%M-%dT%H:%m:%S%z', $
	              YEAR = year, MONTH  = month,  DAY    = day, $
	              HOUR = hour, MINUTE = minute, SECOND = second
	MrCDF_Epoch, trange, fix(year), fix(month), fix(day), fix(hour), fix(minute), fix(second), /TT2000, /COMPUTE_EPOCH
	MrCDF_Epoch, t0,     fix(year[0]), fix(month[0]), fix(day[0]), /TT2000, /COMPUTE_EPOCH
	
	;Create a title
	title = strupcase(sc) + ' ' + strmid(tstart, 0, 10)
	xrange = MrCDF_epoch2sse(trange, t0)

;-------------------------------------------------------
; DFG //////////////////////////////////////////////////
;-------------------------------------------------------
	instr   = 'dfg'
	mode    = 'srvy'
	level   = 'ql'
	optdesc = ''

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
	t_fg  = MrCDF_epoch2sse(fg_ql.tt2000,  t0)
	
	;Filter out range-change blip approaching perigee
	ib = where(fg_ql.B_dmpa[3,*] gt 2000, nb)
	if nb gt 0 then fg_ql.B_dmpa[*,ib] = !values.f_nan
	
	;Limit range
	fg_ql.B_dmpa = fg_ql.B_dmpa[[3,0,1,2], *]
	yrange = -150 > [min(fg_ql.B_dmpa, MAX=ymax, /NAN), ymax] < 150

	;Plot the data
	pDFG = MrPlot(t_fg, fg_ql.B_dmpa, /CURRENT, $
	              DIMENSION     = 2, $
	              COLOR         = ['Black', 'Blue', 'Forest Green', 'Red'], $
	              NAME          = 'B DFG', $
	              NODATA        = nodata, $
	              TITLE         = title, $
	              XRANGE        = xrange, $
	              XTICKFORMAT   = '(a1)', $
	              YRANGE        = yrange, $
	              YTITLE        = 'B$\downDFG$!C(nT)')

	;normalize the magnetic field
	b_hat = MrVector_Normalize(fg_ql.B_dmpa[1:3,*])
	angle = (!pi/2.0 - acos(b_hat[2,*])) * !radeg

	;Angle between B and Spin-plane
	pDFGa = MrPlot(t_fg, angle, /CURRENT, $
	               NAME          = 'B Spin-Plane Angle', $
	               NODATA        = nodata, $
	               TITLE         = title, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YTITLE        = 'B$\downAngle$ Spin-Plane!C(Deg)')
	
	;Create a legend
	lDFG = MrLegend(ALIGNMENT        = 'NW', $
	                LABEL            = ['|B|', 'B$\downX$', 'B$\downY$', 'B$\downZ$'], $
	                NAME             = 'FG Legend', $
	                POSITION         = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH     = 0.0, $
	                TARGET           = pDFG, $
	                TEXT_COLOR       = ['Black', 'Blue', 'Forest Green', 'Red'], $
	                VERTICAL_SPACING = 1.5)

;-------------------------------------------------------
; EDP //////////////////////////////////////////////////
;-------------------------------------------------------
	instr   = 'edp'
	mode    = 'comm'
	level   = 'ql'
	optdesc = 'dce2d'

	;Search for the file
	files_edp = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles_edp, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TIMEORDER = '%Y%M%d%H%m%S', $
	                          TSTART    = tstart, $
	                          TEND      = tend)

	;Read data files
	if nfiles_edp gt 0 then begin
		nodata = 0
		edp_ql = mms_edp_read_ql(files_edp, TSTART=tstart, TEND=tend)
	endif
	
	;Use filler data?
	if nfiles_edp eq 0 || edp_ql eq !Null then begin
		nodata = 1
		edp_ql = { tt2000: trange, $
		           E_dsl:  rebin(findgen(1,2), 3, 2) }
	endif

	;Remove fill values
	edp_ql.e_dsl = replace_fillval(edp_ql.e_dsl, -1e31)

	;Convert to seconds
	t_edp = MrCDF_epoch2sse(edp_ql.tt2000, t0)
	
	;Ex
	pEDPx = MrPlot(t_edp, edp_ql.E_dsl[0,*], /CURRENT, $
	               DIMENSION     = 2, $
	               NAME          = 'Ex EDP', $
	               NODATA        = nodata, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YTITLE        = 'E$\downX$!C(mV/m)')

	;Ey
	pEDPy = MrPlot(t_edp, edp_ql.E_dsl[1,*], /CURRENT, $
	               NAME          = 'Ey EDP', $
	               NODATA        = nodata, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YTITLE        = 'E$\downY$!C(mV/m)')

	;Ez
	pEDPz = MrPlot(t_edp, edp_ql.E_dsl[2,*], /CURRENT, $
	               NAME          = 'Ez EDP', $
	               NODATA        = nodata, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = 'time_labels', $
	               XTITLE        = 'Time (UT)', $
	               YTITLE        = 'E$\downZ$!C(mV/m)')

;-------------------------------------------------------
; EDI //////////////////////////////////////////////////
;-------------------------------------------------------
	instr   = 'edi'
	mode    = 'slow'
	level   = 'ql'
	optdesc = 'efield'
	
	;Search for file
	files_edi = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles_edi, $
	                          DIRECTORY = edi_dir, $
	                          OPTDESC   = optdesc, $
;	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	
	;Create a null data set to use as filler
	null_data = { tt2000:     trange, $
	              B_dmpa:     rebin(findgen(1,2), 3, 2), $
	              E_dmpa:     rebin(findgen(1,2), 3, 2), $
	              v_ExB_dmpa: rebin(findgen(1,2), 3, 2) }

	;Read in the data
	if nfiles_edi gt 0 then begin
		nodata = 0
		edi_ql = mms_edi_read_ql_efield(files_edi, TSTART=tstart, TEND=tend)
	endif
		
	;No data found?
	if nfiles eq 0 || edi_ql eq !Null then begin
		nodata = 1
		edi_ql = { tt2000:     trange, $
		           B_dmpa:     rebin(findgen(1,2), 3, 2), $
		           E_dmpa:     rebin(findgen(1,2), 3, 2), $
		           v_ExB_dmpa: rebin(findgen(1,2), 3, 2) }
	endif

	;Remove fill values
	edi_ql.e_dmpa = replace_fillval(edi_ql.e_dmpa, -1e31)

	;Convert to seconds
	t_edi = MrCDF_epoch2sse(edi_ql.tt2000, t0)

	;B
	pEDIx = MrPlot(t_edi, edi_ql.B_dmpa, $
	               CURRENT       = nfiles_edp eq 0, $
	               COLOR         = ['Blue', 'Forest Green', 'Red'], $
	               DIMENSION     = 2, $
	               NAME          = 'B EDI', $
	               NODATA        = nodata, $
	               OVERPLOT      = pDFG, $
	               PSYM          = 5, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YTITLE        = 'B$\downEDI$!C(mV/m)')

	;Ex
	pEDIx = MrPlot(t_edi, edi_ql.E_dmpa[0,*], $
	               CURRENT       = nfiles_edp eq 0, $
	               COLOR         = 'Blue', $
	               NAME          = 'Ex EDI', $
	               NODATA        = nodata, $
	               OVERPLOT      = pEDPx, $
	               PSYM          = 2, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YTITLE        = 'E$\downX$!C(mV/m)')

	;Ey
	pEDIy = MrPlot(t_edi, edi_ql.E_dmpa[1,*], $
	               CURRENT       = nfiles_edp eq 0, $
	               COLOR         = 'Blue', $
	               NAME          = 'Ey EDI', $
	               NODATA        = nodata, $
	               OVERPLOT      = pEDPy, $
	               PSYM          = 2, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YTITLE        = 'E$\downX$!C(mV/m)')

	;Ez
	pEDIz = MrPlot(t_edi, edi_ql.E_dmpa[2,*], $
	               CURRENT       = nfiles_edp eq 0, $
	               COLOR         = 'Blue', $
	               NAME          = 'Ez EDI', $
	               NODATA        = nodata, $
	               OVERPLOT      = pEDPz, $
	               PSYM          = 2, $
	               XRANGE        = xrange, $
	               XTITLE        = 'Time (UT)', $
	               XTICKFORMAT   = nfiles_edp eq 0 ? 'time_labels' : '(a1)', $
	               YTITLE        = 'E$\downX$!C(mV/m)')

;-------------------------------------------------------
; Make Pretty //////////////////////////////////////////
;-------------------------------------------------------
	;Set the time-range
	xrange = [t_edi[0], t_edi[-1]]
	win -> SetGlobal, XRANGE=xrange

	;Pick data only within specified time range
	ix_fg  = MrIndexRange(t_fg,  xrange)
	ix_edp = MrIndexRange(t_edp, xrange)

	;Search for range
	B_range  = [min( fg_ql.B_dmpa[*,ix_fg[0]:ix_fg[1]], MAX=xmax ), xmax]
	Ex_range = [min( [[edi_ql.E_dmpa[0,*]], [edp_ql.E_dsl[0,ix_edp[0]:ix_edp[1]]]], /NAN, MAX=xmax ), xmax]
	Ey_range = [min( [[edi_ql.E_dmpa[1,*]], [edp_ql.E_dsl[1,ix_edp[0]:ix_edp[1]]]], /NAN, MAX=xmax ), xmax]
	Ez_range = [min( [[edi_ql.E_dmpa[2,*]], [edp_ql.E_dsl[2,ix_edp[0]:ix_edp[1]]]], /NAN, MAX=xmax ), xmax]

	;Set Range
	pDFG.YRANGE  = -75 > B_range       < 75
	pEDPx.YRANGE = -25 > Ex_range*1.07 < 25
	pEDPy.YRANGE = -25 > Ey_range*1.07 < 25
	pEDPz.YRANGE = -25 > Ez_range*1.07 < 25
	
	;Create a legend
	lEfield = MrLegend(ALIGNMENT        = 'NW', $
	                   LABEL            = ['EDP', 'EDI'], $
	                   NAME             = 'E-field Legend', $
	                   POSITION         = [1.0, 1.0], $
	                   /RELATIVE, $
	                   SAMPLE_WIDTH     = 0.0, $
	                   TARGET           = pEDPx, $
	                   TEXT_COLOR       = ['Black', 'Blue'], $
	                   VERTICAL_SPACING = 1.5)

;-------------------------------------------------------
; Save /////////////////////////////////////////////////
;-------------------------------------------------------
	;Save the file?
	if save_dir ne '' && nplots gt 0 then begin
		fname = sc + '_ql_edi_efield_' + year[0] + month[0] + day[0] + hour[0] + minute[0] + second[0] + '.png'
		fname = filepath(fname, ROOT_DIR=save_dir)
		win.SaveAs -> SetProperty, IM_RASTER = 0
		win   -> Refresh
		win   -> Save, fname
	endif else begin
		win -> Refresh
	endelse
	
	return, win
end
