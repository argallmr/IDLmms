; docformat = 'rst'
;
; NAME:
;    mms_ql_4sc_befields
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
function mms_ql_4sc_befields, tstart, tend, $
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
	
	;Create a window
	mms_colors = ['Black', 'Green', 'Blue', 'Red']
	win = MrWindow(OXMARGIN=[10,8], YSIZE=600, YGAP=0.5, REFRESH=0, BUFFER=save_dir ne '')
	nplots = 0
	
	;Midnight on TSTART
	MrTimeParser, [tstart, tend], '%Y-%M-%dT%H:%m:%S%z', $
	              YEAR = year, MONTH  = month,  DAY    = day, $
	              HOUR = hour, MINUTE = minute, SECOND = second
	MrCDF_Epoch, trange, fix(year), fix(month), fix(day), fix(hour), fix(minute), fix(second), /TT2000, /COMPUTE_EPOCH
	MrCDF_Epoch, t0,     fix(year[0]), fix(month[0]), fix(day[0]), /TT2000, /COMPUTE_EPOCH
	
	;Create a title
	title = strmid(tstart, 0, 10)
	xrange = MrCDF_epoch2sse(trange, t0)

	;Search for files
	for isc = 1, 4 do begin
		;Spacecraft
		sc    = 'mms' + strtrim(isc, 2)
		color = mms_colors[isc-1]

	;-------------------------------------------------------
	; DFG //////////////////////////////////////////////////
	;-------------------------------------------------------
		instr   = 'dfg'
		mode    = 'srvy'
		level   = 'ql'
		optdesc = ''
		
		;Search for files
		files = mms_find_file(sc, instr, mode, level, $
		                      COUNT     = nfiles, $
		                      OPTDESC   = optdesc, $
		                      SDC_ROOT  = sdc_dir, $
		                      SEARCHSTR = searchstr, $
		                      TSTART    = tstart, $
		                      TEND      = tend)
		
		;Read data files
		if nfiles gt 0 then begin
			nodata = 0
			fg_ql = mms_fg_read_ql(files, TSTART=tstart, TEND=tend)
		endif

		;Fake data or real data?
		if nfiles eq 0 || fg_ql eq !Null then begin
			nodata = 1
			fg_ql = { tt2000: trange, $
			          B_dmpa: rebin(findgen(1,2), 4, 2) }
		endif
			
		;Convert to seconds
		t_fg  = MrCDF_epoch2sse(fg_ql.tt2000,  t0)
	
		;Limit range
		yrange = -150 > [min(fg_ql.B_dmpa, MAX=ymax, /NAN), ymax] < 150

		;|B|
		pDFGm = MrPlot(t_fg, fg_ql.B_dmpa[3,*], /CURRENT, $
		               COLOR         = color, $
		               NAME          = sc + ' |B| DFG', $
		               NODATA        = nodata, $
		               OVERPLOT      = pDFGm, $
		               TITLE         = title, $
		               XRANGE        = xrange, $
		               XTICKFORMAT   = '(a1)', $
		               YRANGE        = yrange, $
		               YTITLE        = '|B|!C(nT)')

		;Bx
		pDFGx = MrPlot(t_fg, fg_ql.B_dmpa[0,*], /CURRENT, $
		               COLOR         = color, $
		               NAME          = sc + ' Bx DFG', $
		               NODATA        = nodata, $
		               OVERPLOT      = pDFGx, $
		               XRANGE        = xrange, $
		               XTICKFORMAT   = '(a1)', $
		               YRANGE        = yrange, $
		               YTITLE        = 'B$\downX$!C(nT)')

		;By
		pDFGy = MrPlot(t_fg, fg_ql.B_dmpa[1,*], /CURRENT, $
		               COLOR         = color, $
		               NAME          = sc + ' By DFG', $
		               NODATA        = nodata, $
		               OVERPLOT      = pDFGy, $
		               XRANGE        = xrange, $
		               XTICKFORMAT   = '(a1)', $
		               YRANGE        = yrange, $
		               YTITLE        = 'B$\downY$!C(nT)')

		;Bz
		pDFGz = MrPlot(t_fg, fg_ql.B_dmpa[2,*], /CURRENT, $
		               COLOR         = color, $
		               NAME          = sc + ' Bz DFG', $
		               NODATA        = nodata, $
		               OVERPLOT      = pDFGz, $
		               XRANGE        = xrange, $
		               XTICKFORMAT   = '(a1)', $
		               YRANGE        = yrange, $
		               YTITLE        = 'B$\downZ$!C(nT)')

	;-------------------------------------------------------
	; EDP //////////////////////////////////////////////////
	;-------------------------------------------------------
		instr   = 'edp'
		mode    = 'comm'
		level   = 'ql'
		optdesc = 'dce2d'

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

		;Filler data
		if nfiles_edp eq 0 ||edp_ql eq !Null then begin
			nodata = 1
			edp_ql = { tt2000: trange, $
			           E_dsl:  rebin(findgen(1,2), 3, 2) }
		endif

		;Convert to seconds
		t_edp = MrCDF_epoch2sse(edp_ql.tt2000, t0)
		
		;Replace fill value
		edp_ql.E_dsl = replace_fillval(edp_ql.E_dsl, -1e31)
		
		;Ex
		pEDPx = MrPlot(t_edp, edp_ql.E_dsl[0,*], /CURRENT, $
		               COLOR         = color, $
		               DIMENSION     = 2, $
		               NAME          = sc + ' Ex EDP', $
		               NODATA        = nodata, $
		               OVERPLOT      = pEDPx, $
		               XRANGE        = xrange, $
		               XTICKFORMAT   = '(a1)', $
		               YTITLE        = 'E$\downX$!C(mV/m)')

		;Ey
		pEDPy = MrPlot(t_edp, edp_ql.E_dsl[1,*], /CURRENT, $
		               COLOR         = color, $
		               NAME          = sc + ' Ey EDP', $
		               NODATA        = nodata, $
		               OVERPLOT      = pEDPz, $
		               XRANGE        = xrange, $
		               XTICKFORMAT   = '(a1)', $
		               YTITLE        = 'E$\downY$!C(mV/m)')

		;Ez
		pEDPz = MrPlot(t_edp, edp_ql.E_dsl[2,*], /CURRENT, $
		               COLOR         = color, $
		               NAME          = sc + ' Ez EDP', $
		               NODATA        = nodata, $
		               OVERPLOT      = pEDPz, $
		               XRANGE        = xrange, $
		               XTICKFORMAT   = 'time_labels', $
		               XTITLE        = 'Time (UT)', $
		               YTITLE        = 'E$\downZ$!C(mV/m)')
	endfor
			
	lDFG = MrLegend(ALIGNMENT        = 'NW', $
	                LABEL            = ['mms1', 'mms2', 'mms3', 'mms4'], $
	                NAME             = 'SC Legend', $
	                POSITION         = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH     = 0.0, $
	                TARGET           = pDFGm, $
	                TEXT_COLOR       = mms_colors, $
	                VERTICAL_SPACING = 1.5)
	
;-------------------------------------------------------
; Set Ranges ///////////////////////////////////////////
;-------------------------------------------------------
	
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
