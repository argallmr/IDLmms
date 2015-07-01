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
;   Create a quick-look plot of the electric fields.
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
function mms_sdc_ql_Efields, sc, tstart, tend
	compile_opt strictarr
	on_error, 2

; MMS2: May 9, 2015  16:08 - 16:13
; MMS4: May 6, 2015  15:30 - 15:35


	;General inputs
	sc      = 'mms2'
	tstart  = '2015-05-09T16:08:00Z'
	tend    = '2015-05-09T16:13:00Z'
	
	sdc_dir = '/nfs/mmsa/sdc/'
	edi_dir = '/home/argall/data/mms/edi/'

;-------------------------------------------------------
; EDI //////////////////////////////////////////////////
;-------------------------------------------------------
	instr   = 'edi'
	mode    = 'slow'
	level   = 'ql'
	optdesc = 'efield-5min'
	
	;Search for file
	files_edi = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles, $
	                          DIRECTORY = edi_dir, $
	                          OPTDESC   = optdesc, $
;	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TIMEORDER = '%Y%M%d%H%m%S', $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	if nfiles eq 0 then message, 'No EDI file found: "' + searchstr + '".'

	;Read files
	edi_ql = mms_edi_read_ql_efield(files_edi, TSTART=tstart, TEND=tend)

	;Remove fill values
	edi_ql.e_dmpa = replace_fillval(edi_ql.e_dmpa, 1e-31)

;-------------------------------------------------------
; EDP //////////////////////////////////////////////////
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
	if nfiles eq 0 then message, 'No EDP file found: "' + search_str + '".'

	;Read files
	edp_ql = mms_edp_read_ql(files_edp, TSTART=tstart, TEND=tend)

;-------------------------------------------------------
; Plot /////////////////////////////////////////////////
;-------------------------------------------------------
	;Create a title
	title = strupcase(sc) + ' ' + strmid(tstart, 0, 10)
	
	;Convert time to seconds since midnight
	MrCDF_Epoch, edi_ql.tt2000[0], yr, mo, day, /BREAKDOWN_EPOCH
	MrCDF_Epoch, t0, yr, mo, day, /COMPUTE_EPOCH
	t_edi = MrCDF_epoch2sse(edi_ql.tt2000, t0)
	t_edp = MrCDF_epoch2sse(edp_ql.tt2000, t0)
	
	Exrange = [min( [[edp_ql.E_dsl[0,*]], [edi_ql.E_dmpa[0,*]]], MAX=xmax ), xmax]
	Eyrange = [min( [[edp_ql.E_dsl[1,*]], [edi_ql.E_dmpa[1,*]]], MAX=ymax ), ymax]
	Ezrange = [min( [[edp_ql.E_dsl[2,*]], [edi_ql.E_dmpa[2,*]]], MAX=zmax ), zmax]
	
	;Create a window
	win = MrWindow(REFRESH=0, YSIZE=600)

	;X-EDP
	pEDPx = MrPlot(t_edp, edp_ql.E_dsl[0,*], /CURRENT, $
	               DIMENSION   = 2, $
	               COLOR       = 'Black', $
	               NAME        = 'Ex EDP', $
	               TITLE       = title, $
	               XTITLE      = 'Time (UT)', $
	               XTICKFORMAT = 'time_labels', $
	               YRANGE      = Exrange, $
	               YTITLE      = 'E$\downX$!C(mV/m)')

	;Y-EDP
	pEDPy = MrPlot(t_edp, edp_ql.E_dsl[1,*], /CURRENT, $
	               DIMENSION   = 2, $
	               COLOR       = 'Black', $
	               NAME        = 'Ey EDP', $
	               TITLE       = title, $
	               XTITLE      = 'Time (UT)', $
	               XTICKFORMAT = 'time_labels', $
	               YRANGE      = Eyrange, $
	               YTITLE      = 'E$\downY$!C(mV/m)')

	;Z-EDP
	pEDPz = MrPlot(t_edp, edp_ql.E_dsl[2,*], /CURRENT, $
	               DIMENSION   = 2, $
	               COLOR       = 'Black', $
	               NAME        = 'Ez EDP', $
	               TITLE       = title, $
	               XTITLE      = 'Time (UT)', $
	               XTICKFORMAT = 'time_labels', $
	               YRANGE      = Ezrange, $
	               YTITLE      = 'E$\downZ$!C(mV/m)')

	;Plot the data
	pEDIX = MrPlot(t_edi, edi_ql.E_dmpa[0,*], $
	               DIMENSION   = 2, $
	               COLOR       = 'Blue', $
	               NAME        = 'E EDI', $
	               OVERPLOT    = pEDPx, $
	               PSYM        = 2, $
	               TITLE       = title, $
	               XTITLE      = 'Time (UT)', $
	               XTICKFORMAT = 'time_labels', $
	               YTITLE      = 'E$\downEDI$!C(mV/m)')

	;Plot the data
	pEDIY = MrPlot(t_edi, edi_ql.E_dmpa[1,*], $
	               DIMENSION   = 2, $
	               COLOR       = 'Blue', $
	               NAME        = 'E EDI', $
	               OVERPLOT    = pEDPy, $
	               PSYM        = 2, $
	               TITLE       = title, $
	               XTITLE      = 'Time (UT)', $
	               XTICKFORMAT = 'time_labels', $
	               YTITLE      = 'E$\downEDI$!C(mV/m)')

	;Plot the data
	pEDIZ = MrPlot(t_edi, edi_ql.E_dmpa[2,*], $
	               DIMENSION   = 2, $
	               COLOR       = 'Blue', $
	               NAME        = 'E EDI', $
	               OVERPLOT    = pEDPz, $
	               PSYM        = 2, $
	               TITLE       = title, $
	               XTITLE      = 'Time (UT)', $
	               XTICKFORMAT = 'time_labels', $
	               YTITLE      = 'E$\downEDI$!C(mV/m)')
	
	;Create a legend
	lEDP = MrLegend(ALIGNMENT        = 'NE', $
	                LABEL            = ['EDP', 'EDI'], $
	                NAME             = 'EDP Legend', $
	                POSITION         = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH     = 0.0, $
	                TARGET           = [pEDPx, pEDIx], $
	                TEXT_COLOR       = ['Black', 'Blue'], $
	                VERTICAL_SPACING = 1.5)
	
	win -> Refresh
	return, win
end
