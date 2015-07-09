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
function mms_ql_dsp_bepsd, sc, tstart, tend, $
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
	win = MrWindow(REFRESH=0, OXMARGIN=[10,15], YSIZE=650, YGAP=0.5, BUFFER=buffer)
	nplots = 0
	
	;Midnight on TSTART
	MrTimeParser, [tstart, tend], '%Y-%M-%dT%H:%m:%S%z', $
	              YEAR = year, MONTH  = month,  DAY    = day, $
	              HOUR = hour, MINUTE = minute, SECOND = second
	MrCDF_Epoch, trange, fix(year), fix(month), fix(day), $
	                     fix(hour), fix(minute), fix(second), /TT2000, /COMPUTE_EPOCH
	
	;Create a title
	title = strupcase(sc) + ' ' + strmid(tstart, 0, 10)
	xrange = MrCDF_epoch2sse(trange, trange[0])

;-------------------------------------------------------
; DSP B PSD ////////////////////////////////////////////
;-------------------------------------------------------
	instr   = 'dsp'
	mode    = 'fast'
	level   = 'l2'
	optdesc = 'bpsd'

	;Search for files
	files_dsp = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	
	;Read data files
	if nfiles gt 0 then begin
		nodata  = 0
		bpsd_l2 = mms_dsp_read_l2_bpsd(files_dsp, TSTART=tstart, TEND=tend)
	endif

	;Use filler data?
	if nfiles eq 0 || bpsd_l2 eq !Null then begin
		nodata  = 1
		bpsd_l2 = { tt2000: trange, $
		            freq:   [1, 10], $
		            b1_psd: replicate(-1e31, 2, 2), $
		            b2_psd: replicate(-1e31, 2, 2), $
		            b3_psd: replicate(-1e31, 2, 2) $
		          }
	endif

	;Data range
;	tmp  = [[bpsd_l2.b1_psd], [bpsd_l2.b2_psd], [bpsd_l2.b3_psd]]
;	ipos = where(tmp gt 0, npos)
;	if npos gt 0 then range = [ min( temporary(tmp), /NAN, MAX=bmax ), bmax ]

	;Convert to seconds
	t_dsp  = MrCDF_epoch2sse(bpsd_l2.tt2000, trange[0])
	
	;B1 PSD
	imB1 = MrImage(bpsd_l2.b1_psd, t_dsp, bpsd_l2.freq, /CURRENT, $
	               /AXES, $
	               /LOG, $
	               /SCALE, $
	               /YLOG, $
	               CTINDEX       = 13, $
	               MISSING_VALUE = -1e31, $
	               MISSING_COLOR = 'white', $
	               NAME          = 'B1 DSP', $
;	               RANGE         = range, $
	               TITLE         = title, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YRANGE        = yrange, $
	               YTITLE        = 'f!C(Hz)')
	
	;B2 PSD
	imB2 = MrImage(bpsd_l2.b2_psd, t_dsp, bpsd_l2.freq, /CURRENT, $
	               /AXES, $
	               /LOG, $
	               /SCALE, $
	               /YLOG, $
	               CTINDEX       = 13, $
	               MISSING_VALUE = -1e31, $
	               MISSING_COLOR = 'white', $
	               NAME          = 'B2 DSP', $
	               RANGE         = range, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YRANGE        = yrange, $
	               YTITLE        = 'f!C(Hz)')
	
	;B3 PSD
	imB3 = MrImage(bpsd_l2.b3_psd, t_dsp, bpsd_l2.freq, /CURRENT, $
	               /AXES, $
	               /LOG, $
	               /SCALE, $
	               /YLOG, $
	               CTINDEX       = 13, $
	               MISSING_VALUE = -1e31, $
	               MISSING_COLOR = 'white', $
	               NAME          = 'B3 DSP', $
	               RANGE         = range, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YRANGE        = yrange, $
	               YTITLE        = 'f!C(Hz)')
	               
	b1_pos = imB1.position
	b3_pos = imB3.position
	cb_pos = [b3_pos[2]+0.01,  0.5*(b3_pos[1]+b3_pos[3]), $
	          b3_pos[2]+0.025, 0.5*(b1_pos[1]+b1_pos[3])]

	;Create a legend
	cbB = MrColorbar( CTINDEX  = 13, $
	                  POSITION = cb_pos, $
	                  NAME     = 'CB B PSD', $
	                  RANGE    = range, $
	                  /RIGHT, $
	                  TARGET   = imB2, $
	                  YTITLE   = 'Power!CnT$\up2$/Hz', $
	                  /VERTICAL )

;For some reason, creating a colorbar leaves REFRESH on.
win -> Refresh, /DISABLE
;-------------------------------------------------------
; DSP B PSD ////////////////////////////////////////////
;-------------------------------------------------------
	instr   = 'dsp'
	mode    = 'fast'
	level   = 'l2'
	optdesc = 'epsd'

	;Search for files
	files_dsp = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = nfiles, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	
	;Read data files
	if nfiles gt 0 then begin
		nodata  = 0
		edsp_l2 = mms_dsp_read_l2_epsd(files_dsp, TSTART=tstart, TEND=tend)
	endif

	;Use filler data?
	if nfiles eq 0 || epsd_l2 eq !Null then begin
		nodata  = 1
		epsd_l2 = { tt2000: trange, $
		            freq:   [1, 10], $
		            e1_psd: replicate(-1e31, 2, 2), $
		            e2_psd: replicate(-1e31, 2, 2), $
		            e3_psd: replicate(-1e31, 2, 2) $
		          }
	endif
	
	;Replace fill value
	epsd_l2.e1_psd = replace_fillval(epsd_l2.e1_psd, -1e31)
	epsd_l2.e2_psd = replace_fillval(epsd_l2.e2_psd, -1e31)
	epsd_l2.e3_psd = replace_fillval(epsd_l2.e3_psd, -1e31)

	;Data range
;	tmp   = [[epsd_l2.e1_psd], [epsd_l2.e2_psd], [epsd_l2.e3_psd]]
;	ipos  = where(tmp gt 0, npos)
;	if npos gt 0 $
;		then range = [ min( temporary(tmp[ipos]), /NAN, MAX=bmax ), bmax ] $
;		else range = !Null

	;Convert to seconds
	t_dsp  = MrCDF_epoch2sse(epsd_l2.tt2000, trange[0])

	;E1 PSD
	imE1 = MrImage(epsd_l2.e1_psd, t_dsp, epsd_l2.freq, /CURRENT, $
	               /AXES, $
	               /LOG, $
	               /SCALE, $
	               /YLOG, $
	               CTINDEX       = 13, $
	               MISSING_VALUE = -1e31, $
	               MISSING_COLOR = 'white', $
	               NAME          = 'E1 DSP', $
;	               RANGE         = range, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YRANGE        = yrange, $
	               YTITLE        = 'f!C(Hz)')
	
	;E2 PSD
	imE2 = MrImage(epsd_l2.e2_psd, t_dsp, epsd_l2.freq, /CURRENT, $
	               /AXES, $
	               /LOG, $
	               /SCALE, $
	               /YLOG, $
	               CTINDEX       = 13, $
	               MISSING_VALUE = -1e31, $
	               MISSING_COLOR = 'white', $
	               NAME          = 'E2 DSP', $
	               RANGE         = range, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = '(a1)', $
	               YRANGE        = yrange, $
	               YTITLE        = 'f!C(Hz)')
	
	;E3 PSD
	imE3 = MrImage(epsd_l2.e3_psd, t_dsp, epsd_l2.freq, /CURRENT, $
	               /AXES, $
	               /LOG, $
	               /SCALE, $
	               /YLOG, $
	               CTINDEX       = 13, $
	               MISSING_VALUE = -1e31, $
	               MISSING_COLOR = 'white', $
	               NAME          = 'E3 DSP', $
	               RANGE         = range, $
	               XRANGE        = xrange, $
	               XTICKFORMAT   = 'time_labels', $
	               YRANGE        = yrange, $
	               YTITLE        = 'f!C(Hz)')
	               
	e1_pos = imE1.position
	e3_pos = imE3.position
	cb_pos = [e3_pos[2]+0.01,  0.5*(e3_pos[1]+e3_pos[3]), $
	          e3_pos[2]+0.025, 0.5*(e1_pos[1]+e1_pos[3])]
	
	;Create a legend
	cbE = MrColorbar( CTINDEX  = 13, $
	                  POSITION = cb_pos, $
	                  RANGE    = range, $
	                  TARGET   = imE2, $
	                  YTITLE   = 'Power!C(mV/m)$\up2$/Hz', $
	                  /VERTICAL )

;-------------------------------------------------------
; Prettify /////////////////////////////////////////////
;-------------------------------------------------------
	
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
