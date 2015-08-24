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
function mms_fdoa_test_interp_ephem, sc, tstart, tend
	compile_opt strictarr
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 && obj_valid(win) then obj_destroy, win
		void = cgErrorMSG(/QUIET)
		return, !Null
	endif
	
	;Directories
	if n_elements(sc)     eq 0 then sc     = 'mms2'
	if n_elements(tstart) eq 0 then tstart = '2015-07-22T09:00:00Z'
	if n_elements(tend)   eq 0 then tend   = '2015-07-22T17:00:00Z'
	eph_dir = filepath('', ROOT_DIR='/nfs', SUBDIRECTORY=['ancillary', sc, 'defeph'])

;-------------------------------------------------------
; Find Files ///////////////////////////////////////////
;-------------------------------------------------------
	instr   = 'dfg'
	mode    = 'srvy'
	level   = 'ql'
	optdesc = ''

	;FG files
	files_fg = mms_find_file(sc, instr, mode, level, $
	                          COUNT     = count, $
	                          OPTDESC   = optdesc, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	if count eq 0 then message, 'Unable to find FG file "' + searchstr + '".'

	;Attitude files
	eph_ftest = filepath( ROOT_DIR=eph_dir, strupcase(sc) + '_DEFEPH_%Y%D_%Y%D.V*' )
	files_defeph = MrFile_Search(eph_ftest, /CLOSEST, $
	                             COUNT     = count, $
	                             TIMEORDER = '%Y%D', $
	                             TSTART    = tstart, $
	                             TEND      = tend, $
	                             VREGEX    = 'V([0-9]{2})')
	if count eq 0 then message, 'Unable to find DEFEPH file "' + eph_ftest + '".'

;-------------------------------------------------------
; Read Data ////////////////////////////////////////////
;-------------------------------------------------------

	;Read data
	ephem = mms_fdoa_read_defeph(files_defeph, tstart, tend)
	fg_ql = mms_fg_read_ql(files_fg, tstart, tend)
	
	;Interpolate ephemeris data onto FG time stamps.
	r = mms_fg_xinterp_ephem(ephem.tt2000, ephem.position, ephem.velocity, fg_ql.tt2000, v)

;-------------------------------------------------------
; Plot Results /////////////////////////////////////////
;-------------------------------------------------------
	;MMS Colors
	blue  = [ 86, 180, 233]
	green = [  0, 158, 115]
	red   = [213,  94,   0]

	;Conver time to seconds since midnight
	MrCDF_Epoch, fg_ql.tt2000, yr, mo, day
	MrCDF_Epoch, t0, yr, mo, day
	t_fg   = MrCDF_epoch2sse(fg_ql.tt2000, t0)
	t_eph  = MrCDF_epoch2sse(ephem.tt2000, t0)
	rrange = [min(r, MAX=rmax), rmax]
	vrange = [min(v, MAX=vmax), vmax]

	;Create a window
	win = MrWindow(OXMARGIN=[13,4], YGAP=0.5, REFRESH=0)
	
	;Plot position
	Pri = MrPlot( t_fg, r, $
	              /CURRENT, $
	              DIMENSION   = 2, $
	              LAYOUT      = [2,2,1], $
	              NAME        = 'R Interp', $
	              XTICKFORMAT = '(a1)', $
	              YRANGE      = rrange, $
	              YTITLE      = 'R Interp!C(km)' )
	              
	Pre = MrPlot( t_eph, ephem.position, $
	              /CURRENT, $
	              DIMENSION   = 2, $
	              LAYOUT      = [2,2,3], $
	              NAME        = 'R', $
	              XTICKFORMAT = 'time_labels', $
	              YRANGE      = rrange, $
	              YTITLE      = 'R!C(km)' )

	;Plot velocity
	Pvx = MrPlot( t_fg, v, $
	              /CURRENT, $
	              DIMENSION   = 2, $
	              LAYOUT      = [2,2,2], $
	              NAME        = 'V Interp', $
	              XTICKFORMAT = '(a1)', $
	              YTITLE      = 'V Interp!C(km/s)' )
	              
	Pvx = MrPlot( t_eph, ephem.velocity, $
	              /CURRENT, $
	              DIMENSION   = 2, $
	              LAYOUT      = [2,2,4], $
	              NAME        = 'V', $
	              XTICKFORMAT = 'time_labels', $
	              YTITLE      = 'V!C(km/s)' )

	win -> Refresh
	return, win
end
