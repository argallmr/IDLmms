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
;       2015/11/17  -   Plot each component in a separate panel.
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

	;Ephemeris files
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
	;   - MUST pass in an Nx1 vector
	r = mms_fg_xinterp_ephem(ephem.tt2000, ephem.position, ephem.velocity, reform(fg_ql.tt2000), v)

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

	;Create a window
	win = MrWindow(LAYOUT=[2,3], OXMARGIN=[13,4], YGAP=0.5, REFRESH=0)

	rxrange = [min(r[0,*], MAX=rmax), rmax]
	ryrange = [min(r[1,*], MAX=rmax), rmax]
	rzrange = [min(r[2,*], MAX=rmax), rmax]
	rxrange += abs(rxrange) * [-0.1, 0.1]
	ryrange += abs(ryrange) * [-0.1, 0.1]
	rzrange += abs(rzrange) * [-0.1, 0.1]

	vxrange = [min(v[0,*], MAX=vmax), vmax]
	vyrange = [min(v[1,*], MAX=vmax), vmax]
	vzrange = [min(v[2,*], MAX=vmax), vmax]
	vxrange += abs(vxrange) * [-0.1, 0.1]
	vyrange += abs(vyrange) * [-0.1, 0.1]
	vzrange += abs(vzrange) * [-0.1, 0.1]

	;RX
	Prix = MrPlot( t_fg, r[0,*], $
	               /CURRENT, $
	               LAYOUT      = [2,3,1], $
	               NAME        = 'Rx Interp', $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = rxrange, $
	               YTITLE      = 'R$\downX$ Interp!C(km)' )
	Prex = MrPlot( t_eph, ephem.position[0,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Rx', $
	               OVERPLOT    = Prix, $
	               PSYM        = 2 )
	;RY
	Priy = MrPlot( t_fg, r[1,*], $
	               /CURRENT, $
	               LAYOUT      = [2,3,3], $
	               NAME        = 'Ry Interp', $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = ryrange, $
	               YTITLE      = 'R$\downY$ Interp!C(km)' )
	Prey = MrPlot( t_eph, ephem.position[1,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Ry', $
	               OVERPLOT    = Priy, $
	               PSYM        = 2 )
	;RZ
	Priz = MrPlot( t_fg, r[2,*], $
	               /CURRENT, $
	               LAYOUT      = [2,3,5], $
	               NAME        = 'Rz Interp', $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = rrange, $
	               YTITLE      = 'R$\downZ$ Interp!C(km)' )
	Prez = MrPlot( t_eph, ephem.position[2,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Rz', $
	               OVERPLOT    = Priz, $
	               PSYM        = 2 )
	;VX
	Pvix = MrPlot( t_fg, v[0,*], $
	               /CURRENT, $
	               LAYOUT      = [2,3,2], $
	               NAME        = 'Vx Interp', $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = vxrange, $
	               YTITLE      = 'V$\downX$ Interp!C(km)' )
	Pvex = MrPlot( t_eph, ephem.velocity[0,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Vx', $
	               OVERPLOT    = Pvix, $
	               PSYM        = 2 )
	;VY
	Pviy = MrPlot( t_fg, v[1,*], $
	               /CURRENT, $
	               LAYOUT      = [2,3,4], $
	               NAME        = 'Vy Interp', $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = vyrange, $
	               YTITLE      = 'V$\downY$ Interp!C(km)' )
	Pvey = MrPlot( t_eph, ephem.velocity[1,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Vy', $
	               OVERPLOT    = Pviy, $
	               PSYM        = 2 )
	;VZ
	Pviz = MrPlot( t_fg, v[2,*], $
	               /CURRENT, $
	               LAYOUT      = [2,3,6], $
	               NAME        = 'Vz Interp', $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = vzrange, $
	               YTITLE      = 'V$\downZ$ Interp!C(km)' )
	Pvez = MrPlot( t_eph, ephem.velocity[2,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Vz', $
	               OVERPLOT    = Pviz, $
	               PSYM        = 2 )

	win -> Refresh
	return, win
end
