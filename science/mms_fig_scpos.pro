; docformat = 'rst'
;
; NAME:
;       mms_fig_vdrift.pro
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
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
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
;   Return the spacecraft position.
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           Spacecraft for which data is to be plotted.
;       TSTART:             in, required, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, required, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;       T_OUT:              in, out, required, type=lon64arr (cdf_time_tt2000)
;                           If present, position and velocity will be interpolated to
;                               these time stamps. If not provided, the time stamps
;                               will be returned.
;
; :Keywords:
;       EPHEM_DIR:          in, optional, type=string, default='/nfs/ancillary/`SC`/defeph'
;                           Directory in which to find ephemeris data.
;       SDC_ROOT:           in, optional, type=string, default='/nfs'
;                           Directory at which the SDC-like data repository is located.
;       V_DMPA:             out, optional, type=3xN float
;                           Spacecraft velocity in DMPA coordinates.
;
; :Returns:
;       R_DMPA:         Spacecraft position in DMPA coordinates.
;-
pro mms_fig_scpos, tstart, tend
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif

	;Read the data
	r1 = mms_fdoa_scpos('mms1', tstart, tend)
	r2 = mms_fdoa_scpos('mms2', tstart, tend)
	r3 = mms_fdoa_scpos('mms3', tstart, tend)
	r4 = mms_fdoa_scpos('mms4', tstart, tend)

	;Average position during the interval
	r1 = mean(r1, DIMENSION=2)
	r2 = mean(r2, DIMENSION=2)
	r3 = mean(r3, DIMENSION=2)
	r4 = mean(r4, DIMENSION=2)

	;Calculate positions relative to MMS1
	r4 = r4 - r1
	r3 = r3 - r1
	r2 = r2 - r1
	r1 = r1 - r1
	
	x = [r1[0], r2[0], r3[0], r4[0]]
	y = [r1[1], r2[1], r3[1], r4[1]]
	z = [r1[2], r2[2], r3[2], r4[2]]
	
;-----------------------------------------------------
; Plot Positions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	xrange = [min([r1[0], r2[0], r3[0], r4[0]], MAX=rmax), rmax]
	yrange = [min([r1[1], r2[1], r3[1], r4[1]], MAX=rmax), rmax]
	zrange = [min([r1[2], r2[2], r3[2], r4[2]], MAX=rmax), rmax]

	xrange += abs(xrange) * [-0.1, 0.1]
	yrange += abs(yrange) * [-0.1, 0.1]
	zrange += abs(zrange) * [-0.1, 0.1]

	colors = mms_color(['blue', 'green', 'red', 'black'])

	;Create a cgWindow
	cgWindow

	;Establish the 3D coordinate sytem using cgSurf
	cgSurf, fltarr(2,2), [0], [0], /NODATA, $
	        TITLE  = 'MMS Spacecraft Positions'
	        XRANGE = xrange, $
	        XSTYLE = 1, $
	        XTITLE = 'X (km)', $
	        YRANGE = yrange, $
	        YSTYLE = 1, $
	        YTITLE = 'Y (km)', $
	        ZSTYLE = 1, $
	        ZRANGE = zrange, $
	        ZTITLE = 'Z (km)', $
	        /SAVE, $
	        /ADDCMD
	
	;Plot positions
	cgPlotS, x, y, z, $
	         SYMCOLOR = colors, $
	         SYMSIZE  = 4, $
	         PSYM     = 'FilledDiamond', $
	         /T3D, $
	         /ADDCMD

	;Step through each spacecraft
	for i = 0, 3 do begin
		;Project onto XY-plane
		cgPlotS, x[[i,i]], y[[i,i]], [z[i],zrange[0]], $
		         COLOR     = colors[i], $
		         LINESTYLE = 2, $
		         /T3D, $
		         /ADDCMD
		
		;Project onto XZ-plane
		cgPlotS, x[[i,i]], [y[i], yrange[0]], z[[i,i]], $
		         COLOR     = colors[i], $
		         LINESTYLE = 2, $
		         /T3D, $
		         /ADDCMD
		
		;Project onto YZ-plane
		cgPlotS, [x[i], xrange[0]], y[[i,i]], z[[i,i]], $
		         COLOR     = colors[i], $
		         LINESTYLE = 2, $
		         /T3D, $
		         /ADDCMD
	endfor
end