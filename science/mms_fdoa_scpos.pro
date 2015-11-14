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
function mms_fdoa_scpos, sc, tstart, tend, t_out, $
EPH_DIR=eph_dir, $
SDC_ROOT=sdc_root, $
V_DMPA=v_dmpa
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		void = cgErrorMSG(/QUIET)
		return, !Null
	endif

	;Defaults
	if n_elements(sdc_root)  eq 0 then sdc_root  = '/nfs'
	if n_elements(eph_dir)   eq 0 then eph_dir   = filepath('', ROOT_DIR=sdc_root, $
	                                                        SUBDIRECTORY=['ancillary', sc, 'defeph'])
	if n_elements(att_dir)   eq 0 then att_dir   = filepath('', ROOT_DIR=sdc_root, $
	                                                        SUBDIRECTORY=['ancillary', sc, 'defatt'])

;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	; Ephemeris file
	str = filepath(ROOT_DIR=eph_dir, strupcase(sc) + '_DEFEPH_%Y%D_%Y%D.V*' )
	files_eph = MrFile_Search( str, $
	                           /CLOSEST, $
	                           COUNT     = nfiles_eph, $
	                           TSTART    = tstart, $
	                           TEND      = tend, $
	                           TIMEORDER = '%Y%D', $
	                           VREGEX    = 'V([0-9]{2})' )
	if nfiles_eph eq 0 then message, 'No ephemeris files found: "' + str + '".'

	; Attitude file
	str = filepath(ROOT_DIR=att_dir, strupcase(sc) + '_DEFATT_%Y%D_%Y%D.V*' )
	files_att = MrFile_Search( str, $
	                           /CLOSEST, $
	                           COUNT     = nfiles_att, $
	                           TSTART    = tstart, $
	                           TEND      = tend, $
	                           TIMEORDER = '%Y%D', $
	                           VREGEX    = 'V([0-9]{2})' )
	if nfiles_att eq 0 then message, 'No attitude files found: "' + str + '".'
	
;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Attitude
	defatt = mms_fdoa_read_defatt(files_att, tstart, tend)
	
	;Ephemeris
	defeph = mms_fdoa_read_defeph(files_eph, tstart, tend)

;-----------------------------------------------------
; Rotate Ephemeris to DMPA from GEI \\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Interpolate ephemeris data
	if n_elements(t_out) gt 0 then begin
		r_gei      = mms_fg_xinterp_ephem(defeph.tt2000, defeph.position, defeph.velocity, reform(t_out), v_gei)
		gei2despun = mms_fdoa_xgei2despun(defatt, reform(t_out), TYPE='P')
	endif else begin
		if arg_present(t_out) then t_out = defatt.tt2000
		r_gei      = defeph.position
		v_gei      = defeph.velocity
		gei2despun = mms_fdoa_xgei2despun(defatt, defatt.tt2000, TYPE='P')
	endelse

	;Rotate from GEI to DMPA
	r_dmpa = MrVector_Rotate(gei2despun, r_gei)
	v_dmpa = MrVector_Rotate(gei2despun, v_gei)
	
	return, r_dmpa
end