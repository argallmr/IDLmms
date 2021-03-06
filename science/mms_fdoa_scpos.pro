; docformat = 'rst'
;
; NAME:
;       mms_fdoa_scpos.pro
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
;       V:                  out, optional, type=3xN float
;                           Spacecraft velocity in DMPA coordinates.
;       DMPA:               in, optional, type=boolean, default=0
;                           If set, `R` and `V` will be returned in DMPA coordinates.
;       GEI:                in, optional, type=boolean, default=0
;                           If set, `R` and `V` will be returned in GEI coordinates.
;       GSE:                in, optional, type=boolean, default=0
;                           If set, `R` and `V` will be returned in GSE coordinates.
;                               This is the default of `DMPA`, `GEI`, and `GSM` are
;                               not set.
;       GSM:                in, optional, type=boolean, default=0
;                           If set, `R` and `V` will be returned in GSM coordinates.
;
; :Returns:
;       R:                  Spacecraft position in DMPA coordinates.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015-11-27  -   Written by Matthew Argall
;-
function mms_fdoa_scpos, sc, tstart, tend, t_out, $
EPH_DIR=eph_dir, $
SDC_ROOT=sdc_root, $
V=v, $
DMPA=dmpa, $
GSE=gse, $
GEI=gei, $
GSM=gsm
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	endif

	;Defaults
	if n_elements(sdc_root)  eq 0 then sdc_root  = '/nfs'
	if n_elements(eph_dir)   eq 0 then eph_dir   = filepath('', ROOT_DIR=sdc_root, $
	                                                        SUBDIRECTORY=['ancillary', sc, 'defeph'])
	if n_elements(att_dir)   eq 0 then att_dir   = filepath('', ROOT_DIR=sdc_root, $
	                                                        SUBDIRECTORY=['ancillary', sc, 'defatt'])

	;Coordinate system
	dmpa = keyword_set(dmpa)
	gse  = keyword_set(gse)
	gsm  = keyword_set(gsm)
	gei  = keyword_set(gei)
	if dmpa + gse + gsm + gei eq 0 then gse = 1
	if dmpa + gse + gsm + gei GT 1 then $
		message, 'DMPA, GSE, GSM, and GEI are mutually exclusive.'
	
	;Get the velocity
	tf_v_out = arg_present(v)

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
	
	;Ephemeris
	defeph = mms_fdoa_read_defeph(files_eph, tstart, tend)
	
	;Attitude
	if ~gei then defatt = mms_fdoa_read_defatt(files_att, tstart, tend)

;-----------------------------------------------------
; Interpolate to Output Times \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Interpolate ephemeris data
	if n_elements(t_out) gt 0 then begin
		r = mms_fg_xinterp_ephem(defeph.tt2000, defeph.position, defeph.velocity, reform(t_out), v)
	endif else begin
		t_out = defeph.tt2000
		r     = defeph.position
		if tf_v_out then v = defeph.velocity
	endelse

;-----------------------------------------------------
; Coordinate System \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Vectors begin in GEI
	;
	
	;DMPA
	if dmpa then begin
		gei2despun = mms_fdoa_xgei2despun(defatt, t_out, TYPE='P')
		r          = MrVector_Rotate(gei2despun, r)
		if tf_v_out then v = MrVector_Rotate(gei2despun, v)
	
	;GSE
	endif else if gse then begin
		r = mms_rot_gei2gse(t_out, temporary(r))
		if tf_v_out then v = mms_rot_gei2gse(t_out, temporary(v))
	
		;GSM
		if gsm then begin
			r = mms_rot_gse2gsm(t_out, temporary(r))
			if tf_v_out then v = mms_rot_gse2gsm(t_out, temporary(v))
		endif
	endif
	
	return, r
end