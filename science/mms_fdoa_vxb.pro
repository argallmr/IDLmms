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
;   Calculate the convective electric field due the spacecraft velocity.
;
;   Calling Sequence:
;       E_VxB = mms_fdoa_vxb(sc, tstart, tend)
;       E_VxB = mms_fdoa_vxb(v, B)
;       E_VxB = mms_fdoa_vxb(v, B, t_in, t_out)
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           Spacecraft for which data is to be plotted.
;       TSTART:             in, required, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, required, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       EIGVECS:        out, optional, type=3x3 float
;                       Rotation matrix (into the minimum variance coordinate system).
;
; :Returns:
;       E_VXB:          Convective electric field due to the spacecraft velocity, in
;                           the frame moving with the spacecraft.
;-
function mms_fdoa_vxb, sc, tstart, tend, $
EPH_DIR=eph_dir, $
FGM_INSTR=fgm_instr, $
R_DMPA=r_dmpa, $
SDC_ROOT=sdc_root, $
TIME=t_fgm, $
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
	if n_elements(fgm_instr) eq 0 then fgm_instr = 'dfg'
	if n_elements(eph_dir)   eq 0 then eph_dir   = filepath('', ROOT_DIR=sdc_root, $
	                                                        SUBDIRECTORY=['ancillary', sc, 'defeph'])
	if n_elements(att_dir)   eq 0 then att_dir   = filepath('', ROOT_DIR=sdc_root, $
	                                                        SUBDIRECTORY=['ancillary', sc, 'defatt'])

;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;FGM QL SRVY file
	files_fgm = mms_find_file(sc, fgm_instr, 'srvy', 'ql', $
	                          COUNT     = nfgm, $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	if nfgm eq 0 then message, 'No FGM files found: "' + searchstr + '".'

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
	
	;Read DFG Data
	mms_fgm_ql_read, files_fgm, tstart, tend, $
	                 TIME   = t_fgm, $
	                 B_DMPA = b_fgm

;-----------------------------------------------------
; Rotate Ephemeris to DMPA from GEI \\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Interpolate ephemeris data
	r_gei = mms_fg_xinterp_ephem(defeph.tt2000, defeph.position, defeph.velocity, t_fgm, v_gei)

	;Rotate from GEI to DMPA
	gei2despun = mms_fdoa_xgei2despun(defatt, t_fgm, TYPE='P')
	r_dmpa     = MrVector_Rotate(gei2despun, r_gei)
	v_dmpa     = MrVector_Rotate(gei2despun, v_gei)

;-----------------------------------------------------
; Compute VxB Electric Field \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Compute electric field
	;   - V is in the frame of the plasma
	;   - We need to be in the frame of the spacecraft
	;   - In the spacecraft frame, the plasma is moving at minus the spacecraft velocity
	E_VxB = -MrVxB_EField(v_dmpa, B_fgm[0:2,*])
	
	return, E_VxB
end