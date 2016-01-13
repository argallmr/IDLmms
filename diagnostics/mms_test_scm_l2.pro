; docformat = 'rst'
;
; NAME:
;       mms_test_scm_l2.pro
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
;   Create a plot of FIELDS data
;       1) Bx
;       2) By
;       3) Bz
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
;-
function mms_test_scm_l2, sc, mode, tstart, tend, $
EIGVECS=eigvecs
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
;	sc     = 'mms1'
;	mode   = 'fast'
;	tstart = '2015-10-16T13:00:00Z'
;	tend   = '2015-10-16T13:30:00Z'

	sc     = 'mms1'
	mode   = 'slow'
	tstart = '2015-08-15T09:00:00Z'
	tend   = '2015-08-15T09:15:00Z'



;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;SCM Magnetic Field
	scopt = 'sc' + strmid(mode, 0, 1)
	b_scm = mms_cdf_read(scopt + '_scm123', sc, 'scm', mode, 'l1b', tstart, tend, scopt, DEPEND_0=t_scm)
	
	;UNH-Calibrated SCM Magnetic Field
	file  = '/nfs/fsm/scm/mms1_scm_slow_l2_scs-64s_20150815_v0.0.0.cdf'
	oscm  = MrCDF_File(file)
	b_unh = oscm -> Read(sc + '_scm_b_dmpa', REC_START=tstart, REC_END=tend, DEPEND_0=t_unh)
	obj_destroy, oscm

	;Despin
	dss_file = mms_find_file(sc, 'fields', 'hk', 'l1b', $
	                         OPTDESC   = '101', $
	                         COUNT     = count, $
	                         SDC_ROOT  = '/nfs/hk/', $
	                         SEARCHSTR = str, $
	                         TSTART    = tstart, $
	                         TEND      = tend)
	if count eq 0 then message, 'Cannot find DSS file: "' + str + '".'
	sunpulse = mms_dss_read_sunpulse(dss_file, tstart, tend, /UNIQ_PACKETS)

;-----------------------------------------------------
; OMB -> DMPA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;OMB -> SMPA
	omb2smpa = mms_fg_xomb2smpa()
	b_scm_smpa = MrVector_Rotate(omb2smpa, b_scm)
	
	;SMPA -> DMPA
	smpa2dmpa = mms_dss_xdespin(sunpulse, t_scm)
	b_scm_dmpa = MrVector_Rotate(smpa2dmpa, b_scm_smpa)

;-----------------------------------------------------
; Power Spectral Density \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to seconds
	t0        = t_scm[0]
	t_scm_ssm = MrCDF_epoch2ssm( temporary(t_scm), t0)
	t_unh_ssm = MrCDF_epoch2ssm( temporary(t_unh), t0)

	;Create the window
	win = MrWindow(LAYOUT=[1,3], XSIZE=800, XGAP=0.5, YGAP=0.5, YSIZE=650, REFRESH=0)

	;Bx SCM
	gBx = MrPlot( t_scm_ssm, b_scm_dmpa[0,*], $
	              /CURRENT, $
	              NAME        = 'Bx SCM', $
	              XTICKFORMAT = '(a1)', $
	              YTITLE      = 'Bx!C(nT)')

	;By SCM
	gBy = MrPlot( t_scm_ssm, b_scm_dmpa[1,*], $
	              /CURRENT, $
	              NAME        = 'By SCM', $
	              XTICKFORMAT = '(a1)', $
	              YTITLE      = 'By!C(nT)')

	;Bz SCM
	gBz = MrPlot( t_scm_ssm, b_scm_dmpa[2,*], $
	              /CURRENT, $
	              NAME        = 'Bz SCM', $
	              XTICKFORMAT = 'time_labels', $
	              YTITLE      = 'Bz!C(nT)')

	;Bx UNH
	oBx = MrPlot( t_unh_ssm, b_unh[0,*], $
	              COLOR       = 'Blue', $
	              NAME        = 'Bx UNH', $
	              OVERPLOT    = gBx)

	;By UNH
	oBy = MrPlot( t_unh_ssm, b_unh[1,*], $
	              COLOR       = 'Blue', $
	              NAME        = 'By UNH', $
	              OVERPLOT    = gBy)

	;Bz UNH
	oBz = MrPlot( t_unh_ssm, b_unh[2,*], $
	              COLOR       = 'Blue', $
	              NAME        = 'Bz UNH', $
	              OVERPLOT    = gBz)
	
	;Legend
	gl = MrLegend( ALIGNMENT    = 'NE', $
	               LABEL        = ['SCM', 'UNH'], $
	               POSITION     = [1,0, 1.0], $
	               /RELATIVE, $
	               SAMPLE_WIDTH = 0, $
	               TEXT_COLOR   = ['Black', 'Blue'], $
	               TARGET       = [gBx, oBx] )

	win -> Refresh
	return, win
end