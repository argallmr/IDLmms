; docformat = 'rst'
;
; NAME:
;       mms_fig_fields.pro
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
;       1) DFG Magnetic Field
;       2) EDP Electric Field
;       3) EDP Spacecraft Potential
;       4) EDI 0-degree ambient counts
;       5) EDI 180-degree ambient counts
;       6) EDI Anisotropy (0/180 counts)
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
function mms_fig_fsmfgmscm_spectra, sc, mode, tstart, tend, $
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

;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;FGM Magnetic Field
	fgm_mode = mode eq 'brst' ? mode : 'srvy'
	mms_fgm_ql_read, sc, 'dfg', fgm_mode, 'l2pre', tstart, tend, $
	                 B_GSE = b_fgm, $
	                 TIME  = t_fgm
	
	;SCM Magnetic Field
	scopt = 'sc' + strmid(mode, 0, 1)
	scm_fname = mms_find_file(sc, 'scm', mode, 'l2', $
	                          DIRECTORY = '/nfs/fsm/scm/', $
	                          OPTDESC   = scopt + '-64s', $
	                          SEARCHSTR = str, $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	oscm = MrCDF_File(scm_fname)
	b_scm = oscm -> Read(sc + '_scm_b_gse', DEPEND_0=t_scm, REC_START=tstart, REC_END=tend)
	obj_destroy, oscm
	
	;FSM Magnetic Field
	fsm_mode = mode eq 'brst' ? mode : 'srvy'
	mms_fsm_ql_read, sc, fsm_mode, tstart, tend, $
	                 B_DMPA = b_fsm, $
	                 TIME   = t_fsm

	;Separate |B|
	bmag  = b_fgm[3,*]
	b_fgm = b_fgm[0:2,*]

;-----------------------------------------------------
; Power Spectral Density \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to seconds
	t0        = t_fsm[0]
	t_fsm_ssm = MrCDF_epoch2ssm( temporary(t_fsm), t0)
	t_fgm_ssm = MrCDF_epoch2ssm( temporary(t_fgm), t0)
	t_scm_ssm = MrCDF_epoch2ssm( temporary(t_scm), t0)
	
	nfft   = 512
	nshift = nfft / 4.0

	;Sampling rates
	dt_fsm = median(t_fsm_ssm[1:*] - t_fsm_ssm)
	dt_fgm = median(t_fgm_ssm[1:*] - t_fgm_ssm)
	dt_scm = median(t_scm_ssm[1:*] - t_scm_ssm)

	;FSM
	b_fsm_psd = MrPSD( b_fsm, nfft, dt_fsm, nshift, $
	                   DIMENSION   = 2, $
	                   FMAX        = 8, $
	                   FREQUENCIES = f_fsm, $
	                   T0          = t_fsm_ssm[0], $
	                   TIME        = t_fsm_psd )
	
	;FGM
	b_fgm_psd = MrPSD( b_fgm, nfft, dt_fgm, nshift, $
	                   DIMENSION   = 2, $
	                   FREQUENCIES = f_fgm, $
	                   T0          = t_fgm_ssm[0], $
	                   TIME        = t_fgm_psd )
	
	;SCM
	b_scm_psd = MrPSD( b_scm, nfft, dt_scm, nshift, $
	                   DIMENSION   = 2, $
	                   FREQUENCIES = f_scm, $
	                   T0          = t_scm_ssm[0], $
	                   TIME        = t_scm_psd )

;-----------------------------------------------------
; Plot Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create the window
	win = MrWindow(LAYOUT=[1,3], OXMARGIN=[10,15], XSIZE=800, XGAP=0.5, YGAP=0.5, YSIZE=650, REFRESH=0)

	;B FSM
	im_fsm = MrImage( b_fsm_psd[*,*,0], t_fsm_psd, f_fsm, $
	                  /AXES, $
	                  /CURRENT, $
	                  /SCALE, $
	                  /LOG, $
	                  /YLOG, $
	                  CTINDEX     = 13, $
	                  LAYOUT      = [1,3,1], $
	                  NAME        = 'B PSD FSM', $
	                  RANGE       = range, $
	                  TITLE       = 'PSD Comparison from FSM, DFG & SCM', $
	                  XTICKFORMAT = '(a1)', $
	                  YTITLE      = 'f!C(Hz)')
	
	;B FGM
	im_fgm = MrImage( b_fgm_psd[*,*,0], t_fgm_psd, f_fgm, $
	                  /AXES, $
	                  /CURRENT, $
	                  /SCALE, $
	                  /LOG, $
	                  /YLOG, $
	                  CTINDEX     = 13, $
	                  LAYOUT      = [1,3,2], $
	                  NAME        = 'B PSD FGM', $
	                  RANGE       = range, $
	                  XTICKFORMAT = '(a1)', $
	                  YTITLE      = 'f!C(Hz)')
	
	;B SCM
	im_scm = MrImage( b_scm_psd[*,*,0], t_scm_psd, f_scm, $
	                  /AXES, $
	                  /CURRENT, $
	                  /SCALE, $
	                  /LOG, $
	                  /YLOG, $
	                  CTINDEX     = 13, $
	                  LAYOUT      = [1,3,3], $
	                  NAME        = 'B PSD SCM', $
	                  RANGE       = range, $
	                  XTICKFORMAT = 'time_labels', $
	                  YTITLE      = 'f!C(Hz)')
	
	
	;CB FSM
	cb_fsm = MrColorBar(LOCATION    = 'Right', $
	                    OFFSET      = 0.5, $
	                    ORIENTATION = 1, $
	                    TARGET      = im_fsm, $
	                    TITLE       = 'B!C(nT^2/Hz)')
	
	;CB FGM
	cb_fgm = MrColorBar(LOCATION    = 'Right', $
	                    OFFSET      = 0.5, $
	                    ORIENTATION = 1, $
	                    TARGET      = im_fgm, $
	                    TITLE       = 'B!C(nT^2/Hz)')
	
	;CB SCM
	cb_scm = MrColorBar(LOCATION    = 'Right', $
	                    OFFSET      = 0.5, $
	                    ORIENTATION = 1, $
	                    TARGET      = im_scm, $
	                    TITLE       = 'B!C(nT^2/Hz)')


	win -> Refresh
	return, win
end