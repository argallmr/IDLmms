; docformat = 'rst'
;
; NAME:
;    mms_ql_sc_psdhist
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
;   Plot the histogrammed power for SCM.
;
; :Categories:
;    MMS, SCM
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
;       2015/05/20  -   Written by Matthew Argall
;-
function mms_ql_sc_psdhist
	compile_opt strictarr
	on_error, 2
	
	;FFT Parameters
	duration = 32.0

	;General inputs
	sc      = 'mms2'
	tstart  = '2015-03-17T14:34:00Z'
	tend    = '2015-03-17T24:00:00Z'
	scm_dir = '/Users/argall/Documents/Work/Data/MMS/SCM/'

;-------------------------------------------------------
; Find and Read Data ///////////////////////////////////
;-------------------------------------------------------
	instr   = 'scm'
	mode    = 'comm'
	level   = 'l1b'
	optdesc = 'sc256'

	;Create DFG file name
	fname = mms_construct_filename(sc, instr, mode, level, $
	                               DIRECTORY = scm_dir, $
	                               OPTDESC   = optdesc, $
	                               /TOKENS)

	;Search for the file
	files = MrFile_Search(fname, /CLOSEST, $
	                      COUNT     = nfiles, $
	                      TIMEORDER = '%Y%M%d', $
	                      TSTART    = tstart, $
	                      TEND      = tend)
	if nfiles eq 0 then message, 'No file found: "' + fname + '".'

	;Create variable names
	b_vname = mms_construct_varname(sc, instr, optdesc, OPTDESC=instr + '123')
	
	;Read the data
	b_omb = MrCDF_Read(files, b_vname, $
	                   DEPEND_0  = epoch, $
	                   REC_START = tstart, $
	                   REC_END   = tend)

;-------------------------------------------------------
; Plot /////////////////////////////////////////////////
;-------------------------------------------------------
	
	;Plot title
	title = strupcase(sc) + ' ' + strupcase(instr) + ' ' + strmid(tstart, 0, 10)
	
	;Convert time to seconds since midnight
	MrCDF_Epoch, epoch[0], yr, mo, day, /BREAKDOWN_EPOCH
	MrCDF_Epoch, t0, yr, mo, day, /COMPUTE_EPOCH
	t_ssm = MrCDF_epoch2sse(epoch, t0)
	
	;FFT parameters
	dt          = median(t_ssm[1:*] - t_ssm)
	sample_rate = round(1.0 / dt)
	nfft        = long(duration * sample_rate)
	nshift      = long(nfft / 2.0)
	
	;Create a window
	win = MrWindow(OXMARGIN=[10,12], XSIZE=600, YSIZE=550, YGAP=0.5)
	win -> Refresh, /DISABLE

	;X Hist
	hX = MrPSDHistPlot(reform(b_omb[0,*]), nfft, sample_rate, nshift, $
	                   /CURRENT, $
	                   LAYOUT      = [1,3,1], $
	                   NBINS       = nbins, $
	                   XTICKFORMAT = '(a1)', $
	                   YTITLE      = 'Power!C(Log$\down10$)', $
	                   TITLE       = title, $
	                   _REF_EXTRA  = extra)

	;Y Hist
	hY = MrPSDHistPlot(reform(b_omb[1,*]), nfft, sample_rate, nshift, $
	                   /CURRENT, $
	                   LAYOUT      = [1,3,2], $
	                   NBINS       = nbins, $
	                   XTICKFORMAT = '(a1)', $
	                   YTITLE      = 'Power!C(Log$\down10$)', $
	                   _REF_EXTRA  = extra)

	;Z Hist
	hZ = MrPSDHistPlot(reform(b_omb[2,*]), nfft, sample_rate, nshift, $
	                   /CURRENT, $
	                   LAYOUT     = [1,3,3], $
	                   NBINS      = nbins, $
	                   XTITLE     = 'Frequency (Hz)', $
	                   YTITLE     = 'Power!C(Log$\down10$)', $
	                   _REF_EXTRA = extra)
	
	win -> Refresh
	return, win
end