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
function mms_ql_mag_fftdiff
	compile_opt strictarr
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		void = cgErrorMSG(/QUIET)
		return, !Null
	endif
	
	;FFT Parameters
	duration = 32.0
	nbins    = 100

	;General inputs
	sc       = 'mms2'
	fg_instr = 'dfg'
	tstart   = '2015-03-17T14:34:00Z'
	tend     = '2015-03-17T24:00:00Z'
	dfg_dir  = '/Users/argall/Documents/Work/Data/MMS/DFG/'
	afg_dir  = '/Users/argall/Documents/Work/Data/MMS/AFG/'
	scm_dir  = '/Users/argall/Documents/Work/Data/MMS/SCM/'

;-------------------------------------------------------
; Find and Read AFG Data ///////////////////////////////
;-------------------------------------------------------
	mode  = 'srvy'
	level = 'l1b'

	;Data directory
	directory = fg_instr eq 'dfg' ? dfg_dir : afg_dir

	;Create DFG file name
	fname = mms_construct_filename(sc, fg_instr, mode, level, $
	                               DIRECTORY = directory, $
	                               /TOKENS)

	;Search for the file
	files = MrFile_Search(fname, /CLOSEST, $
	                      COUNT     = nfiles, $
	                      TIMEORDER = '%Y%M%d', $
	                      TSTART    = tstart, $
	                      TEND      = tend)
	if nfiles eq 0 then message, 'No file found: "' + fname + '".'

	;Create variable names
	b_vname = mms_construct_varname(sc, fg_instr, mode, OPTDESC='omb')
	
	;Read the data
	b_fg_omb = MrCDF_Read(files, b_vname, $
	                       DEPEND_0  = epoch_fg, $
	                       REC_START = tstart, $
	                       REC_END   = tend)

;-------------------------------------------------------
; Find and Read SCM Data ///////////////////////////////
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
	b_sc_omb = MrCDF_Read(files, b_vname, $
	                      DEPEND_0  = epoch_sc, $
	                      REC_START = tstart, $
	                      REC_END   = tend)

;-------------------------------------------------------
; Histogram PSD ////////////////////////////////////////
;-------------------------------------------------------
	
	;Convert time to seconds since midnight
	MrCDF_Epoch, epoch_fg[0], yr, mo, day, /BREAKDOWN_EPOCH
	MrCDF_Epoch, t0, yr, mo, day, /COMPUTE_EPOCH
	t_fg_ssm = MrCDF_epoch2sse(epoch_fg, t0)
	t_sc_ssm = MrCDF_epoch2sse(epoch_sc, t0)
	
	;FG FFT parameters
	dt_fg     = median(t_fg_ssm[1:*] - t_fg_ssm)
	sr_fg     = round(1.0 / dt_fg)
	nfft_fg   = long(duration * sr_fg)
	nshift_fg = long(nfft_fg / 2.0)
	
	;SCM FFT parameters
	dt_sc     = median(t_sc_ssm[1:*] - t_sc_ssm)
	sr_sc     = round(1.0 / dt_sc)
	nfft_sc   = long(duration * sr_sc)
	nshift_sc = long(nfft_sc / 2.0)

	;Histogram FG Data
	fft_fg = MrFFT(b_fg_omb, nfft_fg, 1.0/sr_fg, nshift_fg, $
	               DIMENSION   = 2, $
	               FREQUENCIES = f_fg)
	
	;Histogram FG Data
	fft_sc = MrFFT(b_sc_omb, nfft_sc, 1.0/sr_sc, nshift_sc, $
	               DIMENSION   = 2, $
	               FREQUENCIES = f_sc)

	;Pick the positive frequencies
	f_fg   = f_fg[0:nfft_fg/2]
	fft_fg = fft_fg[*, 0:nfft_fg/2, *]
	
	f_sc   = f_sc[0:nfft_sc/2]
	fft_sc = fft_sc[*, 0:nfft_sc/2, *]
	
;-------------------------------------------------------
; Histogram Differences ////////////////////////////////
;-------------------------------------------------------
	nfreqs = nfft_fg/2 + 1

	;Compute the amplitude and phase
	amp_fg   = MrLog( sqrt( real_part(fft_fg)^2 + imaginary(fft_fg) ) )
	amp_sc   = MrLog( sqrt( real_part(fft_sc)^2 + imaginary(fft_sc) ) )
	phase_fg = atan( imaginary(fft_fg), real_part(fft_fg) )
	phase_sc = atan( imaginary(fft_sc), real_part(fft_sc) )
	
	;Amplitude and phase differences
	;   - SCM will be truncated along the frequency dimension.
	dAmp   = amp_fg   - amp_sc
	dPhase = phase_fg - phase_sc
	
	;Allocate memory
	hAmp   = fltarr(nbins, nfreqs, 3)
	hPhase = fltarr(nbins, nfreqs, 3)
	
	;Histogram inputs
	amin = min(dAmp[where(finite(hAmp))],     MAX=amax)
	pmin = min(dPhase[where(finite(hPhase))], MAX=pmax)
	abinsize = (amax - amin) / (nbins - 1)
	pbinsize = (pmax - pmin) / (nbins - 1)
	
	;Independent variables
	amplitude = MrMake_Array(nbins, START=amin, INCREMENT=abinsize)
	phase     = MrMake_Array(nbins, START=pmin, INCREMENT=pbinsize) * !radeg

	;Histogram the differences
	for i = 0, nfreqs - 1 do begin
		hAmp[*, i, 0]   = cgHistogram(dAmp[*,i,0],   MIN=amin, MAX=amax, BINSIZE=abinsize, /NAN)
		hAmp[*, i, 1]   = cgHistogram(dAmp[*,i,1],   MIN=amin, MAX=amax, BINSIZE=abinsize, /NAN)
		hAmp[*, i, 2]   = cgHistogram(dAmp[*,i,2],   MIN=amin, MAX=amax, BINSIZE=abinsize, /NAN)
		hPhase[*, i, 0] = cgHistogram(dPhase[*,i,0], MIN=pmin, MAX=pmax, BINSIZE=pbinsize, /NAN)
		hPhase[*, i, 1] = cgHistogram(dPhase[*,i,1], MIN=pmin, MAX=pmax, BINSIZE=pbinsize, /NAN)
		hPhase[*, i, 2] = cgHistogram(dPhase[*,i,2], MIN=pmin, MAX=pmax, BINSIZE=pbinsize, /NAN)
	endfor
	
	;Order [frequency, counts, component]
	hAmp   = transpose(hAmp,   [1,0,2])
	hPhase = transpose(hPhase, [1,0,2])

;-------------------------------------------------------
; Plot /////////////////////////////////////////////////
;-------------------------------------------------------
	
	;Plot title
	title  = strupcase(sc) + ' ' + strupcase(fg_instr) + '-SCM ' + strmid(tstart, 0, 10)
	arange = [min(hAmp,   MAX=rmax), rmax]
	prange = [min(hPhase, MAX=rmax), rmax]
	
	
	;Create a window
	win = MrWindow(OXMARGIN=[10,12], XSIZE=850, YSIZE=550, XGAP=18, YGAP=0.5)
	win -> Refresh, /DISABLE

	;X Amplitude
	gAx = MrImage(hAmp[*,*,0], f_fg, amplitude, $
	              /AXES, $
	              /CURRENT, $
	              /SCALE, $
	              LAYOUT        = [2,3,1], $
	              NAME          = 'dAmp Bx', $
	              MISSING_COLOR = 'white', $
	              MISSING_VALUE = 0, $
	              RANGE         = arange, $
	              XTICKFORMAT   = '(a1)', $
	              XRANGE        = xrange, $
	              YTITLE        = '$\Delta$Log(Amp)', $
	              TITLE         = title)

	;Y Amplitude
	gAy = MrImage(hAmp[*,*,1], f_fg, amplitude, $
	              /AXES, $
	              /CURRENT, $
	              /SCALE, $
	              LAYOUT        = [2,3,3], $
	              MISSING_COLOR = 'white', $
	              MISSING_VALUE = 0, $
	              NAME          = 'dAmp By', $
	              RANGE         = arange, $
	              XTICKFORMAT   = '(a1)', $
	              XRANGE        = xrange, $
	              YTITLE        = '$\Delta$Log(Amp)')

	;Z Amplitude
	gAz = MrImage(hAmp[*,*,2], f_fg, amplitude, $
	              /AXES, $
	              /CURRENT, $
	              /SCALE, $
	              LAYOUT        = [2,3,5], $
	              MISSING_COLOR = 'white', $
	              MISSING_VALUE = 0, $
	              NAME          = 'dAmp Bz', $
	              RANGE         = arange, $
	              XRANGE        = xrange, $
	              XTITLE        = 'Frequency (Hz)', $
	              YTITLE        = '$\Delta$Log(Amp)')

	;X Phase
	gPx = MrImage(hPhase[*,*,0], f_fg, phase, $
	              /AXES, $
	              /CURRENT, $
	              /SCALE, $
	              LAYOUT        = [2,3,2], $
	              MISSING_COLOR = 'white', $
	              MISSING_VALUE = 0, $
	              NAME          = 'dPhase Bx', $
	              RANGE         = prange, $
	              XTICKFORMAT   = '(a1)', $
	              XRANGE        = xrange, $
	              YTITLE        = '$\Delta$Phase!C(Deg)', $
	              TITLE         = title)

	;Y Phase
	gPy = MrImage(hPhase[*,*,1], f_fg, phase, $
	              /AXES, $
	              /CURRENT, $
	              /SCALE, $
	              LAYOUT        = [2,3,4], $
	              MISSING_COLOR = 'white', $
	              MISSING_VALUE = 0, $
	              NAME          = 'dPhase By', $
	              RANGE         = prange, $
	              XTICKFORMAT   = '(a1)', $
	              XRANGE        = xrange, $
	              YTITLE        = '$\Delta$Phase!C(Deg)')

	;Z Phase
	gPz = MrImage(hPhase[*,*,2], f_fg, phase, $
	              /AXES, $
	              /CURRENT, $
	              /SCALE, $
	              LAYOUT        = [2,3,6], $
	              MISSING_COLOR = 'white', $
	              MISSING_VALUE = 0, $
	              NAME          = 'dPhase Bz', $
	              RANGE         = prange, $
	              XRANGE        = xrange, $
	              XTITLE        = 'Frequency (Hz)', $
	              YTITLE        = '$\Delta$Phase!C(Deg)')

	;CB Amplitude
	cbA = MrColorbar(NAME   = 'CB Amplitude', $
	                 TARGET = gAy, $
	                 TITLE  = 'Counts', $
	                 WIDTH  = 0.5)
	
	;CB Phase
	cbP = MrColorbar(NAME   = 'CB Phase', $
	                 TARGET = gPy, $
	                 TITLE  = 'Counts', $
	                 WIDTH  = 0.5)

	win -> Refresh
	return, win
end