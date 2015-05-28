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
function mms_ql_mag_psdfloor
	compile_opt strictarr
	on_error, 2
	
	;FFT Parameters
	duration = 32.0
	nbins    = 100

	;General inputs
	sc       = 'mms2'
	tstart   = '2015-03-17T14:34:00Z'
	tend     = '2015-03-17T24:00:00Z'
	dfg_dir  = '/Users/argall/Documents/Work/Data/MMS/DFG/'
	afg_dir  = '/Users/argall/Documents/Work/Data/MMS/AFG/'
	scm_dir  = '/Users/argall/Documents/Work/Data/MMS/SCM/'

;-------------------------------------------------------
; Find and Read AFG Data ///////////////////////////////
;-------------------------------------------------------
	instr = 'afg'
	mode  = 'srvy'
	level = 'l1b'

	;Create DFG file name
	fname = mms_construct_filename(sc, instr, mode, level, $
	                               DIRECTORY = afg_dir, $
	                               /TOKENS)

	;Search for the file
	files = MrFile_Search(fname, /CLOSEST, $
	                      COUNT     = nfiles, $
	                      TIMEORDER = '%Y%M%d', $
	                      TSTART    = tstart, $
	                      TEND      = tend)
	if nfiles eq 0 then message, 'No file found: "' + fname + '".'

	;Create variable names
	b_vname = mms_construct_varname(sc, instr, mode, OPTDESC='omb')
	
	;Read the data
	b_afg_omb = MrCDF_Read(files, b_vname, $
	                       DEPEND_0  = epoch_afg, $
	                       REC_START = tstart, $
	                       REC_END   = tend)

;-------------------------------------------------------
; Find and Read DFG Data ///////////////////////////////
;-------------------------------------------------------
	instr = 'dfg'
	mode  = 'srvy'
	level = 'l1b'

	;Create DFG file name
	fname = mms_construct_filename(sc, instr, mode, level, $
	                               DIRECTORY = dfg_dir, $
	                               /TOKENS)

	;Search for the file
	files = MrFile_Search(fname, /CLOSEST, $
	                      COUNT     = nfiles, $
	                      TIMEORDER = '%Y%M%d', $
	                      TSTART    = tstart, $
	                      TEND      = tend)
	if nfiles eq 0 then message, 'No file found: "' + fname + '".'

	;Create variable names
	b_vname = mms_construct_varname(sc, instr, mode, OPTDESC='omb')
	
	;Read the data
	b_dfg_omb = MrCDF_Read(files, b_vname, $
	                       DEPEND_0  = epoch_dfg, $
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
	MrCDF_Epoch, epoch_afg[0], yr, mo, day, /BREAKDOWN_EPOCH
	MrCDF_Epoch, t0, yr, mo, day, /COMPUTE_EPOCH
	t_afg_ssm = MrCDF_epoch2sse(epoch_afg, t0)
	t_dfg_ssm = MrCDF_epoch2sse(epoch_dfg, t0)
	t_sc_ssm = MrCDF_epoch2sse(epoch_sc, t0)
	
	;AFG FFT parameters
	dt_afg     = median(t_afg_ssm[1:*] - t_afg_ssm)
	sr_afg     = round(1.0 / dt_afg)
	nfft_afg   = long(duration * sr_afg)
	nshift_afg = long(nfft_afg / 2.0)
	
	;DFG FFT parameters
	dt_dfg     = median(t_dfg_ssm[1:*] - t_dfg_ssm)
	sr_dfg     = round(1.0 / dt_dfg)
	nfft_dfg   = long(duration * sr_dfg)
	nshift_dfg = long(nfft_dfg / 2.0)
	
	;SCM FFT parameters
	dt_sc     = median(t_sc_ssm[1:*] - t_sc_ssm)
	sr_sc     = round(1.0 / dt_sc)
	nfft_sc   = long(duration * sr_sc)
	nshift_sc = long(nfft_sc / 2.0)

	;Histogram FG Data
	histc = MrPSDHist(b_afg_omb, nfft_afg, sr_afg, nshift_afg, $
	                  DIMENSION  = 2, $
	                  NBINS      = nbins, $
	                  FREQUENCY  = afg_f, $
	                  POWER      = afg_power, $
	                  PEAK_POWER = afg_peak_power)
	
	;Histogram FG Data
	histc = MrPSDHist(b_dfg_omb, nfft_dfg, sr_dfg, nshift_dfg, $
	                  DIMENSION  = 2, $
	                  NBINS      = nbins, $
	                  FREQUENCY  = dfg_f, $
	                  POWER      = dfg_power, $
	                  PEAK_POWER = dfg_peak_power)
	
	;Histogram SC Data
	histc = MrPSDHist(b_sc_omb, nfft_sc, sr_sc, nshift, $
	                  DIMENSION  = 2, $
	                  NBINS      = nbins, $
	                  FREQUENCY  = sc_f, $
	                  POWER      = sc_power, $
	                  PEAK_POWER = sc_peak_power)
	
;-------------------------------------------------------
; Plot /////////////////////////////////////////////////
;-------------------------------------------------------
	
	;Plot title
	title  = strupcase(sc) + ' AFG-DFG-SCM ' + strmid(tstart, 0, 10)
	
	afg_yrange = [min(afg_peak_power, MAX=ymax), ymax]
	dfg_yrange = [min(dfg_peak_power, MAX=ymax), ymax]
	sc_yrange  = [min(sc_peak_power,  MAX=ymax), ymax]
	yrange     = [min([afg_yrange, dfg_yrange, sc_yrange], MAX=ymax), ymax]
	xrange     = [min(dfg_f, MAX=xmax), xmax]
	
	;Create a window
	win = MrWindow(XSIZE=600, YSIZE=550, YGAP=0.5)
	win -> Refresh, /DISABLE

	;X Hist
	pX = MrPlot(afg_f, afg_peak_power[0,*], $
	            /CURRENT, $
	            NAME        = 'AFG Bx', $
	            XTICKFORMAT = '(a1)', $
	            XRANGE      = xrange, $
	            YTITLE      = 'Power!C(Log$\down10$)', $
	            YRANGE      = yrange, $
	            TITLE       = title)
	opx1 = MrPlot(dfg_f, dfg_peak_power[0,*], OVERPLOT=pX, COLOR='Red',  NAME='DFG Bx')
	opx2 = MrPlot(sc_f,  sc_peak_power[0,*],  OVERPLOT=pX, COLOR='Blue', NAME='SCM Bx')

	;Y Hist
	pY = MrPlot(afg_f, afg_peak_power[1,*], $
	            /CURRENT, $
	            NAME        = 'AFG By', $
	            XTICKFORMAT = '(a1)', $
	            XRANGE      = xrange, $
	            YTITLE      = 'Power!C(Log$\down10$)', $
	            YRANGE      = yrange)
	opy1 = MrPlot(dfg_f, dfg_peak_power[1,*], OVERPLOT=pY, COLOR='Red',  NAME='DFG By')
	opy2 = MrPlot(sc_f,  sc_peak_power[1,*],  OVERPLOT=pY, COLOR='Blue', NAME='SCM By')

	;Z Hist
	pZ = MrPlot(afg_f, afg_peak_power[2,*], $
	            /CURRENT, $
	            NAME        = 'AFG Bz', $
	            XRANGE      = xrange, $
	            XTITLE      = 'Frequency (Hz)', $
	            YTITLE      = 'Power!C(Log$\down10$)', $
	            YRANGE      = yrange)
	opz1 = MrPlot(dfg_f, dfg_peak_power[2,*], OVERPLOT=pZ, COLOR='Red',  NAME='DFG Bz')
	opz2 = MrPlot(sc_f,  sc_peak_power[2,*],  OVERPLOT=pZ, COLOR='Blue', NAME='SCM Bz')
	
	;Legend
	gleg = MrLegend(/AUTO_TEXT_COLOR, $
	                ALIGNMENT    = 'NE', $
	                LABEL        = ['AFG', 'DFG', 'SCM'], $
	                NAME         = 'Legend', $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH = 0, $
	                TARGET       = [pX, opx1, opx2])

	win -> Refresh
	return, win
end