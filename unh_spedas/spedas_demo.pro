; docformat = 'rst'
;
; NAME:
;    spedas_demo
;
; PURPOSE:
;+
;   Demo basics of SPEDAS.
;
; :Categories:
;    SPEDAS
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
;-
;*****************************************************************************************
;Initialize tplot
;   - TPlot required IDL to be in 8-bit color mode (DECOMPOSED=0)
;   - Load the rainbow color table
;   - Put the color white rgb = [255,255,255] at the top of the color table
;     so that the axes are white, not red.
tplot
device, DECOMPOSED=0
loadct, 13
tvlct, 255, 255, 255, 255


;-----------------------------------------------------
; 2D Time Series Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;Create data
;   - Time is stored as Unix times:
;       - Seconds elapsed since January 1, 1970 UTC
;       - Does not include leap seconds
;
t0   = systime(1)                    ;Current time in Unix foramt
time = t0 + findgen(100)             ;Artificial time array of 100 seconds
y1   = sin(2*!pi*(time-t0)/99.0)        ;Sine wave, one period over 100 seconds
y2   = cos(2*!pi*(time-t0)/99.0)        ;Cosine wave, one period over 100 seconds
y3   = tan(2*!pi*(time-t0)/99.0)        ;Tangent wave, one period over 100 seconds

;
;Store data as a TPlot variable
;   - Time series are stored as structures with fields "x" and "y"
;
data1 = { x: time, y: y1 }
data2 = { x: time, y: y2 }
data3 = { x: time, y: y3 }
store_data, 'SineWave',    DATA=data1
store_data, 'CosineWave',  DATA=data2
store_data, 'TangentWave', DATA=data3

;See which data are stored
tplot_names

;-----------------------------------------------------
; Plot Single Variable \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;Plot a single variable
;   - Can use either the variable number or variable name
tplot, 'SineWave'

;Update axis properties
;   - Can set any IDL axis property: 
options, 'SineWave', 'ytitle', 'Amplitude'
options, 'SineWave', 'title', 'A Sine Wave'
options, 'SineWave', 'yrange', [-2, 2]

;Update the plot with new properties
tplot

;-----------------------------------------------------
; Stack Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;Use the /ADD_VAR keyword to tplot
;   - Adds plot above original
tplot, 'CosineWave', /ADD_VAR
tplot, 'TangentWave', /ADD_VAR

;Or give an array of variables
;   - This time I will use variable numbers
tplot, [1,2,3]

;Set Properties
options, 'CosineWave',  'ytitle', 'Amplitude'
options, 'TangentWave', 'ytitle', 'Amplitude'
tplot

;-----------------------------------------------------
; OverPlot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;To plot data one on top of the other
;   - First overplot the cosine, then the tangent
tplot, 'SineWave'
tplot_panel, v='SineWave', o='CosineWave'
tplot_panel, v='SineWave', o='TangentWave'

;
;TPLOT_PANEL appears to be temporary
;   - Calling tplot to refresh after setting properties erases the overplots
;   - Also, properties of overplots are not applied -- symbols to not change.
;
options, 'CosineWave',  'psym', 2
options, 'TangentWave', 'psym', 3
tplot
tplot_panel, v='SineWave', o='CosineWave'
tplot_panel, v='SineWave', o='TangentWave'

;-----------------------------------------------------
; Delete Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;Delete variables
store_data, DELETE=['SineWave', 'CosineWave', 'TangentWave']

;Confirm they were deleted
tplot_names

;-----------------------------------------------------
; 3D Spectra \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;Create data for a power spectra
nSec = 3600
time = t0 + findgen(nSec)

;Amplitude and frequency
amp  = [2.0, 0.80, 1.10]
f    = [0.4, 0.03, 0.17]

;Create the wave
wave = fltarr(nSec)
for i = 0, n_elements(f) - 1 do wave += amp[i] * sin(2*!pi*(time-t0)*f[i])

;Do a 5-point smooth to broaden the signal
;wave = smooth(wave, 5, /EDGE_TRUNCATE)

;Add random noise of amplitude 0.1.
wave += 0.2 * (randomu(5, nSec) - 0.5)

;Store the data as a tplot variable
store_data, 'Wave', DATA={ x: time, y: wave }


;Frequency bins of the resulting FFT
dt     = 1.0
nfft   = 512
nshift = nfft / 2
df     = 1.0 / (dt * nfft)
freqs  = findgen( (nfft - 1) / 2.0) + 1
freqs  = df * [0.0, freqs, nfft/2, -nfft/2 + freqs]

;Compute the FFT
nWin     = floor((nSec - nfft) / nshift) + 1
wave_fft = fltarr(nWin, nfft)
for i = 0, nWin - 1 do wave_fft[i,*] = fft(wave[i*nshift:i*nshift+nfft-1])

;Power spectra
wave_fft = wave_fft[*,0:nfft/2]
wave_psd = 2.0 / dt * sqrt( abs(wave_fft[*, 1:*]) )

;Save the data
t_temp = t0 + findgen(nWin) / (nWin-1) * nSec
store_data, 'WavePSD', DATA={x: t_temp, y: wave_psd, v: freqs[1:nfft/2]}, LIM={spec: 1}

;Plot the data
tplot, 'WavePSD'






t0   = systime(1)
time = t0 + findgen(100)
freq = 32 * findgen(256) / 255.0
data = dist(100, 256)
store_data, 'Spec', DATA = {x: time, y: data/137, v: freq}, LIM={spec: 1}