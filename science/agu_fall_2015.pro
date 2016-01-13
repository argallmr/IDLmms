; docformat = 'rst'
;
; NAME:
;       chapman_2015.pro
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
;   Create a set of plots used for the Chapman Conference in 2015
;
; Categories
;   Conference
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
;       2015-09-24  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Focus on Pc5 waves observed on 2016-08-16 between 09:30:00 and 10:30:00
;-
function AGU2015_epar_20151016
	compile_opt strictarr
	on_error, 2
	
	;Inputs
	sc      = 'mms1'
	mode    = 'brst'
	tstart  = '2015-10-16T13:06:40Z'
	tend    = '2015-10-16T13:07:20Z'

;-----------------------------------------------------
; Create and Doctor Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Compute the exact time range
	win = mms_fig_fsm_epar(sc, mode, tstart, tend)
	win -> Refresh, /DISABLE

	cgSetColorState, 0
	color = mms_color(['blue', 'green', 'red'], DECOMPOSED=0)

	win            -> SetProperty, OXMARGIN=[14,4]
	win            -> SetGlobal, XTICKS=4, XRANGE=hms_to_ssm(['13:06:40', '13:07:20'])
	win['Bx']      -> SetProperty, YRANGE=[-40,40], COLOR=color[0]
	win[1]         -> SetProperty, COLOR=color[1]
	win[2]         -> SetProperty, COLOR=color[2]
	win['E Par']   -> SetProperty, YRANGE=[-30,30]
	win['E PerpX'] -> SetProperty, YRANGE=[-30,30]
	win['E PerpY'] -> SetProperty, YRANGE=[-30,30]
	win['E PerpZ'] -> SetProperty, YRANGE=[-30,30]
	win -> Refresh
	
	cgSetColorState, 1
	
	return, win
end



;+
;   Focus on Pc5 waves observed on 2016-08-16 between 09:30:00 and 10:30:00
;-
function AGU2015_spectra_20150815
	compile_opt strictarr
	on_error, 2
	
	;Inputs
	sc      = 'mms1'
	mode    = 'slow'
	tstart  = '2015-08-15T08:30:00Z'
	tend    = '2015-08-15T10:00:00Z'

;-----------------------------------------------------
; Create and Doctor Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Compute the exact time range
	win = mms_fig_fsmfgmscm_spectra(sc, mode, tstart, tend)
	win -> Refresh, /DISABLE
	
	win -> SetGlobal, RANGE=[1e-6, 1e4]
	
	win -> Refresh
	return, win
end


;+
;   Focus on Pc5 waves observed on 2016-08-16 between 09:30:00 and 10:30:00
;-
function AGU2015_psd_20150804
	compile_opt strictarr
	on_error, 2
	
	;Inputs
	sc      = 'mms1'
	mode    = 'brst'
	tstart  = '2015-08-04T16:21:13Z'
	tend    = '2015-08-04T16:21:14Z'

;-----------------------------------------------------
; Create and Doctor Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Compute the exact time range
	win = mms_fig_fsm_psd(sc, mode, tstart, tend)
	win -> Refresh, /DISABLE
	
	win -> SetProperty, XSIZE=510, YSIZE=710
	win -> SetGlobal, XRANGE=[1e1, 2048], YRANGE=[1e-12, 1e1]
;	win[3] -> Order, /BRING_TO_FRONT
;	win[2] -> Order, /BRING_TO_FRONT
;	win[1] -> Order, /BRING_TO_FRONT
	
	win -> Refresh
	return, win
end


;+
;   Focus on Pc5 waves observed on 2016-08-16 between 09:30:00 and 10:30:00
;-
function AGU2015_psd_20150815
	compile_opt strictarr
	on_error, 2
	
	;Inputs
	sc      = 'mms1'
	mode    = 'slow'
	tstart  = '2015-08-15T09:00:30Z'
	tend    = '2015-08-15T09:01:30Z'

;-----------------------------------------------------
; Create and Doctor Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Compute the exact time range
	win = mms_fig_fsm_psd(sc, mode, tstart, tend)
	win -> Refresh, /DISABLE
	
	win -> SetProperty, XSIZE=510, YSIZE=710
	win -> SetGlobal, YRANGE=[1e-8, 1e1], XRANGE = [1e-2, 16]

;	win[3] -> Order, /BRING_TO_FRONT
;	win[2] -> Order, /BRING_TO_FRONT
;	win[1] -> Order, /BRING_TO_FRONT
	
	win -> Refresh
	return, win
end


;+
;   Focus on Pc5 waves observed on 2016-08-16 between 09:30:00 and 10:30:00
;-
function AGU2015_currents_20151016
	compile_opt strictarr
	on_error, 2
	
	;Inputs
	mode    = 'brst'
	tstart  = '2015-10-16T13:06:05Z'
	tend    = '2015-10-16T13:07:30Z'

;-----------------------------------------------------
; Create and Doctor Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Compute the exact time range
	win = mms_fig_fsmfgm_currents(mode, tstart, tend)
	win -> Refresh, /DISABLE
	
	win             -> SetProperty, OXMARGIN=[10,4]
	win             -> SetGlobal,   XRANGE=hms_to_ssm(['13:06:47', '13:07:05'])
	win['Jx FSM']   -> SetProperty, YRANGE=[-1.0, 1.0]
	win['Jy FSM']   -> SetProperty, YRANGE=[-0.5, 1.5]
	win['Jz FSM']   -> SetProperty, YRANGE=[-0.5, 1.0]
	win['DivB FSM'] -> SetProperty, YRANGE=[-1.0, 1.0]
	
	win -> Refresh
	return, win
end


;+
;   Generate a figure.
;
; :Params:
;       FIGURE:             in, required, type=string
;                           Name of the figure to create.
;-
function agu_fall_2015, figure, $
EPS=eps, $
PS=ps, $
PNG=png, $
IM_PNG=im_png, $
SAVE=tf_save
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Create the figure
	win = obj_new()
	case strupcase(figure) of
		'FSM CURRENTS':      win = AGU2015_currents_20151016()
		'FSM EMIC PSD':      win = AGU2015_psd_20150815()
		'FSM EMIC SPECTRA':  win = AGU2015_spectra_20150815()
		'FSM CHORUS PSD':    win = AGU2015_psd_20150804()
		'FSM E PAR':         win = AGU2015_epar_20151016()
		else: message, 'Figure name not recognized: "' + figure + '".'
	endcase
	
	
	if keyword_set(tf_save) then begin
		;Save as what?
		eps    = keyword_set(eps)
		ps     = keyword_set(ps)
		png    = keyword_set(png)
		im_png = keyword_set(im_png)
		if eps + png + ps + im_png eq 0 then begin
			eps    = 1
			ps     = 1
			png    = 1
			im_png = 1
		endif

		;Number of files and where to save them.
		nWins = n_elements(win)
		froot = '/home/argall/figures/'

		;Single window
		if nWins eq 1 then begin
			;Create the file name
			fname = 'AGUfall2015_' + idl_validname(figure, /CONVERT_ALL)
			fbase = filepath(fname, ROOT_DIR=froot)
	
			;Take a snapshot
			if png then begin
				;Set a true type font
				device, SET_FONT='Helvetica Bold', /TT_FONT
				win -> SetGlobal, FONT=1
			
				win.SAVEAS -> SetProperty, IM_RASTER=0
				win -> Save, fbase + '-ss.png'
				win.SAVEAS -> SetProperty, IM_RASTER=1
			endif
	
			;
			; PostScript
			;
			
			;Hardware fonts
			win -> SetGlobal, FONT=0;, CHARSIZE=1.0
	
			;Save a variety of file types.
			if im_png then win -> Save, fbase + '_im.png'
			if eps    then win -> Save, fbase + '.eps'
			if ps     then win -> Save, fbase + '.ps'
		
		;Multiple windows
		endif else begin
			for i = 0, nWins - 1 do begin
				fname = FilePath('MrProximity_' + fnames[i], ROOT_DIR=froot)
				if im_png then win[i] -> Save, fname + '_im.png'
				if eps    then win[i] -> Save, fname + '.eps'
				if ps     then win[i] -> Save, fname + '.ps'
				
				;Snapshot
				if png then begin
					win[i].SAVEAS -> SetProperty, IM_RASTER=0
					win[i] -> Save, fname + '-ss.png'
					win[i].SAVEAS -> SetProperty, IM_RASTER=1
				endif
			endfor
		endelse
	endif
	
	;Refresh the window
	return, win
end