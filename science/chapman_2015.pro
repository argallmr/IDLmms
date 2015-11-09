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
function chapman_2015_pc5_20150816
	compile_opt strictarr
	on_error, 2
	
	;Inputs
	sc      = 'mms1'
	tstart  = '2015-08-16T09:30:00Z'
	tend    = '2015-08-16T10:30:00Z'
	radial  = 1B

;-----------------------------------------------------
; Create and Doctor Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Compute the exact time range
	MrCDF_Epoch_Compute, x0, 2015, 8, 16, 09, 30, /TT2000
	MrCDF_Epoch_Compute, x1, 2015, 8, 16, 10, 30, /TT2000
	xrange = MrCDF_epoch2ssm([x0, x1])
	
	;FGM QL SRVY file
	win = mms_fig_efield_fac(sc, tstart, tend, RADIAL=radial)
	win -> Refresh, /DISABLE
	
	;Set properties
	win                   -> SetProperty, OXMARGIN=[10,6], YSIZE=690
	win['B FGM']          -> SetProperty, TITLE=strupcase(sc) + ' ' + strmid(tstart, 0, 10)
	win['Q0 Counts GDU1'] -> SetProperty, YRANGE=[0,40]
	win                   -> SetGlobal, XTICKS=6, XMINOR=10, XTICKLEN=0.05, XRANGE=xrange
	
	return, win
end


;+
;   Focus on SAPS observed on 2016-08-16 between 08:00:00 and 09:00:00
;-
function chapman_2015_saps_20150816
	compile_opt strictarr
	on_error, 2
	
	sc      = 'mms1'
	tstart  = '2015-08-16T08:00:00Z'
	tend    = '2015-08-16T09:00:00Z'
	radial  = 1B

;-----------------------------------------------------
; Create and Doctor Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Compute the exact time range
	MrCDF_Epoch_Compute, x0, 2015, 08, 16, 8, 0, /TT2000
	MrCDF_Epoch_Compute, x1, 2015, 08, 16, 9, 0, /TT2000
	xrange = MrCDF_epoch2ssm([x0, x1])
	
	;FGM QL SRVY file
	win = mms_fig_vdrift_fac(sc, tstart, tend, RADIAL=radial)
	win -> Refresh, /DISABLE
	
	;Set properties
	win                   -> SetProperty, OXMARGIN=[10,6], YSIZE=690
	win['B FGM']          -> SetProperty, TITLE=sc + ' ' + strmid(tstart, 0, 10)
	win['Vperp2 ExB']     -> SetProperty, YRANGE=[-20,15]
	win['Q0 Counts GDU1'] -> SetProperty, YRANGE=[0,25]
	win                   -> SetGlobal, XTICKS=6, XMINOR=10, XTICKLEN=0.05, XRANGE=xrange
	win['B legend']       -> SetProperty, ORIENTATION=1
	win['E legend']       -> SetProperty, ORIENTATION=1
	win['E legend']       -> SetProperty, 0, LABEL='Er'
	win['E legend']       -> SetProperty, 1, LABEL='E$\phi$'
	win['Vperp1 legend']  -> SetProperty, ORIENTATION=1
	win['Vperp2 legend']  -> SetProperty, ORIENTATION=1
	win['Vperp1 ExB']     -> SetProperty, YTITLE = 'Vr!C(km/s)'
	win['Vperp2 ExB']     -> SetProperty, YTITLE = 'V$\phi$!C(km/s)'
	
	return, win
end


;+
;   Focus on Dipolarization Front observed on 2016-08-29 between 00:00:00 and 04:00:00
;-
function chapman_2015_saps_20150829
	compile_opt strictarr
	on_error, 2
	
	sc      = 'mms1'
	tstart  = '2015-08-29T03:00:00Z'
	tend    = '2015-08-29T03:30:00Z'
	radial  = 0B
	bc      = 0B
	
	;Compute the exact time range
	MrCDF_Epoch_Compute, x0, 2015, 08, 29, 3, 05, /TT2000
	MrCDF_Epoch_Compute, x1, 2015, 08, 29, 3, 20, /TT2000
	xrange = MrCDF_epoch2ssm([x0, x1])
	
	;Create the plot
	win = mms_fig_vdrift_fac(sc, tstart, tend, BC=bc, RADIAL=radial)
	win -> Refresh, /DISABLE
	
	;Set global properties
	win['B FGM']          -> SetProperty, TITLE=strupcase(sc) + ' ' + strmid(tstart, 0, 10)
;	win['Eperp1 EDP']     -> SetProperty, YRANGE=[-3,3], YTICKS=4, YMINOR=3
;	win['Eperp2 EDP']     -> SetProperty, YRANGE=[-3,3], YTICKS=4, YMINOR=3
;	win['EpAR EDP']       -> SetProperty, YRANGE=[-3,3], YTICKS=4, YMINOR=3
;	win['Q0 Counts GDU1'] -> SetProperty, YRANGE=[0,650]
	win                   -> SetGlobal, XTICKS=3, XMINOR=5, XTICKLEN=0.05, XRANGE=xrange
	
	return, win
end


;+
;   Focus on Dipolarization Front observed on 2016-08-16 between 03:00:00 and 03:30:00
;-
function chapman_2015_dpf_20150816
	compile_opt strictarr
	on_error, 2
	
	sc      = 'mms1'
	tstart  = '2015-08-16T03:00:00Z'
	tend    = '2015-08-16T03:30:00Z'
	nsmooth = 0
	
	;Create the plot
	win = mms_fig_dpf(sc, tstart, tend, NSMOOTH=nsmooth)
	win -> Refresh, /DISABLE
	
	;Set global properties
	win['Eperp1 EDP'] -> SetProperty, YRANGE=[-6,6], YTICKS=4, YMINOR=3
	win['Eperp2 EDP'] -> SetProperty, YRANGE=[-6,6], YTICKS=4, YMINOR=3
	win               -> SetGlobal, XTICKS=5, XMINOR=6, XTICKLEN=0.04
	
	return, win
end


;+
;   Focus on Dipolarization Front observed on 2016-08-16 between 03:00:00 and 03:30:00
;-
function chapman_2015_dpf_vdrift_20150816
	compile_opt strictarr
	on_error, 2
	
	sc      = 'mms1'
	tstart  = '2015-08-16T03:00:00Z'
	tend    = '2015-08-16T03:30:00Z'

;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;FGM QL SRVY file
	win = mms_fig_vdrift_fac(sc, tstart, tend)
	win -> Refresh, /DISABLE
	
	;Set properties
	win                   -> SetProperty, OXMARGIN=[10,6], YSIZE=690
	win['B FGM']          -> SetProperty, TITLE=sc + ' ' + strmid(tstart, 0, 10)
	win['Q0 Counts GDU1'] -> SetProperty, YRANGE=[0,25]
	win                   -> SetGlobal, XTICKS=5, XMINOR=6, XTICKLEN=0.04
	
	return, win
end


;+
;   Focus on Dipolarization Front observed on 2016-07-21 between 06:00:00 and 07:30:00
;-
function chapman_2015_dpf_20150721
	compile_opt strictarr
	on_error, 2
	
	sc      = 'mms1'
	tstart  = '2015-07-21T06:00:00Z'
	tend    = '2015-07-21T07:30:00Z'
	nsmooth = 0
	
	;Compute the exact time range
	MrCDF_Epoch_Compute, x0, 2015, 07, 21, 6,  0, /TT2000
	MrCDF_Epoch_Compute, x1, 2015, 07, 21, 7, 30, /TT2000
	xrange = MrCDF_epoch2ssm([x0, x1])
	
	;Create the plot
	win = mms_fig_dpf(sc, tstart, tend, NSMOOTH=nsmooth)
	win -> Refresh, /DISABLE
	
	;Set global properties
	win['Bx FGM']     -> SetProperty, TITLE=strupcase(sc) + ' ' + strmid(tstart, 0, 10)
	win['Eperp1 EDP'] -> SetProperty, YRANGE=[-6,6], YTICKS=4, YMINOR=3
	win['Eperp2 EDP'] -> SetProperty, YRANGE=[-6,6], YTICKS=4, YMINOR=3
	win['Q0 Counts GDU1'] -> SetProperty, YRANGE=[0,400]
	win               -> SetGlobal, XTICKS=3, XMINOR=6, XTICKLEN=0.05, XRANGE=xrange
	
	return, win
end


;+
;   Focus on Dipolarization Front observed on 2016-07-21 between 06:00:00 and 07:30:00
;-
function chapman_2015_dpf_vdrift_20150721
	compile_opt strictarr
	on_error, 2
	
	sc      = 'mms1'
	tstart  = '2015-07-21T06:00:00Z'
	tend    = '2015-07-21T07:30:00Z'

;-----------------------------------------------------
; Find Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Compute the exact time range
	MrCDF_Epoch_Compute, x0, 2015, 07, 21, 6,  0, /TT2000
	MrCDF_Epoch_Compute, x1, 2015, 07, 21, 7, 30, /TT2000
	xrange = MrCDF_epoch2ssm([x0, x1])

	;FGM QL SRVY file
	win = mms_fig_vdrift_fac(sc, tstart, tend)
	win -> Refresh, /DISABLE
	
	;Set properties
	win                   -> SetProperty, YSIZE=690
	win['B FGM']          -> SetProperty, TITLE=strupcase(sc) + ' ' + strmid(tstart, 0, 10)
	win['Q0 Counts GDU1'] -> SetProperty, YRANGE=[0,400]
	win                   -> SetGlobal, XTICKS=3, XMINOR=6, XTICKLEN=0.05, XRANGE=xrange
	
	return, win
end


;+
;   Focus on Dipolarization Front observed on 2016-08-29 between 00:00:00 and 04:00:00
;-
function chapman_2015_dpf_20150829
	compile_opt strictarr
	on_error, 2
	
	sc      = 'mms1'
	tstart  = '2015-08-29T00:00:00Z'
	tend    = '2015-08-29T01:45:00Z'
	nsmooth = 0
	
	;Compute the exact time range
	MrCDF_Epoch_Compute, x0, 2015, 08, 29, 0,  0, /TT2000
	MrCDF_Epoch_Compute, x1, 2015, 08, 29, 1, 45, /TT2000
	xrange = MrCDF_epoch2ssm([x0, x1])
	
	;Create the plot
	win = mms_fig_dpf(sc, tstart, tend, NSMOOTH=nsmooth)
	win -> Refresh, /DISABLE
	
	;Set global properties
	win['Bx FGM']         -> SetProperty, TITLE=strupcase(sc) + ' ' + strmid(tstart, 0, 10)
	win['Eperp1 EDP']     -> SetProperty, YRANGE=[-3,3], YTICKS=4, YMINOR=3
	win['Eperp2 EDP']     -> SetProperty, YRANGE=[-3,3], YTICKS=4, YMINOR=3
	win['EpAR EDP']       -> SetProperty, YRANGE=[-3,3], YTICKS=4, YMINOR=3
	win['Q0 Counts GDU1'] -> SetProperty, YRANGE=[0,650]
	win                   -> SetGlobal, XTICKS=3, XMINOR=5, XTICKLEN=0.05, XRANGE=xrange
	
	return, win
end


;+
;   Generate a figure.
;
; :Params:
;       FIGURE:             in, required, type=string
;                           Name of the figure to create.
;-
function chapman_2015, figure
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(win) then obj_destroy, win
		void = cgErrorMSG(/QUIET)
		return, !Null
	endif
	
	;Create the figure
	win = obj_new()
	case strupcase(figure) of
		'DPF 20150816':        win = chapman_2015_dpf_20150816()
		'DPF VDRIFT 20150816': win = chapman_2015_dpf_vdrift_20150816()
		'SAPS 20150816':       win = chapman_2015_saps_20150816()
		'SAPS 20150829':       win = chapman_2015_saps_20150829()
		'PC5 20150816':        win = chapman_2015_pc5_20150816()
		'DPF 20150721':        win = chapman_2015_dpf_20150721()
		'DPF VDRIFT 20150721': win = chapman_2015_dpf_vdrift_20150721()
		'DPF 20150829':        win = chapman_2015_dpf_20150829()
		else: message, 'Figure name not recognized: "' + figure + '".'
	endcase
	
	;Refresh the window
	win -> Refresh
	return, win
end