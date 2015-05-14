; docformat = 'rst'
;
; NAME:
;    MrSim_Create
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
;   The purpose of this program is to provide information about the available simulations.
;
; :Categories:
;    Bill Daughton, Simulation
;
; :Params:
;       THESIM:         in, optional, type=string/integer
;                       Name or index of the simulation to be created. If not present,
;                           a list of all available simulations will be printed to the
;                           display window.
;
; :Keywords:
;       ASCII_VERSION:  in, optional, type=integer, default=1
;                       Version of the ASCII info file. Ignored if `BINDARY`=1.
;                           See MrSim_Which.pro.
;       BINARY:         in, optional, type=boolean, default=0
;                       If set, `INFO_FILE` points to the binary info file.
;       DIRECTORY:      in, optional, type=string, default=pwd
;                       Directory in which to find the ".gda" data.
;       INFO_FILE:      in, optional, type=string, default=`DIRECTORY`/../info`
;                       The ASCII info file containing information about the simulation
;                           setup. If `BINARY` is set, the default file will be
;                           `DIRECTORY`/info.
;       SIM_INFO:       in, optional, type=boolean, default=0
;                       If set, information about each simulation will be printed to the
;                           command window. Initialization will be aborted.
;       XRANGE:         in, optional, type=dblarr(2), default=width of domain
;                       X-range over which data will be read and stored, in units defined
;                           by `ION_SCALE`.
;       YRANGE:         in, optional, type=dblarr(2), default=depth of domain
;                       Y-range over which data will be read and stored, in units defined
;                           by `ION_SCALE`. This keyword is ignored in 2D simulations.
;       ZRANGE:         in, optional, type=dblarr(2), default=height of domain
;                       Z-range over which data will be read and stored, in units defined
;                           by `ION_SCALE`.
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 113
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@wildcats.unh.edu
;
; :History:
;    Modification History::
;       2013/09/12  -   Written by Matthew Argall
;       2014/10/03  -   Added the ASCII_VERSION keyword. - MRA
;       2014/10/11  -   Forgot the DIRECTORY keyword. Fixed. - MRA
;-
function mms_ql_figure1
	compile_opt strictarr
	on_error, 2

	;General inputs
	sc      = 'mms3'
	tstart  = '2015-05-07T05:00:00Z'
	tend    = '2015-05-07T08:00:00Z'
	dfg_dir = '/Users/argall/Documents/Work/Data/MMS/DFG/'
	edp_dir = '/Users/argall/Documents/Work/Data/MMS/EDP/'

;-------------------------------------------------------
; DFG //////////////////////////////////////////////////
;-------------------------------------------------------
	instr = 'dfg'
	mode  = 'srvy'
	level = 'ql'

	;Create DFG file name
	fname_dfg = mms_construct_filename(sc, instr, mode, level, $
	                                   DIRECTORY = dfg_dir, $
	                                   /TOKENS)

	;Search for the file
	files_dfg = MrFile_Search(fname_dfg, /CLOSEST, $
	                          COUNT     = nfiles, $
	                          TIMEORDER = '%Y%M%d', $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	if nfiles eq 0 then message, 'No DFG file found: "' + fname_dfg + '".'

	;Create variable names
	b_vname = mms_construct_varname(sc, instr, mode, OPTDESC='dmpa')
	
	;Read the data
	b_ql_dmpa = MrCDF_Read(files_dfg, b_vname, $
	                       DEPEND_0  = epoch_dfg, $
	                       REC_START = tstart, $
	                       REC_END   = tend)

;-------------------------------------------------------
; EDP //////////////////////////////////////////////////
;-------------------------------------------------------
	instr   = 'edp'
	mode    = 'brst'
	level   = 'ql'
	optdesc = 'dce2d'

	;Create DFG file name
	fname_edp = mms_construct_filename(sc, instr, mode, level, $
	                                   DIRECTORY = edp_dir, $
	                                   OPTDESC   = optdesc, $
	                                   TSTART    = '%Y%M%d%H%m%S')

	;Search for the file
	files_edp = MrFile_Search(fname_edp, /CLOSEST, $
	                          COUNT     = nfiles, $
	                          TIMEORDER = '%Y%M%d%H%m%S', $
	                          TSTART    = tstart, $
	                          TEND      = tend)
	if nfiles eq 0 then message, 'No DFG file found: "' + fname_dfg + '".'

	;Create variable names
	e_vname = mms_construct_varname(sc, instr, 'dce_xyz', OPTDESC='dsl')
	
	;Read the data
	e_ql_dmpa = MrCDF_nRead(files_edp, e_vname, $
	                        DEPEND_0  = epoch_edp, $
	                        TSTART    = tstart, $
	                        TEND      = tend)

;-------------------------------------------------------
; Plot /////////////////////////////////////////////////
;-------------------------------------------------------
	;Create a title
	title = strupcase(sc) + ' ' + strupcase(instr) + ' ' + strmid(tstart, 0, 10)
	
	;Convert time to seconds since midnight
	MrCDF_Epoch, epoch_dfg[0], yr, mo, day, /BREAKDOWN_EPOCH
	MrCDF_Epoch, t0, yr, mo, day, /COMPUTE_EPOCH
	t_ssm_dfg = MrCDF_epoch2sse(epoch_dfg, t0)
	t_ssm_edp = MrCDF_epoch2sse(epoch_edp, t0)
	
	;Create a window
	win = MrWindow(XSIZE=600, YSIZE=550, XGAP=0)

	;DFG Plot
	pDFG = MrPlot(t_ssm_dfg, b_ql_dmpa, /CURRENT, $
	              DIMENSION   = 2, $
	              COLOR       = ['Red', 'Forest Green', 'Blue', 'Black'], $
	              NAME        = 'B DFG', $
	              TITLE       = title, $
	              XTITLE      = 'Time UT', $
	              XTICKFORMAT = 'time_labels', $
	              YTITLE      = 'B$\downDFG$!C(nT)')

	;EDP Plot
	pEDP = MrPlot(t_ssm_edp, e_ql_dmpa, /CURRENT, $
	              DIMENSION   = 2, $
	              COLOR       = ['Red', 'Forest Green', 'Blue', 'Black'], $
	              NAME        = 'B DFG', $
	              XTICKFORMAT = '(a1)', $
	              YTITLE      = 'E!C(mV/m)')
	
	;DFG legend
	lDFG = MrLegend(ALIGNMENT        = 'NE', $
	                LABEL            = ['B$\downX$', 'B$\downY$', 'B$\downZ$', '|B|'], $
	                POSITION         = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH     = 0.0, $
	                TARGET           = pDFG, $
	                TEXT_COLOR       = ['Red', 'Forest Green', 'Blue', 'Black'], $
	                VERTICAL_SPACING = 1.5)
	
	;EDP Legend
	lDFG = MrLegend(ALIGNMENT        = 'NE', $
	                LABEL            = ['E$\downX$', 'E$\downY$'], $
	                POSITION         = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH     = 0.0, $
	                TARGET           = pEDP, $
	                TEXT_COLOR       = ['Red', 'Forest Green'], $
	                VERTICAL_SPACING = 1.5)
	
	return, win
end