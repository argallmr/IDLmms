; docformat = 'rst'
;
; NAME:
;       MMS_FSM_FGM_L2
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
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
;   A Batch file for creating L2 data in OMB coordinates.
;
;   Calling Sequence:
;       mms_fsm_fgm_l2 sc instr mode tstart
;       mms_fsm_fgm_l2 sc instr mode tstart fgm_cal_yversion
;       mms_fsm_fgm_l2 sc instr mode tstart fgm_cal_yversion logfile
;       mms_fsm_fgm_l2 sc instr mode tstart logfile
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
;       2017-03-14  -   Written by Matthew Argall
;       2017-03-18  -   Log messages are appended to input log file. - MRA
;       2017-08-08  -   Search for specific Y-Versions of FGM cal files. - MRA
;-
;*****************************************************************************************

;-------------------------------------------
; Configure IDL ////////////////////////////
;-------------------------------------------
fsm_idl_root = GetEnv('FSM_IDL_ROOT')
!Path = Expand_Path('<IDL_DEFAULT>')
!Path = !Path + Expand_Path('+'+fsm_idl_root+'/')
mms_fsm_config
mms_fsm_startup

;-------------------------------------------
; Parse Inputs /////////////////////////////
;-------------------------------------------

;args    = StrArr(5)
;args[0] = 'mms1'
;args[1] = 'dfg'
;args[2] = 'brst'
;args[3] = '20161123175334'
;args[4] = '52'
;args[5] = '/nfs/fsm/logs/mms1/fsm/brst/l3/2016/12/29/mms1_fsm_brst_l3_20161229035834_20170714_084322.log'
;nArgs   = N_Elements(args)

;Arguments passed in from the command line
;   - mms_fsm_fgm_l2 sc instr mode tstart
args = Command_Line_Args(COUNT=nArgs)

; mms_fsm_fgm_l2 sc instr mode tstart
IF nArgs EQ 4 THEN BEGIN & $
	logfile = 'stderr' & $

; mms_fsm_fgm_l2 sc instr mode tstart fgm_cal_yversion
; mms_fsm_fgm_l2 sc instr mode tstart logfile
ENDIF ELSE IF nArgs EQ 5 THEN BEGIN & $
	IF StRegEx(args[4], '^[0-9]+$', /BOOLEAN) $
		THEN fgm_cal_yversion = args[4] $
		ELSE logfile          = args[4] & $
	
; mms_fsm_fgm_l2 sc instr mode tstart fgm_cal_yversion logfile
ENDIF ELSE IF nArgs EQ 6 THEN BEGIN & $
	fgm_cal_yversion = args[4] & $
	logfile          = args[5] & $
	
; Error
ENDIF ELSE BEGIN & $
	MrPrintF, 'LogErr', 'Incorrect number of arguments (' + StrTrim(nArgs, 2) + ').' & $
	Exit, STATUS=200 & $
ENDELSE

;Inputs
sc      = args[0]
instr   = args[1]
mode    = args[2]
level   = 'l2'
optdesc = 'temp'
tstart  = args[3]
file    = StrJoin([sc, instr, mode, level, optdesc, tstart, 'v0.0.0.cdf'], '_')

;-------------------------------------------
; Log File /////////////////////////////////
;-------------------------------------------

;Open the log file
;   - Append to the end
IF nArgs EQ 5 THEN BEGIN & $
	oLog = MrStdLog(logfile, /APPEND) & $
	lun  = oLog.lun & $
ENDIF ELSE lun = -2

;-------------------------------------------
; Configure IDL ////////////////////////////
;-------------------------------------------

;Configure system variables
mms_fg_config
!mms_fg.logu = lun

;-------------------------------------------
; Search for Cal Files /////////////////////
;-------------------------------------------
IF N_Elements(fgm_cal_yversion) GT 0 THEN BEGIN & $
	;Lo-Range Cal
	lo_cal_files = mms_find_file( sc, instr, 'lorangecal', 'l2pre', $
	                              COUNT     = nLoCal, $
	                              /RELAXED_TSTART, $
	                              SDC_ROOT  = !mms_fg.cal_path_root, $
	                              SEARCHSTR = str, $
	                              VERSION   = '*.' + fgm_cal_yversion + '.*' ) & $
	
	;Make sure cal file was found
	IF nLoCal EQ 0 THEN BEGIN & $
		MrPrintF, 'LogErr', 'Unable to find lo-range cal file: "' + str + '".' & $
		Exit, STATUS=201 & $
	ENDIF & $
	
	;Hi-Range Cal
	hi_cal_files = mms_find_file( sc, instr, 'hirangecal', 'l2pre', $
	                              COUNT     = nHiCal, $
	                              /RELAXED_TSTART, $
	                              SDC_ROOT  = !mms_fg.cal_path_root, $
	                              SEARCHSTR = str, $
	                              VERSION   = '*.' + fgm_cal_yversion + '.*' ) & $
	
	;Make sure cal file was found
	IF nHiCal EQ 0 THEN BEGIN & $
		MrPrintF, 'LogErr', 'Unable to find lo-range cal file: "' + str + '".' & $
		Exit, STATUS=201 & $
	ENDIF & $
ENDIF

;-------------------------------------------
; Create L2 Data ///////////////////////////
;-------------------------------------------

;Create data
data = mms_fg_l2pre( sc, instr, mode, tstart, $
                     HI_CAL_FILES = hi_cal_files, $
                     /L2, $
                     LO_CAL_FILES = lo_cal_files, $
                     /SIDEEXIT, $
                     STATUS       = status )

;MMS_FG_L2Pre closes the log file. Open it again.
oLog = MrStdLog(logfile, /APPEND)

;-------------------------------------------
; Create Variables /////////////////////////
;-------------------------------------------

;Variable names
t_vname        = 'Epoch'
b_vname        = StrJoin([sc, instr, 'b',       'omb',  mode, level], '_')
flag_vname     = StrJoin([sc, instr, 'flag',            mode, level], '_')
l1a_mode_vname = StrJoin([sc, instr, 'l1a',     'mode', mode, level], '_')
rate_vname     = StrJoin([sc, instr, 'rate',            mode, level], '_')
range_vname    = StrJoin([sc, instr, 'hirange',         mode, level], '_')
stemp_vname    = StrJoin([sc, instr, 'stemp',           mode, level], '_')
etemp_vname    = StrJoin([sc, instr, 'etemp',           mode, level], '_')
mpa_vname      = StrJoin([sc, instr, 'mpa',             mode, level], '_')

;Convert data to MrVariables
oEpoch = MrTimeVar(data.time, 'TT2000', /CACHE, NAME=t_vname)
oB     = MrVectorTS(oEpoch, data.bomb,        /CACHE, NAME=b_vname)
oFlag  = MrScalarTS(oEpoch, Byte(data.bflag), /CACHE, NAME=flag_vname)
oMode  = MrScalarTS(oEpoch, data.l1a_mode,    /CACHE, NAME=l1a_mode_vname)
oRate  = MrScalarTS(oEpoch, data.brate,       /CACHE, NAME=rate_vname)
oRange = MrScalarTS(oEpoch, data.bhirange,    /CACHE, NAME=range_vname)
oSTemp = MrScalarTS(oEpoch, data.stemp,       /CACHE, NAME=stemp_vname)
oETemp = MrScalarTS(oEpoch, data.etemp,       /CACHE, NAME=etemp_vname)
oMPA   = MrVectorTS(oEpoch, data.mpa,         /CACHE, NAME=mpa_vname)

;-------------------------------------------
; Write to File ////////////////////////////
;-------------------------------------------
;Parent files
IF hi_cal_files[0] NE '' THEN parents = hi_cal_files
IF lo_cal_files[0] NE '' THEN parents = N_Elements(parents) EQ 0 ? lo_cal_files : [parents, lo_cal_files]
MrPrintF, 'LogText', '---------------------------'
MrPrintF, 'LogText', '| Parent Cal Files        |'
MrPrintF, 'LogText', '---------------------------'
MrPrintF, 'LogText', parents

;Create global attributes
parents = File_BaseName(parents)
pos     = StRegEx(parents, '\.cdf$', LENGTH=len)
iCDF = Where( pos NE -1, nCDF )
IF nCDF GT 0 THEN parents[iCDF] = 'CDF>' + StrMid(parents[iCDF], 0, Transpose(pos))
global_attrs = hash( 'Parents', parents )

;Write to file
file = FilePath(file, ROOT_DIR=!mms_fg.dropbox_root)
MrVar_ExportToCDF, file, '', global_attrs, /CLOBBER

;Return the file to standard output
MrPrintF, 'LogText', ''
MrPrintF, 'LogText', '==============================='
MrPrintF, 'LogText', 'Finished creating ' + StrUpCase( StrJoin( [sc, instr, mode, tstart], ' ' ) )
MrPrintF, 'LogText', 'Temporary file written to:'
MrPrintF, 'LogText', '    ' + file
MrPrintF, 'LogText', '==============================='

;Print the file name to standard out
;   - MATLAB's system function captures text only from stderr and stdout
;   - File name must be last line written to stdout or stderr
MrPrintF, 'StdOut', 'Temporary file written to:'
MrPrintF, 'StdOut', file

Exit, STATUS=status
;END