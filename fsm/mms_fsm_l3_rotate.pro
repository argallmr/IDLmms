; docformat = 'rst'
;
; NAME:
;       MMS_FSM_L3_Rotate
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
;   A Batch file for rotating FSM L3 data from OMB to GSE coordinates.
;
;   Calling Sequence:
;       $ idl mms_fsm_l3_rotate data_file
;       $ idl mms_fsm_l3_rotate data_file log_file
;       $ idl mms_fsm_l3_rotate data_file tf_all
;       $ idl mms_fsm_l3_rotate data_file tf_all log_file
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
;       2017-03-16  -   Written by Matthew Argall
;       2017-10-08  -   Added TF_ALL input parameter to write data in all coordinates
;                           to output file. - MRA
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

;nArgs   = 2
;args    = StrArr(nArgs)
;args[0] = '/nfs/fsm/temp/mms1_fsm_brst_l3_8khz-temp-x_20170711232803_v4.0.0.cdf'
;args[1] = '1'

;Arguments passed in from the command line
;   - mms_fsm_fgm_l2 sc instr mode tstart
args = Command_Line_Args(COUNT=nArgs)
IF nArgs EQ 0 || nArgs GE 3 THEN BEGIN & $
	MrPrintF, 'LogErr', 'Incorrect number of arguments.' & $
	Exit, STATUS=200 & $
ENDIF

;Parse inputs
IF nArgs GE 2 THEN BEGIN & $
	Catch, the_error & $
	IF the_error EQ 0 $
		THEN tf_all  = Keyword_Set(Fix(args[1])) $
		ELSE logfile = args[1] & $
	Catch, /CANCEL & $
ENDIF
IF nArgs EQ 3 THEN logfile = args[2]

;Default Inputs
fsm_file = args[0]
IF N_Elements(logfile) EQ 0 THEN logfile = 'stderr'
IF N_Elements(tf_all)  EQ 0 THEN tf_all  = 0B
mms_dissect_filename, fsm_file, SC=sc, INSTR=instr, MODE=mode, LEVEL=level, TSTART=tstart

;Open the log file
;   - Append to the end
IF logfile NE 'stderr' THEN BEGIN & $
	oLog = MrStdLog(logfile, /APPEND) & $
	lun  = oLog.lun & $
ENDIF ELSE lun = -2

;-------------------------------------------
; Configure Ken's Routines /////////////////
;-------------------------------------------

;Configure system variables
mms_fg_config
!mms_fg.logu = lun

;-------------------------------------------
; Read FSM File ////////////////////////////
;-------------------------------------------

;Variable name
b_omb_vname = StrJoin([sc, instr, 'b', 'omb', mode, level], '_')

;Read the data
MrVar_ReadCDF, fsm_file, $
               VARFORMAT = b_omb_vname

;Grab the variables
oB_omb = MrVar_Get(b_omb_vname)
t      = oB_omb['TIME', 'TT2000']

;T must be Nx1 for the coordinate transforamtion routines to work
t = Reform(t)

;-------------------------------------------
; DEFATT ///////////////////////////////////
;-------------------------------------------

;Read attitude data
;   - Finds attitude files automatically
mms_fg_read_defatt, sc, t, $
                    DEF_ATT   = def_att, $
                    filenames = attfiles, $
                    MPA       = mpa, $
                    Q         = Q, $
                    TT2000    = att_tt2000

;No data found
IF Size(att_tt2000, /TYPE) EQ 0 THEN BEGIN & $
	MrPrintF, 'LogErr', 'No definitive attitude found.' & $
	Exit, STATUS=170 & $
ENDIF

;Allowing 30 sec extrapolation, to let cluster test proceed...
IF att_tt2000[0] GT t[0] || att_tt2000[-1] + uLong64(30.d9) LT t[-1] THEN BEGIN & $
	MrPrintF, 'LogErr', 'Definitive attitude loaded does not cover entire timespan' & $
	Exit, STATUS=171 & $
ENDIF

;-------------------------------------------
; DEFEPH ///////////////////////////////////
;-------------------------------------------

;Read ephemeris data
;   - Finds ephemeris files automatically
mms_fg_read_eph, sc, t, 'defeph', $
                 eph       = def_eph, $
                 tt2000    = eph_tt2000, $
                 filenames = ephfiles

;No data found
IF Size(eph_tt2000, /TYPE) EQ 0 THEN BEGIN & $
	MrPrintF, 'LogErr', 'No definitive ephemeris found.' & $
	Exit, STATUS=180 & $
ENDIF

; subset the data -- adding 15 seconds tolerance to start/stop of 
;                    the ephemeris data, so that we always find 
;                    at least one ephemeris point for bursts
;                    shorter than 30 sec.  (assumes ephemeris 
;                    will always have 30 second cadence.)
keepstate = Where( eph_tt2000+15*Long64(1d9) GE t[0] AND $
                   eph_tt2000-15*Long64(1d9) LE t[-1], nstate)

IF nstate EQ 0 THEN BEGIN & $
  MrPrintF, 'LogErr', 'Definitive ephemeris loaded does not cover entire timespan' & $
  Exit, STATUS=181 & $
ENDIF

; add extra points so we don't have to extrapolate ephemeris later on
eb = keepstate[0] -1
ee = keepstate[-1] +1
IF eb GE 0 THEN keepstate = [eb, keepstate]
IF ee LT N_Elements(eph_tt2000) THEN keepstate = [keepstate, ee]

;-------------------------------------------
; Despin & Rotate //////////////////////////
;-------------------------------------------

; The calibrated data is in OMB
; 
;Rotate  to SMPA
; - 135 degree rotation
;
mms_fg_xomb2smpa, Transpose(oB_omb['DATA']), b_smpa

; transform to GSE, getting BCS and DMPA coordinates as intermediate outputs.
; There are two possible methods: 
; 0) transform BCS to J2000, then J2000 to GSE.    (the default)  Vulnerable to noise on quaternion.
; 1) treat DMPA as equivalent to DSL.  Rotate DMPA to GSE. Valid if nutation is small.
; the use_method keyword to mms_fg_xsmpa2gse can be used to select which method to use.
;    TODO: better fit to L: currently, this routine takes the average of the given def_att data 
;    -- which should be one orbit of data, or possibly two. 
mms_fg_xsmpa2gse, att_tt2000, q, def_att, eph_tt2000, def_eph, mpa, t, b_smpa, b_gse, $
                  DMPA       = b_dmpa, $
                  BCS        = b_bcs, $
                  USE_METHOD = 1 ;*******  this method allows for smoothing the phase: gives slightly lower noise.   
; ***** On the down side, we might see discontinuities at data file boundaries due to change in averaging interval for L from file to file..
; ***** needs more evaluation.

; transform GSE to GSM
mms_fg_xgse2gsm, t, b_gse, b_gsm

;-------------------------------------------
; Create Variables /////////////////////////
;-------------------------------------------

;Variable names
t_vname      = 'Epoch'
b_dmpa_vname = StrJoin([sc, instr, 'b', 'dmpa',  mode, level], '_')
b_gse_vname  = StrJoin([sc, instr, 'b', 'gse',   mode, level], '_')
b_gsm_vname  = StrJoin([sc, instr, 'b', 'gsm',   mode, level], '_')

b_dmpa_labl_vname = StrJoin([sc, instr, 'b', 'dmpa', 'labls', mode, level], '_')
b_gse_labl_vname  = StrJoin([sc, instr, 'b', 'gse',  'labls', mode, level], '_')
b_gsm_labl_vname  = StrJoin([sc, instr, 'b', 'gsm',  'labls', mode, level], '_')

;Convert data to MrVariables
oEpoch  = MrTimeVar( t, 'TT2000', NAME=t_vname )
oB_dmpa = MrVectorTS( oEpoch, b_dmpa, /CACHE, NAME=b_dmpa_vname )
oB_gse  = MrVectorTS( oEpoch, b_gse,  /CACHE, NAME=b_gse_vname  )
oB_gsm  = MrVectorTS( oEpoch, b_gsm,  /CACHE, NAME=b_gsm_vname  )

;Labels
oB_dmpa_labl = MrVariable( ['Bx_DMPA', 'By_DMPA', 'Bz_DMPA'], /CACHE, NAME=b_dmpa_labl_vname )
oB_gse_labl  = MrVariable( ['Bx_GSE',  'By_GSE',  'Bz_GSE'],  /CACHE, NAME=b_gse_labl_vname )
oB_gsm_labl  = MrVariable( ['Bx_GSM',  'By_GSM',  'Bz_GSM'],  /CACHE, NAME=b_gsm_labl_vname )

;-------------------------------------------
; Add Attributes ///////////////////////////
;-------------------------------------------

;NOTE: DEPEND_0 is set automatically by MrVectorTS

; B_DMPA
oB_dmpa['CATDESC']       = 'Three components of the magnetic field in DMPA coordinates.'
oB_dmpa['DISPLAY_TYPE']  = 'time_series'
oB_dmpa['FIELDNAM']      = 'B'
oB_dmpa['FILLVAL']       = -1e31
oB_dmpa['FORMAT']        = 'f11.4'
oB_dmpa['LABL_PTR_1']    = oB_dmpa_labl
oB_dmpa['SI_CONVERSION'] = '1e-9>T'
oB_dmpa['UNITS']         = 'nT'
oB_dmpa['VALIDMIN']      = 0.0
oB_dmpa['VALIDMAX']      = 1e5
oB_dmpa['VAR_TYPE']      = 'data'

; B_GSE
oB_gse['CATDESC']       = 'Three components of the magnetic field in GSE coordinates.'
oB_gse['DISPLAY_TYPE']  = 'time_series'
oB_gse['FIELDNAM']      = 'B'
oB_gse['FILLVAL']       = -1e31
oB_gse['FORMAT']        = 'f11.4'
oB_gse['LABL_PTR_1']    = oB_gse_labl
oB_gse['SI_CONVERSION'] = '1e-9>T'
oB_gse['UNITS']         = 'nT'
oB_gse['VALIDMIN']      = 0.0
oB_gse['VALIDMAX']      = 1e5
oB_gse['VAR_TYPE']      = 'data'

; B_GSM
oB_gsm['CATDESC']       = 'Three components of the magnetic field in GSM coordinates.'
oB_gsm['DISPLAY_TYPE']  = 'time_series'
oB_gsm['FIELDNAM']      = 'B'
oB_gsm['FILLVAL']       = -1e31
oB_gsm['FORMAT']        = 'f11.4'
oB_gsm['LABL_PTR_1']    = oB_gsm_labl
oB_gsm['SI_CONVERSION'] = '1e-9>T'
oB_gsm['UNITS']         = 'nT'
oB_gsm['VALIDMIN']      = 0.0
oB_gsm['VALIDMAX']      = 1e5
oB_gsm['VAR_TYPE']      = 'data'

;Remove attributes that were generated automatically
oB_dmpa -> RemoveAttr, 'DIMENSION'
oB_gse  -> RemoveAttr, 'DIMENSION'
oB_gsm  -> RemoveAttr, 'DIMENSION'

; B_DMPA_LABL
oB_dmpa_labl['CATDESC']  = 'Axis labels for the vector magnetic field in DMPA coordinates.'
oB_dmpa_labl['FIELDNAM'] = 'Magnetic field labels.'
oB_dmpa_labl['FORMAT']   = 'A7'
oB_dmpa_labl['VAR_TYPE'] = 'metadata'

; B_GSE_LABL
oB_gse_labl['CATDESC']  = 'Axis labels for the vector magnetic field in GSE coordinates.'
oB_gse_labl['FIELDNAM'] = 'Magnetic field labels.'
oB_gse_labl['FORMAT']   = 'A6'
oB_gse_labl['VAR_TYPE'] = 'metadata'

; B_GSM_LABL
oB_gsm_labl['CATDESC']  = 'Axis labels for the vector magnetic field in GSM coordinates.'
oB_gsm_labl['FIELDNAM'] = 'Magnetic field labels.'
oB_gsm_labl['FORMAT']   = 'A6'
oB_gsm_labl['VAR_TYPE'] = 'metadata'

;-------------------------------------------
; Write to File ////////////////////////////
;-------------------------------------------

;Show additional parents
parents = File_Basename([attfiles, ephfiles])
MrPrintF, 'LogText', '--------------------------------------------'
MrPrintF, 'LogText', '| FDOA Parents                             |'
MrPrintF, 'LogText', '--------------------------------------------'
MrPrintF, 'LogText', parents

;Add to global attributes
global_attrs = Hash( 'Parents', Temporary(parents) )

;Write to file
MrVar_ExportToCDF, fsm_file, [b_dmpa_vname, b_gse_vname, b_gsm_vname], global_attrs, /MODIFY

;-------------------------------------------
; Re-Order the Variables ///////////////////
;-------------------------------------------

;Open the new and old files
file_new = tf_all ? StrJoin(StrSplit(fsm_file, '-temp', /EXTRACT, /REGEX), '-cs-test') $
                  : StrJoin(StrSplit(fsm_file, '-temp', /EXTRACT, /REGEX))
ocdf_new = MrCDF_File(file_new, /CREATE, /CLOBBER)
ocdf_old = MrCDF_File(fsm_file)

;Copy Global Attributes
gAttrs = ocdf_old -> GetAttrNames(COUNT=nGAttrs)
FOR i = 0, nGAttrs - 1 DO ocdf_old -> CopyGlobalAttrTo, gAttrs[i], ocdf_new

;Copy Variable Attributes
varAttrs = ocdf_old -> GetAttrNames(COUNT=nVarAttrs, /VARIABLE_SCOPE)
FOR i = 0, nVarAttrs - 1 DO ocdf_old -> CopyVarAttrTo, varAttrs[i], ocdf_new

;Copy Variables
;   - Remove vectors from coordinates other than GSE
;   - OMB: 3, 5
;   - DMPA: 19, 20
;   - GSM: 14, 17, 23, 24

;OLD VARIABLES
;  0   Epoch
;  1   mms1_fsm_epoch_delta_brst_l3
;  2   mms1_fsm_b_mag_brst_l3
;  3   mms1_fsm_b_omb_brst_l3
;  4   mms1_fsm_flag_brst_l3
;  5   mms1_fsm_b_omb_labels_brst_l3
;  6   Epoch_fgm
;  7   Epoch_state
;  8   mms1_fsm_bdeltahalf_brst_l3
;  9   mms1_fsm_hirange_brst_l3
; 10   mms1_fsm_stemp_brst_l3
; 11   mms1_fsm_etemp_brst_l3
; 12   mms1_fsm_mode_brst_l3
; 13   mms1_fsm_r_gse_brst_l3
; 14   mms1_fsm_r_gsm_brst_l3
; 15   mms1_fsm_rdeltahalf_brst_l3
; 16   label_r_gse
; 17   label_r_gsm
; 18   represent_vec_tot
; 19   mms1_fsm_b_dmpa_brst_l3
; 20   mms1_fsm_b_dmpa_labls_brst_l3
; 21   mms1_fsm_b_gse_brst_l3
; 22   mms1_fsm_b_gse_labls_brst_l3
; 23   mms1_fsm_b_gsm_brst_l3
; 24   mms1_fsm_b_gsm_labls_brst_l3

;NORMAL OUTPUT
;  0   Epoch
;  1   mms1_fsm_epoch_delta_brst_l3
;  2   mms1_fsm_b_mag_brst_l3
; 21   mms1_fsm_b_gse_brst_l3
;  4   mms1_fsm_flag_brst_l3
; 22   mms1_fsm_b_gse_labls_brst_l3
;  6   Epoch_fgm
;  7   Epoch_state
;  8   mms1_fsm_bdeltahalf_brst_l3
; 15   mms1_fsm_rdeltahalf_brst_l3
; 13   mms1_fsm_r_gse_brst_l3
; 16   label_r_gse
; 18   represent_vec_tot
;  9   mms1_fsm_hirange_brst_l3
; 10   mms1_fsm_stemp_brst_l3
; 11   mms1_fsm_etemp_brst_l3
; 12   mms1_fsm_mode_brst_l3

;OUTPUT ALL
;  0   Epoch
;  1   mms1_fsm_epoch_delta_brst_l3
;  2   mms1_fsm_b_mag_brst_l3
;  3   mms1_fsm_b_omb_brst_l3
; 19   mms1_fsm_b_dmpa_brst_l3
; 21   mms1_fsm_b_gse_brst_l3
; 23   mms1_fsm_b_gsm_brst_l3
;  4   mms1_fsm_flag_brst_l3
;  5   mms1_fsm_b_omb_labels_brst_l3
; 20   mms1_fsm_b_dmpa_labls_brst_l3
; 22   mms1_fsm_b_gse_labls_brst_l3
; 24   mms1_fsm_b_gsm_labls_brst_l3
;  6   Epoch_fgm
;  7   Epoch_state
;  8   mms1_fsm_bdeltahalf_brst_l3
; 15   mms1_fsm_rdeltahalf_brst_l3
; 13   mms1_fsm_r_gse_brst_l3
; 14   mms1_fsm_r_gsm_brst_l3
; 16   label_r_gse
; 17   label_r_gsm
; 18   represent_vec_tot
;  9   mms1_fsm_hirange_brst_l3
; 10   mms1_fsm_stemp_brst_l3
; 11   mms1_fsm_etemp_brst_l3
; 12   mms1_fsm_mode_brst_l3

vnames = ocdf_old -> GetVarNames(COUNT=nVars)
IF tf_all $
	THEN vorder = [0,1,2,3,19,21,23,4,5,20,22,24,6,7,8,15,13,14,16,17,18,9,10,11,12] $
	ELSE vorder = [0,1,2,21,4,22,6,7,8,15,13,16,18,9,10,11,12]
FOR i = 0, N_Elements(vorder)-1 DO ocdf_old -> CopyVariableTo, vnames[vorder[i]], ocdf_new

;Close the new file
Obj_Destroy, ocdf_new
Obj_Destroy, ocdf_old

;Delete the temporary file
File_Delete, fsm_file

;-------------------------------------------
; Finished! ////////////////////////////////
;-------------------------------------------

;Return the file to standard output
MrPrintF, 'StdOut', file_new

Exit, STATUS=status
;END