; docformat = 'rst'
;
; NAME:
;       mms_dss_read_sunpulse
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;         contributors may  be used to endorse or promote products derived from this     ;
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
;   Read fluxgate calibration files.
;
; :Categories:
;   MMS, DFG, AFG
;
; :Params:
;       FILENAMES:          in, required, type=string
;                           Names of the calibration files to read.
;       TSTART:             in, required, type=string
;                           Start time of the data interval to read, as an ISO-8601 string.
;       TEND:               in, required, type=string
;                           End time of the data interval to read, as an ISO-8601 string.
;
; :Returns:
;       CAL_PARAMS:         Structure of calibration parameters. Fields are::
;                               epoch  - TT2000 epoch times
;                               gain   - Instrument gain
;                               dTheta - Othogonalization angle
;                               dPhi   - Othogonalization angle
;                               U3     - Tilt of 123 3-axis
;                               offset - DC offset
;                               mpa    - Major principle axis, as viewed from BCS
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
;       2015/05/03  -   Written by Matthew Argall
;-
function mms_fg_bcs, sc, instr, mode, level, tstart, tend, $
DATA_DIR=data_dir, $
CAL_DIR=cal_dir
	compile_opt idl2
	on_error, 2

	;Default directories
	if n_elements(data_dir) eq 0 then cd, CURRENT=data_dir
	if n_elements(cal_dir)  eq 0 then cal_dir = data_dir

;-------------------------------------------------------
; Read Calibration Data ////////////////////////////////
;-------------------------------------------------------
	;File name patterns
	hical_fname = mms_construct_filename(sc, instr, 'hirangecal', 'l2pre', /TOKENS, $
	                                     DIRECTORY = cal_dir)
	local_fname = mms_construct_filename(sc, instr, 'lorangecal', 'l2pre', /TOKENS, $
	                                     DIRECTORY = cal_dir)

	;Find the calibration file
	hiCal_file = MrFile_Search(hical_fname, COUNT=nHiCal)
	loCal_file = MrFile_Search(local_fname, COUNT=nLoCal)

	;Make sure the files exist
	if nHiCal eq 0 then message, 'HiCal file not found.'
	if nLoCal eq 0 then message, 'LoCal file not found.'

	;Calibrate hi-range
	hiCal = mms_fg_read_cal(hiCal_file, tstart, tend)
	loCal = mms_fg_read_cal(loCal_file, tstart, tend)

;-------------------------------------------------------
; Read Mag Data ////////////////////////////////////////
;-------------------------------------------------------

	;Create the file names
	fpattern = mms_construct_filename(sc, instr, mode, 'l1a', /TOKENS, $
	                                  DIRECTORY = data_dir)
	
	;Find the files
	[files, nFiles] = MrFile_Search(fpattern, /CLOSEST, $
	                                TSTART    = tstart, $
	                                TEND      = tend, $
	                                TIMEORDER = '%Y%M%d')
	if nFiles eq 0 $
		then message, 'No files found matching "' + fpattern + '".'

	;Create variable names
	b_name = mms_construct_varname(sc, instr, '123')
	if strcmp(instr, 'afg') $
		then range_name = mms_construct_varname(sc, instr, 'hirange') $
		else range_name = mms_construct_varname(sc, instr, 'range')

	;Read the magnetometer data
	b_123 = MrCDF_nRead(files, b_name,     TSTART=tstart, TEND=tend, DEPEND_0=t)
	range = MrCDF_nRead(files, range_name, TSTART=tstart, TEND=tend, DEPEND_0=t_range)

;-------------------------------------------------------
; Calibrate Mag Data ///////////////////////////////////
;-------------------------------------------------------
	;Calibrate
	[b_omb, mpa] = mms_fg_calibrate(b_123, t, range, t_range, hiCal, loCal, MPA=mpa)

;-------------------------------------------------------
; OMB -> SMPA //////////////////////////////////////////
;-------------------------------------------------------

	;OMB -> SMPA
	omb2smpa = mms_fg_xomb2smpa()
	b_smpa   = MrVector_Rotate(omb2smpa, b_omb)

;-------------------------------------------------------
; SMPA -> BCS //////////////////////////////////////////
;-------------------------------------------------------

	;Build transformation from SMPA to BCS
	bcs2smpa = mms_fg_xbcs2smpa(mpa)
	smpa2bcs = transpose(bcs2smpa, [1, 0, 2])

	;Rotate to SMPA
	b_bcs = MrVector_Rotate(smpa2bcs, b_smpa)

	return, b_bcs
end
