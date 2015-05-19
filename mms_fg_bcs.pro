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
;       FILES:          in, required, type=string/strarr
;                       Name(s) of the AFG or DFG L1A file(s) to read.
;
; :Keywords:
;       BCS:            out, optional, type=boolean, default=1
;                       If set, data in BCS will be included in `FG_BCS`.
;       CAL_DIR:        in, optional, type=string, default=pwd
;                       Directory in which to find fluxgate calibration files.
;       OMB:            out, optional, type=boolean, default=0
;                       If set, data in OMB will be included in `FG_BCS`.
;       SENSOR:         out, optional, type=boolean, default=0
;                       If set, data in sensor 123 will be included in `FG_BCS`.
;       SMPA:           out, optional, type=boolean, default=0
;                       If set, data in SMPA will be included in `FG_BCS`.
;       TSTART:         in, optional, type=string
;                       Start time of the data interval to read, as an ISO-8601 string.
;       TEND:           in, optional, type=string
;                       End time of the data interval to read, as an ISO-8601 string.
;
; :Returns:
;       FG_BCS:         Fluxgate magnetic field data structure. Fields include::
;                           'epoch'        - TT2000 epoch times for 'b_123'
;                           'epoch_stat'   - TT2000 epoch times for 'range' and 'sample_rate'
;                           'b_123'        - 4xN (Bx, By, Bz, |B|) in 123 coordinates
;                           'b_omb'        - 4xN (Bx, By, Bz, |B|) in OMB coordinates
;                           'b_smpa'       - 4xN (Bx, By, Bz, |B|) in SMPA coordinates
;                           'b_bcs'        - 4xN (Bx, By, Bz, |B|) in BCS coordinates
;                           'range'        - Instrument range flag (1=hi, 0=lo)
;                           'sample_rate'  - sampling rate
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
;       2015/05/18  -   Require file names instead of search for files. TSTART and TEND
;                           are keywords, not parameters. - MRA
;-
function mms_fg_bcs, files, $
CAL_DIR=cal_dir, $
BCS=bcs, $
OMB=omb, $
SENSOR=sensor, $
SMPA=smpa, $
TSTART=tstart, $
TEND=tend
	compile_opt idl2
	on_error, 2

	;Default directories
	bcs    = n_elements(bcs) eq 0 ? 0 : keyword_set(bcs)
	sensor = keyword_set(sensor)
	smpa   = keyword_set(smpa)
	omb    = keyword_set(omb)
	if n_elements(data_dir) eq 0 then cd, CURRENT=data_dir
	if n_elements(cal_dir)  eq 0 then cal_dir = data_dir

	if bcs + smpa + omb eq 0 then bcs = 1

;-------------------------------------------------------
; Read Calibration Data ////////////////////////////////
;-------------------------------------------------------
	;Get the spacecraft and instrument names
	mms_dissect_filename, files[0], SC=sc, INSTR=instr

	;File name patterns
	hical_fname = mms_construct_filename(sc, instr, 'hirangecal', 'l2pre', /TOKENS, $
	                                     DIRECTORY = cal_dir)
	local_fname = mms_construct_filename(sc, instr, 'lorangecal', 'l2pre', /TOKENS, $
	                                     DIRECTORY = cal_dir)

	;Find the calibration file
	hiCal_file = MrFile_Search(hical_fname, COUNT=nHiCal)
	loCal_file = MrFile_Search(local_fname, COUNT=nLoCal)

	;Make sure the files exist
	if nHiCal eq 0 then message, 'HiCal file not found: "' + hical_fname + '".'
	if nLoCal eq 0 then message, 'LoCal file not found: "' + local_fname + '".'

	;Calibrate hi-range
	hiCal = mms_fg_read_cal(hiCal_file, tstart, tend)
	loCal = mms_fg_read_cal(loCal_file, tstart, tend)

;-------------------------------------------------------
; Read Mag Data ////////////////////////////////////////
;-------------------------------------------------------

	;Read the data
	fg_l1a = mms_fg_read_l1a(files, TSTART=tstart, TEND=tend)

;-------------------------------------------------------
; Calibrate Mag Data ///////////////////////////////////
;-------------------------------------------------------
	;Calibrate
	b_omb = mms_fg_calibrate(fg_l1a.b_123, fg_l1a.epoch, fg_l1a.range, fg_l1a.epoch_stat, hiCal, loCal, MPA=mpa)

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
	smpa2bcs = mms_fg_xbcs2smpa(mpa, /INVERSE)

	;Rotate to SMPA
	b_bcs = MrVector_Rotate(smpa2bcs, b_smpa)

;-------------------------------------------------------
; Output Structure /////////////////////////////////////
;-------------------------------------------------------
	;Copy the l1a structure
	fg_bcs = temporary(fg_l1a)
	
	;Add fields
	fg_bcs = create_struct(fg_bcs, 'zmpa', mpa)
	if ~sensor then fg_bcs = remove_tag(fg_bcs, 'b_123')
	if bcs     then fg_bcs = create_struct(fg_bcs, 'b_bcs',  b_bcs)
	if omb     then fg_bcs = create_struct(fg_bcs, 'b_omb',  b_omb)
	if smpa    then fg_bcs = create_struct(fg_bcs, 'b_smpa', b_smpa)

	return, fg_bcs
end
