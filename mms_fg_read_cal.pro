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
function mms_fg_read_cal, filenames, tstart, tend
	compile_opt idl2
	on_error, 2

	;Dissect the file name
	[sc, instr, mode, level] = mms_dissect_filename(filename[0])

	;Construct the param name for the cal variables
	if strcmp(mode, 'lorangecal')
		name = ['lo_' level]
	else
		name = ['hi_' level]
	end
	
	;Construct the variable names
	epoch_name  = mms_construct_varname(sc, instr, name, 'Epoch')
	gain_name   = mms_construct_varname(sc, instr, name, 'G')
	dTheta_name = mms_construct_varname(sc, instr, name, 'dTheta')
	dPhi_name   = mms_construct_varname(sc, instr, name, 'dPhi')
	u3_name     = mms_construct_varname(sc, instr, name, 'U3')
	offset_name = mms_construct_varname(sc, instr, name, 'O')
	mpa_name    = mms_construct_varname(sc, instr, name, 'MPA')
	
	;Read data calibration data
	;  - Read all of it because there may not be calibration
	;    parameters within the time range yet.
	;  - Also, dTheta and dPhi do not have DEPEND_0 variables.
	;    However, their values appear to change. I have only
	;    pre-flight calibration files so cannot say.
	gain_data   = MrCDF_nRead(filename, gain_name, DEPEND_0=t_cal)
	dTheta_data = MrCDF_nRead(filename, dTheta_name)
	dPhi_data   = MrCDF_nRead(filename, dPhi_name)
	u3_data     = MrCDF_nRead(filename, u3_name)
	offset_data = MrCDF_nRead(filename, offset_name)
	mpa_data    = MrCDF_nRead(filename, mpa_name)
	
	;Find the closest calibration time and pick those calibration params.
	;  - Transpose to return row vectors instead of column vectors.
	cal_params = { epoch:  t_cal, $
	               gain:   gain_data, $
	               dtheta: dtheta_data, $
	               dphi:   dphi_data, $
	               u3:     u3_data, $
	               offset: offset_data, $
	               mpa:    mpa_data }
	               
	return, cal_params
end
