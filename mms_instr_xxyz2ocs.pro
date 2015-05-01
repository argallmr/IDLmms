; docformat = 'rst'
;
; NAME:
;       MMS_Instr_xXYZ2OCS
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
;   Return the transformation matrix that rotates an instrument's native coordinate system
;   (XYZ) into the Observatory Coordinate System (OCS).
;
;   NOTE:
;       - Physical offsets in z are not considered, therefore, OCS is the same as BCS
;       - Instrument coordinate systems do not necessarily have an axis that lies along
;             the line between the s/c center and the instrument's origin. 
;
; :Categories:
;       MMS
;
; :Params:
;       INSTRUMENT:         in, required, type=string
;                           Name of the instrument.
;
; :Keywords:
;       NAMES:              out, optional, type=strarr
;                           If set, the names of each coordinate system are returned.
;       SEQUENCE:           out, optional, type=intarr(3)
;                           The order in which `ANGLES`, are to be performed.
;                               ['Z','Y','X'] implies Z ## Y ## X.
;       ANGLES:             out, optional, type=double
;                           Euler angles for rotations about the axes specified in `SEQUENCE`.
;
; :Returns:
;       XYZ2OCS:            The rotation matrix that rotates EDI mechanical coordinate
;                               system into OCS. Note that::
;
;                                   v_xyz     = [[vx], [vy], [vz]]
;                                   A_xyz2ocx = [[ Axx,  Axy,  Axz], $
;                                                [ Ayx,  Ayy,  Ayz], $
;                                                [ Azx,  Azy,  Azz]]
;
;                                   | vx' |   | Axx Axy Axz | | vx |
;                                   | vy' | = | Ayx Ayy Ayz | | vy |
;                                   | vz' |   | Azx Azy Azz | | vz |
;
;                                   v_ocs = A_xyz2ocs ## v_xyz
;                                   [1,3] =   [3,3]   ## [1,3]
;                                              ^            ^
;
; :References:
;   Magnetospheric Multiscale (MMS) Project Alignment and Coordinate System Document,
;       461-SYS-SPEC-0115, Revision C, Effective Date: July 22, 2014
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/02/18  -   Written by Matthew Argall
;       2015-05-01      Magnetometer 123 and XYZ systems are now directly
;                         related to BCS. Removed OMB, as it is with respect
;                         to SMPA. Remove NO_MATRIX keyword and use updated
;                         MrEulerMatrix. - MRA
;-
function mms_instr_xXYZ2OCS, instrument, $
NAMES=names, $
ANGLES=angles, $
SEQUENCE=sequence
	compile_opt idl2
	on_error, 2

	;Calculate the rotation matrix?
	tf_matrix = ~keyword_set(no_matrix)

;-------------------------------------------------------
; Define Rotations to OCS //////////////////////////////
;-------------------------------------------------------
	;
	; NOTE!!
	;   - It is important to other programs that the rotation about z take place last --
	;       after the instrument z-axis has been aligned with the OCS z-axis. This allows
	;       the ROTZ keyword provide information about relative angular offsets between
	;       instrument coordinate systems.
	;
	;            INSTRUMENT                ALPHA    BETA    GAMMA   ORDER ([3,2,1] => Gamma ## Beta ## Alpha)
	euler_angles                  = hash()
	euler_angles['ADP1']          = list( [    0.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['ADP2']          = list( [    0.0D,  180.0D,  0.0D], ['Z','Y','X'])
	euler_angles['AFG_XYZ']       = list( [   45.0D,  -90.0D,  0.0D], ['Z','Y','X'])
	euler_angles['AFG_123']       = list( [  135.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['AFG_BOOM']      = list( [  135.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['BCS']           = list( [    0.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['DFG_XYZ']       = list( [ -135.0D,  -90.0D,  0.0D], ['Z','Y','X'])
	euler_angles['DFG_123']       = list( [  135.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['DFG_BOOM']      = list( [  -45.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['DSS']           = list( [  -76.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['EDI1']          = list( [  221.0D,  -90.0D,  0.0D], ['Z','Y','X'])
	euler_angles['EDI1_GUN']      = list( [  221.0D,  -90.0D,  0.0D], ['Z','Y','X'])
	euler_angles['EDI1_DETECTOR'] = list( [  221.0D,  -90.0D,  0.0D], ['Z','Y','X'])
	euler_angles['EDI2']          = list( [   41.0D,  -90.0D,  0.0D], ['Z','Y','X'])
	euler_angles['EDI2_GUN']      = list( [   41.0D,  -90.0D,  0.0D], ['Z','Y','X'])
	euler_angles['EDI2_DETECTOR'] = list( [   41.0D,  -90.0D,  0.0D], ['Z','Y','X'])
	euler_angles['OCS']           = list( [    0.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['SC']            = list( [    0.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['SCM_XYZ']       = list( [  -45.0D,    0.0D, 90.0D], ['Z','Y','X'])
	euler_angles['SCM_123']       = list( [  135.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['SCM_BOOM']      = list( [  135.0D,    0.0D,  0.0D], ['Z','Y','X'])
	euler_angles['SDP1']          = list( [  -60.0D,  180.0D,  0.0D], ['Z','Y','X'])
	euler_angles['SDP2']          = list( [  120.0D,  180.0D,  0.0D], ['Z','Y','X'])
	euler_angles['SDP3']          = list( [   30.0D,  180.0D,  0.0D], ['Z','Y','X'])
	euler_angles['SDP4']          = list( [  210.0D,  180.0D,  0.0D], ['Z','Y','X'])

;-------------------------------------------------------
; Names ////////////////////////////////////////////////
;-------------------------------------------------------
	;Return only the instrument names?
	if keyword_set(names) then begin
		instr_names = instr_hash  -> Keys()
		instr_names = instr_names -> ToArray()
		return, instr_names
	endif

	;Check that we have the instrument
	instr = strupcase(instrument)
	if euler_angles -> HasKey(instr) eq 0 $
		then message, 'INSTRUMENT "' + instrument + '" is not a valid option.'

;-------------------------------------------------------
; Create Rotation Matrix ///////////////////////////////
;-------------------------------------------------------
	;Collect the data
	angles   = euler_angles[instr, 0]
	sequence = euler_angles[instr, 1]
	
	;Get the rotation matrix that rotates XYZ into OCS.
	xyz2ocs = MrEulerMatrix(angles, sequence, /DEGREES, /MATH)

	;Return
	return, xyz2ocs
end