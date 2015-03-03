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
;                           Name of the instrument. Options are::
;                               "EDI1"
;                               "EDI2"
;                               "DSS"
;
; :Keywords:
;       NO_MATRIX:          in, optional, type=boolean, default=0
;                           If set, do not calculate the rotation matrix. This is useful
;                               for determining offsets between different instrument
;                               coordinate systems via the ROT[XYZ]. Since `ROTZ` is
;                               always performed last, it indicates the angle required
;                               to rotate the instrument's x-axis into the OCS x-axis,
;                               with z along the satellite spin axis.
;       ORDER:              out, optional, type=intarr(3)
;                           The order in which `ROTX`, `ROTY`, and `ROTZ`, are to be
;                               performed. [3,2,1] implies ROTZ ## ROTY ## ROTX.
;       ROTX:               out, optional, type=double
;                           Euler angle for a rotation about the instrument's x-axis.
;       ROTY:               out, optional, type=double
;                           Euler angle for a rotation about the instrument's y-axis.
;       ROTZ:               out, optional, type=double
;                           Euler angle for a rotation about the instrument's z-axis.
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
;-
function mms_instr_xXYZ2OCS, instrument, $
NAMES=names, $
NO_MATRIX=no_matrix, $
ORDER=order, $
ROTX=rotx, $
ROTY=roty, $
ROTZ=rotz
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
	;             INSTRUMENT          ALPHA    BETA    GAMMA   ORDER ([3,2,1] => Gamma ## Beta ## Alpha)
	euler_angles                  = hash()
	euler_angles['ADP1']          = list(  0.0D,    0.0D,    0.0D, [3,2,1])
	euler_angles['ADP2']          = list(  0.0D,  180.0D,    0.0D, [3,2,1])
	euler_angles['AFG_XYZ']       = list(  0.0D,  -90.0D,  -90.0D, [3,2,1])     ;AFG_XYZ To AFG BOOM
	euler_angles['AFG_123']       = list(  0.0D,    0.0D,    0.0D, [3,2,1])     ;AFG_123 To AFG_BOOM
	euler_angles['AFG_BOOM']      = list(  0.0D,    0.0D,  135.0D, [3,2,1])
	euler_angles['BCS']           = list(  0.0D,    0.0D,    0.0D, [3,2,1])
	euler_angles['DFG_XYZ']       = list(  0.0D,  -90.0D,  -90.0D, [3,2,1])     ;DFG_XYZ To DFG_BOOM
	euler_angles['DFG_123']       = list(  0.0D,    0.0D,  180.0D, [3,2,1])     ;DFG_123 To DFG_BOOM
	euler_angles['DFG_BOOM']      = list(  0.0D,    0.0D,  -45.0D, [3,2,1])
	euler_angles['DSS']           = list(  0.0D,    0.0D,  -76.0D, [3,2,1])
	euler_angles['EDI1']          = list(  0.0D,  -90.0D,  221.0D, [3,2,1])
	euler_angles['EDI1_GUN']      = list(  0.0D,  -90.0D,  221.0D, [3,2,1])
	euler_angles['EDI1_DETECTOR'] = list(  0.0D,  -90.0D,  221.0D, [3,2,1])
	euler_angles['EDI2']          = list(  0.0D,  -90.0D,   41.0D, [3,2,1])
	euler_angles['EDI2_GUN']      = list(  0.0D,  -90.0D,   41.0D, [3,2,1])
	euler_angles['EDI2_DETECTOR'] = list(  0.0D,  -90.0D,   41.0D, [3,2,1])
	euler_angles['OCS']           = list(  0.0D,    0.0D,    0.0D, [3,2,1])
	euler_angles['OMB']           = list(  0.0D,    0.0D,  225.0D, [3,2,1])
	euler_angles['SC']            = list(  0.0D,    0.0D,    0.0D, [3,1,2])
	euler_angles['SCM_XYZ']       = list( 90.0D,    0.0D,  180.0D, [3,1,2])     ;SCM_XYZ To SCM_BOOM
	euler_angles['SCM_123']       = list( 90.0D,    0.0D,  180.0D, [3,1,2])     ;SCM_123 To SCM_BOOM
	euler_angles['SCM_BOOM']      = list(  0.0D,    0.0D,  135.0D, [3,2,1])
	euler_angles['SDP1']          = list(  0.0D,  180.0D,  -60.0D, [3,2,1])
	euler_angles['SDP2']          = list(  0.0D,  180.0D,  120.0D, [3,2,1])
	euler_angles['SDP3']          = list(  0.0D,  180.0D,   30.0D, [3,2,1])
	euler_angles['SDP4']          = list(  0.0D,  180.0D,  210.0D, [3,2,1])

	;This is how the rotations are given in the instrument manual.
;	euler_angles['AFG123']     = list(  0.0D,  -90.0D,  -90.0D, [3,2,1])     ;AFG-to-AFG123
;	euler_angles['DFG123']     = list(  0.0D,  -90.0D,   90.0D, [3,2,1])     ;DFG-to-DFG123
;	euler_angles['SCM123']     = list( 90.0D,    0.0D,  180.0D, [3,1,2])     ;SCM-to-SCM123

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
	rotx  = euler_angles[instr, 0]
	roty  = euler_angles[instr, 1]
	rotz  = euler_angles[instr, 2]
	order = euler_angles[instr, 3]
	
	;Get the rotation matrix that rotates XYZ into OCS.
	if tf_matrix $
		then xyz2ocs = MrEulerMatrix(rotx, roty, rotz, ORDER=order, /ANGLES, /MATH) $
		else xyz2ocs = identity(3)
	
;-------------------------------------------------------
; Finish Rotation to OCS ///////////////////////////////
;-------------------------------------------------------

	;Magnetometer mechanical and sensor frames must be rotated from MAG_BOOM to OCS.
	;   - e.g. 'AFG_123' and 'AFG_XYZ' must be rotated from 'AFG_BOOM' to 'OCS'
	;   - The process is the same for all three.
	if stregex(instr, '(AFG|DFG|SCM)_(XYZ|123)', /BOOLEAN) then begin
		;Extract the instrument
		mag_name = stregex(instr, '^(AFG|DFG|SCM)', /SUBEXP, /EXTRACT)
		mag_name = mag_name[1]
		
		;What we really have is the transformation to the MAG_BOOM system
		mag2mag_boom = xyz2ocs
		
		;Now rotate into OCS
		rotinfo = euler_angles[mag_name + '_BOOM']
		if tf_matrix then begin
			mag_boom2ocs = MrEulerMatrix(rotinfo[0], rotinfo[1], rotinfo[2], ORDER=rotinfo[3], /ANGLES, /MATH)
			xyz2ocs      = mag_boom2ocs ## mag2mag_boom
		endif
		
		;Since the rotation about Z is always last, the final rotation about z is
		;the sum of the two original rotations.
		rotz += rotinfo[2]
	endif

	;Return
	return, xyz2ocs
end