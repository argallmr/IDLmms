; docformat = 'rst'
;
; NAME:
;       MMS_Instr_Origins_OCS
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
;   Return the position of the origin of an instrument's coordinate system with respect
;   to the origin of OCS.
;
; :Categories:
;       MMS
;
; :Examples:
;   Example using a single instrument name::
;       IDL> print, mms_instr_origins_ocs('EDI1')
;             -1332.7480       889.06897       1051.0000
;
;   Example using multiple instrument names with values returned in spherical coordinates::
;       IDL> print, mms_instr_origins_ocs(['EDI1', 'EDI2'], /SPHERICAL)
;       EDI2:      -0.58829881      0.58059704       1916.0537
;       EDI1:        2.5532939      0.58059704       1916.0537
;
;   Return the names of each available instrument::
;       names = mms_instr_origins_ocs(/NAMES)
;
; :Params:
;       INSTRUMENT:         in, required, type=string/strarr
;                           Name of the instrument for which the location is returned.
;                               See the `NAMES` keyword.
;
; :Keywords:
;       NAMES:              in, optional, type=boolean, default=0
;                           If set, then the names of the available instruments are returned.
;       SPHERICAL:          in, optional, type=boolean, default=0
;                           If set, `ORIGIN` will return spherical coordinates as
;                               (theta, phi, r), where r is the radius, theta is the
;                               polar anglie and phi is the azimuthal angle.
;
; :Returns:
;       ORIGIN:             3-element vector of the (x,y,z) coordinates of the requested
;                               `INSTRUMENT` in OCS coordinates. Units: meters. If
;                               more than one instrument name is given, origins are
;                               returned in a hash with the instrument names as keys.
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
function mms_instr_origins_ocs, instrument, $
NAMES=names, $
SPHERICAL=spherical
	compile_opt idl2
	on_error, 2

	;Instrument list
	instr_hash                  = hash()
	;                                    X           Y            Z
	instr_hash['ADP1']          = [  0.0D,       -0.161925D,    15.745D      ]
	instr_hash['ADP2']          = [  0.0D,       -0.161925D,   -15.185D      ]
	instr_hash['AFG_BOOM']      = [ -0.99147D,   -0.99147D,     -0.0771D     ]
	instr_hash['AFG_MECH']      = [  5.18785D,   -0.0080D,       0.0021262D  ]    ;From AFG_BOOM origin
	instr_hash['DFG_BOOM']      = [ -0.99147D,    0.99147D,     -0.0771D     ]
	instr_hash['DFG_MECH']      = [  5.18785D,   -0.0080D,       0.0021262D  ]    ;From DFG_BOOM origin
	instr_hash['EDI1']          = [ -1.332748D,   0.889069D,     1.051D      ]
	instr_hash['EDI1_GUN']      = [ -1.45598D,    1.11837D,      0.0D        ]
	instr_hash['EDI1_DETECTOR'] = [ -1.35885D,    1.03395D,      0.0D        ]
	instr_hash['EDI2']          = [  1.332748D,  -0.889069D,     1.051D      ]
	instr_hash['EDI2_GUN']      = [  1.45598D,   -1.11837D,      0.0D        ]
	instr_hash['EDI2_DETECTOR'] = [  1.35885D,   -1.03395D,      0.0D        ]
	instr_hash['IDCS']          = [  0.0D,        0.0D,          1.0510D     ]
	instr_hash['DSS']           = [  1.01764D,    1.25506D,      0.127220D   ]
	instr_hash['OCS']           = [  0.0D,        0.0D,          0.0D        ]
	instr_hash['SC']            = [  0.0D,        0.0D,          0.1670D     ]
	instr_hash['SCM_BOOM']      = [ -0.99147D,   -0.99147D,     -0.0771D     ]
	instr_hash['SCM_MECH']      = [  4.14785D,   -0.00479899D,  -0.0332010D  ]    ;From SCM_BOOM origin
	instr_hash['SDP1']          = [  1.342598D,   0.865542D,     1.050D      ]
	instr_hash['SDP2']          = [ -1.342598D,  -0.865542D,     1.050D      ]
	instr_hash['SDP3']          = [ -0.865542D,   1.342598D,     1.050D      ]
	instr_hash['SDP4']          = [  0.865542D,  -1.342598D,     1.050D      ]
	
	;Convert to OCS origin
	instr_hash['AFG_123'] = instr_hash['AFG_MECH'] + instr_hash['AFG_BOOM']
	instr_hash['AFG_XYZ'] = instr_hash['AFG_MECH'] + instr_hash['AFG_BOOM']
	instr_hash['DFG_123'] = instr_hash['DFG_MECH'] + instr_hash['DFG_BOOM']
	instr_hash['DFG_XYZ'] = instr_hash['DFG_MECH'] + instr_hash['DFG_BOOM']
	instr_hash['SCM_XYZ'] = instr_hash['SCM_MECH'] + instr_hash['SCM_BOOM']
	instr_hash['SCM_123'] = instr_hash['SCM_MECH'] + instr_hash['SCM_BOOM']
	
	;Remove non-OCS origins
	instr_hash -> Remove, 'AFG_MECH'
	instr_hash -> Remove, 'DFG_MECH'
	instr_hash -> Remove, 'SCM_MECH'

	;Return only the instrument names?
	if keyword_set(names) then begin
		instr_names = instr_hash -> Keys()
		instr_names = instr_names -> ToArray()
		return, instr_names
	endif

	;Default to cartesian coordinates
	spherical = keyword_set(spherical)
	instr     = strupcase(instrument)
	nInstr    = n_elements(instrument)

	;Case insensitive version
	tf_has = instr_hash -> HasKey(instr)
	if min(tf_has) eq 0 then begin
		ibad = where(tf_has eq 0)
		message, 'Invalid instruments given: "' + strjoin(instrument[ibad], '”, “') + '".'
	endif
	
	;Retrieve the desired origin(s).
	origin = instr_hash[instr]

	;Return sperical coordinates?
	if spherical then begin
		if nInstr eq 1 $
			then origin = cv_coord(FROM_RECT=origin, /TO_SPHERE) $
			else foreach value, origin, key do origin[key] = cv_coord(FROM_RECT=value, /TO_SPHERE, /DOUBLE)
	endif
	
	return, origin
end
