; docformat = 'rst'
;
; NAME:
;       MMS_Construct_Varname
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
;       The purpose of this program is to create a Cluster CDF file name using the user
;       input information::
;
;       scId_instrumentId_paramName_optionalDescriptor
;
; :Categories:
;   MMS
;
; :Examples:
;   See the example program at the end of this file::
;       IDL> .r mms_construct_varname
;
; :Params:
;       SPACECRAFT:         out, optional, type=string
;                           The spacecraft ID. Possible values are::
;                               'mms1'
;                               'mms2'
;                               'mms3'
;                               'mms4'
;       INSTRUMENT:         out, optional, type=string
;                           Instrument ID. Possible values are::
;                               'hpca'              'edi'
;                               'aspoc'             'adp'
;                               'epd'               'sdp'
;                               'epd-eis'           'adp-sdp'
;                               'epd-feeps'         'afg'
;                               'fpi'               'dfg'
;                               'des'               'afg-dfg'
;                               'dis'               'scm'
;                               'des-dis'           'fields'
;       MODE:               out, optional, type=string
;                           Data capture mode. Possible values are::
;                               'fast'
;                               'slow'
;                               'brst'
;                               'srvy'
;       LEVEL:              out, optional, type=string
;                           Level of data production. Possible values are::
;                               'l1a'
;                               'l1b'
;                               'l2'
;                               'ql'
;                               'l2pre'
;                               'l2plus'
;
; :Keywords:
;       OPTDESC:            in, optional, type=string, default=''
;                           Optional field that may not be needed for all
;                               products (e.g. Quicklook and SITL).  Where it is used, 
;                               identifiers should be short (e.g. 3-8 character)
;                               descriptors that are helpful to end-users.  If a descriptor
;                               contains multiple components, hyphens are used to separate
;                               those components.
;       PATTERN:            in, optional, type=boolean, default=0
;                           If set, then the default value of `START_TIME` is '%Y%M%d'.
;                               This pattern is recognized by MrTokens and can be used
;                               to find files with year, month, and day in the file name.
;       START_TIME:         in, optional, type=string, default='*'
;                           Start time of the data interval formatted as yyyymmddhhmmss,
;                               with irrelevant, least significant, fields dropped
;                               when files start on regular hourly or minute boundaries.
;       VERSION:            in, optional, type=string, default='*'
;                           Version of the data file, formatted as 'vX.Y.Z'
;                               X - Interface number.  Increments in this number represent a
;                                       significant change to the processing software and/or to the contents of the 
;                                       file. These changes will likely break existing code that expects a specific 
;                                       file format (e.g. file reading software).  Additionally, increments in this 
;                                       number may require code changes to analysis software that expects the 
;                                       data to have been created using specific processing algorithms. The user 
;                                       should consult the appropriate meta-data for or changelogs.
;                               Y - Quality number. This number represents a change in the quality of
;                                       the data in the file, such as change in calibration or increase in fidelity. 
;                                       Changes should not impact software, but may require consideration when 
;                                       processing data.
;                               Z - Bug fix/revision number. This number changes to indicate minor
;                                       changes to the contents of the file due to reprocessing of missing data.  
;                                       Any dependent data products should generally be reprocessed if this value 
;                                       changes.
;
; :Returns:
;       VARNAME:             The MMS variable name.
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
;       2015/02/06  -   Written by Matthew Argall
;       2015/05/01  -   Removed instrument validation. - MRA
;
function mms_construct_varname, sc, instrument, param_name, optdesc
	compile_opt strictarr
	on_error, 2
	
	desc = n_elements(optdesc) eq 0 ? '' : '_' + optdesc
	
	;Spacecraft ID
	if size(sc, /TNAME) eq 'STRING' then begin
		_sc = strlowcase(sc)
		case _sc of
			'mms1': ;Ok
			'mms2': ;Ok
			'mms3': ;Ok
			'mms4': ;Ok
			'1':    _sc = 'mms1'
			'2':    _sc = 'mms2'
			'3':    _sc = 'mms3'
			'4':    _sc = 'mms4'
			else: message, 'SC ("' + sc + '") must be {mms1 | mms2 | mms3 | mms4}.'
		endcase
	endif else begin
		case sc of
			1: _sc = 'mms1'
			2: _sc = 'mms2'
			3: _sc = 'mms3'
			4: _sc = 'mms4'
			else: message, 'SC ("' + strtrim(sc, 2) + '") must be {1 | 2 | 3 | 4}.'
		endcase
	endelse
	
	;Create the MMS variable name
	varname = _sc + '_' + $
	          instrument  + '_' + $
	          param_name  +  $
	          desc
	
	return, varname
end

;MAIN level program to see how mms_construct_file.pro works.
;To test,
;      IDL> .run mms_construct_file
sc         = 1
instrument = 'afg'
param_name = 'bmrg'
descriptor = 'merged'

filename = mms_construct_varname(sc, instrument, param_name, DESCRIPTOR=descriptor)
print, filename
end