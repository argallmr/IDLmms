; docformat = 'rst'
;
; NAME:
;       mms_dissect_filename
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
;       The purpose of this program is to dissect the filename of a data file from the
;       MMS mission.
;
; :Categories:
;       MMS
;
; :Params:
;       FILENAME:       in, required, type=string
;                       The filename of the MMS file to be dissected.
;
; :Keywords:
;       OPTDESC:        out, optional, type=string
;                       Optional data product descriptor.
;       INSTR:          out, optional, type=string
;                       Instrument ID. Possible values are::
;                           'hpca'              'edi'
;                           'aspoc'             'adp'
;                           'epd'               'sdp'
;                           'epd-eis'           'adp-sdp'
;                           'epd-feeps'         'afg'
;                           'fpi'               'dfg'
;                           'des'               'afg-dfg'
;                           'dis'               'scm'
;                           'des-dis'           'fields'
;       LEVEL:          out, optional, type=string
;                       Level of data production. Possible values are::
;                           'l1a'
;                           'l1b'
;                           'l2'
;                           'ql'
;                           'l2pre'
;                           'l2plus'
;       MODE:           out, optional, type=string
;                       Data capture mode. Possible values are::
;                           'fast'
;                           'slow'
;                           'brst'
;                           'srvy'
;       SC:             out, optional, type=string
;                       The spacecraft ID. Possible values are::
;                           'mms1'
;                           'mms2'
;                           'mms3'
;                           'mms4'
;       TSTART:         out, optional, type=string
;                       Start time of the data interval formatted as yyyymmddhhmmss,
;                           with irrelevant, least significant, fields dropped
;                           when files start on regular hourly or minute boundaries.
;       VERSION:        out, optional, type=string
;                       Version of the data file, formatted as 'vX.Y.Z'
;                           X - Interface number.  Increments in this number represent a
;                                   significant change to the processing software and/or to the contents of the 
;                                   file. These changes will likely break existing code that expects a specific 
;                                   file format (e.g. file reading software).  Additionally, increments in this 
;                                   number may require code changes to analysis software that expects the 
;                                   data to have been created using specific processing algorithms. The user 
;                                   should consult the appropriate meta-data for or changelogs.
;                           Y - Quality number. This number represents a change in the quality of
;                                   the data in the file, such as change in calibration or increase in fidelity. 
;                                   Changes should not impact software, but may require consideration when 
;                                   processing data.
;                           Z - Bug fix/revision number. This number changes to indicate minor
;                                   changes to the contents of the file due to reprocessing of missing data.  
;                                   Any dependent data products should generally be reprocessed if this value 
;                                   changes.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2013
;
; :History::
;   Modification History::
;       2015/02/06  -   Written by Matthew Argall
;       2015/05/04  -   Renamed keywords DESCRIPTOR to OPTDESC, SPACECRAFT to SC,
;                           START_TIME to TSTART, and INSTRUMENT to INSTR to be
;                           consistent with other programs. - MRA
;       2015/05/04  -   Added the DIRECTORY keyword. Accept full file paths. - MRA
;       2015/06/15  -   Version numbers can be greater than 9. - MRA
;       2015/10/21  -   Burst mode dates not longer get caught by OPTDESC. - MRA
;-
pro mms_dissect_filename, filename, $
DIRECTORY=directory, $
INSTR=instr, $
LEVEL=level, $
MODE=mode, $
OPTDESC=optdesc, $
SC=sc, $
TSTART=tstart, $
VERSION=version
	compile_opt strictarr
	on_error, 2

	;Check that a filename was provided.
	if n_elements(filename) eq 0 then message, 'A file name must be given.'
	
	;Remove the directory, if it is present
	directory = file_dirname(filename)
	fname     = file_basename(filename)
	
;-----------------------------------------------------
;DISSECT FILENAMES \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	str = stregex(fname, '(mms[1-4])_'       + $                  ;Spacecraft ID
	                     '([a-z-]+)_'        + $                  ;Instrument ID
	                     '([a-z0-9]+)_'      + $                  ;Mode
	                     '([a-z0-4]+)_'      + $                  ;Data Level
	                     '(([a-zA-Z0-9-]*)_)?' + $                ;Optional Descriptor
	                     '([0-9]{4}[0-9]{2}[0-9]{2}[0-9]*)_' + $  ;Start Time
	                     'v([0-9]+\.[0-9]+\.[0-9]+)\.cdf', $      ;Version
	                     /EXTRACT, /SUBEXP)

	;Find non-matches
	iFail = where(str[0,*] eq '', nFail, COMPLEMENT=iPass, NCOMPLEMENT=nPass)
	if nFail gt 0 then begin
		message, 'Cannot dissect filenames: ', /INFORMATIONAL
		print, '    "' + transpose(filename[iFail]) + '"'
	endif
	if nPass eq 0 then return

	;Extract the subexpressions
	sc      = reform(str[1,iPass])
	instr   = reform(str[2,iPass])
	mode    = reform(str[3,iPass])
	level   = reform(str[4,iPass])
	optdesc = reform(str[6,iPass])
	tstart  = reform(str[7,iPass])
	version = reform(str[8,iPass])
	
	;Return scalars?
	if nPass eq 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
		tstart  = tstart[0]
		version = version[0]
	endif
end

;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;pick a few filenames
filenames = ['mms3_dfg_hr_l2p_duration-1h1m_20010704_v1.0.0.cdf', $
             'mms3_dfg_hr_l2p_duration-1h1m_20010704_v1.0.0.cdf', $
             'mms3_edi_fast_l1a_beam_20010704_v0.2.0.cdf', $
             'mms3_edi_fast_l1a_beam_20010704_v0.2.0.cdf']

;print, the filenames
print, format='(%"The filenames used are: %s")', filenames[0]
print, format='(%"                        %s")', filenames[1]
print, format='(%"                        %s")', filenames[2]
print, format='(%"                        %s")', filenames[3]

;Dissect all of the names
mms_dissect_filename, filenames, $
                      OPTDESC = descriptor, $
                      INSTR   = instrument, $
                      LEVEL   = level, $
                      MODE    = mode, $
                      TSTART  = start_time, $
                      SC      = spacecraft, $
                      VERSION = version
                    
;breakdown the filenames and display the results.
for i = 0, n_elements(filenames) - 1 do begin
    print, '----------------------------------------------------------------------'
    print, format='(%"Results for: %s")', filenames[i]
    print, FORMAT='(%"  Spacecraft:    %s")', spacecraft[i]
    print, FORMAT='(%"  Instrument:    %s")', instrument[i]
    print, FORMAT='(%"  Mode:          %s")', mode[i]
    print, FORMAT='(%"  Level:         %s")', level[i]
    print, FORMAT='(%"  Descriptor:    %s")', descriptor[i]
    print, FORMAT='(%"  Start Time:    %s")', start_time[i]
    print, FORMAT='(%"  Version:       %s")', version[i]
    print, ''
endfor

end