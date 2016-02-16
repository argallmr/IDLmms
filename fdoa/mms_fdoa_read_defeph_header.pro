; docformat = 'rst'
;
; NAME:
;    mms_fdoa_read_defatt_header
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
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
;   Read the MMS definitive attitude files.
;
; :Categories:
;    MMS, FDOA, ATTITUDE
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 348
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/06/29  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Read the header of MMS definitive attitude files.
;
; :Params:
;       FILE:           in, required, type=string
;                       Name of the file of which the header will be read.
;
; :Returns:
;       HEADER:         Structure containing header information. Fields are:
;                           NHEADER     - Number of header lines in the file.
;                           SPACECRAFT  - Spacecraft identifier.
;                           START_TIME  - Start time of the data interval in the file.
;                           STOP_TIME   - Stop time of the data interval in the file.
;-
function mms_fdoa_read_defeph_header, file
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(lun) gt 0 then free_lun, lun
		void = cgErrorMSG(/QUIET)
		return, !Null
	endif
	
	data_start  = 0
	nHeader     = 0
	errors_next = 0B
	line        = ''
	
	;Open the file
	if ~file_test(file) then message, 'File does not exist: "' + file + '".'
	openr, lun, file, /GET_LUN
	
	;Start reading the file
	while data_start eq 0 do begin
		;Read a line of data
		readf, lun, line
		
		;
		; Check for flags of interest
		;
	
		;Start Time
		if stregex(line, 'StartTime', /BOOLEAN) then begin
			parts      = strsplit(line, '[ =()]+', /REGEX, /EXTRACT, COUNT=nParts)
			start_time = mms_fdoa_epoch2tt2000( parts[1], /EPHEMERIS )
		
		;Stop Time
		endif else if stregex(line, 'StopTime', /BOOLEAN) then begin
			parts     = strsplit(line, '[ =()]+', /REGEX, /EXTRACT, COUNT=nParts)
			stop_time = mms_fdoa_epoch2tt2000( parts[1], /EPHEMERIS )
		
		;Spacecraft
		endif else if stregex(line, 'Spacecraft', /BOOLEAN) then begin
			parts = strsplit(line, '[ =()]+', /REGEX, /EXTRACT, COUNT=nParts)
			sc    = parts[1]
		
		;Data start
		;   - Check for date 2015-077/01:00:25.000
		endif else if stregex(line, '^[ \t]*[0-9]{4}-[0-9]{3}', /BOOLEAN) then begin
			data_start  = nHeader + 1
			nHeader    -= 1
		endif
		
		;Advance to the next line
		nHeader += 1
	endwhile
	
	;Close the file and free the LUN
	free_lun, lun
	
	;Create the header structure
	header = { nHeader:      nHeader, $
	           spacecraft:   sc, $
	           start_time:   start_time, $
	           stop_time:    stop_time $
	         }
	
	return, header
end


; Definitive Orbit Ephemeris
; Spacecraft = MMS1
; StartTime = 2015-077/01:00:25.000 UTC  (MMS TAI: 20895.042361111)
; StopTime = 2015-078/09:59:25.000 UTC  (MMS TAI: 20896.416666666)
; MMS TAI Reference Epoch = 1958-001/00:00:00 UTC
; CentralBody = Earth
; ReferenceFrame = Mean of J2000
; PrincipalPlane = Equatorial
; Project = PPFA_ProductGeneration_DefinitiveEphem.m
; Source = FDGSS OPS
; FileCreationDate = Apr 08 2015 20:58:45.771 UTC
;  
; Epoch (UTC)             Epoch MMS TAI         X                       Y                       Z                       VX                      VY                      VZ                      Mass             
;                                               Km                      Km                      Km                      Km/Sec                  Km/Sec                  Km/Sec                  Kg               
; 2015-077/01:00:25.000   20895.042361111       8165.741418464          -13267.112442189        -8571.019193681         1.4112746511000         5.3676428971000         2.1162750924000         1350.9730257811  
