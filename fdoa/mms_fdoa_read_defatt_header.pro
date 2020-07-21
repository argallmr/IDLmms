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
;                           ATT_ERR     - Error in attitude information.
;                           NHEADER     - Number of header lines in the file.
;                           INERTIA_CAL - Moment of inertia calibration file.
;                           START_TIME  - Start time of the data interval in the file.
;                           STOP_TIME   - Stop time of the data interval in the file.
;                           RATE_ERR    - Error in the spin rate information.
;                           ZMPA        - MPA z-Axis viewed from BCS. 
;
;-
function mms_fdoa_read_defatt_header, file
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(lun) gt 0 then free_lun, lun
		MrPrintF, 'LogErr'
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
	while data_start eq 0B do begin
		;Read a line of data
		readf, lun, line
		
		;
		; Check for flags of interest
		;
	
		;Inertia Calibration File
		if stregex(line, 'INERTIACAL', /BOOLEAN) then begin
			parts      = strsplit(line, ' ', /EXTRACT, COUNT=nParts)
			inertiacal = parts[nParts-1]
		
		;MPA axis
		endif else if stregex(line, 'Major principal axis', /BOOLEAN) then begin
			parts = strsplit(line, ' ', /EXTRACT, COUNT=nParts)
			zMPA  = float(parts[nParts-3:nParts-1])
		
		;Errors are on the next line?
		endif else if stregex(line, 'Estimated EKF.*errors', /BOOLEAN) then begin
			errors_next = 1B
		
		;Errors
		endif else if errors_next then begin
			parts       = strsplit(line, ' ', /EXTRACT, COUNT=nParts)
			att_err     = float(parts[1:3])
			rate_err    = float(parts[nParts-4:nParts-2])
			errors_next = 0B
			
		;START_TIME
		endif else if stregex(line, 'START_TIME', /BOOLEAN) then begin
			parts      = strsplit(line, ' ', /EXTRACT, COUNT=nParts)
			start_time = mms_fdoa_epoch2tt2000(parts[nParts-1])
			
		;START_TIME
		endif else if stregex(line, 'STOP_TIME', /BOOLEAN) then begin
			parts     = strsplit(line, ' ', /EXTRACT, COUNT=nParts)
			stop_time = mms_fdoa_epoch2tt2000(parts[nParts-1])
		
		;DATA_START
		endif else if stregex(line, 'DATA_START', /BOOLEAN) then begin
			data_start  = 1B
			nHeader    += 1
		endif
		
		;Advance to the next line
		nHeader += 1
	endwhile
	
	;Close the file and free the LUN
	free_lun, lun
	
	;Create the header structure
	header = { att_err:      att_err, $
	           nHeader:      nHeader, $
	           inertia_cal:  inertiacal, $
	           start_time:   start_time, $
	           stop_time:    stop_time, $
	           rate_err:     rate_err, $
	           zMPA:         zMPA $
	         }
	
	return, header
end


