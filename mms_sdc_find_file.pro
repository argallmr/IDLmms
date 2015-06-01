;+
; docformat = 'rst'
;
; NAME:
;   mms_sdc_find_file 
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;   The purpose of this program is to provide information about the available simulations.
;
; :Categories:
;    Bill Daughton, Simulation
;
; :Params:
;       SC:             in, required, type=string
;                       Spacecraft (e.g., 'mms1').
;       INSTR:          in, required, type=string
;                       Instrument name (e.g., 'dfg')
;       MODE:           in, required, type=string
;                       Telemetry mode (e.g. 'fast', 'slow', 'brst')
;       LEVEL:          in, required, type=string
;                       Data level (e.g. 'l1a', 'l1b')
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Named varaible to receive the number of files found.
;       DIRECTORY:      in, optional, type=string, default=built from inputs
;                       Directory in which to find the desired data.
;       OPTDESC:        in, optional, type=string, default=''
;                       Optional descriptor in file name.
;       SEARCHSTR:      out, optional, type=string
;                       Named variable to receive the string used to search for files.
;       TIMEORDER:      in, optional, type=string, default='%Y%M%d'
;                       A MrTokens expression denoting how TSTART is ordered in the
;                           file name. The default, '%Y%M%d', is year, month, day.
;       TSTART:         in, optional, type=string
;                       Start of the time interval in which to search for files.
;       TEND:           in, optional, type=string
;                       End of the time interval in which to search for files.
; :Returns:
;       FILES:          File name(s) matching the input conditions.
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
;       2015/05/14  -   Written by Matthew Argall
;-
function mms_sdc_find_file, sc, instr, mode, level, $
COUNT=count, $
DIRECTORY=sdc_dir, $
OPTDESC=optdesc, $
SEARCHSTR=fpattern, $
TIMEORDER=timeorder, $
TSTART=tstart, $
TEND=tend
	compile_opt idl2
	on_error, 2

	;Defaults
	fpattern = ''
	if n_elements(optdesc)   eq 0 then optdesc   = ''
	if n_elements(timeorder) eq 0 then timeorder = '%Y%M%d'

;-------------------------------------------------------
; Directory ////////////////////////////////////////////
;-------------------------------------------------------

	;Build the directory
	if n_elements(directory) eq 0 then begin
		sdc_root = '/mmsa/sdc/'
		sdc_dir = filepath('', ROOT_DIR=sdc_root, SUBDIRECTORY=[sc, instr, mode, level, optdesc])
	
		;Test the directory
		if ~file_test(sdc_dir, /DIRECTORY) then $
			message, 'SDC directory does not exist: "' + sdc_dir + '".'

		;Now append the year, month, and day
		sdc_dir = filepath('', ROOT_DIR=sdc_dir, SUBDIRECTORY=['%Y', '%M', '%d'])
		
	;Use the directory given
	endif else begin
		if ~file_test(directory, /DIRECTORY) then $
			message, 'SDC directory does not exist: "' + directory + '".'
		sdc_dir = directory
	endelse

;-------------------------------------------------------
; Filename  ////////////////////////////////////////////
;-------------------------------------------------------

	;Create the file name
	fpattern = mms_construct_filename(sc, instr, mode, level, $
	                                  OPTDESC   = optdesc, $
	                                  DIRECTORY = sdc_dir, $
	                                  TSTART    = timeorder)

	;Search for the file name
	files = MrFile_Search(fpattern, $
	                      /CLOSEST, $
	                      COUNT     = count, $
	                      TIMEORDER = timeorder, $
	                      TSTART    = tstart, $
	                      TEND      = tend)

	;Because MMS file names contain a start time, but no end time,
	;the first file returned by MrFile_Search may not lie between
	;TSTART and TEND. Here, we compare the date within the file name
	;and ensure that it is within the same day as what was given
	mms_dissect_filename, files[0], TSTART=fstart
	if strmid(fstart, 0, 4) ne strmid(tstart, 0, 4) || $
	   strmid(fstart, 4, 2) ne strmid(tstart, 5, 2) || $
	   strmid(fstart, 6, 2) ne strmid(tstart, 8, 2) $
	then begin
		count -= 1
		if count gt 1 $
			then files = files[1:*] $
			else files = ''
	endif

	;Return the results.
	return, files
end
