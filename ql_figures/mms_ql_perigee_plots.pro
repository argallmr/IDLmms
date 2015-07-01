; docformat = 'rst'
;
; NAME:
;    mms_ql_perigee_plots
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
;   Create a quick-look plot of the magnetic and electric fields.
;
; :Categories:
;    MMS, QL
;
; :Params:
;
; :Keywords:
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
pro mms_ql_perigee_plots
	compile_opt strictarr
	on_error, 2
	
	;Constants
	sdc_dir  = '/nfs/'
	edi_dir  = '/nfs/edi/'
	save_dir = '/nfs/edi/orbit_plots/'

;-------------------------------------------------------
; Search For FDOA DefAtt files /////////////////////////
;-------------------------------------------------------
	sc        = 'mms%([1-4]%)'
	fname     = strupcase(sc) + '_DEFATT_%Y%D_%Y%D.V*'
	test_file = filepath(fname, ROOT_DIR=sdc_dir, SUBDIRECTORY=['ancillary', sc, 'defatt'])
	files     = MrFile_Search(test_file, $
	                          COUNT     = nFiles, $
	                          TIMEORDER = '%Y%D', $
	                          VREGEX    = '[0-9]{2}')
	if nFiles eq 0 then $
		message, 'No defatt files found: "' + searchstr + '".'

;-------------------------------------------------------
; Extract Data Intervals ///////////////////////////////
;-------------------------------------------------------	
	;Plot the data
	for i = 0, nFiles - 1 do begin
		;Dissect the file name
		filebase = file_basename(files[i])
		sc = strlowcase(strmid(filebase, 0, 4))
		
		;Read the file header to get the time range
		header = mms_fdoa_read_defatt_header(files[i])
		
		;Convert TT2000 to ISO string
		trange = MrCDF_Epoch_Encode([header.start_time, header.stop_time], $
		                            PATTERN='%Y-%M-%dT%H:%m:%SZ')

		;Create the plot
;		win = mms_ql_edi_efield(sc, trange[0], trange[1], SAVE_DIR=save_dir)
;		win = mms_ql_4sc_befields(sc, trange[0], trange[1], SAVE_DIR=save_dir)
		win = mms_sdc_ql_befields(sc, trange[0], trange[1], SAVE_DIR=save_dir)
		if obj_valid(win) then obj_destroy, win
	endfor
	
end