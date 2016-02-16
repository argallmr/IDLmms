; docformat = 'rst'
;
; NAME:
;    mms_fdoa_create_orbitfile
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
;   FDOA attitude files start at perigee and end at the following perigee. The file
;   headers contain START_TIME and STOP_TIME metadata fields that indicate the start
;   and stop of the data file (i.e. times of perigee). This program extracts those
;   times, associates them with an orbit number, then writes to a file.
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
pro mms_fdoa_create_orbitfile
	compile_opt strictarr
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(lun) gt 0 then free_lun, lun
		void = cgErrorMSG(/QUIET)
		return
	endif
	
	;Constants
	sdc_dir  = '/nfs/'
	sc       = ['mms1', 'mms2', 'mms3', 'mms4']

	;
	; NOTE:
	;   During boom deployment, the MMS angular frequency was increased
	;   beyond the start tracker's ability to track the stars. During
	;   this period (DOY 97-113), there is no attitude solutions and no 
	;   data files exist for any spacecraft.
	;
	;   Below, I associate orbits with file index. That means, when I
	;   get to the attitude file gap, I have to add an offset.
	;
	date = MrDOY2Date(113, 2015)
	MrCDF_Epoch, t_offset, 2015, date[1], date[0], /TT2000
	orbit_offset = 113 - 97

;-------------------------------------------------------
; Search For FDOA DefAtt files /////////////////////////
;-------------------------------------------------------
	;Step through each spacecraft
	for i = 0, 3 do begin
		;Find files
		ftemp     = strupcase(sc[i]) + '_DEFATT_%Y%D_%Y%D.V*'
		test_file = filepath(ftemp, ROOT_DIR=sdc_dir, SUBDIRECTORY=['ancillary', sc[i], 'defatt'])
		files     = MrFile_Search(test_file, $
		                          COUNT     = nFiles, $
		                          TIMEORDER = '%Y%D', $
		                          VREGEX    = '[0-9]{2}')
		if nFiles eq 0 then $
			message, 'No defatt files found: "' + test_file + '".'
		
	;-------------------------------------------------------
	; Open File & Print Header /////////////////////////////
	;-------------------------------------------------------
		fname = filepath(sc[i] + '_orbit_file.txt', ROOT_DIR=sdc_dir, SUBDIRECTORY='edi')
		openw, lun, fname, /GET_LUN
		
		;Write a header
;		print, 'ORBIT', 'START_TIME', 'STOP_TIME', FORMAT='(a5, 3x, a10, 15x, a9)'
		printf, lun, 'ORBIT', 'START_TIME', 'STOP_TIME', FORMAT='(a5, 3x, a10, 15x, a9)'
		
	;-------------------------------------------------------
	; Read & Print Orbit Times /////////////////////////////
	;-------------------------------------------------------
		for j = 0, nFiles - 1 do begin
			;Read the file header to get the time range
			header   = mms_fdoa_read_defatt_header(files[j])
			interval = [header.start_time, header.stop_time]
		
			;Convert TT2000 to ISO string
			trange = MrCDF_Epoch_Encode(interval, PATTERN='%Y-%M-%dT%H:%m:%S.%1')

			;Determine the orbit
			orbit = j + 1
			if interval[0] gt t_offset then orbit += orbit_offset

			;Write to file
;			print, j+1, trange, FORMAT='(1x, i03, 4x, a23, 2x, a23)'
			printf, lun, orbit, trange, FORMAT='(1x, i03, 4x, a23, 2x, a23)'
		endfor
		
		;Close the file
		free_lun, lun
	endfor
end