; docformat = 'rst'
;
; NAME:
;       mms_edi_data29_l1a_read
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
;   Read EDI electric field mode data.
;
; :Categories:
;   MMS, EDI, Bestarg
;
; :Params:
;       FILES:          in, required, type=string/strarr
;                       Name of the EDI e-field mode file or files to be read.
;       TSTART:         in, optional, type=string
;                       Start time of the data interval to read, as an ISO-8601 string.
;       TEND:           in, optional, type=string
;                       End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       DIRECTORY:      in, optional, type=string, default=pwd
;                       Directory in which to find EDI data.
;       QUALITY:        in, optional, type=integer/intarr, default=pwd
;                       Quality of EDI beams to return. Can be a scalar or vector with
;                           values [0, 1, 2, 3].
;
; :Returns:
;       EDI:            Structure of EDI data. Fields are below. If zero beams are
;                           detected from a GD pair, its fields will be missing from
;                           the output structure. Use COUNT_GD12 and COUNT_GD21 to test.
;
; :Author:
;   Matthew Argall::
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
function mms_edi_data29_l1a_read, files, tstart, tend, $
QUALITY=quality, $
STRUCTARR=structarr
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(cdfIDs) gt 0 then $
			for i = 0, nFiles - 1 do if cdfIDs[i] ne 0 then cdf_close, cdfIDs[i]
		MrPrintF, 'LogErr'
		return, !Null
	endif

;-----------------------------------------------------
; Check Input Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of files given
	nFiles    = n_elements(files)
	tf_struct = keyword_set(structarr)
	tf_sort   = 0B

	;Dissect the file name
	mms_dissect_filename, files, $
	                      INSTR   = instr, $
	                      LEVEL   = level, $
	                      MODE    = mode, $
	                      OPTDESC = optdesc, $
	                      SC      = sc, $
	                      VX      = vx, $
	                      VY      = vy, $
	                      VZ      = vz
	
	;Ensure L1A EDI files were given
	if min(file_test(files, /READ)) eq 0 then message, 'Files must exist and be readable.'
	if min(sc      eq sc[0])    eq 0 then message, 'All files must be from the same spacecraft.'
	if min(instr   eq 'edi')    eq 0 then message, 'Only EDI files are allowed.'
	if min(level   eq 'l1a')    eq 0 then message, 'Only L1A files are allowed.'
	if min(optdesc eq 'efield') eq 0 then message, 'Only EDI eField-mode files are allowed.'
	if min(mode    eq 'brst')   eq 0 then message, 'Only burst files are allowed.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	end

;-----------------------------------------------------
; Check Version \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; v0.1.z and below had data29_gd12 and data29_gd21 swapped.
	;
	iswap = where( (vx eq 0) and (vy le 1), nswap )
	
	;Files get read in bulk, so they must follow the same convention
	if nswap ne 0 && nswap ne nFiles $
		then message, 'File version incompatability.'
	
	;Swap?
	tf_swap = nswap gt 0

;-----------------------------------------------------
; Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Variable names
	optics_vname      = mms_construct_varname(sc, instr, 'optics')
	e_gd12_vname      = mms_construct_varname(sc, instr, 'e_gd21')
	e_gd21_vname      = mms_construct_varname(sc, instr, 'e_gd21')
	data29_gd12_vname = mms_construct_varname(sc, instr, 'data29_gd12')
	data29_gd21_vname = mms_construct_varname(sc, instr, 'data29_gd21')

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])
	
	;Read data
	optics      = MrCDF_nRead(cdfIDs, e_gd12_vname,      TSTART=tstart, TEND=tend, DEPEND_0=epoch_timetag)
	e_gd12      = MrCDF_nRead(cdfIDs, e_gd12_vname,      TSTART=tstart, TEND=tend, DEPEND_0=epoch_gd12)
	e_gd21      = MrCDF_nRead(cdfIDs, e_gd21_vname,      TSTART=tstart, TEND=tend, DEPEND_0=epoch_gd21)
	data29_gd12 = MrCDF_nRead(cdfIDs, data29_gd12_vname, TSTART=tstart, TEND=tend, DEPEND_0=epoch_data29)
	data29_gd21 = MrCDF_nRead(cdfIDs, data29_gd21_vname, TSTART=tstart, TEND=tend)
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor

;-----------------------------------------------------
; Swap \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_swap then begin
		temp        = data29_gd12
		data29_gd12 = temporary(data29_gd21)
		data29_gd21 = temporary(temp)
	endif

;-----------------------------------------------------
; Return Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	edi = { tt2000_data29:  temporary(epoch_data29), $
	        tt2000_gd12:    temporary(epoch_gd12), $
	        tt2000_gd21:    temporary(epoch_gd21), $
	        tt2000_timetag: temporary(epoch_timetag), $
	        optics:         temporary(optics), $
	        energy_gd12:    temporary(e_gd12), $
	        energy_gd21:    temporary(e_gd21), $
	        data29_gd12:    temporary(data29_gd12), $
	        data29_gd21:    temporary(data29_gd21) $
	      }
	
	;Return the data
	return, edi
end