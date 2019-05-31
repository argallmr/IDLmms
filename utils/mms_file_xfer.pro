; docformat = 'rst'
;
; NAME:
;    mms_file_xfer
;
; PURPOSE:
;+
;   Copy files from DROPBOX_ROOT to DATA_PATH_ROOT.
;
; :Categories:
;    MMS
;
; :Params:
;       INSTR:              in, required, type=string
;                           Spacecraft ID of the data to be processed. Choices are:
;                               'mms1', 'mms2', 'mms3', 'mms4'
;       OPTDESC:            in, optional, type=string
;                           
;
; :Keywords:
;       DATA_PATH_ROOT:     in, optional, type=string, default=!mms_init.data_path
;                           Root of the SDC-like directory structure where data files
;                               find their final resting place.
;       DROPBOX_ROOT:       in, optional, type=string, default=!mms_init.dropbox
;                           Directory into which data files are initially saved.
;       DELETE:             in, optional, type=boolean, default=0
;                           If set, files in `DATA_PATH_ROOT` with the same file names,
;                               but different version numbers will be deleted.
;       MOVE:               in, optional, type=boolean, default=0
;                           If set, files will be moved instead of copied.
;       VERBOSE:            in, optional, type=boolean, default=0
;                           If set, print information regarding the transfer.
;
; :Author:
;    Matthew Argall::
;        University of New Hampshire
;        Morse Hall Room 348
;        8 College Road
;        Durham, NH 03824
;        matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2016/01/28  -   Written by Matthew Argall
;       2016/02/19  -   Filter was weeding out files, but not updating filter
;                           conditions. Fixed. - MRA
;       2016/02/24  -   Added the MOVE keyword. - MRA
;-
pro mms_file_xfer, $
SC=sc, $
INSTR=instr, $
MODE=mode, $
LEVEL=level, $
OPTDESC=optdesc, $
TSTART=tstart, $
VERBOSE=verbose, $
VERSION=version, $
DELETE=delete, $
DATA_PATH_ROOT=data_path_root, $
DROPBOX_ROOT=dropbox_root, $
MOVE=move
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_move    = keyword_set(move)
	tf_delete  = keyword_set(delete)
	tf_verbose = keyword_set(verbose)
	if n_elements(sc)             eq 0 then sc             = ''
	if n_elements(instr)          eq 0 then instr          = ''
	if n_elements(mode)           eq 0 then mode           = ''
	if n_elements(level)          eq 0 then level          = ''
	if n_elements(optdesc)        eq 0 then optdesc        = ''
	if n_elements(tstart)         eq 0 then tstart         = ''
	if n_elements(version)        eq 0 then version        = ''
	if n_elements(data_path_root) eq 0 then data_path_root = '/nfs/edi/'
	if n_elements(dropbox_root)   eq 0 then dropbox_root   = '/nfs/edi/temp'

;-----------------------------------------------------
; Find Files to Transfer \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find files
	files = file_search(dropbox_root, 'mms*.cdf', /FULLY_QUALIFY_PATH, COUNT=count)
	if count eq 0 then return
	
;-----------------------------------------------------
; Filter Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;SC
	if count gt 0 && sc ne '' then begin
		;Extract the spacecraft
		mms_dissect_filename, files, SC=fsc
		
		;Filter the files
		ifilter = where(fsc eq sc, count)
		if count gt 0 then files = files[ifilter]
	endif
	
	;
	;Repeat
	;
	
	;INSTR
	if count gt 0 && instr ne '' then begin
		mms_dissect_filename, files, INSTR=finstr
		ifilter = where(finstr eq instr, count)
		if count gt 0 then files = files[ifilter]
	endif
	
	;MODE
	if count gt 0 && mode ne '' then begin
		mms_dissect_filename, files, MODE=fmode
		ifilter = where(fmode eq mode, count)
		if count gt 0 then files = files[ifilter]
	endif
	
	;LEVEL
	if count gt 0 && level ne '' then begin
		mms_dissect_filename, files, LEVEL=flevel
		ifilter = where(flevel eq level, count)
		if count gt 0 then files = files[ifilter]
	endif
	
	;OPTDESC
	if count gt 0 && optdesc ne '' then begin
		mms_dissect_filename, files, OPTDESC=foptdesc
		ifilter = where(foptdesc eq optdesc, count)
		if count gt 0 then files = files[ifilter]
	endif
	
	;TSTART
	if count gt 0 && tstart ne '' then begin
		mms_dissect_filename, files, TSTART=fstart
		ifilter = where(fstart eq tstart, count)
		if count gt 0 then files = files[ifilter]
	endif
	
	;VERSION
	if count gt 0 && version ne '' then begin
		mms_dissect_filename, files, VERSION=version
		ifilter = where(fversion eq version, count)
		if count gt 0 then files = files[ifilter]
	endif

	;No files to transfer
	if count eq 0 then return

;-----------------------------------------------------
; Transfer Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;TODO: Smarter about versions:
	;        * Currently, LATEST_FILE and LATEST_ZVERSION do not look in /nfs/edi/amb/...
	;        * There may be multiple z-version in DROPBOX_ROOT
	;

	;Dissect the remaining files
	mms_dissect_filename, files, SC=fsc, INSTR=finstr, MODE=fmode, $
	                             LEVEL=flevel, OPTDESC=foptdesc, TSTART=fstart

	;Parse the start time
	mms_parse_time, fstart, year, month, day

	;Loop over each file
	for i = 0, n_elements(files) - 1 do begin
		;Form the output path
		xfr_path = mms_forge_path(data_path_root, fsc[i], finstr[i], fmode[i], flevel[i], fstart[i], $
		                          OPTDESC=foptdesc[i], /MKDIR)

		;Notify of transfer
		xfr_file = filepath(file_basename(files[i]), ROOT_DIR=xfr_path)
		if tf_verbose then MrPrintF, 'LogText', files[i], xfr_path, FORMAT='(%"Transferring file: %s --> %s")'
		
		;Delete old files
		if tf_delete then begin
			;Find similar files with different versions
			ftemp = mms_forge_filename(fsc[i], finstr[i], fmode[i], flevel[i], fstart[i], '*', OPTDESC=foptdesc[i])
			ftemp = file_search(xfr_path, ftemp, COUNT=ndelete)
			
			;Delete
			if ndelete gt 0 then begin
				if tf_verbose then MrPrintF, 'LogText', '    Deleting Files: "' + ftemp + '".'
				file_delete, ftemp
			endif
		
		;Skip transfer if file exists
		endif else begin
			if file_test(xfr_file, /REGULAR) then begin
				if tf_verbose then MrPrintF, 'LogWarn', 'File already exists. Skipping. "' + xfr_file + '".'
				continue
			endif
		endelse

		;Transfer the file
		if tf_move $
			then file_move, files[i], xfr_file $
			else file_copy, files[i], xfr_file
	endfor
end