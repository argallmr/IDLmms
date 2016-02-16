;+
; docformat = 'rst'
;
; NAME:
;   mms_find_file 
;
; PURPOSE:
;+
;   Find MMS data files.
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
;       2015/10/22  -   Properly report COUNT when searching for more than one
;                           file type. - MRA
;-
function mms_find_file, sc, instr, mode, level, $
COUNT=nfiles, $
DROPBOX=dropbox, $
DIRECTORY=directory, $
OPTDESC=optdesc, $
RELAXED_TSTART=relaxed_tstart, $
SDC_ROOT=sdc_root, $
SEARCHSTR=fpattern, $
TIMEORDER=timeorder, $
TSTART=tstart, $
TEND=tend, $
UNIFORM=uniform
	compile_opt idl2
	on_error, 2

	;Defaults
	fpattern = ''
	relaxed_tstart = keyword_set(relaxed_tstart)
	uniform = n_elements(uniform) eq 0 ? 1 : keyword_set(uniform)
	if n_elements(directory) eq 0 then directory = ''
	if n_elements(dropbox)   eq 0 then dropbox   = ''
	if n_elements(sdc_root)  eq 0 then sdc_root  = ''
	if n_elements(timeorder) eq 0 then timeorder = ''
;	if n_elements(tstart)    eq 0 then tstart    = ''
	if sdc_root eq '' && directory eq '' then sdc_root = '/nfs'

;-------------------------------------------------------
; Filename  ////////////////////////////////////////////
;-------------------------------------------------------
	;Create the file name
	fpattern = mms_construct_filename(sc, instr, mode, level, $
	                                  OPTDESC   = optdesc,    $
	                                  DIRECTORY = directory,  $
	                                  SDC_ROOT  = sdc_root,   $
;	                                  TSTART    = tstart, $
	                                  UNIFORM   = uniform)

	;Search for the file name
	nfiles = 0
	for i = 0, n_elements(fpattern) - 1 do begin
		;Extract the time from the file
		ftime = stregex(fpattern[i], '([0-9]{8}[0-9]{0,6})', /SUBEXP, /EXTRACT)
		ftime = ftime[1]
		
		;Extract the mode
		mode = (strsplit(fpattern[i], '_', /EXTRACT))[2]
		if timeorder eq '' $
			then torder = mode eq 'brst' ? '%Y%M%d%H%m%S' : '%Y%M%d' $
			else torder = timeorder

		;Search for the file
		files = MrFile_Search(fpattern[i], $
		                      /CLOSEST, $
		                      COUNT     = count, $
		                      TIMEORDER = torder, $
		                      TSTART    = tstart, $
		                      TEND      = tend)
		
		;Search in the dropbox as well?
		if dropbox ne '' then begin
			;Reform the file name
			fbase        = file_basename(fpattern[i])
			fpattern_new = filepath(fbase, ROOT_DIR=dropbox)
			
			;Search for the file
			files_box = MrFile_Search(fpattern[i], $
			                          /CLOSEST, $
			                          COUNT     = nDropbox, $
			                          TIMEORDER = torder, $
			                          TSTART    = tstart, $
			                          TEND      = tend)
			
			;Append files
			if nDropbox gt 0 then begin
				files = [files, files_box]
				
			endif
		endif

		;Because MMS file names contain a start time, but no end time,
		;the first file returned by MrFile_Search may not lie between
		;TSTART and TEND. Here, we compare the date within the file name
		;and ensure that it is within the same day as what was given
		if ~relaxed_tstart && count gt 0 then begin
			mms_dissect_filename, files[0], TSTART=fstart
			if strmid(fstart, 0, 4) ne strmid(tstart, 0, 4) || $
			   strmid(fstart, 4, 2) ne strmid(tstart, 5, 2) || $
			   strmid(fstart, 6, 2) ne strmid(tstart, 8, 2) $
			then begin
				if count gt 1 $
					then files = files[1:*] $
					else files = ''
				count -= 1
			endif
		endif

		;Append
		if nfiles eq 0 $
			then fnames = files $
			else if count gt 0 then fnames = [fnames, files]
			
		;Keep count
		nfiles += count
	endfor
		
	;Return the results.
	if nfiles eq 0 then fnames = ''
	return, fnames
end