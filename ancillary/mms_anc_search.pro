;
; docformat = 'rst'
;
; NAME:
;   mms_anc_search 
;
; PURPOSE:
;+
;   Find the latest version of MMS ancillary data files.
;
; :Categories:
;    MMS, Ancillary
;
; :Params:
;       DIR:            in, required, type=string
;                       Directory in which to search.
;       SC:             in, required, type=string
;                       Spacecraft: 'mms1', 'mms2', 'mms3', 'mms4'.
;       INSTR:          in, required, type=string
;                       Instrument name (e.g., 'defatt', 'defeph', 'predatt', 'predeph')
;       TSTART:         in, required, type=string
;                       Start time of data. Formatted as either 'YYYYMMDD'
;                           or 'YYYYMMDDhhmmss'
;       VERSION:        in, optional, type=string, default='*'
;                       File version, formatted as 'XX', Where XX is an integer
;                           of no fewer than 2 digits (e.g. 00, 01). If not provided,
;                           the most recent version of the files are returned.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Named varaible to receive the number of files found.
;       ROOT:           in, optional, type=boolean | string, default=0
;                       Either a scalar boolean or string value. If boolean and set,
;                           then `DIR` is treated as the root of an SDC-like directory
;                           structure. If a scalar string, then `DIR` is treated
;                           normally and ROOT is the root of an SDC-like directory
;                           structure. In the latter case, files are searched for in
;                           both DIR and `ROOT`/[...]/.
;
; :Returns:
;       FILES:          File name(s) matching the input conditions. If no files are
;                           found, the empty string is returned and `COUNT`=0.
;
; :See Also:
;   mms_file_search.pro
;   mms_latest_file.pro
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
;       2015/01/16  -   Written by Matthew Argall
;-
function mms_anc_search, dir, sc, instr, tstart, version, $
COUNT=count, $
ROOT=root
	compile_opt idl2
	on_error, 2

	;Defaults
	tf_root = keyword_set(root)
	if n_elements(version) eq 0 then version = '*'
	
	data_path = ''
	count     = 0
	
	;Where are we searching?
	if keyword_set(root) then begin
		;ROOT is the root of an SDC directory structure
		if isa(root, /SCALAR, 'STRING') then begin
			data_path = root
		;DIR is the root of an SDC directory structure
		endif else if isa(root, /SCALAR, /INTEGER) then begin
			data_path = dir
		endif else begin
			message, 'ROOT must be a scalar boolean or string value.'
		endelse
	endif

	;Day before and day after
	mms_parse_time, tstart, year, month, day, /INTEGER
	doy  = julday(month, day, year) - julday(1, 1, year) + 1

	;Forge the file name
	filename = string(FORMAT='(%"%s_%s_*%04i%03i*.V%s")', $
	                  strupcase(sc), strupcase(instr), year, doy, version)
	
;------------------------------------------------------
; Search in DIR                                       |
;------------------------------------------------------

	;Search in DIR
	;   - TF_ROOT not set:  search only in DIR
	;   - DIR ~= DATA_PATH: search in both DIR and ROOT
	if ~tf_root || dir ne data_path then begin
		files = file_search(dir, filename, COUNT=count, /FULLY_QUALIFY_PATH, /TEST_REGULAR)
	endif
	
;------------------------------------------------------
; Search in ROOT                                      |
;------------------------------------------------------
	if tf_root then begin
		;Housekeeping files are found in DATA_PATH/HK
		data_path = filepath('', ROOT_DIR=data_path, SUBDIRECTORY=['ancillary', sc, instr])

		;Search for files
		froot = file_search(data_path, filename, COUNT=croot, /FULLY_QUALIFY_PATH, /TEST_REGULAR)

		;Combine results
		;   - If both methods return results, combine them
		;   - If the first method returned 0 results, use ROOT results
		;     regardless of whether files were found.
		if count gt 0 && croot gt 0 then begin
			files  = [files, temporary(froot)]
			count += temporary(croot)
		endif else if count eq 0 then begin
			files = temporary(froot)
			count = temporary(croot)
		endif
	endif
	
;------------------------------------------------------
; Find Latest Version                                 |
;------------------------------------------------------
	;Strip the version number from all files
	if count gt 1 then begin
		fbase = file_basename(files)
		fnov = stregex(fbase, '(.*)[0-9][0-9]+$', /SUBEXP, /EXTRACT)
		fnov = reform(fnov[1,*])
	
		;Sort and find unique files
		isort = sort(fnov)
		iuniq = uniq(fnov, isort)
		nuniq = n_elements(iuniq)
		fout  = strarr(nuniq)

		;Find the latest version of each unique file
		for i = 0, nuniq-1 do begin
			;Extract the file version
			these_files = files[iuniq[i]]
			v = stregex(these_files, '([0-9][0-9]+)$', /SUBEXP, /EXTRACT)
			v = fix(reform(v[1,*]))
		
			;Find the largest version
			!Null   = max(v, imax)
			fout[i] = these_files[imax]
		endfor
		
		files = temporary(fout)
		count = temporary(nuniq)
	endif

	;Scalar
	if count le 1 then files = files[0]

	return, files
end