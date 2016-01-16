;
; docformat = 'rst'
;
; NAME:
;   mms_file_search 
;
; PURPOSE:
;+
;   Find an MMS data file.
;
; :Categories:
;    MMS
;
; :Params:
;       DIR:            in, required, type=string
;                       Directory in which to search.
;       SC:             in, required, type=string
;                       Spacecraft (e.g., 'mms1').
;       INSTR:          in, required, type=string
;                       Instrument name (e.g., 'dfg')
;       MODE:           in, required, type=string
;                       Telemetry mode (e.g. 'fast', 'slow', 'brst')
;       LEVEL:          in, required, type=string
;                       Data level (e.g. 'l1a', 'l1b')
;       TSTART:         in, required, type=string
;                       Start time of data. Format is 'YYYYMMDD' for slow, fast, and srvy
;                           data, and 'YYYYMMDDhhmmss' for burst data.
;       VERSION:        in, optional, type=string, default='*'
;                       File version, formatted as 'X.Y.Z', where X, Y, and Z
;                           are integers. If not provided, all versions of the
;                           file are returned.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Named varaible to receive the number of files found.
;       OPTDESC:        in, optional, type=string, default=''
;                       Optional descriptor in file name.
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
;   mms_latest_version.pro
;   mms_latest_zversion.pro
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
;       2015/01/13  -   Written by Matthew Argall
;-
function mms_file_search, dir, sc, instr, mode, level, tstart, version, $
COUNT=count, $
OPTDESC=optdesc, $
ROOT=root
	compile_opt idl2
	on_error, 2

	;Defaults
	tf_root = keyword_set(root)
	if n_elements(optdesc) eq 0 then optdesc = ''
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

	;Forge the file name
	filename = mms_forge_filename(sc, instr, mode, level, tstart, version, OPTDESC=optdesc)

	;Search in DIR
	;   - TF_ROOT not set:  search only in DIR
	;   - DIR ~= DATA_PATH: search in both DIR and ROOT
	if ~tf_root || dir ne data_path then begin
		files = file_search(dir, filename, COUNT=count, /FULLY_QUALIFY_PATH, /TEST_REGULAR)
	endif
		
	;Search in ROOT
	if tf_root then begin
		;Create the SDC directory chain
		data_path = mms_forge_path(data_path, sc, instr, mode, level, tstart, OPTDESC=optdesc)
		
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

	;Scalar
	if count le 1 then files = files[0]

	return, files
end