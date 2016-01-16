;
; docformat = 'rst'
;
; NAME:
;   mms_latest_file 
;
; PURPOSE:
;+
;   Search for the latest version of an MMS data file.
;
;   At the SDC, data files are initially saved in a dropbox location before
;   being mored to their final resting place in the directory structure. This
;   program provide a mechanism for searching in one folder or the other, or
;   both.
;
;   Calling sequence::
;       FILES = mms_latest_file(DIR, SC, INSTR, MDOE, LEVEL, TSTART)
;       FILES = mms_latest_file(DIR, SC, INSTR, MDOE, LEVEL, TSTART, /ROOT)
;       FILES = mms_latest_file(DIR, SC, INSTR, MDOE, LEVEL, TSTART, ROOT=string)
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
;       FILES:          File name matching the input conditions. If no files are
;                           found, the empty string is returned.
;
; :See Also:
;   mms_file_search.pro
;   mms_latest_version.pro
;   mms_latest_zversion.pro
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
;       2015/01/14  -   Written by Matthew Argall
;-
function mms_latest_file, dir, sc, instr, mode, level, tstart, version, $
OPTDESC=optdesc, $
ROOT=root
	compile_opt idl2
	on_error, 2
	
	;Search in DIR
	;   - TF_ROOT not set:  search only in DIR
	;   - DIR ~= DATA_PATH: search in both DIR and ROOT
	fnames = mms_file_search(dir, sc, instr, mode, level, tstart, version, $
	                         OPTDESC=optdesc, COUNT=count, ROOT=root)

	;Find latest version
	if count gt 0 $
		then file = mms_latest_version(fnames) $
		else file = ''

	return, file
end