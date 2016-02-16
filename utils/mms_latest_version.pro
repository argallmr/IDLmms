;+
; docformat = 'rst'
;
; NAME:
;   mms_latest_version
;
; PURPOSE:
;+
;   Find the newest version of a given set of files. File names are assumed to be
;   identical, with the exception of their version numbers.
;
; :Categories:
;    MMS
;
; :Params:
;       FILES:          in, required, type=string/strarr
;                       Names of MMS CDF data files. All names must be the same,
;                           with the exception of their version numbers.
;
; :Keywords:
;       VERSION:        out, optional, type=string
;                       File version of the most recent file given.
;
; :Returns:
;       FILES:          File name(s) matching the input conditions. If no files are
;                           found, the empty string is returned and `COUNT`=0.
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
function mms_latest_version, files, $
VERSION=version
	compile_opt idl2
	on_error, 2

	;File with the newest version
	newest_file = files

	;File names end in vX.Y.Z.cdf, where X, Y, and Z are integers
	;   - Extract X, Y, and Z
	v = stregex(files, 'v([0-9]+)\.([0-9]+)\.([0-9]+)\.cdf$', /SUBEXP, /EXTRACT)
	
	;Step through X, Y, and Z
	for i = 0, 2 do begin
		;Find the highest version
		vsn = fix(reform(v[i+1,*]))
		idx = where( vsn eq max(vsn), cnt )
		
		;Filter out
		v           = v[*,idx]
		newest_file = newest_file[idx]
	endfor
	
	;If there are more than one file with the same version,
	;select the first occurrence
	if cnt gt 1 then begin
		v           = v[*,0]
		newest_file = newest_file[*,0]
	endif
	
	;Form the version number
	version = v[1] + '.' + v[2] + '.' + v[3]

	return, newest_file
end