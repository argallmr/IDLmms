;
; docformat = 'rst'
;
; NAME:
;   mms_file_vnewest
;
; PURPOSE:
;+
;   Return the next newest Z-version of a given file. The z-version returned is one
;   larger than the latest existing file.
;
; :Categories:
;    MMS
;
; :Params:
;       DIR:            in, required, type=string
;                       Directory in which to search for files with matching X.Y
;                           version numbers.
;       FILENAME:       in, required, type=string
;                       Name of the file for which the latest Z-version is determined.
;
; :Returns:
;       ZVERSION:       The latest z-version of `FILENAME`. Note that this is 1 +
;                           the currently existing z-version number for the given
;                           X.Y. version. If no matching X.Y version are present,
;                           version 0 is returned.
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
function mms_latest_zversion, dir, filename, $
ROOT=root
	compile_opt idl2
	on_error, 2

	;Remove the directory, if any.
	fbase = file_basename(filename)
	
	;Breakdown the file name
	mms_dissect_filename, filename, SC=sc, INSTR=instr, MODE=mode, LEVEL=level, $
	                                OPTDESC=optdesc, TSTART=tstart, VX=vx, VY=vy, VZ=vz
	
	;Replace the z-version with wildcard '*' and search
	version  = vx + '.' + vy + '.' + '*'
	fXYmatch = mms_file_search(dir, sc, instr, mode, level, tstart, version, $
	                           COUNT=count, OPTDESC=optdesc, ROOT=root)

	;Extract the z-version
	if count eq 0 then begin
		zversion = 0
	endif else begin
		mms_dissect_filename, fXYmatch, VZ=zversion
		zversion = max(fix(zversion)) + 1
	endelse

	return, zversion
end