; docformat = 'rst'
;
; NAME:
;    mms_data_fetch
;
; PURPOSE:
;+
;   Find CDF data files on the UNH MMS SDC data mirror and load variable data into TPlot
;
; :Categories:
;    MMS, SITL
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
;       2015/07/24  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Search for the latest version of data files within a given time range.
;
; :Params:
;       LOCAL_FLIST:            out, required, type=strarr
;                               Names of the local MMS data files that meet search
;                                   criteria.
;       LOGIN_FLAG:             out, optional, type=bytarr
;                               Not used. Kept to prevent clashes with official version.
;       DOWNLOAD_FAIL:          out, optional, type=bytarr
;                               Not used. Kept to prevent clashes with official version.
;
; :Keywords:
;       INSTRUMENT_ID:          in, optional, type=string, default='dfg'
;                               MMS instrument ID.
;       LEVEL:                  in, optional, type=string, default='ql'
;                               Data level.
;       MODE:                   in, optional, type=string, default='srvy'
;                               Telemetry mode.
;       NO_UPDATE:              in, optional, type=boolean, default=0
;                               Not used. Kept to prevent clashes from official
;                                   mms_data_fetch.
;       OPTIONAL_DESCRIPTOR:    in, optioanl, type=string, default='*'
;                               An optional descriptor that appears in the file name.
;       RELOAD:                 in, optional, type=boolean, default=0
;                               Not used. Kept to prevent clashes from official
;                                   mms_data_fetch.
;       SC_ID:                  in, optional, type=strarr, default='1'
;                               MMS spacecraft ID: {'mms1' | 'mms2' | 'mms3' | 'mms4'}.
;-
pro mms_data_fetch, local_flist, login_flag, download_fail, $
INSTRUMENT_ID       = instr, $
LEVEL               = level, $
MODE                = mode, $
NO_UPDATE           = no_update, $
OPTIONAL_DESCRIPTOR = optdesc, $
RELOAD              = reload, $
SC_ID               = sc
	compile_opt idl2
	on_error, 2
	
	;Defaults
	_optdesc = n_elements(optdesc) eq 0 ? '' : optdesc
	
	; Check and see if data product requires full timespan
	if mode eq 'brst' then begin
		date_struct = mms_convert_timespan_to_date(/full_span)
		burst_flag = 1
	endif else begin
		date_struct = mms_convert_timespan_to_date()
	endelse

	tstart = strsplit(date_struct.start_date, '-', /EXTRACT)
	tend   = strsplit(date_struct.end_date,   '-', /EXTRACT)
	tstart = strjoin(tstart[0:2])
	tend   = strjoin(tend[0:2])

;------------------------------------
; Search For Files \\\\\\\\\\\\\\\\\\
;------------------------------------
	if instr eq 'edi' and _optdesc eq 'efield' then begin
		dirname = filepath('', $
		                   ROOT_DIR     = !mms.local_data_dir, $
		                   SUBDIRECTORY = 'edi' )
	endif else begin
		dirname = filepath('', $
		                   ROOT_DIR     = !mms.local_data_dir, $
		                   SUBDIRECTORY = [sc, instr, mode, level, _optdesc] )
	endelse
	
	;Build the file name
	;   mms#_instr_mode_level_optdesc_tstart_v#.#.#.cdf
	basename = sc + '_' + instr + '_' + mode + '_' + level
	if _optdesc ne '' then basename += '_' + _optdesc
	basename += '_*_v*.cdf'
	
	;Recursively search through all /YEAR/MONTH/[DAY/] directories
	files = file_search(dirname, basename, COUNT=count)
	if count eq 0 then $
		message, 'No files found under "' + dirname + '" matching "' + basename + '".'

;------------------------------------
; Filter by Date Range \\\\\\\\\\\\\\
;------------------------------------
	;Build the file structure
	fstart = long(strjoin(strsplit(tstart, '-', /EXTRACT)))
	fend   = long(strjoin(strsplit(tend,   '-', /EXTRACT)))
	
	;Search for most recent version
	fbases = file_basename(files)
	fparts = stregex(fbases, '(20[1-9][0-9]' + $              ;Year
	                         '[0-9]{2}'      + $              ;Month
	                         '[0-9]{2})'     + $              ;Day
	                         '([0-9]*)'      + $              ;Hour + Minute + Second
	                         '_v([0-9])+\.([0-9])+\.([0-9])' + $  ;Version
	                         '\.cdf', $                       ;Extension
	                         /SUBEXP, /EXTRACT)
	dates    = long(reform(fparts[1,*]))
	times    = long(reform(fparts[2,*]))
	versions = long(fparts[3:5,*])

	;Filter dates
	idates = where( (dates ge fstart) and (dates le fend), count )
	if count eq 0 then $
		message, 'No files found between ' + tstart + ' and ' + tend + '.'

	files    = files[idates]
	fbases   = fbases[idates]
	dates    = dates[idates]
	times    = times[idates]
	versions = versions[*,idates]

;------------------------------------
; Find Newest Version \\\\\\\\\\\\\\\
;------------------------------------
	;Find unique dates
	iUniq = uniq(dates, sort(dates))
	
	;Allocate memory
	nUniq       = n_elements(iUniq)
	local_flist = strarr(nUniq)
	
	;Step through each unique date and find the most recent version
	for i = 0, n_elements(iUniq) - 1 do begin
		;Check this date
		idate = iuniq[i]
	
		;Find duplicates
		iduplicate = where(dates eq dates[idate], count)
		
		;Select the newest version
		if count gt 1 then begin
			;Find all versions of this file
			copies = versions[*,iduplicate]
			
			;Pick the highest version
			imax1 = where(copies[0,*]     eq max(copies[0,*]))
			imax2 = where(copies[1,imax1] eq max(copies[1,imax1]))
			imax3 = where(copies[2,imax2] eq max(copies[2,imax2]))
			
			;Index of highest version
			iVersion = imax1[imax2[imax3[0]]]
		endif else begin
			iVersion = iduplicate[0]
		endelse
		
		;Store the file name
		local_flist[i] = files[iDuplicate[iVersion]]
	endfor
	
	;Return scalar?
	if nUniq eq 1 then local_flist = local_flist[0]
	download_fail = intarr(nUniq)
	login_flag    = intarr(nUniq) + 0S
end