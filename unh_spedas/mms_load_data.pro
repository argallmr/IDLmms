; docformat = 'rst'
;
; NAME:
;    mms_load_data
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
;       SC:             in, optional, type=strarr, default='1'
;                       MMS spacecraft ID: {'1' | '2' | '3' | '4'}.
;       INSTR:          in, optional, type=string, default='dfg'
;                       MMS instrument ID.
;       MODE:           in, optional, type=string, default='srvy'
;                       Telemetry mode.
;       LEVEL:          in, optional, type=string, default='ql'
;                       Data level.
;
; :Keywords:
;       TSTART:         in, optional, type=string, default=''
;                       Start of the time interval in which to load data. Time should
;                           be formatted as 'YY-MM-DD/hh:mm:ss'.
;       TEND:           in, optional, type=string, default=''
;                       End of the time interval in which to load data. Time should
;                           be formatted as 'YY-MM-DD/hh:mm:ss'.
;       OPTDESC:        in, optioanl, type=string, default='*'
;                       An optional descriptor that appears in the file name.
;-
function mms_load_data_file_search, sc, instr, mode, level, $
TSTART  = tstart, $
TEND    = tend, $
OPTDESC = optdesc
	compile_opt idl2
	on_error, 2
	
	;Defaults
	_optdesc = n_elements(optdesc) eq 0 || optdesc eq '*' ? '' : optdesc

;------------------------------------
; Search For Files \\\\\\\\\\\\\\\\\\
;------------------------------------
	if instr eq 'edi' and optdesc eq 'efield' then begin
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
	if optdesc ne '' && _optdesc ne '*' then basename += '_' + _optdesc
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
	nUniq  = n_elements(iUniq)
	fnames = strarr(nUniq)
	
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
		fnames[i] = files[iDuplicate[iVersion]]
	endfor
	
	;Return scalar?
	if nUniq eq 1 then fnames = fnames[0]
	return, fnames
end


;+
;   Load MMS data from the UNH MMS SDC mirror into TPLOT.
;
;   NOTE:
;       In order to use this function, you must initialize SPEDAS with the
;       LOCAL_DATA_DIR set to wherever the root of the MMS mirror is located.
;       For example, on Chipolata, I would do
;
;           mms_init, LOCAL_DATA_DIR = '/nfs/'
;
;       From what I understand, the trailing "/" is required, even on Windows.
;
; :Keywords:
;       TRANGE:             in, optional, type=strarr(2)
;                           Time range over which to load data. Time should be
;                               formatted as 'YY-MM-DD/hh:mm:ss'. If not provided,
;                               and if one has not previously been loaded with the
;                               TIMESPAN or similar procedure, the command prompt
;                               will ask you to enter a time range.
;       PROBES:             in, optional, type=strarr, default='1'
;                           MMS spacecraft ID: {'1' | '2' | '3' | '4'}.
;       DATATYPE:           in, optioanl, type=string, default='*'
;                           An optional descriptor that appears in the file name.
;       LEVEL:              in, optional, type=string, default='ql'
;                           Data level.
;       INSTRUMENT:         in, optional, type=string, default='dfg'
;                           MMS instrument ID.
;       DATA_RATE:          in, optional, type=string, default='srvy'
;                           Telemetry mode.
;       LOCAL_DATA_DIR:     in, optional, type=string, default='/nfs/'
;                           Local location of data. Directory structure must be the
;                               same as the SDC or SDC mirror.
;       SOURCE:             in, optional, type=structure, default=!mms
;                           Initialization source -- set by *_init.
;       GET_SUPPORT_DATA:   in, optional, type=boolean, default=false
;                           If set, data classified as "support_data" will also be
;                               loaded from the CDF into TPLOT.
;       LOGIN_INFO:         in, optional, deprecated, type=struct
;                           SDC log-in information. Not used, but kept to ensure there
;                               are no problems when we replace the official "mms_load_data"
;                               with this version.
;       TPLOTNAMES:         out, optional, type=strarr
;                           Names of the data variables loaded into TPlot.
;-
pro mms_load_data, $
TRANGE           = trange, $
PROBES           = probes, $
DATATYPE         = datatype, $
LEVEL            = level, $
INSTRUMENT       = instrument, $
DATA_RATE        = data_rate, $
LOCAL_DATA_DIR   = local_data_dir, $
SOURCE           = source, $
GET_SUPPORT_DATA = get_support_data, $
LOGIN_INFO       = login_info, $
TPLOTNAMES       = tplotnames
	compile_opt idl2
	on_error, 2

;------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\
;------------------------------------
	;Setup for MMS version of SPEDAS
	;   - Data is searched for/stored in LOCAL_DATA_DIR
	if n_elements(local_data_dir) eq 0 then local_data_dir = '/nfs/'
	mms_init, LOCAL_DATA_DIR = local_data_dir

	;Default values
	if undefined(source)         then source         = !mms
	if undefined(probes)         then probes         = ['1'] ; default to MMS 1
	if undefined(datatype)       then datatype       = '*' else datatype = strlowcase(datatype)
	if undefined(level)          then level          = 'ql' ; default to quick look
	if undefined(instrument)     then instrument     = 'dfg'
	if undefined(data_rate)      then data_rate      = 'srvy'
	if undefined(local_data_dir) then local_data_dir = !mms.local_data_dir
	
	;If a time range is not given, TIMERANGE() will ask for one at the command prompt
	if ~undefined(trange) && n_elements(trange) eq 2 $
		then tr = timerange(trange) $
		else tr = timerange()

;------------------------------------
; Search For Files \\\\\\\\\\\\\\\\\\
;------------------------------------
	;Step through each given spacecraft
	for probe_idx = 0, n_elements(probes)-1 do begin
		probe       = 'mms' + strcompress(string(probes[probe_idx]), /rem)
		pathformat  = '/YYYY/MM/DD'
		daily_names = file_dailynames(file_format=pathformat, trange=tr, /unique, times=times)

		; updated to match the path at SDC; this path includes data type for 
		; the following instruments: EDP, DSP, EPD-EIS, FEEPS, FIELDS, HPCA, SCM (as of 7/23/2015)
		sdc_path = instrument + '/' + data_rate + '/' + level
		sdc_path = datatype ne '*' ? sdc_path + '/' + datatype + daily_names : sdc_path + daily_names

		;Step through each path
		for name_idx = 0, n_elements(sdc_path)-1 do begin
			day_string = time_string(tr[0], tformat='YYYY-MM-DD')
			end_string = time_string(tr[1], tformat='YYYY-MM-DD')

			;Search for files in LOCAL_DATA_DIR
			files = mms_load_data_file_search( probe, instrument, data_rate, level, $
			                                   TSTART  = day_string, $
			                                   TEND    = end_string, $
			                                   OPTDESC = datatype )
		endfor

		if ~undefined(files) then cdf2tplot, files, tplotnames = tplotnames, varformat='*'
	
		; forget about the daily files for this probe
		undefine, files
	endfor

;------------------------------------
; Select Time Range \\\\\\\\\\\\\\\\\
;------------------------------------
	; time clip the data
	if ~undefined(tr) && ~undefined(tplotnames) then begin
		if (n_elements(tr) eq 2) and (tplotnames[0] ne '') then begin
			time_clip, tplotnames, tr[0], tr[1], replace=1, error=error
		endif
	endif
end