; docformat = 'rst'
;
; NAME:
;    unh_mms_load_edi_amb
;
; PURPOSE:
;+
;   Fetch EDI Ambient mode SITL products from the SDC for display using tplot.
;   The routine creates tplot variables based on the names in the mms CDF files.
;   Data files are cached locally in !mms.local_data_dir.
;
; :Categories:
;    MMS, EDI, SITL, QL
;
; :Keywords:
;        SC:           in, optional, type=string/strarr, default='mms1'
;                      Array of strings containing spacecraft
;                        ids for http query (e.g. 'mms1' or ['mms1', 'mms3']).
;                        If not used, or set to invalid id, the routine defaults'
;                        to 'mms1'
;        NO_UPDATE:    in, optional, type=boolean, default=0
;                      Set if you don't wish to replace earlier file versions
;                        with the latest version. If not set, earlier versions are deleted
;                        and replaced.
;        RELOAD:       in, optional, type=boolean, default=0
;                      Set if you wish to download all files in query, regardless
;                        of whether file exists locally. Useful if obtaining recent data files
;                        that may not have been full when you last cached them. Cannot
;                        be used with `NO_UPDATE`.
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
;       2015/07/20  -   Written by Matthew Argall
;-
pro mms_load_edi_amb, $
SC_ID     = sc_id, $
NO_UPDATE = no_update, $
RELOAD    = reload
	compile_opt idl2

;------------------------------------
; Time Interval \\\\\\\\\\\\\\\\\\\\\
;------------------------------------
	;Get the current time range
	;   - Convert it to YYYY-MM-DD/hh:mm:ss
	;   - Form start and end dates
	t = timerange(/CURRENT)
	st = time_string(t)
	start_date = strmid(st[0],0,10)
	end_date = strmatch(strmid(st[1],11,8),'00:00:00')  ? $   ;Midnight?
	               strmid(time_string(t[1]-10.d0),0,10) : $   ;Subtract 10 seconds to get previous day
	               strmid(st[1],0,10)                         ;Use date as is

;------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\
;------------------------------------
	;on_error, 2
	if keyword_set(no_update) and keyword_set(reload) then $
		message, 'ERROR: Keywords /no_update and /reload are ' + $
		         'conflicting and should never be used simultaneously.'

	;Load only quick-look survey data
	instr   = 'edi'
	level   = 'l1a'
	mode    = 'fast'
	optdesc = 'amb'

	;Default SC
	if ~keyword_set(sc) then begin
		;Use MMS1
		print, 'Spacecraft ID not set, defaulting to mms1'
		sc = 'mms1'
		
	;Check SC
	endif else begin
		;Convert to lowercase
		sc_id = strlowcase(sc_id)
		iValid = where( sc_id eq 'mms1' || sc_id eq 'mms2' || sc_id eq 'mms3' || sc_id eq 'mms4', nValid, $
		                COMPLEMENT=iInvalid, NCOMPLEMENT=nInvalid )
		
		;No valid spacecraft given
		if nValid eq 0 then begin
			message,"Invalid spacecraft ids. Using default spacecraft mms1", /CONTINUE
			sc_id='mms1'
			
		;Some valid and invalid
		endif else if nInvalid gt 0 then begin
			message, "Both valid and invalid entries in spacecraft id array. Neglecting invalid entries...", /CONTINUE
			print,"... using entries: ", sc_id[iValid]
			sc_id = sc_id[iValid]
		endif
	endelse

;------------------------------------
; Check for EDI data first \\\\\\\\\\
;------------------------------------
	for j = 0, n_elements(sc)-1 do begin
		;fetch the data
		;   - LOGIN_FLAG=1 indicates failed download. Need to check local cache.
		mms_data_fetch, local_flist, login_flag, download_fail, $
		                SC_ID               = sc_id[j], $
		                INSTRUMENT_ID       = instr, $
		                MODE                = mode, $
		                OPTIONAL_DESCRIPTOR = optdesc, $
		                LEVEL               = level, $
		                NO_UPDATE           = no_update, $   ;Do not check for newer file versions
		                RELOAD              = reload         ;Download no matter what

		;Which downloads succeeded/failed
		loc_fail = where(download_fail eq 1, count_fail, $
		                 COMPLEMENT=loc_success, NCOMPLEMENT=count_success)
		
		;Did some fail?
		if count_fail gt 0 then $
			message, 'Some of the downloads from the SDC timed out. Try again later if plot is missing data.', /INFORMATIONAL
		
		;Were downloads successful?
		if count_success gt 0 then begin
			local_flist = local_flist[loc_success]
		endif else if count_success eq 0 then begin
			login_flag = 1
		endif

		;
		;if n_elements(local_flist) eq 1 and strlen(local_flist(0)) eq 0 then begin
		;  login_flag = 1
		;endif

		; Now we need to do one of two things:
		; If the communication to the server fails, or no data on server, we need to check for local data
		; If the communication worked, we need to open the flist

		; First lets handle failure of the server


		;Check local cache.
		;   - FILE_FLAG=1 means no file found.
		file_flag = 0
		if login_flag eq 1 then begin
			message, 'Unable to locate files on the SDC server, checking local cache...', /INFORMATIONAL
			mms_check_local_cache, local_flist, file_flag, mode, 'dfg', level, sc_id[j], $
			                       OPTIONAL_DESCRIPTOR = optdesc
		endif

		;Were files successfully found either on the server or locally?
		if login_flag eq 0 or file_flag eq 0 then begin
			; We can safely verify that there is some data file to open, so lets do it

			;If more than one file was found, sort by date.
			;   - Guaranteed to have at least one file via flag checking.
			if n_elements(local_flist) gt 1 $
				 then files_open = mms_sort_filenames_by_date(local_flist) $
				 else files_open = local_flist

			;Open the initial file and get its data
			edi_struct = mms_sitl_open_edi_amb_cdf(files_open[0])
			time              = edi_struct.time
			counts_gdu1       = edi_struct.counts_gdu1
			counts_gdu2       = edi_struct.counts_gdu2
			counts_gdu1_vname = edi_struct.counts_gdu1_vname
			counts_gdu2_vname = edi_struct.counts_gdu2_vname
			edi_struct        = !Null

			;Open additonal files, if found.
			if n_elements(files_open) gt 1 then begin
				for i = 1, n_elements(files_open)-1 do begin
					;Append data
					temp_struct = unh_mms_sitl_open_edi_amb_cdf(files_open[i])
					time        = [time,        temp_struct.time]
					counts_gdu1 = [counts_gdu1, temp_struct.counts_gdu1]
					counts_gdu2 = [counts_gdu1, temp_struct.counts_gdu2]
				endfor
			endif

			;Store as TPlot variables.
			store_data, counts_gdu1_vname, DATA = {x: time, y: counts_gdu1}
			store_data, counts_gdu2_vname, DATA = {x: time, y: counts_gdu2}
			
		;No data found
		endif else begin
			message, 'No EDI Ambient data available locally or at SDC or invalid query!', /INFORMATIONAL
		endelse
	endfor
end