; docformat = 'rst'
;
; NAME:
;       MMS_CONSTRUCT_FILENAME
;
; PURPOSE:
;+
;       The purpose of this program is to create an MMS CDF file name using the user
;       input information::
;
;       scId_instrumentId_mode_dataLevel_optionalDataProductDescriptor_startTime_vX.Y.Z.cdf
;
; :Categories:
;   MMS
;
; :Params:
;       SPACECRAFT:     out, optional, type=string
;                       The spacecraft ID. Possible values are::
;                           'mms1'
;                           'mms2'
;                           'mms3'
;                           'mms4'
;       INSTRUMENT:     out, optional, type=string
;                       Instrument ID. Possible values are::
;                           'hpca'              'edi'
;                           'aspoc'             'adp'
;                           'epd'               'sdp'
;                           'epd-eis'           'adp-sdp'
;                           'epd-feeps'         'afg'
;                           'fpi'               'dfg'
;                           'des'               'afg-dfg'
;                           'dis'               'scm'
;                           'des-dis'           'fields'
;       MODE:           out, optional, type=string
;                       Data capture mode. Possible values are::
;                           'fast'
;                           'slow'
;                           'brst'
;                           'srvy'
;       LEVEL:          out, optional, type=string
;                       Level of data production. Possible values are::
;                           'l1a'
;                           'l1b'
;                           'l2'
;                           'ql'
;                           'l2pre'
;                           'l2plus'
;       TSTART:         in, optional, type=string, default='%Y%M%d' or '%Y%M%d%H%m%S'
;                       Start time of the data interval formatted as YYYYMMDD or YYYYMMDDhhmmss,
;                           with irrelevant, least significant, fields dropped
;                           when files start on regular hourly or minute boundaries.
;       VERSION:        in, optional, type=string, default='*'
;                       Version of the data file, formatted as 'X.Y.Z'
;                           X - Interface number.  Increments in this number represent a
;                                   significant change to the processing software and/or to the contents of the 
;                                   file. These changes will likely break existing code that expects a specific 
;                                   file format (e.g. file reading software).  Additionally, increments in this 
;                                   number may require code changes to analysis software that expects the 
;                                   data to have been created using specific processing algorithms. The user 
;                                   should consult the appropriate meta-data for or changelogs.
;                           Y - Quality number. This number represents a change in the quality of
;                                   the data in the file, such as change in calibration or increase in fidelity. 
;                                   Changes should not impact software, but may require consideration when 
;                                   processing data.
;                           Z - Bug fix/revision number. This number changes to indicate minor
;                                   changes to the contents of the file due to reprocessing of missing data.  
;                                   Any dependent data products should generally be reprocessed if this value 
;                                   changes.
;
; :Keywords:
;       OPTDESC:        in, optional, type=string, default=''
;                       Optional field that may not be needed for all
;                           products (e.g. Quicklook and SITL).  Where it is used, 
;                           identifiers should be short (e.g. 3-8 character)
;                           descriptors that are helpful to end-users.  If a descriptor
;                           contains multiple components, hyphens are used to separate
;                           those components.
;       DIRECTORY:      in, optional, type=string, default=''
;                       Directory to be appended to the file name.
;       SDC_ROOT:       in, optional, type=string, default=''
;                       Root of an SDC-like directory structure. To this, the following
;                           folders will be appended: SDC_ROOT/sc/instr/mode/level[/optdesdc]/year/month[/day]
;                           where "optdesc" is optional and and "day" is for brst data.
;
; :Returns:
;       FNAME:          The MMS file name.
;
; :See Also:
;   mms_construct_filename.pro
;   mms_dissect_filename.pro
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/01/14  -   Written by Matthew Argall
;-
;*****************************************************************************************
function mms_forge_filename, sc, instr, mode, level, tstart, version, $
OPTDESC=optdesc, $
DIRECTORY=directory, $
SDC_ROOT=sdc_root
	compile_opt strictarr
	on_error, 2
	
	
	;if no directory was supplied, get the current directory
	;if no base was chosen, go with the complete base
	if n_elements(optdesc)   eq 0 then optdesc   = ''
	if n_elements(version)   eq 0 then version   = '*'
	if n_elements(sdc_root)  eq 0 then sdc_root  = ''
	if n_elements(directory) eq 0 then directory = ''

	;Default TSTART
	;   - Default to using tokens
	;   - FPI and EDP srvy files resemble brst files
	if n_elements(tstart) eq 0 then begin
		if mode eq 'brst' then begin
			tstart = '%Y%M%d%H%m%S'
		endif else begin
			case instr of
				'edp': tstart = '%Y%M%d%H%m%S'
				'fpi': tstart = '%Y%M%d%H%m%S'
				else:  tstart = '%Y%M%d'
			endcase
		endelse
	endif
	
	;Conflicts
	if sdc_root ne '' && directory ne '' then $
		message, 'SDC_ROOT and DIRECTORY are mutually exclusive.'
	
	;Optional descriptor is optional
	if optdesc eq '' $
		then fname = strjoin([sc, instr, mode, level, tstart, 'v' + version], '_') + '.cdf' $
		else fname = strjoin([sc, instr, mode, level, optdesc, tstart, 'v' + version], '_') + '.cdf'
	
	;Directory
	if directory ne '' then data_path = directory
	if sdc_root ne '' then begin
		;Date components
		;   - DAY directory is used only for burst files.
		if strmid(tstart, 0, 1) eq '%' then begin
			year = strmid(tstart, 0, 2)
			month = strmid(tstart, 2, 2)
			day   = mode eq 'brst' ? strmid(tstart, 4, 2) : ''
		endif else if stregex(tstart, '^[0-9]+$', /BOOLEAN) then begin
			year  = strmid(tstart, 0, 4)
			month = strmid(tstart, 4, 2)
			day   = mode eq 'brst' ? strmid(tstart, 6, 2) : ''
		endif else begin
			message, 'Unknown time format. Cannot make SDC dir structure: "' + tstart + '".'
		endelse
		
		;SDC directory structure
		data_path = filepath('', ROOT_DIR=sdc_root, $
		                     SUBDIRECTORY=[sc, instr, mode, level, optdesc, year, month, day])
	endif
	
	;Create the file name(s)
	if n_elements(data_path) gt 0 then fname = filepath(fname, ROOT_DIR=data_path)

	return, fname
end