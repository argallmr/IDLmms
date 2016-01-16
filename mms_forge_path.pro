; docformat = 'rst'
;
; NAME:
;       mms_forge_path
;
; PURPOSE:
;+
;       The purpose of this program is to create a directory structure resembling
;       that of the SDC::
;
;       scId/instrumentId/mode/dataLevel[/optionalDataProductDescriptor]/year/month[/day]
;
; :Categories:
;   MMS
;
; :Params:
;       ROOT:           in, required, type=string
;                       Root directory at which to form the directory structure.
;       SPACECRAFT:     in, required, type=string
;                       The spacecraft ID. Possible values are::
;                           'mms1'
;                           'mms2'
;                           'mms3'
;                           'mms4'
;       INSTRUMENT:     in, required, type=string
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
;       MODE:           in, required, type=string
;                       Data capture mode. Possible values are::
;                           'fast'
;                           'slow'
;                           'brst'
;                           'srvy'
;       LEVEL:          in, required, type=string
;                       Level of data production. Possible values are::
;                           'l1a'
;                           'l1b'
;                           'l2'
;                           'ql'
;                           'l2pre'
;                           'l2plus'
;       TSTART:         in, required, type=string, default='%Y%M%d' or '%Y%M%d%H%m%S'
;                       Start time of the data interval formatted as YYYYMMDD or YYYYMMDDhhmmss,
;                           with irrelevant, least significant, fields dropped
;                           when files start on regular hourly or minute boundaries.
;
; :Keywords:
;       MKDIR:          in, optional, type=boolean, default=0
;                       If set and `DATA_PATH` does not exist, it will be created. 
;       OPTDESC:        in, optional, type=string, default=''
;                       Optional field that may not be needed for all
;                           products (e.g. Quicklook and SITL).  Where it is used, 
;                           identifiers should be short (e.g. 3-8 character)
;                           descriptors that are helpful to end-users.  If a descriptor
;                           contains multiple components, hyphens are used to separate
;                           those components.
;
; :Returns:
;       FNAME:          The MMS file name.
;
; :See Also:
;   mms_construct_filename.pro
;   mms_dissect_filename.pro
;   mms_forge_filename.pro
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
function mms_forge_path, root, sc, instr, mode, level, tstart, $
OPTDESC=optdesc, $
MKDIR=mkdir
	compile_opt strictarr
	on_error, 2
	
	;Default
	if n_elements(optdesc) eq 0 then optdesc   = ''

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
	data_path = filepath('', ROOT_DIR=root, $
	                     SUBDIRECTORY=[sc, instr, mode, level, optdesc, year, month, day])
	
	;Make the directories?
	if keyword_set(mkdir) && ~file_test(data_path, /DIRECTORY) $
		then file_mkdir, data_path

	return, data_path
end