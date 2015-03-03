; docformat = 'rst'
;
; NAME:
;
;       MMS_DISSECT_FILENAME
;
; PURPOSE:
;+
;       The purpose of this program is to dissect the filename of a data file from the
;       MMS mission.
;
; :Categories:
;       MMS
;
; :Params:
;       FILENAME:           in, required, type=string
;                           The filename of the MMS file to be dissected.
;
; :Keywords:
;       DESCRIPTOR:         out, optional, type=string
;                           Optional data product descriptor.
;       INSTRUMENT:         out, optional, type=string
;                           Instrument ID. Possible values are::
;                               'hpca'              'edi'
;                               'aspoc'             'adp'
;                               'epd'               'sdp'
;                               'epd-eis'           'adp-sdp'
;                               'epd-feeps'         'afg'
;                               'fpi'               'dfg'
;                               'des'               'afg-dfg'
;                               'dis'               'scm'
;                               'des-dis'           'fields'
;       LEVEL:              out, optional, type=string
;                           Level of data production. Possible values are::
;                               'l1a'
;                               'l1b'
;                               'l2'
;                               'ql'
;                               'l2pre'
;                               'l2plus'
;       MODE:               out, optional, type=string
;                           Data capture mode. Possible values are::
;                               'fast'
;                               'slow'
;                               'brst'
;                               'srvy'
;       SPACECRAFT:         out, optional, type=string
;                           The spacecraft ID. Possible values are::
;                               'mms1'
;                               'mms2'
;                               'mms3'
;                               'mms4'
;       START_TIME:         out, optional, type=string
;                           Start time of the data interval formatted as yyyymmddhhmmss,
;                               with irrelevant, least significant, fields dropped
;                               when files start on regular hourly or minute boundaries.
;       VERSION:            out, optional, type=string
;                           Version of the data file, formatted as 'vX.Y.Z'
;                               X - Interface number.  Increments in this number represent a
;                                       significant change to the processing software and/or to the contents of the 
;                                       file. These changes will likely break existing code that expects a specific 
;                                       file format (e.g. file reading software).  Additionally, increments in this 
;                                       number may require code changes to analysis software that expects the 
;                                       data to have been created using specific processing algorithms. The user 
;                                       should consult the appropriate meta-data for or changelogs.
;                               Y - Quality number. This number represents a change in the quality of
;                                       the data in the file, such as change in calibration or increase in fidelity. 
;                                       Changes should not impact software, but may require consideration when 
;                                       processing data.
;                               Z - Bug fix/revision number. This number changes to indicate minor
;                                       changes to the contents of the file due to reprocessing of missing data.  
;                                       Any dependent data products should generally be reprocessed if this value 
;                                       changes.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2013
;
; :History::
;   Modification History::
;       2015/02/06  -   Written by Matthew Argall
;-
pro mms_dissect_filename, filename, $
DESCRIPTOR=descriptor, $
INSTRUMENT=instrument, $
LEVEL=level, $
MODE=mode, $
SPACECRAFT=spacecraft, $
START_TIME=start_time, $
VERSION=version
	compile_opt strictarr
	on_error, 2

	;Check that a filename was provided.
	if n_elements(filename) eq 0 then message, 'A file name must be given.'
;-----------------------------------------------------
;DISSECT FILENAMES \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	str = stregex(filename, '(mms[1-4])_'       + $                ;Spacecraft ID
	                        '([a-z-]+)_'        + $                ;Instrument ID
	                        '([a-z]+)_'         + $                ;Mode
	                        '([a-z0-4]+)_'         + $             ;Data Level
	                        '([a-zA-Z0-9-]*)_?' + $                ;Optional Descriptor
	                        '([0-9]{4}[0-9]{2}[0-9]{2}[0-9]*)_' + $ ;Start Time
	                        'v([0-9]+\.[0-9]+\.[0-9])\.cdf', $     ;Version
	                        /EXTRACT, /SUBEXP)

	;Find non-matches
	iFail = where(str[0,*] eq '', nFail, COMPLEMENT=iPass, NCOMPLEMENT=nPass)
	if nFail gt 0 then begin
		message, 'Cannot dissect filenames: ', /INFORMATIONAL
		print, '    "' + transpose(filename[iFail]) + '"'
	endif
	if nPass eq 0 then return

	;Extract the subexpressions
	spacecraft   = str[1,iPass]
	instrument   = str[2,iPass]
	mode         = str[3,iPass]
	level        = str[4,iPass]
	descriptor   = str[5,iPass]
	start_time   = str[6,iPass]
	version      = str[7,iPass]
	
	;Return scalars?
	if nPass eq 1 then begin
		spacecraft = spacecraft[0]
		instrument = instrument[0]
		mode       = mode[0]
		level      = level[0]
		descriptor = descriptor[0]
		start_time = start_time[0]
		version    = version[0]
	endif
end

;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;pick a few filenames
filenames = ['mms3_dfg_hr_l2p_duration-1h1m_20010704_v1.0.0.cdf', $
             'mms3_dfg_hr_l2p_duration-1h1m_20010704_v1.0.0.cdf', $
             'mms3_edi_fast_l1a_beam_20010704_v0.2.0.cdf', $
             'mms3_edi_fast_l1a_beam_20010704_v0.2.0.cdf']

;print, the filenames
print, format='(%"The filenames used are: %s")', filenames[0]
print, format='(%"                        %s")', filenames[1]
print, format='(%"                        %s")', filenames[2]
print, format='(%"                        %s")', filenames[3]

;Dissect all of the names
mms_dissect_filename, filenames, $
                      DESCRIPTOR = descriptor, $
                      INSTRUMENT = instrument, $
                      LEVEL      = level, $
                      MODE       = mode, $
                      START_TIME = start_time, $
                      SPACECRAFT = spacecraft, $
                      VERSION    = version
                    
;breakdown the filenames and display the results.
for i = 0, n_elements(filenames) - 1 do begin
    print, '----------------------------------------------------------------------'
    print, format='(%"Results for: %s")', filenames[i]
    print, FORMAT='(%"  Spacecraft:    %s")', spacecraft[i]
    print, FORMAT='(%"  Instrument:    %s")', instrument[i]
    print, FORMAT='(%"  Mode:          %s")', mode[i]
    print, FORMAT='(%"  Level:         %s")', level[i]
    print, FORMAT='(%"  Descriptor:    %s")', descriptor[i]
    print, FORMAT='(%"  Start Time:    %s")', start_time[i]
    print, FORMAT='(%"  Version:       %s")', version[i]
    print, ''
endfor

end