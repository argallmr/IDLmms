; docformat = 'rst'
;
; NAME:
;    mms_fdoa_read_defeph
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Read the MMS definitive ephemeris files.
;
; :Categories:
;    MMS, FDOA, Ephemeris
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
;       2015/07/08  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Read MMS definitive ephemeris files.
;
; :Examples:
;   Read data from two files::
;       IDL> files = ['MMS1_DEFEPH_2015170_2015171.V00', $
;                     'MMS1_DEFEPH_2015171_2015172.V00']
;       IDL> data = mms_fdoa_read_defeph(files)
;       IDL> help, data
;       ** Structure <213a088>, 4 tags, length=367680, data length=367680, refs=1:
;          TT2000          LONG64    Array[5745]
;          POSITION        DOUBLE    Array[3, 5745]
;          VELOCITY        DOUBLE    Array[3, 5745]
;          MASS            DOUBLE    Array[5745]
; :Params:
;       FILENAMES:      in, required, type=string/strarr
;                       Names of the files to read.
;       TSTART:         in, optional, type=string
;                       Start time of the data interval to read, formatted as
;                           an ISO-8601 string (e.g. '2015-06-29T00:00:00')
;       TEND:           in, optional, type=string
;                       End time of the data interval to read, formatted as
;                           an ISO-8601 string (e.g. '2015-06-29T00:00:00')
;
; :Keywords:
;       HEADER:         out, optional, type=structure
;                       Named variable to recieve values contained within the header.
;       UTC:            in, optional, type=boolean, default=0
;                       If set, UTC strings will be kept in the output structre. The
;                           default is to remove them and include TT2000 times.
;       TAI:            in, optional, type=boolean, default=0
;                       If set, TAI epoch will be kept in the output structre. The
;                           default is to remove them and include TT2000 times.
;
; :Returns:
;       ATTITUDE:       Structure of attitude data. Fields are::
;                           'TT2000'   - TT2000 CDF epoch times
;                           'UTC'      - UTC time stamp of data
;                           'TAI'      - TAI elapsed SI seconds since the mission
;                                          reference epoch, 1958-001T00:00:00 UTC
;                           'POSITION' - Spacecraft position (Km) in GEI J2000 coordinates
;                           'VELOCITY' - Spacecraft velocity (Km/s) in GEI J2000 coordinates
;                           'MASS'     - Spacecraft mass (Kg)
;-
function mms_fdoa_read_defeph, filenames, tstart, tend, $
HEADER=header, $
UTC=utc, $
TAI=tai
	compile_opt idl2
	on_error, 2
	
	;Number of files given
	nFiles = n_elements(filenames)
	utc    = keyword_set(utc)
	tai    = keyword_set(tai)
	if n_elements(tstart) eq 0 then tstart = ''
	if n_elements(tstop)  eq 0 then tstop  = ''

;-------------------------------------------------------
; Read Headers \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-------------------------------------------------------
	;Read the header from the first file
	header = { nHeader:    lonarr(nFiles), $
	           spacecraft: strarr(nFiles), $
	           start_time: lon64arr(nFiles), $
	           stop_time:  lon64arr(nFiles) $
	         }
	
	;Read the rest of the headers
	for i = 0, nFiles - 1 do begin
		;Read the header
		temp = mms_fdoa_read_defeph_header(filenames[0])
		if temp eq !Null then return, !Null
		
		;Save the info
		header.nHeader[*]    = temp.nHeader
		header.spacecraft[i] = temp.spacecraft
		header.start_time[i] = temp.start_time
		header.stop_time[i]  = temp.stop_time
	endfor

;-------------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-------------------------------------------------------
	;Names of the columns
	groups       = [1, 2, 3, 3, 3, 4, 4, 4, 5]
	column_names = ['UTC', 'TAI', 'Position', 'Position', 'Position', $
	                'Velocity', 'Velocity', 'Velocity', 'Mass']
	column_types = ['String', 'Double', 'Double', 'Double', 'Double', $
	                'Double', 'Double', 'Double', 'Double']

	;Read the files
	ephemeris = MrFile_Read_nAscii(filenames, $
	                               COLUMN_NAMES = column_names, $
	                               COLUMN_TYPES = column_types, $
	                               COUNT        = count, $
	                               GROUPS       = groups, $
	                               NHEADER      = header.nHeader[0])

	;Convert TAI to TT2000
	tt2000 = mms_fdoa_epoch2tt2000(ephemeris.tai, /TAI)
	
	;Remove TAI and UTC?
	case 1 of
		tai && utc: to_remove = ['tai', 'utc']
		tai:        to_remove = 'tai'
		utc:        to_remove = 'utc'
		else:       to_remove = ''
	endcase
	
	;Remove data
	if tai || utc then ephemeris = remove_tags(ephemeris, to_remove)
	
	;Append TT2000 times
	ephemeris = create_struct('TT2000', tt2000, ephemeris)

;-------------------------------------------------------
; Unique Records \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-------------------------------------------------------
	;
	; Definitive attitude files overlap, meaning if more than
	; one file is found, it could have repeated data. Since
	; attitude data will often be interpolated to data sample
	; times, and MATLAB's interp1() function does not consider
	; repeated values to be strictly monotonically increasing,
	; we have to remove repeats.
	;
	if nFiles gt 1 then begin
		iuniq = uniq(ephemeris.tt2000, sort(ephemeris.tt2000))
		
		;Create a new structure with trimmed data
		temp = { tt2000:    ephemeris.tt2000[iuniq], $
		         Position:  ephemeris.position[*,iuniq], $
		         Velocity:  ephemeris.velocity[*,iuniq], $
		         Mass:      ephemeris.mass[iuniq] $
		       }
		if utc then temp = create_struct('UTC', ephemeris.utc[iuniq], temp)
		if tai then temp = create_struct('TAI', ephemeris.tai[iuniq], temp)
		
		;Replace the data
		ephemeris = temporary(temp)
	endif

;-------------------------------------------------------
; Select Time Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-------------------------------------------------------
	if tstart ne '' || tstop ne '' then begin
		;tt2000 range
		if tstart eq '' then tstart = attitude.tt2000[0]
		if tstop  eq '' then tstop  = attitude.tt2000[count-1]
		trange = MrCDF_Epoch_Parse([tstart, tend], '%Y-%M-%dT%H:%m:%S', /TO_TT2000)
		
		;Records to keep
		irange = MrIndexRange(attitude.tt2000, trange)
		
		;Create a new structure with trimmed data
		temp = { tt2000:   ephemeris.tt2000[irange[0]:irange[1]], $
		         Position: ephemeris.position[*,irange[0]:irange[1]], $
		         Velocity: ephemeris.velocity[*,irange[0]:irange[1]], $
		         Mass:     ephemeris.mass[irange[0]:irange[1]] $
		       }
		if utc then temp = create_struct('UTC', ephemeris.utc[irange[0]:irange[1]], temp)
		if tai then temp = create_struct('TAI', ephemeris.tai[irange[0]:irange[1]], temp)
		
		;Replace the data
		ephemeris = temporary(temp)
	endif
	
	return, ephemeris
end
