; docformat = 'rst'
;
; NAME:
;    mms_fdoa_read_defatt
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
;   Read the MMS definitive attitude files.
;
; :Categories:
;    MMS, FDOA, ATTITUDE
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
;       2015/06/29  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Read MMS definitive attitude files.
;
; :Examples:
;   Read data from two files::
;       IDL> files = ['MMS1_DEFATT_2015174_2015175.V00', $
;                     'MMS1_DEFATT_2015175_2015176.V00']
;       IDL> data = mms_fdoa_read_defatt(files)
;       IDL> help, data
;       ** Structure <82ca8b8>, 8 tags, length=44561376, data length=44561376, refs=1:
;          TT2000          LONG64    Array[464181]
;          Q               FLOAT     Array[4, 464181]
;          W               FLOAT     Array[4, 464181]
;          Z               FLOAT     Array[3, 464181]
;          L               FLOAT     Array[3, 464181]
;          P               FLOAT     Array[3, 464181]
;          NUT             FLOAT     Array[464181]
;          QF              STRING    Array[464181]
;
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
;                           'TT2000' - TT2000 CDF epoch times
;                           'Q'      - quaternion (ECI-to-BCS), X,Y,Z rotation rate components (deg/s)
;                                        (instantaneous spin axis in body frame)
;                           'W'      - w-phase (Sun-to-body-X dihedral angle about rotation rate vector) (deg)
;                           'Z'      - right ascension (deg) and declination (deg) of body Z-axis, 
;                                        Z-phase (Sun-to-body-X dihedral angle about body Z-axis) (deg)
;                           'L'      - right ascension (deg) and declination (deg) of angular momentum (L),
;                                        L-phase (Sun-to-body-X dihedral angle about angular momentum vector L) (deg),
;                           'P'      - right ascension (deg) and declination (deg) of major principal axis (P),
;                                        P-phase (Sun-to-body-X dihedral angle about major principal axis P) (deg)
;                           'Nut'    - nutation angle (deg)
;                           'QF'     - quality flag: EKF=good extended Kalman filter solution with star tracker data
;                                        CNV = filter not yet converged
;                                        SUN = no tracker data; spin phase obtained from Sun sensor data
;                                        INT = no tracker or Sun sensor data; phase interpolated from neighboring tracker data
;                                        BAD = no tracker or Sun sensor data for a time span too large for interpolation
;-
function mms_fdoa_read_defatt, filenames, tstart, tend, $
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
	if n_elements(tend)   eq 0 then tend   = ''

;-------------------------------------------------------
; Read Headers \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-------------------------------------------------------
	;Read the header from the first file
	header = { att_err:      fltarr(3, nFIles), $
	           nHeader:      lonarr(nFiles), $
	           inertia_cal:  strarr(nFiles), $
	           start_time:   strarr(nFiles), $
	           stop_time:    strarr(nFiles), $
	           rate_err:     fltarr(3, nFIles), $
	           zMPA:         fltarr(3, nFIles) $
	         }
	
	;Read the rest of the headers
	for i = 0, nFiles - 1 do begin
		;Read the header
		temp = mms_fdoa_read_defatt_header(filenames[0])
		if temp eq !Null then return, !Null
		
		;Save the info
		header.att_err[*,i]   = temp.att_err
		header.nHeader[*]     = temp.nHeader
		header.inertia_cal[i] = temp.inertia_cal
		header.start_time[i]  = temp.start_time
		header.stop_time[i]   = temp.stop_time
		header.rate_err[*,i]  = temp.rate_err
		header.zMPA[*,i]      = temp.zMPA
		
		;Make sure zMPA has not changed
		if i gt 0 then $
			if ~array_equal(temp.zMPA, header.zMPA[*,i-1]) $
				then message, 'z-MPA has changed.', /INFORMATIONAL
	endfor

;-------------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-------------------------------------------------------
	;Names of the columns
	groups       = [1, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 9]
	column_names = ['UTC', 'TAI', 'q', 'q', 'q', 'q', 'w', 'w', 'w', 'w', $
	                'z', 'z', 'z', 'L', 'L', 'L', 'P', 'P', 'P', 'Nut', 'QF']
	column_types = ['String', 'Double', 'Float', 'Float', 'Float', 'Float', $
	                'Float', 'Float', 'Float', 'Float', $
	                'Float', 'Float', 'Float', 'Float', 'Float', 'Float', $
	                'Float', 'Float', 'Float', 'Float', 'String']
	
	;Read the files
	attitude = MrFile_Read_nAscii(filenames, $
	                              COLUMN_NAMES = column_names, $
	                              COLUMN_TYPES = column_types, $
	                              COUNT        = count, $
	                              GROUPS       = groups, $
	                              NHEADER      = header.nHeader[0], $
	                              NFOOTER      = 1)

	;Convert TAI to TT2000
	tt2000 = mms_fdoa_epoch2tt2000(attitude.tai, /TAI)
	
	;Remove TAI and UTC?
	case 1 of
		tai && utc: to_remove = ['tai', 'utc']
		tai:        to_remove = 'tai'
		utc:        to_remove = 'utc'
		else:       to_remove = ''
	endcase
	
	;Remove data
	if tai || utc then attitude = remove_tags(attitude, to_remove)
	
	;Append TT2000 times
	attitude = create_struct('TT2000', tt2000, attitude)

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
		iuniq = uniq(attitude.tt2000, sort(attitude.tt2000))
		
		;Create a new structure with trimmed data
		temp = { tt2000: attitude.tt2000[iuniq], $
		         q:      attitude.q[*,iuniq], $
		         w:      attitude.w[*,iuniq], $
		         z:      attitude.z[*,iuniq], $
		         L:      attitude.L[*,iuniq], $
		         P:      attitude.P[*,iuniq], $
		         Nut:    attitude.Nut[iuniq], $
		         QF:     attitude.QF[iuniq] $
		       }
		if utc then temp = create_struct('UTC', attitude.utc[iuniq], temp)
		if tai then temp = create_struct('TAI', attitude.tai[iuniq], temp)
		
		;Replace the data
		attitude = temporary(temp)
	endif

;-------------------------------------------------------
; Select Time Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-------------------------------------------------------
	if tstart ne '' || tend ne '' then begin
		;Number of elements might have been reduced
		n = n_elements(attitude.tt2000)
	
		;tt2000 range
		if tstart eq '' then tstart = attitude.tt2000[0]
		if tend   eq '' then tend   = attitude.tt2000[n-1]
		trange = MrCDF_Epoch_Parse([tstart, tend], PATTERN='%Y-%M-%dT%H:%m:%S', /TO_TT2000)
		
		;Records to keep
		irange = MrIndexRange(attitude.tt2000, trange)
		
		;Create a new structure with trimmed data
		temp = { tt2000: attitude.tt2000[irange[0]:irange[1]], $
		         q:      attitude.q[*,irange[0]:irange[1]], $
		         w:      attitude.w[*,irange[0]:irange[1]], $
		         z:      attitude.z[*,irange[0]:irange[1]], $
		         L:      attitude.L[*,irange[0]:irange[1]], $
		         P:      attitude.P[*,irange[0]:irange[1]], $
		         Nut:    attitude.Nut[irange[0]:irange[1]], $
		         QF:     attitude.QF[irange[0]:irange[1]] $
		       }
		if utc then temp = create_struct('UTC', attitude.utc[irange[0]:irange[1]])
		if tai then temp = create_struct('TAI', attitude.tai[irange[0]:irange[1]])
		
		;Replace the data
		attitude = temporary(temp)
	endif
	
	return, attitude
end
