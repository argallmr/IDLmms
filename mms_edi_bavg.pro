; docformat = 'rst'
;
; NAME:
;       MMS_EDI_BAVG
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;         contributors may  be used to endorse or promote products derived from this     ;
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
;   Interpolate DFG magnetometer data onto EDI beam times, then take a component-by-component
;   average. Requires the following data::
;
;       Magnetometer:
;           Time
;           Magnetic field
;       EDI 1 & 2:
;           Time
;
;   Creates::
;       Magnetometer:
;           Interpolated time
;           Interpolated magnetic field
;           Average time
;           Average magnetic field
;       EDI 1 & 2:
;           Index array showing which beams correspond to which B_avg
;
; :Categories:
;   MMS, EDI, Bestarg
;
; :Params:
;       T_DFG:              in, required, type=lon64arr
;                           CDF_TIME_TT2000 time stamps for the magnetometer data to be
;                               averaged.
;       B_DFG_DOCS:         in, required, type=3xN fltarr
;                           Magnetic field data to be averaged, in DOCS.
;       T_EDI:              in, required, type=lon64arr
;                           General time stamps from EDI.
;       T_EDI_BEAM1:        in, required, type=lon64arr
;                           Time stamps from EDI GDU1 beam hits.
;       T_EDI_BEAM2:        in, required, type=lon64arr
;                           Time stamps from EDI GDU2 beam hits.
;
; :Keywords:
;       B_STDEV:            out, optional, type=3xN float
;                           Standard deviation for each point in `B_AVG`. It is computed
;                               as `B_BEAM1` and `B_BEAM2` are being averaged to create
;                               `B_AVG`.
;       B_BEAM1_DOCS:        out, optional, type=3xM float
;                           `B_DFG_DOCS` interpolated onto `T_EDI_BEAM1`.
;       B_BEAM2_DOCS:       out, optional, type=3xM float
;                           `B_DFG_DOCS` interpolated onto `T_EDI_BEAM2`.
;       EDI1_BEAM_INDS:     in, optional, type=3xL float.
;                           A map between the EDI beam1 hits onto which the DFG magnetic
;                               field were interpolated, and the average magnetic field 
;                               that resulted from the interopolation. INDS_EDI_BEAM2[0]
;                               returns the index into `B_AVG` for which the beam at
;                               index 0 was used.
;       EDI2_BEAM_INDS:     in, optional, type=3xK float.
;                           A map between the EDI beam2 hits onto which the DFG magnetic
;                               field were interpolated, and the average magnetic field 
;                               that resulted from the interopolation. INDS_EDI_BEAM2[0]
;                               returns the index into `B_AVG` for which the beam at
;                               index 0 was used.
;       T_AVG:              out, optional, type=long64arr
;                           Time stamps for each `B_AVG`.
;       T_ERR:              out, optional, type=2x1 float
;                           The uniform upper and lower extent of each point in `T_AVG`.
;
; :Returns:
;       B_AVG:              5-s averaged magnetic field values.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/02/15  -   Written by Matthew Argall
;       2015/03/06  -   Accepting data instead of file names. Return secondary data via
;                           keywords. Removied view and file output capabilities. - MRA
;       2015/05/03  -   Remove keywords and return data structure instead. - MRA
;-
function mms_edi_bavg, t_fg, b_fg_docs, t_edi_beam1, t_edi_beam2
	compile_opt idl2

	;Error hand1ling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg(/QUIET)
		return, -1
	endif
;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	test = keyword_set(test_case)
	view = keyword_set(view)
	if n_elements(directory) eq 0 then directory = ''
	
;-----------------------------------------------------
; Prepare Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find initial time
	;   - Convert to UT
	;   - Round down to the nearest 5-second interval 
	;   - Convert back to CDF_TIME_TT2000
	t0 = t_fg[0] < t_edi_beam1[0] < t_edi_beam2[0]
	MrCDF_Epoch, temporary(t0), yr, mo, day, hr, mn, sec, /BREAKDOWN_EPOCH
	sec -= sec mod 5
	MrCDF_Epoch, t0, yr, mo, day, hr, mn, sec, /TT2000, /COMPUTE_EPOCH
	
	;SPLINE accepts only floats or doubles
	;   - Subtract the initial time from all points and convert from nono-seconds to seconds
	time_fg    = double(t_fg        - t0) * 1d-9
	time_beam1 = double(t_edi_beam1 - t0) * 1d-9
	time_beam2 = double(t_edi_beam2 - t0) * 1d-9
	t_stop     = time_fg[-1] < time_beam1[-1] < time_beam2[-1]

	;Number of elements in each array
	n_fg    = n_elements(t_fg)
	n_beam1 = n_elements(time_beam1)
	n_beam2 = n_elements(time_beam2)
	nmax    = n_fg < n_beam1 < n_beam2
	
	;Allocate memory to output arrays
	t_avg          = lon64arr(nmax)
	b_avg          = fltarr(3, nmax)
	b_stdev        = fltarr(3, nmax)
	b_beam1_docs   = make_array(3, n_beam1, VALUE=-1e31, /FLOAT)
	b_beam2_docs   = make_array(3, n_beam2, VALUE=-1e31, /FLOAT)
	edi1_beam_inds = lonarr(n_beam1)
	edi2_beam_inds = lonarr(n_beam2)

	;Initial conditions
	;   - T_START:        Start at the first magnetometer data point.
	;   - DT:             Average over this many seconds.
	;   - DT_INTERP:      Number of seconds on either side of the averaging interval that are interpolated
	;   - TRANGE_AVG:     First time interval to be averaged.
	;   - TRANGE_INTEPR:  First time interval to be interpolated.
	;   - N_BEAM[12]_TOT: Total number of beams included throughout the averaging process.
	t_start       = time_fg[0]
	dt            = 5D
	dt_interp     = 5D
	trange_avg    = [t_start, t_start+dt]
	trange_interp = [t_start-dt_interp, t_start+dt+dt_interp]
	count         = 0L
	n_beam1_tot   = 0L
	n_beam2_tot   = 0L

;-----------------------------------------------------
; Loop Through Each Averaging Interval \\\\\\\\\\\\\\\
;-----------------------------------------------------
	while trange_avg[0] le t_stop do begin
	;-----------------------------------------------------
	; Find Interpolation Interval \\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Interpolation window
		;   it_interp  = The magnetic field is interpolated onto the beam times. Cubic
		;                Splines interpolation will be used, so to avoid interpolaing
		;                at edges, we will interpolate over a larger interval than the
		;                averaging interval, then extract the averaging interval.
		it_fg_interp    = MrIndexRange(time_fg,    trange_interp, /RIGHT_EXCLUSIVE, STATUS=status_fg)
		it_beam1_interp = MrIndexRange(time_beam1, trange_interp, /RIGHT_EXCLUSIVE, STATUS=status_gd12)
		it_beam2_interp = MrIndexRange(time_beam2, trange_interp, /RIGHT_EXCLUSIVE, STATUS=status_gd21)
		
		;No values found?
		if status_fg ne 0 then $
			message, string(FORMAT='(%"No mag found in interval %f-%fs")', trange_interp[0], trange_interp[1]), /INFORMATIONAL
		if status_gd12 ne 0 then $
			message, string(FORMAT='(%"No gd12 beams found in interval %f-%fs")', trange_interp[0], trange_interp[1]), /INFORMATIONAL
		if status_gd21 ne 0 then $
			message, string(FORMAT='(%"No gd21 beams found in interval %f-%fs")', trange_interp[0], trange_interp[1]), /INFORMATIONAL

		;Skip to next interval
		if status_fg ne 0 || (status_gd12 ne 0 && status_gd21 ne 0) then begin
			trange_avg    += dt
			trange_interp += dt
			continue
		endif
		
		;Extract the time interval for ease
		b_fg_temp = b_fg_docs[*, it_fg_interp[0]:it_fg_interp[1]]
		t_fg_temp = time_fg[it_fg_interp[0]:it_fg_interp[1]]

	;-----------------------------------------------------
	; GD12 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if status_gd12 eq 0 then begin
			;Extract the interpolateion interval
			t_beam1_temp = time_beam1[it_beam1_interp[0]:it_beam1_interp[1]]
			
			;Allocate memory to output array
			b_fg_beam1 = fltarr(3, n_elements(t_beam1_temp))

			;Interpolate (x, y, z) components
			;   - sigma ~ 0 => cubic spline
			b_fg_beam1[0,*] = spline(t_fg_temp, b_fg_temp[0,*], t_beam1_temp, 0.001)
			b_fg_beam1[1,*] = spline(t_fg_temp, b_fg_temp[1,*], t_beam1_temp, 0.001)
			b_fg_beam1[2,*] = spline(t_fg_temp, b_fg_temp[2,*], t_beam1_temp, 0.001)
			
			;Get the beams within the averaging interval
			;  --|-----{-----}-----|--
			;    |  Interpolation  |
			;          { Avg }
			it_beam1_avg = MrIndexRange(temporary(t_beam1_temp), trange_avg, /RIGHT_EXCLUSIVE, STATUS=status_gd12)
			if status_gd12 eq 0 then begin
				;Magnetic field in averaging interval
				b_fg_beam1  = b_fg_beam1[*, it_beam1_avg[0]:it_beam1_avg[1]]

				;Number of beams in the average
				n_beam1_avg = it_beam1_avg[1] - it_beam1_avg[0] + 1
			endif else begin
				n_beam1_avg = 0
				message, string(FORMAT='(%"No gd12 beams found in interval %f-%fs")', trange_avg[0], trange_avg[1]), /INFORMATIONAL
			endelse
		endif else begin
			n_beam1_avg = 0
		endelse

	;-----------------------------------------------------
	; GD21 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if status_gd21 eq 0 then begin
			;Extract the interpolateion interval
			t_beam2_temp = time_beam2[it_beam2_interp[0]:it_beam2_interp[1]]
			
			;Allocate memory to output array
			b_fg_beam2 = fltarr(3, n_elements(t_beam2_temp))

			;Interpolate (x, y, z) components
			;   - sigma ~ 0 => cubic spline
			b_fg_beam2[0,*] = spline(t_fg_temp, b_fg_temp[0,*], t_beam2_temp, 0.001)
			b_fg_beam2[1,*] = spline(t_fg_temp, b_fg_temp[1,*], t_beam2_temp, 0.001)
			b_fg_beam2[2,*] = spline(t_fg_temp, b_fg_temp[2,*], t_beam2_temp, 0.001)
			
			;Get the beams within the averaging interval
			;  --|-----{-----}-----|--
			;    |  Interpolation  |
			;          { Avg }
			it_beam2_avg = MrIndexRange(temporary(t_beam2_temp), trange_avg, /RIGHT_EXCLUSIVE, STATUS=status_gd21)
			if status_gd21 eq 0 then begin
				;Magnetic field in averaging interval
				b_fg_beam2  = b_fg_beam2[*, it_beam2_avg[0]:it_beam2_avg[1]]

				;Number of beams in the average
				n_beam2_avg = it_beam2_avg[1] - it_beam2_avg[0] + 1
			endif else begin
				n_beam2_avg = 0
				message, string(FORMAT='(%"No gd21 beams found in interval %f-%fs")', trange_avg[0], trange_avg[1]), /INFORMATIONAL
			endelse
		endif else begin
			n_beam2_avg = 0
		endelse

		b_fg_temp = !Null

	;-----------------------------------------------------
	; Find Averaging Interval & Average \\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Skip to the next interval?
		if n_beam1_avg eq 0 && n_beam2_avg eq 0 then begin
			trange_avg    += dt
			trange_interp += dt
			continue
		endif

		;Take the averge
		;   - Convert time back to nanoseconds.
		case 1 of
			n_beam1_avg gt 0 && $
			n_beam2_avg gt 0:   !Null = moment( [[b_fg_beam1], [b_fg_beam2]], DIMENSION=2, $
			                                    MEAN=temp_mean, SDEV=temp_sdev, MAXMOMENT=2 )
			n_beam1_avg gt 0:   !Null = moment( b_fg_beam1, DIMENSION=2, $
			                                    MEAN=temp_mean, SDEV=temp_sdev, MAXMOMENT=2 )
			n_beam2_avg gt 0:   !Null = moment( b_fg_beam2, DIMENSION=2, $
			                                    MEAN=temp_mean, SDEV=temp_sdev, MAXMOMENT=2 )
			;Skip to the next interval
			else: 
		endcase
		b_avg[*,count]   = temporary(temp_mean)
		b_stdev[*,count] = temporary(temp_sdev)
		t_avg[count]     = long64(trange_avg[0] * 1d9) + t0

	;-----------------------------------------------------
	; Record B-Beam Relationship \\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Record the index of B associated with each beam in GD12.
		if n_beam1_avg gt 0 then begin
			;Store the associated B-field index
			edi1_beam_inds[n_beam1_tot:n_beam1_tot+n_beam1_avg-1] = count
			
			;Keep the magnetic field value
			b_beam1_docs[*, n_beam1_tot:n_beam1_tot+n_beam1_avg-1] = b_fg_beam1
		endif

		;Record the index of B associated with each beam in GD21.
		if n_beam2_avg gt 0 then begin
			;Store the associated B-field index
			edi2_beam_inds[n_beam2_tot:n_beam2_tot+n_beam2_avg-1] = count
			
			;Keep the magnetic field value
			b_beam2_docs[*, n_beam2_tot:n_beam2_tot+n_beam2_avg-1] = b_fg_beam2
		endif

	;-----------------------------------------------------
	; Next Iteration \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Next sample
		count       += 1L
		n_beam1_tot += n_beam1_avg
		n_beam2_tot += n_beam1_avg

		;Move to next interval
		trange_avg    += dt
		trange_interp += dt
	endwhile

;-----------------------------------------------------
; Prepare Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Trim data
	avg = { t_avg:     t_avg[0:count-1], $
	        b_avg:     b_avg[*, 0:count-1], $
	        b_std:     b_stdev[*, 0:count-1], $
	        t_err:     [0.0, dt], $
	        b_gd12:    b_beam1_docs, $
	        b_gd21:    b_beam2_docs, $
	        inds_gd12: edi1_beam_inds, $
	        inds_gd21: edi2_beam_inds $
	      }

	return, avg
end