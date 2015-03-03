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
;   The purpose of this program is to DFG magnetometer data onto the beam times of EDI
;   then average the components of B within a short window.
;
; :Categories:
;   MMS, EDI
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       mms_construct_filename.pro
;       mms_construct_varname.pro
;       mms_dissect_filename.pro
;       MrCDF_File__Define.pro
;       MrIndexRange.pro
;       MrTimeStamp.pro
;       MrWindow__Define.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2015/02/15  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Create a file with the averaged magnetic field data.
;
; :Params:
;       DIRECTORY:          in, required, type=string
;                           Directory in which the file will be saved.
;       T_AVG:              in, required, type=long64
;                           Time stamps of the `B_AVG`, in CDF_TIME_TT2000 format.
;       B_AVG:              in, required, type=3xN fltarr
;                           Average magnetic field data.
;       IND_EDI_BEAM1:      in, required, type=long
;                           A map between the EDI beam1 hits onto which the DFG magnetic
;                               field were interpolated, and the average magnetic field
;                               that resulted from the interopolation. INDS_EDI_BEAM2[0]
;                               returns the index into `B_AVG` for which the beam at index
;                               0 was used.
;       IND_EDI_BEAM2:      in, required, type=long
;                           A map between the EDI beam2 hits onto which the DFG magnetic
;                               field were interpolated, and the average magnetic field
;                               that resulted from the interopolation. INDS_EDI_BEAM2[0]
;                               returns the index into `B_AVG` for which the beam at index
;                               0 was used.
;       B_BEAM1             in, required, type=3xM fltarr
;                           Magnetic field data interpolated onto the EDI beam1 times. It
;                               is this magnetic field that was averaged to get `B_AVG`.
;       B_BEAM2             in, required, type=3xM fltarr
;                           Magnetic field data interpolated onto the EDI beam2 times. It
;                               is this magnetic field that was averaged to get `B_AVG`.
;       DFG_FILE:           in, required, type=string
;                           The original magnetometer file(s) use to create `B_AVG`
;       EDI_FILE:           in, required, type=string
;                           The original EDI file(s) used to create `B_AVG`
;-
function mms_edi_bavg_file, directory, t_avg, b_avg, $
                            inds_edi_beam1, inds_edi_beam2, $
                            b_beam1, b_beam2, $
                            dfg_file, edi_file
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(theCDF) then begin
			theCDF -> Delete
			obj_destroy, theCDF
		endif
		void = cgErrorMsg()
		return, ''
	endif
	
	;Current version of the program
	version = '0.0.0'
	
	;Get pieces from the DFG file name
	file = file_basename(dfg_file)
	mms_dissect_filename, file, SPACECRAFT=sc, INSTRUMENT=inst, MODE=mode, LEVEL=level, START_TIME=start_time

	;Create a file name
	optdesc = 'beam-avg'
	outfile = mms_construct_filename(sc, inst, mode, level, $
	                                 DESCRIPTOR = descriptor, $
	                                 DIRECTORY  = directory, $
	                                 START_TIME = start_time, $
	                                 VERSION    = version)
	mms_no = strmid(sc, 3, 1)
	
	;Time stamp
	void  = MrTimeStamp(YEAR=yr, MONTH=mo, DAY=day)
	today = yr + mo + day
	
	;Create the CDF file
	theCDF = MrCDF_File(outfile, /CREATE, /CLOBBER)

;-------------------------------------------------------
; Global Attributes ////////////////////////////////////
;-------------------------------------------------------
	;Create Global Attributes
	theCDF -> WriteGlobalAttr, 'Data_type',                  mode + '_' + level + '_' + optdesc,     /CREATE
	theCDF -> WriteGlobalAttr, 'Data_version',               version,                                /CREATE
	theCDF -> WriteGlobalAttr, 'Descriptor',                 strupcase(inst),                        /CREATE
	theCDF -> WriteGlobalAttr, 'Discipline',                 'Space Physics>Magnetospheric Science', /CREATE
	theCDF -> WriteGlobalAttr, 'Generation_date',            today,                                  /CREATE
	theCDF -> WriteGlobalAttr, 'Instrument_type',            'Magnetic Fields (space)',              /CREATE
	theCDF -> WriteGlobalAttr, 'Logical_file_id',            file,                                   /CREATE
	theCDF -> WriteGlobalAttr, 'Logical_source',             sc + '_dfg_' + mode + level + optdesc, /CREATE
	theCDF -> WriteGlobalAttr, 'Logical_source_description', ' ',                                    /CREATE
	theCDF -> WriteGlobalAttr, 'Mission_group',              'MMS',                                  /CREATE
	theCDF -> WriteGlobalAttr, 'PI_affiliation',             'SWRI, UNH',                            /CREATE
	theCDF -> WriteGlobalAttr, 'PI_name',                    'J. Burch, R. Torbert',                 /CREATE
	theCDF -> WriteGlobalAttr, 'Project',                    'STP>Solar Terrestrial Physics',        /CREATE
	theCDF -> WriteGlobalAttr, 'Source_name',                'MMS' + mms_no + '>MMS Satellite Number ' + mms_no, /CREATE
	theCDF -> WriteGlobalAttr, 'TEXT',                       'Averaged magnetic field is ' + $
	                           'created by interpolating high resolution DFG magnetometer data ' + $
	                           'onto the beam times of EDI within a selected window, then doing ' + $
	                           'averaging over each component. Time tags are at the beginning ' + $
	                           'of the averaging interval. Instrument papers can be found at ' + $
	                           'the following links: ' + $
	                           'FIELDS: http://link.springer.com/10.1007/s11214-014-0109-8  ' + $
	                           'DFG: ' + $
	                           'EDI: ', /CREATE
	theCDF -> WriteGlobalAttr, 'HTTP_LINK',                  ['http://mms-fields.unh.edu/', $
	                                                          'http://mms.gsfc.nasa.gov/index.html'], /CREATE
	theCDF -> WriteGlobalAttr, 'LINK_TEXT',                  ['UNH FIELDS Home Page', $
	                                                          'NASA MMS Home'],                      /CREATE
	theCDF -> WriteGlobalAttr, 'MODS',                       version,                                /CREATE
	theCDF -> WriteGlobalAttr, 'Acknowledgements',           ' ',                                    /CREATE
	theCDF -> WriteGlobalAttr, 'Generated_by',               ' ',                                    /CREATE
	theCDF -> WriteGlobalAttr, 'Parents',                    ['CDF>' + file_basename(dfg_file), $
	                                                          'CDF>' + file_basename(edi_file)],     /CREATE
	theCDF -> WriteGlobalAttr, 'Skeleton_version',           ' ',                                    /CREATE
	theCDF -> WriteGlobalAttr, 'Rules_of_use',               ' ',                                    /CREATE
	theCDF -> WriteGlobalAttr, 'Time_resolution',            ' ',                                    /CREATE

;-------------------------------------------------------
; Variables ////////////////////////////////////////////
;-------------------------------------------------------
	;Names
	t_epoch_name    = 'Epoch'
	b_avg_name      = mms_construct_varname(sc, inst, 'b',    DESCRIPTOR='beam_avg')
	b_beam1_name    = mms_construct_varname(sc, inst, 'b',    DESCRIPTOR='beam1')
	b_beam2_name    = mms_construct_varname(sc, inst, 'b',    DESCRIPTOR='beam2')
	inds_beam1_name = mms_construct_varname(sc, inst, 'inds', DESCRIPTOR='edi_beam1')
	inds_beam2_name = mms_construct_varname(sc, inst, 'inds', DESCRIPTOR='edi_beam2')
	b_labl_name     = mms_construct_varname(sc, inst, 'b',    DESCRIPTOR='labl_ptr')

	;Create the variables and write their data
	theCDF -> WriteVar, t_epoch_name,    t_avg,           /CREATE
	theCDF -> WriteVar, b_avg_name,      b_avg,           /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
	theCDF -> WriteVar, b_beam1_name,    b_beam1,         /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
	theCDF -> WriteVar, b_beam2_name,    b_beam1,         /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
	theCDF -> WriteVar, inds_beam1_name, inds_edi_beam1,  /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
	theCDF -> WriteVar, inds_beam2_name, inds_edi_beam2,  /CREATE, COMPRESSION='GZIP', GZIP_LEVEL=6
	theCDF -> WriteVar, b_labl_name,     ['X', 'Y', 'Z'], /CREATE

;-------------------------------------------------------
; Variable Attributes //////////////////////////////////
;-------------------------------------------------------
	;
	;Epoch
	;
	theCDF -> WriteVarAttr, t_epoch_name, 'CATDESC',       'Time variable', /CREATE
	theCDF -> WriteVarAttr, t_epoch_name, 'FIELDNAM',      'Time',          /CREATE
	theCDF -> WriteVarAttr, t_epoch_name, 'FILLVAL',       -1.0E31,         /CREATE
	theCDF -> WriteVarAttr, t_epoch_name, 'FORMAT',        'I16',           /CREATE
	theCDF -> WriteVarAttr, t_epoch_name, 'LABLAXIS',      'UT',            /CREATE
	theCDF -> WriteVarAttr, t_epoch_name, 'SI_CONVERSION', '1E-9>seconds',  /CREATE
	theCDF -> WriteVarAttr, t_epoch_name, 'UNITS',         'ns',            /CREATE
	theCDF -> WriteVarAttr, t_epoch_name, 'VALIDMIN',      datetime_to_epoch('2015-01-01T00:00:00.000000000Z', /TO_TT2000), /CREATE
	theCDF -> WriteVarAttr, t_epoch_name, 'VALIDMAX',      datetime_to_epoch('2050-01-01T00:00:00.000000000Z', /TO_TT2000), /CREATE
	theCDF -> WriteVarAttr, t_epoch_name, 'VARTYPE',       'support_data',  /CREATE
	
	;
	;DFG Average magnetic field
	;
	theCDF -> WriteVarAttr, b_avg_name, 'CATDESC',       'Averaged magnetic field.'
	theCDF -> WriteVarAttr, b_avg_name, 'DEPEND_0',      t_epoch_name,     /CREATE
	theCDF -> WriteVarAttr, b_avg_name, 'DISPLAY_TYPE',  'time_series',    /CREATE
	theCDF -> WriteVarAttr, b_avg_name, 'FIELDNAM',      'Magnetic Field'
	theCDF -> WriteVarAttr, b_avg_name, 'FILLVAL',       -1.0E31
	theCDF -> WriteVarAttr, b_avg_name, 'FORMAT',        'F12.6'
	theCDF -> WriteVarAttr, b_avg_name, 'LABL_PTR_1',    b_labl_name,      /CREATE
	theCDF -> WriteVarAttr, b_avg_name, 'SI_CONVERSION', '1e-9>Tesla'
	theCDF -> WriteVarAttr, b_avg_name, 'UNITS',         'nT'
	theCDF -> WriteVarAttr, b_avg_name, 'VALIDMIN',      -100000.0
	theCDF -> WriteVarAttr, b_avg_name, 'VALIDMAX',       100000.0
	theCDF -> WriteVarAttr, b_avg_name, 'VARTYPE',       'data'
	
	;
	;DFG Magnetic Field interpolated to Beam1 times
	;
	theCDF -> WriteVarAttr, b_beam1_name, 'CATDESC',       'Magnetic field interpolated to EDI beam 1 times.'
	theCDF -> WriteVarAttr, b_beam1_name, 'DEPEND_0',      t_epoch_name,     /CREATE
	theCDF -> WriteVarAttr, b_beam1_name, 'DISPLAY_TYPE',  'time_series',    /CREATE
	theCDF -> WriteVarAttr, b_beam1_name, 'FIELDNAM',      'Magnetic Field'
	theCDF -> WriteVarAttr, b_beam1_name, 'FILLVAL',       -1.0E31
	theCDF -> WriteVarAttr, b_beam1_name, 'FORMAT',        'F12.6'
	theCDF -> WriteVarAttr, b_beam1_name, 'LABL_PTR_1',    b_labl_name,      /CREATE
	theCDF -> WriteVarAttr, b_beam1_name, 'SI_CONVERSION', '1e-9>Tesla'
	theCDF -> WriteVarAttr, b_beam1_name, 'UNITS',         'nT'
	theCDF -> WriteVarAttr, b_beam1_name, 'VALIDMIN',      -100000.0
	theCDF -> WriteVarAttr, b_beam1_name, 'VALIDMAX',       100000.0
	theCDF -> WriteVarAttr, b_beam1_name, 'VARTYPE',       'data'
	
	;
	;DFG Magnetic Field interpolated to Beam2 times
	;
	theCDF -> WriteVarAttr, b_beam2_name, 'CATDESC',       'Magnetic field interpolated to EDI beam 2 times.'
	theCDF -> WriteVarAttr, b_beam2_name, 'DEPEND_0',      t_epoch_name,     /CREATE
	theCDF -> WriteVarAttr, b_beam2_name, 'DISPLAY_TYPE',  'time_series',    /CREATE
	theCDF -> WriteVarAttr, b_beam2_name, 'FIELDNAM',      'Magnetic Field'
	theCDF -> WriteVarAttr, b_beam2_name, 'FILLVAL',       -1.0E31
	theCDF -> WriteVarAttr, b_beam2_name, 'FORMAT',        'F12.6'
	theCDF -> WriteVarAttr, b_beam2_name, 'LABL_PTR_1',    b_labl_name,      /CREATE
	theCDF -> WriteVarAttr, b_beam2_name, 'SI_CONVERSION', '1e-9>Tesla'
	theCDF -> WriteVarAttr, b_beam2_name, 'UNITS',         'nT'
	theCDF -> WriteVarAttr, b_beam2_name, 'VALIDMIN',      -100000.0
	theCDF -> WriteVarAttr, b_beam2_name, 'VALIDMAX',       100000.0
	theCDF -> WriteVarAttr, b_beam2_name, 'VARTYPE',       'data'
	
	;
	;EDI Indices for Beam 1
	;
	theCDF -> WriteVarAttr, inds_beam1_name, 'CATDESC',  'A map between the EDI beam1 hits ' + $
	                                                     'onto which the DFG magnetic field were ' + $
	                                                     'interpolated, and the average magnetic ' + $
	                                                     'field that resulted from the interopolation. ' + $
	                                                     'INDS_EDI_BEAM1[0] returns the index into ' + $
	                                                     'B_AVG for which the beam at index 0 was used.'
	theCDF -> WriteVarAttr, inds_beam1_name, 'FIELDNAM', 'EDI Beam 1 Index'
	theCDF -> WriteVarAttr, inds_beam1_name, 'FILLVAL',  -1.0E31
	theCDF -> WriteVarAttr, inds_beam1_name, 'VALIDMIN', 0L
	theCDF -> WriteVarAttr, inds_beam1_name, 'VALIDMAX', 2147483647L
	theCDF -> WriteVarAttr, inds_beam1_name, 'VARTYPE',  'metadata'
	
	;
	;EDI Indices for Beam 2
	;
	theCDF -> WriteVarAttr, inds_beam2_name, 'CATDESC',  'A map between the EDI beam2 hits ' + $
	                                                     'onto which the DFG magnetic field were ' + $
	                                                     'interpolated, and the average magnetic ' + $
	                                                     'field that resulted from the interopolation. ' + $
	                                                     'INDS_EDI_BEAM2[0] returns the index into ' + $
	                                                     'B_AVG for which the beam at index 0 was used.'
	theCDF -> WriteVarAttr, inds_beam2_name, 'FIELDNAM', 'EDI Beam 2 Index'
	theCDF -> WriteVarAttr, inds_beam2_name, 'FILLVAL',  -1.0E31
	theCDF -> WriteVarAttr, inds_beam2_name, 'VALIDMIN', 0L
	theCDF -> WriteVarAttr, inds_beam2_name, 'VALIDMAX', 2147483647L
	theCDF -> WriteVarAttr, inds_beam2_name, 'VARTYPE',  'metadata'
	
	;
	;B Labl Pointers
	;
	theCDF -> WriteVarAttr, b_labl_name, 'CATDESC',  'Labels for ' + b_avg_name
	theCDF -> WriteVarAttr, b_labl_name, 'FIELDNAM', b_labl_name
	theCDF -> WriteVarAttr, b_labl_name, 'FORMAT',   'A1'
	theCDF -> WriteVarAttr, b_labl_name, 'VARTYPE',  'metadata'
	
	;Close the file
	theCDF -> Close
	obj_destroy, theCDF
	
	;Status messages
	print, 'File written to "' + outfile + '"'
	return, outfile
end


;+
;   Run a test case.
;
; :Params:
;       EXAMPLE:            in, optional, type=integer, default=1
;                           Number of the example to run
;-
function mms_edi_bavg_test, example
	compile_opt idl2
	on_error, 2
	
	;Run the first example
	if n_elements(example) eq 0 then example = 1
	
	case example of
	;-----------------------------------------------------
	; Test Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		1: begin
			;EXAMPLE 2
			;   - Test using MMS_EDI_Test_Data
			test_data   = mms_edi_test_data()

			;Despin data
			offset      = mms_instr_origins_instr('DFG_123', 'DSS')
			offset      = offset[0]
			test_data.B = mms_dss_despin(test_data.srt, test_data.b_time, test_data.B, $
			                             OMEGA=test_data.spin_freq)

			;Rotate B into OCS
			dfg2ocs     = mms_instr_xxyz2ocs('DFG_123', ROTZ=dfg_angle2ocs)
			test_data.B = rotate_vector(dfg2ocs, test_data.B)

			;View results
			!Null       = mms_edi_bavg(TEST_DATA=test_data, /VIEW, WINDOW=win)
		endcase
	;-----------------------------------------------------
	; Cluster Example Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		2: begin
			root     = '/Users/argall/Documents/IDL/MMS/test_data/'
			dfg_file = filepath('mms3_dfg_brst_l2pre_duration-1h1m_20010704_v1.0.0.cdf', ROOT_DIR=root)
			edi_file = filepath('mms3_edi_fast_l1a_beam_20010704_v0.2.0.cdf',            ROOT_DIR=root)
			!Null    = mms_edi_bavg(dfg_file, edi_file, /VIEW, WINDOW=win)
		endcase
	;-----------------------------------------------------
	; Cluster Example Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		else: message, 'Invalid example. Choose a number 1-2.'
	endcase
	
	return, win
end


;+
;   Display the results of averaging.
;
; :Params:
;       T_AVG:              in, required, type=lon64arr
;                           Time stamps in CDF_TIME_TT2000 format of `B_AVG`.
;       B_AVG:              in, required, type=lon64arr(3\,N)
;                           Averaged DFG magnetic field data.
;       T_DFG:              in, required, type=lon64arr
;                           Time stamps in CDF_TIME_TT2000 format of `B_DFG`
;       B_DFG:              in, required, type=lon64arr(3\,N)
;                           DFG magnetic field data.
;-
function mms_edi_bavg_view, t_avg, b_avg, b_stdev, t_dfg, b_dfg
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		void = cgErrorMsg()
		return, obj_new()
	endif

	;Convert time to seconds
	t_min   = t_dfg[0] < t_avg[0]
	t_mean  = (t_avg - t_min) * 1d-9
	t_mag   = (t_dfg - t_min) * 1d-9

	;Create a window
	win = Window(DIMENSIONS=[800,500])
	win -> Refresh, /DISABLE

	;YRange
	ymin      = min(b_dfg, DIMENSION=2, MAX=ymax)
	margin    = [0.17, 0.27, 0.08, 0.15]
	font_size = 12

	;Plot the average magnetic field
	px_dfg = Plot(t_mag, b_dfg[0,*], /CURRENT, $
	              COLOR       = 'Grey', $
	              FONT_SIZE   = font_size, $
	              LAYOUT      = [1,3,1], $
	              MARGIN      = margin, $
	              NAME        = 'Bx', $
	              TITLE       = 'Beam-Averaged Magnetic Field', $
	              XTICKFORMAT = '(a1)', $
	              YRANGE      = [ymin[0] * (ymin[0] gt 0 ? 0.9 : 1.1), ymax[0] * (ymax[0] gt 0 ? 1.1 : 0.8)], $
	              YSTYLE      = 1, $
	              YTITLE      = 'Bx!C(nT)')
	
	py_dfg = Plot(t_mag, b_dfg[1,*], /CURRENT, $
	              COLOR       = 'Grey', $
	              FONT_SIZE   = font_size, $
	              LAYOUT      = [1,3,2], $
	              MARGIN      = margin, $
	              NAME        = 'By', $
	              XTICKFORMAT = '(a1)', $
	              YRANGE      = [ymin[1] * (ymin[1] gt 0 ? 0.9 : 1.1), ymax[1] * (ymax[1] gt 0 ? 1.1 : 0.8)], $
	              YSTYLE      = 1, $
	              YTITLE      = 'By!C(nT)')
	
	pz_dfg = Plot(t_mag, b_dfg[2,*], /CURRENT, $
	              COLOR       = 'Grey', $
	              FONT_SIZE   = font_size, $
	              LAYOUT      = [1,3,3], $
	              MARGIN      = margin, $
	              NAME        = 'Bz', $
	              XTITLE      = 'Time (s)', $
	              YRANGE      = [ymin[2] * (ymin[2] gt 0 ? 0.9 : 1.1), ymax[2] * (ymax[2] gt 0 ? 1.1 : 0.8)], $
	              YSTYLE      = 1, $
	              YTITLE      = 'Bz!C(nT)')

	;Plot the measured magnetic field
	opx_avg = ErrorPlot(t_mean, b_avg[0,*], b_stdev[0,*], OVERPLOT=px_dfg, NAME='Bx Avg', SYMBOL='Diamond', /CURRENT)
	opy_avg = ErrorPlot(t_mean, b_avg[1,*], b_stdev[1,*], OVERPLOT=py_dfg, NAME='By Avg', SYMBOL='Diamond', /CURRENT)
	opz_avg = ErrorPlot(t_mean, b_avg[2,*], b_stdev[2,*], OVERPLOT=pz_dfg, NAME='Bz Avg', SYMBOL='Diamond', /CURRENT)
	
	;Create a legend
	leg_b = Legend(HORIZONTAL_ALIGNMENT = 'RIGHT', $
	               LABEL                = 'Measured', $
	               NAME                 = 'Averaged Data Legend', $
	               POSITION             = [0.9,0.95], $
	               /NORMAL, $
;	               /RELATIVE, $
;	               SAMPLE_COLOR         = 'Grey', $
	               TARGET               = px_dfg, $
	               TEXT_COLOR           = 'Grey', $
	               VERTICAL_ALIGNMENT   = 'TOP')
	leg_b -> Add, opx_avg, LABEL='Average', TEXT_COLOR='Black', SAMPLE_COLOR='Black'
	
	;Refresh the window and return
	win -> Refresh
	return, win
end


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
; :Params:
;       DFG_FILE:           in, required, type=string
;                           Filename of the DFG magnetometer data to average.
;       EDI_FILE:           in, required, type=string
;                           Filename of the EDI beam data.
;
; :Keywords:
;       DIRECTORY:          in, optional, type=string
;                           If given, the averaged data will be writen to a file in this
;                               directory.
;       T_AVG:              in, optional, type=lon64arr
;                           Time stamps in CDF_TT2000 format
;       TEST_DATA:          in, optional, type=lon64arr
;                           EDI test data. If provided, `DFG_FILE` and `EDI_FILE` are ignored.
;       VIEW:               in, optional, type=boolean, default=0
;                           If set, a graphic of the averaged magnetic field data will made.
;       WINDOW:             out, optional, type=object
;                           If `VIEW`=1, then set this keyword to a named variable to
;                               hold the MrWindow object reference.
;-
function mms_edi_bavg, dfg_file, edi_file, $
DIRECTORY=directory, $
T_AVG=t_avg, $
TEST_DATA=test_data, $
VIEW=view, $
WINDOW=win
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(oCDF) then obj_destroy, oCDf
		void = cgErrorMsg()
		return, -1
	endif
;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	test = keyword_set(test_data)
	view = keyword_set(view)
	if n_elements(directory) eq 0 then directory = ''
	
;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if test eq 0 then begin
		;Read the DFG magnetic field data
		oCDF  = MrCDF_File(dfg_file)
		b_dfg = oCDF -> Read('B_xyz_scs_sc3', DEPEND_0=t_dfg)
		obj_destroy, oCDF

		;Read the SRT data
		oCDF          = MrCDF_File(edi_file)
		t_edi         = oCDF -> Read('Epoch')
		t_edi_beam1   = oCDF -> Read('Epoch_beam1')
		t_edi_beam2   = oCDF -> Read('Epoch_beam2')
		obj_destroy, oCDF
	endif else begin
		;Grab the test data
		if size(test_data, /TNAME) ne 'STRUCT' then test_data = mms_edi_test_data()
		b_dfg_123   = test_data.B_123
		t_dfg       = test_data.B_time
		t_edi       = test_data.edi_time
		t_edi_beam1 = test_data.edi_time
		t_edi_beam2 = test_data.edi_time
		
		;Rotate magnetometer data into OCS and despin
		xyz2ocs   = mms_instr_xxyz2ocs('DFG_123')
		b_dfg_ocs = rotate_vector(xyz2ocs, temporary(b_dfg_123))
		b_dfg     = mms_dss_despin(test_data.srt, t_dfg, temporary(b_dfg_ocs), OMEGA=test_data.spin_freq)
	endelse
	
;-----------------------------------------------------
; Prepare Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Find initial time
	;   - Convert to UT
	;   - Round down to the nearest 5-second interval 
	;   - Convert back to CDF_TIME_TT2000
	t0 = t_dfg[0] < t_edi[0] < t_edi_beam1[0] < t_edi_beam2[0]
	MrCDF_Epoch, temporary(t0), yr, mo, day, hr, mn, sec, /BREAKDOWN_EPOCH
	sec -= sec mod 5
	MrCDF_Epoch, t0, yr, mo, day, hr, mn, sec, /TT2000, /COMPUTE_EPOCH
	
	;SPLINE accepts only floats or doubles
	;   - Subtract the initial time from all points and convert from nono-seconds to seconds
	t_dfg       = double(t_dfg       - t0) * 1d-9
	t_edi       = double(t_edi       - t0) * 1d-9
	t_edi_beam1 = double(t_edi_beam1 - t0) * 1d-9
	t_edi_beam2 = double(t_edi_beam2 - t0) * 1d-9
	t_stop      = t_dfg[-1] < t_edi[-1] < t_edi_beam1[-1] < t_edi_beam2[-1]

	;Number of elements in each array
	n_dfg   = n_elements(t_dfg)
	n_beam1 = n_elements(t_edi_beam1)
	n_beam2 = n_elements(t_edi_beam2)
	nmax    = n_dfg < n_beam1 < n_beam2
	
	;Allocate memory to output arrays
	t_avg          = lon64arr(nmax)
	b_avg          = fltarr(3, nmax)
	b_stdev        = fltarr(3, nmax)
	b_beam1        = make_array(3, n_beam1, VALUE=-1e31, /FLOAT)
	b_beam2        = make_array(3, n_beam2, VALUE=-1e31, /FLOAT)
	inds_edi_beam1 = lonarr(n_beam1)
	inds_edi_beam2 = lonarr(n_beam2)

	;Initial conditions
	;   - T_START:        Start at the first magnetometer data point.
	;   - DT:             Average over this many seconds.
	;   - DT_INTERP:      Number of seconds on either side of the averaging interval that are interpolated
	;   - TRANGE_AVG:     First time interval to be averaged.
	;   - TRANGE_INTEPR:  First time interval to be interpolated.
	;   - N_BEAM[12]_TOT: Total number of beams included throughout the averaging process.
	t_start       = t_dfg[0]
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
		it_dfg_interp   = MrIndexRange(t_dfg,       trange_interp, /RIGHT_EXCLUSIVE, STATUS=status_dfg)
		it_beam1_interp = MrIndexRange(t_edi_beam1, trange_interp, /RIGHT_EXCLUSIVE, STATUS=status_beam1)
		it_beam2_interp = MrIndexRange(t_edi_beam2, trange_interp, /RIGHT_EXCLUSIVE, STATUS=status_beam2)
		
		;Error?
		if status_dfg + status_beam1 + status_beam2 gt 0 then begin
			message, string(FORMAT='(%"No beams found in interval %f-%fs")', trange_avg[0], trange_avg[1]), /INFORMATIONAL

			;Move to next interval
			trange_avg    += dt
			trange_interp += dt
		endif
		
		;Extract the time interval for ease
		b_dfg_temp   = b_dfg[*, it_dfg_interp[0]:it_dfg_interp[1]]
		t_dfg_temp   = t_dfg[it_dfg_interp[0]:it_dfg_interp[1]]
		t_beam1_temp = t_edi_beam1[it_beam1_interp[0]:it_beam1_interp[1]]
		t_beam2_temp = t_edi_beam2[it_beam2_interp[0]:it_beam2_interp[1]]

	;-----------------------------------------------------
	; Interpolate \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Allocate memory to temporary output array
		b_dfg_beam1 = fltarr(3, n_elements(t_beam1_temp))
		b_dfg_beam2 = fltarr(3, n_elements(t_beam2_temp))

		;Interpolate (x, y, z) components
		;   - sigma ~ 0 => cubic spline
		b_dfg_beam1[0,*] = spline(t_dfg_temp, b_dfg_temp[0,*], t_beam1_temp, 0.001)
		b_dfg_beam1[1,*] = spline(t_dfg_temp, b_dfg_temp[1,*], t_beam1_temp, 0.001)
		b_dfg_beam1[2,*] = spline(t_dfg_temp, b_dfg_temp[2,*], t_beam1_temp, 0.001)
		b_dfg_beam2[0,*] = spline(t_dfg_temp, b_dfg_temp[0,*], t_beam2_temp, 0.001)
		b_dfg_beam2[1,*] = spline(t_dfg_temp, b_dfg_temp[1,*], t_beam2_temp, 0.001)
		b_dfg_beam2[2,*] = spline(t_dfg_temp, (temporary(b_dfg_temp))[2,*], t_beam2_temp, 0.001)

	;-----------------------------------------------------
	; Find Averaging Interval & Average \\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Get the beams within the averaging interval
		;  --|-----{-----}-----|--
		;    |  Interpolation  |
		;          { Avg }
		it_beam1_avg = MrIndexRange(temporary(t_beam1_temp), trange_avg, /RIGHT_EXCLUSIVE)
		it_beam2_avg = MrIndexRange(temporary(t_beam2_temp), trange_avg, /RIGHT_EXCLUSIVE)
		b_dfg_beam1  = b_dfg_beam1[*, it_beam1_avg[0]:it_beam1_avg[1]]
		b_dfg_beam2  = b_dfg_beam2[*, it_beam2_avg[0]:it_beam2_avg[1]]

		;Take the averge
		;   - Convert time back to nanoseconds.
		!Null            = moment([[b_dfg_beam1], [b_dfg_beam2]], DIMENSION=2, $
		                          MEAN=temp_mean, SDEV=temp_sdev, MAXMOMENT=2)
		b_avg[*,count]   = temporary(temp_mean)
		b_stdev[*,count] = temporary(temp_sdev)
		t_avg[count]     = long64(trange_avg[0] * 1d9) + t0

		;Number of beams in the average
		n_beam1_avg = it_beam1_avg[1] - it_beam1_avg[0] + 1
		n_beam2_avg = it_beam2_avg[1] - it_beam2_avg[0] + 1
		
		;Record the index of B associated with each beam.
		inds_edi_beam1[n_beam1_tot:n_beam1_tot+n_beam1_avg-1] = count
		inds_edi_beam2[n_beam2_tot:n_beam2_tot+n_beam2_avg-1] = count

	;-----------------------------------------------------
	; Store the Interpolated Data \\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Pick out appropriate beam information
		b_beam1[*, n_beam1_tot:n_beam1_tot+n_beam1_avg-1] = b_dfg_beam1
		b_beam2[*, n_beam2_tot:n_beam2_tot+n_beam2_avg-1] = b_dfg_beam2

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
	
	;Trim data
	t_avg   = t_avg[0:count-1]
	b_avg   = b_avg[*, 0:count-1]
	b_stdev = b_stdev[*, 0:count-1]
	
	;Convert DFG time back to TT2000
	t_dfg = long64(t_dfg * 1D9) + t0

	;View the data?
	if view then win = mms_edi_bavg_view(t_avg, b_avg, b_stdev, t_dfg, b_dfg)
	
	;Write the data to a file?
	if directory ne '' then begin
		fname = mms_edi_bavg_file(directory, t_avg, b_avg, b_stdev, $
		                          inds_edi_beam1, inds_edi_beam2, $
		                          b_beam1, b_beam2, $
		                          dfg_file, edi_file)
		return, fname
	endif else if n_elements(test_data) gt 0 then begin
		test_data = create_struct(test_data, $
		                          't_avg',          t_avg, $
		                          'b_avg',          b_avg, $
		                          'b_stdev',        b_stdev, $
		                          'b_beam1',        b_beam1, $
		                          'b_beam2',        b_beam2, $
		                          'edi1_beam_inds', inds_edi_beam1, $
		                          'edi2_beam_inds', inds_edi_beam2 $
		                         )
		return, test_data
	endif else begin
		return, b_avg
	endelse
end


;-----------------------------------------------------
; Main Level Example Program: IDL> mms_edi_bavg \\\\\\
;-----------------------------------------------------

;EXAMPLE 1
;   - Test using Cluster-converted-to-MMS data
root     = '/Users/argall/Documents/IDL/MMS/test_data/'
dfg_file = filepath('mms3_dfg_brst_l2pre_duration-1h1m_20010704_v1.0.0.cdf', ROOT_DIR=root)
edi_file = filepath('mms3_edi_fast_l1a_beam_20010704_v0.2.0.cdf',            ROOT_DIR=root)
!Null    = mms_edi_bavg(dfg_file, edi_file, /VIEW, WINDOW=win)

end