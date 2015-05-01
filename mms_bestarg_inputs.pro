; docformat = 'rst'
;
; NAME:
;       MMS_Bestarg_Inputs
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
;   Process magnetometer, digital sun sensor, and electron drift instrument data and
;   prepare it for input into Bestarg.
;
; :Categories:
;   MMS, EDI
;
; :Examples:
;   Create data from test case 1 and view it::
;       IDL> data = MMS_Bestarg_Inputs(TEST_CASE=1, /VIEW)
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
;       2015/03/06  -   Read/create test data, then pass it to helper programs. Added
;                           MMS_Bestarg_Inputs_View and MMS_Bestarg_Inputs_File procedures. - MRA
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
;       T_ERR:              in, required, type=fltarr(2)
;                           Uniform upper and lower limits of each `T_AVG`.
;       B_AVG:              in, required, type=3xN fltarr
;                           Average magnetic field data.
;       B_STDEV:            in, required, type=3xN float
;                           Standard deviation of `B_AVG`.
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
function mms_bestarg_inputs_file, directory, t_avg, t_err, b_avg, b_stdev, $
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
;   View the results of rotating beams into their bpp.
;
; :Params:
;       DATA_STRUCT:        in, required, type=structure
;                           Results of the averaging and BPP-finding process.
;
; :Keywords:
;       DOCS:               in, optional, type=boolean, default=0
;                           If set, data is given in despun OCS instead of BPP.
;-
function mms_bestarg_inputs_view, t_dfg, b_dfg, t_avg, b_avg, t_err, b_stdev, $
                                  gun1_pos, gun2_pos, gun1_fire, gun2_fire, $
                                  edi1_beam_inds, edi2_beam_inds, drift_step, $
DOCS=docs, $
FILENAME=filename
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		void = cgErrorMsg()
		return, obj_new()
	endif

	;Save
	docs     = keyword_set(docs)
	tf_drift = n_elements(drift_step) gt 0
	if n_elements(filename) eq 0 then filename = ''

;-----------------------------------------------------
; Create Window \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Dimensions of 2:1 window
	wdims   = get_screen_size()
	waspect = wdims[0] / wdims[1]
	winy    = 500
	winx    = 2 * fix(winy * waspect)
	
	;Center it on the screen
	center = wdims / 2
	corner = center + [-winx/2, -winy/2]
	
	;Create window
	win    = window(DIMENSIONS=[winx, winy], LOCATION=corner)
	win   -> Refresh, /DISABLE

;-----------------------------------------------------
; Plot Bx, By, and Bz \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert time to seconds
	t_min   = t_dfg[0] < t_avg[0]
	t_mean  = (t_avg - t_min) * 1d-9
	t_mag   = (t_dfg - t_min) * 1d-9

	;YRange
	ymin      = min(b_dfg, DIMENSION=2, MAX=ymax)
	margin    = [0.17, 0.27, 0.08, 0.15]
	font_size = 12

	;Plot the average magnetic field
	px_dfg = Plot(t_mag, b_dfg[0,*], /CURRENT, $
	              COLOR       = 'Grey', $
	              FONT_SIZE   = font_size, $
	              MARGIN      = margin, $
	              NAME        = 'Bx', $
	              POSITION    = [0.1, 0.64, 0.45, 0.93], $
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
	              POSITION    = [0.1, 0.37, 0.45, 0.64], $
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
	              POSITION    = [0.1, 0.1, 0.45, 0.37], $
	              XTITLE      = 'Time (s)', $
	              YRANGE      = [ymin[2] * (ymin[2] gt 0 ? 0.9 : 1.1), ymax[2] * (ymax[2] gt 0 ? 1.1 : 0.8)], $
	              YSTYLE      = 1, $
	              YTITLE      = 'Bz!C(nT)')

;-----------------------------------------------------
; Plot B_AVG with Error Bars \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Make T_ERR the proper size
	t_error = rebin(t_err, 2, n_elements(t_mean))

	;Plot the measured magnetic field
	opx_avg = ErrorPlot(t_mean, b_avg[0,*], t_error, b_stdev[0,*], /CURRENT, $
	                    OVERPLOT=px_dfg, NAME='Bx Avg', SYMBOL='Diamond', $
	                    ERRORBAR_COLOR='Red')
	opy_avg = ErrorPlot(t_mean, b_avg[1,*], t_error, b_stdev[1,*], /CURRENT, $
	                    OVERPLOT=py_dfg, NAME='By Avg', SYMBOL='Diamond', $
	                    ERRORBAR_COLOR='Red')
	opz_avg = ErrorPlot(t_mean, b_avg[2,*], t_error, b_stdev[2,*], /CURRENT, $
	                    OVERPLOT=pz_dfg, NAME='Bz Avg', SYMBOL='Diamond', $
	                    ERRORBAR_COLOR='Red')

	;Create a legend
	leg_b = Legend(HORIZONTAL_ALIGNMENT = 'RIGHT', $
	               LABEL                = 'Average', $
	               NAME                 = 'Averaged Data Legend', $
	               POSITION             = [0.53, 0.97], $
	               /NORMAL, $
	               TARGET               = opx_avg, $
	               TEXT_COLOR           = 'Black', $
	               VERTICAL_ALIGNMENT   = 'TOP')
	leg_b -> Add, px_dfg, LABEL='Measured', TEXT_COLOR='Grey'

;-----------------------------------------------------
; Create S/C \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Spacecraft outline
	;   - Create a circle with a radius that extends from optics to gun.
	nVerts           = 200
	radius           = mms_instr_origins_instr('EDI1_GUN', 'EDI2_DETECTOR')
	sc_sphr_ocs      = fltarr(3, nVerts)
	sc_sphr_ocs[2,*] = sqrt(total(radius^2))
	sc_sphr_ocs[0,*] = 2.0 * !pi * findgen(nVerts)/(nVerts-1.0)
	sc_xyz_ocs       = cv_coord(FROM_SPHERE=sc_sphr_ocs, /TO_RECT)

;-----------------------------------------------------
; Associate Beams with B_avg \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Figure out which beams correspond to which average value
	hist1 = histogram(edi1_beam_inds, MIN=0, BINSIZE=1, REVERSE_INDICES=ri1)
	hist2 = histogram(edi2_beam_inds, MIN=0, BINSIZE=1, REVERSE_INDICES=ri2)

;-----------------------------------------------------
; Draw S/C, Guns, Fire Vectors, & Target \\\\\\\\\\\\\
;-----------------------------------------------------
	;Draw a set of axes 1.5 times bigger than the s/c
	range = 1.25 * [-sc_sphr_ocs[2,0], sc_sphr_ocs[2,0]]
	gAxes = plot(range, range, /NODATA, /CURRENT, $
	             ASPECT_RATIO = 1.0, $
	             MARGIN       = [0.5, 0.1, 0.05, 0.1], $
	             TITLE        = 'Beam intersections in ' + (docs ? '$B_{Avg}$ BPP' : '$B_{Interp}$ BPP'), $
	             XRANGE       = range, $
	             XSTYLE       = 1, $
	             XTITLE       = 'Distance (m)', $
	             YRANGE       = range, $
	             YSTYLE       = 1, $
	             YTITLE       = 'Distance (m)')
	
	;Draw the s/c
	gSC   = polygon(reform(sc_xyz_ocs[0,*]), reform(sc_xyz_ocs[1,*]), /DATA, TARGET=gAxes)
	
	;Firing directions
	gFire1 = Polyline(range, range, COLOR='Blue', /DATA, TARGET=gAxes)
	gFire2 = Polyline(range, range, COLOR='Red',  /DATA, TARGET=gAxes)
		
	;Draw drift step
	if tf_drift then begin
		gTarget = symbol(drift_step[0], drift_step[1], 'X', /DATA, $
		                 TARGET    = gAxes, $
		                 SYM_THICK = 2, $
		                 SYM_SIZE  = 2.0)
	endif

;-----------------------------------------------------
; Draw Each Bavg Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	nPts = n_elements(hist1)
	for i = 0, nPts - 2 do begin
		;Number of beams used to compute B_avg
		n1 = ri1[ri1[i+1]] - ri1[ri1[i]]
		n2 = ri2[ri2[i+1]] - ri2[ri1[i]]
		
		;Are there beams associated with this time
		if n1 eq 0 && n2 eq 0 then continue
		
	;-----------------------------------------------------
	; Find Beams \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Get the beam indices
		inds1 = ri1[ri1[i]:ri1[i+1]-1]
		inds2 = ri2[ri2[i]:ri2[i+1]-1]
		
		;Gun positions
		g1_pos = gun1_pos[*,inds1]
		g2_pos = gun2_pos[*,inds2]

		;Gun firing directions
		g1_fire = gun1_fire[*,inds1]
		g2_fire = gun2_fire[*,inds2]
		
		;Drift step
		if tf_drift then d = drift_step[*,inds1]
		
	;-----------------------------------------------------
	; Rotate into B_avg BPP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Rotate the s/c into B_AVG bpp
		xyz2bpp    = mms_instr_xb2bpp(b_avg[*,i])
		sc_xyz_bpp = rotate_vector(xyz2bpp, sc_xyz_ocs)
		
		;Rotate the positions and firing directions into average BPP
		if docs then begin
			g1_pos  = rotate_vector(xyz2bpp, g1_pos)
			g2_pos  = rotate_vector(xyz2bpp, g2_pos)
			g1_fire = rotate_vector(xyz2bpp, g1_fire)
			g2_fire = rotate_vector(xyz2bpp, g2_fire)
			if tf_drift then d = rotate_vector(xyz2bpp, d)
		endif
		
	;-----------------------------------------------------
	; Two Points to Define the Beams \\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Beam slope, y-intercept, (x1,x2) and (y1,y2)
		;   - slope (m)       = rise / run
		;   - y-intercept (b) = y1 - m * x1
		;   - (x1,x2)         = range
		;   - (y1,y2)         = m*x + b
		m   = reform( g1_fire[1,*] / g1_fire[0,*] )
		b   = reform( g1_pos[1,*] - g1_pos[0,*] * m )
		g1x = rebin( [range[0], range[1]], 2, n1 )
		g1y = transpose( [[m * g1x[0,*] + b], [m * g1x[1,*] + b]] )
		
		;Beam slope, y-intercept, (x1,x2) and (y1,y2)
		m   = reform( g2_fire[1,*] / g2_fire[0,*] )
		b   = reform( g2_pos[1,*] - g2_pos[0,*] * m )
		g2x = rebin( [range[0], range[1]], 2, n2)
		g2y = transpose( [[m * g2x[0,*] + b], [m * g2x[1,*] + b]] )
		
		;Define connectivity
		;   - Make (x1,x2) and (y1,y2) pairs adjacent
		;   - Indicate connectivity: [2,       2,       2,
		;                                0, 1,    2, 3,    4, 5, ...]
		g1x = reform(g1x, 2 * n1)
		g1y = reform(g1y, 2 * n1)
		g2x = reform(g2x, 2 * n2)
		g2y = reform(g2y, 2 * n2)
		conn1 = reform([replicate(2,1,n1), lindgen(2,n1)], 3*n1)
		conn2 = reform([replicate(2,1,n2), lindgen(2,n2)], 3*n2)
		
	;-----------------------------------------------------
	; Update Graphics \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		
		;Draw the spacecraft
		gSC -> SetData, reform(sc_xyz_bpp[0,*]), reform(sc_xyz_bpp[1,*])

		;Draw the gun positions
		gGuns = symbol(reform([[g1_pos[0,*]], [g2_pos[0,*]]]), $
		               reform([[g1_pos[1,*]], [g2_pos[1,*]]]), 'circle', $
		               /DATA, TARGET=gAxes)

		;Draw the beams
		gFire1 -> SetData, g1x, g1y, CONNECTIVITY=conn1
		gFire2 -> SetData, g2x, g2y, CONNECTIVITY=conn2
		
		;Draw the target
		if tf_drift then gTarget -> SetData, d[0,*], d[1,*]
		
	;-----------------------------------------------------
	; Next Iteration\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Save?
		if filename ne '' then begin
			if nPts le 2 then begin
				gAxes -> Save, filename
			endif else begin
				if i eq nPts - 2 then close = 1 else close = 0
				gAxes -> Save, filename, /APPEND, CLOSE=close
			endelse
		endif
	
		;Let me see what is happening
		win -> Refresh
		wait, 0.5
		win -> Refresh, /DISABLE
		
		;Delete the guns
		if i ne nPts - 2 then gGuns -> Delete
	endfor

	win -> Refresh
	return, win
end


;+
;   Determine the firing angle of each beam in their own BPP.
;
; :Params:
;       SRT_FILE:           in, required, type=string
;                           Name of a CDF file containing MMS DSS times.
;       DFG_FILE:           in, required, type=string
;                           Name of a CDF file containing MMS magnetic field data.
;       EDI_FILE:           in, required, type=string
;                           Name of a CDF file containing MMS EDI data.
;
; :Keywords:
;       TEST_CASE:          in, optional, type=integer
;                           An integer specifying the case of test data to use. If
;                               provided, all input parameters are ignored. If TEST_CASE
;                               is LE 0, then a description of the available test cases
;                               will be printed to the command window.
;       VIEW:               in, optional, type=boolean, default=0
;                           If set, a graph of the results will be created.
;       BAVG_BPP:           in, optional, type=boolean, default=0
;                           If set, results will be transformed into BPP as defined by
;                               the 5-s averaged magnetic field product. The default is
;                               to project each beam into its own BPP, then draw them
;                               all onto a common BPP. This keyword is used only if
;                               `VIEW` is set.
;-
function mms_bestarg_inputs, srt_file, dfg_file, edi_file, $
BAVG_BPP=Bavg_bpp, $
FILENAME=filename, $
SAVE_FILE=save_file, $
TEST_CASE=test_case, $
VIEW=view
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg(/QUIET)
		return, -1
	endif

	;Defaults
	view     = keyword_set(view)
	bavg_bpp = keyword_set(bavg_bpp)
	if n_elements(filename)  eq 0 then filename  = ''
	if n_elements(save_file) eq 0 then save_file = ''

;-----------------------------------------------------
; Get Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Test data.
	if test_case ne !Null then begin
		;Print a description of the test data?
		if test_case le 0 then begin
			!Null = mms_edi_test_data()
			return, -1
		endif
	
		;Get the test data
		test_data   = mms_edi_test_data(test_case)
		srt         = test_data.srt
		b_dfg_123   = test_data.B_123
		t_dfg       = test_data.B_time
		t_edi       = test_data.edi_time
		t_edi_beam1 = test_data.edi_time
		t_edi_beam2 = test_data.edi_time
		vax1        = reform(test_data.edi1_avolt[0,*])
		vay1        = reform(test_data.edi1_avolt[1,*])
		vax2        = reform(test_data.edi2_avolt[0,*])
		vay2        = reform(test_data.edi2_avolt[1,*])
		omega       = test_data.spin_freq
		drift_step  = test_data.drift_step_docs
		
		;Rotate magnetometer data into OCS and despin
		xyz2ocs    = mms_instr_xxyz2ocs('DFG_123')
		b_dfg_ocs  = rotate_vector(xyz2ocs, temporary(b_dfg_123))
		b_dfg_docs = mms_dss_despin(test_data.srt, t_dfg, temporary(b_dfg_ocs), OMEGA=omega)
		
		;Destroy the test data
		test_data   = !Null

	;Read from file.
	endif else begin
		;Read the DFG magnetic field data
		oCDF      = MrCDF_File(dfg_file)
		b_dfg_ocs = oCDF -> Read('B_xyz_scs_sc3', DEPEND_0=t_dfg)
		obj_destroy, oCDF

		;Read the EDI data
		oCDF = MrCDF_File(edi_file)
		t_edi = oCDF -> Read('Epoch')
		vax1  = oEDI -> Read('mms3_edi_vax1', DEPEND_0=t_edi_beam1)
		vay1  = oEDI -> Read('mms3_edi_vay1')
		vax2  = oEDI -> Read('mms3_edi_vax2', DEPEND_1=t_edi_beam2)
		vay2  = oEDI -> Read('mms3_edi_vay2')
		obj_destroy, oCDF
		
		;Read the SRT data
		oSRT = MrCDF_File(srt_file)
		srt  = oSRT -> Read('tspin')
		obj_destroy, oCDF
	endelse

;-----------------------------------------------------
; Create Bavg \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Compute the average magnetic field
	b_avg = mms_edi_bavg(t_dfg, b_dfg_docs, t_edi, t_edi_beam1, t_edi_beam2, $
	                     B_STDEV        = B_stdev, $
	                     B_BEAM1_DOCS   = b_beam1_docs, $
	                     B_BEAM2_DOCS   = b_beam2_docs, $
	                     EDI1_BEAM_INDS = edi1_beam_inds, $
	                     EDI2_BEAM_INDS = edi2_beam_inds, $
	                     T_AVG          = t_avg, $
	                     T_ERR          = t_err)

;-----------------------------------------------------
; Project Into BPP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Get beam information in BPP
	gun_info_bpp = mms_edi_beam_bpp(srt, t_edi_beam1, t_edi_beam2, $
	                                b_beam1_docs, b_beam2_docs, $
	                                vax1, vay1, vax2, vay2, $
	                                SPIN_FREQ = omega, $
	                                GUN_INFO_DOCS = gun_info_docs)

;-----------------------------------------------------
; View the Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if view then begin
		if bavg_bpp then begin
			win = mms_bestarg_inputs_view(t_dfg, b_dfg_docs, t_avg, b_avg, t_err, b_stdev, $
			                              gun_info_docs.gun1_pos_docs, $
			                              gun_info_docs.gun2_pos_docs, $
			                              gun_info_docs.gun1_fire_docs, $
			                              gun_info_docs.gun2_fire_docs, $
			                              edi1_beam_inds, $
			                              edi2_beam_inds, $
			                              drift_step, $
			                              /DOCS, $
			                              FILENAME = filename)
		endif else begin
			;Rotate the drift step into BPP
			xyz2bpp        = mms_instr_xb2bpp(b_beam1_docs)
			drift_step_bpp = rotate_vector(xyz2bpp, drift_step)
		
			;View
			win = mms_bestarg_inputs_view(t_dfg, b_dfg_docs, t_avg, b_avg, t_err, b_stdev, $
			                              gun_info_bpp.gun1_pos_bpp, $
			                              gun_info_bpp.gun2_pos_bpp, $
			                              gun_info_bpp.gun1_fire_bpp, $
			                              gun_info_bpp.gun2_fire_bpp, $
			                              edi1_beam_inds, $
			                              edi2_beam_inds, $
			                              drift_step_bpp, $
			                              FILENAME = filename)
		endelse
	endif

;-----------------------------------------------------
; Form Inputs to Bestarg \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	mms_bestarg_input = create_struct(gun_info_bpp, $
	                                  't_avg',          t_avg, $
	                                  'B_avg_docs',     b_avg, $
	                                  't_err',          t_err, $
	                                  'b_stdev',        b_stdev, $
	                                  'b_beam1',        b_beam1_docs, $
	                                  'b_beam2',        b_beam2_docs, $
	                                  'edi1_beam_inds', edi1_beam_inds, $
	                                  'edi2_beam_inds', edi2_beam_inds)

;-----------------------------------------------------
; Write to File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;CDF File?
	if filename ne '' then begin
		fout = mms_bestarg_inputs_file()
	endif
	
	;IDL save file?
	if save_file ne '' then save, mms_bestarg_input, FILENAME=save_file, $
	                              DESCRIPTION='Data structure for input into the Bestarg program.'

	;Return the data
	return, mms_bestarg_input
end