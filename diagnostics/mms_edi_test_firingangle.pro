; docformat = 'rst'
;
; NAME:
;    mms_edi_test_interp
;
; PURPOSE:
;+
;   Test different interpolation method for the magnetic field.
;
;   Results:
;       1) The difference between bulk spline interpolation and spline
;          interpolation in a sliding 15 second window is on the order
;          of floating poitn roundoff error.
;       2) The difference between bulk spline and bulk linear
;          interpolation is on the order of 0.01nT, and is more pronounced
;          in the spin-plane components than in the spin-axis component.
;
; :Categories:
;    MMS, QL
;
; :Params:
;
; :Keywords:
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
;       2015/10/18  -   Written by Matthew Argall
;-
function mms_edi_test_firingangle, sc, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif

	sc       = 'mms2'
	mode     = 'brst' ;or {'srvy' | 'brst'}
	tstart   = '2015-08-04T15:44:00Z'  ;'2015-08-04T15:44:00Z'
	tend     = '2015-08-04T15:49:00Z'  ;'2015-08-04T16:00:00Z'
	edi_dir  = '/nfs/edi/temp/'
	sdc_dir  = '/nfs/'
	hk_dir   = '/nfs/hk/'
	max_tt_distance = long64(0.25 * 1d9)
	verbose = 0

;-------------------------------------------------------
; Find Files ///////////////////////////////////////////
;-------------------------------------------------------
	_mode = mode eq 'brst' ? mode : ['fast', 'slow']
	
	;EDI QL FAST file
	files_edi = mms_find_file(sc, 'edi', _mode, 'l1a', $
	                          COUNT     = nedi, $
	                          OPTDESC   = 'amb', $
	                          SDC_ROOT  = sdc_dir, $
	                          SEARCHSTR = searchstr, $
	                          TSTART    = tstart, $
	                          TEND      = tend)

	if nedi eq 0 then message, 'No EDI fast or slow survey files found.'
	
	;FG L1B file
	files_fg = mms_find_file(sc, 'dfg', mode, 'l1b', $
	                         COUNT     = nfiles_fg, $
	                         OPTDESC   = '', $
	                         SDC_ROOT  = sdc_dir, $
	                         SEARCHSTR = searchstr, $
	                         TSTART    = tstart, $
	                         TEND      = tend)
	if nfiles_fg eq 0 then message, 'No FG files found: "' + searchstr + '".'

;-------------------------------------------------------
; Read Files ///////////////////////////////////////////
;-------------------------------------------------------
	
	;FG
	fgm_l1b = mms_fg_read_l1b(files_fg, tstart, tend)
	
	;EDI
	edi = mms_edi_read_l1a_amb(files_edi, tstart, tend)

;-------------------------------------------------------
; Interpolate Angle ////////////////////////////////////
;-------------------------------------------------------
	method = 1
	if mode eq 'brst' then begin
		if method eq 1 then begin
			;Locate data values in angle array
			iangle     = value_locate(edi.epoch_angle, edi.epoch_gdu1)
			azimuth    = reform(edi.azimuth[iangle])
			polar      = reform(edi.polar[iangle])
			pitch_gdu1 = reform(edi.pitch_gdu1[iangle])
			pitch_gdu2 = reform(edi.pitch_gdu2[iangle])

		endif else if method eq 2 then begin
			;Interpolate angles
			azimuth = interpol(edi.azimuth, edi.epoch_angle, edi.epoch_gdu1)
			polar   = interpol(edi.polar,   edi.epoch_angle, edi.epoch_gdu1)
		
			;Create bin sectors
			abins = indgen(32)  * 360.0/32.0
			pbins = indgen(512) * 360.0/512.0
		
			;Locate angles in bin sectors
			iaz     = value_locate(abins, azimuth)
			ipol    = value_locate(pbins, polar)
			azimuth = abins[iaz]
			polar   = pbins[ipol]
		endif else begin
			message, 'METHOD must be 1 or 2.'
		endelse
		
		t_gdu1  = reform(edi.epoch_gdu1)
		t_gdu2  = reform(edi.epoch_gdu2)
		t_angle = reform(edi.epoch_angle)
	endif else begin
		t_gdu1  = reform(edi.epoch_gdu1)
		t_gdu2  = reform(edi.epoch_gdu2)
		t_angle = reform(edi.epoch_angle)
		azimuth = reform(edi.azimuth)
		polar   = reform(edi.polar)
	endelse
	
	edi = !Null

;-------------------------------------------------------
; Extract Data /////////////////////////////////////////
;-------------------------------------------------------
	;Extract data
	t_fgm    = fgm_l1b.tt2000
	b_fgm    = fgm_l1b.b_bcs[0:2,*]
	
	;Delete extra data
	fg_l1b = !Null
;-------------------------------------------------------
; Azimuth and Polar of B in EDI CS /////////////////////
;-------------------------------------------------------
	
	;Rotation matrices from BCS to EDI1 & EDI2
	bcs2edi1 = mms_instr_xxyz2instr('BCS', 'EDI1')
	bcs2edi2 = mms_instr_xxyz2instr('BCS', 'EDI2')

	;Rotate B from BCS to EDI1 & EDI2
	b_edi1 = MrVector_Rotate(bcs2edi1, b_fgm)
	b_edi2 = MrVector_Rotate(bcs2edi2, temporary(b_fgm))
	
	;Determine polar and azimuthal angles
	b1_hat = MrVector_Normalize(temporary(b_edi1))
	b2_hat = MrVector_Normalize(temporary(b_edi2))
	b1_az  = atan(b1_hat[1,*], b1_hat[0,*]) * !radeg
	b1_pol = acos(b1_hat[2,*]) * !radeg
	b2_az  = atan(b2_hat[1,*] / b2_hat[0,*]) * !radeg
	b2_pol = acos(b2_hat[2,*]) * !radeg
	b1_hat = !Null
	b2_hat = !Null
	
	b1_az += (b1_az lt 0) * 360.0
;	b2_az += (b2_az lt 0) * 360.0

	b1_az  = reform(b1_az)
	b2_az  = reform(b2_az)
	b1_pol = reform(b1_pol)
	b2_pol = reform(b2_pol)

;-------------------------------------------------------
; View Results /////////////////////////////////////////
;-------------------------------------------------------
	;Convert time to seconds
	t_fgm_ssm   = MrCDF_epoch2ssm(t_fgm,   t_fgm[0])
	t_gdu1_ssm  = MrCDF_epoch2ssm(t_gdu1,  t_fgm[0])
	t_gdu2_ssm  = MrCDF_epoch2ssm(t_gdu2,  t_fgm[0])
	t_angle_ssm = MrCDF_epoch2ssm(t_angle, t_fgm[0])
	
	
	win = MrWindow(NAME='EDI Interp', YSIZE=690, YGAP=0.5, REFRESH=0)

	yaz  = [min([b1_az,  b2_az,  azimuth], MAX=ymax), ymax]
	ypol = [min([b1_pol, b2_pol, polar],   MAX=ymax), ymax]

	;EDI1 Azimuth
	pB1az = MrPlot(t_fgm_ssm, b1_az, $
	               /CURRENT, $
	               NAME        = 'B1 Azimuth', $
	               TITLE       = '', $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = yaz, $
	               YTITLE      = 'B$\sub\\phi$ (deg)')
	p1az = MrPlot(t_gdu1_ssm, azimuth, $
	              COLOR       = 'Blue', $
	              NAME        = 'EDI1 Azimuth', $
	              OVERPLOT    = pB1az, $
	              PSYM        = 4)

	;EDI1 Polar
	pB1pol = MrPlot(t_fgm_ssm, b1_pol, $
	                /CURRENT, $
	                NAME        = 'B1 Polar', $
	                TITLE       = '', $
	                XTICKFORMAT = '(a1)', $
	                YRANGE      = ypol, $
	                YTITLE      = 'B$\sub\\theta$ (deg)')
	p1pol = MrPlot(t_gdu1_ssm, polar, $
	               COLOR       = 'Blue', $
	               NAME        = 'EDI1 Polar', $
	               OVERPLOT    = pB1pol, $
	               PSYM        = 4)

	;Pitch Angle
	;  - The gun looks in the same direction as B
	;  - Captrues electrons traveling against B
	pB1pol = MrPlot(t_angle_ssm, pitch_gdu1, $
	                /CURRENT, $
	                NAME        = 'PA GDU1', $
	                TITLE       = '', $
	                XTICKFORMAT = '(a1)', $
	                YRANGE      = [-1, 190], $
	                YTITLE      = 'PA GDU1')

	;EDI2 Azimuth
	pB2az = MrPlot(t_fgm_ssm, b2_az, $
	               /CURRENT, $
	               NAME        = 'B2 Azimuth', $
	               TITLE       = '', $
	               XTICKFORMAT = '(a1)', $
	               YRANGE      = yaz, $
	               YTITLE      = 'B$\down\\phi$ (deg)')
	p2az = MrPlot(t_gdu1_ssm, azimuth, $
	              COLOR       = 'Blue', $
	              NAME        = 'EDI1 Azimuth', $
	              OVERPLOT    = pB2az, $
	              PSYM        = 4)
	
	;EDI2 Polar
	pB2pol = MrPlot(t_fgm_ssm, b2_pol, $
	                /CURRENT, $
	                NAME        = 'B2 Polar', $
	                TITLE       = '', $
	                XTICKFORMAT = 'time_labels', $
	                YRANGE      = ypol, $
	                YTITLE      = 'B$\down\\theta$ (deg)')
	p1az = MrPlot(t_gdu1_ssm, polar, $
	              COLOR       = 'Blue', $
	              NAME        = 'EDI2 Polar', $
	              OVERPLOT    = pB2pol, $
	              PSYM        = 4)


	win -> Refresh
	return, win
end
