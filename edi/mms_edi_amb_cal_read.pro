; docformat = 'rst'
;
; NAME:
;       mms_edi_amb_cal_read
;
; PURPOSE:
;+
;   Read EDI ambient mode calibration data.
;
; :Categories:
;   MMS, EDI
;
; :Params:
;       FILES:          in, required, type=string/strarr
;                       Name of the EDI e-field mode file or files to be read.
;
; :Keywords:
;       TSTART:         in, optional, type=string
;                       Start time of the data interval to read, as an ISO-8601 string.
;       TEND:           in, optional, type=string
;                       End time of the data interval to read, as an ISO-8601 string.
;
; :Returns:
;       EDI_CAL:        Structure of EDI calibration data. Fields are below.
;                             'EPOCH'          -  TT2000 times
;                             'PHI'            -  Look-direction azimuth bin angles
;                             'THETA'          -  Look-direction polar bin angles
;                             'RELCAL_GDU1'    -  Relative calibration factors for GDU1
;                             'RELCAL_GDU2'    -  Relative calibration factors for GDU2
;                             'ABSCAL_GDU1'    -  Absolute calibration factors for GDU1
;                             'ABSCAL_GDU2'    -  Absolute calibration factors for GDU2
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
;       2015/01/30  -   Written by Matthew Argall
;-
function mms_edi_amb_cal_read, files, tstart, tend
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(cdfIDs) gt 0 then $
			for i = 0, nFiles - 1 do if cdfIDs[i] ne 0 then cdf_close, cdfIDs[i]
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
;-----------------------------------------------------
; Check Input Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of files given
	nFiles  = n_elements(files)

	;Dissect the file name
	mms_dissect_filename, files, $
	                      INSTR   = instr, $
	                      LEVEL   = level, $
	                      MODE    = mode, $
	                      OPTDESC = optdesc, $
	                      SC      = sc, $
	                      VX      = vx, $
	                      VY      = vy, $
	                      VZ      = VZ

	;Ensure L1A EDI files were given
	if min(file_test(files, /READ)) eq 0 then message, 'Files must exist and be readable.'
	if max(sc[0] eq ['mms1', 'mms2', 'mms3', 'mms4']) eq 0 then message, 'Invalid spacecraft identifier: "' + sc[0] + '".'
	if min(sc      eq sc[0]) eq 0 then message, 'All files must be from the same spacecraft.'
	if min(instr   eq 'edi') eq 0 then message, 'Only EDI files are allowed.'
	if min(level   eq 'l2')  eq 0 then message, 'Only L2 files are allowed.'
	if min(optdesc eq 'amb') eq 0 then message, 'Only EDI ambient-mode files are allowed.'
	if min(mode    eq 'cal') eq 0 then message, 'Only EDI calibration files are allowed.'

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	end

;-----------------------------------------------------
; Version Control \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;v0.y.z had different names
	tf_v0_names = 0B
	iv0 = where(vx eq 0, nv0)
	if nv0 gt 0 then begin
		tf_v0_names = 1B
		if nv0 ne nFiles then message, 'Incompatible file versions given.'
	endif
	
	;v1.y.z added optics state
	tf_optics = 0B
	iv1 = where(vx ge 1, nv1)
	if nv1 gt 0 then begin
		tf_optics = 1B
		if nv1 ne nFiles then message, 'Incompatible file versions given.'
	endif

;-----------------------------------------------------
; Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	prefix = strjoin([sc, instr], '_') + '_'
	suffix = '_' + strjoin([mode, level], '_')
	
	
	trel_vname       = 'epoch_rel'
	tabs_vname       = 'epoch_abs'
	theta_vname      = prefix + 'theta'
	phi_vname        = prefix + 'phi'
	optics_rel_vname = prefix + 'optics_rel' + suffix
	optics_abs_vname = prefix + 'optics_abs' + suffix

	;Cal names
	if tf_v0_names then begin
		relcal_gdu1_vname = prefix + 'relcal_gdu1' + suffix
		relcal_gdu2_vname = prefix + 'relcal_gdu2' + suffix
		abscal_gdu1_vname = prefix + 'abscal_gdu1' + suffix
		abscal_gdu2_vname = prefix + 'abscal_gdu2' + suffix
	endif else begin
		relcal_gdu1_vname = prefix + 'rel_gdu1' + suffix
		relcal_gdu2_vname = prefix + 'rel_gdu2' + suffix
		abscal_gdu1_vname = prefix + 'abs_gdu1' + suffix
		abscal_gdu2_vname = prefix + 'abs_gdu2' + suffix
	endelse
		

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])

	;Read the data for GD12
	relcal_gdu1 = MrCDF_nRead(cdfIDs, relcal_gdu1_vname, $
	                          DEPEND_0 = tt2000_rel, $
	                          DEPEND_1 = phi, $
	                          DEPEND_2 = theta, $
	                          STATUS   = status, $
	                          TSTART   = tstart, $
	                          TEND     = tend)
	
	;Read the rest of the variables?
	if status eq 0 then begin
		;Calibration data
		relcal_gdu2 = MrCDF_nRead(cdfIDs, relcal_gdu2_vname, TSTART=tstart, TEND=tend, NRECS=nt_abs)
		abscal_gdu1 = MrCDF_nRead(cdfIDs, abscal_gdu1_vname, TSTART=tstart, TEND=tend, NRECS=nabs_gdu1, DEPEND_0=tt2000_abs)
		abscal_gdu2 = MrCDF_nRead(cdfIDs, abscal_gdu2_vname, TSTART=tstart, TEND=tend, NRECS=nabs_gdu2)
		
		if tf_optics then begin
			optics_rel = MrCDF_nRead(cdfIDs, optics_rel_vname, TSTART=tstart, TEND=tend, NRECS=nOptRel)
			optics_abs = MrCDF_nRead(cdfIDs, optics_abs_vname, TSTART=tstart, TEND=tend, NRECS=nOptAbs)
		endif else begin
			nOptRel = 0
			nOptAbs = 0
		endelse
	endif else begin
		message, /REISSUE_LAST
	endelse
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor

;-----------------------------------------------------
; Return Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;All data
	edi_cal = { tt2000_rel:  temporary(tt2000_rel), $
	            phi:         temporary(phi), $
	            theta:       temporary(theta), $
	            relcal_gdu1: temporary(relcal_gdu1), $
	            relcal_gdu2: temporary(relcal_gdu2) $
	          }
	
	if nOptRel   gt 0 then edi_cal = create_struct(edi_cal, 'optics_rel', temporary(optics_rel))
	if nOptAbs   gt 0 then edi_cal = create_struct(edi_cal, 'optics_abs', temporary(optics_abs))
	if nt_abs    gt 0 then edi_cal = create_struct(edi_cal, 'tt2000_abs',  temporary(tt2000_abs))
	if nabs_gdu1 gt 0 then edi_cal = create_struct(edi_cal, 'abscal_gdu1', temporary(abscal_gdu1))
	if nabs_gdu2 gt 0 then edi_cal = create_struct(edi_cal, 'abscal_gdu2', temporary(abscal_gdu2))
	
	;Return the data
	return, edi_cal
end