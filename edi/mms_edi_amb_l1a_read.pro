; docformat = 'rst'
;
; NAME:
;       mms_edi_amb_l1a_read
;
; PURPOSE:
;+
;   Read EDI level 1A ambient mode data.
;
; :Categories:
;   MMS, EDI
;
; :Params:
;       FILES:          in, required, type=string/strarr
;                       Name of the EDI e-field mode file or files to be read.
;       TSTART:         in, optional, type=string
;                       Start time of the data interval to read, as an ISO-8601 string.
;       TEND:           in, optional, type=string
;                       End time of the data interval to read, as an ISO-8601 string.
;
; :Keywords:
;       STATUS:         in, optional, type=integer
;                       Error status. Output codes are:
;                           OK      = 0
;                           Warning = 1-99
;                           Error   = 100-255
;                               100      -  Unknown trapped error
;                               101      -  Error reading file
;                               102      -  No data in file
;                               104      -  Incompatible file versions
;
; :Returns:
;       EDI:            Structure of EDI data. Fields are below.
;                             'EPOCH'          -  TT2000 times
;                             'EPOCH_ANGLE'    -  TT2000 times for AZIMUTH and POLAR
;                             'EPOCH_TIMETAGS' -  TT2000 times for PITCH_MODE and PACK_MODE
;                             'COUNTS_GDU1'    -  Electron counts from GDU1
;                             'ENERGY_GDU1'    -  Energy mode of GDU1
;                                                   0:      0 eV
;                                                   1:      250 eV
;                                                   2:      500 eV
;                                                   3:      1000 eV
;                             'COUNTS_GDU2'    -  Electron counts from GDU2
;                             'ENERGY_GDU2'    -  Energy mode of GDU2
;                             'AZIMUTH'        -  Azimuthal look direction in GDU1 system
;                             'POLAR'          -  Polar look direction in GDU1 system
;                             'PITCH_MODE'     -  Pitch angle mode
;                                                   0:      0 & 180 degrees
;                                                   2:      90 degrees
;                                                   1 or 3: Alternate 90, 0 & 180 degrees
;                             'PACK_MODE'      -  Correlator length
;                                                   0:      2 detector pads in use
;                                                   4:      4 detector pads in use
;
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
;       2015/06/01  -   Written by Matthew Argall
;       2015/10/15  -   Read burst data. - MRA
;       2015/11/24  -   Renamed from mms_edi_read_l1a_amb to mms_edi_amb_l1a_read. - MRA
;       2016/02/01  -   Accommodate packing mode = 2 files. - MRA
;       2016/02/27  -   Added STATUS keyword. - MRA
;       2017/10/13  -   FLIP_FLAG variable was added in v1.2.0. Was checking VX GE 1
;                           instead of VX GT 1. Fixed. - MRA
;       2018/03/16  -   Use Hans's mms_edi_amb_l1a_expandangles to expand angles. Avoids
;                           problem where epoch_angle variable is time shifted by one
;                           point in amb-alt-oob. Fixes other problems associated with
;                           flipping look directions. - MRA
;       2019/05/09  -   Removed the QUALITY keyword; a remnant from e-field mode. - MRA
;       2019/12/13  -   In alternating mode, the third flip flag following a flip from
;                           field-aligned to perpendicular mode is set only in burst
;                           mode. Flip flag had same time tags as counts while pitchmode
;                           had packet time tags. Expand pitchmode variable. - MRA
;       2020/01/30  -   Incorporate mmsedi_ambburst_determineflips.pro to calculate
;                           flip flags in burst mode. - MRA
;       2020/03/24  -   Take flip flags from survey files for L1A >= v1.5.0. - MRA
;-
function mms_edi_amb_l1a_read, files, tstart, tend, $
EXPAND_ANGLES=expand_angles, $
STATUS=status
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Unknown status?
		if n_elements(status) eq 0 || status eq 0 then status = 100
		
		;Close files
		if n_elements(cdfIDs) gt 0 then $
			for i = 0, nFiles - 1 do if cdfIDs[i] ne 0 then cdf_close, cdfIDs[i]
		
		;Report error and return
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	tf_expand_angles = keyword_set(expand_angles)
	
;-----------------------------------------------------
; Check Input Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of files given
	nFiles  = n_elements(files)
	tf_sort = 0

	;Dissect the file name
	mms_dissect_filename, files, $
	                      INSTR   = instr, $
	                      LEVEL   = level, $
	                      MODE    = mode, $
	                      OPTDESC = optdesc, $
	                      SC      = sc, $
	                      VX      = vx, $
	                      VY      = vy, $
	                      VZ      = vz

	;Ensure L1A EDI files were given
	if min(file_test(files, /READ)) eq 0 then message, 'Files must exist and be readable.'
	if max(sc[0] eq ['mms1', 'mms2', 'mms3', 'mms4']) eq 0 then message, 'Invalid spacecraft identifier: "' + sc[0] + '".'
	if min(sc      eq sc[0])      eq 0 then message, 'All files must be from the same spacecraft.'
	if min(instr   eq 'edi')      eq 0 then message, 'Only EDI files are allowed.'
	if min(level   eq 'l1a')      eq 0 then message, 'Only L1A files are allowed.'
	if min(optdesc eq optdesc[0]) eq 0 then message, 'All files must have the same optional descriptor.'
	if min(mode    eq mode[0])    eq 0 then begin
		if total((mode eq 'fast') + (mode eq 'slow')) ne n_elements(mode) $
			then message, 'All files must have the same telemetry mode.' $
			else tf_sort = 1
	endif

	;We now know all the files match, so keep on the the first value.
	if nFiles gt 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
	end
	
	;The optional descriptor contains the packing mode for the file
	;   - 'amb'    = ambient mode, packing mode 1
	;   - 'amb-pm2 = ambient mode, packing mode 2
	suffix   = ''
	optparts = strsplit(optdesc, '-', /EXTRACT, COUNT=nparts)
	if nparts gt 1 then begin
		optdesc = optparts[0]
		case optparts[1] of
			'pm2': suffix = '_pm2'
			else: message, 'Unknown optional descriptor: "' + optdesc + '".'
		endcase
	endif else begin
		suffix = ''
	endelse

;-----------------------------------------------------
; Version Control \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;v0.6.z brst -- DEPEND_0 for pitch_gdu[1,2] points to wrong variable.
	;               It should be "Epoch", but instead is "epoch_angle".
	;               No special action is required so long as you are
	;               aware of this.
	;

	
	;Convert from energy bit to energy
	tf_energy_units = 1B
	
	;vX.Y.Z and earlier brst files have energy bit values
;	tf_energy_units = 0B
;	if mode eq 'brst' then begin
;		;Convert energy units?
;		ibad = where( (vx eq 0) and (vy le 7), nbad )
;		if nbad gt 0 then tf_energy_units = 1B
;		
;		;All files must follow the same conventions
;		if nbad ne 0 && nbad ne nFiles $
;			then message, 'Incompatible file versions.'
;	endif

;-----------------------------------------------------
; Varialble Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	prefix = sc + '_' + instr + '_' + optdesc + '_'
	
	;Variable names for GDU1
	counts1_gdu1_name = prefix + 'gdu1_raw_counts1'
	counts2_gdu1_name = prefix + 'gdu1_raw_counts2'
	counts3_gdu1_name = prefix + 'gdu1_raw_counts3'
	counts4_gdu1_name = prefix + 'gdu1_raw_counts4'
	energy_gdu1_name  = prefix + 'energy1'
	pitch_gdu1_name   = sc + '_' + instr + '_pitch_gdu1'
	
	;Variable names for GDU2
	counts1_gdu2_name = prefix + 'gdu2_raw_counts1'
	counts2_gdu2_name = prefix + 'gdu2_raw_counts2'
	counts3_gdu2_name = prefix + 'gdu2_raw_counts3'
	counts4_gdu2_name = prefix + 'gdu2_raw_counts4'
	energy_gdu2_name  = prefix + 'energy2'
	pitch_gdu2_name   = sc + '_' + instr + '_pitch_gdu2'
	
	;Other variable names
	phi_name          = prefix + 'phi'
	theta_name        = prefix + 'theta'
	dwell_name        = prefix + 'dwell'
	pitch_name        = prefix + 'pitchmode'
	pacmo_name        = prefix + 'pacmo'
	optics_name       = prefix + 'optics'
	perp_oneside_name = prefix + 'perp_onesided'
	perp_bidir_name   = prefix + 'perp_bidirectional'
	flip_flag_name    = sc + '_' + instr + '_flip'

;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Open the files
	cdfIDs = lonarr(nFiles)
	for i = 0, nFiles - 1 do cdfIDs[i] = cdf_open(files[i])

	;Read the data for GD12
	counts1_gdu1 = MrCDF_nRead(cdfIDs, counts1_gdu1_name, $
	                           DEPEND_0 = epoch_gdu1, $
	                           NRECS    = nRecs_gdu1, $
	                           STATUS   = status_gdu1, $
	                           TSTART   = tstart, $
	                           TEND     = tend)

	;Read the data for GD21
	counts1_gdu2 = MrCDF_nRead(cdfIDs, counts1_gdu2_name, $
	                           DEPEND_0 = epoch_gdu2, $
	                           NRECS    = nRecs_gdu2, $
	                           STATUS   = status_gdu2, $
	                           TSTART   = tstart, $
	                           TEND     = tend)
	
	;No records in file
	;   - It is possible for only one of the detectors to be operating.
	if nRecs_gdu1 + nRecs_gdu2 eq 0 then begin
		status = 102
		message, 'No records in file.'
	endif

	;Other file-related problem
	if status_gdu1 ne 0 || status_gdu2 ne 0 then begin
		if status_gdu1 eq 2 || status_gdu2 eq 2 then begin
			status = 1
		endif else begin
			status = 101
			message, /REISSUE_LAST
		endelse
	endif

	;Read the rest of the variables?
	energy_gdu1 = MrCDF_nRead(cdfIDs, energy_gdu1_name, TSTART=tstart, TEND=tend)
	pitch_gdu1  = MrCDF_nRead(cdfIDs, pitch_gdu1_name,  TSTART=tstart, TEND=tend)
	energy_gdu2 = MrCDF_nRead(cdfIDs, energy_gdu2_name, TSTART=tstart, TEND=tend)
	pitch_gdu2  = MrCDF_nRead(cdfIDs, pitch_gdu2_name,  TSTART=tstart, TEND=tend)
	
	;Read other data
	optics     = MrCDF_nRead(cdfIDs, optics_name, TSTART=tstart, TEND=tend)
	phi        = MrCDF_nRead(cdfIDs, phi_name,    TSTART=tstart, TEND=tend, DEPEND_0=epoch_angle)
	theta      = MrCDF_nRead(cdfIDs, theta_name,  TSTART=tstart, TEND=tend)
	pitch_mode = MrCDF_nRead(cdfIDs, pitch_name,  TSTART=tstart, TEND=tend, DEPEND_0=epoch_timetag)
	pack_mode  = MrCDF_nRead(cdfIDs, pacmo_name,  TSTART=tstart, TEND=tend)
	dwell      = MrCDF_nRead(cdfIDs, dwell_name,  TSTART=tstart, TEND=tend)
	
	;Burst data?
	if mode eq 'brst' then begin
		counts2_gdu1 = MrCDF_nRead(cdfIDs, counts2_gdu1_name,  TSTART=tstart, TEND=tend)
		counts3_gdu1 = MrCDF_nRead(cdfIDs, counts3_gdu1_name,  TSTART=tstart, TEND=tend)
		counts4_gdu1 = MrCDF_nRead(cdfIDs, counts4_gdu1_name,  TSTART=tstart, TEND=tend)
		counts2_gdu2 = MrCDF_nRead(cdfIDs, counts2_gdu2_name,  TSTART=tstart, TEND=tend)
		counts3_gdu2 = MrCDF_nRead(cdfIDs, counts3_gdu2_name,  TSTART=tstart, TEND=tend)
		counts4_gdu2 = MrCDF_nRead(cdfIDs, counts4_gdu2_name,  TSTART=tstart, TEND=tend)
	endif
	
	;Perp one-sided and bidirectional
	;   - v1.0.0 introduced these variables
	;   - If the variables cannot be read, make their default = 0
	iRead = where(vx ge 1, nRead, COMPLEMENT=iMake, NCOMPLEMENT=nMake)
	if nMake eq 0 then begin
		perp_oneside = MrCDF_nRead(cdfIDs, perp_oneside_name, TSTART=tstart, TEND=tend)
		perp_bidir   = MrCDF_nRead(cdfIDs, perp_bidir_name,   TSTART=tstart, TEND=tend)
	endif else if nRead eq 0 then begin
		perp_oneside = bytarr(n_elements(epoch_timetag))
		perp_bidir   = bytarr(n_elements(epoch_timetag))
	endif else begin
		status = 104B
		message, 'Unfortunate mix of version numbers. Cannot continue.'
	endelse
	
	;Flip flag
	;   - v1.2.0 introduced flip flags to burst files
	;   - If the variables cannot be read, make their default = 0
	;   - mmsedi_ambburst_determineflips overrides the flip flags in L1A burst files
	;   - v1.5.0 of L1A introduced flip flags to survey files
	iFlip = where( (vx gt 1) or ((vx eq 1) and (vy ge 2)), nFlip, COMPLEMENT=iNoFlip, NCOMPLEMENT=nNoFlip )
	iSrvyFlip = where( (vx gt 1) or ((vx eq 1) and (vy ge 5)), nSrvyFlip, NCOMPLEMENT=nSrvyNoFlip )
	if mode eq 'brst' then begin
		flip_flag = mmsedi_ambburst_determineflips(pitch_gdu1)
		flip_flag = byte(flip_flag.combined_flip)
	endif else begin
		if (nSrvyFlip gt 0) && (nSrvyNoFlip gt 0) then begin
			status = 104B
			message, 'Unfortunate mix of version numbers. Cannot continue.'
		endif
		
		if nSrvyFlip gt 0 then begin
			flip_flag = MrCDF_nRead(cdfIDs, flip_flag_name, TSTART=tstart, TEND=tend)
		endif else begin
			flip_flag = bytarr(n_elements(epoch_gdu1))
		endelse
	endelse
	
	;Close the files
	for i = 0, nFiles - 1 do begin
		cdf_close, cdfIDs[i]
		cdfIDs[i] = 0L
	endfor

;-----------------------------------------------------
; Convert Dwell Time to Seconds \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;DWELL has units of 1/512 seconds
	dwell = float(dwell) / 512.0

;-----------------------------------------------------
; Convert Energy \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Convert to unsigned shorts
	if tf_energy_units then begin
		energy_gdu1 = fix(energy_gdu1, TYPE=12)
		energy_gdu2 = fix(energy_gdu2, TYPE=12)

		;GDU1
		i250 = where(energy_gdu1 eq 3, n250)
		i500 = where(energy_gdu1 eq 2, n500)
		i1k  = where(energy_gdu1 eq 1, n1k)
		if n250 gt 0 then energy_gdu1[i250] = 250US
		if n500 gt 0 then energy_gdu1[i500] = 500US
		if n1k  gt 0 then energy_gdu1[i1k]  = 1000US
	
		;GDU2
		i250 = where(energy_gdu2 eq 3, n250)
		i500 = where(energy_gdu2 eq 2, n500)
		i1k  = where(energy_gdu2 eq 1, n1k)
		if n250 gt 0 then energy_gdu2[i250] = 250US
		if n500 gt 0 then energy_gdu2[i500] = 500US
		if n1k  gt 0 then energy_gdu2[i1k]  = 1000US
	endif

;-----------------------------------------------------
; Expand Angles \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Expand the EPOCH_ANGLE values to match the resolution of EPOCH_GDU[12]
	;
	nangle = n_elements(epoch_angle)
	nepoch = n_elements(epoch_gdu1)
	if tf_expand_angles && nangle ne nepoch then begin
		;Print a warning
		MrPrintF, 'LogText', '---------------------------------------------------------------'
		MrPrintF, 'LogWarn', 'EPOCH_ANGLE and EPOCH_GDU1 do not have same number of elements.'
		MrPrintF, 'LogWarn', nangle, FORMAT='(%"   EPOCH_ANGLE:  %i")'
		MrPrintF, 'LogWarn', nepoch, FORMAT='(%"   EPOCH_GDU1:   %i")'
		MrPrintF, 'LogWarn', '   ---> Expanding EPOCH_ANGLE.'
		MrPrintF, 'LogText', '---------------------------------------------------------------'
		MrPrintF, 'LogText', ''

		;How many points are we extrapolating
		iextrap = where(epoch_gdu2 lt epoch_angle[0], nextrap)
		if nextrap gt 0 $
			then MrPrintF, 'LogWarn', nextrap, FORMAT='(%"%i counts before first epoch_angle time.")'
		
		;Locate each EPOCH_GDU1 within EPOCH_ANGLE
;		iloc = value_locate(epoch_angle, epoch_gdu1) > 0
;		iloc = iloc + 1
		
		;Expand the angle arrays
;		epoch_angle = epoch_gdu1
;		theta       = theta[iloc]
;		phi         = phi[iloc]
		
		temp  = mmsedi_amb_expandangles(reform(pitch_gdu1), reform(phi), reform(theta))
		theta = temp.theta
		phi   = temp.phi
		temp  = !Null
	endif

;-----------------------------------------------------
; Update Flip Flag \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Expand the pitchmode variable so that it has the same
	; number of elements as the flip flag. Then, adjust the
	; flip flags.
	;
	; NOTE:
	;   Expanding pitchmode adversely affects other programs
	;   in the processing chain, such as mms_edi_amb_ops_bitmask,
	;   which expects pitchmode to be on packet time stamps.
	;   Therefore, the expansion is not propagated out.
	;
	; There are two flip flags in sequence each time the GDU
	; changes look directions. If we take every other flipped
	; flag (the second of the pair for each pair), then add
	; one, we get the index of the first element after the
	; flipped bit.
	;
	; Note:
	;     IDL> a = ingen(10)
	;     IDL> print, a[ [1,99999] ]
	;            9
	;
	if mode eq 'brst' then begin
		nflip = n_elements(flip_flag)
		npitch = n_elements(pitch_mode)
		if nflip ne npitch then begin
			;Print a warning
			MrPrintF, 'LogText', '---------------------------------------------------------------'
			MrPrintF, 'LogWarn', 'FLIP and PITCHMODE do not have same number of elements.'
			MrPrintF, 'LogWarn', nflip,  FORMAT='(%"   FLIP:       %i")'
			MrPrintF, 'LogWarn', npitch, FORMAT='(%"   PITCHMODE:  %i")'
			MrPrintF, 'LogWarn', '   ---> Expanding PITCHMODE Variable.'
			MrPrintF, 'LogText', '---------------------------------------------------------------'
			MrPrintF, 'LogText', ''
	
			;How many points are extrapolating
			iextrap = where(epoch_gdu1 lt epoch_timetag[0], nextrap)
			if nextrap gt 0 $
				then MrPrintF, 'LogWarn', nextrap, FORMAT='(%"%i counts before first epoch_timetag time.")'
		
			;Locate EPOCH_GDU1 within EPOCH_TIMETAG
			iloc = value_locate(epoch_timetag, epoch_gdu1) > 0
		
			;Expand flip flags
			temp_pitch_mode = pitch_mode[iloc]
		endif
	endif
	
;-----------------------------------------------------
; Return Structure \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;All data
	edi_amb = { epoch_gdu1:    reform( temporary(epoch_gdu1) ), $
	            epoch_gdu2:    reform( temporary(epoch_gdu2) ), $
	            epoch_angle:   reform( temporary(epoch_angle) ), $
	            epoch_timetag: reform( temporary(epoch_timetag) ), $
	            optics:        reform( temporary(optics) ), $
	            dwell:         reform( temporary(dwell) ), $
	            pitch_gdu1:    reform( temporary(pitch_gdu1) ), $
	            energy_gdu1:   reform( temporary(energy_gdu1) ), $
	            pitch_gdu2:    reform( temporary(pitch_gdu2) ), $
	            energy_gdu2:   reform( temporary(energy_gdu2) ), $
	            azimuth:       reform( temporary(phi) ), $
	            polar:         reform( temporary(theta) ), $
	            pitch_mode:    reform( temporary(pitch_mode) ), $
	            pack_mode:     reform( temporary(pack_mode) ), $
	            perp_oneside:  reform( temporary(perp_oneside) ), $
	            perp_bidir:    reform( temporary(perp_bidir) ), $
	            flip_flag:     reform( temporary(flip_flag) ) $
	          }
	
	if mode eq 'brst' then begin
		edi_amb = create_struct(edi_amb, $
		                        'counts_gdu1', transpose( [ temporary(counts1_gdu1), $
		                                                    temporary(counts2_gdu1), $
		                                                    temporary(counts3_gdu1), $
		                                                    temporary(counts4_gdu1) ] ), $
		                        'counts_gdu2', transpose( [ temporary(counts1_gdu2), $
		                                                    temporary(counts2_gdu2), $
		                                                    temporary(counts3_gdu2), $
		                                                    temporary(counts4_gdu2) ] ) )
	endif else begin
		edi_amb = create_struct(edi_amb, $
		                        'counts_gdu1', reform( temporary(counts1_gdu1) ), $
		                        'counts_gdu2', reform( temporary(counts1_gdu2) ) )
	endelse

	;If fast and slow survey files were given, we need to sort in time.
	if tf_sort then begin
		igdu1  = sort(edi_amb.epoch_gdu1)
		igdu2  = sort(edi_amb.epoch_gdu2)
		iangle = sort(edi_amb.epoch_angle)
		itt    = sort(edi_amb.epoch_timetag)
		
		edi_amb.epoch_gdu1    = edi_amb.epoch_gdu1[igdu1]
		edi_amb.epoch_gdu2    = edi_amb.epoch_gdu2[igdu2]
		edi_amb.epoch_angle   = edi_amb.epoch_angle[iangle]
		edi_amb.epoch_timetag = edi_amb.epoch_timetag[itt]
		edi_amb.optics        = edi_amb.optics[itt]
		edi_amb.dwell         = edi_amb.dwell[itt]
		edi_amb.counts_gdu1   = edi_amb.counts_gdu1[igdu1,*]
		edi_amb.pitch_gdu1    = edi_amb.pitch_gdu1[iangle]
		edi_amb.energy_gdu1   = edi_amb.energy_gdu1[itt]
		edi_amb.counts_gdu2   = edi_amb.counts_gdu2[igdu2,*]
		edi_amb.pitch_gdu2    = edi_amb.pitch_gdu2[iangle]
		edi_amb.energy_gdu2   = edi_amb.energy_gdu2[itt]
		edi_amb.azimuth       = edi_amb.azimuth[iangle]
		edi_amb.polar         = edi_amb.polar[iangle]
		edi_amb.pitch_mode    = edi_amb.pitch_mode[itt]
		edi_amb.pack_mode     = edi_amb.pack_mode[itt]
		edi_amb.perp_oneside  = edi_amb.perp_oneside[itt]
		edi_amb.perp_bidir    = edi_amb.perp_bidir[itt]
	endif
	
	;Return the data
	if n_elements(status) eq 0 then status = 0
	return, edi_amb
end