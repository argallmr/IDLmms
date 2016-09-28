; docformat = 'rst'
;
; NAME:
;    mms_fsm_plot_calhist
;
; PURPOSE:
;+
;   Plot FSM histogrammed calibration data.
;
; :Categories:
;    MMS, FSM, L2Plus
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
;       2016/08/31  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;
;-
function mms_fsm_calhist_read, files
	compile_opt idl2
	on_error, 2

;------------------------------------;
; Variable Names                     ;
;------------------------------------;

	; Parse file name to get variable name prefix and suffix
	mms_dissect_filename, files[0], SC=sc, INSTR=instr, MODE=mode, LEVEL=level, OPTDESC=optdesc
	mag_instr = (strsplit(optdesc, '-', /EXTRACT))[1]
	prefix    = sc + '_' + instr + '_'
	suffix    = '_' + mag_instr + '_' + mode + '_' + level

	; Create Variable Names
	amph_vname   = prefix + 'amp'   + '_hist'  + suffix
	phaseh_vname = prefix + 'phase' + '_hist'  + suffix
	psdh_vname   = prefix + 'psd'   + '_hist'  + suffix

;------------------------------------;
; Read Data                          ;
;------------------------------------;
	
	;Structure fields
	hHist = hash( 'amp_hist',   hash(), $
	              'phase_hist', hash(), $
	              'psd_hist',   hash() )

	;Loop through all files
	nFiles = n_elements(files)
	for i = 0, nFiles - 1 do begin
		print, i+1, nFiles, FORMAT='(%"Reading file %i of %i")'
	
		;Read the data
		amph_temp   = MrCDF_Read(files[i], amph_vname, DEPEND_0=f, DEPEND_1=amp_bins, DEPEND_2=hflag, DEPEND_3=comp)
		phaseh_temp = MrCDF_Read(files[i], phaseh_vname, DEPEND_1=phase_bins)
		psdh_temp   = MrCDF_Read(files[i], psdh_vname, DEPEND_1=psd_bins)
		
		;Put DEPEND_0 as first dimension
		;   - [Bins, Flag, Comp, Freq] -> [Freq, Bins, Comp, Flag]
		;   - Do not put Flag last. This prevents the shallow dimension
		;     from being removed when there is only one flag.
		amph_temp   = transpose( amph_temp,   [3,0,1,2] )
		phaseh_temp = transpose( phaseh_temp, [3,0,1,2] )
		psdh_temp   = transpose( psdh_temp,   [3,0,1,2] )
		
		;Step through each flag
		;   - Flags can vary between files
		for j = 0, n_elements(hflag) - 1 do begin
			;Sum all histograms
			if hHist['amp_hist'] -> HasKey(hflag[j]) then begin
				hHist['amp_hist',   hflag[j]] +=   amph_temp[*,*,j,*]
				hHist['phase_hist', hflag[j]] += phaseh_temp[*,*,j,*]
				hHist['psd_hist',   hflag[j]] +=   psdh_temp[*,*,j,*]
			
			;Store the data
			endif else begin
				hHist['amp_hist',   hflag[j]] =   amph_temp[*,*,j,*]
				hHist['phase_hist', hflag[j]] = phaseh_temp[*,*,j,*]
				hHist['psd_hist',   hflag[j]] =   psdh_temp[*,*,j,*]
			endelse
		endfor
	endfor
	
	;Dependent data
	hHist['f']          = temporary(f)
	hHist['flag']       = temporary(hflag)
	hHist['comp']       = temporary(comp)
	hHist['amp_bins']   = temporary(amp_bins)
	hHist['phase_bins'] = temporary(phase_bins)
	hHist['psd_bins']   = temporary(psd_bins)

	return, hHist
end


;+
;
;-
function mms_fsm_calhist_noiseFloor, hist, f, bins
	compile_opt idl2
	on_error, 2
	
	;Number of bins
	nF    = n_elements(f)
	nBins = n_elements(bins)
	
	;Size of bins
	dF    = median(f[1:*] - f)
	dBins = median(bins[1:*] - bins)
	
	;Area of each bin
	area = dBins * hist
	
	;Total area
	area_tot = total(area, 2)
	iZero = where(area_tot eq 0, nZero)
	if nZero gt 0 then area_tot[iZero] = !values.f_nan

	;Relative area
	area_rel = total(area, 2, /CUMULATIVE) / rebin(area_tot, nF, nBins)
	if nZero gt 0 then area_rel[iZero,*] = 0.0
	
	;Find indices of the 50% area mark
	void   = min( abs(area_rel - 0.5), iMin, DIMENSION=2 )
	iFloor = array_indices(area, iMin)
	
	;Extract the noise floor
	noiseFloor = bins[iFloor[1,*]]
	return, noiseFloor
end



;+
;
;-
function mms_fsm_l2plus_cal_create, sc, mode
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Inputs and constants
	theComp   = 'z'
	flag      = 1
	sc        = 'mms4'
	mode      = 'brst'
	out_desc  = 'week'
	tstart    = '2015-11-01T00:00:00'
	tend      = '2015-11-09T00:00:00'
	dropbox   = '/nfs/fsm/temp'
	instr     = 'fsm'
	mag_instr = 'scm'
	level     = 'l2plus'
	optdesc   = 'cal-' + mag_instr
	sdc_root  = '/nfs/'
	timeorder = '%Y%M%d%H%m%S'
	png_file  = ''

;------------------------------------;
; Noise Floor from SCM               ;
;------------------------------------;
	;Read the nosie floor data from LPP
	lpp_file = mode eq 'srvy' ? '/home/argall/data/lpp_scm/noise_mms4_z_201606_srvy.txt' : $
	                            '/home/argall/data/lpp_scm/noise_mms4_z_201606_brst.txt'
	lpp_data = MrFile_Read_Ascii( lpp_file, $
	                              NHEADER      = 1, $
	                              COLUMN_NAMES = ['f', 'Amp'], $
	                              COLUMN_TYPES = ['FLOAT', 'FLOAT'], $
	                              GROUPS       = [1, 2] )

;------------------------------------;
; Read Data                          ;
;------------------------------------;
	
	;Find files within time range
	cal_files = mms_find_file( sc, instr, mode, level, $
	                           COUNT          = nFiles, $
	                           DROPBOX        = dropbox, $
	                           OPTDESC        = optdesc, $
	                           SDC_ROOT       = sdc_root, $
	                           TIMEORDER      = timeorder, $
	                           TSTART         = tstart, $
	                           TEND           = tend )

	;Read data
	hHist = mms_fsm_calhist_read(cal_files)
stop
;------------------------------------;
; Determine Noise Floor              ;
;------------------------------------;
	;Flags
	flags  = (hHist['amp_hist'] -> Keys()) -> ToArray()
	flags  = flags[sort(flags)]
	nFlags = hHist['amp_hist'] -> Count()
	
	;Step through each spectral type
	histData   = hash()
	noiseFloor = hash()
	foreach hSpecType, hHist, keySpec do begin
		;Only take the hashes
		if ~isa(hSpecType, 'hash') then continue

		;Allocate memory
		dims      = size( hSpecType[flags[0]], /DIMENSIONS)
		tempFloor = fltarr( dims[0], dims[2], nFlags )
		tempHist  = ulonarr(  dims[0], dims[1], dims[2], nflags )
		keyBin    = strmid(keySpec, 0, strlen(keySpec)-5) + '_bins'
		keyFloor  = strmid(keySpec, 0, strlen(keySpec)-5) + '_floor'
		
		;Step through each histogram flag
		;   - Loop over keys, not the hash, so that the keys remain in asciending order.
		foreach keyFlag, flags, iFlag do begin

			;Step through each component
			for iComp = 0, 2 do begin
				tempFloor[*, iComp, iFlag] = mms_fsm_calhist_noiseFloor( hSpecType[keyFlag, *, *, iComp], hHist['f'], hHist[keyBin] )
			endfor
			
			;Transfer the data from the hash into the array
			tempHist[*,*,*,iFlag] = hSpecType -> Remove(keyFlag)
		endforeach
		
		;Remove data from histogram hash
		hHist -> Remove, keySpec
		
		;Store data
		histData[keySpec]    = temporary(tempHist)
		noiseFloor[keyFloor] = temporary(tempFloor)
	endforeach
	
;------------------------------------;
; Write to File                      ;
;------------------------------------;
	hHist['flag'] = flags
	parents       = file_basename(temporary(cal_files))
	MrTimeParser, tstart, '%Y-%M-%dT%H:%m:%S', '%Y%M%d', fstart

	;Create the file
	fsm_file = mms_fsm_l2plus_cal_mkfile( sc, mode, fstart, hHist, $
	                                      BRST           = brst, $
	                                      DROPBOX_ROOT   = dropbox, $
	                                      DATA_PATH_ROOT = data_path, $
	                                      EMPTY_FILE     = empty_file, $
	                                      OPTDESC        = optdesc + '-' + out_desc, $
	                                      PARENTS        = parents, $
	                                      STATUS         = status )

	;Write the data
	status = mms_fsm_l2plus_cal_write(fsm_file, temporary(hHist) + temporary(histData) + temporary(noiseFloor))
	return, status
end

;
;;------------------------------------;
;; Plot                               ;
;;------------------------------------;
;	win = MrWindow(LAYOUT=[1,3], OXMARGIN=[10,15], REFRESH=0, XGAP=20, XSIZE=1100, YGAP=0.5, YSIZE=900)
;
;	;FGM AMP
;	im_amph = MrImage( amph, f, amp_bins, $
;	                   /AXES, $
;	                   /CURRENT, $
;	                   /LOG, $
;	                   /SCALE, $
;	                   NAME        = 'Hist Amp', $
;	                   TITLE       = 'FGM', $
;	                   XTICKFORMAT = '(a1)', $
;	                   YTITLE      = 'Log10( Amplitude )')
;	p_ampf = MrPlot( f, ampf, $
;	                 COLOR    = 'White', $
;	                 NAME     = 'Floor Amp', $
;	                 OVERPLOT = im_amph )
;	cb_amph = MrColorbar( ORIENTATION = 1, $
;	                      NAME        = 'CB: Hist Amp', $
;	                      TARGET      = im_amph, $
;	                      TITLE       = 'Occurrence')
;
;	;FGM PHASE
;	im_phaseh = MrImage( phaseh, f, phase_bins, $
;	                     /AXES, $
;	                     /CURRENT, $
;	                     /LOG, $
;	                     /SCALE, $
;	                     NAME          = 'Hist Phase', $
;	                     XTICKFORMAT   = '(a1)', $
;	                     YRANGE        = [-180.0, 180.0], $
;	                     YTICKINTERVAL = 90.0, $
;	                     YTITLE        = 'Phase')
;	p_phasef = MrPlot( f, phasef, $
;	                   COLOR    = 'White', $
;	                   NAME     = 'Floor Phase', $
;	                   OVERPLOT = im_phaseh )
;	cb_phaseh = MrColorbar( ORIENTATION = 1, $
;	                        NAME        = 'CB: Hist Phase', $
;	                        TARGET      = im_phaseh, $
;	                        TITLE       = 'Occurrence')
;
;	;FGM PSD
;	im_psdh = MrImage( psdh, f, psd_bins, $
;	                   /AXES, $
;	                   /CURRENT, $
;	                   /LOG, $
;	                   /SCALE, $
;	                   NAME        = 'Hist PSD FGM', $
;	                   XTITLE      = 'Frequency', $
;	                   YTITLE      = 'Log10( PSD )')
;	p_psdf = MrPlot( f, psdf, $
;	                 COLOR    = 'White', $
;	                 NAME     = 'Floor PSD FGM', $
;	                 OVERPLOT = im_psdh )
;	p_ampf_lpp = MrPlot( lpp_data.f, MrLog(lpp_data.amp^2), $
;	                     COLOR     = 'Blue', $
;	                     LINESTYLE = 2, $
;	                     OVERPLOT  = im_psdh, $
;	                     PSYM      = 17 )
;	cb_psdh = MrColorbar( ORIENTATION = 1, $
;	                      NAME        = 'CB: Hist PSD FGM', $
;	                      TARGET      = im_psdh, $
;	                      TITLE       = 'Occurrence')
;
;;------------------------------------;
;; Finish                             ;
;;------------------------------------;
;	;Update properties
;;	win -> SetGlobal, XTICKS=8
;	
;	;Refresh the window
;	win -> Refresh
;	
;	;Save the graphic
;	if png_file ne '' then win -> Save, png_file
;
;	return, win
;end