; docformat = 'rst'
;
; NAME:
;       MMS_CONSTRUCT_FILENAME
;
; PURPOSE:
;+
;       The purpose of this program is to create a Cluster CDF file name using the user
;       input information::
;
;       scId_instrumentId_mode_dataLevel_optionalDataProductDescriptor_startTime_vX.Y.Z.cdf
;
; :Categories:
;   MMS
;
; :Examples:
;   See the example program at the end of this file::
;       IDL> .r mms_construct_filename
;
; :Params:
;       SPACECRAFT:     out, optional, type=string
;                       The spacecraft ID. Possible values are::
;                           'mms1'
;                           'mms2'
;                           'mms3'
;                           'mms4'
;       INSTRUMENT:     out, optional, type=string
;                       Instrument ID. Possible values are::
;                           'hpca'              'edi'
;                           'aspoc'             'adp'
;                           'epd'               'sdp'
;                           'epd-eis'           'adp-sdp'
;                           'epd-feeps'         'afg'
;                           'fpi'               'dfg'
;                           'des'               'afg-dfg'
;                           'dis'               'scm'
;                           'des-dis'           'fields'
;       MODE:           out, optional, type=string
;                       Data capture mode. Possible values are::
;                           'fast'
;                           'slow'
;                           'brst'
;                           'srvy'
;       LEVEL:          out, optional, type=string
;                       Level of data production. Possible values are::
;                           'l1a'
;                           'l1b'
;                           'l2'
;                           'ql'
;                           'l2pre'
;                           'l2plus'
;
; :Keywords:
;       OPTDESC:        in, optional, type=string, default=''
;                       Optional field that may not be needed for all
;                           products (e.g. Quicklook and SITL).  Where it is used, 
;                           identifiers should be short (e.g. 3-8 character)
;                           descriptors that are helpful to end-users.  If a descriptor
;                           contains multiple components, hyphens are used to separate
;                           those components.
;       TOKENS:         in, optional, type=boolean, default=0
;                       If set, then the default value of `START_TIME` is '%Y%M%d'.
;                           This pattern is recognized by MrTokens and can be used
;                           to find files with year, month, and day in the file name.
;       TSTART:         in, optional, type=string, default='*'
;                       Start time of the data interval formatted as yyyy-mm-ddThh:mm:ss,
;                           with irrelevant, least significant, fields dropped
;                           when files start on regular hourly or minute boundaries.
;       VERSION:        in, optional, type=string, default='*'
;                       Version of the data file, formatted as 'vX.Y.Z'
;                           X - Interface number.  Increments in this number represent a
;                                   significant change to the processing software and/or to the contents of the 
;                                   file. These changes will likely break existing code that expects a specific 
;                                   file format (e.g. file reading software).  Additionally, increments in this 
;                                   number may require code changes to analysis software that expects the 
;                                   data to have been created using specific processing algorithms. The user 
;                                   should consult the appropriate meta-data for or changelogs.
;                           Y - Quality number. This number represents a change in the quality of
;                                   the data in the file, such as change in calibration or increase in fidelity. 
;                                   Changes should not impact software, but may require consideration when 
;                                   processing data.
;                           Z - Bug fix/revision number. This number changes to indicate minor
;                                   changes to the contents of the file due to reprocessing of missing data.  
;                                   Any dependent data products should generally be reprocessed if this value 
;                                   changes.
;
; :Returns:
;       FILENAME:       The MMS file name.
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/02/06  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;
;-
function mms_construct_filename_dir, root, sc, instr, mode, level, optdesc, subdir
	compile_opt idl2
	on_error, 2

	;Append the SDC directory structure to the file name.
	dirname = filepath('', $
	                   ROOT_DIR     = root,      $
	                   SUBDIRECTORY = [ sc,      $
	                                    instr,   $
	                                    mode,    $
	                                    level,   $
	                                    optdesc  $
	                                  ]          $
	                  )
	dirname = filepath('', ROOT_DIR=dirname, SUBDIRECTORY=subdir)
	
	return, dirname
end


;+
;
;-
function mms_construct_filename_join, sc, instr, mode, level, optdesc, tstart, version
	compile_opt idl2
	on_error, 2

	;Build the file name
	filename = sc         + '_' + $
	           instr      + '_' + $
	           mode       + '_' + $
	           level      + '_' + $
	           optdesc          + $
	           tstart     + '_' + $
	           'v' + version    + $
	           '.cdf'
	
	return, filename
end


;+
;
;-
function mms_construct_filename, sc, instr, mode, level, $
TSTART=tstart, $
OPTDESC=optdesc, $
DIRECTORY=directory, $
SDC_ROOT=sdc_root, $
;SUBDIR=subdir, $
;TOKENS=tokens, $
UNIFORM=uniform, $
VERSION=version
	compile_opt strictarr
;	on_error, 2
	
	
	;if no directory was supplied, get the current directory
	;if no base was chosen, go with the complete base
	tf_uniform = keyword_set(uniform)
;	tokens     = keyword_set(tokens)
	if n_elements(sdc_root)  eq 0 then sdc_root  = ''
	if n_elements(directory) eq 0 then directory = ''
	
	;Conflicts
	if sdc_root ne '' && directory ne '' then $
		message, 'SDC_ROOT and DIRECTORY are mutually exclusive.'
	
	nSC      = n_elements(sc)
	nInstr   = n_elements(instr)
	nMode    = n_elements(mode)
	nLevel   = n_elements(level)
	nDesc    = n_elements(optdesc)
	nTStart  = n_elements(tstart)
	nVersion = n_elements(version)
	nnn      = [nSC, nInstr, nMode, nLevel, nDesc, nTStart, nVersion]
	nmax     = max(nnn)
	
	;Avoid loops if possible
	if total( (nnn eq 1) + (nnn eq nmax) ) eq n_elements(nnn) $
		then tf_uniform = 1
	
;-----------------------------------------------------
; Uniform Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_uniform then begin
		;No start time given
		if nTStart eq 0 then begin
			;BRST
			;   - All files have: year month day hour minute second
			if mode eq 'brst' then begin
				tstart = '%Y%M%d%H%m%S'
			
			;SRVY
			;   - Most files have year month day
			;   - FPI also has hour minute second
			;   - EDP also has hour minute second, but only versions below L2
			endif else begin
				case instr of
					'fpi': tstart = '%Y%M%d%H%m%S'
					'fsm': begin
						if stregex(optdesc, '^cal-(afg|dfg|scm)$', /FOLD_CASE, /BOOLEAN) $
							then tstart = '%Y%M%d%H%m%S' $
							else tstart = (mode eq 'brst') ? '%Y%M%d%H%m%S' : '%Y%M%d'
					endcase
					'edp': begin
						if optdesc eq 'dce' $
							then tstart = (level eq 'l2') ? '%Y%M%d' : '%Y%M%d%H%m%S' $
							else tstart = '%Y%M%d%H%m%S'
					endcase
					else: tstart = '%Y%M%d'
				endcase
			endelse
			nTStart = 1
		endif

		_sc    = nSC    eq nmax ? sc    : replicate(sc,    nmax)
		_instr = nInstr eq nmax ? instr : replicate(instr, nmax)
		_mode  = nMode  eq nmax ? mode  : replicate(mode,  nmax)
		_level = nLevel eq nmax ? level : replicate(level, nmax)
		
		;TStart defined?
		_tstart = nTStart eq 0 ? '%Y%M%d' : tstart
		_tstart = n_elements(_tstart) eq nmax ? _tstart : replicate(_tstart, nmax)
		ibrst   = where(mode eq 'brst', nbrst)
		if nbrst gt 0 then _tstart[ibrst] = '%Y%M%d%H%m%S'

		;Optional descriptor
		dirdesc = nDesc                gt 0   ? optdesc : replicate('', nmax)
		dirdesc = n_elements(dirdesc) eq nMax ? dirdesc : replicate(dirdesc, nmax)
		fdesc   = dirdesc
		iopt = where(fdesc ne '', nopt)
		if nopt gt 0 then fdesc[iopt] += '_'
		
		;Version
		if nVersion eq 0 $
			then _version = replicate('*', nmax) $
			else _version = nVersion eq nmax ? version : replicate(version, nmax)
		
		;Subdirs are different for burst data.
		if directory eq '' $
			then subdir = mode[0] eq 'brst' ? ['%Y', '%M', '%d'] : ['%Y', '%M'] $
			else subdir  = ''

		;Create the file name(s)
		fname = mms_construct_filename_join(_sc, _instr, _mode, _level, fdesc, _tstart, _version)
		
		;Create the directory
		if directory ne '' then begin
			for i = 0, nmax-1 do fname[i] = filepath(fname[i], ROOT_DIR=directory)
		endif else begin
			dir = strarr(nmax)
			for i=0, nmax-1 do begin
				dir = mms_construct_filename_dir(sdc_root, _sc[i], _instr[i], _mode[i], $
				                                 _level[i], dirdesc[i], subdir)
				fname[i] = filepath(fname[i], ROOT_DIR=dir)
			endfor
		endelse

;-----------------------------------------------------
; Non-Uniform Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Allocate memory to output
		fname = strarr(nSC*nInstr*nMode*nLevel*nDesc)
		n     = 0
		
		;Is TSTART defined?
		if nTStart  eq 0 then tstart  = ''
		if nDesc    eq 0 then optdesc = ''
		if nVersion eq 0 then version = '*'
	
		;Create the MMS filename
		;   - Must use loops because:
		;   - Replicate does not work with arrays.
		;   - Rebin does not work with strings.
		for i = 0, nSC    - 1 do $
		for j = 0, nInstr - 1 do $
		for k = 0, nMode  - 1 do $
		for l = 0, nLevel - 1 do $
		for m = 0, nDesc  - 1 do begin
	
			;Start time
			if tstart[0] eq '' then begin
				_tstart = mode[k] eq 'brst' ? '%Y%M%d%H%m%S' : '%Y%M%d'
			endif else begin
				_tstart = strmid(tstart, 0, 4) + strmid(tstart, 5, 2) + strmid(tstart, 8, 2)
				if mode[k] eq 'brst' then _tstart += strmid(tstart, 11, 2) + strmid(tstart, 14, 2) + strmid(tstart, 17, 2)
			endelse

			fdesc = optdesc[m] eq '' ? '' : optdesc[m] + '_'

			;Subdirs are different for burst data.
			if n_elements(subdir) eq 0 $
				then _subdir = mode[0] eq 'brst' ? ['%Y', '%M', '%d'] : ['%Y', '%M'] $
				else _subdir = subdir
			
			;Filename
			fname[n] = mms_construct_filename_join(sc[i], instr[j], mode[k], level[l], fdesc, $
			                                       _tstart, version)
			
			;Directory
			if directory eq '' $
				then dir = mms_construct_filename_dir(dir, root, sc[i], instr[j], mode[k], level[l], optdesc[m], _subdir) $
				else dir = directory
			
			;Append the directory name
			if dir ne '' then fname[n] = filepath(fname[n], ROOT_DIR=dir)
			
			;Number of files
			n += 1
		endfor ;Loop over m
	endelse
	
	;Return a scalar
	if n_elements(fname) eq 1 then fname = fname[0]
	return, fname
end

;MAIN level program to see how mms_construct_file.pro works.
;To test,
;      IDL> .run mms_construct_file
;sc         = 1
;instrument = 'afg'
;mode       = 'srvy'
;level      = 'l2pre'
;optdesc    = 'duration-1h1m'
;start_time = '20150313'
;version    = '1.1.2'
;
;filename = mms_construct_filename(sc, instrument, mode, level, OPTDESC=optdesc, $
;                                  TSTART=start_time, VERSION=version)
;print, filename
;end