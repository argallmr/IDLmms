; docformat = 'rst'
;
; NAME:
;    mms_edi_test_interp
;
; PURPOSE:
;+
;   G
;
; :Categories:
;    MMS, Utility
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
;       2015/11/19  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Array of FGM variable names
;
; :Params:
;       SC:             in, required, type=string
;                       Spacecraft identifier. Options are: "mms1", "mms2", "mms3", "mms4"
;       INSTR:          in, required, type=string
;                       Instrument identifier. Options are: "afg", "dfg"
;       MODE:           in, required, type=string
;                       Data rate mode. Options are: "slow", "fast", "srvy", "brst", "f128"
;       LEVEL:          in, required, type=string
;                       Data level. Options are: "l1a", "l1b", "l2pre"
;
; :Returns:
;       VARS:           String array of variable names.
;-
function mms_cdf_edp_variables, sc, instr, mode, level, optdesc
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; L1B //////////////////////////////////////////////////
;-------------------------------------------------------
	if level eq 'l1b' then begin
	;-------------------------------------------------------
	; DCE & DCE2D //////////////////////////////////////////
	;-------------------------------------------------------
		if optdesc eq 'dce' || optdesc eq 'dce2d' then begin
			;BRST & SRVY & FAST & SLOW
			if mode ne 'brst' && mode ne 'srvy' && mode ne 'slow' && mode eq 'fast' $
				then message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'
			
			vars = [ 'Epoch', $
			         'epoch_timetag', $
			         'delta_t', $
			         'coarse_timetag', $
			         'fine_time', $
			         'seq_cnt', $
			         'samp_per_pkt', $
			         'epoch_crsf', $
			         'crsf_10min', $
			         mms_construct_varname(sc, instr, 'samplerate', optdesc), $
			         mms_construct_varname(sc, instr, 'e12',        'enable'), $
			         mms_construct_varname(sc, instr, 'e34',        'enable'), $
			         mms_construct_varname(sc, instr, 'e56',        'enable'), $
			         mms_construct_varname(sc, instr, 'v1',         'enable'), $
			         mms_construct_varname(sc, instr, 'v2',         'enable'), $
			         mms_construct_varname(sc, instr, 'v3',         'enable'), $
			         mms_construct_varname(sc, instr, 'v4',         'enable'), $
			         mms_construct_varname(sc, instr, 'v5',         'enable'), $
			         mms_construct_varname(sc, instr, 'v6',         'enable'), $
			         mms_construct_varname(sc, instr, 'label',      optdesc), $
			         mms_construct_varname(sc, instr, 'label',      'dcv'), $
			         mms_construct_varname(sc, instr, optdesc,      'sensor'), $
			         mms_construct_varname(sc, instr, 'dcv',        'sensor'), $
			         mms_construct_varname(sc, instr, 'sweep',      'start'), $
			         mms_construct_varname(sc, instr, 'sweep',      'stop'), $
			         mms_construct_varname(sc, instr, 'sweep',      'swept') $
			       ]
		
	;-------------------------------------------------------
	; HFESP ////////////////////////////////////////////////
	;-------------------------------------------------------
		endif else if optdesc eq 'hfesp' then begin
			;SRVY
			if mode ne 'srvy' $
				then message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'
			
			vars = [ 'Epoch', $
			         mms_construct_varname(strupcase(sc), HF, 'freq'), $
			         mms_construct_varname(strupcase(sc), HF, 'EPSD', 'x') $
			       ]
		
	;-------------------------------------------------------
	; SWEEPS ///////////////////////////////////////////////
	;-------------------------------------------------------
		endif else if optdesc eq 'sweeps' then begin
			;SRVY
			if mode ne 'srvy' $
				then message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'
			
			vars = [ 'Epoch', $
			         'epoch_timetag', $
			         'delta_t', $
			         'coarse_timetag', $
			         'epoch_sweepsamp', $
			         'epoch_crsf', $
			         'crsf_10min', $
			         mms_construct_varname(sc, 'sweep', 'memdmpepoch'), $
			         mms_construct_varname(sc, instr, 'label'), $
			         mms_construct_varname(sc, instr, 'sweeps'), $
			         mms_construct_varname(sc, instr, 'samplerate',        'sweeps'), $
			         mms_construct_varname(sc, 'sweep', 'memdmpaddr'), $
			         mms_construct_varname(sc, 'sweep', 'memdmpphase'), $
			         mms_construct_varname(sc, 'sweep', 'za'), $
			         mms_construct_varname(sc, 'sweep', 'spin', 'phase'), $
			         mms_construct_varname(sc, 'sweep', 'table'), $
			         mms_construct_varname(sc, 'spin', 'sync'), $
			         mms_construct_varname(sc, 'sweep', 'table'), $
			         mms_construct_varname(sc, 'sweep', 'steps'), $
			         mms_construct_varname(sc, 'sweep', 'opf'), $
			         mms_construct_varname(sc, 'sweep', 'opb'), $
			         mms_construct_varname(sc, 'sweep', 'sop'), $
			         mms_construct_varname(sc, 'sweep', 'start'), $
			         mms_construct_varname(sc, 'sweep', 'stop'), $
			         mms_construct_varname(sc, 'sweep', 'swept'), $
			         mms_construct_varname(sc, 'sweep', 'repeat'), $
			         mms_construct_varname(sc, 'sweep', 'bias1'), $
			         mms_construct_varname(sc, 'sweep', 'bias2') $
			       ]
		
	;-------------------------------------------------------
	; ACE //////////////////////////////////////////////////
	;-------------------------------------------------------
		endif else if optdesc eq 'ace' then begin
			;BRST
			if mode ne 'brst' $
				then message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'

			vars = [ 'Epoch', $
			         'epoch_timetag', $
			         'delta_t', $
			         'coarse_timetag', $
			         'fine_time', $
			         'seq_cnt', $
			         'samp_per_pkt', $
			         'epoch_crsf', $
			         'crsf_10min', $
			         mms_construct_varname(sc, 'epoch', 'sweep'), $
			         mms_construct_varname(sc, instr, 'samplerate',   optdesc), $
			         mms_construct_varname(sc, instr, 'e12ac',        'enable'), $
			         mms_construct_varname(sc, instr, 'e34ac',        'enable'), $
			         mms_construct_varname(sc, instr, 'e56ac',        'enable'), $
			         mms_construct_varname(sc, instr, 'label'), $
			         mms_construct_varname(sc, instr, 'sweepstatus'), $
			         mms_construct_varname(sc, instr, 'sweep12'), $
			         mms_construct_varname(sc, instr, 'sweep34'), $
			         mms_construct_varname(sc, instr, 'sweep56'), $
			         mms_construct_varname(sc, instr, 'sweep',        'start'), $
			         mms_construct_varname(sc, instr, 'sweep34',      'stop'), $
			         mms_construct_varname(sc, instr, 'sweep34',      'swept') $
			       ]
		
	;-------------------------------------------------------
	; DCV //////////////////////////////////////////////////
	;-------------------------------------------------------
		endif else if optdesc eq 'dcv' then begin
			;BRST
			if mode ne 'brst' $
				then message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'

			vars = [ 'Epoch', $
			         'epoch_timetag', $
			         'delta_t', $
			         'coarse_timetag', $
			         'fine_time', $
			         'seq_cnt', $
			         'samp_per_pkt', $
			         'epoch_crsf', $
			         'crsf_10min', $
			         mms_construct_varname(sc, instr, 'samplerate', optdesc), $
			         mms_construct_varname(sc, instr, 'v1',         'enable'), $
			         mms_construct_varname(sc, instr, 'v2',         'enable'), $
			         mms_construct_varname(sc, instr, 'v3',         'enable'), $
			         mms_construct_varname(sc, instr, 'v4',         'enable'), $
			         mms_construct_varname(sc, instr, 'v5',         'enable'), $
			         mms_construct_varname(sc, instr, 'v6',         'enable'), $
			         mms_construct_varname(sc, instr, 'label'), $
			         mms_construct_varname(sc, instr, optdesc,      'sensor'), $
			         mms_construct_varname(sc, instr, 'sweep',      'start'), $
			         mms_construct_varname(sc, instr, 'sweep34',    'stop'), $
			         mms_construct_varname(sc, instr, 'sweep34',    'swept') $
			       ]
		
	;-------------------------------------------------------
	; HMFE /////////////////////////////////////////////////
	;-------------------------------------------------------
		endif else if optdesc eq 'hmfe' then begin
			;BRST
			if mode ne 'brst' $
				then message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'

			vars = [ 'Epoch', $
			         'epoch_crsf', $
			         'crsf_10min', $
			         mms_construct_varname(sc, instr, 'label'), $
			         mms_construct_varname(sc, instr, 'hmfe', 'sensor') $
			       ]
			       
		endif else begin
			message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'
		endelse
	
;-------------------------------------------------------
; Quick Look ///////////////////////////////////////////
;-------------------------------------------------------
	endif else if level eq 'ql' then begin
		;BRST & FAST & SLOW only
		if mode ne 'brst' && mode ne 'slow' && mode eq 'fast' $
			then message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'
		;DCE & DCE2D only
		if optdesc ne 'dce' && optdesc ne 'dce2d' $
			then message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'
		
		;(BRST & FAST & SLOW) QL (DCE & DCE2D)
		vars = [ 'LABL_1', $
		         mms_construct_varname(sc, instr, 'dce', 'epoch'), $
		         mms_construct_varname(sc, instr, 'dce', 'xyz_dsl'), $
		         mms_construct_varname(sc, instr, 'dce', 'bitmask'), $
		         mms_construct_varname(sc, instr, 'dce', 'quality') $
		       ]
	
;-------------------------------------------------------
; L2 ///////////////////////////////////////////////////
;-------------------------------------------------------
	endif else if level eq 'l2' then begin
		;Spacecraft Potential
		if optdesc eq 'scp' then begin
			if mode ne 'brst' && mode ne 'slow' && mode ne 'fast' $
				then message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'
			
			;(BRST & FAST & SLOW) QL (DCE & DCE2D)
			vars = [ 'LABL_1', $
			         mms_construct_varname(sc, instr, optdesc, 'epoch'), $
			         mms_construct_varname(sc, instr, optdesc), $
			         mms_construct_varname(sc, instr, 'psp'), $
			         mms_construct_varname(sc, instr, 'dcv'), $
			         mms_construct_varname(sc, instr, optdesc, 'bitmask'), $
			         mms_construct_varname(sc, instr, optdesc, 'quality') $
			       ]
		endif else begin
			message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'
		endelse
;-------------------------------------------------------
; NADA /////////////////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		message, 'No files for "' + strjoin([sc, instr, mode, level, optdesc], '_') + '".'
	endelse

	return, vars
end



;+
;   Array of FGM variable names
;
; :Params:
;       SC:             in, required, type=string
;                       Spacecraft identifier. Options are: "mms1", "mms2", "mms3", "mms4"
;       INSTR:          in, required, type=string
;                       Instrument identifier. Options are: "afg", "dfg"
;       MODE:           in, required, type=string
;                       Data rate mode. Options are: "slow", "fast", "srvy", "brst", "f128"
;       LEVEL:          in, required, type=string
;                       Data level. Options are: "l1a", "l1b", "l2pre"
;
; :Returns:
;       VARS:           String array of variable names.
;-
function mms_cdf_fgm_variables, sc, instr, mode, level
	compile_opt idl2
	on_error, 2

	;L1A
	if level eq 'l1a' then begin
		;BRST & FAST & SLOW only
		if mode ne 'brst' && mode ne 'slow' && mode eq 'fast' $
			then message, 'No files for "' + strjoin([sc, instr, mode, level], '_') + '".'
		
		;Array of variable names
		vars = [ 'Epoch', $
		         'epoch_stat', $
		         'delta_t', $
		         'epoch_timetag', $
		         'coarse_timetag', $
		         'fine_timetag', $
		         'epoch_crsf', $
		         'crsf_10min', $
		         mms_construct_varname(sc, instr, 'serecout'), $
		         mms_construct_varname(sc, instr, 'packetcounter'), $
		         mms_construct_varname(sc, instr, 'syncerror'), $
		         mms_construct_varname(sc, instr, 'hmux'), $
		         mms_construct_varname(sc, instr, 'commandcounter'), $
		         mms_construct_varname(sc, instr, 'sfmode'), $
		         mms_construct_varname(sc, instr, 'range'), $
		         mms_construct_varname(sc, instr, 'pfmode'), $
		         mms_construct_varname(sc, instr, 'synced'), $
		         mms_construct_varname(sc, instr, 'extref'), $
		         mms_construct_varname(sc, instr, 'excen'), $
		         mms_construct_varname(sc, instr, 'apid'), $
		         mms_construct_varname(sc, instr, 'delay'), $
		         mms_construct_varname(sc, instr, 'samplerate'), $
		         mms_construct_varname(sc, instr, 'label'), $
		         mms_construct_varname(sc, instr, '123') $
		       ]
		
	;L1B
	endif else if level eq 'l1b' then begin
		;BRST & SRVY
		if mode ne 'brst' && mode ne 'srvy' $
			then message, 'No files for "' + strjoin([sc, instr, mode, level], '_') + '".'
	
		vars = [ 'Epoch', $
		         'label_b_bcs', $
		         'label_b_omb', $
		         mms_construct_varname(sc, instr, mode, 'bcs'), $
		         mms_construct_varname(sc, instr, mode, 'omb'), $
		         mms_construct_varname(sc, instr, mode, level + '_flag'), $
		         mms_construct_varname(sc, instr, mode, 'hirange'), $
		         mms_construct_varname(sc, instr, mode, 'rate'), $
		         mms_construct_varname(sc, instr, mode, 'stemp'), $
		         mms_construct_varname(sc, instr, mode, 'etemp'), $
		         mms_construct_varname(sc, instr, mode, 'l1a_mode') $
		       ]
	
	;QL
	endif else if level eq 'ql' then begin
		;SRVY
		if mode ne 'srvy' $
			then message, 'No files for "' + strjoin([sc, instr, mode, level], '_') + '".'
		
		vars = [ 'Epoch', $
		         'Epoch_state', $
		         'label_b_dmpa', $
		         'label_b_gsm_dmpa', $
		         'label_r_gse', $
		         'label_r_gsm', $
		         'label_RADec_gse', $
		         mms_construct_varname(sc, instr, mode,    'dmpa'), $
		         mms_construct_varname(sc, instr, mode,    'gsm_dmpa'), $
		         mms_construct_varname(sc, level, 'pos',   'gse'), $
		         mms_construct_varname(sc, level, 'pos',   'gsm'), $
		         mms_construct_varname(sc, level, 'RADec', 'gse'), $
		         mms_construct_varname(sc, instr, mode, level + '_flag'), $
		         mms_construct_varname(sc, instr, mode, level + '_status') $
		       ]
	
	;L2PRE
	endif else if level eq 'l2pre' then begin
		;BRST & SRVY
		if mode ne 'brst' && mode ne 'srvy' $
			then message, 'No files for "' + strjoin([sc, instr, mode, level], '_') + '".'
		
		vars = [ 'Epoch', $
		         'Epoch_state', $
		         'label_b_gse', $
		         'label_b_gsm', $
		         'label_b_dmpa', $
		         'label_b_bcs', $
		         'label_r_gse', $
		         'label_r_gsm', $
		         mms_construct_varname(sc, instr, mode, level + '_gse'), $
		         mms_construct_varname(sc, instr, mode, level + '_gsm'), $
		         mms_construct_varname(sc, instr, mode, level + '_dmpa'), $
		         mms_construct_varname(sc, instr, mode, level + '_bcs'), $
		         mms_construct_varname(sc, instr, mode, level + '_flag'), $
		         mms_construct_varname(sc, instr, mode, level + '_hirange'), $
		         mms_construct_varname(sc, instr, mode, level + '_rate'), $
		         mms_construct_varname(sc, instr, mode, level + '_stemp'), $
		         mms_construct_varname(sc, instr, mode, level + '_etemp'), $
		         mms_construct_varname(sc, instr, mode, level + '_l1a_mode'), $
		         mms_construct_varname(sc, 'pos', 'gsm'), $
		         mms_construct_varname(sc, 'pos', 'gse') $
		       ]
	endif else begin
		message, 'No files for "' + strjoin([sc, instr, mode, level], '_') + '".'
	endelse
	
	return, vars
end


;+
;   Return CDF variable names.
;
; :Params:
;       VARNAMES:       in, required, type=string/strarr
;                       Requested variable names. These are the CDF variable names, but
;                           with `SC`, `INSTR`, `MODE`, `LEVEL` stripped from the
;                           beginning of the name.
;       SC:             in, required, type=string
;                       Spacecraft identifier. Options are: "mms1", "mms2", "mms3", "mms4"
;       INSTR:          in, required, type=string
;                       Instrument identifier. Options are: "afg", "dfg"
;       MODE:           in, required, type=string
;                       Data rate mode. Options are: "slow", "fast", "srvy", "brst", "f128"
;       LEVEL:          in, required, type=string
;                       Data level. Options are: "l1a", "l1b", "l2pre"
;       OPTDESC:        in, optional, type=string
;                       Optional variable name descriptor.
;
; :Returns:
;       VARIABLES:      CDF Variable names matching the input parameters.
;-
function mms_cdf_variables, varnames, sc, instr, mode, level, optdesc, $
ALL=all
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(win) then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif


;-----------------------------------------------------
; Gather Variable Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	case instr of
		'afg': vars = mms_cdf_fgm_variables(sc, instr, mode, level)
		'dfg': vars = mms_cdf_fgm_variables(sc, instr, mode, level)
		'edp': vars = mms_cdf_edp_variables(sc, instr, mode, level, optdesc)
		else: message, 'Instrument not recognized: "' + instr + '".'
	endcase
	
	;Return all of the variables?
	if keyword_set(all) then return, vars

;-----------------------------------------------------
; Select Specific Variables \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	nvars = n_elements(varnames)
	variables = strarr(nvars)
	count = 0
	for i = 0, nvars-1 do begin
		;Search for th name
		iname = where( stregex(vars, varname + '$') ne -1, nname)
		
		;Variable not found.
		if nname eq 0 then begin
			MrPrintF, 'LogText', 'Unable to find variable: "' + $
			         strjoin([sc, instr, mode, level, '*'+varname], '_') + '".'
		
		;Too many variables found
		endif else if nname gt 1 then begin
			MrPrintF, 'LogText', 'More than one variable found: ' + $
			          strjoin([sc, instr, mode, level, '*'+varname], '_') + '".'
		
		;Variable found
		endif else begin
			variables[count] = vars[iname]
			count++
		endelse
	endfor
	
	;Trim the variables
	if count eq 0 $
		then message, 'No variables found.' $
		else variables = variables[0:count-1]

	return, variables
end
