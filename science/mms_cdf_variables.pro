; docformat = 'rst'
;
; NAME:
;    mms_cdf_variables
;
; PURPOSE:
;+
;   Gather MMS variable names for a specific data product.
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
;       2015/11/21  -   Added various L1A/QL/L2 EDI varaibles. Fixed bugs. - MRA
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
;       OPTDESC:        in, required, type=string
;                       Optional variable name descriptor.
;
; :Returns:
;       VARS:           String array of variable names.
;-
function mms_cdf_edi_variables, sc, instr, mode, level, optdesc
	compile_opt idl2
	on_error, 2

	;Type of data requested
	data = strjoin([sc, instr, mode, level, optdesc], '_')

;-------------------------------------------------------
; Ambient Mode /////////////////////////////////////////
;-------------------------------------------------------
	if optdesc eq 'amb' then begin
	;-------------------------------------------------------
	; L1A Amb //////////////////////////////////////////////
	;-------------------------------------------------------
		if level eq 'l1a' then begin
			;SLOW & FAST
			if mode eq 'slow' || mode eq 'fast' then begin
				vars = [ 'Epoch', $
				         'epoch_angle', $
				         'epoch_timetag', $
				         'delta_t', $
				         mms_construct_varname(sc, instr, optdesc, 'gdu1_raw_counts1'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu2_raw_counts1'), $
				         mms_construct_varname(sc, instr, optdesc, 'phi'), $
				         mms_construct_varname(sc, instr, optdesc, 'theta'), $
				         mms_construct_varname(sc, instr, 'pitch', 'gdu1'), $
				         mms_construct_varname(sc, instr, 'pitch', 'gdu2'), $
				         mms_construct_varname(sc, instr, optdesc, 'dwell'), $
				         mms_construct_varname(sc, instr, optdesc, 'energy1'), $
				         mms_construct_varname(sc, instr, optdesc, 'energy2'), $
				         mms_construct_varname(sc, instr, optdesc, 'pitchmode'), $
				         mms_construct_varname(sc, instr, optdesc, 'pacmo'), $
				         mms_construct_varname(sc, instr, optdesc, 'optics'), $
				         'epoch_crsf', $
				         'crsf_10min' $
				       ]
			;SRVY
			endif else if mode eq 'srvy' then begin
				vars = [ 'Epoch', $
				         'epoch_angle', $
				         'epoch_timetag', $
				         'delta_t', $
				         mms_construct_varname(sc, instr, optdesc, 'gdu1_raw_counts1'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu2_raw_counts1'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu1_raw_counts2'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu2_raw_counts2'), $
				         mms_construct_varname(sc, instr, optdesc, 'phi'), $
				         mms_construct_varname(sc, instr, optdesc, 'theta'), $
				         mms_construct_varname(sc, instr, optdesc, 'dwell'), $
				         mms_construct_varname(sc, instr, optdesc, 'energy1'), $
				         mms_construct_varname(sc, instr, optdesc, 'energy2'), $
				         mms_construct_varname(sc, instr, optdesc, 'pitchmode'), $
				         mms_construct_varname(sc, instr, optdesc, 'pacmo'), $
				         mms_construct_varname(sc, instr, optdesc, 'optics'), $
				         'epoch_crsf', $
				         'crsf_10min' $
				       ]
			;BRST
			endif else if mode eq 'brst' then begin
				vars = [ 'Epoch', $
				         'epoch_angle', $
				         'epoch_timetag', $
				         'delta_t', $
				         mms_construct_varname(sc, instr, optdesc, 'gdu1_raw_counts1'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu2_raw_counts1'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu1_raw_counts2'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu2_raw_counts2'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu1_raw_counts3'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu2_raw_counts3'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu1_raw_counts4'), $
				         mms_construct_varname(sc, instr, optdesc, 'gdu2_raw_counts4'), $
				         mms_construct_varname(sc, instr, optdesc, 'phi'), $
				         mms_construct_varname(sc, instr, optdesc, 'theta'), $
				         mms_construct_varname(sc, instr, 'pitch', 'gdu1'), $
				         mms_construct_varname(sc, instr, 'pitch', 'gdu2'), $
				         mms_construct_varname(sc, instr, optdesc, 'dwell'), $
				         mms_construct_varname(sc, instr, optdesc, 'energy1'), $
				         mms_construct_varname(sc, instr, optdesc, 'energy2'), $
				         mms_construct_varname(sc, instr, optdesc, 'pitchmode'), $
				         mms_construct_varname(sc, instr, optdesc, 'pacmo'), $
				         mms_construct_varname(sc, instr, optdesc, 'optics'), $
				         'epoch_crsf', $
				         'crsf_10min' $
				       ]
			endif else begin
				message, 'No files for "' + data + '".'
			endelse
	;-------------------------------------------------------
	; QL Amb ///////////////////////////////////////////////
	;-------------------------------------------------------
		endif else if mode eq 'ql' then begin
			if mode eq 'srvy' then begin
				message, 'No files for "' + data + '".'
			endif else if mode eq 'srvy' then begin
				message, 'No files for "' + data + '".'
			endif else begin
				message, 'No files for "' + data + '".'
			endelse
	;-------------------------------------------------------
	; L2 Amb ///////////////////////////////////////////////
	;-------------------------------------------------------
		endif else if mode eq 'l2' then begin
			;SRVY
			if mode eq 'srvy' then begin
				vars = [ 'epoch_gd12', $
				         'epoch_gd21', $
				         mms_construct_varname(sc, instr, energy, 'gd12'), $
				         mms_construct_varname(sc, instr, energy, 'gd21'), $
				         mms_construct_varname(sc, instr, counts, 'gd12'), $
				         mms_construct_varname(sc, instr, counts, 'gd21') $
				       ]
			;BRST
			endif else if mode eq 'brst' then begin
				message, 'No files for "' + data + '".'
			endif else begin
				message, 'No files for "' + data + '".'
			endelse
	;-------------------------------------------------------
	; Invalid Level ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			message, 'No files for "' + data + '".'
		endelse
		
;-------------------------------------------------------
; E-Field Mode /////////////////////////////////////////
;-------------------------------------------------------
	endif else if optdesc eq 'efield' then begin
	;-------------------------------------------------------
	; L1A EField ///////////////////////////////////////////
	;-------------------------------------------------------
		if level eq 'l1a' then begin
			;SLOW & FAST
			if mode eq 'slow' || mode eq 'fast' then begin
				vars = [ 'Epoch', $
				         'epoch_time_tag', $
				         'delta_t', $
				         mms_construct_varname(sc, instr, 'optics'), $
				         'Epoch_beam_gd12', $
				         'Epoch_beam_gd21', $
				         mms_construct_varname(sc, instr, 'tof1',     'us'), $
				         mms_construct_varname(sc, instr, 'tof2',     'us'), $
				         mms_construct_varname(sc, instr, 'vax',      'gd12'), $
				         mms_construct_varname(sc, instr, 'vax',      'gd21'), $
				         mms_construct_varname(sc, instr, 'vay',      'gd12'), $
				         mms_construct_varname(sc, instr, 'vay',      'gd21'), $
				         mms_construct_varname(sc, instr, 'theta',    'gd12'), $
				         mms_construct_varname(sc, instr, 'theta',    'gd21'), $
				         mms_construct_varname(sc, instr, 'phi',      'gd12'), $
				         mms_construct_varname(sc, instr, 'phi',      'gd21'), $
				         mms_construct_varname(sc, instr, 'word14',   'gd12'), $
				         mms_construct_varname(sc, instr, 'word14',   'gd21'), $
				         mms_construct_varname(sc, instr, 'word15',   'gd12'), $
				         mms_construct_varname(sc, instr, 'word15',   'gd21'), $
				         mms_construct_varname(sc, instr, 'numchips', 'gd12'), $
				         mms_construct_varname(sc, instr, 'numchips', 'gd21'), $
				         mms_construct_varname(sc, instr, 'e',        'gd12'), $
				         mms_construct_varname(sc, instr, 'e',        'gd21'), $
				         mms_construct_varname(sc, instr, 'sq',       'gd12'), $
				         mms_construct_varname(sc, instr, 'sq',       'gd21'), $
				         mms_construct_varname(sc, instr, 'm',        'gd12'), $
				         mms_construct_varname(sc, instr, 'm',        'gd21'), $
				         mms_construct_varname(sc, instr, 'n',        'gd12'), $
				         mms_construct_varname(sc, instr, 'n',        'gd21'), $
				         mms_construct_varname(sc, instr, 'max_addr', 'gd12'), $
				         mms_construct_varname(sc, instr, 'max_addr', 'gd21'), $
				         'epoch_crsf', $
				         'crsf_10min' $
				       ]
			;BRST
			endif else if mode eq 'brst' then begin
				vars = [ 'Epoch', $
				         'epoch_time_tag', $
				         'epoch_time_tag', $
				         mms_construct_varname(sc, instr, 'optics'), $
				         'Epoch_beam_gd12', $
				         'Epoch_beam_gd21', $
				         'Epoch_data29', $
				         mms_construct_varname(sc, instr, 'tof1',     'us'), $
				         mms_construct_varname(sc, instr, 'tof2',     'us'), $
				         mms_construct_varname(sc, instr, 'data29',   'gd12'), $
				         mms_construct_varname(sc, instr, 'data29',   'gd21'), $
				         mms_construct_varname(sc, instr, 'vax',      'gd12'), $
				         mms_construct_varname(sc, instr, 'vax',      'gd21'), $
				         mms_construct_varname(sc, instr, 'vay',      'gd12'), $
				         mms_construct_varname(sc, instr, 'vay',      'gd21'), $
				         mms_construct_varname(sc, instr, 'theta',    'gd12'), $
				         mms_construct_varname(sc, instr, 'theta',    'gd21'), $
				         mms_construct_varname(sc, instr, 'phi',      'gd12'), $
				         mms_construct_varname(sc, instr, 'phi',      'gd21'), $
				         mms_construct_varname(sc, instr, 'word14',   'gd12'), $
				         mms_construct_varname(sc, instr, 'word14',   'gd21'), $
				         mms_construct_varname(sc, instr, 'word15',   'gd12'), $
				         mms_construct_varname(sc, instr, 'word15',   'gd21'), $
				         mms_construct_varname(sc, instr, 'numchips', 'gd12'), $
				         mms_construct_varname(sc, instr, 'numchips', 'gd21'), $
				         mms_construct_varname(sc, instr, 'e',        'gd12'), $
				         mms_construct_varname(sc, instr, 'e',        'gd21'), $
				         mms_construct_varname(sc, instr, 'sq',       'gd12'), $
				         mms_construct_varname(sc, instr, 'sq',       'gd21'), $
				         mms_construct_varname(sc, instr, 'm',        'gd12'), $
				         mms_construct_varname(sc, instr, 'm',        'gd21'), $
				         mms_construct_varname(sc, instr, 'n',        'gd12'), $
				         mms_construct_varname(sc, instr, 'n',        'gd21'), $
				         mms_construct_varname(sc, instr, 'max_addr', 'gd12'), $
				         mms_construct_varname(sc, instr, 'max_addr', 'gd21'), $
				         'epoch_crsf', $
				         'crsf_10min' $
				       ]
			endif else begin
				message, 'No files for "' + data + '".'
			endelse
		
	;-------------------------------------------------------
	; QL EField ////////////////////////////////////////////
	;-------------------------------------------------------
		endif else if level eq 'ql' then begin
			message, 'No files for "' + data + '".'
	
	;-------------------------------------------------------
	; L2 EField ////////////////////////////////////////////
	;-------------------------------------------------------
		endif else if level eq 'l2' then begin
			message, 'No files for "' + data + '".'
		endif else begin
			message, 'No files for "' + data + '".'
		endelse
		
;-------------------------------------------------------
; Q0 ///////////////////////////////////////////////////
;-------------------------------------------------------
	endif else if optdesc eq 'q0' then begin
	;-------------------------------------------------------
	; L2 Q0 ////////////////////////////////////////////////
	;-------------------------------------------------------
		if level eq 'l2' then begin
			if mode eq 'srvy' then begin
				vars = [ 'epoch_gd12', $
				         'epoch_gd21', $
				         mms_construct_varname(sc, instr, energy, 'gd12'), $
				         mms_construct_varname(sc, instr, energy, 'gd21'), $
				         mms_construct_varname(sc, instr, counts, 'gd12'), $
				         mms_construct_varname(sc, instr, counts, 'gd21') $
				       ]
			endif else if mode eq 'brst' then begin
				message, 'No files for "' + data + '".'
			endif else begin
				message, 'No files for "' + data + '".'
			endelse
	;-------------------------------------------------------
	; Invalid Level ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			message, 'No files for "' + data + '".'
		endelse
;-------------------------------------------------------
; Invalid OptDesc //////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		message, 'No files for "' + data + '".'
	endelse
	
	return, vars
end


;+
;   Array of EDP variable names
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
;       OPTDESC:        in, required, type=string
;                       Optional variable name descriptor.
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
; :Examples:
;   Get all of the variable names for a given data product::
;       IDL> mms_cdf_variables('', 'mms1', 'dfg', 'srvy', 'l2pre', /ALL)
;           Epoch
;           Epoch_state
;           label_b_gse
;           label_b_gsm
;           label_b_dmpa
;           label_b_bcs
;           label_r_gse
;           label_r_gsm
;           mms1_dfg_srvy_l2pre_gse
;           mms1_dfg_srvy_l2pre_gsm
;           mms1_dfg_srvy_l2pre_dmpa
;           mms1_dfg_srvy_l2pre_bcs
;           mms1_dfg_srvy_l2pre_flag
;           mms1_dfg_srvy_l2pre_hirange
;           mms1_dfg_srvy_l2pre_rate
;           mms1_dfg_srvy_l2pre_stemp
;           mms1_dfg_srvy_l2pre_etemp
;           mms1_dfg_srvy_l2pre_l1a_mode
;           mms1_pos_gsm
;           mms1_pos_gse
;   Get the magnetic field in GSE coordinates::
;       IDL> mms_cdf_variables('gse', 'mms1', 'dfg', 'srvy', 'l2pre')
;           mms1_dfg_srvy_l2pre_gse
;
; :Params:
;       VARNAMES:       in, required, type=string/strarr
;                       Requested variable names. These are the CDF variable names, but
;                           with `SC`, `INSTR`, `MODE`, `LEVEL` stripped from the
;                           beginning of the name (e.g. the variable suffix).
;       SC:             in, required, type=string
;                       Spacecraft identifier.
;       INSTR:          in, required, type=string
;                       Instrument identifier.
;       MODE:           in, required, type=string
;                       Data rate mode.
;       LEVEL:          in, required, type=string
;                       Data level.
;       OPTDESC:        in, optional, type=string
;                       Optional variable name descriptor.
;
; :Keywords:
;       ALL:            in, optional, type=boolean, default=0
;                       If set, all variable names for the given inputs are returned.
;       SUFFIX:         in, optional, type=boolean, default=0
;                       Return the suffixes (i.e. the values accepted by `VARNAMES`)
;                           for the given inputs. Ignored if `ALL` is set.
;
; :Returns:
;       VARIABLES:      CDF Variable names matching the input parameters.
;-
function mms_cdf_variables, varnames, sc, instr, mode, level, optdesc, $
ALL=all, $
SUFFIX=tf_suffix
	compile_opt idl2
	on_error, 2

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
; Variable Suffixes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find the suffixes
	;   - The suffix is anything not containing the generic input information
	;   - If no suffix is found, select the entire variable
	prefix = strjoin([sc, instr, mode, level], '|')
	parts  = stregex(vars, '^((' + prefix + ')_)+(.*)$', /SUBEXP, /EXTRACT)
	suffix = reform(parts[3,*])
	
	;If no suffix was found, select the entire variable
	iNoSuffix = where(parts[0,*] eq '', nNoSuffix)
	if nNoSuffix gt 0 then suffix[iNoSuffix] = vars[iNoSuffix]
	
	;Return the suffixes?
	if keyword_set(tf_suffix) then return, suffix

;-----------------------------------------------------
; Select Specific Variables \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	nvars     = n_elements(varnames)
	variables = strarr(nvars)
	count     = 0

	;Loop through each variable to look for matches
	for i = 0, nvars-1 do begin
		;Search for the name
		iname = where( stregex(suffix, '^' + varnames[i] + '$') ne -1, nname)
		if nname eq 0 then iname = where( stregex(vars, varnames[i] + '$') ne -1, nname)
		
		;Variable not found.
		if nname eq 0 then begin
			MrPrintF, 'LogText', 'Unable to find variable: "' + varnames[i] + '".'
		
		;Too many variables found
		endif else if nname gt 1 then begin
			MrPrintF, 'LogText', 'More than one variable found matching: "' + varnames[i] + '".'
		
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
