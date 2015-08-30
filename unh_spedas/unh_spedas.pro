;
; NAME:
;   unh_spedas
;
; PURPOSE:
;   Set up SPEDAS in a modified fashion and includes a patch, of sorts.
;   It is intended for local data analysis (making cribs) using spedas.
;   It will never check the SDC for anything (no password required) and
;   grabs all data from the local mirror. Since it cannot access the SDC,
;   it cannot grab the FOM structures and EVA will not work.
;
pro unh_spedas
	;Directories in which SPEDAS and the patch are located.
	spedas_dir = '/argall/home/spedas/'
	patch_dir  = '/home/argall/IDL/MMS/unh_spedas/'
	
	;Directory of the local mirror
	;  - I have read in program notes that the trailing "/" is
	;    required, even on Windows machines.
	local_dir = '/nfs/'

	;Point IDL to the SPEDAS directory
	;   - SPEDAS must come before the normal IDL path.
	!path = expand_path('+' + spedas_dir) + ':' + !path
	
	;Point IDL to the patch
	;   - Must come before SPEDAS to short-circuit SDC functionality
	!path = expand_path('+' + patch_dir) + ':' + !path
	
	;Set the local data directory
	;   - Reset MMS configuration of SPEDAS in case it was already configured
	mms_init, LOCAL_DATA_DIR=local_dir, /RESET
end
