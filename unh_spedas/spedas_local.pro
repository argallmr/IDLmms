;
; NAME:
;   spedas_local
;
; PURPOSE:
;   Set up SPEDAS in a modified fashion. Configures your IDL path to use
;   spedas and sets your local cache to the MMS mirror. EVA will work, so
;   long as the data is on the mirror. If EVA has to download something,
;   it not have permission and will crash.
;
pro spedas_local
	;Directory in which SPEDAS is located.
	spedas_dir = '/argall/home/spedas/'
	
	;Directory of the local mirror
	;  - I have read in program notes that the trailing "/" is
	;    required, even on Windows machines.
	local_dir = '/nfs/'

	;Point IDL to the SPEDAS directory
	;   - SPEDAS must come first in the IDL path.
	!path = expand_path('+' + spedas_dir) + ':' + !path
	
	;Set the local data directory
	;   - Reset MMS configuration of SPEDAS in case it was already configured
	mms_init, LOCAL_DATA_DIR=local_dir, /RESET
end
