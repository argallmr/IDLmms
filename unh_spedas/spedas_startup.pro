;
; NAME:
;   spedas_startup
;
; PURPOSE:
;   Set up SPEDAS in the normal fashion. Configures your IDL path to use
;   spedas in the normal way. It will download all data to the default
;   local cache and EVA will work with all of its functionality (FOM
;   selection and submission included).
;
pro spedas_startup
	;Directory in which SPEDAS is located.
	spedas_dir = '/argall/home/spedas/'

	;Point IDL to the SPEDAS directory
	;   - SPEDAS must come first in the IDL path.
	!path = expand_path('+' + spedas_dir) + ':' + !path
end
