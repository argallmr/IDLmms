; docformat = 'rst'
;
; NAME:
;    unh_mms_load_edi_amb
;
; PURPOSE:
;+
;   Fetch EDI Ambient mode SITL products from the SDC for display using tplot.
;   The routine creates tplot variables based on the names in the mms CDF files.
;   Data files are cached locally in !mms.local_data_dir.
;
; :Categories:
;    MMS, SITL, QL
;
; :Keywords:
;        SC:           in, optional, type=string/strarr, default='mms1'
;                      Array of strings containing spacecraft
;                        ids for http query (e.g. 'mms1' or ['mms1', 'mms3']).
;                        If not used, or set to invalid id, the routine defaults'
;                        to 'mms1'
;        NO_UPDATE:    in, optional, type=boolean, default=0
;                      Set if you don't wish to replace earlier file versions
;                        with the latest version. If not set, earlier versions are deleted
;                        and replaced.
;        RELOAD:       in, optional, type=boolean, default=0
;                      Set if you wish to download all files in query, regardless
;                        of whether file exists locally. Useful if obtaining recent data files
;                        that may not have been full when you last cached them. Cannot
;                        be used with `NO_UPDATE`.
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
;       2015/03/15  -   Written by Matthew Argall
;-
function unh_mms_sitl_open_edi_efield_cdf, filename
	compile_opt idl2
	
	;Read the data file
	cdf_struct = cdf_load_vars(filename, /SPDF_DEPENDENCIES, $
	                           VAR_TYPE = ['data', 'support_data'], $
	                           VARNAMES = varnames)

	;Build a structure
	return, { time:             time_double(*cdf_struct.vars[0].dataptr, /TT2000), $
	          efield:          *cdf_struct.vars[4].dataptr, $
	          efield_vname:     cdf_struct.vars[4].name, $
	          vxb_drift:       *cdf_struct.vars[5].dataptr, $
	          vxb_drift_vname:  cdf_struct.vars[5].name $
	        }
end