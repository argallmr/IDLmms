; docformat = 'rst'
;
; NAME:
;    mms_sitl_open_edi_amb_cdf
;
; PURPOSE:
;+
;   Open an EDI ambient mode data file and return data in the form of a structure.
;
; :Categories:
;    MMS, EDI, SITL, QL
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
;       2015/07/20  -   Written by Matthew Argall
;-
function mms_sitl_open_edi_amb_cdf, filename
	compile_opt idl2
	
	;Read the data file
	cdf_struct = cdf_load_vars(filename, /SPDF_DEPENDENCIES, $
	                           VAR_TYPE = ['data', 'support_data'], $
	                           VARNAMES = varnames)

	;Build a structure
	return, { time:               time_double(*cdf_struct.vars[0].dataptr, /TT2000), $
	          counts_gdu1:       *cdf_struct.vars[4].dataptr, $
	          counts_gdu1_vname:  cdf_struct.vars[4].name, $
	          counts_gdu2:       *cdf_struct.vars[5].dataptr, $
	          counts_gdu2_vname:  cdf_struct.vars[5].name $
	        }
end