; docformat = 'rst'
;
; NAME:
;    unh_mms_load_edi_amb
;
; PURPOSE:
;+
;   This function can be called to determine whether the given netUrl is valid, i.e.,
;   hits a valid LaTiS server with appropriate authentication. Authentication is
;   performed only if the netUrl hostname is 'lasp.colorado.edu'.
;
; :Categories:
;    MMS, SITL, SDC
;
; :Params:
;        NETURL:       in, required, type=object
;                      The IDLnetURL object to be verified.
;
; :Returns:
;        STATUS:       Validation status:
;                          -1                No data
;                          integer > 0       Authentication error code
;                          0                 Passed authentication
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
;       2015/07/25  -   Written by Matthew Argall
;-
function validate_mms_sitl_connection, netUrl

  ;Assume validation will pass.
  status = 0
  netUrl -> GetProperty, URL_HOSTNAME=hostname

  ;If the host is the LASP SDC, then perform extra verification.
  if stregex(!mms.remote_data_dir, 'lasp.colorado.edu', /BOOLEAN) || $
     (hostname eq 'lasp.colorado.edu') $
  then begin
    ;A simple fast call that goes through any front-end proxies
    ;and into LaTiS
    path = "mms/sdc/sitl/latis/dap/properties.txt"
    query = "version"
    
    ;Make the request. Get the text response
    data = execute_mms_sitl_query(netUrl, path, query)

    ;Check if we got an error code instead of string data.
    if size(data, /type) ne 7 then status = data ;return error code

    ;Check that we at least got some text back
    if n_elements(data) lt 1 then status = -1  ;return value implies no data
  endif

  return, status
end
