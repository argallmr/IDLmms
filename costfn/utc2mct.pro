; ***************************************************************************
; MMS FIELDS -- UTC2MCT()
;
; Convert a UTC date/time string in ISO format to seconds since
; 1958-01-01t00:00:00 UTC
; 
; Created: April 1, 2015
; Author:  Hans Vaith, University of New Hampshire, Space Science Center
; ***************************************************************************

; ===========================================================================
  FUNCTION UTC2MCT, iso_time_utc
; ===========================================================================
; iso_time_utc is a string or string array of UTC calendar times in ISO format
; ***************************************************************************

    mct_epoch = cdf_parse_tt2000('1958-01-01T00:00:00.000000000')

    year  = fix( strmid(iso_time_utc,  0, 4) )
    month = fix( strmid(iso_time_utc,  5, 2) )
    day   = fix( strmid(iso_time_utc,  8, 2) )
    hh    = fix( strmid(iso_time_utc, 11, 2) )
    mm    = fix( strmid(iso_time_utc, 14, 2) )
    ss    = fix( strmid(iso_time_utc, 17, 2) )
    ms    = fix( strmid(iso_time_utc, 20, 3) )
    us    = fix( strmid(iso_time_utc, 23, 3) )
    ns    = fix( strmid(iso_time_utc, 26, 3) )

    cdf_tt2000, mct_tt2000, year, month, day, hh, mm, ss, $
                            ms, us, ns, /compute_epoch

    mct = (mct_tt2000 - mct_epoch) / 1.d9

    return, mct

END
