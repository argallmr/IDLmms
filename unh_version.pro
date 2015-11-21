function unh_version, file_base

;Created: Mark Chutter, UNH
;2014
;
;Modified:
;
;Arguments:
;file_base - string - fully specified path to where output file will be
;stored after being ingested at SDC, e.g.:
;'/mms/data/mms1/edp/fast/ql/dce/2015/10/mms1_edp_fast_ql_dce_20151016000000_v0.2.'
;
;Description:
;checks for files matching file_base, returns "next" z version 

version_list = findfile(file_base+'*', count = version_count)

if version_count eq 0 then begin

    version_z = '0'

endif else begin

    max_version = -1
    for kk = 0, version_count-1 do begin

        z_kk = strsplit(version_list[kk], '.', /extract)
        if fix(z_kk[2]) gt max_version then max_version = fix(z_kk[2])
    
    endfor
    version_z = strcompress(string(max_version+1), /remove_all)

endelse

return, version_z

end
