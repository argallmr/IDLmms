; ***************************************************************************
; MMS FIELDS -- MMSEDI_GetElfStruct()
;
; Return structure definition for electric field mode data
;
; Created: April 1, 2015
; Author:  Hans Vaith, University of New Hampshire, Space Science Center
; ***************************************************************************

; ===========================================================================
  FUNCTION MMSEDI_GetElfStruct, $
                hdr = hdr, $                    ; telemetry header
                cnv = cnv, $                    ; 'converted' telemetry
                raw = raw, $                    ; 'raw' telemetry
                burst_counts = burst_counts     ; burst counts
; ===========================================================================
; One of the keywords (hdr,cnv,raw,burst_counts) must be set
; ***************************************************************************

    ;
    ; Breakout of electric field mode header word (plus some CCSDS hdr info)
    ;
    elf_hdr_struct = $
    { EDI_ELF_HDR_ST, $
        coarsetime  :   ulong(0),  $ ; coarse time
        finetime    :   ulong(0),  $ ; fine time
        ct          :   0.0d,      $ ; time tag in seconds since  01-Jan-1958
        tt2000      :   long64(0), $ ; time tag (coarse+fine) in tt2000 format
        seqcnt      :   0, $         ; sequence count
        gun1_energy :   0, $         ; gun energy in GDU1 (0=0eV, 1=1keV, 2=500eV, 3=250eV)
        gun2_energy :   0, $         ; gun energy in GDU2 (0=0eV, 1=1keV, 2=500eV, 3=250eV)
        bad_data    :   0, $         ; indicates potentially bad initial data in packet
        pacmode     :   0, $         ; packing mode
        tksim       :   0, $         ; Tracking Simulator on
        optstate    :   0  $         ; Optics State
    }

    ;
    ; Electric Field Mode raw data structure
    ; Survey uses tof1 and dtof, burst uses tof1 and tof2
    ; Survey Packing Mode 1 does not use data14/15 and maxch
    ;
    elf_raw_struct = $
    { EDI_ELF_RAW_ST, $
        coarsetime: ulong(0),   $   ; coarse time in full seconds
        finetime  : ulong(0),   $   ; fine time   in "Fields micro-seconds" (fus)
        pkt_idx  : ulong(0),    $   ; packet index
        st_idx   : 0,           $   ; structure index (0 .. 31)
        vx1      : uint(0),     $   ; offsetted (0x3600) and scaled (/4) analytic voltage of Gun 1
        vy1      : uint(0),     $   ; offsetted (0x3600) and scaled (/4) analytic voltage of Gun 1
        vx2      : uint(0),     $   ; offsetted (0x3600) and scaled (/4) analytic voltage of Gun 2
        vy2      : uint(0),     $   ; offsetted (0x3600) and scaled (/4) analytic voltage of Gun 2
        tof1     : uint(0),     $   ; Detector 1 time of flight in units of correlator shift steps
        tof2     : uint(0),     $   ; Detector 2 time of flight in units of correlator shift steps
        dtof     : 0,           $   ; Time-of-flight difference (tof1-tof2), units as above
        cor1_dv1  : 0,          $   ; Correlator code clock divider DV1
        cor1_dv2  : 0,          $   ; Correlator code clock divider DV2
        cor1_csl  : 0,          $   ; Correlator code length selection
        cor2_dv1  : 0,          $   ; Correlator code clock divider DV1
        cor2_dv2  : 0,          $   ; Correlator code clock divider DV2
        cor2_csl  : 0,          $   ; Correlator code length selection
        q1       : 0,           $   ; Detector 1 quality flag
        q2       : 0,           $   ; Detector 2 quality flag
        maxch1   : 0,           $   ; Detector 1 correlator channel number of peak
        maxch2   : 0,           $   ; Detector 2 correlator channel number of peak
        data14_1 : 0,           $   ; Detector 1 GDU DATA word 14 ('smax')
        data14_2 : 0,           $   ; Detector 2 GDU DATA word 14 ('smax')
        data15_1 : uint(0),     $   ; Detector 1 GDU DATA word 15 ('ALL')
        data15_2 : uint(0),     $   ; Detector 2 GDU DATA word 15 ('ALL')
        bci_idx1 : 0,           $   ; Detector 1 beam pass BCI timing index
        bci_idx2 : 0,           $   ; Detector 2 beam pass BCI timing index
        smp_idx1 : 0,           $   ; Detector 1 beam pass sample timing index
        smp_idx2 : 0            $   ; Detector 2 beam pass sample timing index
    }

    ;
    ; Data structure for converted data; identical for Survey and Burst
    ; Survey PM1 will not populate data14/15 and maxch
    ;
    elf_cnv_struct = $
    { EDI_ELF_CNV_ST,       $
        ct      : double(0),    $   ; absolute time tag in seconds since epoch
                                $   ; (1958-01-01t00:00:00 UTC)
        tt2000  : long64(0),    $   ; time tag in tt2000 format (ns since MJD2000)
        st_idx  : 0,            $   ; structure index (0 .. {15,31,127})
        gd      : 0,            $   ; gun/detector pair identifier (12 or 21)
        vax     : 0,            $   ; analytic voltage (range: +/- 0x3600)
        vay     : 0,            $   ; analytic voltage (range: +/- 0x3600)
        theta   : float(0),     $   ; gun firing direction: polar angle in degrees
        phi     : float(0),     $   ; gun firing direction: azimuth in degrees
        tof     : float(0),     $   ; time of flight in "fus"
        ovfl    : 0,            $   ; time-of-flight overflow flag
        gun_en  : 0,            $   ; energy index
        cor_n   : 0,            $   ; correlator code clock multiplier 'n' (1,2,4,8)
        cor_m   : 0,            $   ; correlator code clock multiplier 'm' (2,4,8,16)
        cor_len : 0,            $   ; correlator code length (number of chips)
        q       : 0,            $   ; quality flag
        maxch   : 0,            $   ; max channel address
        data14  : 0,            $   ; GDU DATA word 14 ('smax')
        data15  : uint(0)       $   ; GDU DATA word 15 ('ALL')
    }

    ;
    ; Electric Field Mode Burst counts data
    ; 1024 compressed samples per sec
    ;
    elf_burst_counts_struct = $
    { EDI_ELF_BURST_COUNTS_ST,      $
        coarsetime       : ulong(0),     $
        finetime         : ulong(0),     $
        ct               : double(0),    $
        tt2000           : long64(0),    $
        gdu1_counts_cmprs: uint(256),    $  ; GDU1 compressed counts from DATA29
        gdu2_counts_cmprs: uint(256),    $  ; GDU2 compressed counts from DATA29
        gdu1_counts      : uint(0),      $  ; GDU1 counts from DATA29
        gdu2_counts      : uint(0)       $  ; GDU2 counts from DATA29
    }

    if      keyword_set(hdr)          then return, elf_hdr_struct $
    else if keyword_set(raw)          then return, elf_raw_struct $
    else if keyword_set(cnv)          then return, elf_cnv_struct $
    else if keyword_set(burst_counts) then return, elf_burst_counts_struct $
    else begin
        message, 'No keyword set or unsupported keyword', /cont
        retall
    endelse

END
