; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ops_bitmask
;
; PURPOSE:
;+
;   Turn operational mode flags into a single bitmask.
;
;        |-------------------------------------------------------|
;        |             |  pitch  |  pack  |  perp-     |  perp-  |
;        |             |  mode   |  mode  |  onesided  |  bidir  |
;        |-------------------------------------------------------|
;        | amb         |    0    |   0,1  |     --     |   --    |
;        | amb-pm2     |    0    |    2   |     --     |   --    |
;        | amb-alt-cc  |   1,3   |   0,1  |      0     |    0    |
;        | amb-alt-oc  |   1,3   |    2   |      0     |    0    |
;        | amb-alt-oom |   1,3   |    2   |      1     |    0    |
;        | amb-alt-oob |   1,3   |    2   |      1     |    1    |
;        | amb-perp-ob |    2    |   --   |      1     |    1    |
;        |-------------------------------------------------------------|
;        |                           bitmask                           |
;        |-------------------------------------------------------------|
;        | amb         |   2^0   |   2^3  |      0     |    0    |   9 |
;        | amb-pm2     |   2^0   |   2^4  |      0     |    0    |  17 |
;        | amb-alt-cc  |   2^1   |   2^3  |      0     |    0    |  10 |
;        | amb-alt-oc  |   2^1   |   2^4  |      0     |    0    |  18 |
;        | amb-alt-oom |   2^1   |   2^4  |     2^5    |    0    |  50 |
;        | amb-alt-oob |   2^1   |   2^4  |     2^5    |   2^6   | 114 |
;        | amb-perp-c  |   2^2   |    0   |      0     |    0    |   4 |
;        | amb-perp-om |   2^2   |    0   |     2^5    |    0    |  36 |
;        | amb-perp-ob |   2^2   |    0   |     2^5    |   2^6   | 100 |
;        |-------------------------------------------------------------|
;        |     bit     |    Meaning                                    |
;        |-------------------------------------------------------------|
;        |      1      | Field-aligned                                 |
;        |      2      | Perpendicular                                 |
;        |      3      | Alternating                                   |
;        |      4      | Packing mode 0,1                              |
;        |      5      | Packing mode 2                                |
;        |      6      | One-sided                                     |
;        |      7      | Bi-directional                                |
;        |-------------------------------------------------------------|
;
; :Params:
;       EDI:        in, required, type=struct
;                   Data structure with the following tags:
;                       EPOCH_TIMETAG   - Packet times (TT2000) of EDI ambient data
;                       EPOCH_GDU1      - TT2000 time tags of GDU1 data (assumed to be the same as GDU2)
;                       PITCH_MODE      - Pitch angle mode flag
;                       PACK_MDOE       - One-sided vs. centered for PA=0,180
;                       PERP_ONESIDE    - One-sided vs. centered for PA=90
;                       PERP_BIDER      - Bi-directional for PA=90
;
; :Returns:
;       MODEBIT:    A bitmask indicating the operational mode.
;
; :Author:
;    Matthew Argall::
;        University of New Hampshire
;        Morse Hall Room 348
;        8 College Road
;        Durham, NH 03824
;        matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2016/09/12  -   Written by Matthew Argall
;       2016/09/14  -   Use MrBitSet to set bits. - MRA
;       2020/05/26  -   Flux and packet timestamps have offset and drift. Properly
;                           expand packet times to flux times.
;-
function mms_edi_amb_ops_bitmask, edi
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Separate into Operational Modes \\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Do not set identical modes separately. One would unset the other.
	;   - Ignore PACK_MODE if PITCH_MODE is "perpendicular"
	modeBit = bytarr(n_elements(edi.epoch_timetag))
	modeBit = MrBitSet( modeBit, 1, (edi.pitch_mode   eq 0) )
	modeBit = MrBitSet( modeBit, 2, (edi.pitch_mode   eq 1) or (edi.pitch_mode eq 3) ) ;PITCH_MODE 1,3 identical
	modeBit = MrBitSet( modeBit, 3, (edi.pitch_mode   eq 2) )
	modeBit = MrBitSet( modeBit, 4, (edi.pack_mode    eq 0) or (edi.pack_mode eq 1) )  ;PACMO 0,1 identical
	modeBit = MrBitSet( modeBit, 5, (edi.pack_mode    eq 2) )
	modeBit = MrBitSet( modeBit, 6, (edi.perp_oneside eq 1) )
	modeBit = MrBitSet( modeBit, 7, (edi.perp_bidir   eq 1) )

	;Unset packing mode if in perpendicular mode
	iperp = where(edi.pitch_mode eq 2, nperp)
	if nperp gt 0 then begin
		modeBit[iperp] = MrBitSet(modeBit[iperp], 4, 0)
		modeBit[iperp] = MrBitSet(modeBit[iperp], 5, 0)
	endif
	
	;Hans Vaith: 2020/05/26 1:40 PM
	;------------------------------
	;Here are the time tag formulas for counts/flux time tags with respect to
	;the packet time tag ('epoch_timetag' in L1A speak). Not taken into
	;account in these formulas are small adjustments that come from a fixed
	;correction term determined during I&T and from the FIELDS clock
	;frequency error correction:

	;Time tags relative to packet time in micro-seconds:

	;For survey:  (index - 1) * 31250 + 1e6 * 7/512
	;For burst:   (index - 1.5) * 1e6 / 1024

	;'index' runs from 0 to 127 for survey (128 samples per packet) and from
	;0 to 1024 for burst (1024 samples per packet)

	;With these formulas the first time tag for survey (index=0) and the
	;first two time tags for burst (index=0,1) come out negative with respect
	;to the packet time.
	npackets = n_elements(modeBit)
	nperpacket = median(edi.epoch_gdu1[1:*] - edi.epoch_gdu1)/1e3 < 128 ? 128 : 1024
	iPacket2Count = reform(rebin(lindgen(1, npackets), nperpacket, npackets), nperpacket*npackets)
	
	;Sanity Check: Convert count to packet indices
;	a = mms_edi_amb_ops_bitmask2mode(modeBit)
;	a_iuniq = uniq(a, sort(a))
;	b = mms_edi_amb_ops_bitmask2mode(modeBit[iPacket2Count])
;	b = reform(b, nperpacket, npackets)
;	b_iuniq = uniq(b[0,*], sort(b[0,*]))
;	print, a_iuniq, a[a_iuniq]
;	print, b_iuniq, b[0,b_iuniq]

	;Return
	return, modeBit[iPacket2Count]
end
