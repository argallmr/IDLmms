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
;        |-------------------------------------------------------------|
;        |                           bitmask                           |
;        |-------------------------------------------------------------|
;        | amb         |   2^0   |   2^3  |      0     |    0    |   9 |
;        | amb-pm2     |   2^0   |   2^4  |      0     |    0    |  17 |
;        | amb-alt-cc  |   2^1   |   2^3  |      0     |    0    |  10 |
;        | amb-alt-oc  |   2^1   |   2^4  |      0     |    0    |  18 |
;        | amb-alt-oom |   2^1   |   2^4  |     2^5    |    0    |  50 |
;        | amb-alt-oob |   2^1   |   2^4  |     2^5    |   2^6   | 114 |
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
;-
function mms_edi_amb_ops_bitmask, edi
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Separate into Operational Modes \\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Do not set identical modes separately. One would unset the other.
	modeBit = bytarr(n_elements(edi.epoch_timetag))
	modeBit = MrBitSet( modeBit, 1, (edi.pitch_mode   eq 0) )
	modeBit = MrBitSet( modeBit, 2, (edi.pitch_mode   eq 1) or (edi.pitch_mode eq 3) ) ;PITCH_MODE 1,3 identical
	modeBit = MrBitSet( modeBit, 3, (edi.pitch_mode   eq 2) )
	modeBit = MrBitSet( modeBit, 4, (edi.pack_mode    eq 0) or (edi.pack_mode eq 1) )  ;PACMO 0,1 identical
	modeBit = MrBitSet( modeBit, 5, (edi.pack_mode    eq 2) )
	modeBit = MrBitSet( modeBit, 6, (edi.perp_oneside eq 1) )
	modeBit = MrBitSet( modeBit, 7, (edi.perp_bidir   eq 1) )

	;Look for counts outside of the packet time range
	iLT = where( edi.epoch_gdu1 lt edi.epoch_timetag[0],  nLT )
	iGT = where( edi.epoch_gdu1 gt edi.epoch_timetag[-1], nGT )
	if nLT gt 0 then MrPrintF, 'LogWarn', nLT, FORMAT='(%"%i points before first packet time.")'
	if nGT gt 0 then MrPrintF, 'LogWarn', nGT, FORMAT='(%"%i points after last packet time.")'
	
	;Expand the bits
	iBits = value_locate(edi.epoch_timetag, edi.epoch_gdu1)

	;Return
	return, modeBit[iBits]
end
