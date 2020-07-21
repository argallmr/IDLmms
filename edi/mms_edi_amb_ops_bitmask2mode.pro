; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_ops_bitmask2mode
;
; PURPOSE:
;+
;   Convert bitmask value to operating mode.
;
; :Params:
;       BITMASK:        in, required, type=bytarr
;                       A bitmask indicate the operational mode.
;
; :Returns:
;       MODE:           out, str, required
;                       Operating mode corresponding to the bitmask value.
;
; :See Also:
;   mms_edi_amb_ops_bitmask.pro
;   MrBitGet.pro
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
;       2020/05/04  -   Written by Matthew Argall
;-
FUNCTION mms_edi_amb_ops_bitmask2mode, bitmask
	Compile_Opt idl2
	On_Error, 2
	
	mode = StrArr(N_Elements(bitmask))
	FOR i = 0, N_Elements(bitmask) - 1 DO BEGIN
		CASE 1 OF
			Array_Equal( MrBitGet( bitmask[i], [2,5,6,7] ), 1): mode[i] = 'amb-alt-oob'
			Array_Equal( MrBitGet( bitmask[i], [2,5,6] ),   1): mode[i] = 'amb-alt-oom'
			Array_Equal( MrBitGet( bitmask[i], [2,5] ),     1): mode[i] = 'amb-alt-oc'
			Array_Equal( MrBitGet( bitmask[i], [2,4] ),     1): mode[i] = 'amb-alt-cc'
			Array_Equal( MrBitGet( bitmask[i], [1,4] ),     1): mode[i] = 'amb'
			Array_Equal( MrBitGet( bitmask[i], [1,5] ),     1): mode[i] = 'amb-pm2'
			Array_Equal( MrBitGet( bitmask[i], [3,6,7] ),   1): mode[i] = 'amb-perp-ob'
			ELSE: Message, 'Bitmask value unknown: ' + String(bitmask[i], FORMAT='(i0)')
		ENDCASE
	ENDFOR
	
	RETURN, mode
END
