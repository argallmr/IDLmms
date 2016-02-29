; docformat = 'rst'
;
; NAME:
;       mms_rot_gse2gsm
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Transform a vector in despun spacecraft coordinates to GSE.
;
; :Params:
;       DEFATT:             in, required, type=struct
;                           Structure of attitude data returned by mms_fdoa_read_defatt.pro.
;       TIME:               in, required, type=long64arr (cdf_time_tt2000)
;                           CDF TT2000 epoch time of each vector.
;       V:                  in, required, type=3xN fltarr
;                           Set of 3-vectors in despun coordinates to undergo the
;                               transformation to GSE coordinates.
;
; :Keywords:
;       TYPE:               in, optional, type=string, default='P'
;                           The despun coordinate system in which `V` resides. Options
;                               are: 'P', 'L', 'W', 'Z'
;
; :Returns:
;       V_GSE:              `V` transformed to GSE coordinates.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015-11-27  -   Written by Matthew Argall
;-
function mms_rot_despun2gse, defatt, t, v, $
TYPE=type
	compile_opt idl2
	on_error, 2

	;Load the CoTrans library
	cotrans_lib


	;Get transformation to GEI
	gei2despun = mms_fdoa_xgei2despun(defatt, t, TYPE=type)
	despun2gei = transpose( temporary(gei2despun), [1,0,2] )
	
	;Rotate vector to GEI
	v_gei = MrVector_Rotate(despun2gei, v)

	;Breakdown time
	MrCDF_Epoch_Breakdown, reform(t), yr, mo, day, hr, mnt, sec, milli, micro, nano
	doy = MrDate2DOY(mo, day, yr)
	sec = sec + milli*1d-3 + micro*1d-6 + nano*1d-9
		
	;GEI -->  GSE
	tgeigse_vect, yr, doy, hr, mnt, sec, $
	              reform(v_gei[0,*]), reform(v_gei[1,*]), reform(v_gei[2,*]), $
	              xgse, ygse, zgse

	;Combine GSE components
	v_gse = transpose( [ [temporary(xgse)], [temporary(ygse)], [temporary(zgse)] ] )
	
	return, v_gse
end