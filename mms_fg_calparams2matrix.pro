; docformat = 'rst'
;
; NAME:
;    mms_fg_calibrate
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may  be used to endorse or promote products derived from this     ;
;         software without specific prior written permission.                            ;
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
;   Turn fluxgate calibration parameters into an orthogonalization coupling matrix.
;
; :Categories:
;   MMS, DFG, AFG
;
; :Params:
;       G:              in, required, type=3xN float
;                       Gains for each axis.
;       DPHI:           in, required, type=fltarr
;                       Orthogonalization angle.
;       DTHETA:         in, required, type=fltarr
;                       Orthogonalization angle.
;       U3:             in, required, type=struct
;                       Orientation of 3 with respect to OMB.
;
; :Keywords:
;       M:              out, optional, type=3x3xN float
;                       OMG -> 123 transformation matrix
;
; :Returns:
;       SENSOR_MATRIX:  123 -> OMB transformation.
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
;   Modification History::
;       2015-03-25  -   Taken from Ken Brommund.
;       2015-05-03  -   Interchanged rows and columns to use ## operator. - MRA
;-
function mms_fg_calparams2matrix, G, dphi, dtheta, u3, $
M=m
	
	;Convert degrees to radians
	deg2rad = !dpi / 180.0D
	
	;Theta is measured from the y-axis. Measure from X-axis instead.
	theta1 = (90 - dtheta[0,*]) * deg2rad
	theta2 = (90 - dtheta[1,*]) * deg2rad
	phi1   = dphi[0,*] * deg2rad
	phi2   = (90+dphi[1,*]) * deg2rad
	a      = u3[0,*]
	b      = u3[1,*]
	N      = n_elements(G)/3
	m      = dblarr(3,3,N)

	;row 1
	m[0,0,*] = G[0,*]*sin(theta1)*cos(phi1)
	m[1,0,*] = G[0,*]*sin(theta1)*sin(phi1)
	m[2,0,*] = G[0,*]*cos(theta1)

	;row 2
	m[0,1,*] = G[1,*]*sin(theta2)*cos(phi2) 
	m[1,1,*] = G[1,*]*sin(theta2)*sin(phi2)
	m[2,1,*] = G[1,*]*cos(theta2)

	;row 3
	m[0,2,*] = G[2,*]*a
	m[1,2,*] = G[2,*]*b
	m[2,2,*] = G[2,*]*sqrt(1-(a*a + b*b))

	;Invert the matrix
	sensor_matrix = m
	for i = 0, N-1 do begin
		sensor_matrix[*,*,i] = la_invert(m[*,*,i])
	endfor

	return, sensor_matrix
end



;----------------------------------------------------------------
; Main-level example program: IDL> .r mms_fg_calparams2matrix ///
;----------------------------------------------------------------

;Sample data
G      = [0.965819,     0.998249,   0.993915]
dPhi   = [-0.0760805,  -0.0660412]
dTheta = [-0.241677,   -0.0810649]
u3     = [-0.00187507,  0.00423245]
sensor_matrix = mms_fg_calparams2matrix(G, dPhi, dTheta, u3, M=m)

;Print results
print, FORMAT='(%"         [ %7.4f, %7.4f, %7.4f ]")', sensor_matrix[*,0]
print, FORMAT='(%"sensor = [ %7.4f, %7.4f, %7.4f ]")', sensor_matrix[*,1]
print, FORMAT='(%"         [ %7.4f, %7.4f, %7.4f ]")', sensor_matrix[*,2]

print, FORMAT='(%"    [ %7.4f, %7.4f, %7.4f ]")', m[*,0]
print, FORMAT='(%"m = [ %7.4f, %7.4f, %7.4f ]")', m[*,1]
print, FORMAT='(%"    [ %7.4f, %7.4f, %7.4f ]")', m[*,2]


end
