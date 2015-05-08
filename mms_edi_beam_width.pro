; docformat = 'rst'
;
; NAME:
;       mms_edi_beam_width
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
;   Calculate the beam width in the plane perpendicular to the magnetic field.
;
;   Method:                    y    /
;                              |   / (x,y)
;                    __________|__*_______                 ___
;                  /           | / alpha   \                b
;           -----(-------------|-------------)------ x     ___
;                  \ _________/|__________ /
;                            / |
;                   b_perp  /  |
;                          /   |------ a  ---|
;
;
;       We start by finding the semi-major and -minor axes of the uncertainty ellipse.
;       These are the phi-hat and theta-hat spherical coordinate system axes at the
;       location of the beam, with r-hat being in the direction of the beam.
;
;       Next, we find the B-perp direction by crossing the firing vector with the
;       magnetic field vector::
;
;             B_perp = v x B
;
;       Finally, we find the beam uncertainty in BPP using the equation of an ellipse and
;       that of distance::
;
;             x^2 / a^2  +  y^2 / b^2 = 1
;             d = sqrt( x^2 + y^2 )
;
;       where a and b are the lengths of the semi-major and semi-minor axes, and
;       (x,y) is the location at which B_perp intersects the edge of the ellipse.
;       Solving the first equation for x^2, we get
;
;             x^2 = ( 1/a^2 + tan(alpha)^2/b^2 )^-1
;
;       where tan(alpha) = y / x, and alpha is the angle from the semi-major axes
;       to B_perp. Then, we not that the beam width, sigma = 2*d, factor out an x^2,
;       and make substitutions
;
;            sigma = 2 * x^2 * sqrt( 1 + y^2 / x^2 )
;            sigma = 2 * x^2 * sqrt( 1 + tan(alpha)^2 )
;            sigma = 2*sqrt( (1+tan(alpha)^2) / $
;                            ( (1/a^2) + (tan(alpha)^2/b^2) ) )
;
; :Categories:
;   MMS, EDI, Bestarg
;
; :Params:
;       FV:             in, required, type=3xN float
;                       Firing vectors in the EDI coordinate system in which they are fired.
;       B:              in, required, type=3xN float
;                       Three-component magnetic field.
;
; :Keywords:
;       DPHI:           in, optional, type=float
;                       Length of the minor axis of the uncertainty ellipse. This is the
;                           beam spread along the polar firing direction.
;       DTHETA:         in, optional, type=float, default=varies linearly from 0.2 - 2
;                       Length of the major axis of the uncertainty ellipse. This is the
;                           beam spread along the azimuthal firing direction.
;
; :Returns:
;       EDI:            Structure of EDI data. Fields are below. If zero beams are
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
;       2015/05/08  -   Written by Matthew Argall
;-
function mms_edi_beam_width, fv, b, $
DPHI=dPhi, $
DTHETA=dTheta
	compile_opt idl2
	on_error, 2
	
	;Number of points
	dims = size(fv, /DIMENSIONS)
	nPts = dims[1]

;-----------------------------------------------------
; Polar and Azimuthal Firing Directions \\\\\\\\\\\\\\
;-----------------------------------------------------

	;Normalize firing vector
	fv_hat = MrVector_Normalize(fv)

	;Azimuth firing angle
	;   - Careful of the poles
	azimuth  = dblarr(nPts)
	iNotPole = where( abs(fv_hat[0,*]) ge 1.0d-8 or abs(fv_hat[1,*]) ge 1.0d-8, nNotPole)
	if nNotPole gt 0 then azimuth[iNotPole] = atan(fv_hat[1, iNotPole], fv_hat[0, iNotPole])

	;Polar firing angle
	polar = acos(fv_hat[2,*])
	
;-----------------------------------------------------
; Semi-Axes of Firing Ellipse \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; The semi-major and -minor axes of the ellipse are named DPHI and DTHETA,
	; respectively. Unless provided explicitly, they are assumed to vary linearly
	; as the beam firing direction is directed downward from the z-axis.
	;
	;            Polar Angle
	;            0.0    |  90.0
	;   --------------------------
	;    dPhi    0.5    |   0.25
	;    dTheta  0.5    |   2.0
	;   
	
	
	;Semi-minor axis
	if n_elements(dPhi) gt 0 $
		then semi_min = replicate(0.5D * dPhi, nPts) $
		else semi_min = 0.5 * ( 1 - polar/!pi )

	;Semi-major axis
	if n_elements(dTheta) gt 0 $
		then semi_maj = replicate(0.5D * dTheta, nPts) $
		else semi_maj = 0.5 * ( 1 + 6.0 * polar / !pi )


	;Direction perpendicular to both B and the firing vector.
	e_perp = MrVector_Cross(fv, b)
	e_perp = MrVector_Normalize(ep)

	;e_theta is a unit vector pointing in the polar direction along the
	;spherical surface connecting z-hat with the firing vector.
	e_theta      =  fltarr(dims)
	e_theta[0,*] =  cos(polar) * cos(azimuth)
	e_theta[1,*] =  cos(polar) * sin(azimuth)
	e_theta[2,*] = -sin(polar)

	;Angle between e_perp and e_theta
	alpha = acos( MrVector_Dot(e_theta, e_perp) )

	;Beam uncertainty width in BPP
	sigma = 2.0 * sqrt( ( 1 + tan(alpha)^2 ) / $
	                    ( (1 / semi_maj^2) + ( tan(alpha)^2 / semi_min^2 ) ) )

	return, sigma
end