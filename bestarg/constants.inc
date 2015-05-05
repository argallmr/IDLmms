; ============================================================================
; constants.inc
; ============================================================================

; 2*pi*m / e
BTG_CONV = 3.5723875d4 ; convert B in nT to Tg in us, and vice versa


VE_1KEV  = 18727.9 ; km/s

; using c = 2.99793d8
;       me = 9.1093897d-31
;       e  = 1.60217733d-19
;       Ek = e * 1.e3 ( 1kev kinetic energy)
;
;                             me       ^2
; and v = c * SQRT [ 1 - (------------)   ]
;                          me + Ek/c^2

; Constants -------------------------------------
; c      =  2.99792458e+08;  % speed of light (SI)
; deg2rad=  pi / 180.0;
; e_mass =  9.10938188e-31;  % electron mass, kg (SI)
; halfPi   = pi / 2.0;
; q        = 1.602177e-19;   % coulomb (SI)
; q_over_m = -q / e_mass;    % used only for Prolate Cycloid in loop; e is negative charge
; rad2deg  = 180.0 / pi;
; twoPi    = 2.0 * pi;
; v_1keV_electron  = 18755373.; % m/s
; v_500eV_electron = 13262052.; % m/s

; Conversions -----------------------------------
; mV2V   =  1.0e-3;          % mV > V (SI)
; nT2T   =  1.0e-9;          % nT > T (SI)
; C_V_T  =  mV2V / nT2T;     % Combining constants to save flops; potentially used 100Ks of times

; Tg = 2 pi m / |q| B * (1/1e-9) (1e6) -> Tg = 2 pi m / |q| * 1e15 = 35723.884068 / BnT -> Tg = nT2µs / B
B2Tg_nTus = 35723.884068 ; convert B in nT to Tg in µs, and vice versa
