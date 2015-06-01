; cb_constants.pro

; Constants -------------------------------------
c        = 2.99792458e+08;  % m/s, speed of light (SI)
deg2rad  = pi / 180.0;
halfPi   = pi / 2.0;
me       = 9.10938188e-31;  % kg, electron mass (SI)
q        = 1.602177e-19;   % coulomb (SI)
q_over_m = -q / me;
rad2deg  = 180.0 / pi;
twoPi    = 2.0 * pi;

; v = c * SQRT [ 1 - (me0 / (me0 + 1keV))^2 ], where me0 = rest mass energy of electron, 0.510998910 MeV
v_1keV_electron  = 18727897.366; % m/s, 18755373. m/s relativistic: difference of 0.147%
v_500eV_electron = 13252328.354; % m/s, 13262052. m/s relativistic: difference of 0.073%

; Conversions ----------------------------------- may depend on constants above
mV2V   =  1.0e-3;          % mV > V (SI)
nT2T   =  1.0e-9;          % nT > T (SI)
C_V_T  =  mV2V / nT2T;     % Combining constants to save flops; potentially used 100Ks of times

; Tg = 2 pi m / |q| B * (1/1e-9) (1e6) -> Tg = 2 pi m / |q| * 1e15 = 35723.884068 / BnT -> Tg = nT2µs / B
B2Tg_nTus = 35723.884068 ; convert B in nT to Tg in µs, and vice versa
