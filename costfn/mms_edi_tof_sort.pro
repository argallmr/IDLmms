; docformat = 'rst'
;
; NAME:
;       mms_edi_tof_sort
;
; PURPOSE:
;+
;   Segregate EDI beams into groups fired toward and away from the target.
;
;   NOTE:
;       "Toward" and "away" is ambiguous here. In order to determine if
;       on group is firing toward the target, you would need to examine
;       where the drift step is pointing.
;
;       For Cluster, ep_hav_sorter was called from within ep_toaw_sort_sa.
;       ep_hav_sorter iteratively used the average firing vector from a
;       single gun. Then, ep_toaw_sort_sa took the indices and re-calculated
;       the average and standard deviation of all beams, doing a lot of
;       extra math to rotate away beams into the toward direction.
;
;       In this version, we iteratively use the average firing angle from
;       both guns, cutting out the need for a wrapper like ep_toaw_sort_sa.
;       The reasons for this are 1) so that the standard deviation is
;       linked directly to the mean, and 2) because the decision between
;       time of flight and triangulation is based on the standard
;       deviation computed here. TOF is used when the difference between
;       times of flights between the two guns is significant -- i.e. when
;       the drift step is large beams from both guns are quasi-parallel
;       such that stdev is small. Conversely, triangulation is used when
;       stdev is large -- i.e. when the drift step is small and beams are
;       quasi-perpendicular.
;
;       The towards and away beams, then, should
;
; :Categories:
;   MMS, EDI
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/10/24  -   Written by Matthew Argall. Adapted from Hans Vaith's
;                           ep_hav_sorter.pro
;-
;*****************************************************************************************
;+
;   Sort firing vectors into the toward and away firing directions.
;
; :Params:
;       PHASEARR:       in, required, type=float
;                       Firing angle (degrees) of EDI electron beams projected into the
;                           plane perpendicular to B (BPP), positive clock-wise from
;                           the +x-axis.
;       REFPHASE:       in, required, type=float
;                       Reference firing phase. Ideally, this phase should be half-
;                            way between toward and away beams.
;       AVG:            out, required, type=lonarr
;                       Average phase (degrees) of beams.
;       X1:             out, required, type=lonarr
;                       Indices into `PHASEARR` of the beams that were fired toward
;                           the virtual source point.
;       X2:             out, required, type=lonarr
;                       Indices into `PHASEARR` of the beams that were fired away
;                           from the virtual source point.
;
; :Returns:
;       STDEV:          Combined standard deviation (degrees) of the phase of toward and
;                           away beams from the mean toward and away directions.
;-
function mms_edi_tof_sort1, phasearr, refphase, avg, x1, x2
	compile_opt idl2
	on_error, 2

	; NOTE: Either x1 or x2 could be [-1], but not both.

	;Editable copy of input
	tmpphase = phasearr
	
	;
	; The following two expressions will force PHASEARR to be
	; between -270 and +90 of REFPHASE
	;
	tmpphase -= (tmpphase gt refphase +  90.0) * 360.0
	tmpphase += (tmpphase lt refphase - 270.0) * 360.0
	
	;We will be computing the variance of the firing phase
	;   sum_i{ (x_i - x_avg)^2 }
	sqsum = 0.0
	
	;
	; Split the phases into two groups of 180 degrees
	; centered around 0 and 180 degree phase-angles.
	;    1. -270 <= PHASEARR < -90
	;    2.  -90 <= PHASEARR <= 90
	;
	; Groups 1 and 2 represent
	;    1. Quadrants II & III  (left-half plane)
	;    2. Quadrants  I &  II  (right-half plane)
	;
	; Ideally, Group 1 (2) should be firing toward (away)
	; from the virtual source, or vice versa. REFPHASE
	; should be chosen to make this true.
	;
	
	;GROUP 1
	x2 = where(tmpphase lt refphase-90, cnt2)
	if cnt2 gt 0 then begin
		;Rotate beams into the "Toward" sector
		ph2 = tmpphase[x2] + 180.0
	
		;Average value of the firing phase
		avg2  = total(ph2) / cnt2
		sqsum = sqsum + total((ph2-avg2)^2)
		
		;Force between [-180, 180]
		if avg2 lt -180.0 then avg2 += 360.0
		if avg2 gt  180.0 then avg2 -= 360.0
	endif

	;GROUP 2
	;   - Combine with the results from GROUP 1
	x1 = where(tmpphase ge refphase-90, cnt1)
	if cnt1 gt 0 then begin
		;Average value of the firing phase
		ph1   = tmpphase[x1]
		avg1  = total(ph1)/cnt1
		sqsum = sqsum + total((ph1-avg1)^2)
		
		;Force between [-180, 180]
		if avg1 lt -180.0 then avg1 += 360.0
		if avg1 gt  180.0 then avg1 -= 360.0
	endif
	
	;Pick the average of the toward beams when possible
	;  - Do not mix. We are trying to hone in on the firing
	;    dirction of a single GDU.
	avg = cnt1 gt 0 ? avg1 : avg2

	;Standard deviation of all groups
	stdev = sqrt (sqsum / (cnt1+cnt2))

	return, stdev
end


;+
;   Sort firing vectors into the toward and away firing directions.
;
; :Params:
;       PHASEARR:       in, required, type=float
;                       Firing angle (degrees) of EDI electron beams projected into the
;                           plane perpendicular to B (BPP), positive clock-wise from
;                           the +x-axis.
;       IX:             out, required, type=lonarr
;                       Indices into `PHASEARR` of the beams that were fired toward
;                           the virtual source point. Note that there is a 180 degree
;                           ambiguity between "toward" and "away". Either `IX` or `IY`
;                           could be "toward".
;       IY:             out, required, type=lonarr
;                       Indices into `PHASEARR` of the beams that were fired away
;                           from the virtual source point. Note that there is a 180 degree
;                           ambiguity between "toward" and "away". Either `IX` or `IY`
;                           could be "away".
;       AVG:            out, optional, type=float
;                       Average firing angle (degrees). Away beams are rotated 180 degrees
;                           into the toward-direction, then both sets are averaged.
;       STDEV:          out, optional, type=float
;                       Combined standard deviation (degrees) of the toward and away beams
;                           from the mean toward and away directions.
;-
pro mms_edi_tof_sort, phasearr, ix, iy, avg, stdev
	compile_opt idl2
	on_error, 2

	;Pick three reference phases in an attempt to
	;split the beams into toward and away groups
	refa =   0.0
	refb = 120.0
	refc = 240.0
	
	;Find the average angle between the two groups
	;   We hope to adjust the reference phase until all
	;   beams are clustered around 0 and 180 degree phase
	;   angles.
	stdeva = mms_edi_tof_sort1(phasearr, refa, avga, ix, iy)
	stdevb = mms_edi_tof_sort1(phasearr, refb, avgb, ix, iy)
	stdevc = mms_edi_tof_sort1(phasearr, refc, avgc, ix, iy)

	;Five more iterations
	for icnt=2,6 do begin
		;Update the reference phase with the average phase,
		;hopefully centering the beams around 0 and 180 with
		;respect to the reference phase angle.
		refa = avga
		refb = avgb
		refc = avgc

		;Compute again
		stdeva = mms_edi_tof_sort1(phasearr, refa, avga, ix, iy)
		stdevb = mms_edi_tof_sort1(phasearr, refb, avgb, ix, iy)
		stdevc = mms_edi_tof_sort1(phasearr, refc, avgc, ix, iy)
	endfor

	;One final time. Choose the maximum phase as the reference.
	ref   = avga > avgb > avgc
	stdev = mms_edi_tof_sort1(phasearr, ref, avg, ix, iy)
	
	;Make do not return anything if no beams were found in a given directin
	;   - This might be true for one group, but not for both, so long as
	;     as PHASEARR has at least one element.
	if ix[0] eq -1 then void = temporary(ix)
	if iy[0] eq -1 then void = temporary(iy)
end



;---------------------------------------------------------------------
; Main Level Test Program (.r ep_hav_sorter) /////////////////////////
;---------------------------------------------------------------------

;---------------------------------------------------------------------
; Spacecraft and Beams ///////////////////////////////////////////////
;---------------------------------------------------------------------
;Create the spacecraft outline
nsc  = 50
r_sc = 3.5 ;meters
x_sc = r_sc * cos(2*!pi*findgen(nsc)/(nsc-1))
y_sc = r_sc * sin(2*!pi*findgen(nsc)/(nsc-1))

;Gun positions
nguns = 8
xg    = x_sc[round(nsc*randomu(3, nguns))]
yg    = y_sc[round(nsc*randomu(3, nguns))]

;Target
tmax = 30.0 ;meters
tx   = 0 ;tmax * 2.0*((randomu(!Null, 1))[0] - 0.5)
ty   = 0 ;tmax * 2.0*((randomu(!Null, 1))[0] - 0.5)

;Firing angles
vx    = tx - xg
vy    = ty - yg
alpha = atan(vy, vx)

;Some toward, others away
nto = floor(nguns/2)
naw = nguns-nto
ito = indgen(nto)
iaw = indgen(naw) + nto
alpha[iaw] = alpha[iaw] - !pi

;---------------------------------------------------------------------
; Sort ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
;Compute toward and away
mms_edi_tof_sort, alpha*!radeg, ix, iy, avg, stdev
nx = n_elements(ix)
ny = n_elements(iy)


print, FORMAT='(%"alpha = %0.4f +/- %0.4f")', avg, stdev
print, 'ito = [' + strjoin(strtrim(ito, 2), ', ') + ']'
if nx gt 0 then print, 'ix  = [' + strjoin(strtrim(ix,  2), ', ') + ']' else print, 'nx = 0'
print, 'iaw = [' + strjoin(strtrim(iaw, 2), ', ') + ']'
if ny gt 0 then print, 'iy  = [' + strjoin(strtrim(iy,  2), ', ') + ']' else print, 'ny = 0'

;---------------------------------------------------------------------
; Plot Results ///////////////////////////////////////////////////////
;---------------------------------------------------------------------

;Plot range
range  = (max(abs([tx, ty])) > r_sc) * [-1.1, 1.1]

;Average firing vector, fired from the average gun location
print, 'Plotting average firing vector, fired from the average "toward" gun position.'
ox = nto gt 0 ? mean(xg[ito]) : mean(xg[iaw])
oy = nto gt 0 ? mean(yg[ito]) : mean(yg[iaw])
xf = [ox, range[1]*cos(avg*!dtor)]
yf = [oy, range[1]*sin(avg*!dtor)]

;Did we get the direction correct?
if (xf[1]*tx + yf[1]*ty) lt 0 then begin
	avg = avg + 180.0
	xf  = [0.0, range[1]*cos(avg*!dtor)]
	yf  = [0.0, range[1]*sin(avg*!dtor)]
	print, 'Alpha is off by 180 degrees.'
endif

xf_minus = [0.0, range[1]*cos((avg-stdev)*!dtor)]
xf_plus  = [0.0, range[1]*cos((avg+stdev)*!dtor)]
yf_plus  = [0.0, range[1]*sin((avg-stdev)*!dtor)]
yf_minus = [0.0, range[1]*sin((avg+stdev)*!dtor)]
xf       = reform([[xf], [xf_minus], [xf_plus]], 6)
yf       = reform([[yf], [yf_minus], [yf_plus]], 6)
f_conn   = reform([replicate(2,1,3), lindgen(2,3)], 3*3)

;Beam slope, y-intercept, (x1,x2) and (y1,y2)
;   - slope (m)       = rise / run
;   - y-intercept (b) = y1 - m * x1
;   - (x1,x2)         = range
;   - (y1,y2)         = m*x + b
m     = vy / vx
b     = yg - m * xg
x0 = replicate(range[0], nguns)
y0 = m*x0 + b
x1 = replicate(range[1], nguns)
y1 = m*x1 + b

;Vectors from the guns to the left (neg) and right (pos) of the plot
negx = transpose([[xg], [x0]])
negy = transpose([[yg], [y0]])
posx = transpose([[xg], [x1]])
posy = transpose([[yg], [y1]])

;Toward vectors will be solid, away vectors dashed.
;   - Vectors from toward guns have a unique color
to1_x = reform(negx[*,ito], 2*nto)  ;Toward gun "toward" vector
to2_x = reform(posx[*,ito], 2*nto)  ;Toward gun "away"   vector
to1_y = reform(negy[*,ito], 2*nto)  ;Toward gun "toward" vector
to2_y = reform(posy[*,ito], 2*nto)  ;Toward gun "away"   vector

;Toward vectors will be solid, away vectors dashed.
;   - Vectors from away guns have a unique color
aw1_x = reform(posx[*,iaw], 2*naw)  ;Away   gun "toward" vector
aw2_x = reform(negx[*,iaw], 2*naw)  ;Away   gun "away"   vector
aw1_y = reform(posy[*,iaw], 2*naw)  ;Away   gun "toward" vector
aw2_y = reform(negy[*,iaw], 2*naw)  ;Away   gun "away"   vector

;Define the connectivity between firing vectors.
to_conn = reform([replicate(2,1,nto), lindgen(2,nto)], 3*nto)
aw_conn = reform([replicate(2,1,naw), lindgen(2,naw)], 3*naw)

;Plot the results
p1 = Plot([0], [0], /NODATA, $
          TITLE  = 'Spacecraft', $
          XRANGE = range, $
          XTITLE = 'x (m)', $
          YRANGE = range, $
          YTITLE = 'y (m)')
gsc   = polyline(x_sc, y_sc, /DATA, TARGET=p1)
ggto  = Symbol(xg[ito], yg[ito], 'Circle', /DATA, TARGET=p1, SYM_COLOR='Blue')
ggaw  = Symbol(xg[iaw], yg[iaw], 'Circle', /DATA, TARGET=p1, SYM_COLOR='Green')
gto1  = polyline(to1_x, to1_y, CONNECTIVITY=to_conn, /CLIP, /DATA, TARGET=p1, COLOR='Blue')
gto2  = polyline(to2_x, to2_y, CONNECTIVITY=to_conn, /CLIP, /DATA, TARGET=p1, COLOR='Blue',  LINESTYLE='--')
gaw1  = polyline(aw1_x, aw1_y, CONNECTIVITY=aw_conn, /CLIP, /DATA, TARGET=p1, COLOR='Green')
gaw1  = polyline(aw2_x, aw2_y, CONNECTIVITY=aw_conn, /CLIP, /DATA, TARGET=p1, COLOR='Green', LINESTYLE='--')
r     = polyline(xf,    yf, CONNECTIVITY=f_conn, /CLIP, /DATA, TARGET=p1, COLOR='Red')
gtarg = Symbol(tx, ty, 'X', /DATA, TARGET=p1, SYM_COLOR='Red', SYM_THICK=2)

end