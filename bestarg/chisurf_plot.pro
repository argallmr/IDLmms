pro chisurf_plot, $
	mm, GDU_locX, GDU_locY, alpha, out, gunid, maxorder, bestord, $ ; IN
	tri_ok, $     ; IN
	chisurf, xsurf, ysurf, $ ; IN
	xd, yd, rd, pd, $ ; IN
	rchi2, $      ; IN
	conlevel, inside, rderr, pderr, $ ; IN
	conlevel_info, conlevel_xy, $ ; OUT
	init=init, last=last, siz=siz, $
	conlevel_color=conlevel_color, $
	info_in=info_in, xy_in=xy_in, color_in=color_in, title=title, $
	oplot_x=oplot_x, oplot_y=oplot_y, rsiz=rsiz, ninside=ninside, $
	coarse=coarse, edge_error=edge_error, ps=ps

	ps = keyword_set(ps)
	if (!d.name eq 'X') then $
		device, true = 24, decomposed = 0, retain = 2
	
	if (keyword_set(init)) then begin
		!p.multi = [0,2,1]          ; !p.multi = [0,2,2,0,1]
		os = 600
		if !d.name eq 'X' then $
			window,1,xs=os*2.,ys=os
	endif

	if (ps) then begin
		!p.charsize = 0.85
		!p.thick = 2.0
		!p.charthick = 2.0
		sz_grid = 0.5
		sz_min = .5
	endif $
	else begin
		!p.charsize = 1.0
		!p.thick = 1.0
		!p.charthick = 1.0
		sz_grid = 0.5
		sz_min = 1.0
	endelse

	edi_setcolors, cs
	!p.background = cs.white
	!p.color = cs.black
	!y.omargin = [3,5]
	
	edge_error = keyword_set(edge_error)
	if (edge_error) then $
		ninside = 0
	
	if (edge_error) then $
		goto, skip_eswath
	
	; Create the [x], [y] footprints for polyfilling the target error swath
	rmin = rd - rderr > 0.
	rmax = rd + rderr
	amin = pd - pderr
	amax = pd + pderr
	
	n = 100

	slp = (rmax - rmin) / float(n-1)
	r1 = slp*findgen(n) + rmin
	a1 = make_array(n,/float,value=amax)
	
	slp = (amin - amax) / float(n-1)
	a2 = slp*findgen(n) + amax
	r2 = make_array(n,/float,value=rmax)
	
	slp = (rmin - rmax) / float(n-1)
	r3 = slp*findgen(n) + rmax
	a3 = make_array(n,/float,value=amin)
	
	slp = (amax - amin) / float(n-1)
	a4 = slp*findgen(n) + amin
	r4 = make_array(n,/float,value=rmin)
	
	eswathx_gyro = [r1*cos(a1),r2*cos(a2),r3*cos(a3),r4*cos(a4)]
	eswathy_gyro = [r1*sin(a1),r2*sin(a2),r3*sin(a3),r4*sin(a4)]
	skip_eswath:

	if (n_elements(rsiz) ne 0) then begin ; plotting size relative to target location
		xrange = [-rsiz,rsiz]+xd
		yrange = [-rsiz,rsiz]+yd
	endif $
	else begin
		if (n_elements(siz) ne 0) then begin
			if (siz ne 0) then begin
				xrange = [-siz,siz]
				yrange = [-siz,siz]
			endif $
			else begin
				xmin = min([min(eswathx_gyro),min(xsurf)])
				xmax = max([max(eswathx_gyro),max(xsurf)])
				xmid = (xmax+xmin)/2.
				ymin = min([min(eswathy_gyro),min(ysurf)])
				ymax = max([max(eswathy_gyro),max(ysurf)])
				ymid = (ymax+ymin)/2.

				xlen = xmax-xmin
				ylen = ymax-ymin
				maxlen = max([xlen,ylen])
				maxlen = maxlen*1.1

				xrange = [xmid-maxlen/2.,xmid+maxlen/2.]
				yrange = [ymid-maxlen/2.,ymid+maxlen/2.]
			endelse $
		endif $
		else begin
			xmin = min([min(eswathx_gyro),min(xsurf)])
			xmax = max([max(eswathx_gyro),max(xsurf)])
			xmid = (xmax+xmin)/2.
			ymin = min([min(eswathy_gyro),min(ysurf)])
			ymax = max([max(eswathy_gyro),max(ysurf)])
			ymid = (ymax+ymin)/2.

			xlen = xmax-xmin
			ylen = ymax-ymin
			maxlen = max([xlen,ylen])
			maxlen = maxlen*1.1

			xrange = [xmid-maxlen/2.,xmid+maxlen/2.]
			yrange = [ymid-maxlen/2.,ymid+maxlen/2.]
		endelse
	endelse

	;******************************************************************
	;contour plot combined likelihood function
	;******************************************************************
	xdtit='!5X!dmin!n = '+strcompress(string(format='(f9.3)',xd),/remove_all)
	ydtit='Y!dmin!n ='+strcompress(string(format='(f9.3)',yd),/remove_all)

	chisurfmin = min(chisurf)
	contour, $
		(chisurf-chisurfmin),$
		xsurf,ysurf,xra=xrange,yra=yrange,$
		xsty=1,ysty=1,xtit=xdtit,ytit=ydtit,$
		title=title, /nodata

	; Error swath goes down first
	if (not edge_error) then $
		polyfill, eswathx_gyro, eswathy_gyro, color=cs.grey40, /data

	; Grid points go down second
	oplot, xsurf, ysurf, psym=2, color=cs.blue, symsize=sz_grid
	if (not edge_error) then $
		oplot, xsurf(inside), ysurf(inside), psym=2, color=color, symsize=sz_grid

	level = [1,5,10,100,1000] ; Remember:  the conlevel will be < 16 or so...
	nlevels = 5

	; Isocontours overplotted
	contour, $
		(chisurf-chisurfmin),$
		xsurf,ysurf,nlevels=nlevels,/overplot, $
		c_labels=make_array(nlevels,/long,value=1), levels=level

	; Confidence level isocontour overplotted (conlevel)
	if n_elements(conlevel_color) ne 0 then $
		istat = execute ('color=cs.' +conlevel_color) $
	else $
		color=cs.green
	if (not edge_error) then $
		contour, $
			(chisurf-chisurfmin), $
			xsurf,ysurf,nlevels=1,/overplot, $
			level=[conlevel], c_colors=[color], c_labels=[0]

	; Return some isocontour information
	if (not edge_error) then begin
		; Return this contour information
		contour,(chisurf-chisurfmin), $
			xsurf,ysurf,nlevels=1, /overplot, $
			level=[conlevel], c_colors=[color], c_labels=[0], $
			PATH_XY=conlevel_xy, PATH_INFO=conlevel_info, /path_data_coords

		;FOR I = 0, (N_ELEMENTS(conlevel_info) - 1 ) DO BEGIN
		;    S = [INDGEN(conlevel_info(I).N), 0]
		;    PLOTS, conlevel_xy(*,conlevel_info(i).OFFSET + S ), color=cs.green
		;ENDFOR

		if n_elements(info_in) ne 0 then begin
			istat=execute('color=cs.'+color_in)
			FOR I = 0, (N_ELEMENTS(info_in) - 1 ) DO BEGIN
				S = [INDGEN(info_in(I).N), 0]
				PLOTS, xy_in(*,info_in(I).OFFSET + S ), color=color
			ENDFOR
		endif
	endif

	; Best target
	usersym, [-1,-1,1,1], [-1,1,1,-1], color=cs.black, /fill
	oplot, [xd],[yd], psym=8, symsize=sz_min

	; Oplot the coarse target on the fine grid
	oplot_annot = ''
	if (n_elements(oplot_x) ne 0) then begin
		oplot_annot = 'Coarse grid target at green square'
		for i=0,n_elements(oplot_x)-1 do begin
			usersym, [-1,-1,1,1], [-1,1,1,-1], color=cs.green, /fill
			oplot, [oplot_x(i)], [oplot_y(i)], psym=8, symsize=sz_min
		endfor
	endif

	; Annotation
	if keyword_set(coarse) then $
		conp='99%' else conp='90%'
	outs = norm_axis([.1,.95])
	s1 = 'Grid points are blue stars'
	s2 = '!cChiSurf min. at black square'
	if (edge_error) then $
		s3 = '' $
	else $
		s3 = '!c!c' +conp +' ConLevel contour at ' +string (conlevel,'(f4.1)')
	if (edge_error) then $
		s4 = '' $
	else $
		s4 = '!c!c!c# pts. within ConLevel: ' +string (ninside,'(i3)')
	if (edge_error) then $
		s5 = '' $
	else $
		s5 = '!c!c!c!cError Swath determined from ConLevel coutour in grey'
	s6 = '!c!c!c!c!c' +oplot_annot
	xyouts, outs(0), outs(1), [s1,s2,s3,s4,s5,s6], color=[cs.blue,cs.black,color,color,cs.grey40,cs.green]

	;******************************************************************
	;draw firing directions and a symbol at each gun location
	;thick lines point to the target consistent with the respective tof
	;******************************************************************
	runner = lonarr(maxorder)
	for im=0,mm-1 do begin
		if (tri_ok(im) eq 1) then runner(bestord(im)-1) = runner(bestord(im)-1) + 1
	endfor
	text5 = strcompress(runner(0),/remove_all)
	for order= 2,maxorder do begin
		text5 = text5+', '+strcompress(runner(order-1),/remove_all)
	endfor

	plot,	$
		[0],[0],psym=1,xra=xrange,yra=yrange, $
		xsty=1,ysty=1,xtit=xdtit,ytit=ydtit,title='Runner types: '+text5, /nodata
	;oplot,	xrange,[yd,yd],linestyle=1
	;oplot,	[xd,xd],yrange,linestyle=1

	; Grid
	oplot, xsurf, ysurf, psym=2, color=cs.blue, symsize=sz_grid
	oplot,	[0],[0], psym=1, color=cs.blue

	; Beams and gun locations
	xx=xrange
	for im = 0, mm-1 do begin
		if tri_ok(im) eq 1 then begin
			oplot,[GDU_locX(im)],[GDU_locY(im)],psym=8-gunid(im)
			yy=tan(alpha(im)+1e-4)*(xx-GDU_locX(im))+GDU_locY(im)
			oplot,xx,yy,thick=0.01, color=cs.grey80
		endif
	endfor

	; Error swath
	if (not edge_error) then $
		polyfill, eswathx_gyro, eswathy_gyro, color=cs.grey40, /data

	if (keyword_set(coarse)) then begin ; Coarse grid, put beams on top of error swath
		; Beams and gun locations
		xx=xrange
		for im = 0, mm-1 do begin
			if tri_ok(im) eq 1 then begin
				oplot,[GDU_locX(im)],[GDU_locY(im)],psym=8-gunid(im)
				yy=tan(alpha(im)+1e-4)*(xx-GDU_locX(im))+GDU_locY(im)
				oplot,xx,yy,thick=0.01, color=cs.grey80
			endif
		endfor
	endif

	; Best target
	usersym, [-1,-1,1,1], [-1,1,1,-1], color=cs.black, /fill
	oplot, [xd],[yd], psym=8, symsize=sz_min

return
end
