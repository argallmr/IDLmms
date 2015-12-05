; docformat = 'rst'
;
; NAME:
;    mms_test_v_fdoa_mec
;
; PURPOSE:
;+
;   Test the similarity between positions and velocity reported in FDOA and MEC data.
;
; :Categories:
;    MMS, Diagnostics, FDOA, MEC
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
;    Modification History::
;       2015/11/28  -   Written by Matthew Argall
;-
function mms_test_rv_fdoa_mec, sc, tstart, tend
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if obj_valid(rwin) then obj_destroy, rwin
		if obj_valid(vwin) then obj_destroy, vwin
		MrPrintF, 'LogErr'
		return, !Null
	endif

	sc       = 'mms4'
	tstart   = '2015-08-15T00:00:00Z'
	tend     = '2015-08-15T24:00:00Z'

;-------------------------------------------------------
; Find Files ///////////////////////////////////////////
;-------------------------------------------------------
	
	;Read ephemeris and attitude data
	r_fdoa_eci = mms_fdoa_scpos(sc, tstart, tend, t_fdoa, V=v_fdoa_eci, /GEI)
	
	;T89D
	mms_mec_read, sc, 'epht89d', tstart, tend, $
	              R_ECI    = r_t89d_eci, $
	              V_ECI    = v_t89d_eci, $
	              TIME     = t_t89d
	
	;T89Q
	mms_mec_read, sc, 'epht89q', tstart, tend, $
	              R_ECI    = r_t89q_eci, $
	              V_ECI    = v_t89q_eci, $
	              TIME     = t_t89q

	;T04D
	mms_mec_read, sc, 'ephts04d', tstart, tend, $
	              R_ECI    = r_t04d_eci, $
	              V_ECI    = v_t04d_eci, $
	              TIME     = t_t04d

;-------------------------------------------------------
; ECI Results //////////////////////////////////////////
;-------------------------------------------------------
	t_fdoa_ssm = MrCDF_epoch2ssm(t_fdoa)
	t_t89d_ssm = MrCDF_epoch2ssm(t_t89d)
	t_t89q_ssm = MrCDF_epoch2ssm(t_t89q)
	t_t04d_ssm = MrCDF_epoch2ssm(t_t04d)

	;Create a window
	rwin = MrWindow(LAYOUT=[1,3], YGAP=0.5, REFRESH=0)

	;Rx
	p_rx1 = MrPlot(t_fdoa_ssm, r_fdoa_eci[0,*], $
	               /CURRENT, $
	               NAME        = 'Rx FDOA', $
	               XTICKFORMAT = '(a1)', $
	               TITLE       = 'FDOA vs. MEC: R ECI', $
	               YTITLE      = 'R$\downX$!C(km)')
	p_rx2 = MrPlot(t_t89d_ssm, r_t89d_eci[0,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Rx T89D', $
	               OVERPLOT    = p_rx1)
	p_rx3 = MrPlot(t_t89q_ssm, r_t89q_eci[0,*], $
	               COLOR       = 'Forest Green', $
	               NAME        = 'Rx T89Q', $
	               OVERPLOT    = p_rx1)
	p_rx4 = MrPlot(t_t04d_ssm, r_t04d_eci[0,*], $
	               COLOR       = 'Red', $
	               NAME        = 'Rx T04D', $
	               OVERPLOT    = p_rx1)

	;Ry
	p_ry1 = MrPlot(t_fdoa_ssm, r_fdoa_eci[1,*], $
	               /CURRENT, $
	               NAME        = 'Ry FDOA', $
	               XTICKFORMAT = '(a1)', $
	               YTITLE      = 'R$\downY$!C(km)')
	p_ry2 = MrPlot(t_t89d_ssm, r_t89d_eci[1,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Ry MEC', $
	               OVERPLOT    = p_ry1)
	p_ry3 = MrPlot(t_t89q_ssm, r_t89q_eci[1,*], $
	               COLOR       = 'Forest Green', $
	               NAME        = 'Ry T89Q', $
	               OVERPLOT    = p_ry1)
	p_ry4 = MrPlot(t_t04d_ssm, r_t04d_eci[1,*], $
	               COLOR       = 'Red', $
	               NAME        = 'Ry T04D', $
	               OVERPLOT    = p_ry1)

	;Rz
	p_rz1 = MrPlot(t_fdoa_ssm, r_fdoa_eci[2,*], $
	               /CURRENT, $
	               NAME        = 'Rz FDOA', $
	               XTICKFORMAT = 'time_labels', $
	               XTITLE      = 'Time (UT)', $
	               YTITLE      = 'R$\downZ$!C(km)')
	p_rz2 = MrPlot(t_t89d_ssm, r_t89d_eci[2,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Rz MEC', $
	               OVERPLOT    = p_rz1)
	p_rz3 = MrPlot(t_t89q_ssm, r_t89q_eci[2,*], $
	               COLOR       = 'Forest Green', $
	               NAME        = 'Rz T89Q', $
	               OVERPLOT    = p_rz1)
	p_rz4 = MrPlot(t_t04d_ssm, r_t04d_eci[2,*], $
	               COLOR       = 'Red', $
	               NAME        = 'Rz T04D', $
	               OVERPLOT    = p_rz1)
	
	;Legend
	l = MrLegend(ALIGNMENT    = 'NE', $
	             LABEL        = ['FDOA', 'T89D', 'T89Q', 'TS04D'], $
	             POSITION     = [1.0, 1.0], $
	             /RELATIVE, $
	             SAMPLE_WIDTH = 0, $
	             TEXT_COLOR   = ['Black', 'Blue', 'Forest Green', 'Red'], $
	             TARGET       = [p_rx1, p_rx2, p_rx3, p_rx4])
	
	;ECI Window
	rwin -> Refresh

;-------------------------------------------------------
; ECI Results //////////////////////////////////////////
;-------------------------------------------------------
	;Create a window
	vwin = MrWindow(LAYOUT=[1,3], YGAP=0.5, REFRESH=0)

	;Rx
	p_vx1 = MrPlot(t_fdoa_ssm, v_fdoa_eci[0,*], $
	               /CURRENT, $
	               NAME        = 'Vx FDOA', $
	               XTICKFORMAT = '(a1)', $
	               TITLE       = 'FDOA vs. MEC: V ECI', $
	               YTITLE      = 'V$\downX$!C(km)')
	p_vx2 = MrPlot(t_t89d_ssm, v_t89d_eci[0,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Vx T89D', $
	               OVERPLOT    = p_vx1)
	p_vx3 = MrPlot(t_t89q_ssm, v_t89q_eci[0,*], $
	               COLOR       = 'Forest Green', $
	               NAME        = 'Vx T89Q', $
	               OVERPLOT    = p_vx1)
	p_vx4 = MrPlot(t_t04d_ssm, v_t04d_eci[0,*], $
	               COLOR       = 'Red', $
	               NAME        = 'Vx T04D', $
	               OVERPLOT    = p_vx1)

	;Vy
	p_vy1 = MrPlot(t_fdoa_ssm, v_fdoa_eci[1,*], $
	               /CURRENT, $
	               NAME        = 'Vy FDOA', $
	               XTICKFORMAT = '(a1)', $
	               YTITLE      = 'V$\downY$!C(km)')
	p_vy2 = MrPlot(t_t89d_ssm, v_t89d_eci[1,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Vy MEC', $
	               OVERPLOT    = p_vy1)
	p_vy3 = MrPlot(t_t89q_ssm, v_t89q_eci[1,*], $
	               COLOR       = 'Forest Green', $
	               NAME        = 'Vy T89Q', $
	               OVERPLOT    = p_vy1)
	p_vy4 = MrPlot(t_t04d_ssm, v_t04d_eci[1,*], $
	               COLOR       = 'Red', $
	               NAME        = 'Vy T04D', $
	               OVERPLOT    = p_vy1)

	;Vz
	p_vz1 = MrPlot(t_fdoa_ssm, v_fdoa_eci[2,*], $
	               /CURRENT, $
	               NAME        = 'Vz FDOA', $
	               XTICKFORMAT = 'time_labels', $
	               XTITLE      = 'Time (UT)', $
	               YTITLE      = 'V$\downZ$!C(km)')
	p_vz2 = MrPlot(t_t89d_ssm, v_t89d_eci[2,*], $
	               COLOR       = 'Blue', $
	               NAME        = 'Vz MEC', $
	               OVERPLOT    = p_vz1)
	p_vz3 = MrPlot(t_t89q_ssm, v_t89q_eci[2,*], $
	               COLOR       = 'Forest Green', $
	               NAME        = 'Vz T89Q', $
	               OVERPLOT    = p_vz1)
	p_vz4 = MrPlot(t_t04d_ssm, v_t04d_eci[2,*], $
	               COLOR       = 'Red', $
	               NAME        = 'Vz T04D', $
	               OVERPLOT    = p_vz1)
	
	;Legend
	l = MrLegend(ALIGNMENT    = 'NE', $
	             LABEL        = ['FDOA', 'T89D', 'T89Q', 'TS04D'], $
	             POSITION     = [1.0, 1.0], $
	             /RELATIVE, $
	             SAMPLE_WIDTH = 0, $
	             TEXT_COLOR   = ['Black', 'Blue', 'Forest Green', 'Red'], $
	             TARGET       = [p_vx1, p_vx2, p_vx3, p_vx4])
	
	;ECI Window
	vwin -> Refresh
	return, [rwin, vwin]
end
