; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_apply_cals
;
; PURPOSE:
;+
;   Sort burst data by pad into 0 and 180 degree counts.
;
; :Params:
;       FILE_GDU1:  in, required, type=string
;                   Name of the IDL save file containing calibration data for GDU1.
;       FILE_GDU2:  in, required, type=string
;                   Name of the IDL save file containing calibration data for GDU1.
;
; :Returns:
;      EDI_OUT:     A structure with the following tags::
;                       TT2000_0    - Time tags for 0 degree pitch angle counts
;                       TT2000_180  - Time tags for 180 degree pitch angle counts
;                       PA0         - Center pitch angle of each annode with PA 0
;                       PA0_LO      - Lower pitch angle of each annode with PA 0
;                       PA0_HI      - Upper pitch angle of each annode with PA 0
;                       PA180       - Center pitch angle of each annode with PA 180
;                       PA180_L0    - Lower pitch angle of each annode with PA 180
;                       PA180_HI    - Upper pitch angle of each annode with PA 180
;                       GDU_0       - Flag to sort PA 0 counts by GDU.
;                       GDU_180     - Flag to sort PA 180 counts by GDU.
;-
function mms_test_edi_cals
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	endif

	;Files
	f_brst    = '/nfs/mms3/edi/brst/l1a/amb/2015/07/23/mms3_edi_brst_l1a_amb_20150723194544_v0.6.2.cdf'
	fcal_gdu1 = '/home/argall/amb-cals/mms3_edi_brst_l1a_amb_20150723194544_v0.6.1_GDU1_correction_factors.sav'
	fcal_gdu2 = '/home/argall/amb-cals/mms3_edi_brst_l1a_amb_20150723194544_v0.6.1_GDU2_correction_factors.sav'
	
	;Read the data
	amb_data = mms_edi_amb_l1a_read(f_brst, /EXPAND_ANGLES)

	;Calibrate data
	cal_data = mms_edi_amb_apply_cals(amb_data, fcal_gdu1, fcal_gdu2)
	
	;Convert time to ssm
	t_gdu1 = MrCDF_epoch2ssm(amb_data.epoch_gdu1)
	t_gdu2 = MrCDF_epoch2ssm(amb_data.epoch_gdu2)

;-----------------------------------------------------
; Plot GDU1 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	win1  = MrWindow(XSIZE=600, YGAP=0.5, YSIZE=1000, REFRESH=0)
	
	;COUNTS1 GDU1
	p1 = MrPlot(t_gdu1, amb_data.counts1_gdu1, $
	            /CURRENT, $
	            NAME        = 'Counts1 GDU1', $
	            TITLE       = 'EDI Corrected Counts GDU1', $
	            XTICKFORMAT = '(a1)', $
	            YTITLE      = 'Counts1!CGDU1')
	p1c = MrPlot(t_gdu1, cal_data.counts1_gdu1_cal, $
	             /CURRENT, $
	             COLOR       = 'blue', $
	             NAME        = 'Counts1 GDU1 Corrected', $
	             XTICKFORMAT = '(a1)', $
	             YTITLE      = 'Corr Counts1!CGDU1')
	
	;COUNTS2 GDU1
	p2 = MrPlot(t_gdu1, amb_data.counts2_gdu1, $
	            /CURRENT, $
	            NAME        = 'Counts2 GDU1', $
	            XTICKFORMAT = '(a1)', $
	            YTITLE      = 'Counts2!CGDU1')
	
	;Corrected
	p2c = MrPlot(t_gdu1, cal_data.counts2_gdu1_cal, $
	             /CURRENT, $
	             COLOR       = 'blue', $
	             NAME        = 'Counts2 GDU1 Corrected', $
	             XTICKFORMAT = '(a1)', $
	             YTITLE      = 'Corr Counts2!CGDU1')
	
	;COUNTS3 GDU1
	p3 = MrPlot(t_gdu1, amb_data.counts3_gdu1, $
	            /CURRENT, $
	            NAME        = 'Counts3 GDU1', $
	            XTICKFORMAT = '(a1)', $
	            YTITLE      = 'Counts3!CGDU1')
	p3c = MrPlot(t_gdu1, cal_data.counts3_gdu1_cal, $
	             /CURRENT, $
	             COLOR       = 'blue', $
	             NAME        = 'Counts3 GDU1 Corrected', $
	             XTICKFORMAT = '(a1)', $
	             YTITLE      = 'Corr Counts3!CGDU1')
	
	;COUNTS4 GDU1
	p4 = MrPlot(t_gdu1, amb_data.counts4_gdu1, $
	            /CURRENT, $
	            NAME        = 'Counts4 GDU1', $
	            XTICKFORMAT = '(a1)', $
	            YTITLE      = 'Counts4!CGDU1')
	p4c = MrPlot(t_gdu1, cal_data.counts4_gdu1_cal, $
	             /CURRENT, $
	             COLOR       = 'blue', $
	             NAME        = 'Counts4 GDU1 Corrected', $
	             XTICKFORMAT = 'time_labels', $
	             YTITLE      = 'Corr Counts4!CGDU1')

;-----------------------------------------------------
; Plot GDU2 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	win2  = MrWindow(XSIZE=600, YGAP=0.5, YSIZE=1000, REFRESH=0)
	
	;COUNTS1 GDU2
	p5 = MrPlot(t_gdu2, amb_data.counts1_gdu2, $
	            /CURRENT, $
	            NAME        = 'Counts1 GDU2', $
	            TITLE       = 'EDI Corrected Counts GDU2', $
	            XTICKFORMAT = '(a1)', $
	            YTITLE      = 'Counts1!CGDU1')
	p5c = MrPlot(t_gdu2, cal_data.counts1_gdu2_cal, $
	             /CURRENT, $
	             COLOR       = 'blue', $
	             NAME        = 'Counts1 GDU2 Corrected', $
	             XTICKFORMAT = '(a1)', $
	             YTITLE      = 'Corr Counts1!CGDU2')
	
	;COUNTS2 GDU2
	p6 = MrPlot(t_gdu2, amb_data.counts2_gdu2, $
	            /CURRENT, $
	            NAME        = 'Counts2 GDU2', $
	            XTICKFORMAT = '(a1)', $
	            YTITLE      = 'Counts2!CGDU2')
	p6c = MrPlot(t_gdu2, cal_data.counts2_gdu2_cal, $
	             /CURRENT, $
	             COLOR       = 'blue', $
	             NAME        = 'Counts2 GDU2 Corrected', $
	             XTICKFORMAT = '(a1)', $
	             YTITLE      = 'Corr Counts2!CGDU2')
	
	;COUNTS3 GDU2
	p7 = MrPlot(t_gdu2, amb_data.counts3_gdu2, $
	            /CURRENT, $
	            NAME        = 'Counts3 GDU2', $
	            XTICKFORMAT = '(a1)', $
	            YTITLE      = 'Counts3!CGDU2')
	p7c = MrPlot(t_gdu2, cal_data.counts3_gdu2_cal, $
	             /CURRENT, $
	             COLOR       = 'blue', $
	             NAME        = 'Counts3 GDU2 Corrected', $
	             XTICKFORMAT = '(a1)', $
	             YTITLE      = 'Corr Counts3!CGDU2')
	
	;COUNTS4 GDU2
	p8 = MrPlot(t_gdu2, amb_data.counts4_gdu2, $
	            /CURRENT, $
	            NAME        = 'Counts4 GDU2', $
	            XTICKFORMAT = '(a1)', $
	            YTITLE      = 'Counts4!CGDU2')
	p8c = MrPlot(t_gdu2, cal_data.counts4_gdu2_cal, $
	             /CURRENT, $
	             COLOR       = 'blue', $
	             NAME        = 'Counts4 GDU2 Corrected', $
	             XTICKFORMAT = 'time_labels', $
	             YTITLE      = 'Corr Counts4!CGDU2')
	
	
	;Create a legend
;	l1 = MrLegend(ALIGNMENT    = 'NE', $
;	              NAME         = 'GDU Legend', $
;	              LABEL        = ['Uncorrected', 'Corrected'], $
;	              SAMPLE_WIDTH = 0.0, $
;	              /RELATIVE, $
;	              POSITION     = [1.0, 1.0], $
;	              TARGET       = p1, $
;	              TEXT_COLOR   = ['Black', 'Blue'], $
;	              ORIENTATION  = 1)
	
	win1 -> Refresh
	win2 -> Refresh
	return, [win1, win2]
end
