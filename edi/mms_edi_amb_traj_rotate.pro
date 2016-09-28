; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_traj
;
; PURPOSE:
;+
;   Despin and rotate electron trajectory vectors into DBCS, GSE, and GSM coordinates.
;
; :Categories:
;    MMS, EDI
;
; :See Also:
;   mms_fdoa_read_defatt.pro
;   mms_dss_read_sunpulse.pro
;   mms_instr_xxyz2ocs.pro
;   mms_dss_xdespin.pro
;   mms_rot_despun2gse.pro
;   mms_rot_gse2gsm.pro
;   MrStruct_HasTag.pro
;   MrVector_Rotate.pro
;   
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
;       2016/09/14  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Rotate particle incident trajectory vectors to spherical coordinates in
;   DBCS, GSE and GSM.
;
; :Params:
;       TIME:           in, required, type=int64arr (cdf_time_tt2000)
;                       Time stamps of trajectory data.
;       TRAJ:           in, required, type=fltarr
;                       Particle trajectories in cartesian coordinates for a single channel.
;       EDI2BCS:        in, required, type=3x3 fltarr
;                       Rotation matrix from EDI to BCS coordinates.
;       DEFATT:         in, required, type=struct
;                       Definitive attitude data.
;       DSS:            in, required, type=struct
;                       Sunpulse data.
;
; :Returns:
;       RESULT:         out, required, type=struct
;                       Trajectory data in DBCS, GSE, GSM. Tags are::
;                           DBCS: Nx2 [time, coord] trajectory data in spherical DBCS.
;                           GSE:  Nx2 [time, coord] trajectory data in spherical GSE.
;                           GSM:  Nx2 [time, coord] trajectory data in spherical GSM.
;-
function mms_edi_amb_traj_rotate_channel, time, traj, edi2bcs, bcs2dbcs, defatt
	compile_opt idl2
	on_error, 2
	
	;EDI to BCS
	traj_bcs = MrVector_Rotate(edi2bcs, traj)

	;BCS to DBCS
	traj_dbcs = MrVector_Rotate(bcs2dbcs, traj_bcs )

	;DBCS to GSE
	traj_gse = mms_rot_despun2gse(defatt, time, traj_dbcs, TYPE='Z')
	
	;GSE to GSM
	traj_gsm = mms_rot_gse2gsm(time, traj_gse)
	
	;Cartesian to Spherical
	traj_bcs  = cv_coord(FROM_RECT=traj_bcs,  /TO_SPHERE, /DEGREES)
	traj_dbcs = cv_coord(FROM_RECT=traj_dbcs, /TO_SPHERE, /DEGREES)
	traj_gse  = cv_coord(FROM_RECT=traj_gse,  /TO_SPHERE, /DEGREES)
	traj_gsm  = cv_coord(FROM_RECT=traj_gsm,  /TO_SPHERE, /DEGREES)

	;Remove the radial coordinate
	traj_bcs  = traj_bcs[0:1,*]
	traj_dbcs = traj_dbcs[0:1,*]
	traj_gse  = traj_gse[0:1,*]
	traj_gsm  = traj_gsm[0:1,*]
	
	;Convert from elevation angle to polar angle
	traj_bcs[1,*]  = abs(traj_bcs[1,*]  - 90.0)
	traj_dbcs[1,*] = abs(traj_dbcs[1,*] - 90.0)
	traj_gse[1,*]  = abs(traj_gse[1,*]  - 90.0)
	traj_gsm[1,*]  = abs(traj_gsm[1,*]  - 90.0)

	;Output a structure of data
	data = { bcs:  temporary(traj_bcs), $
	         dbcs: temporary(traj_dbcs), $
	         gse:  temporary(traj_gse), $
	         gsm:  temporary(traj_gsm) $
	       }
	return, data
end


;+
;   Rotate particle incident trajectory vectors to spherical coordinates in
;   DBCS, GSE and GSM.
;
; :Params:
;       TIME:           in, required, type=int64arr (cdf_time_tt2000)
;                       Time stamps of trajectory data.
;       TRAJ:           in, required, type=struct
;                       Particle trajectories in cartesian coordinates for each channel
;                           and GDU. Tags are::
;                             TRAJ_GDU1  -  Trajectories for each channel of GDU1
;                             TRAJ_GDU2  -  Trajectories for each channel of GDU2
;       DSS_FILE:       in, required, type=string
;                       Name of file containing HK101 sunpulse data.
;       DEFATT_FILE:    in, required, type=string
;                       Name file containing definitive attitude data.
;
; :Returns:
;       OUT:            out, required, type=struct
;                       Trajectory data in DBCS, GSE, GSM. Tags are::
;                           DBCS_GDU1: [coord, time, channel] Trajectory data for GDU1 in DBCS.
;                           GSE_GDU1:  [coord, time, channel] Trajectory data for GDU1 in GSE.
;                           GSM_GDU1:  [coord, time, channel] Trajectory data for GDU1 in GSM.
;                           DBCS_GDU2: [coord, time, channel] Trajectory data for GDU2 in DBCS.
;                           GSE_GDU2:  [coord, time, channel] Trajectory data for GDU2 in GSE.
;                           GSM_GDU2:  [coord, time, channel] Trajectory data for GDU2 in GSM.
;-
function mms_edi_amb_traj_rotate, time, traj, dss_file, defatt_file
	compile_opt idl2
	on_error, 2
	
	;Burst data?
	dims      = size(traj.traj_gdu1, /DIMENSIONS)
	tf_brst   = n_elements(dims) eq 3
	nPts      = dims[1]
	nChannels = tf_brst ? dims[2] : 1
	
;-----------------------------------------------------
; Coordinate Transform Data \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Read data
	dss    = mms_dss_read_sunpulse(dss_file)
	defatt = mms_fdoa_read_defatt(defatt_file)
	
	;EDI --> BCS
	edi1_to_bcs = mms_instr_xxyz2ocs('EDI1')
	edi2_to_bcs = mms_instr_xxyz2ocs('EDI2')
	
	;Despin matrices
	bcs2dbcs = mms_dss_xdespin(dss, time)

;-----------------------------------------------------
; Transform Each Channel \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Output dimensions
	;   [time, component, channel]
	outdims   = [2, nPts, nChannels]
	
	;Allocate memroy
	traj_bcs_gdu1  = fltarr(outdims)
	traj_dbcs_gdu1 = fltarr(outdims)
	traj_gse_gdu1  = fltarr(outdims)
	traj_gsm_gdu1  = fltarr(outdims)
	traj_bcs_gdu2  = fltarr(outdims)
	traj_dbcs_gdu2 = fltarr(outdims)
	traj_gse_gdu2  = fltarr(outdims)
	traj_gsm_gdu2  = fltarr(outdims)

	;Rotate each channel
	for i = 0, nChannels - 1 do begin
		;GDU1
		temp_traj = mms_edi_amb_traj_rotate_channel(time, traj.traj_gdu1[*,*,i], edi1_to_bcs, bcs2dbcs, defatt)
		traj_bcs_gdu1[0,0,i]  = temp_traj.bcs
		traj_dbcs_gdu1[0,0,i] = temp_traj.dbcs
		traj_gse_gdu1[0,0,i]  = temp_traj.gse
		traj_gsm_gdu1[0,0,i]  = temp_traj.gsm
		temp_traj             = 0B
	
		;GDU2
		temp_traj = mms_edi_amb_traj_rotate_channel(time, traj.traj_gdu2[*,*,i], edi2_to_bcs, bcs2dbcs, defatt)
		traj_bcs_gdu2[0,0,i]  = temp_traj.bcs
		traj_dbcs_gdu2[0,0,i] = temp_traj.dbcs
		traj_gse_gdu2[0,0,i]  = temp_traj.gse
		traj_gsm_gdu2[0,0,i]  = temp_traj.gsm
		temp_traj             = 0B
	endfor
	
;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Combine into structure
	result = { traj_bcs_gdu1:  temporary(traj_bcs_gdu1), $
	           traj_dbcs_gdu1: temporary(traj_dbcs_gdu1), $
	           traj_gse_gdu1:  temporary(traj_gse_gdu1), $
	           traj_gsm_gdu1:  temporary(traj_gsm_gdu1), $
	           traj_bcs_gdu2:  temporary(traj_bcs_gdu2), $
	           traj_dbcs_gdu2: temporary(traj_dbcs_gdu2), $
	           traj_gse_gdu2:  temporary(traj_gse_gdu2), $
	           traj_gsm_gdu2:  temporary(traj_gsm_gdu2) $
	         }
	return, result
end