pro ep_define_worthiness, $
	beam

	common ep_envar_con_cb

	; This code only defines those beams worthy for Poorman's TOF analysis
	; Worthiness for TRI is defined in ep_choose_tri_beams.pro
	; Worthiness for Richman's TOF is defined in ep_method_logic_rmt_sa.pro

	;============================================================
	; Poorman's ToF-worthy beams:
	; 1) maxchan = 7
	; 2) beam quality = 2 or 3
	; 3) not triangulation outlier (out ne 2)
	; 4) Class "A" and "B" only
	; Note: code_type not checked here (but it is for Richman's TOF)

	id_pmt = where $
		(beam.maxchan eq 7 and                   $ ; Correlator channel with max. signal = 7
		beam.qual ge 2 and                       $ ; Quality = 2 or 3
		beam.out ne 2 and                        $ ; Not a triangulation outlier
		(beam.class eq 'A' or beam.class eq 'B'))

	if (id_pmt(0) ne -1) then $
		beam.pmt_ok (id_pmt) = 1

	return
end
