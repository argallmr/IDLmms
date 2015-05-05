pro ep_choose_tri_beams, $
	beam, TRI_OK

	common ep_envar_con_cb

	;============================================================
	; Triangulation-worthy beams:
	; 1) maxchan = 7
	; 2) beam quality = 1, 2 or 3
	; 3) not triangulation outlier (out ne 2)
	; 4) Class "A" or "B" only
	; 5) short or long code (no check on code type)

	beam.tri_ok (*) = 0 ; Initialize

	id_tri = where ( $
		beam.maxchan eq 7 and                   $ ; Correlator channel with max. signal = 7
		(beam.qual gt 0 and beam.qual le 3) and  $ ; Quality = 1, 2 or 3
		beam.out ne 2 and                        $ ; Not a triangulation outlier
		(beam.class eq 'A' or beam.class eq 'B'))

	if (id_tri(0) ne -1) then $
		beam.tri_ok (id_tri) = 1

	TRI_OK = 1
	if (n_elements (where (beam.tri_ok eq 1)) lt pp_nbeam_min) then $
		TRI_OK = 0

	return
end
