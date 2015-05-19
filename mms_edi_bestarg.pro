; docformat = 'rst'
;
; NAME:
;       mms_edi_bestarg
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
;   Prepare EDI data for input into bestarg.pro
;
; :Categories:
;   MMS, EDI, Bestarg
;
; :Returns:
;       WIN:        Graphics window containing the plot of EDI beams.
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
;       2015/05/05  -   Written by Matthew Argall
;-
function mms_edi_bestarg, edi_files, fg_l1b_files, fg_ql_files, $
TSTART=tstart, $
TEND=tend
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Get Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Get FG data
	fg_l1b = mms_fg_read_l1b(fg_l1b_files, TSTART=tstart, TEND=tend)
	fg_ql  = mms_fg_read_ql(fg_ql_files,   TSTART=tstart, TEND=tend)

	;Get EDI data
	edi = mms_edi_gse(sc, 'slow', tstart, tend, /DMPA, /EDI, $
	                  DIRECTORY    = edi_dir, $
	                  QUALITY      = quality, $
	                  SUNPULSE_DIR = sunpulse_dir)

;-----------------------------------------------------
; Beam Widths \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Interpolate the magnetic field in BCS onto beam times
	;   - There will be a 1-to-1 correspondence between field and beams
	avg = mms_edi_bavg(fg_l1b.epoch, fg_l1b.b_bcs, edi.epoch_gd12, edi.epoch_gd21)

	;Rotate the firing vectors and magnetic field into the EDI CS
	bcs2edi1 = mms_instr_xxyz2instr('BCS', 'EDI1')
	bcs2edi2 = mms_instr_xxyz2instr('BCS', 'EDI2')
	
	;Rotate Field to EDI coordinate system
	b_edi1 = MrVector_Rotate(bcs2edi1, avg.b_gd12)
	b_edi2 = MrVector_Rotate(bcs2edi2, avg.b_gd21)
	avg    = !Null
	
	;Beam widths
	beam_width_gd12 = mms_edi_beam_width(edi.fv_gd12, b_edi1, ALPHA=beam_alpha_gd12)
	beam_width_gd21 = mms_edi_beam_width(edi.fv_gd21, b_edi2, ALPHA=beam_alpha_gd21)
	
	;Clear the data
	fg_l1b = !Null

;-----------------------------------------------------
; Get Beams Associated with each B_AVG \\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Average DMPA magnetic field
	;   - Interpolate the field onto beam times.
	;   - Average over 5-second intervals.
	;   - Keep track of which beams are associated with each averaged field.
	avg = mms_edi_bavg(fg_ql.epoch, fg_ql.b_dmpa, edi.epoch_gd12, edi.epoch_gd21)

	;Look at each averaging interval
	navg = n_elements(avg.t_avg)

	;Figure out which beams correspond to which average value
	hist1 = histogram(avg.inds_gd12, MIN=0, MAX=navg-1, BINSIZE=1, REVERSE_INDICES=ri1)
	hist2 = histogram(avg.inds_gd21, MIN=0, MAX=navg-1, BINSIZE=1, REVERSE_INDICES=ri2)

;-----------------------------------------------------
; Draw Each Bavg Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Step through each averaging interval
	for i = 0, navg - 2 do begin
		;Number of beams used to compute B_avg
		n1 = ri1[ri1[i+1]] - ri1[ri1[i]]
		n2 = ri2[ri2[i+1]] - ri2[ri1[i]]
		
		;If no beams, continue to next time interval.
		if n1 eq 0 && n2 eq 0 then continue
		
		;Extract data
		t_avg = avg.t_avg[*,i]
		b_avg = avg.b_avg[*,i]
		b_std = avg.b_std[*,i]
		
	;-----------------------------------------------------
	; Allocate Memory \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		nBeams         = n1 + n2
		beam_time      = lon64arr(nBeams)     ; Epoch time of beam
		beam_b         = fltarr(nBeams)       ; Magnetic field vector associated with beam
		beam_gunid     = bytarr(nBeams)       ; Gun Number {1 | 2}
		beam_xg        = fltarr(nBeams)       ; X-coordinate of gun in BPP
		beam_yg        = fltarr(nBeams)       ; Y-Coordinate of gun in BPP
		beam_alpha     = fltarr(nBeams)       ; Angle between (fv x B) and e_polar
		beam_tof       = lonarr(nBeams)       ; Time of flight (micro-seconds)
		beam_code_type = lonarr(nBeams)       ; Beam code type (raw num_chips)
		beam_qual      = bytarr(nBeams)       ; Quality
		beam_bwidth    = fltarr(nBeams)       ; Beam width in BPP
		beam_runstat   = fltarr(nBeams)       ; ????
		beam_maxchan   = lonarr(nBeams)       ; Max Channel/Address
		beam_btime     = lon64arr(nBeams)     ; Beam times
		beam_tchip     = lonarr(nBeams)       ; Chip period (a.k.a. chip width)
		beam_tcode     = lonarr(nBeams)       ; Code period (a.k.a. code length)
		num_chips      = intarr(nBeams)       ; Number of chips used in the code
		erg            = fltarr(nBeams)       ; Energy
		rmax           = fltarr(nBeams)       ; ????
		gyrorad        = fltarr(nBeams)       ; Gyroradius
		gyroper        = fltarr(nBeams)       ; Gyroperiod
		flip           = bytarr(nBeams)       ; ????
		bmag           = fltarr(nBeams)       ; Magnetic field magnitude
		poc            = intarr(nBeams)       ; ???
		
		;
		; Unknown value defaults
		;    RUNSTAT = 0B
		;    RMAX    = 15.0
		;    POC     = 2B
		;    FLIP    = 0B
		;
		
	;-----------------------------------------------------
	; GD12 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if n1 gt 0 then begin
			;Get the beam indices
			inds1 = ri1[ri1[i]:ri1[i+1]-1]
			
			;Extract data
			beam_time[0:n1-1]    = edi.epoch_gd12[inds1]
			beam_b[*,0:n1-1]     = avg.b_gd12[*,inds1]
			beam_gunid[0:n1-1]   = 1B
			beam_xg[0:n1-1]      = edi.virtual_gun1_dmpa[0,inds1]
			beam_yg[0:n1-1]      = edi.virtual_gun1_dmpa[0,inds1]
			beam_alpha[0:n1-1]   = beam_alpha_gd12[inds1]
			beam_tof[0:n1-1]     = edi.tof_gd12[inds1]
			beam_qual[0:n1-1]    = edi.quality_gd12[inds1]
			beam_bwidth[0:n1-1]  = beam_width_gd12[inds1]
			beam_runstat[0:n1-1] = 0B
			beam_maxchan[0:n1-1] = edi.max_addr_gd12[inds1]
			beam_btime[0:n1-1]   = edi.epoch_gd12[inds1]
			num_chips[0:n1-1]    = edi.num_chips_gd12[inds1]
			beam_tchip[0:n1-1]   = edi.chip_width_gd12[inds1]
			beam_tcode[0:n1-1]   = edi.code_length_gd12[inds1]
			erg[0:n1-1]          = edi_energy_gd12[inds1]
			rmax[0:n1-1]         = 15.0
			flip[0:n1-1]         = 0B
			poc[0:n1-1]          = 2B
			
			;Translate back to raw code type
			beam_code_type[0:n1-1] = 0S * (num_chips eq  255S) + $
			                              (num_chips eq  511S) + $
			                         3S * (num_chips eq 1023S)
			
			;Transformation to BPP
			xyz2bpp = mms_edi_xxyz2bpp(b_gd12)
			
			;Rotate to BPP
			fv_gd12_bpp  = MrVector_Rotate(xyz2bpp, fv_gd12)
			gun1_pos_bpp = MrVector_Rotate(xyz2bpp, gun1_pos_bpp)
		endif
		
	;-----------------------------------------------------
	; GD21 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if n2 gt 0 then begin
			;Get the beam indices
			inds2 = ri2[ri2[i]:ri2[i+1]-1]
			
			;Extract data
			beam_time[n1:n2-1]      = edi.epoch_gd21[inds2]
			beam_b[*,n1:n2-1]       = avg.b_gd21[*,inds2]
			beam_gunid[n1:n2-1]     = 2B
			beam_xg[n1:n2-1]        = edi.virtual_gun1_dmpa[0,inds2]
			beam_yg[n1:n2-1]        = edi.virtual_gun1_dmpa[0,inds2]
			beam_alpha[n1:n2-1]     = beam_alpha_gd12[inds2]
			beam_tof[n1:n2-1]       = edi.tof_gd21[inds2]
			beam_qual[n1:n2-1]      = edi.quality_gd21[inds2]
			beam_bwidth[n1:n2-1]    = beam_width_gd12[inds1]
			beam_runstat[n1:n2-1]   = 0B
			beam_maxchan[n1:n2-1]   = edi.max_addr_gd21[inds2]
			beam_btime[n1:n2-1]     = edi.epoch_gd21[inds2]
			num_chips[n1:n2-1]      = edi.num_chips_gd21[inds2]
			beam_tchip[n1:n2-1]     = edi.chip_width_gd21[inds2]
			beam_tcode[n1:n2-1]     = edi.code_length_gd21[inds2]
			erg[n1:n2-1]            = edi_energy_gd21[inds2]
			rmax[n1:n2-1]           = 15.0
			flip[n1:n2-1]           = 0B
			poc[n1:n2-1]            = 2B
			
			;Translate back to raw code type
			beam_code_type[n1:n2-1] = 0S * (num_chips eq  255S) + $
			                               (num_chips eq  511S) + $
			                          3S * (num_chips eq 1023S)
			
			;Transformation to BPP
			xyz2bpp = mms_edi_xxyz2bpp(b_gd21)
			
			;Rotate to BPP
			fv_gd21_bpp  = MrVector_Rotate(xyz2bpp, fv_gd21)
			gun2_pos_bpp = MrVector_Rotate(xyz2bpp, gun2_pos_bpp)
		endif
		
	;-----------------------------------------------------
	; Generate Data for Bestart \\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Other quantitues
		; bmag   = |B| = sqrt( Bx^2 + By^2 + Bz^2 )
		; T      = 2 pi m / (q |B|)
		; r      = m v_perp / ( q |B| )
		; v_perp = sqrt( 2 E / m )
		;
		;Unit conversions
		; T      = kg / C-nT --> kg-C-s / C-kg * 1e9 --> s * 1e9
		; r      = kg km/s / C-nT --> kg-m-C-s / C-kg-s * 1e3 * 1e9 --> m * 1e12
		; v_perp = eV / kg --> J / kg * 1.602e-19 --> kg-m / s-kg * 1.602e-19 --> m/s * 1.602e-19 -> km/s * 1.602e-19 * 1e-3
		bmag    = MrVector_Magnitude(beam_b)
		gyroper = ( 1e9 * 2 * !pi * constants('m_e') / constants('q') ) / bmag
		v_perp  = sqrt( ( 2.0 * constants('q') / constants('m_e') ) * erg )
		gyrorad = ( 1e12 * constants('m_e') / constants('q') ) * (v_perp / bmag)
		
	;-----------------------------------------------------
	; Bestarg \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		bestarg, nBeams, beam_gunid, beam_out, beam_xg, beam_yg, $ ; IN
		         beam_alpha, beam_tof, beam_code_type, beam_qual, $ ; IN
		         beam_bwidth, beam_runstat, $ ; IN
		         beam_maxchan, beam_btime, $ ; IN
		         beam_tchip, beam_tcode, $
		         erg,rmax,gyrorad,gyroper,flip, $ ; IN
		         bmag=bmag, $       ; IN
		         keyplot3=keyplot3, $ ; IN
		         poc=poc, $         ; IN
		         plot_title=kp3_title
	endfor
	
	
	return, win
end
