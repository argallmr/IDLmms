PROGRAM FLOW
======================
edi_piso
  .
  .
  .
  ep_prep_runest
  runest
  ep_analyze_runest
  .
  .
  .
  edi_piso_onechunk
    ep_calc_beamwidth
    ep_construct_scs2bpp
  whatech
  bestarg
    ep_prep_order
      ep_calc_runner_penalty
    ep_method_logic_rmt_sa
      ep_toaw_sort
        ep_hav_sorter
          BeamPhaseSort
      ep_richmans_tof_sa
        ToFError
      ep_recalc_aerror



NAME                       DESCRIPTION
==========================================================================================
edi_piso               --- Process raw EDI beam files and create an E-field data product
ep_prep_runest         --- Prepare data for runest. Selects data that is
                              a) Not a fill value
                              b) Meets minimum quality requirements
                              c) Has MAXCHAN == 7
                           Created from data in EDI_PISO_ONEHOUR_COMMON and combines GDU1 and
                           GDU2 data into single arrays.
runest                 --- Assigns runner order to each beam
ep_analyze_runest      --- Sort data according to how they were used in RUNEST. Create
                           a structure of the 
edi_piso_onechunk      --- Processes "one chunk" of data.
ep_calc_beamwidth      --- Calculates width of beam in BPP
ep_construct_scs2bpp   --- Create a transformation matrix from SCS to BPP
whatech                --- Determines the maximum radius of the grid used for determining
                           the triangulation drift step.
bestarg                --- Computes drift step using triangulation and time-of-flight methods
ep_prep_order          --- Assigns beam class: "A", "B", "C", "D", "F", "G", "H"
ep_calc_runner_penalty --- Assigns runner penalty equal to the number of runner orders
                           with probability within 1/e of the maximum probability.
ep_method_logic_rmt_sa --- Richman's time-of-flight method. All time of flight analysis
                           occurs within this routine.
ep_toaw_sort           --- Takes the results of EP_HAV_SORTER and re-computes the average
                           beam firing direction and its standard deviation. Contains a
                           lot of work that is already done in EP_HAV_SORTER
ep_hav_sorter          --- Iteratively hones in on the mean "toward" and "away" firing
                           angles, with a 180 degree ambiguity.
BeamPhaseSort          --- One iteration to determine "toward" and "away"
ep_richmans_tof_sa     --- The actual time-of-flight analsys. Returns a 6-element array with
                               0 = drift step magnitude, meters
                               1 = drift step magnitude error, meters
                               2 = drift step azimuthal angle, radians
                               3 = drift step azimuthal angle error, radians
                               4 = gyrotime, micro-seconds
                               5 = gyrotime error, micro-seconds
ToFError               --- Computes ToF error to within certain confidence levels.
ep_recalc_aerror       --- Calcutes the standard deviation between angle from gun to
                           target and the actual firing angle.
