MODULE WAM_OUTPUT_PARAMETER_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS: OUTPUT PARAMTER NAMES AND SCALES FOR PRINTING.       !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !
USE WAM_FILE_MODULE,  ONLY: IU05, FILE05, IU06, FILE06, ITEST
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE

public:: initialize_integrated_parameters
! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. NUMBER OF INTEGRATED PARAMETER.                                       !
!        -------------------------------                                       !

INTEGER, PARAMETER :: NOUT_P = 70
logical            :: dir_true = .true. , dir_false = .false.

CHARACTER(LEN=60), DIMENSION(NOUT_P) :: NAME_IP = (/ &
& 'WIND_SPEED_U10 ' , & !!1
& 'WIND_DIRECTION ' , & !!2
& 'FRICTION_VELOCITY ' , & !!3
& 'DRAG_COEFFICIENT ' , & !!4
& 'CHARNOCK_PARAMETER ' , & !!5
& 'WATER_DEPTH ' , & !!6
& 'CURRENT_SPEED ' , & !!7
& 'CURRENT_DIRECTION ' , & !!8
& 'SIGNIFICANT_WAVE_HEIGHT ' , & !!9
& 'WAVE_PEAK_PERIOD ' , & !!10
& 'WAVE_MEAN_PERIOD ' , & !!11
& 'WAVE_TM1_PERIOD ' , & !!12
& 'WAVE_TM2_PERIOD ' , & !!13
& 'WAVE_DIRECTION ' , & !!14
& 'DIRECTIONAL_SPREAD ' , & !!15
& 'NORMALISED_WAVE_STRESS ' , & !!16
& 'SEA_SIGNIFICANT_WAVE_HEIGHT ' , & !!17
& 'SEA_PEAK_PERIOD ' , & !!18
& 'SEA_MEAN_PERIOD ' , & !!19
& 'SEA_TM1_PERIOD ' , & !!20
& 'SEA_TM2_PERIOD ' , & !!21
& 'SEA_DIRECTION ' , & !!22
& 'SEA_DIRECTIONAL_SPREAD ' , & !!23
& 'DUMMY1 ' , & !!24
& 'SWELL_SIGNIFICANT_WAVE_HEIGHT ' , & !!25
& 'SWELL_PEAK_PERIOD ' , & !!26
& 'SWELL_MEAN_PERIOD ' , & !!27
& 'SWELL_TM1_PERIOD ' , & !!28
& 'SWELL_TM2_PERIOD ' , & !!29
& 'SWELLDIRECTION ' , & !!30
& 'SWELL_DIRECTIONAL_SPREAD ' , & !!31
& 'ROUGHNESS_LENGTH_Z0 ' , & !!32
& 'GODA_PEAKEDNESS_PARAMETER ' , & !!33
& 'KURTOSIS ' , & !!34
& 'BENJAMIN-FEIR_INDEX ' , & !!35
& 'NORMALIZED_MAXIMUM_WAVE_HEIGHT ' , & !!36
& 'MAXIMUM_WAVE_PERIOD ' , & !!37
& 'PEAK_FREQUENCY ' , & !!38
& 'PEAK_DIRECTION ' , & !!39
& 'MEAN_SQUARE_SLOPE ' , & !!40
& 'FIRST_SWELL_SIGNIFICANT_WAVE_HEIGHT ' , & !!41
& 'FIRST_SWELL_TM1_PERIOD ' , & !!42
& 'FIRST_SWELL_DIRECTION ' , & !!43
& 'SECOND_SWELL_SIGNIFICANT_WAVE_HEIGHT ' , & !!44
& 'SECOND_SWELL_TM1_PERIOD ' , & !!45
& 'SECOND_SWELL_DIRECTION ' , & !!46
& 'THIRD_SWELL_SIGNIFICANT_WAVE_HEIGHT ' , & !!47
& 'THIRD_SWELL_TM1_PERIOD ' , & !!48
& 'THIRD_SWELL_DIRECTION ' , & !!49
& 'DUMMY2' , & !!50
& 'RADIATION_STRESS_TENSOR_SXX ' , & !!51
& 'RADIATION_STRESS_TENSOR_SYY ' , & !!52
& 'RADIATION_STRESS_TENSOR_SXY ' , & !!53
& 'DUMMY3' , & !!54
& 'X-COMP._WAVE_FORCE_PER_SURFACE_UNIT ' , & !!55
& 'Y-COMP._WAVE_FORCE_PER_SURFACE_UNIT ' , & !!56
& 'X-COMP._STOKES_DRIFT ' , & !!57
& 'Y-COMP._STOKES_DRIFT ' , & !!58
& 'ENERGY_FLUX_TO_OCEAN ' , & !!59
& 'TOTAL_ENERGY_FLUX_FROM_WIND_TO_WAVES ' , & !!60
& 'X-COMP._MOMENTUM_FLUX_INTO_OCEAN ' , & !!61
& 'Y-COMP._MOMENTUM_FLUX_INTO_OCEAN ' , & !!62
& 'ENERGY_FLUX_FROM_WAVES_TO_BOTTOM ' , & !!63
& 'X-COMP._MOMENTUM_FLUX_FROM_WAVES_TO_BOTTOM ' , & !!64
& 'Y-COMP._MOMENTUM_FLUX_FROM_WAVES_TO_BOTTOM ' , & !!65
& 'DUMMY4' , & !!66
& 'CREST_MAX ' , & !!67
& 'HMAX ' , & !!68
& 'MAXIMUM_CREST_H.-SPACE-TIME ' , & !!69
& 'MAXIMUM_WAVE_H.-SPACE-TIME '/) !!70

CHARACTER(LEN=15), DIMENSION(NOUT_P) :: UNITS_P = (/ &
& 'm/s '  , & !! 1
& 'degree '  , & !! 2
& 'm/s '  , & !! 3
& ' '  , & !! 4
& ' '  , & !! 5
& 'm '  , & !! 6
& 'm/s '  , & !! 7
& 'degree '  , & !! 8
& 'm '  , & !! 9
& 's '  , & !! 10
& 's '  , & !! 11
& 's '  , & !! 12
& 's '  , & !! 13
& 'degree '  , & !! 14
& 'degree '  , & !! 15
& ' '  , & !! 16
& 'm '  , & !! 17
& 's '  , & !! 18
& 's '  , & !! 19
& 's '  , & !! 20
& 's '  , & !! 21
& 'degree '  , & !! 22
& 'degree '  , & !! 23
& ' ' , & !! 24
& 'm '  , & !! 25
& 's '  , & !! 26
& 's '  , & !! 27
& 's '  , & !! 28
& 's '  , & !! 29
& 'degree '  , & !! 30
& 'degree '  , & !! 31
& 'm '  , & !! 32
& ' '  , & !! 33
& ' '  , & !! 34
& ' '  , & !! 35
& 'm '  , & !! 36
& 's '  , & !! 37
& 's '  , & !! 38
& 'degree '  , & !! 39
& ' '  , & !! 40
& 'm '  , & !! 41
& 'm/s '  , & !! 42
& 'degree '  , & !! 43
& 'm '  , & !! 44
& 'm/s '  , & !! 45
& 'degree '  , & !! 46
& 'm '  , & !! 47
& 'm/s '  , & !! 48
& 'degree '  , & !! 49
& ' ' , & !! 50
& 'kg/s/s '  , & !! 51
& 'kg/s/s '  , & !! 52
& 'kg/s/s '  , & !! 53
& ' ' , & !! 54
& 'n/m/m '  , & !! 55
& 'n/m/m '  , & !! 56
& 'm/s '  , & !! 57
& 'm/s '  , & !! 58
& 'kg/s/s/s '  , & !! 59
& 'kg/s/s/s '  , & !! 60
& 'kg/m/s/s '  , & !! 61
& 'kg/m/s/s '  , & !! 62
& 'kg/s/s/s '  , & !! 63
& 'kg/m/s/s '  , & !! 64
& 'kg/m/s/s '  , & !! 65
& ' ' , & !! 66
& 'm '  , & !! 67
& 'm '  , & !! 68
& 'm '  , & !! 69
& 'm '  /) !! 70

CHARACTER(LEN=100), DIMENSION(NOUT_P) :: STANDARD_NAME_P = (/ &
& 'wind_speed ' , & !!1
& 'wind_from_direction ' , & !!2
& 'friction_velocity_at_sea_water_surface ' , & !!3
& 'surface_drag_coefficient_in_air ' , & !!4
& 'charnock_coefficient_for_surface_roughness_length_for_momentum_in_air ' , & !!5
& 'sea_floor_depth_below_sea_surface ' , & !!6
& 'sea_water_velocity_to_direction ' , & !!7
& 'sea_water_speed ' , & !!8
& 'sea_surface_wave_significant_height ' , & !!9
& 'sea_surface_wave_period_at_variance_spectral_density_maximum ' , & !!10
& 'sea_surface_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment ' , & !!11
& 'sea_surface_wave_mean_period_from_variance_spectral_density_first_frequency_moment ' , & !!12
& 'sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment ' , & !!13
& 'sea_surface_wave_to_direction ' , & !!14
& 'sea_surface_wave_directional_spread ' , & !!15
& 'normalised_wave_stress ' , & !!16
& 'sea_surface_wind_wave_significant_height ' , & !!17
& 'sea_surface_wind_wave_period_at_variance_spectral_density_maximum ' , & !!18
& 'sea_surface_wind_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment ' , & !!19
& 'sea_surface_wind_wave_mean_period_from_variance_spectral_density_first_frequency_moment ' , & !!20
& 'sea_surface_wind_wave_mean_period_from_variance_spectral_density_second_frequency_moment ' , & !!21
& 'sea_surface_wind_wave_to_direction ' , & !!22
& 'sea_surface_wind_wave_directional_spread ' , & !!23
& 'dummy ' , & !!24
& 'sea_surface_swell_wave_significant_height ' , & !!25
& 'sea_surface_swell_wave_period_at_variance_spectral_density_maximum ' , & !!26
& 'sea_surface_swell_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment ' , & !!27
& 'sea_surface_swell_wave_mean_period_from_variance_spectral_density_first_frequency_moment ' , & !!28
& 'sea_surface_swell_wave_mean_period_from_variance_spectral_density_second_frequency_moment ' , & !!29
& 'sea_surface_swell_wave_to_direction ' , & !!30
& 'sea_surface_swell_wave_directional_spread ' , & !!31
& 'surface_roughness_length ' , & !!32
& 'goda_peakness_parameter ' , & !!33
& 'kurtosis ' , & !!34
& 'benjamin_feir_index ' , & !!35
& 'sea_surface_wave_maximum_height ' , & !!36
& 'sea_surface_wave_maximum_period ' , & !!37
& 'sea_surface_wave_frequency_at_variance_spectral_density_maximum ' , & !!38
& 'sea_surface_wave_from_direction_at_variance_spectral_density_maximum ' , & !!39
& 'sea_surface_wave_mean_square_slope ' , & !!40
& 'sea_surface_primary_swell_wave_significant_height ' , & !!41
& 'sea_surface_primary_swell_wave_mean_period ' , & !!42
& 'sea_surface_primary_swell_wave_to_direction ' , & !!43
& 'sea_surface_secondary_swell_wave_significant_height ' , & !!44
& 'sea_surface_secondary_swell_wave_mean_period ' , & !!45
& 'sea_surface_secondary_swell_wave_to_direction ' , & !!46
& 'sea_surface_tertiary_swell_wave_significant_height ' , & !!47
& 'sea_surface_tertiary_swell_wave_mean_period ' , & !!48
& 'sea_surface_tertiary_swell_wave_to_direction ' , & !!49
& 'dummy ' , & !!50
& 'sea_surface_wave_xx_radiation_stress ' , & !!51
& 'sea_surface_wave_xy_radiation_stress ' , & !!52
& 'sea_surface_wave_yy_radiation_stress ' , & !!53
& 'dummy ' , & !!54
& 'wave_x_force_per_surface_unit ' , & !!55
& 'wave_y_force_per_surface_unit ' , & !!56
& 'sea_surface_wave_stokes_drift_x_velocity ' , & !!57
& 'sea_surface_wave_stokes_drift_y_velocity ' , & !!58
& 'wave_mixing_energy_flux_into_sea_water ' , & !!59
& 'total_energy_flux_from_wind_to_waves ' , & !!60
& 'eastward_wave_momentum_flux_into_sea_water ' , & !!61
& 'northward_wave_momentum_flux_into_sea_water ' , & !!62
& 'energy_flux_from_waves_to_bottom ' , & !!63
& 'eastward_wave_momentum_flux_into_from_waves_to_bottom ' , & !!64
& 'westward_wave_momentum_flux_into_from_waves_to_bottom ' , & !!65
& 'dummy ' , & !!66
& 'sea_surface_wave_maximum_crest_height ' , & !!67
& 'sea_surface_wave_maximum_height ' , & !!68
& 'sea_surface_wave_maximum_crest_height ' , & !!69
& 'sea_surface_wave_maximum_height ' /)!!70

CHARACTER(LEN=100), DIMENSION(NOUT_P) :: LONG_NAME_P = (/ &
& 'Equivalent 10-m wind speed derived from altimeter', &  !! 1
& 'Wind degrees from north', &  !! 2
& 'Friction velocity at sea water surface', &  !! 3
& 'Drag coefficient', &  !! 4
& 'Charnock coefficient', &  !! 5
& 'Sea floor depth below sea surface', &  !! 6
& 'Sea water velocity to direction', &  !! 7
& 'Sea water speed', &  !! 8
& 'Spectral significant wave height (Hm0)', &  !! 9
& 'Wave period at spectral peak / peak period (Tp)', &  !! 10
& 'Spectral moments (-1,0) wave period (Tm-10)', &  !! 11
& 'Spectral moments (0,1) wave period (Tm01)', &  !! 12
& 'Spectral moments (0,2) wave period (Tm02)', &  !! 13
& 'Mean wave direction from (Mdir)', &  !! 14
& 'Total directional spreed', &  !! 15
& 'normalised_wave_stress', &  !! 16
& 'Spectral significant wind wave height', &  !! 17
& 'Sea peak period', &  !! 18
& 'Sea mean period', &  !! 19
& 'Spectral moments (0,1) wind wave period', &  !! 20
& 'Sea m2-period', &  !! 21
& 'Mean wind wave direction from', &  !! 22
& 'Sea directional spreed', &  !! 23
& 'Dummy', &  !! 24
& 'Swell significant wave height', &  !! 25
& 'Swell peak period', &  !! 26
& 'Swell mean period', &  !! 27
& 'Swell m1-period', &  !! 28
& 'Swell tm2-period', &  !! 29
& 'Swell mean wave direction', &  !! 30
& 'Swell directional spread', &  !! 31
& 'Surface roughness length', &  !! 32
& 'goda peakness parameter', &  !! 33
& 'kurtosis', &  !! 34
& 'Benjamin Feir index', &  !! 35
& 'Maximum zero crossing wave height (Hmax)', &  !! 36
& 'Maximum wave period (Tmax)', &  !! 37
& 'interpolated peak frequency', &  !! 38
& 'Wave principal direction at spectral peak', &  !! 39
& 'mean square slope', &  !! 40
& 'Spectral significant primary swell wave height', &  !! 41
& 'Spectral moments (0,1) primary swell wave period', &  !! 42
& 'Mean primary swell wave direction from', &  !! 43
& 'Spectral significant secondary swell wave height', &  !! 44
& 'Spectral moments (0,1) secondary swell wave period', &  !! 45
& 'Mean secondary swell wave direction from', &  !! 46
& 'Sea surface tertiary swell wave significant height', &  !! 47
& 'Sea surface tertiary swell wave mean period', &  !! 48
& 'Sea surface tertiary swell wave to direction', &  !! 49
& 'Dummy', &  !! 50
& 'radiation stress tensor sxx', &  !! 51
& 'radiation stress tensor syy', &  !! 52
& 'radiation stress tensor sxy', &  !! 53
& 'Dummy', &  !! 54
& 'x-comp. wave force per surface unit', &  !! 55
& 'y-comp. wave force per surface unit', &  !! 56
& 'Stokes drift U', &  !! 57
& 'Stokes drift V', &  !! 58
& 'Energy flux into ocean', &  !! 59
& 'normalized energy flux from wind to waves', &  !! 60
& 'Eastward wave momentum flux into sea water', &  !! 61
& 'Northward wave momentum flux into sea water', &  !! 62
& 'energy flux from waves to bottom', &  !! 63
& 'x-comp. momentum flux from waves into bottom', &  !! 64
& 'y-comp. momentum flux from waves into bottom', &  !! 65
& 'Dummy', &  !! 66
& 'Height of the highest crest'  , &  !! WAM-MAX  !! 67
& 'Maximum crest trough wave height (Hc,max)' , &  !! WAM-MAX  !! 68
& 'maximum crest height - space-time (stqd)' , &   !! WAM-MAX  !! 69
& 'maximum wave height - space-time (stqd)' /)    !! WAM-MAX  !! 70

REAL, DIMENSION(NOUT_P) :: VARIABLE_MIN = (/ &
0.,0.,0.,0.,0.,0.,-2.,0.,0.,1.,1.,1.,1.,0.,0.,0.,0.,1.,1.,1.,1.,0.,0.,0.,0., &
1.,1.,1.,1.,0.,0.,0.,0.,0.,-10.,0.,1.,0.,0.,0.,0.,1.,0.,0.,1.,0.,0.,1.,0.,0., &
-10000000.,-10000000.,-10000000.,0.,-1.,-1.,-1.,-1.,-10.,0.,-5.,-5.,-1.,-0.1,0.,-0.1,0.,0.,0.,0. /)


REAL, DIMENSION(NOUT_P) :: VARIABLE_MAX = (/ &
40.,360.,3.,0.01,0.1,1000.,2.,360.,20.,30.,20.,20.,20.,360.,120.,2.,20.,20.,20.,20.,20.,360., &
120.,0.,20.,30.,25.,25.,25.,360.,120.,100.,15.,1.,10.,40.,30.,0.8,360.,1.,20.,25.,360.,20.,25., &
360.,20.,25.,360.,0.,10000000.,10000000.,10000000.,0.,1.,1.,1.,1.,30.,30.,5.,5.,10.,0.1,    &
0.,0.,30.,50.,30.,50. /)


! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. TITLE FOR OUTPUT PARAMETER AND SPECTRA.                               !
!        ---------------------------------------                               !

CHARACTER(LEN=60), DIMENSION(NOUT_P) :: TITL_P = (/                  &
& ' WIND SPEED U10 ( METRES/SECOND )                           ',    &   !!  1
& ' WIND DIRECTION ( DEGREE FROM NORTH TO )                    ',    &   !!  2
& ' FRICTION VELOCITY ( METRES/SECOND )                        ',    &   !!  3
& ' DRAG COEFFICIENT ( PROMILLE )                              ',    &   !!  4
& ' CHARNOCK PARAMETER                                         ',    &   !!  5
& ' WATER DEPTH (METRES) (DEEPER THAN 999M ARE PRINTED AS 999) ',    &   !!  6
& ' CURRENT SPEED ( METRES/SECOND )                            ',    &   !!  7
& ' CURRENT DIRECTION ( DEGREE FROM NORTH TO )                 ',    &   !!  8
& ' SIGNIFICANT WAVE HEIGHT ( METRES )                         ',    &   !!  9
& ' WAVE PEAK PERIOD ( SECONDS )                               ',    &   !! 10
& ' WAVE MEAN PERIOD (SECONDS )                                ',    &   !! 11
& ' WAVE TM1 PERIOD ( SECONDS )                                ',    &   !! 12
& ' WAVE TM2 PERIOD ( SECONDS )                                ',    &   !! 13
& ' WAVE DIRECTION ( DEGREE FROM NORTH TO )                    ',    &   !! 14
& ' DIRECTIONAL SPREAD ( DEGREES )                             ',    &   !! 15
& ' NORMALISED WAVE STRESS ( % )                               ',    &   !! 16
& ' SEA SIGNIFICANT WAVE HEIGHT ( METRES )                     ',    &   !! 17
& ' SEA PEAK PERIOD ( SECONDS )                                ',    &   !! 18
& ' SEA MEAN PERIOD ( SECONDS )                                ',    &   !! 19
& ' SEA TM1 PERIOD ( SECONDS )                                 ',    &   !! 20
& ' SEA TM2 PERIOD (  SECONDS )                                ',    &   !! 21
& ' SEA DIRECTION ( DEGREE FROM NORTH TO )                     ',    &   !! 22
& ' SEA DIRECTIONAL SPREAD ( DEGREES )                         ',    &   !! 23
& ' DUMMY                                                      ',    &   !! 24
& ' SWELL SIGNIFICANT WAVE HEIGHT ( METRES )                   ',    &   !! 25
& ' SWELL PEAK PERIOD ( SECONDS )                              ',    &   !! 26
& ' SWELL MEAN PERIOD ( SECONDS )                              ',    &   !! 27
& ' SWELL TM1 PERIOD ( SECONDS )                               ',    &   !! 28
& ' SWELL TM2 PERIOD ( SECONDS )                               ',    &   !! 29
& ' SWELL DIRECTION ( DEGREE FROM NORTH TO )                   ',    &   !! 30
& ' SWELL DIRECTIONAL SPREAD ( DEGREES )                       ',    &   !! 31
& ' ROUGHNESS LENGTH Z0 ( METRES )                             ',    &   !! 32
& ' GODA PEAKEDNESS PARAMETER                                  ',    &   !! 33
& ' KURTOSIS                                                   ',    &   !! 34
& ' BENJAMIN-FEIR INDEX                                        ',    &   !! 35
& ' NORMALIZED MAXIMUM WAVE HEIGHT                             ',    &   !! 36
& ' MAXIMUM WAVE PERIOD ( SECONDS )                            ',    &   !! 37
& ' PEAK FREQUENCY (INTERPOLATED) ( HZ )                       ',    &   !! 38
& ' PEAK DIRECTION ( DEGREE FROM NORTH TO )                    ',    &   !! 39
& ' MEAN SQUARE SLOPE                                          ',    &   !! 40
& ' FIRST SWELL SIGNIFICANT WAVE HEIGHT ( METRES )             ',    &   !! 41
& ' FIRST SWELL TM1 PERIOD ( SECONDS )                         ',    &   !! 42
& ' FIRST SWELL DIRECTION ( DEGREE FROM NORTH TO )             ',    &   !! 43
& ' SECOND SWELL SIGNIFICANT WAVE HEIGHT ( METRES )            ',    &   !! 44
& ' SECOND SWELL TM1 PERIOD ( SECONDS )                        ',    &   !! 45
& ' SECOND SWELL DIRECTION ( DEGREE FROM NORTH TO )            ',    &   !! 46
& ' THIRD SWELL SIGNIFICANT WAVE HEIGHT ( METRES )             ',    &   !! 47
& ' THIRD SWELL TM1 PERIOD ( SECONDS )                         ',    &   !! 48
& ' THIRD SWELL DIRECTION ( DEGREE FROM NORTH TO )             ',    &   !! 49
& ' DUMMY                                                      ',    &   !! 50
& ' RADIATION STRESS TENSOR SXX ( KG/S/S )                     ',    &   !! 51
& ' RADIATION STRESS TENSOR SYY ( KG/S/S )                     ',    &   !! 52
& ' RADIATION STRESS TENSOR SXY ( KG/S/S )                     ',    &   !! 53
& ' DUMMY                                                      ',    &   !! 54
& ' X-COMP. WAVE FORCE PER SURFACE UNIT ( N/M/M )              ',    &   !! 55
& ' Y-COMP. WAVE FORCE PER SURFACE UNIT ( N/M/M )              ',    &   !! 56
& ' X-COMP. STOKES DRIFT ( M/S )                               ',    &   !! 57
& ' Y-COMP. STOKES DRIFT ( M/S )                               ',    &   !! 58
& ' ENERGY FLUX TO OCEAN ( KG/S/S/S )                          ',    &   !! 59
& ' TOTAL ENERGY FLUX FROM WIND TO WAVES ( KG/S/S/S )          ',    &   !! 60
& ' X-COMP.  MOMENTUM FLUX INTO OCEAN ( KG/M/S/S )             ',    &   !! 61
& ' Y-COMP.  MOMENTUM FLUX INTO OCEAN ( KG/M/S/S )             ',    &   !! 62
& ' ENERGY FLUX FROM WAVES TO BOTTOM ( KG/S/S/S )              ',    &   !! 63
& ' X-COMP.  MOMENTUM FLUX FROM WAVES TO BOTTOM ( KG/M/S/S )   ',    &   !! 64
& ' Y-COMP.  MOMENTUM FLUX FROM WAVES TO BOTTOM ( KG/M/S/S )   ',    &   !! 65
& ' DUMMY                                                      ',    &   !! 66
& ' CREST MAX (TIME, FORRISTALL)                               ',    &   !! 67  !! WAM-MAX
& ' HMAX (TIME, NAESS)                                         ',    &   !! 68  !! WAM-MAX
& ' MAXIMUM CREST H.- SPACE-TIME (STQD)                        ',    &   !! 69  !! WAM-MAX
& ' MAXIMUM WAVE H.- SPACE-TIME (STQD)                         '/)       !! 70  !! WAM-MAX

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. SCALING FACTORS FOR OUTPUT PARAMETER.                                 !
!        -------------------------------------                                 !

REAL, PARAMETER, DIMENSION(NOUT_P) :: SCAL_P = (/                              &
&                     10.            ,    &   !!  1
&                      1.            ,    &   !!  2
&                    100.            ,    &   !!  3
&                  10000.            ,    &   !!  4
&                  10000.            ,    &   !!  5
&                      1.            ,    &   !!  6
&                    100.            ,    &   !!  7
&                      1.            ,    &   !!  8
&                     10.            ,    &   !!  9
&                     10.            ,    &   !! 10
&                     10.            ,    &   !! 11
&                     10.            ,    &   !! 12
&                     10.            ,    &   !! 13
&                      1.            ,    &   !! 14
&                      1.            ,    &   !! 15
&                    100.            ,    &   !! 16
&                     10.            ,    &   !! 17
&                     10.            ,    &   !! 18
&                     10.            ,    &   !! 19
&                     10.            ,    &   !! 20
&                     10.            ,    &   !! 21
&                      1.            ,    &   !! 22
&                      1.            ,    &   !! 23
&                      1.            ,    &   !! 24
&                     10.            ,    &   !! 25
&                     10.            ,    &   !! 26
&                     10.            ,    &   !! 27
&                     10.            ,    &   !! 28
&                     10.            ,    &   !! 29
&                      1.            ,    &   !! 30
&                      1.            ,    &   !! 31
&                  10000.            ,    &   !! 32
&                     10.            ,    &   !! 33
&                    100.            ,    &   !! 34
&                     10.            ,    &   !! 35
&                     10.            ,    &   !! 36
&                     10.            ,    &   !! 37
&                   1000.            ,    &   !! 38
&                      1.            ,    &   !! 39
&                   1000.            ,    &   !! 40
&                     10.            ,    &   !! 41
&                     10.            ,    &   !! 42
&                      1.            ,    &   !! 43
&                     10.            ,    &   !! 44
&                     10.            ,    &   !! 45
&                      1.            ,    &   !! 46
&                     10.            ,    &   !! 47
&                     10.            ,    &   !! 48
&                      1.            ,    &   !! 49
&                      1.            ,    &   !! 50
&                      0.            ,    &   !! 51
&                      0.            ,    &   !! 52
&                      0.            ,    &   !! 53
&                      1.            ,    &   !! 54
&                      0.            ,    &   !! 55
&                      0.            ,    &   !! 56
&                    100.            ,    &   !! 57
&                    100.            ,    &   !! 58
&                    100.            ,    &   !! 59
&                    100.            ,    &   !! 60
&                    100.            ,    &   !! 61
&                    100.            ,    &   !! 62
&                    100.            ,    &   !! 63
&                    100.            ,    &   !! 64
&                    100.            ,    &   !! 65
&                    100.            ,    &   !! 66
&                     10.            ,    &   !! 67  !! WAM-MAX
&                     10.            ,    &   !! 68  !! WAM-MAX
&                     10.            ,    &   !! 69  !! WAM-MAX
&                     10.            /)       !! 70  !! WAM-MAX

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. FLAG FOR DIRECTION PARAMTERS.                                         !
!        -----------------------------                                         !

LOGICAL, DIMENSION(NOUT_P) :: DIR_FLAG = (/                          &
& .FALSE.   ,    &   !!  1
& .TRUE.    ,    &   !!  2
& .FALSE.   ,    &   !!  3
& .FALSE.   ,    &   !!  4
& .FALSE.   ,    &   !!  5
& .FALSE.   ,    &   !!  6
& .FALSE.   ,    &   !!  7
& .TRUE.    ,    &   !!  8
& .FALSE.   ,    &   !!  9
& .FALSE.   ,    &   !! 10
& .FALSE.   ,    &   !! 11
& .FALSE.   ,    &   !! 12
& .FALSE.   ,    &   !! 13
& .TRUE.    ,    &   !! 14
& .FALSE.   ,    &   !! 15
& .FALSE.   ,    &   !! 16
& .FALSE.   ,    &   !! 17
& .FALSE.   ,    &   !! 18
& .FALSE.   ,    &   !! 19
& .FALSE.   ,    &   !! 20
& .FALSE.   ,    &   !! 21
& .TRUE.    ,    &   !! 22
& .FALSE.   ,    &   !! 23
& .FALSE.   ,    &   !! 24
& .FALSE.   ,    &   !! 25
& .FALSE.   ,    &   !! 26
& .FALSE.   ,    &   !! 27
& .FALSE.   ,    &   !! 28
& .FALSE.   ,    &   !! 29
& .TRUE.    ,    &   !! 30
& .FALSE.   ,    &   !! 31
& .FALSE.   ,    &   !! 32
& .FALSE.   ,    &   !! 33
& .FALSE.   ,    &   !! 34
& .FALSE.   ,    &   !! 35
& .FALSE.   ,    &   !! 36
& .FALSE.   ,    &   !! 37
& .FALSE.   ,    &   !! 38
& .TRUE.    ,    &   !! 39
& .FALSE.   ,    &   !! 40
& .FALSE.   ,    &   !! 41
& .FALSE.   ,    &   !! 42
& .TRUE.    ,    &   !! 43
& .FALSE.   ,    &   !! 44
& .FALSE.   ,    &   !! 45
& .TRUE.    ,    &   !! 46
& .FALSE.   ,    &   !! 47
& .FALSE.   ,    &   !! 48
& .TRUE.    ,    &   !! 49
& .FALSE.   ,    &   !! 50
& .FALSE.   ,    &   !! 51
& .FALSE.   ,    &   !! 52
& .FALSE.   ,    &   !! 53
& .FALSE.   ,    &   !! 54
& .FALSE.   ,    &   !! 55
& .FALSE.   ,    &   !! 56
& .FALSE.   ,    &   !! 57
& .FALSE.   ,    &   !! 58
& .FALSE.   ,    &   !! 59
& .FALSE.   ,    &   !! 60
& .FALSE.   ,    &   !! 61
& .FALSE.   ,    &   !! 62
& .FALSE.   ,    &   !! 63
& .FALSE.   ,    &   !! 64
& .FALSE.   ,    &   !! 65
& .FALSE.   ,    &   !! 66
& .FALSE.   ,    &   !! 67
& .FALSE.   ,    &   !! 68
& .FALSE.   ,    &   !! 69
& .FALSE.   /)       !! 70

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. NUMBER OF OUTPUT SPECTRA.                                             !
!        -------------------------                                             !

INTEGER, PARAMETER :: NOUT_S = 4

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. TITLE OF OUTPUT SPECTRA.                                              !
!        -----------------------                                               !

CHARACTER(LEN=60), DIMENSION(NOUT_S) :: TITL_S = (/                  &
& ' SPECTRUM                                                   ',    &   !!  1
& ' SEA SPECTRUM                                               ',    &   !!  2
& ' SWELL SPECTRUM                                             ',    &   !!  3
& ' DUMMY                                                      '/)       !!  4

! ---------------------------------------------------------------------------- !! ModR05: Include SRC-OUT
!                                                                              !
!     6. NUMBER OF SOURCE PARAMETER.                                           !
!        ---------------------------                                           !

INTEGER, PARAMETER :: NOUT_SCR = 10

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. TITLE FOR SOURCE PARAMETER.                                           !
!        ---------------------------                                           !

CHARACTER(LEN=60), DIMENSION(NOUT_SCR) :: TITL_SCR = (/              &
& ' WIND INPUT PHILLIPS SOURCE TERM ( M*M/S )                  ',    &   !!  1
& ' WIND INPUT SOURCE TERM ( M*M/S )                           ',    &   !!  2
& ' NON-LINEAR SOURCE TERM ( M*M/S )                           ',    &   !!  3
& ' WHITE CAPPING DISSIPATION SOURCE TERM ( M*M/S )            ',    &   !!  4
& ' BOTTOM FRICTION DISSIPATION SOURCE TERM ( M*M/S )          ',    &   !!  5
& ' WAVE BREAKING DISSIPATION SOURCE TERM ( M*M/S )            ',    &   !!  6
& ' FRACTION OF BREAKING                                       ',    &   !!  7
& ' SIG. WAVE HEIGHT ( M )                                     ',    &   !!  8
& ' PEAK WAVE DIRECTION ( DEG )                                ',    &   !!  9
& ' MEAN WAVE LENGTH ( M )                                     '/)       !! 10

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. SCALING FACTORS FOR SOURCE PARAMETER.                                 !
!        -------------------------------------                                 !

REAL, PARAMETER, DIMENSION(NOUT_SCR) :: SCAL_SCR = (/                          &
&                      100000.            ,    &   !!  1
&                      100000.            ,    &   !!  2
&                      100000.            ,    &   !!  3
&                      100000.            ,    &   !!  4
&                      100000.            ,    &   !!  5
&                      100000.            ,    &   !!  6
&                      100000.            ,    &   !!  7
&                     1000000.            ,    &   !!  8
&                      100000.            ,    &   !!  9
&                      100000.            /)       !! 10
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !! End ModR05


! Derived type to hold all the metadata for the integrated parameters

type t_integrated_parameters
  integer             :: id(NOUT_P)
  character(len=60)   :: name_ip(NOUT_P)
  character(len=100)   :: long_name(NOUT_P)
  character(len=100)   :: standard_name(NOUT_P)
  character(len=15)   :: units(NOUT_P)
  real                :: scaling_factor(NOUT_P)
  logical             :: direction_flag(NOUT_P)
  real                :: fill_value(NOUT_P)
  real                :: missing_value(NOUT_P)
  real                :: variable_min(NOUT_P)
  real                :: variable_max(NOUT_P)
end type t_integrated_parameters

type, extends(t_integrated_parameters) :: t_integrated_parameters_init
  contains
    procedure, pass(this) :: init
    procedure, pass(this) :: set_name_ip
    procedure, pass(this) :: set_long_name
    procedure, pass(this) :: set_standard_name
    procedure, pass(this) :: set_direction_flag
    procedure, pass(this) :: get_long_name
    procedure, pass(this) :: get_name_ip
    procedure, pass(this) :: get_scaling_factor
    procedure, pass(this) :: get_direction_flag
    procedure, pass(this) :: get_units
    procedure, pass(this) :: get_fill_value
    procedure, pass(this) :: get_missing_value
    procedure, pass(this) :: get_standard_name
    procedure, pass(this) :: get_vl_min
    procedure, pass(this) :: get_vl_max
end type

type(t_integrated_parameters_init), public :: params

public  :: t_integrated_parameters_init

contains
!
  subroutine init(this)
    class(t_integrated_parameters_init), intent(inout) :: this
    integer :: i

    !Fill ID values
    DO i = 1,NOUT_P,1
      this%id(i) = i
    END DO

    !.... Fill Name
    this%name_ip=NAME_IP

    !----- Fill title
    this%long_name=LONG_NAME_P

    !----- Fill standard name
    this%standard_name = STANDARD_NAME_P

    !----- Fill units
    this%units=UNITS_P

    !---- Fill scaling factor using SCAL_P
    this%scaling_factor = SCAL_P

    !---- Fill direction flag with .FALSE.
    this%direction_flag = dir_false

    !---- Fill _FillValue and missing_value
    this%fill_value = -999
    this%missing_value = -999
    
    !---- Fill vlmin and max
    this%variable_min = VARIABLE_MIN
    this%variable_max = VARIABLE_MAX
    
  end subroutine init

  subroutine set_name_ip(this, name_ip, id)
    class(t_integrated_parameters_init), intent(inout) :: this

    character(len=60), intent(in) :: name_ip
    integer, intent(in) :: id
    this%name_ip(id) = name_ip
  end subroutine set_name_ip

  
  subroutine set_long_name(this, long_name, id)
    class(t_integrated_parameters_init), intent(inout) :: this

    character(len=*), intent(in) :: long_name
    integer, intent(in) :: id
    this%long_name(id) = long_name
  end subroutine set_long_name

  subroutine set_standard_name(this, standard_name, id)
    class(t_integrated_parameters_init), intent(inout) :: this

    character(len=*), intent(in) :: standard_name
    integer, intent(in) :: id
    this%standard_name(id) = standard_name
  end subroutine set_standard_name


  subroutine set_direction_flag(this, direction_flag, id)
    class(t_integrated_parameters_init), intent(inout) :: this
    logical, intent(in) :: direction_flag
    integer, intent(in) :: id 
    this%direction_flag(id) = direction_flag
  end subroutine set_direction_flag

  function get_long_name(this, id) result(long_name)
    class(t_integrated_parameters_init), intent(in) :: this
    integer, intent(in) :: id
    character(len=100) :: long_name
    long_name = this%long_name(id) 
  end function

  function get_name_ip(this, id) result(name_ip)
    class(t_integrated_parameters_init), intent(in) :: this
    integer, intent(in) :: id
    character(len=60) :: name_ip
    name_ip = this%name_ip(id)
  end function

  function get_scaling_factor(this, id) result(scaling_factor)
    class(t_integrated_parameters_init), intent(in) :: this
    integer, intent(in) :: id
    real :: scaling_factor
    scaling_factor = this%scaling_factor(id)
  end function

  function get_direction_flag(this, id) result(direction_flag)
    class(t_integrated_parameters_init), intent(in) :: this
    integer, intent(in) :: id
    logical :: direction_flag
    direction_flag = this%direction_flag(id)
  end function  

  function get_units(this, id) result(units)
    class(t_integrated_parameters_init), intent(in) :: this
    integer, intent(in) :: id
    character(len=15) :: units
    units = this%units(id)
  end function

  function get_standard_name(this, id) result(standard_name)
    class(t_integrated_parameters_init), intent(in) :: this
    integer, intent(in) :: id
    character(len=100) :: standard_name
    standard_name = this%standard_name(id)
  end function

  function get_fill_value(this, id) result(fill_value)
    class(t_integrated_parameters_init), intent(in) :: this
    integer, intent(in) :: id
    real :: fill_value
    fill_value = this%fill_value(id)
  end function
  
  function get_missing_value(this, id) result(missing_value)
    class(t_integrated_parameters_init), intent(in) :: this
    integer, intent(in) :: id
    real :: missing_value
    missing_value = this%missing_value(id)
  end function

  function get_vl_min(this, id) result(vl_min)
    class(t_integrated_parameters_init), intent(in) :: this
    integer, intent(in) :: id
    real :: vl_min
    vl_min = this%variable_min(id)
  end function

  function get_vl_max(this, id) result(vl_max)
    class(t_integrated_parameters_init), intent(in) :: this
    integer, intent(in) :: id
    real :: vl_max
    vl_max = this%variable_max(id)
  end function

  subroutine  initialize_integrated_parameters
  
    !set default parameter metadata values
    call params%init()

    !set parameter metadata - direction flags
    call params%set_direction_flag(dir_true,id=2)
    call params%set_direction_flag(dir_true,id=8)
    call params%set_direction_flag(dir_true,id=14)
    call params%set_direction_flag(dir_true,id=22)
    call params%set_direction_flag(dir_true,id=30)
    call params%set_direction_flag(dir_true,id=39)
    call params%set_direction_flag(dir_true,id=43)
    call params%set_direction_flag(dir_true,id=46)
    call params%set_direction_flag(dir_true,id=49)

  end subroutine initialize_integrated_parameters

END MODULE WAM_OUTPUT_PARAMETER_MODULE
