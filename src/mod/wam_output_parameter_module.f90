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
& 'WIND SPEED U10 ' , & !!1
& 'WIND DIRECTION ' , & !!2
& 'FRICTION VELOCITY ' , & !!3
& 'DRAG COEFFICIENT ' , & !!4
& 'CHARNOCK PARAMETER ' , & !!5
& 'WATER DEPTH ' , & !!6
& 'CURRENT SPEED ' , & !!7
& 'CURRENT DIRECTION ' , & !!8
& 'SIGNIFICANT WAVE HEIGHT ' , & !!9
& 'WAVE PEAK PERIOD ' , & !!10
& 'WAVE MEAN PERIOD ' , & !!11
& 'WAVE TM1 PERIOD ' , & !!12
& 'WAVE TM2 PERIOD ' , & !!13
& 'WAVE DIRECTION ' , & !!14
& 'DIRECTIONAL SPREAD ' , & !!15
& 'NORMALISED WAVE STRESS ' , & !!16
& 'SEA SIGNIFICANT WAVE HEIGHT ' , & !!17
& 'SEA PEAK PERIOD ' , & !!18
& 'SEA MEAN PERIOD ' , & !!19
& 'SEA TM1 PERIOD ' , & !!20
& 'SEA TM2 PERIOD ' , & !!21
& 'SEA DIRECTION ' , & !!22
& 'SEA DIRECTIONAL SPREAD ' , & !!23
& 'DUMMY ' , & !!24
& 'SWELL SIGNIFICANT WAVE HEIGHT ' , & !!25
& 'SWELL PEAK PERIOD ' , & !!26
& 'SWELL MEAN PERIOD ' , & !!27
& 'SWELL TM1 PERIOD ' , & !!28
& 'SWELL TM2 PERIOD ' , & !!29
& 'SWELL DIRECTION ' , & !!30
& 'SWELL DIRECTIONAL SPREAD ' , & !!31
& 'ROUGHNESS LENGTH Z0 ' , & !!32
& 'GODA PEAKEDNESS PARAMETER ' , & !!33
& 'KURTOSIS ' , & !!34
& 'BENJAMIN-FEIR INDEX ' , & !!35
& 'NORMALIZED MAXIMUM WAVE HEIGHT ' , & !!36
& 'MAXIMUM WAVE PERIOD ' , & !!37
& 'PEAK FREQUENCY ' , & !!38
& 'PEAK DIRECTION ' , & !!39
& 'MEAN SQUARE SLOPE ' , & !!40
& 'FIRST SWELL SIGNIFICANT WAVE HEIGHT ' , & !!41
& 'FIRST SWELL TM1 PERIOD ' , & !!42
& 'FIRST SWELL DIRECTION ' , & !!43
& 'SECOND SWELL SIGNIFICANT WAVE HEIGHT ' , & !!44
& 'SECOND SWELL TM1 PERIOD ' , & !!45
& 'SECOND SWELL DIRECTION ' , & !!46
& 'THIRD SWELL SIGNIFICANT WAVE HEIGHT ' , & !!47
& 'THIRD SWELL TM1 PERIOD ' , & !!48
& 'THIRD SWELL DIRECTION ' , & !!49
& 'DUMMY ' , & !!50
& 'RADIATION STRESS TENSOR SXX ' , & !!51
& 'RADIATION STRESS TENSOR SYY ' , & !!52
& 'RADIATION STRESS TENSOR SXY ' , & !!53
& 'DUMMY ' , & !!54
& 'X-COMP. WAVE FORCE PER SURFACE UNIT ' , & !!55
& 'Y-COMP. WAVE FORCE PER SURFACE UNIT ' , & !!56
& 'X-COMP. STOKES DRIFT ' , & !!57
& 'Y-COMP. STOKES DRIFT ' , & !!58
& 'ENERGY FLUX TO OCEAN ' , & !!59
& 'TOTAL ENERGY FLUX FROM WIND TO WAVES ' , & !!60
& 'X-COMP. MOMENTUM FLUX INTO OCEAN ' , & !!61
& 'Y-COMP. MOMENTUM FLUX INTO OCEAN ' , & !!62
& 'ENERGY FLUX FROM WAVES TO BOTTOM ' , & !!63
& 'X-COMP. MOMENTUM FLUX FROM WAVES TO BOTTOM ' , & !!64
& 'Y-COMP. MOMENTUM FLUX FROM WAVES TO BOTTOM ' , & !!65
& 'DUMMY ' , & !!66
& 'CREST MAX ' , & !!67
& 'HMAX ' , & !!68
& 'MAXIMUM CREST H.- SPACE-TIME ' , & !!69
& 'MAXIMUM WAVE H.- SPACE-TIME '/) !!70

CHARACTER(LEN=15), DIMENSION(NOUT_P) :: UNITS_P = (/ &
& 'm s-1 '  , & !! 1
& 'degree '  , & !! 2
& 'm s-1 '  , & !! 3
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
& 'Equivalent 10-m wind speed derived from altimeter', &
& 'Wind degrees from north', &
& 'Friction velocity at sea water surface', &
& 'drag coefficient', &
& 'Charnock coefficient', &
& '', &
& '', &
& '', &
& 'Spectral significant wave height (Hm0)', &
& 'Wave period at spectral peak / peak period (Tp)', &
& 'Spectral moments (-1,0) wave period (Tm-10)', &
& 'Spectral moments (0,1) wave period (Tm01)', &
& 'Spectral moments (0,2) wave period (Tm02)', &
& 'Mean wave direction from (Mdir)', &
& 'Total directional spreed', &
& 'normalised_wave_stress', &
& 'Spectral significant wind wave height', &
& 'Sea peak period', &
& 'Sea mean period', &
& 'Spectral moments (0,1) wind wave period', &
& 'Sea m2-period', &
& 'Mean wind wave direction from', &
& 'Sea directional spreed', &
& '', &
& 'Swell significant wave height', &
& 'Swell peak period', &
& 'Swell mean period', &
& 'Swell m1-period', &
& 'Swell tm2-period', &
& 'Swell mean wave direction', &
& 'Swell directional spread', &
& 'Surface roughness length', &
& 'goda peakness parameter', &
& 'kurtosis', &
& 'Benjamin Feir index', &
& 'Maximum zero crossing wave height (Hmax)', &
& 'Maximum wave period (Tmax)', &
& 'interpolated peak frequency', &
& 'Wave principal direction at spectral peak', &
& 'mean square slope', &
& 'Spectral significant primary swell wave height', &
& 'Spectral moments (0,1) primary swell wave period', &
& 'Mean primary swell wave direction from', &
& 'Spectral significant secondary swell wave height', &
& 'Spectral moments (0,1) secondary swell wave period', &
& 'Mean secondary swell wave direction from', &
& '', &
& '', &
& '', &
& '', &
& 'radiation stress tensor sxx', &
& 'radiation stress tensor syy', &
& 'radiation stress tensor sxy', &
& '', &
& 'x-comp. wave force per surface unit', &
& 'y-comp. wave force per surface unit', &
& 'Stokes drift U', &
& 'Stokes drift V', &
& 'Energy flux into ocean', &
& 'normalized energy flux from wind to waves', &
& 'Eastward wave momentum flux into sea water', &
& 'Northward wave momentum flux into sea water', &
& 'energy flux from waves to bottom', &
& 'x-comp. momentum flux from waves into bottom', &
& 'y-comp. momentum flux from waves into bottom', &
& '', &
& 'Height of the highest crest' , &             !! WAM-MAX
& 'Maximum crest trough wave height (Hc,max)' , &  !! WAM-MAX
& 'maximum crest height - space-time (stqd)' , &    !! WAM-MAX
& 'maximum wave height - space-time (stqd)' /)    !! WAM-MAX


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
  real                :: variable_min(NOUT_P)
  real                :: variable_max(NOUT_P)
end type t_integrated_parameters

type, extends(t_integrated_parameters) :: t_integrated_parameters_init
  contains
    procedure, pass(this) :: init
    procedure, pass(this) :: set_name_ip
    procedure, pass(this) :: set_long_name
    procedure, pass(this) :: set_direction_flag
    procedure, pass(this) :: get_long_name
    procedure, pass(this) :: get_name_ip
    procedure, pass(this) :: get_scaling_factor
    procedure, pass(this) :: get_direction_flag
    procedure, pass(this) :: get_units
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

    character(len=100), intent(in) :: long_name
    integer, intent(in) :: id
    this%long_name(id) = long_name
  end subroutine set_long_name

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
