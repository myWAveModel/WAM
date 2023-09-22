!
!	Dummy OASIS coupling interfave for WAM model
!
!	One module and an abort routine.
!	Separating the abort routine from the module
!	avoids many dependencies in the make file.
!
!	Provides routines and variables for
!	linking of the main routines.
!
MODULE WAM_OASIS_MODULE

 IMPLICIT NONE
 PUBLIC
  LOGICAL :: use_oasis		= .FALSE., &
             use_oasis_wind_in	= .FALSE., &
	     use_oasis_curr_in	= .FALSE., &
	     use_oasis_elev_in	= .FALSE., &
	     use_oasis_ice_in	= .FALSE., &
	     use_oasis_ocea_out	= .FALSE., &
   	     use_oasis_bdy_in	= .FALSE., &
	     use_oasis_nest_out	= .FALSE., &
	     use_oasis_force_source=.FALSE.,&
	     use_oasis_force_output=.FALSE.
  LOGICAL, ALLOCATABLE	:: oasis_output_flags(:,:)
  CHARACTER(len=6) :: comp_name = 'WAM'
  INTEGER :: comp_id
 CONTAINS

  SUBROUTINE Wam_oasis_init_comp
    END SUBROUTINE Wam_oasis_init_comp

  SUBROUTINE Wam_oasis_write_grid
    END SUBROUTINE Wam_oasis_write_grid

  SUBROUTINE Wam_oasis_write_part
    END SUBROUTINE Wam_oasis_write_part

  SUBROUTINE Wam_oasis_check_out
    END SUBROUTINE Wam_oasis_check_out

  SUBROUTINE Wam_oasis_rec_atmo
   END SUBROUTINE Wam_oasis_rec_atmo

  SUBROUTINE Wam_oasis_rec_boundary
    END SUBROUTINE Wam_oasis_rec_boundary

  SUBROUTINE Wam_oasis_send_nest
    END SUBROUTINE Wam_oasis_send_nest

  SUBROUTINE Wam_oasis_rec_topo(cdat,gotfield)
   CHARACTER (LEN=14)	:: cdat
   LOGICAL,INTENT(InOut):: gotfield
    END SUBROUTINE Wam_oasis_rec_topo

  SUBROUTINE Wam_oasis_rec_current(cdat,gotfield)
   CHARACTER (LEN=14)	:: cdat
   LOGICAL,INTENT(InOut):: gotfield
    END SUBROUTINE Wam_oasis_rec_current

  SUBROUTINE Wam_oasis_rec_ice
    END SUBROUTINE Wam_oasis_rec_ice

  SUBROUTINE Wam_oasis_send_gcm(CDTSOU,SOURCE_ARRAY)
   CHARACTER (LEN=14)	:: CDTSOU
   REAL			:: SOURCE_ARRAY(:,:)
    END SUBROUTINE Wam_oasis_send_gcm

  SUBROUTINE Wam_oasis_send_output_parameter(block)
   REAL			:: block(:,:)
    END SUBROUTINE Wam_oasis_send_output_parameter

  SUBROUTINE Wam_oasis_terminate(ierr)
   INTEGER ierr
    END SUBROUTINE Wam_oasis_terminate

  END MODULE WAM_OASIS_MODULE

SUBROUTINE Wam_oasis_abort
  END SUBROUTINE Wam_oasis_abort
