!-----------------------------------------------------------------------
!     SUBROUTINE WELCHE DIE QUERKRAFT IM LETZTEN ITERATIONSSCHRITT
!     EINLIEST UND DIE HAUPTQUERKR�FTE BERECHNET.
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!
!     DATUM:              15.11.15
!-----------------------------------------------------------------------


      SUBROUTINE SUB_GET_VXVY(IELEMENT,NVAL,VX,VY,V0,PHI_0)
!      IMPLICIT NONE
! --> ALSO LOADED AT THE BEGINNING OF USERMAT !?!
#INCLUDE "IMPCOM.INC"

      EXTERNAL :: EMSGET
      INTEGER  :: EMSGET

      INTEGER, INTENT(IN)           :: IELEMENT,NVAL


      DOUBLE COMPLEX, INTENT(OUT) :: VX,VY,V0,PHI_0

!     DOUBLE PRECISION,DIMENSION(NVAL=8*(NN-1)+58):: VALUE_SHELL181 ! GILT NUR SHELL181
      DOUBLE PRECISION,DIMENSION(NVAL) ::   VALUE_SHELL181
      INTEGER                        ::  MDSDATA

      INTERFACE
        FUNCTION ATAN2_C(Y,X)
            DOUBLE COMPLEX             :: ATAN2_C
            DOUBLE COMPLEX, INTENT(IN) :: X,Y
        END FUNCTION ATAN2_C

        FUNCTION SQRT_C(VAL)
            DOUBLE COMPLEX :: SQRT_C
            DOUBLE COMPLEX, INTENT(IN) :: VAL
        END FUNCTION SQRT_C
      END INTERFACE


      MDSDATA = EMSGET(IELEMENT,VALUE_SHELL181)

      VX=CMPLX(VALUE_SHELL181(7),0D0,KIND=8)
      VY=CMPLX(VALUE_SHELL181(8),0D0,KIND=8)

      V0=SQRT_C(VX*VX+VY*VY)
      PHI_0=ATAN2_C(VX,VY)

!      PRINT*, V0

      END SUBROUTINE SUB_GET_VXVY