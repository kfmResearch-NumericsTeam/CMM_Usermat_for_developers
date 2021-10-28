!-----------------------------------------------------------------------
!     SUBROUTINE ZUR BERECHNUNG DER HAUPTVERZERRUNGEN E1 UND E3
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              14.04.2014
!-----------------------------------------------------------------------
      SUBROUTINE SUB_E1_E3(EX,EY,GAMMA_XY,E1,E3)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX,  INTENT(IN)  :: EX, EY, GAMMA_XY
      DOUBLE COMPLEX,  INTENT(OUT) :: E1, E3

      INTERFACE
        FUNCTION SQRT_C(VAL)
            DOUBLE COMPLEX :: SQRT_C
            DOUBLE COMPLEX, INTENT(IN) :: VAL
        END FUNCTION SQRT_C
      END INTERFACE

      ! BERECHNUNG DER HAUPTVERZERRUNGEN
      E1 = (CMPLX(1D0,0D0,KIND=8)/CMPLX(2D0,0D0,KIND=8))*(EX+EY) +
     &     (CMPLX(1D0,0D0,KIND=8)/CMPLX(2D0,0D0,KIND=8))*(SQRT_C((EX-EY)
     &     *(EX-EY)+GAMMA_XY*GAMMA_XY))

      E3 = (CMPLX(1D0,0D0,KIND=8)/CMPLX(2D0,0D0,KIND=8))*(EX+EY) -
     &     (CMPLX(1D0,0D0,KIND=8)/CMPLX(2D0,0D0,KIND=8))*(SQRT_C((EX-EY)
     &     *(EX-EY)+GAMMA_XY*GAMMA_XY))

      END SUBROUTINE SUB_E1_E3