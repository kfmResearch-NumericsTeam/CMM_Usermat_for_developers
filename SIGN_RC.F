!-----------------------------------------------------------------------
!     FUNKTION ZUR BESTIMMUNG EINES KOMPLEXEN VORZEICHENS
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              14.04.2014
!-----------------------------------------------------------------------
      FUNCTION SIGN_RC(VAL1,VAL2)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX               :: SIGN_RC, SIGN_0
      DOUBLE PRECISION, INTENT(IN) :: VAL1
      DOUBLE COMPLEX  , INTENT(IN) :: VAL2

      ! AUSWERTUNG DER FUNKTION
      IF (REAL(VAL2) < 0) THEN
        SIGN_0 = CMPLX(-1D0,0D0,KIND=8)
      ELSE
        SIGN_0 = CMPLX(1D0,0D0,KIND=8)
      END IF

      SIGN_RC = SIGN_0*CMPLX(VAL1,0D0,KIND=8)

      RETURN

      END FUNCTION SIGN_RC