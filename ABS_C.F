!-----------------------------------------------------------------------
!     FUNKTION ZUR BESTIMMUNG DES ABSOLUTWERTES EINER KOMPLEXEN
!     VORZEICHENS
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              14.04.2014
!-----------------------------------------------------------------------
      FUNCTION ABS_C(VAL)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX             :: ABS_C
      DOUBLE COMPLEX, INTENT(IN) :: VAL

      ! BERECHNUNG DES ABSOLUTEN WERTES
      ABS_C = VAL

      IF (REAL(VAL) < 0D0) THEN
        ABS_C = CMPLX(-REAL(VAL),-AIMAG(VAL),KIND=8)
      END IF

      RETURN

      END FUNCTION ABS_C