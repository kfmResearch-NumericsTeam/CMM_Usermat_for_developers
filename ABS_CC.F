!-----------------------------------------------------------------------
!     HILFSFUNKTION F�R IMAGIN�RE ABSOLUTWERTE
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              14.04.2014
!-----------------------------------------------------------------------
      FUNCTION ABS_CC(VAL)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX             :: ABS_CC
      DOUBLE COMPLEX, INTENT(IN) :: VAL

      ! BERECHNUNG DES ABSOLUTEN WERTES
      IF (REAL(VAL) < 0D0 .AND. AIMAG(VAL) < 0D0) THEN
        ABS_CC = CMPLX(-REAL(VAL),-AIMAG(VAL),KIND=8)

      ELSE IF (REAL(VAL) < 0D0 .AND. AIMAG(VAL) >= 0D0) THEN
        ABS_CC = CMPLX(-REAL(VAL),AIMAG(VAL),KIND=8)

      ELSE IF (REAL(VAL) >= 0D0 .AND. AIMAG(VAL) < 0D0) THEN
        ABS_CC = CMPLX(REAL(VAL),-AIMAG(VAL),KIND=8)

      ELSE IF (REAL(VAL) >= 0D0 .AND. AIMAG(VAL) >= 0D0) THEN
        ABS_CC = VAL
      END IF

      RETURN

      END FUNCTION ABS_CC