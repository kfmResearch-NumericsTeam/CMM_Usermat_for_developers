!-----------------------------------------------------------------------
!     FUNKTION ZUR BESTIMMUNG DES MINIMALEN REALWERTES ZWEIER KOMPLEXEN
!     ZAHLEN
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              14.04.2014
!-----------------------------------------------------------------------
      FUNCTION MIN_RC(VAL1,VAL2)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX              :: MIN_RC
      DOUBLE COMPLEX , INTENT(IN) :: VAL1
      DOUBLE COMPLEX , INTENT(IN) :: VAL2

      ! ERMITTLUNG DES MINIMALEN WERTES

      IF (REAL(VAL1) < REAL(VAL2)) THEN
        MIN_RC = VAL1
      ELSE
        MIN_RC = VAL2
      END IF

      RETURN

      END FUNCTION MIN_RC