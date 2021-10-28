!-----------------------------------------------------------------------
!     FUNKTION ZUR BERECHNUNG DES ARKUSTANGENS ZWEIER KOMPLEXEN ZAHLEN
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:             14.04.2014
!-----------------------------------------------------------------------
      FUNCTION TAN_C(Z)

      ! VARIABLENDEKLARATION
      IMPLICIT NONE
      DOUBLE COMPLEX             :: TAN_C
      DOUBLE COMPLEX, INTENT(IN) :: Z

      ! BERECHNUNG DES TAN IN C
      TAN_C=CMPLX(TAN(REAL(Z)),AIMAG(Z)/COS(REAL(Z))**2D0,KIND=8)

      RETURN

      END FUNCTION TAN_C