!-----------------------------------------------------------------------
!     FUNKTION ZUR BERECHNUNG DES ARKUSTANGENS ZWEIER KOMPLEXEN ZAHLEN
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:             14.04.2014
!-----------------------------------------------------------------------
      FUNCTION ATAN2_C(CSN,CCS)

      ! VARIABLENDEKLARATION
      IMPLICIT NONE
      DOUBLE COMPLEX             :: ATAN2_C
      DOUBLE COMPLEX, INTENT(IN) :: CSN, CCS
      DOUBLE PRECISION           :: A,B,C,D

      ! BERECHNUNG DES ARKUSTANGENS
      IF (REAL(CSN) .EQ. 0D0 .AND. REAL(CCS) .EQ. 0D0) THEN
        ATAN2_C = CMPLX(0D0,0D0,KIND=8)
      ELSE
        A=REAL(CSN)
        B=AIMAG(CSN)
        C=REAL(CCS)
        D=AIMAG(CCS)
        ATAN2_C = CMPLX(ATAN2(A,C),((C*B-A*D)/(A**2D0+C**2D0)),KIND=8)
      END IF

      RETURN

      END FUNCTION ATAN2_C