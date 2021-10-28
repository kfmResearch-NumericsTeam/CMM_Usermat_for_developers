
!-----------------------------------------------------------------------
!     HILFSFUNKTION F�R IMAGIN�RE EXPONENTIALFUNKTION
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!
!     DATUM:              16.01.2017
!-----------------------------------------------------------------------
      FUNCTION EXP_C(Z_C)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX, INTENT(IN) :: Z_C
      DOUBLE COMPLEX             :: EXP_C, I

      ! PARAMETERDEFINITION
      I = CMPLX(0D0,1D0,KIND=8)

      ! BERECHNUNG DES ATAN IN C
      EXP_C = EXP(REAL(Z_C))*(COS(AIMAG(Z_C))+I*SIN(AIMAG(Z_C)))

      RETURN

      END FUNCTION EXP_C