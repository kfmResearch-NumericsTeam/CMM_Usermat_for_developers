!-----------------------------------------------------------------------
!     FUNKTION ZUR BERECHNUNG EINER KOMPLEXEN WURZEL
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              14.04.2014
!-----------------------------------------------------------------------
      FUNCTION SQRT_C(VAL)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX              :: SQRT_C,I
      DOUBLE COMPLEX , INTENT(IN) :: VAL
      DOUBLE PRECISION :: PI, R, A, B, PHI

      ! PARAMETERDEFINITION
      I = CMPLX(0D0,1D0,KIND=8)
      PI = 4.D0*ATAN(1.0D0)
      R = ABS(VAL)
      A = REAL(VAL)
      B = AIMAG(VAL)

      ! BERECHNUNG
      IF (A > 0D0) THEN
        PHI = ATAN(B/A)
      ELSE IF (A < 0D0 .AND. B >= 0D0) THEN
        PHI = ATAN(B/A)+PI
      ELSE IF (A < 0D0 .AND. B < 0D0) THEN
        PHI = ATAN(B/A)-PI
      ELSE IF (A == 0D0 .AND. B > 0D0) THEN
        PHI = PI/2D0
      ELSE IF (A == 0D0 .AND. B < 0D0) THEN
        PHI = -PI/2D0
      END IF

      SQRT_C = SQRT(R)*EXP(I*(PHI+0D0*PI)/2D0)

      RETURN

      END FUNCTION SQRT_C