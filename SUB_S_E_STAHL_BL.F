!-----------------------------------------------------------------------
!     SUBROUTINE ZUR BERECHNUNG DER STAHLSPANNUNGEN NACH EINEM
!     BILINEAREN STAHLSTOFFGESETZ
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              14.04.2014
!-----------------------------------------------------------------------
      SUBROUTINE SUB_S_E_STAHL_BL(EPS,FSY,FSU,ESU,ES,SIGMA_S)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX, INTENT(IN)      :: EPS, FSY, FSU, ESU, ES
      DOUBLE COMPLEX, INTENT(OUT)     :: SIGMA_S

      ! INTERFACE ZU EXTERNEN FUNKTIONEN
      INTERFACE
      FUNCTION SIGN_RC(VAL1,VAL2)
        DOUBLE COMPLEX               :: SIGN_RC
        DOUBLE PRECISION, INTENT(IN) :: VAL1
        DOUBLE COMPLEX  , INTENT(IN) :: VAL2
      END FUNCTION SIGN_RC

      FUNCTION ABS_C(VAL)
        DOUBLE COMPLEX             :: ABS_C
        DOUBLE COMPLEX, INTENT(IN) :: VAL
      END FUNCTION ABS_C
      END INTERFACE

      ! BERECHNUNG DER STAHLSPANNUNG
      IF (REAL(-FSY/ES) <= REAL(EPS) .AND. REAL(EPS) <= REAL(FSY/ES))
     &    THEN
        SIGMA_S = ES*EPS
      ELSE IF (ABS(REAL(EPS))<= REAL(ESU)) THEN
        SIGMA_S = SIGN_RC(1D0,EPS)*(FSY+((FSU-FSY)/(ESU-FSY/ES))*
     &            (ABS_C(EPS)-FSY/ES))
      ELSE
        SIGMA_S = SIGN_RC(1D0,EPS)*(FSU+((ABS_C(EPS)-ESU))*
     &            CMPLX(50D0,0D0,KIND=8))
      END IF

      END SUBROUTINE SUB_S_E_STAHL_BL