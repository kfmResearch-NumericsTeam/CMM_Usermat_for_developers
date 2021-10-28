!-----------------------------------------------------------------------
!     SUBROUTINE ZUR BERECHNUNG DER STAHLSPANNUNGEN F�R WARMVERFORMTE
!     ST�HLE (NATURHART)
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              22.04.2014
!-----------------------------------------------------------------------
      SUBROUTINE SUB_S_E_STAHL_WV(EPS,FSY,FSU,ESU,ESV,ES,KA,KB,KC,
     &                            SIGMA_S)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX, INTENT(IN)      :: EPS,FSY,FSU,ESU,ESV,ES,KA,KB,KC
      DOUBLE COMPLEX, INTENT(OUT)     :: SIGMA_S

      DOUBLE COMPLEX :: VORZEICHEN, ALPHA, KCC,EX

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

      FUNCTION EXP_C(Z_C)
            DOUBLE COMPLEX, INTENT(IN) :: Z_C
            DOUBLE COMPLEX             :: EXP_C, I
      END FUNCTION EXP_C

      END INTERFACE

      VORZEICHEN = SIGN_RC(1D0,EPS)
      EX = ABS_C(EPS)


      ! EINGABE VON KA UND KB OBLIGATORISCH -> KC WIRD BERECHNET FALLS KC<0
      IF (REAL(KC) .LT. 0D0) THEN
        KCC = CMPLX(1D0,0D0,KIND=8)/(CMPLX(1D0,0D0,KIND=8)-
     &        EXP_C((ESV-KB)/KA))
      ELSE
        KCC = KC
      END IF

      ALPHA = KA*(ESV-ESU)/(ESV-KB)

      ! BERECHNUNG DER STAHLSPANNUNG
      IF ((0D0 .LE. REAL(EX)) .AND. (REAL(EX) .LT. REAL(FSY/ES))) THEN
        SIGMA_S = ES*EX
      ELSEIF ((REAL(FSY/ES) .LE. REAL(EX)) .AND. (REAL(EX) .LE.
     & REAL(ESV))) THEN
        SIGMA_S = FSY
      ELSEIF (REAL(EX) .GT. REAL(ESV)) THEN
        SIGMA_S = FSY+(FSU-FSY)*KCC*(CMPLX(1D0,0D0,KIND=8)-
     &            EXP_C((ESV-EX)/ALPHA))
      END IF

      SIGMA_S = VORZEICHEN*SIGMA_S

      END SUBROUTINE SUB_S_E_STAHL_WV