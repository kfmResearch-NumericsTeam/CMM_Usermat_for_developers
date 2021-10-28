!-----------------------------------------------------------------------
!     SUBROUTINE ZUR BERECHNUNG DER STAHLSPANNUNGEN F�R KALTVERFORMTE
!     ST�HLE
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              22.04.2014
!-----------------------------------------------------------------------
      SUBROUTINE SUB_S_E_STAHL_KV(EPS,FSY,FSU,ESU,ESV,ES,KA,KB,KC,
     &                            SIGMA_S)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX, INTENT(IN)      :: EPS,FSY,FSU,ESU,ESV,ES,KA,KB,KC
      DOUBLE COMPLEX, INTENT(OUT)     :: SIGMA_S

      DOUBLE COMPLEX      ::  ALPHA, KCC, DEPS_C, KAA, DEPS_C1
      DOUBLE PRECISION    ::  JAC, DF, F, TOL, STEP, D_I
      INTEGER             :: EXITFLAG, COUNTER, MAXCOUNTER

      INTERFACE
        FUNCTION ABS_C(VAL)
            DOUBLE COMPLEX             :: ABS_C
            DOUBLE COMPLEX, INTENT(IN) :: VAL
        END FUNCTION ABS_C

        FUNCTION SIGN_RC(VAL1,VAL2)
            DOUBLE COMPLEX               :: SIGN_RC
            DOUBLE PRECISION, INTENT(IN) :: VAL1
            DOUBLE COMPLEX  , INTENT(IN) :: VAL2
        END FUNCTION SIGN_RC

        FUNCTION F_S_E_KV(SIGMA,ES,KC,ALPHA)
            DOUBLE COMPLEX             :: F_S_E_KV
            DOUBLE COMPLEX, INTENT(IN) :: SIGMA,ES,KC,ALPHA
        END FUNCTION F_S_E_KV

        SUBROUTINE SUB_S_E_STAHL_BL(EPS,FSY,FSU,ESU,ES,SIGMA_S)
           DOUBLE COMPLEX, INTENT(IN) :: EPS,FSY,FSU,ESU,ES
           DOUBLE COMPLEX, INTENT(OUT) :: SIGMA_S
        END SUBROUTINE SUB_S_E_STAHL_BL
      END INTERFACE

      ! BERECHNUNG DER STAHLSPANNUNG
      IF (REAL(KA) .LT. 0D0) THEN
        KAA=FSY/ES
      ELSE
        KAA=KA
      END IF

      ALPHA = LOG((ESU-FSU/ES)/KAA)/LOG(FSU/FSY)

      IF (REAL(KC) .LT. 0D0) THEN
        KCC = FSY/(KAA**(CMPLX(1D0,0D0,KIND=8)/ALPHA))
      ELSE
        KCC = KC
      END IF

      ! SETZEN DER STARTWERTE F�R DIE ITERATION
      STEP  = EPSILON(1D0)
      COUNTER = 0
      EXITFLAG = 1
      DF = 1.0D0
      F  = 1.0D0
      TOL = 1.0D-15
      MAXCOUNTER = 100

      ! STARTWERT F�R SPANNUNG
      CALL SUB_S_E_STAHL_BL(EPS,(FSY+FSU)*CMPLX(0.5D0,0D0,KIND=8),
     &                      FSU,ESU,ES,SIGMA_S)

      ! ITERATION REALTEIL DER SPANNUNG

10    IF ((ABS(F) .GT. TOL)) THEN

        COUNTER = COUNTER + 1

        IF (COUNTER .GT. 1) THEN
            SIGMA_S = CMPLX(REAL(SIGMA_S)-DF,STEP,KIND=8)
        ELSE
            SIGMA_S = CMPLX(REAL(SIGMA_S),STEP,KIND=8)
        END IF

        DEPS_C= F_S_E_KV(SIGMA_S,ES,KCC,ALPHA)-
     &          CMPLX(REAL(EPS),0D0,KIND=8)
        JAC = AIMAG(DEPS_C)/STEP
        F = REAL(DEPS_C)
        DF= F/JAC

        IF (COUNTER .GT. MAXCOUNTER) THEN
            F = 0D0
            DF= 0.0
            EXITFLAG = 2
        END IF

      GOTO 10

      END IF

      SIGMA_S = CMPLX(REAL(SIGMA_S),0D0,KIND=8)
      D_I=1D0
      TOL=1D-22
      COUNTER = 0

20    IF (ABS(D_I) .GT. TOL) THEN

        COUNTER = COUNTER + 1

        IF (COUNTER .GT. 1) THEN
            SIGMA_S = CMPLX(REAL(SIGMA_S),AIMAG(SIGMA_S)-D_I,KIND=8)
        ELSE
            SIGMA_S = CMPLX(REAL(SIGMA_S),0D0,KIND=8)
        END IF

        DEPS_C = F_S_E_KV(SIGMA_S,ES,KCC,ALPHA)-EPS
        DEPS_C1= F_S_E_KV(SIGMA_S+CMPLX(0D0,STEP,KIND=8),ES,KCC,ALPHA)
     &           -EPS
        JAC = (AIMAG(DEPS_C1)-AIMAG(DEPS_C))/STEP
        D_I = AIMAG(DEPS_C)/JAC

        IF (COUNTER .GT. MAXCOUNTER) THEN
            D_I = 0.0
            EXITFLAG = 2
        END IF

      GOTO 20

      END IF

      END SUBROUTINE SUB_S_E_STAHL_KV