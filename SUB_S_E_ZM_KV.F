!-----------------------------------------------------------------------
!     SUBROUTINE ZUR BERECHNUNG DER MAXIMALEN STAHLSPANNUNG AM RISS IN
!     FUNKTION DER MITTLEREN DEHNUNG F�R KALTVERFORMTEN STAHL
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              17.04.2014
!-----------------------------------------------------------------------
      SUBROUTINE SUB_S_E_ZM_KV(ESM,SRM,DM,FSY,FSU,ESU,ESV,ES,TAU_B0,
     &                         TAU_B1,EC,RHO,KA,KB,KC,SIGMA_SR)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX, INTENT(IN)  :: ESM, SRM, DM, FSY, FSU, ESU, ESV,
     &                               ES, TAU_B0, TAU_B1, EC, RHO,
     &                               KA, KB, KC
      DOUBLE COMPLEX, INTENT(OUT) :: SIGMA_SR


      DOUBLE COMPLEX      ::  ALPHA, KCC, KAA, DEPS_C1,STEP1,STEP2,
     &                        STEP3,STEP4,K
      DOUBLE COMPLEX, DIMENSION(4,1) :: DEPS_C
      DOUBLE PRECISION    ::  JAC, DF, F, TOL, STEP, D_I
      INTEGER             ::  EXITFLAG, COUNTER, MAXCOUNTER

      ! INTERFACE ZU EXTERNEN FUNKTIONEN
      INTERFACE

        FUNCTION F_S_E_KV_ZM(SIGMA_SR,SRM,DM,FSY,FSU,ESU,ES,TAU_B0,
     &                       TAU_B1,EC,RHO,KC,ALPHA)
            DOUBLE COMPLEX             :: F_S_E_KV_ZM
            DOUBLE COMPLEX, INTENT(IN) :: SIGMA_SR,SRM,DM,FSY,FSU,ESU,
     &                                    ES,TAU_B0,TAU_B1,EC,RHO,
     &                                    KC,ALPHA
        END FUNCTION F_S_E_KV_ZM

        SUBROUTINE SUB_S_E_ZM_BL_SEELHOFER(ESM,SRM,DM,FSY,FSU,ESU,ES,
     &                                     TAU_B0,TAU_B1,EC,RHO,
     &                                     SIGMA_SR)
            DOUBLE COMPLEX, INTENT(IN) :: ESM,SRM,DM,FSY,FSU,ESU,ES,
     &                                    TAU_B0,TAU_B1,EC,RHO
            DOUBLE COMPLEX, INTENT(OUT) :: SIGMA_SR
        END SUBROUTINE SUB_S_E_ZM_BL_SEELHOFER

      END INTERFACE

      ! BERECHNUNG DER STAHLSPANNUNG
      IF (REAL(DM) .GT. 0.1D0) THEN

      IF (REAL(KA) .LT. 0D0) THEN
        KAA=FSY/ES
      ELSE
        KAA=KA
      END IF

      ALPHA = LOG((ESU-FSU/ES)/KAA)/LOG(FSU/FSY)

      IF (REAL(KC) .LT. 0D0) THEN
        KCC = FSY/(KAA**(1.0D0/ALPHA))
      ELSE
        KCC = KC
      END IF

      ! SETZEN DER STARTWERTE F�R DIE ITERATION
      STEP  = EPSILON(1D0)
      COUNTER = 0
      EXITFLAG = 1
      DF = 1.0D0
      F  = 1.0D0
      TOL = 1.0D-14
      MAXCOUNTER = 100

      K = CMPLX(-0.5D0,0.5D0*(SQRT(3D0)),KIND=8)

      STEP1 = K*CMPLX(STEP,0D0,KIND=8)/CMPLX(2D0,0D0,KIND=8)
      STEP2 = -K*CMPLX(STEP,0D0,KIND=8)/CMPLX(2D0,0D0,KIND=8)
      STEP3 = K*CMPLX(STEP,0D0,KIND=8)
      STEP4 = -K*CMPLX(STEP,0D0,KIND=8)

      ! STARTWERT F�R SPANNUNG

      CALL SUB_S_E_ZM_BL_SEELHOFER(ESM,SRM,DM,FSY,FSU,ESU,ES,TAU_B0,
     &                             TAU_B1,EC,RHO,SIGMA_SR)

      ! ITERATION REALTEIL DER SPANNUNG

10    IF ((ABS(F) .GT. TOL)) THEN

        COUNTER = COUNTER + 1

        IF (COUNTER .GT. 1) THEN
            SIGMA_SR = CMPLX(REAL(SIGMA_SR)-DF,0D0,KIND=8)
        ELSE
            SIGMA_SR = CMPLX(REAL(SIGMA_SR),0D0,KIND=8)
        END IF

        DEPS_C(1,1)= F_S_E_KV_ZM(SIGMA_SR+STEP1,SRM,DM,FSY,FSU,ESU,
     &  ES,TAU_B0,TAU_B1,EC,RHO,KCC,ALPHA)-CMPLX(REAL(ESM),0D0,KIND=8)
        DEPS_C(2,1)= F_S_E_KV_ZM(SIGMA_SR+STEP2,SRM,DM,FSY,FSU,ESU,
     &  ES,TAU_B0,TAU_B1,EC,RHO,KCC,ALPHA)-CMPLX(REAL(ESM),0D0,KIND=8)
        DEPS_C(3,1)= F_S_E_KV_ZM(SIGMA_SR+STEP3,SRM,DM,FSY,FSU,ESU,
     &  ES,TAU_B0,TAU_B1,EC,RHO,KCC,ALPHA)-CMPLX(REAL(ESM),0D0,KIND=8)
        DEPS_C(4,1)= F_S_E_KV_ZM(SIGMA_SR+STEP4,SRM,DM,FSY,FSU,ESU,
     &  ES,TAU_B0,TAU_B1,EC,RHO,KCC,ALPHA)-CMPLX(REAL(ESM),0D0,KIND=8)

        JAC  = AIMAG(32D0*(DEPS_C(1,1)-DEPS_C(2,1))-(DEPS_C(3,1)-
     &         DEPS_C(4,1)))/(15D0*SQRT(3D0)*STEP)

        F = REAL(DEPS_C(1,1))
        DF= F/JAC

        IF (COUNTER .GT. MAXCOUNTER) THEN
            F = 0D0
            DF= 0.0D0
            EXITFLAG = 2
        END IF

        GOTO 10
      END IF

      SIGMA_SR = CMPLX(REAL(SIGMA_SR),0D0,KIND=8)
      D_I=1D0
      TOL=1D-20
      COUNTER = 0

20    IF (ABS(D_I) .GT. TOL) THEN

        COUNTER = COUNTER + 1

        IF (COUNTER .GT. 1) THEN
            SIGMA_SR = CMPLX(REAL(SIGMA_SR),AIMAG(SIGMA_SR)-D_I,KIND=8)
        ELSE
            SIGMA_SR = CMPLX(REAL(SIGMA_SR),0D0,KIND=8)
        END IF

        DEPS_C(1,1) = F_S_E_KV_ZM(SIGMA_SR,SRM,DM,FSY,FSU,ESU,ES,
     &                TAU_B0,TAU_B1,EC,RHO,KCC,ALPHA)-ESM
        DEPS_C1= F_S_E_KV_ZM(SIGMA_SR+CMPLX(0D0,STEP,KIND=8),SRM,DM,
     &           FSY,FSU,ESU,ES,TAU_B0,TAU_B1,EC,RHO,KCC,ALPHA)-ESM
        JAC = (AIMAG(DEPS_C1)-AIMAG(DEPS_C(1,1)))/STEP
        D_I = AIMAG(DEPS_C(1,1))/JAC

       IF (COUNTER .GT. MAXCOUNTER) THEN
            D_I= 0.0
            EXITFLAG = 2
        END IF

        GOTO 20
      END IF

      ELSE
        SIGMA_SR = CMPLX(0D0,0D0,KIND=8)
      END IF

      END SUBROUTINE SUB_S_E_ZM_KV