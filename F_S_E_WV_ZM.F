!-----------------------------------------------------------------------
!     HILFSFUNKTION F�R ZUGGURTMODELL F�R WARMVERFORMTE ST�HLE
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              22.04.2014
!-----------------------------------------------------------------------
      FUNCTION F_S_E_WV_ZM(SIGMA_SR,SRM,DM,FSY,FSU,ESV,ES,TAU_B0,
     &                     TAU_B1,EC,RHO,KC,ALPHA)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX             :: F_S_E_WV_ZM
      DOUBLE COMPLEX, INTENT(IN) :: SIGMA_SR,SRM,DM,FSY,FSU,ESV,ES,
     &                              TAU_B0,TAU_B1,EC,RHO,KC,ALPHA

      DOUBLE COMPLEX             :: SIG_ID, EPS_M,N, X1, X2, X3,
     &                              Z1, Z2, Z2A,KV2

      KV2=CMPLX(10D0,0D0,KIND=8)*(ESV-FSY/ES)/(FSU-FSY)
      N=ES/EC
      SIG_ID =N*SIGMA_SR*RHO/(CMPLX(1D0,0D0,KIND=8)+RHO*(N-
     &        CMPLX(1D0,0D0,KIND=8)))

      X1 = DM/(CMPLX(4D0,0D0,KIND=8)*TAU_B0)*(SIGMA_SR-SIG_ID)
      X2=  DM/(CMPLX(4D0,0D0,KIND=8)*TAU_B0)*(FSY-SIG_ID)

      IF (REAL(SIGMA_SR).LE.REAL(FSU)) THEN
          X3=  DM/(CMPLX(4D0,0D0,KIND=8)*TAU_B1)*(SIGMA_SR-FSY)
          Z1=CMPLX(1D0,0D0,KIND=8)-(SIGMA_SR-FSY)/(KC*(FSU-FSY))
          Z2=CMPLX(1D0,0D0,KIND=8)-(SIGMA_SR-FSY-CMPLX(2D0,0D0,KIND=8)*
     &       TAU_B1*SRM/DM)/(KC*(FSU-FSY))
          Z2A=CMPLX(1D0,0D0,KIND=8)-(SIGMA_SR-FSY-CMPLX(4D0,0D0,KIND=8)*
     &       TAU_B1*X3/DM)/(KC*(FSU-FSY))
      ELSE
          X3=  DM/(CMPLX(4D0,0D0,KIND=8)*TAU_B1)*(FSU-FSY)
          Z1=CMPLX(1D0,0D0,KIND=8)-(FSU-FSY)/(KC*(FSU-FSY))
          Z2=CMPLX(1D0,0D0,KIND=8)-(FSU-FSY-CMPLX(2D0,0D0,KIND=8)*
     &       TAU_B1*SRM/DM)/(KC*(FSU-FSY))
          Z2A=CMPLX(1D0,0D0,KIND=8)-(FSU-FSY-CMPLX(4D0,0D0,KIND=8)*
     &       TAU_B1*X3/DM)/(KC*(FSU-FSY))
      END IF

      IF ((REAL(SIGMA_SR-CMPLX(2D0,0D0,KIND=8)*TAU_B0*SRM/DM) .LE.
     & REAL(SIG_ID)) .AND. (REAL(SIGMA_SR) .LE. REAL(FSY))) THEN

      EPS_M = CMPLX(2D0,0D0,KIND=8)/SRM*(X1*(SIGMA_SR/ES-
     &        CMPLX(2D0,0D0,KIND=8)*TAU_B0*X1/(ES*DM))+(SIGMA_SR/ES*N*
     &        RHO/(CMPLX(1D0,0D0,KIND=8)+N*RHO)*(SRM/
     &        CMPLX(2D0,0D0,KIND=8)-X1)))

      ELSEIF ((REAL(FSY-CMPLX(4D0,0D0,KIND=8)*TAU_B0*(SRM/
     &CMPLX(2D0,0D0,KIND=8)-X3)/DM).LT.REAL(SIG_ID)) .AND. (REAL(X3)
     &.GE. 0D0)) THEN

      EPS_M = (X2*(FSY/ES-CMPLX(2D0,0D0,KIND=8)*TAU_B0*X2/(ES*DM))+
     &        (SIGMA_SR/ES*N*RHO/(CMPLX(1D0,0D0,KIND=8)+N*RHO)*(SRM/
     &        CMPLX(2D0,0D0,KIND=8)-X2-X3)))

      EPS_M = EPS_M + X3*(ESV-DM/(CMPLX(4D0,0D0,KIND=8)*TAU_B1*X3)*KC*
     &        (FSU-FSY)*ALPHA*(Z2A*(LOG(Z2A)-CMPLX(1D0,0D0,KIND=8))-Z1*
     &        (LOG(Z1)-CMPLX(1D0,0D0,KIND=8))))

        IF (REAL(SIGMA_SR).GT.REAL(FSU)) THEN
            EPS_M = (EPS_M+KV2*(SIGMA_SR-FSU)*SRM/
     &              CMPLX(2D0,0D0,KIND=8))/CMPLX(2D0,0D0,KIND=8)/SRM
        ELSE
            EPS_M = CMPLX(2D0,0D0,KIND=8)/SRM*EPS_M
        END IF

      ELSEIF ((REAL(SIGMA_SR-CMPLX(2D0,0D0,KIND=8)*TAU_B0*SRM/DM) .GT.
     & REAL(SIG_ID)) .AND. (REAL(SIGMA_SR) .LE. REAL(FSY))) THEN

      EPS_M = SIGMA_SR/ES-TAU_B0*SRM/(ES*DM)

      ELSEIF ((REAL(SIGMA_SR) .GT. REAL(FSY)) .AND. (REAL(SIGMA_SR)
     &       .LT. REAL(FSY+CMPLX(2D0,0D0,KIND=8)*TAU_B1*SRM/DM))) THEN

      EPS_M = DM/(CMPLX(2D0,0D0,KIND=8)*TAU_B1*SRM)*(ESV*(SIGMA_SR-FSY)+
     &        KC*(FSU-FSY)*ALPHA*(CMPLX(1D0,0D0,KIND=8)+Z1*(LOG(Z1)-
     &        CMPLX(1D0,0D0,KIND=8))))+(SRM-CMPLX(2D0,0D0,KIND=8)*X3)/
     &        SRM*(FSY/ES-(TAU_B0*(SRM-CMPLX(2D0,0D0,KIND=8)*X3))/
     &        (ES*DM))

        IF (REAL(SIGMA_SR).GT.REAL(FSU)) THEN
            EPS_M=EPS_M+KV2*(SIGMA_SR-FSU)
        END IF

      ELSEIF (REAL(SIGMA_SR) .GE. REAL(FSY+CMPLX(2D0,0D0,KIND=8)*
     & TAU_B1*SRM/DM))  THEN

      EPS_M = ESV-DM/(CMPLX(2D0,0D0,KIND=8)*TAU_B1*SRM)*KC*(FSU-FSY)*
     &        ALPHA*(Z2*(LOG(Z2)-CMPLX(1D0,0D0,KIND=8))-Z1*(LOG(Z1)-
     &        CMPLX(1D0,0D0,KIND=8)))

            IF (REAL(SIGMA_SR).GT.REAL(FSU)) THEN
                EPS_M=EPS_M+KV2*(SIGMA_SR-FSU)
            END IF

      END IF

      F_S_E_WV_ZM = EPS_M

      RETURN

      END FUNCTION F_S_E_WV_ZM