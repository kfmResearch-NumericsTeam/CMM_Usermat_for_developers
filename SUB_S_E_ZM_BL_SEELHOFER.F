!-----------------------------------------------------------------------
!     SUBROUTINE ZUR BERECHNUNG DER MAXIMALEN STAHLSPANNUNG AM RISS IN
!     FUNKTION DER MITTLEREN DEHNUNG F�R EIN BILINEARES STAHLSTOFFGESETZ
!     (ERWEITERTES ZUGGURTMODELL NACH SEELHOFER)
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              14.04.2014
!-----------------------------------------------------------------------
      SUBROUTINE SUB_S_E_ZM_BL_SEELHOFER(ESM,SRM,DM,FSY,FSU,ESU,ES,
     &                                   TAU_B0,TAU_B1,EC,RHO,SIGMA_SR)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX, INTENT(IN)  :: ESM, SRM, DM, FSY, FSU, ESU, ES,
     &                               TAU_B0, TAU_B1, EC, RHO
      DOUBLE COMPLEX, INTENT(OUT) :: SIGMA_SR
      DOUBLE COMPLEX   :: X1, X2, N, ESH, ALPHA, ESU3, ESU2B
      DOUBLE COMPLEX   :: SIGMA_SR1A, SIGMA_SR1B, SIGMA_SR2A,
     &                    SIGMA_SR2B, SIGMA_SR3, SIGMA_GRENZ,
     &                    SIGMA_GRENZ1

      ! INTERFACE ZU EXTERNEN FUNKTIONEN
      INTERFACE
      FUNCTION ABS_C(VAL)
        DOUBLE COMPLEX :: ABS_C
        DOUBLE COMPLEX, INTENT(IN) :: VAL
      END FUNCTION ABS_C

      FUNCTION SQRT_C(VAL)
        DOUBLE COMPLEX :: SQRT_C
        DOUBLE COMPLEX, INTENT(IN) :: VAL
      END FUNCTION SQRT_C
      END INTERFACE

      IF (REAL(DM) .GT. 0.1D0) THEN

      ! KONSTANTE MATERIALWERTE
      N = ES/EC
      ESH = (FSU-FSY)/(ESU-FSY/ES)

      ! BERECHNUNG DER STAHLSPANNUNG

      ! HILFSGR�SSEN
      ALPHA = CMPLX(1D0,0D0,KIND=8)+N*RHO

      X1 = SRM/CMPLX(2D0,0D0,KIND=8)*(SQRT_C(N*N*RHO*RHO+((ES*ESM)/
     &     TAU_B0)*(DM/SRM))-N*RHO)

      X2 = ((DM*FSY*ESH)/(CMPLX(4D0,0D0,KIND=8)*TAU_B1*ALPHA*ES))*
     &     (SQRT_C(CMPLX(4D0,0D0,KIND=8)*((ALPHA*ES)/ESH)*
     &     ((((SRM)*TAU_B1)/(DM*FSY))*((ALPHA*ES*ESM)/FSY-N*RHO)-TAU_B1/
     &     (CMPLX(4D0,0D0,KIND=8)*ALPHA*TAU_B0))+CMPLX(1D0,0D0,KIND=8))-
     &     CMPLX(1D0,0D0,KIND=8))


      ! MAXIMALE STAHLSPANNUNG AM RISS (REGIME 1A,1B,2A,2B,3)
      SIGMA_SR1A = X1*((CMPLX(4D0,0D0,KIND=8)*TAU_B0)/DM)*ALPHA
      SIGMA_SR1B = ESM*ES+(TAU_B0*SRM)/DM
      SIGMA_SR2A = FSY+X2*((CMPLX(4D0,0D0,KIND=8)*TAU_B1)/DM)
      SIGMA_SR2B = FSY+CMPLX(2D0,0D0,KIND=8)*(((TAU_B0*SRM)/DM-
     &             SQRT_C((FSY-ES*ESM)*((TAU_B1*SRM)/DM)*
     &             (TAU_B0/TAU_B1-ES/ESH)+(ES/ESH)*TAU_B0*TAU_B1*
     &             ((SRM*SRM)/(DM*DM))))/(TAU_B0/TAU_B1-ES/ESH))
      SIGMA_SR3  = FSY+ESH*(ESM-FSY/ES)+(TAU_B1*SRM)/DM

      ! FALLUNTERSCHEIDUNG
      IF (REAL(SIGMA_SR1A) <= REAL(((CMPLX(2D0,0D0,KIND=8)*TAU_B0*
     &   REAL(SRM))/DM)*ALPHA) .AND. REAL(SIGMA_SR1A) <= REAL(FSY))THEN

        SIGMA_SR = SIGMA_SR1A

      ELSE IF (REAL(SIGMA_SR1B) > REAL(((CMPLX(2D0,0D0,KIND=8)*TAU_B0*
     &        REAL(SRM))/DM)*ALPHA) .AND. REAL(SIGMA_SR1B)
     &        <= REAL(FSY)) THEN

        SIGMA_SR = SIGMA_SR1B

      ELSE IF (REAL(SIGMA_SR2A) <= REAL(((CMPLX(2D0,0D0,KIND=8)*TAU_B0*
     &        REAL(SRM))/DM)*ALPHA) .AND. REAL(FSY) < REAL(SIGMA_SR2A)
     &        .AND. REAL(SIGMA_SR2A) <= REAL(FSY+(CMPLX(2D0,0D0,KIND=8)
     &       *TAU_B1*REAL(SRM))/DM) .AND. REAL(SIGMA_SR2A) <=
     &       REAL(FSU)) THEN

        SIGMA_SR = SIGMA_SR2A

      ELSE IF (REAL(SIGMA_SR2B) > REAL(((CMPLX(2D0,0D0,KIND=8)*TAU_B0*
     &         REAL(SRM))/DM)*ALPHA) .AND. REAL(FSY) < REAL(SIGMA_SR2B)
     &        .AND. REAL(SIGMA_SR2B) <= REAL(FSY+(CMPLX(2D0,0D0,KIND=8)*
     &        TAU_B1*REAL(SRM))/DM) .AND. REAL(SIGMA_SR2B) <=
     &        REAL(FSU)) THEN

        SIGMA_SR = SIGMA_SR2B

      ELSE IF (REAL((FSY+(CMPLX(2D0,0D0,KIND=8)*TAU_B1*REAL(SRM))/DM))
     &        < REAL(SIGMA_SR3) .AND. REAL(SIGMA_SR3) <= REAL(FSU))THEN

        SIGMA_SR = SIGMA_SR3

      ELSE

        ESU2B = (ES*DM*DM*FSY*FSY*TAU_B1-ESH*TAU_B0*DM*DM*FSY*FSY-
     &          CMPLX(2D0,0D0,KIND=8)*ES*DM*DM*FSY*FSU*TAU_B1+
     &          CMPLX(2D0,0D0,KIND=8)*ESH*TAU_B0*DM*DM*FSY*FSU+ES*DM*DM*
     &          FSU*FSU*TAU_B1-ESH*TAU_B0*DM*DM*FSU*FSU+
     &          CMPLX(4D0,0D0,KIND=8)*ESH*DM*FSY*SRM*TAU_B1*TAU_B1-
     &          CMPLX(4D0,0D0,KIND=8)*ESH*TAU_B0*DM*FSY*SRM*TAU_B1+
     &          CMPLX(4D0,0D0,KIND=8)*ESH*TAU_B0*DM*SRM*FSU*TAU_B1-
     &          CMPLX(4D0,0D0,KIND=8)*ESH*TAU_B0*SRM*SRM*TAU_B1*TAU_B1)/
     &          (CMPLX(4D0,0D0,KIND=8)*ES*ESH*DM*SRM*TAU_B1*TAU_B1)

        ESU3 = ((FSU-(TAU_B1*SRM/DM)-FSY)/ESH)+(FSY/ES)

        SIGMA_GRENZ1 = FSY + (CMPLX(2D0,0D0,KIND=8)*TAU_B1*SRM/DM)

        SIGMA_GRENZ = (CMPLX(2D0,0D0,KIND=8)*TAU_B0*SRM/DM)*
     &                (CMPLX(1D0,0D0,KIND=8)+N*RHO)

        IF  (REAL(SIGMA_GRENZ1) <= REAL(FSU)) THEN

          SIGMA_SR = FSU+(ESM-ESU3)*CMPLX(50D0,0D0,KIND=8)

        ELSE

          SIGMA_SR = FSU+(ESM-ESU2B)*CMPLX(50D0,0D0,KIND=8)

        END IF

      END IF

      ELSE
        SIGMA_SR = CMPLX(0D0,0D0,KIND=8)
      END IF

      END SUBROUTINE SUB_S_E_ZM_BL_SEELHOFER