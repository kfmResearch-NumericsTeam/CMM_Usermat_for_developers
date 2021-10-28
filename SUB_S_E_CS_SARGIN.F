!-----------------------------------------------------------------------
!     SUBROUTINE ZUR BERECHNUNG DER BETONHAUPTDRUCKSPANNUNGEN NACH
!      SARGIN (CS = CONCRETE SOFTENING)
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              02.06.2014
!-----------------------------------------------------------------------
      SUBROUTINE SUB_S_E_CS_SARGIN(BETON,E1,E3,FCC,ECU,K_E,SIGMA_C3)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      INTEGER, DIMENSION(2), INTENT(IN) :: BETON
      DOUBLE COMPLEX, INTENT(IN)  :: E3, E1, FCC, ECU, K_E
      DOUBLE COMPLEX, INTENT(OUT) :: SIGMA_C3
      DOUBLE COMPLEX   :: E31, ECU1, KS, FC, DIFF, ECU_95, EC, K


      ! INTERFACE ZU EXTERNEN FUNKTIONEN
      INTERFACE
      FUNCTION ABS_C(VAL)
        DOUBLE COMPLEX :: ABS_C
        DOUBLE COMPLEX, INTENT(IN) :: VAL
      END FUNCTION ABS_C

      FUNCTION MIN_RC(VAL1,VAL2)
        DOUBLE COMPLEX :: MIN_RC
        DOUBLE COMPLEX, INTENT(IN) :: VAL1, VAL2
      END FUNCTION MIN_RC
      END INTERFACE

      ! VERFESTIGENDES WERKSTOFFGESETZ
      K = CMPLX(0.95D0,0D0,KIND=8)

      ! ELASTIZIT�TSMODUL
      EC = K_E*ABS_C(FCC)**CMPLX(1D0/3D0,0D0,KIND=8)

      ! BERECHNUNG DER BETONHAUPTDRUCKSPANNUNG
      IF (REAL(E3) > 0D0) THEN

        SIGMA_C3 = CMPLX(0D0,0D0,KIND=8)

      ELSE IF ((REAL(E3) .LT. REAL(ECU)) .AND. ((2D0-BETON(2))
     &        .LT. 1.10D0)) THEN

        SIGMA_C3 = CMPLX(0D0,0D0,KIND=8)

      ELSE
        IF (REAL(E3) .LE. 0D0 .AND. REAL(E3) .GE. REAL(ECU)) THEN

          ECU1 = ABS_C(ECU)
          E31 = ABS_C(E3)
          FC = MIN_RC(ABS_C((FCC**CMPLX(2D0/3D0,0D0,KIND=8)/
     &         CMPLX(0.4D0,0D0,KIND=8))),ABS_C(FCC**
     &         CMPLX(2D0/3D0,0D0,KIND=8)/(CMPLX(0.4D0,0D0,KIND=8)+
     &         CMPLX(30D0,0D0,KIND=8)*E1)))
          KS = (EC*ECU1)/FC
          SIGMA_C3 = -FC*((KS*(E31/ECU1)-((E31/ECU1)*(E31/ECU1)))/
     &               (CMPLX(1D0,0D0,KIND=8)+(KS-CMPLX(2D0,0D0,KIND=8))*
     &               (E31/ECU1)))

        ELSE

          ECU1 = ABS_C(ECU)
          ECU_95 = K*ECU1
          E31 = ABS_C(E3)
          FC = MIN_RC(ABS_C((FCC**CMPLX(2D0/3D0,0D0,KIND=8)/
     &         CMPLX(0.4D0,0D0,KIND=8))),ABS_C(FCC**
     &         CMPLX(2D0/3D0,0D0,KIND=8)/(CMPLX(0.4D0,0D0,KIND=8)+
     &         CMPLX(30D0,0D0,KIND=8)*E1)))
          KS = (EC*ECU1)/FC
          DIFF = (KS/ECU1-CMPLX(2D0,0D0,KIND=8)*ECU_95/(ECU*ECU))/
     &           (CMPLX(1D0,0D0,KIND=8)+(KS-CMPLX(2D0,0D0,KIND=8))*
     &           ECU_95/ECU1)-(KS*ECU_95/ECU1-(ECU_95*ECU_95)/
     &           (ECU1*ECU1))*(KS-CMPLX(2D0,0D0,KIND=8))/
     &           ((CMPLX(1D0,0D0,KIND=8)+(KS-CMPLX(2D0,0D0,KIND=8))
     &           *ECU_95/ECU1)*(CMPLX(1D0,0D0,KIND=8)+
     &           (KS-CMPLX(2D0,0D0,KIND=8))*ECU_95/ECU1)*ECU1)
          SIGMA_C3 = -FC*(((KS*(ECU1/ECU1)-(ECU1/ECU1)*(ECU1/ECU1))/
     &               (CMPLX(1D0,0D0,KIND=8)+(KS-CMPLX(2D0,0D0,KIND=8))*
     &               (ECU1/ECU1))))+DIFF*(ECU1)-DIFF*(E31)

        END IF
      END IF

      END SUBROUTINE SUB_S_E_CS_SARGIN