!-----------------------------------------------------------------------
!     SUBROUTINE EINFLUSS QUERKRAFT AUF GURTKR�FTE, WELCHE DIE
!     SPANNUNGEN IM GLOBALEN KOORDINATENSYSTEM ZUR�CKGIBT.
!
!       --> STAND GILT NUR F�R ORTHOGONALE BEWEHRUNGEN
!       --> SCHIEFWINKLIGE BEWEHRUNG IST NOCH PENDENT!!!
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!
!     DATUM:              15.11.15
!-----------------------------------------------------------------------

      SUBROUTINE SUB_ZQ(SCHICHTINFO,MATERIAL,
     &                 SIGX_Q,SIGY_Q,TAUXY_Q,RHO_Z,STATE)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      INTEGER,INTENT(IN)            :: SCHICHTINFO
      DOUBLE PRECISION,DIMENSION(60),INTENT(IN):: MATERIAL


      DOUBLE COMPLEX,INTENT(OUT)    :: SIGX_Q,SIGY_Q,TAUXY_Q,RHO_Z
      INTEGER,INTENT(INOUT)         :: STATE

      DOUBLE COMPLEX                :: VX,VY,V0,PHI_0,VR,WINKELD
      DOUBLE COMPLEX                :: T_X_Q,T_Y_Q,T_XY_Q
      INTEGER                       :: NVAL,KLAYER,IELEMENT,KRITQ,GRENZE

      DOUBLE COMPLEX                :: ZS1,ZS2,ZS3,ZS4
      DOUBLE COMPLEX                :: PSI1,PSI2,DH,HOEHE,N
      DOUBLE COMPLEX                :: FCC,FCT,HSW,N_HSW,DV,FSY,F_VR
      INTEGER                       :: IREF1,IREF2,IREF3,IREF4

      INTERFACE

      SUBROUTINE SUB_T_Q(VX,VY,V0,PHI_0,WINKELD,FSY,DV,
     &                       T_X_Q,T_Y_Q,T_XY_Q,RHO_Z)
        DOUBLE COMPLEX,INTENT(IN)     :: VX,VY,V0,PHI_0,WINKELD,FSY,DV
        DOUBLE COMPLEX,INTENT(OUT)    :: T_X_Q,T_Y_Q,T_XY_Q,RHO_Z
      END SUBROUTINE SUB_T_Q

      SUBROUTINE SUB_GET_VXVY(IELEMENT,NVAL,VX,VY,V0,PHI_0)
        INTEGER, INTENT(IN)           :: IELEMENT,NVAL
        DOUBLE COMPLEX, INTENT(OUT) :: VX,VY,V0,PHI_0
      END SUBROUTINE SUB_GET_VXVY

      FUNCTION SQRT_C(VAL)
        DOUBLE COMPLEX :: SQRT_C
        DOUBLE COMPLEX, INTENT(IN) :: VAL
      END FUNCTION SQRT_C

      END INTERFACE

      ! GRUNDDATEN AUS MATERIAL-MATRIX

      IREF1 = INT(MATERIAL(46))
      IREF2 = INT(MATERIAL(47))
      IREF3 = INT(MATERIAL(48))
      IREF4 = INT(MATERIAL(49))

      ZS1  = CMPLX(MATERIAL(50),0D0,KIND=8)
      ZS2  = CMPLX(MATERIAL(51),0D0,KIND=8)
      ZS3  = CMPLX(MATERIAL(52),0D0,KIND=8)
      ZS4  = CMPLX(MATERIAL(53),0D0,KIND=8)

      NVAL = INT(MATERIAL(54))
      WINKELD = CMPLX(MATERIAL(55),0D0,KIND=8)
      KRITQ = INT(MATERIAL(56))
      F_VR  = CMPLX(MATERIAL(57),0D0,KIND=8)
      FSY   = CMPLX(MATERIAL(58),0D0,KIND=8)
      IELEMENT = INT(MATERIAL(59))
      KLAYER   = INT(MATERIAL(60))

      N = CMPLX(MATERIAL(3),0D0,KIND=8)
      HOEHE = CMPLX(MATERIAL(4),0D0,KIND=8)

      DH=HOEHE/N

      FCC = CMPLX(MATERIAL(34),0D0,KIND=8)
      FCT = CMPLX(0.3D0,0D0,KIND=8)*(FCC*FCC)**
     &      CMPLX((1D0/3D0),0D0,KIND=8)

      ! RICHTUNG DER BEWEHRUNG IN X-RICHTUNG:
      PSI1 = CMPLX(MATERIAL(11),0D0,KIND=8)
      ! RICHTUNG DER BEWEHRUNG IN Y-RICHTUNG:
      PSI2 = CMPLX(MATERIAL(24),0D0,KIND=8)

      ! BRUCHKRITERIUM

      IF (KRITQ.EQ.1) THEN
        VR = F_VR*FCT/(CMPLX(3D0,0D0,KIND=8))
      ELSEIF (KRITQ.EQ.2) THEN
        VR = F_VR*SQRT_C(FCC)/(CMPLX(6D0,0D0,KIND=8))
      ELSE
        VR = F_VR*SQRT_C(FCC)/(CMPLX(6D0,0D0,KIND=8))
      END IF


      ! HSW ALS GANZZAHLIGES VERH�LTNIS VON DH
      GRENZE = INT(FLOOR(REAL(N)/2D0))
      IF (KLAYER.LT.GRENZE) THEN
        HSW=(HOEHE+ZS1+ZS2)   !ZS1 UND ZS2 SIND NEGATIV
      ELSE
        HSW=(HOEHE-(ZS3+ZS4))
      END IF

      N_HSW=CMPLX(CEILING(REAL(HSW/DH)),0D0,KIND=8)
      HSW=N_HSW*DH

      IF (REAL(HSW).GT.(REAL(HOEHE)/2D0)) THEN
        HSW=HOEHE/CMPLX(2D0,0D0,KIND=8)
      END IF

       IF (((KLAYER.GE.0).AND.(KLAYER.LE.REAL(N_HSW))).OR.
     &     (KLAYER.GE.(REAL(N-N_HSW)))) THEN

        CALL SUB_GET_VXVY(IELEMENT,NVAL,VX,VY,V0,PHI_0)

        ! STATISCHE H�HE F�R QUERKRAFTSBERECHNUNG
        DV  = (-ZS1-ZS2+ZS3+ZS4)/CMPLX(2D0,0D0,KIND=8)

        IF (REAL(V0/DV).GT.REAL(VR)) THEN

            CALL SUB_T_Q(VX,VY,V0,PHI_0,WINKELD,FSY,DV,
     &                       T_X_Q,T_Y_Q,T_XY_Q,RHO_Z)

            IF (SCHICHTINFO.EQ.0) THEN
            ! BETONSCHICHT ... ALLES AUF DEN STRINGER FALLS
            ! KLAYER INNERHALB STRINGER

                IF (STATE.LE.2) THEN !DRUCKMODELLE
                    IF (KLAYER.LT.GRENZE) THEN
                        SIGX_Q=T_X_Q/HSW*DV/(-ZS1-ZS2)
                        SIGY_Q=T_Y_Q/HSW*DV/(-ZS1-ZS2)
                        TAUXY_Q=T_XY_Q/HSW*DV/(-ZS1-ZS2)
                    ELSE
                        SIGX_Q=T_X_Q/HSW*DV/(ZS3+ZS4)
                        SIGY_Q=T_Y_Q/HSW*DV/(ZS3+ZS4)
                        TAUXY_Q=T_XY_Q/HSW*DV/(ZS3+ZS4)
                    END IF
                ELSEIF (STATE.GE.3) THEN ! CMM UND ZUG/ZUG MODEL
                        SIGX_Q=CMPLX(0D0,0D0,KIND=8)
                        SIGY_Q=CMPLX(0D0,0D0,KIND=8)
                    IF (KLAYER.LT.GRENZE) THEN
                        TAUXY_Q=T_XY_Q/HSW*DV/(-ZS1-ZS2)
                    ELSE
                        TAUXY_Q=T_XY_Q/HSW*DV/(ZS3+ZS4)
                    END IF
                ENDIF
            ELSE

                IF (STATE.LE.2) THEN !DRUCKMODELLE

                    IF (KLAYER.LT.GRENZE) THEN
                        SIGX_Q=T_X_Q/HSW*DV/(-ZS1-ZS2)
                        SIGY_Q=T_Y_Q/HSW*DV/(-ZS1-ZS2)
                        TAUXY_Q=T_XY_Q/HSW*DV/(-ZS1-ZS2)
                    ELSE
                        SIGX_Q=T_X_Q/HSW*DV/(ZS3+ZS4)
                        SIGY_Q=T_Y_Q/HSW*DV/(ZS3+ZS4)
                        TAUXY_Q=T_XY_Q/HSW*DV/(ZS3+ZS4)
                    END IF

                ELSEIF (STATE.GE.3) THEN ! CMM UND ZUG/ZUG-MODELLE

                    IF ((SCHICHTINFO.EQ.1).AND.(REAL(PSI1).LT.1D0)) THEN
                    ! BEWEHRUNGSSCHICHT 1. IN X-RICHTUNG

                        SIGX_Q=T_X_Q/DH*DV/(CMPLX(-2D0,0D0,KIND=8)*ZS1)
                        SIGY_Q=CMPLX(0D0,0D0,KIND=8)
                        TAUXY_Q=T_XY_Q/HSW*DV/(-ZS1-ZS2)

                    ELSEIF ((SCHICHTINFO.EQ.1).AND.
     &                          (REAL(PSI1).GT.1D0)) THEN
                    ! BEWEHRUNGSSCHICHT 1.  IN Y-RICHTUNG

                        SIGX_Q=CMPLX(0D0,0D0,KIND=8)
                        SIGY_Q=T_Y_Q/DH*DV/(CMPLX(-2D0,0D0,KIND=8)*ZS1)
                        TAUXY_Q=T_XY_Q/HSW*DV/(-ZS1-ZS2)

                    ELSEIF ((SCHICHTINFO.EQ.2).AND.
     &                          (REAL(PSI2).GT.1D0)) THEN
                    ! BEWEHRUNGSSCHICHT 2. LAGE IN Y-RICHTUNG

                        SIGX_Q=CMPLX(0D0,0D0,KIND=8)
                        SIGY_Q=T_Y_Q/DH*DV/(CMPLX(-2D0,0D0,KIND=8)*ZS2)
                        TAUXY_Q=T_XY_Q/HSW*DV/(-ZS1-ZS2)

                    ELSEIF ((SCHICHTINFO.EQ.2).AND.
     &                             (REAL(PSI2).LT.1D0)) THEN
                    ! BEWEHRUNGSSCHICHT 2. LAGE IN X-RICHTUNG

                        SIGX_Q=T_X_Q/DH*DV/(CMPLX(-2D0,0D0,KIND=8)*ZS2)
                        SIGY_Q=CMPLX(0D0,0D0,KIND=8)
                        TAUXY_Q=T_XY_Q/HSW*DV/(-ZS1-ZS2)

                    ELSEIF ((SCHICHTINFO.EQ.3).AND.
     &                             (REAL(PSI2).GT.1D0)) THEN
                    ! BEWEHRUNGSSCHICHT 3. LAGE IN Y-RICHTUNG

                        SIGX_Q=CMPLX(0D0,0D0,KIND=8)
                        SIGY_Q=T_Y_Q/DH*DV/(CMPLX(2D0,0D0,KIND=8)*ZS3)
                        TAUXY_Q=T_XY_Q/HSW*DV/(ZS3+ZS4)

                    ELSEIF ((SCHICHTINFO.EQ.3).AND.
     &                             (REAL(PSI2).LT.1D0)) THEN
                    ! BEWEHRUNGSSCHICHT 3. LAGE IN X-RICHTUNG

                        SIGX_Q=T_X_Q/DH*DV/(CMPLX(2D0,0D0,KIND=8)*ZS3)
                        SIGY_Q=CMPLX(0D0,0D0,KIND=8)
                        TAUXY_Q=T_XY_Q/HSW*DV/(ZS3+ZS4)

                    ELSEIF ((SCHICHTINFO.EQ.4).AND.
     &                             (REAL(PSI1).LT.1D0)) THEN
                    ! BEWEHRUNGSSCHICHT 4. LAGE IN X-RICHTUNG

                        SIGX_Q=T_X_Q/DH*DV/(CMPLX(2D0,0D0,KIND=8)*ZS4)
                        SIGY_Q=CMPLX(0D0,0D0,KIND=8)
                        TAUXY_Q=T_XY_Q/HSW*DV/(ZS3+ZS4)

                    ELSEIF ((SCHICHTINFO.EQ.4).AND.
     &                             (REAL(PSI1).GT.1D0)) THEN
                    ! BEWEHRUNGSSCHICHT 4. LAGE IN Y-RICHTUNG

                        SIGX_Q=CMPLX(0D0,0D0,KIND=8)
                        SIGY_Q=T_Y_Q/DH*DV/(CMPLX(2D0,0D0,KIND=8)*ZS4)
                        TAUXY_Q=T_XY_Q/HSW*DV/(ZS3+ZS4)

                    END IF
                END IF
            END IF
           STATE=1

         ELSE
            ! KERN UNGERISSEN
            SIGX_Q = CMPLX(0D0,0D0,KIND=8)
            SIGY_Q = CMPLX(0D0,0D0,KIND=8)
            TAUXY_Q= CMPLX(0D0,0D0,KIND=8)
            RHO_Z  = CMPLX(0D0,0D0,KIND=8)
            STATE = 0
         END IF

      ELSE
        ! LAYER NICHT IN IM SANDWICHDECKEL
        SIGX_Q = CMPLX(0D0,0D0,KIND=8)
        SIGY_Q = CMPLX(0D0,0D0,KIND=8)
        TAUXY_Q= CMPLX(0D0,0D0,KIND=8)
        RHO_Z  = CMPLX(0D0,0D0,KIND=8)
        STATE=2
      END IF


      ! STATE = 1 --> KERN GERISSEN
      ! STATE = 0 --> KERN UNGERISSEN.
      ! STATE = 2 --> ZUSTAND IM LAYER NICHT DEFINIERT.

      END SUBROUTINE SUB_ZQ