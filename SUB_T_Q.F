!-----------------------------------------------------------------------
!     SUBROUTINE WELCHE DIE ZUGKRAFT AUS QUERKRAFT BERECHNET, UND DIE
!     SPANNUNGEN IM GLOBALEN KOORDINATENSYSTEM ZUR�CKGIBT.
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!
!     DATUM:              15.11.15
!-----------------------------------------------------------------------

      SUBROUTINE SUB_T_Q(VX,VY,V0,PHI_0,WINKELD,FSY,DV,
     &                       T_X_Q,T_Y_Q,T_XY_Q,RHO_Z)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX,INTENT(IN)     :: VX,VY,V0,PHI_0,WINKELD,FSY,DV

      DOUBLE COMPLEX,INTENT(OUT)    :: T_X_Q,T_Y_Q,T_XY_Q,RHO_Z

      DOUBLE COMPLEX                :: TAN_T,COT_T, PI

      INTERFACE

      FUNCTION TAN_C(Z)
        DOUBLE COMPLEX             :: TAN_C
        DOUBLE COMPLEX, INTENT(IN) :: Z
      END FUNCTION TAN_C

      END INTERFACE

      PI = CMPLX(4.0D0*ATAN(1.0D0),0D0,KIND=8)

      TAN_T = TAN_C(WINKELD*(PI/CMPLX(180D0,0D0,KIND=8)))
      COT_T = CMPLX(1D0,0D0,KIND=8)/TAN_T

      T_X_Q = VX*VX*COT_T/(CMPLX(2D0,0D0,KIND=8)*V0)
      T_Y_Q = VY*VY*COT_T/(CMPLX(2D0,0D0,KIND=8)*V0)
      T_XY_Q= VX*VY*COT_T/(CMPLX(2D0,0D0,KIND=8)*V0)

      RHO_Z = V0*TAN_T/(FSY*DV)

      END SUBROUTINE SUB_T_Q
