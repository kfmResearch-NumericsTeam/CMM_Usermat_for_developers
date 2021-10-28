!-----------------------------------------------------------------------
!     HILFSFUNKTION F�R STAHLSTOFFGESETZ F�R KALTVERFORMTE ST�HLE
!
!     PROGRAMMIERUNG:     DR. KAREL THOMA, HOCHSCHULE LUZERN
!                         PATRICK ROOS, HOCHSCHULE LUZERN
!     DATUM:              22.04.2014
!-----------------------------------------------------------------------
      FUNCTION F_S_E_KV(SIGMA,ES,KC,ALPHA)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION
      DOUBLE COMPLEX             :: F_S_E_KV
      DOUBLE COMPLEX, INTENT(IN) :: SIGMA,ES,KC,ALPHA

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
      END INTERFACE


      F_S_E_KV = SIGN_RC(1D0,SIGMA)*(ABS_C(SIGMA)/ES+(ABS_C(SIGMA)/KC)
     &           **ALPHA)

      RETURN

      END FUNCTION F_S_E_KV