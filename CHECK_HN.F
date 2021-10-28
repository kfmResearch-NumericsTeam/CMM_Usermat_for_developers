      SUBROUTINE CHECK_HN(R_HN,HN_)

      IMPLICIT NONE

      ! VARIABLENDEKLARATION

      CHARACTER*8,INTENT(IN) :: HN_
      INTEGER*4,INTENT(OUT)  ::R_HN
      INTEGER*4 :: HOSTNM, STATUS
      CHARACTER*8 :: NAME

        R_HN=0
        STATUS = HOSTNM( NAME )
        
      IF (HN_ == NAME) THEN
        R_HN=1
      ENDIF
      
      ! PRINT*, NAME

	END SUBROUTINE