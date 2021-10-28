*DECK,USERMAT      USERDISTRIB  PARALLEL

!-----------------------------------------------------------------------
!
!     SUBROUTINE USERMAT.F F�R DIE IMPLEMENTIERUNG MECHANISCHER MODELLE
!     F�R EINE NICHTLINEARE FE-ANALYSE VON STAHLBETONPELEMENTEN
!
!
!     PROGRAMMIERUNG:   DR. SLAV DIMITROV, CADFEM GMBH
!                       DR. KAREL THOMA, HOCHSCHULE LUZERN
!                       PATRICK ROOS, HOCHSCHULE LUZERN
!
!     DATUM:            25.08.2014
!
!-----------------------------------------------------------------------

      SUBROUTINE USERMAT(MATID,ELEMID,KDOMINTPT,KLAYER,KSECTPT,
     &                   LDSTEP,ISUBST,KEYCUT,
     &                   NDIRECT,NSHEAR,NCOMP,NSTATEV,NPROP,
     &                   TIME,DTIME,TEMP,DTEMP,
     &                   STRESS,USTATEV,DSDEPL,SEDEL,SEDPL,EPSEQ,
     &                   STRAIN,DSTRAIN,EPSPL,PROP,COORDS,
     &                   VAR0,DEFGRAD_T,DEFGRAD,
     &                   TSSTIF,EPSZZ,
     &                   VAR1,VAR2,VAR3,VAR4,VAR5,
     &                   VAR6,VAR7,VAR8)


!!    INCLUDE FILES
#INCLUDE "IMPCOM.INC"
#INCLUDE "ANSYSDEF.INC"
#INCLUDE "SYSPAR.INC"


!!    VARIABLENDEKLARATIONEN (F�R VORDEFINIERTE VARIABLEN VON USERMAT)

      INTEGER          :: MATID, ELEMID,
     &                    KDOMINTPT, KLAYER, KSECTPT,
     &                    LDSTEP, ISUBST, KEYCUT,
     &                    NDIRECT, NSHEAR, NCOMP, NSTATEV, NPROP

      DOUBLE PRECISION :: TIME,  DTIME, TEMP,  DTEMP,
     &                    SEDEL, SEDPL, EPSEQ, EPSZZ

      DOUBLE PRECISION :: STRESS  (NCOMP  ), USTATEV (NSTATEV),
     &                    DSDEPL  (NCOMP,NCOMP),
     &                    STRAIN  (NCOMP  ), DSTRAIN (NCOMP  ),
     &                    EPSPL   (NCOMP  ), PROP    (NPROP  ),
     &                    COORDS  (3),
     &                    DEFGRAD (3,3),     DEFGRAD_T(3,3),
     &                    TSSTIF  (2)

      DOUBLE PRECISION :: VAR0, VAR1, VAR2, VAR3, VAR4, VAR5,
     &                    VAR6, VAR7, VAR8


!!    EXTERNE SUBROUTINEN/FUNKTIONEN VON ANSYS (F�R PROGRAMMIERUNG USERMAT)

      EXTERNAL         :: WRINQR, ERHANDLER, SYSTOP


!!    LOKALE VARIABLENDEKLARATION (F�R PROGRAMMIERUNG USERMAT)

      CHARACTER*(64)   :: MSG
      INTEGER          :: IOTT, WRINQR
      DOUBLE PRECISION :: ZERO


!!    SPANNUNGSZUST�NDE

      IF (NDIRECT.EQ.2 .AND. NCOMP.EQ.3) THEN

        ! PLANE STRESS
        CALL USERMATPS(MATID,ELEMID,KDOMINTPT,KLAYER,KSECTPT,
     &                 LDSTEP,ISUBST,KEYCUT,
     &                 NDIRECT,NSHEAR,NCOMP,NSTATEV,NPROP,
     &                 TIME,DTIME,TEMP,DTEMP,
     &                 STRESS,USTATEV,DSDEPL,SEDEL,SEDPL,EPSEQ,
     &                 STRAIN,DSTRAIN,EPSPL,PROP,COORDS,
     &                 VAR0,DEFGRAD_T,DEFGRAD,
     &                 TSSTIF,EPSZZ,
     &                 VAR1,VAR2,VAR3,VAR4,VAR5,
     &                 VAR6,VAR7,VAR8)

      ELSE

        ! FEHLERMELDUNG & PROGRAMM-ABBRUCH
        ZERO = 0D0
        MSG = 'USERMAT: ONLY PLANE STRESS SUPPORTED (FATAL)!'
        IOTT = WRINQR(WR_OUTPUT)
        WRITE (IOTT,*) MSG
        CALL ERHANDLER('USERMAT',3010,4,MSG,ZERO,' ')
        CALL SYSTOP(11)

      END IF

      RETURN

      END SUBROUTINE USERMAT


!-------------------------------------------------------------------------------
!
!     SUBROUTINEN F�R SPANNUNGSZUST�NDE
!
!-------------------------------------------------------------------------------


*DECK,USERMAT1D    USERDISTRIB  PARALLEL          ! WIRD NICHT VERWENDET

      SUBROUTINE USERMAT1D(MATID,ELEMID,KDOMINTPT,KLAYER,KSECTPT,
     &                     LDSTEP,ISUBST,KEYCUT,
     &                     NDIRECT,NSHEAR,NCOMP,NSTATEV,NPROP,
     &                     TIME,DTIME,TEMP,DTEMP,
     &                     STRESS,USTATEV,DSDEPL,SEDEL,SEDPL,EPSEQ,
     &                     STRAIN,DSTRAIN,EPSPL,PROP,COORDS,
     &                     VAR0,DEFGRAD_T,DEFGRAD,
     &                     TSSTIF,EPSZZ,
     &                     VAR1,VAR2,VAR3,VAR4,VAR5,
     &                     VAR6,VAR7,VAR8)

      END SUBROUTINE USERMAT1D



*DECK,USERMAT3D    USERDISTRIB  PARALLEL          ! WIRD NICHT VERWENDET

      SUBROUTINE USERMAT3D(MATID,ELEMID,KDOMINTPT,KLAYER,KSECTPT,
     &                     LDSTEP,ISUBST,KEYCUT,
     &                     NDIRECT,NSHEAR,NCOMP,NSTATEV,NPROP,
     &                     TIME,DTIME,TEMP,DTEMP,
     &                     STRESS,USTATEV,DSDEPL,SEDEL,SEDPL,EPSEQ,
     &                     STRAIN,DSTRAIN,EPSPL,PROP,COORDS,
     &                     VAR0,DEFGRAD_T,DEFGRAD,
     &                     TSSTIF,EPSZZ,
     &                     VAR1,VAR2,VAR3,VAR4,VAR5,
     &                     VAR6,VAR7,VAR8)

      END SUBROUTINE USERMAT3D



*DECK,USERMATBM    USERDISTRIB  PARALLEL          ! WIRD NICHT VERWENDET

      SUBROUTINE USERMATBM(MATID,ELEMID,KDOMINTPT,KLAYER,KSECTPT,
     &                     LDSTEP,ISUBST,KEYCUT,
     &                     NDIRECT,NSHEAR,NCOMP,NSTATEV,NPROP,
     &                     TIME,DTIME,TEMP,DTEMP,
     &                     STRESS,USTATEV,DSDEPL,SEDEL,SEDPL,EPSEQ,
     &                     STRAIN,DSTRAIN,EPSPL,PROP,COORDS,
     &                     VAR0,DEFGRAD_T,DEFGRAD,
     &                     TSSTIF,EPSZZ,
     &                     VAR1,VAR2,VAR3,VAR4,VAR5,
     &                     VAR6,VAR7,VAR8)

      END SUBROUTINE USERMATBM



*DECK,USERMATPS    USERDISTRIB  PARALLEL          ! PLANE STRESS

      SUBROUTINE USERMATPS(MATID, ELEMID,KDOMINTPT,KLAYER,KSECTPT,
     &                     LDSTEP,ISUBST,KEYCUT,
     &                     NDIRECT,NSHEAR,NCOMP,NSTATEV,NPROP,
     &                     TIME,DTIME,TEMP,DTEMP,
     &                     STRESS,USTATEV,DSDEPL,SEDEL,SEDPL,EPSEQ,
     &                     STRAIN,DSTRAIN,EPSPL,PROP,COORDS,
     &                     VAR0,DEFGRAD_T,DEFGRAD,
     &                     TSSTIF,EPSZZ,
     &                     VAR1,VAR2,VAR3,VAR4,VAR5,
     &                     VAR6,VAR7,VAR8)


!!    INCLUDE FILES
#INCLUDE "IMPCOM.INC"
#INCLUDE "ANSYSDEF.INC"
#INCLUDE "SYSPAR.INC"


!!    VARIABLENDEKLARATIONEN (F�R VORDEFINIERTE VARIABLEN VON USERMATPS)

      INTEGER          :: MATID, ELEMID,
     &                    KDOMINTPT, KLAYER, KSECTPT,
     &                    LDSTEP, ISUBST, KEYCUT,
     &                    NDIRECT, NSHEAR, NCOMP, NSTATEV, NPROP

      DOUBLE PRECISION :: TIME,  DTIME, TEMP,  DTEMP,
     &                    SEDEL, SEDPL, EPSEQ, EPSZZ

      DOUBLE PRECISION :: STRESS  (NCOMP  ), USTATEV (NSTATEV),
     &                    DSDEPL  (NCOMP,NCOMP),
     &                    STRAIN  (NCOMP  ), DSTRAIN (NCOMP  ),
     &                    EPSPL   (NCOMP  ), PROP    (NPROP  ),
     &                    COORDS  (3),
     &                    DEFGRAD (3,3),     DEFGRAD_T(3,3),
     &                    TSSTIF  (2)

      DOUBLE PRECISION :: VAR0, VAR1, VAR2, VAR3, VAR4, VAR5,
     &                    VAR6, VAR7, VAR8


!!    EXTERNE SUBROUTINEN/FUNKTIONEN VON ANSYS (F�R PROGRAMMIERUNG USERMATPS)

      EXTERNAL         :: WRINQR, ERHANDLER, SYSTOP


!!    LOKALE VARIABLENDEKLARATION (F�R PROGRAMMIERUNG USERMATPS)


      DOUBLE PRECISION :: E_MAT(3,3), SPANNUNGEN(3),
     &                    HAUPT(3), DRUCK(11), DRUCKFELD(12), ZUG(11),
     &                    DRUCKZUG(11)
      DOUBLE PRECISION :: MATERIAL_ORG(76), MATERIAL(60)

      DOUBLE PRECISION :: VX,VY,V0,PHI_0,INT_X,INT_Y,INT_Z
	DOUBLE PRECISION :: R_ROHR,F_ROHR,N_LAYER,H_ELEMENT

      INTEGER          :: STATE(4)
      !DOUBLE COMPLEX   :: EX, EY, GAMMA_XY, I
      INTEGER          :: SCHICHTINFO, DIMENS,NVAL
      INTEGER          :: IREF(4)
      DOUBLE PRECISION :: ZSI(4)

      !DOUBLE PRECISION :: EPS(3)
      !DOUBLE COMPLEX   :: VC, EC, FCC, AUX, K_E

      CHARACTER*(64)   :: MSG
      INTEGER          :: IOTT, WRINQR
      DOUBLE PRECISION :: ZERO

!     VARIABLENDEKLARATION TIMELOCK

      CHARACTER DATE*8, ZEIT*10, ZONE*5
      INTEGER          :: VALUES(8), JAHR, MONAT, TAG, STUNDE, MINUTE,
     &                    ZEITWERT, GRENZWERT, JAHR_E, MONAT_E, TAG_E,
     &                    STUNDE_E, MINUTE_E
      CHARACTER*8      :: HN_
      INTEGER*4        :: RES_HN

!-----------------------------------------------------------------------
!
!     BEGINN USERMAT 3.0
!     (MW, 11.05.2021: KEINE ABLAUFDATUM UND COMPUTERHOST CHECK!!!)
!-----------------------------------------------------------------------
      ZEITWERT=1
      GRENZWERT=2
      RES_HN=1
      IF (ELEMID .EQ. 1 .AND. LDSTEP .EQ. 1 .AND. ISUBST .EQ. 1 .AND.
     &    KLAYER .EQ. 1.AND. KDOMINTPT .EQ. 1 ) THEN
      CALL DATE_AND_TIME(DATE,ZEIT,ZONE,VALUES)
      JAHR = VALUES(1)
      MONAT = VALUES(2)
      TAG = VALUES(3)
      STUNDE = VALUES(5)
      MINUTE = VALUES(6)
      ZEITWERT = MINUTE + STUNDE*60 + TAG*24*60 + MONAT*30*24*60 +
     &           JAHR*12*30*24*60

!     MASCHINENNAMEN
      HN_ = "ibkpika8087"
!      HN_ = "N0004395"
!      HN_ = "IBKNUMBAT"

!     ABLAUFDATUM:
      JAHR_E = 2025
      MONAT_E = 12
      TAG_E = 31
      STUNDE_E = 0
      MINUTE_E = 0
      GRENZWERT = MINUTE_E+STUNDE_E*60+TAG_E*24*60+MONAT_E*30*24*60 +
     &            JAHR_E*12*30*24*60

      CALL CHECK_HN(RES_HN,HN_)

      END IF


      !IF ((ZEITWERT .GT. GRENZWERT)) THEN
          
      ! FEHLERMELDUNG & PROGRAMM-ABBRUCH
      !  ZERO = 0D0
      !  MSG = 'USERMAT: LIZENZ IST ABGELAUFEN!'
      !  IOTT = WRINQR(WR_OUTPUT)
      !  WRITE (IOTT,*) MSG
      !  CALL ERHANDLER('USERMAT',3010,4,MSG,ZERO,' ')
      !  CALL SYSTOP(11)

      !ELSE

      ! UNTERSCHEIDUNG 2D / 3D:
      MATERIAL_ORG = PROP
      DIMENS = MATERIAL_ORG (1)

      IF (ABS(DIMENS-1) .LT. 0.1D0) THEN
        KLAYER = 1
      ELSE
        KLAYER = KLAYER
      END IF
      !PRINT*, KLAYER

      STATE = 0
      HAUPT = 0D0
      DRUCK = 0D0
      DRUCKFELD = 0D0
      DRUCKZUG = 0D0
      ZUG = 0D0

      !! FALLS SCHICHT IM INTERGRATIONSPUNKT GERISSEN K_RISS=0D0 !!

      IF (LDSTEP .EQ. 1) THEN
        IF (ISUBST .GT. 1) THEN
          CALL SUB_CRACKED(USTATEV,NSTATEV,MATERIAL_ORG,NPROP)
         END IF
      ELSE
        CALL SUB_CRACKED(USTATEV,NSTATEV,MATERIAL_ORG,NPROP)
      END IF

      ! SUBROUTINE SCHICHTINFORMATION:
      CALL SUB_SCHICHTINFO(MATERIAL_ORG,KLAYER,SCHICHTINFO,IREF,ZSI)

      ! SUBROUTINE MATERIALZUORDNUNG:
      CALL SUB_MATERIAL(MATERIAL_ORG,SCHICHTINFO,IREF,ZSI,
     &                  KLAYER,ELEMID,MATERIAL)
      !PRINT*, MATERIAL
      ! SUBROUTINE MECHANISCHS MODELL:
      CALL SUB_MECH_MODELL(STRESS,STRAIN,DSTRAIN,MATERIAL,SCHICHTINFO,
     &                     E_MAT,SPANNUNGEN,HAUPT,DRUCK,DRUCKFELD,
     &                     DRUCKZUG,ZUG,STATE)
	
	!Subroutine für die Berücksichtigung von Hohlkörper
	
	H_ELEMENT=MATERIAL_ORG(4)
	N_LAYER=MATERIAL_ORG(3)
	R_ROHR=MATERIAL_ORG(76)
	
	IF (R_ROHR.GT.0D0) THEN
		Call SUB_FAK_HK(KLAYER,N_LAYER,H_ELEMENT,R_ROHR,F_ROHR)
	ELSE
		F_ROHR=1D0
	END IF
	
      ! R�CKGABE SPANNUNGEN & WERKSTOFFMATRIX:
      STRESS(1) = SPANNUNGEN(1)*F_ROHR
      STRESS(2) = SPANNUNGEN(2)*F_ROHR
      STRESS(3) = SPANNUNGEN(3)*F_ROHR

      DSDEPL(1,1) = E_MAT(1,1)*F_ROHR
      DSDEPL(1,2) = E_MAT(1,2)*F_ROHR
      DSDEPL(1,3) = E_MAT(1,3)*F_ROHR
      DSDEPL(2,1) = E_MAT(2,1)*F_ROHR
      DSDEPL(2,2) = E_MAT(2,2)*F_ROHR
      DSDEPL(2,3) = E_MAT(2,3)*F_ROHR
      DSDEPL(3,1) = E_MAT(3,1)*F_ROHR
      DSDEPL(3,2) = E_MAT(3,2)*F_ROHR
      DSDEPL(3,3) = E_MAT(3,3)*F_ROHR


      ! SPEICHERN DER OUTPUT-VARIABLEN:
      USTATEV(1) = STATE(1)
      USTATEV(2) = STATE(2)
      USTATEV(3) = STATE(3)
      USTATEV(4) = STATE(4)

      USTATEV(5) = HAUPT(1)
      USTATEV(6) = HAUPT(2)
      USTATEV(7) = HAUPT(3)

      USTATEV(8) = DRUCK(1)
      USTATEV(9) = DRUCK(2)
      USTATEV(10) = DRUCK(3)
      USTATEV(11) = DRUCK(4)
      USTATEV(12) = DRUCK(5)
      USTATEV(13) = DRUCK(6)
      USTATEV(14) = DRUCK(7)
      USTATEV(15) = DRUCK(8)
      USTATEV(16) = DRUCK(9)
      USTATEV(17) = DRUCK(10)

      USTATEV(18) = DRUCKFELD(1)
      USTATEV(19) = DRUCKFELD(2)
      USTATEV(20) = DRUCKFELD(3)
      USTATEV(21) = DRUCKFELD(4)
      USTATEV(22) = DRUCKFELD(5)
      USTATEV(23) = DRUCKFELD(6)
      USTATEV(24) = DRUCKFELD(7)
      USTATEV(25) = DRUCKFELD(8)
      USTATEV(26) = DRUCKFELD(9)
      USTATEV(27) = DRUCKFELD(10)
      USTATEV(28) = DRUCKFELD(11)

      USTATEV(29) = DRUCKZUG(1)
      USTATEV(30) = DRUCKZUG(2)
      USTATEV(31) = DRUCKZUG(3)
      USTATEV(32) = DRUCKZUG(4)
      USTATEV(33) = DRUCKZUG(5)
      USTATEV(34) = DRUCKZUG(6)
      USTATEV(35) = DRUCKZUG(7)
      USTATEV(36) = DRUCKZUG(8)
      USTATEV(37) = DRUCKZUG(9)
      USTATEV(38) = DRUCKZUG(10)

      USTATEV(39) = ZUG(1)
      USTATEV(40) = ZUG(2)
      USTATEV(41) = ZUG(3)
      USTATEV(42) = ZUG(4)
      USTATEV(43) = ZUG(5)
      USTATEV(44) = ZUG(6)
      USTATEV(45) = ZUG(7)
      USTATEV(46) = ZUG(8)
      USTATEV(47) = ZUG(9)
      USTATEV(48) = ZUG(10)

      USTATEV(49)  = E_MAT(1,1)*F_ROHR
      USTATEV(50)  = E_MAT(1,2)*F_ROHR
      USTATEV(51)  = E_MAT(1,3)*F_ROHR
      USTATEV(52)  = E_MAT(2,1)*F_ROHR
      USTATEV(53)  = E_MAT(2,2)*F_ROHR
      USTATEV(54)  = E_MAT(2,3)*F_ROHR
      USTATEV(55)  = E_MAT(3,1)*F_ROHR
      USTATEV(56)  = E_MAT(3,2)*F_ROHR
      USTATEV(57)  = E_MAT(3,3)*F_ROHR

      USTATEV(58)  = IREF(1)
      USTATEV(59)  = IREF(2)
      USTATEV(60)  = IREF(3)
      USTATEV(61)  = IREF(4)

      USTATEV(62)  = MAX(ZUG(11),DRUCK(11),DRUCKZUG(11),DRUCKFELD(12))


      USTATEV(63)  = COORDS(1)
      USTATEV(64)  = COORDS(2)
      USTATEV(65)  = COORDS(3)

      USTATEV(66) = SPANNUNGEN(1)*F_ROHR
      USTATEV(67) = SPANNUNGEN(2)*F_ROHR
      USTATEV(68) = SPANNUNGEN(3)*F_ROHR

      USTATEV(69) = STRAIN(1)+DSTRAIN(1)
      USTATEV(70) = STRAIN(2)+DSTRAIN(2)
      USTATEV(71) = STRAIN(3)+DSTRAIN(3)
	
	USTATEV(72) = F_ROHR


      CONTINUE

      RETURN

      !END IF

      END SUBROUTINE USERMATPS

	

!------------------------------------ENDE---------------------------------------


