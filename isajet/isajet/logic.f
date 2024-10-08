#include "PILOT.inc"
      SUBROUTINE LOGIC
C
C                            10/ 3/80
C            STARTING FROM USER DATA FIND OUT WHICH PARAMETERS SHOULD
C            BE FIXED AND WHICH LIMITS SHOULD BE SET
C
#include "itapes.inc"
#include "jetlim.inc"
#include "primar.inc"
#include "jetpar.inc"
#include "const.inc"
#include "dylim.inc"
#include "keys.inc"
#include "q1q2.inc"
      LOGICAL COMB(8)
      DIMENSION DELPH(3)
C
C        LOGICAL FUNCTIONS
C
      LOGICAL LOGP,LOGPT,LOGYTH,LOGX,LOGPHI
      LOGICAL LOGQM,LOGQT,LOGYW,LOGTHW,LOGPHW,LOGXW
      LOGICAL LOGMIJ,LOGMGM,LOGMGY
      DATA UNDEF/-.9E9/
      DATA ZERO/.00001/,ONE/.99999/
C
C            INVERSE HYPERBOLIC COSINE FUNCTION
      ACOSH(X)=ALOG(X+SQRT(X**2-1.0))
C            INVERSE HYPERBOLIC SINE FUNCTION
      ASINH(X)=ALOG(X+SQRT(X**2+1.0))
C
C          INITIALIZE CONSTANTS
C
      HALFPI=PI/2.
      IFATAL=0
      IERR=0
      DO 1 I=1,36
      SETLMJ(I)=.TRUE.
      IF(BLIMS(I).GT.UNDEF) SETLMJ(I)=.FALSE.
    1 CONTINUE
      DO 2 I=1,12
      SETLMQ(I)=.TRUE.
      IF(BLIM1(I).GT.UNDEF) SETLMQ(I)=.FALSE.
    2 CONTINUE
C
C          SET STANDARD DRELL-YAN IF FIXED QTW=0.
      IF(KEYS(3)) THEN
        IF(QTMIN.EQ.0..AND.QTMAX.LT.UNDEF) THEN
          STDDY=.TRUE.
        ELSE
          STDDY=.FALSE.
        ENDIF
      ELSEIF(KEYS(7).OR.KEYS(9)) THEN
        STDDY=.TRUE.
      ELSEIF(KEYS(11)) THEN
        STDDY=.FALSE.
      ENDIF
C
      IF(STDDY) THEN
        NJET=2
        FIXPT(3)=.TRUE.
        PT(3)=0.
        PTMIN(3)=0.
        PTMAX(3)=0.
        FIXPHI(3)=.FALSE.
        PHIMIN(3)=0.
        PHIMAX(3)=2.*PI
        DELPH(3)=2.*PI
        FIXPHW=.TRUE.
        PHWMIN=0.
        PHWMAX=-1.E9
        PHIW=0.
        QTMIN=0.
        QTMAX=-1.E9
        QTW=0.
        FIXQT=.FALSE.
      ENDIF
C
C      CHECK THAT PARAMETER RANGES MAKE SENSE
C
C            DO LOGIC FOR P
      IF(.NOT.LOGP(IERR)) IFATAL=IFATAL+1
C            DO LOGIC FOR PT
      IF(.NOT.LOGPT(IERR)) IFATAL=IFATAL+1
C            DO LOGIC FOR THETA AND YJ(RAPIDITY)
      IF(.NOT.LOGYTH(IERR)) IFATAL=IFATAL+1
C            DO LOGIC FOR XJ(FEYNMAN X)
C            XJ LIMITS DO NOT REDEFINE PT LIMITS
      IF(.NOT.LOGX(IERR)) IFATAL=IFATAL+1
C            DO LOGIC FOR PHI
C            NOTE THAT PHI INTERVAL IS DEFINED BY PHIMAX-PHIMIN
      IF(.NOT.LOGPHI(IERR,DELPH)) IFATAL=IFATAL+1
C
C            DO LOGIC FOR MADGRAPH IF APPLICABLE
      IF(KEYS(12)) THEN
        IF(.NOT.LOGMGM(IERR)) IFATAL=IFATAL+1
        IF(.NOT.LOGMGY(IERR)) IFATAL=IFATAL+1
        IF(.NOT.LOGMIJ(IERR)) IFATAL=IFATAL+1
      ENDIF
C
C          SET DEFAULT PT LIMITS IF NONE WERE SET
      IF((KEYS(1).OR.KEYS(5).OR.KEYS(6).OR.KEYS(10)).AND.
     $(PTMAX(1).GT..99*HALFE).AND.(PTMAX(2).GT..99*HALFE)) THEN
        PTMIN(1)=0.1*HALFE
        PTMIN(2)=PTMIN(1)
        PTMAX(1)=0.4*HALFE
        PTMAX(2)=PTMAX(1)
        CALL LOGERR(0,1,IERR)
      ENDIF
C
C          CHECK Y LIMITS WITH FINAL PT LIMITS.
      IF(KEYS(1).OR.KEYS(5).OR.KEYS(6).OR.KEYS(10)) THEN
        YMXPT=ALOG(ECM/PTMIN(1))
        DO 11 I=1,2
        YJMAX(I)=AMIN1(YJMAX(I),YMXPT)
11      YJMIN(I)=AMAX1(YJMIN(I),-YMXPT)
      ENDIF
C
C            DO LOGIC FOR DRELL YAN VARIABLES
      IF(KEYS(3).OR.KEYS(7).OR.KEYS(9).OR.KEYS(11)) THEN
C            DO LOGIC FOR QM
        IF(.NOT.LOGQM(IERR)) IFATAL=IFATAL+1
C            DO LOGIC FOR QT
        IF(.NOT.LOGQT(IERR)) IFATAL=IFATAL+1
C            DO LOGIC FOR YW
        IF(.NOT.LOGYW(IERR)) IFATAL=IFATAL+1
C            DO LOGIC FOR THETA
        IF(.NOT.LOGTHW(IERR)) IFATAL=IFATAL+1
C            DO LOGIC FOR PHW
C            NOTE THAT PHW INTERVAL DEFINED BY PHWMAX-PHWMIN
        IF(.NOT.LOGPHW(IERR,DELPH)) IFATAL=IFATAL+1
C            DO LOGIC FOR XW
        IF(.NOT.LOGXW(IERR)) IFATAL=IFATAL+1
C
      ENDIF
C
C            CHECK FOR INCONSISTENCIES
      DO 21 I=1,NJET
      SMIN=SIN(THMIN(I))
      SMAX=SIN(THMAX(I))
      IF(SMAX.LT.SMIN) SMIN=SMAX
      PT1=PMIN(I)*SMIN
      IF(PT1.GT.PTMIN(I)) PTMIN(I)=PT1
      SMAX=1.0
      IF(THMAX(I).LT.ONE*HALFPI) SMAX=SIN(ONE*THMAX(I))
      IF(THMIN(I).GT.ONE*HALFPI) SMAX=SIN(ONE*THMIN(I))
      PT1=PMAX(I)*SMAX
      IF(PT1.LT.ONE*PTMAX(I)) PTMAX(I)=PT1
      IF(PTMAX(I).LT.ONE*PTMIN(I)) CALL LOGERR(2,I,IFATAL)
      IF(PMAX(I).LT.ONE*PMIN(I)) CALL LOGERR(1,I,IFATAL)
      IF(THMAX(I).LT.ONE*THMIN(I)) CALL LOGERR(3,I,IFATAL)
      IF(XJMAX(I).LT.ONE*XJMIN(I)) CALL LOGERR(4,I,IFATAL)
      IF(ABS(XJMAX(I)).GT.1.0+ZERO.OR.ABS(XJMIN(I)).GT.1.0+ZERO)
     1    CALL LOGERR(4,I,IFATAL)
      IF(THMIN(I).LT.-ZERO.OR.THMAX(I).GT.PI+ZERO)
     $CALL LOGERR(6,I,IFATAL)
C
      IF(FIXXJ(I)) THEN
        X1=PMAX(I)*COS(THMIN(I))/HALFE
        X2=PMIN(I)*COS(THMAX(I))/HALFE
        X3=PMAX(I)*COS(THMIN(I))/HALFE
        IF(X3.LT.X2) X2=X3
        IF(X1.EQ.X2) XJ(I)=X1
        IF(XJ(I).LT.ONE*X2.OR.XJ(I).GT.X1/ONE) CALL LOGERR(7,I,IFATAL)
      ENDIF
C
   21 CONTINUE
C
C            CHECK THAT PARAMETERS FOR DRELL YAN ARE CONSISTENT
C
      IF(KEYS(3)) THEN
        COMB(1)=.FALSE.
        DO 31 I=1,2
        COMB(1)=COMB(1).OR.FIXP(I).OR.FIXPT(I).OR.FIXYJ(I).OR.FIXPHI(I)
     1  .OR.FIXXJ(I)
   31   CONTINUE
        IF(COMB(1)) CALL LOGERR(114,1,IFATAL)
        COMB(1)=FIXQT.AND.FIXQM
        COMB(2)=FIXQT.AND.FIXYW
        COMB(3)=FIXQM.AND.FIXYW
        COMB(4)=COMB(1).AND.FIXYW
        COMB(5)=COMB(1).AND.FIXXW
        COMB(6)=COMB(2).AND.FIXXW
        COMB(7)=COMB(3).AND.FIXXW
        IF(COMB(4).AND.FIXXW) CALL LOGERR(115,1,IFATAL)
        IF(COMB(4)) FIXXW=.TRUE.
C
        IF(COMB(4)) THEN
          FIXXW=.TRUE.
          XW=SQRT(QTW**2+QMW**2)*SINH(YW)/HALFE
          IF(XW.LT.XWMIN-ZERO.OR.XW.GT.XWMAX+ZERO)
     $     CALL LOGERR(101,1,IFATAL)
          XWMIN=XW
          XWMAX=XW
        ENDIF
C
        IF(COMB(5)) THEN
          FIXYW=.TRUE.
          YW=ASINH(HALFE*XW/SQRT(QTW**2+QMW**2))
      IF(YW.LT.YWMIN-ZERO.OR.YW.GT.YWMAX+ZERO)
     $CALL LOGERR(102,1,IFATAL)
          YWMIN=YW
          YWMAX=YW
        ENDIF
C
        IF(COMB(6)) THEN
        IF(XW.NE.0.) THEN
          QMW2=((XW*HALFE)/SINH(YW))**2-QTW**2
          IF(QMW2.GE.0) THEN
            QMW=SQRT(QMW2)
      IF(QMW.LT.ONE*QMIN.OR.QMW.GT.QMAX/ONE)
     $CALL LOGERR(103,1,IFATAL)
          ENDIF
          CALL LOGERR(104,1,IFATAL)
        ENDIF
        ENDIF
C
        IF(COMB(7).AND.(YW.NE.0)) THEN
          FIXQT=.TRUE.
          FIXPT(3)=.TRUE.
          QTW2=((XW*HALFE)/SINH(YW))**2-QMW**2
          IF(QTW2.GE.0) THEN
            QTW=SQRT(QTW2)
            PT(3)=QTW
      IF(QTW.LT.ONE*QTMIN.OR.QTW.GT.QTMAX/ONE)
     $CALL LOGERR(105,1,IFATAL)
          ENDIF
          CALL LOGERR(106,1,IFATAL)
        ENDIF
C
        IF(QTMIN.GT.QTMAX/ONE) CALL LOGERR(107,1,IFATAL)
        IF(QMIN.GT.QMAX/ONE) CALL LOGERR(108,1,IFATAL)
        IF(THWMIN.GT.THWMAX/ONE) CALL LOGERR(109,1,IFATAL)
        IF(PHWMIN.GT.PHWMAX/ONE) CALL LOGERR(110,1,IFATAL)
        IF(XWMIN.GT.XWMAX/ONE) CALL LOGERR(111,1,IFATAL)
        IF(YWMIN.GT.YWMAX+ZERO) CALL LOGERR(112,1,IFATAL)
        IF(ABS(XWMIN).GT.1.0+ZERO.OR.ABS(XWMAX).GT.1.0+ZERO)
     1  CALL LOGERR(111,1,IFATAL)
      IF(THWMIN.LT.-ZERO.OR.THWMAX.GT.PI+ZERO)
     $CALL LOGERR(109,1,IFATAL)
      ENDIF
C
C          SPECIAL LOGIC FOR E+E- EVENTS
C
      IF(KEYS(2)) THEN
        THLOW=AMAX1(THMIN(1),PI-THMAX(2))
        THHIGH=AMAX1(THMAX(1),PI-THMIN(2))
        IF(THHIGH-THLOW.LT.ZERO.AND..NOT.(FIXYJ(1).OR.FIXYJ(2))) THEN
          CALL LOGERR(116,1,IFATAL)
        ELSE
          DO 61 I=1,2
          FIXYJ(I)=FIXYJ(1).OR.FIXYJ(2)
          FIXXJ(I)=FIXXJ(1).OR.FIXXJ(2)
          FIXPT(I)=FIXPT(1).OR.FIXPT(2)
          THMIN(I)=THLOW
          THMAX(I)=THHIGH
          IF(FIXYJ(I)) THMAX(I)=THMIN(I)
          XJMIN(I)=COS(THMAX(I))
          XJMAX(I)=COS(THMIN(I))
          PTMIN(I)=HALFE*AMIN1(SIN(THMIN(I)),SIN(THMAX(I)))
          IF(ABS(XJMAX(I)).LT.1.) YJMAX(I)=
     1    .5*ALOG((1.+XJMAX(I))/(1.-XJMAX(I)))
          IF(ABS(XJMIN(I)).LT.1.) YJMIN(I)=
     1    .5*ALOG((1.+XJMIN(I))/(1.-XJMIN(I)))
   61     CONTINUE
        ENDIF
      ENDIF
C
C
      IF(IFATAL.NE.0) THEN
        WRITE(ITLIS,1020) IFATAL
 1020   FORMAT(////10X,I10,' FATAL ERRORS, JOB TERMINATED')
        STOP 99
      ENDIF
C
C
      RETURN
      END
