#include "PILOT.inc"
      SUBROUTINE SIGEE
C
C          Compute d(sigma)/d(cos theta) with interference 
C          and polarization for
C          E+ E- --> GM, Z0 ----> QK QB, L LB, N NB, W+ W-, Z Z
C
C          SIGS(I)  = partial cross section for I1 + I2 --> I3 + I4.
C          INOUT(I) = IOPAK**3*I4 + IOPAK**2*I3 + IOPAK*I2 + I1
C                     USING JETTYPE CODE.
C
C          Extra factor of 1/2 needed because all jets are treated
C          as identical.
C          Version 7.42 includes bremsstrahlung contribution;
C          also, beamstrahlung
C          Version 7.54: Add Z+H
C          Add gamma+gamma -> f+ fbar 1/27/04
C
#ifdef IMPNONE_X
      IMPLICIT NONE
#endif
#include "itapes.inc"
#include "jetsig.inc"
#include "eepar.inc"
#include "primar.inc"
#include "jetpar.inc"
#include "q1q2.inc"
#include "const.inc"
#include "wcon.inc"
#include "brembm.inc"
#include "hcon.inc"
C
      REAL FLEP,FLEM,FREP,FREM,PROPZ,REDZ,SH,E,G,GP,COS2W,
     $TNTHW,CTTHW,ALQ(2),BEQ(2),ALL(2),BEL(2),AE,BE,EQ,AMQ,AMQ2,
     $PCM,Z,AF,BF,PHILRG,PHILRZ,PHILRI,PHIRLG,PHIRLZ,PHIRLI,
     $THT,UH,RSH,UT,PHIRL,PHILR,SIGLR,SIGRL,SIG,AMASS,EE,
     $ALFAEM,AMZ,GAMZ,AMW,JAC,ESTRUC,SSFEL,FACLR,FACRL,EZ0,FAC1,
     $BKT_GG,SIG_GG,GSTRUC,GBEAM
      REAL AMH,SSXLAM
      INTEGER I,IQ,IQ2,IFL,ISGN,IQ2EQ(25),LISTJ(29)
      DATA IQ2EQ/0,2,-2,-1,1,-1,1,2,-2,-1,1,2,-2,0,0,-3,3,
     $0,0,-3,3,0,0,-3,3/
      DATA LISTJ/
     $9,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,
     $11,-11,12,-12,13,-13,14,-14,15,-15,16,-16,
     $10,80,-80,90/
C          Fractional polarizations
      FLEP=(1.+PLEP)/2.
      FLEM=(1.+PLEM)/2.
      FREP=(1.-PLEP)/2.
      FREM=(1.-PLEM)/2.
C          FUNCTIONS
      ALFAEM=1./128.
      AMZ=WMASS(4)
      GAMZ=WGAM(4)
      AMW=WMASS(3)
      AMH=HMASS
      IF (IBREM) THEN
        SH=SHAT
        JAC=2*(1.-SHAT/SCM)*2*SQRT(SH)*(RSHMAX-RSHMIN)/SCM/(X1+X2)
      ELSE
        SH=SCM
      END IF
      PROPZ=(SH-AMZ**2)**2+AMZ**2*GAMZ**2
      REDZ=(SH-AMZ**2)/PROPZ
C
C          CONSTANTS
      RSH=SQRT(SH)
      EE=RSH/2.
      QSQBM=QSQ
      E=SQRT(4*PI*ALFAEM)
      G=SQRT(4*PI*ALFAEM/SIN2W)
      GP=G*SQRT(SIN2W/(1.-SIN2W))
      COS2W=1.-SIN2W
      TNTHW=SQRT(SIN2W/COS2W)
      CTTHW=1./TNTHW
      ALQ(1)=CTTHW/4.-5*TNTHW/12.
      BEQ(1)=-(CTTHW+TNTHW)/4.
      ALQ(2)=TNTHW/12.-CTTHW/4.
      BEQ(2)=-BEQ(1)
      ALL(1)=(CTTHW+TNTHW)/4.
      BEL(1)=-(CTTHW+TNTHW)/4.
      ALL(2)=(3*TNTHW-CTTHW)/4.
      BEL(2)=-BEL(1)
      AE=ALL(2)
      BE=BEL(2)
C
C          ENTRY
      SIG=0.
      SIGMA=0.
      NSIGS=0
      DO 10 I=1,MXSIGS
10    SIGS(I)=0.
C
C          Sum over allowed jet types. IQ labels JETTYPE1.
C
      DO 100 IQ=2,25
        IQ2=MATCH(IQ,4)
        IF(.NOT.(GOQ(IQ,1).AND.GOQ(IQ2,2))) GO TO 100
        IFL=IQ/2
        EQ=ABS(FLOAT(IQ2EQ(IQ))/3.)
        IF (EQ.LT..5.OR.EQ.GT..8) EQ=-EQ
        ISGN=1
        IF(2*IFL.NE.IQ) ISGN=2
        AMQ=AMASS(LISTJ(IQ))
        AMQ2=AMQ**2
        IF(2.*AMQ.GE.ECM) GO TO 100
        PCM=.5*SQRT(SH-4.*AMQ2)
        Z=CTH(ISGN)
        IF (IQ.LE.13.AND.ABS(EQ).GT..5) THEN
          AF=ALQ(1)
          BF=BEQ(1)
        ELSE IF (IQ.LE.13.AND.ABS(EQ).LT..5) THEN
          AF=ALQ(2)
          BF=BEQ(2)
        ELSE IF (IQ.GT.13.AND.ABS(EQ).EQ.0.) THEN
          AF=ALL(1)
          BF=BEL(1)
        ELSE
          AF=ALL(2)
          BF=BEL(2)
        END IF
        PHILRG=EQ**2/SH**2*(EE**2*(1.+Z**2)+AMQ2*(1.-Z**2))
        PHILRZ=(AE-BE)**2/PROPZ*((AF**2+BF**2)*(EE**2+PCM**2*Z**2)-
     ,     4*AF*BF*EE*PCM*Z+(AF**2-BF**2)*AMQ2)
        PHILRI=-2*EQ*(AE-BE)*REDZ/SH*
     ,     (AF*(EE**2*(1.+Z**2)+AMQ2*(1.-Z**2))-2*BF*EE*PCM*Z)
        PHILR=E**4*(PHILRG+PHILRZ+PHILRI)
        PHIRLG=PHILRG
        PHIRLZ=(AE+BE)**2/PROPZ*((AF**2+BF**2)*(EE**2+PCM**2*Z**2)+
     ,     4*AF*BF*EE*PCM*Z+(AF**2-BF**2)*AMQ2)
        PHIRLI=-2*EQ*(AE+BE)*REDZ/SH*
     ,     (AF*(EE**2*(1.+Z**2)+AMQ2*(1.-Z**2))+2*BF*EE*PCM*Z)
        PHIRL=E**4*(PHIRLG+PHIRLZ+PHIRLI)
        SIGLR=4*PCM*PHILR/16./PI/EE
        SIGRL=4*PCM*PHIRL/16./PI/EE
        SIG=(FLEM*FREP*SIGLR+FREM*FLEP*SIGRL)*UNITS/2.
        BKT_GG=(EE**2+PCM**2*Z**2)/(AMQ2+PCM**2*(1.-Z**2))+
     ,        2*AMQ2/(AMQ2+PCM**2*(1.-Z**2))-2*AMQ2**2/
     ,        (AMQ2+PCM**2*(1.-Z**2))**2
        SIG_GG=EQ**4*2*PI*ALFAEM**2/SH*(PCM/EE)*BKT_GG*UNITS/2.
        IF (IQ.LE.13) THEN
          SIG=3*SIG
          SIG_GG=3*SIG_GG
        END IF
        IF (IBREM.AND..NOT.IBEAM) THEN
          SIG=SIG*ESTRUC(X1,QSQ)*ESTRUC(X2,QSQ)*JAC
          SIG=SIG+SIG_GG*GSTRUC(X1,QSQ)*GSTRUC(X2,QSQ)*JAC
        ELSE IF (IBEAM) THEN
          SIG=SIG*SSFEL(X1,0)*SSFEL(X2,0)*JAC
          IF (GAMGAM) THEN
          SIG=SIG+SIG_GG*GSTRUC(X1,QSQ)*GSTRUC(X2,QSQ)*JAC+
     ,    SIG_GG*GBEAM(X1,EB)*GBEAM(X2,EB)*JAC
          END IF
        END IF
        CALL SIGFIL(SIG,0,0,IQ,IQ2)
100   CONTINUE
C           Z Z Cross section
      IF(.NOT.(GOQ(29,1).AND.GOQ(29,2))) GO TO 200
        PCM=.5*SQRT(SH-4.*AMZ**2)
        THT=AMZ**2-SH/2.+RSH*PCM*CTH(1)
        UH=2*AMZ**2-SH-THT
        SIGLR=4*E**4*(AE-BE)**4*PCM/16./PI/SH/RSH*
     ,   (UH/THT+THT/UH+4*AMZ**2*SH/UH/THT-AMZ**4*(1./THT**2+1./UH**2))
        SIGRL=4*E**4*(AE+BE)**4*PCM/16./PI/SH/RSH*
     ,   (UH/THT+THT/UH+4*AMZ**2*SH/UH/THT-AMZ**4*(1./THT**2+1./UH**2))
        SIG=(FLEM*FREP*SIGLR+FREM*FLEP*SIGRL)*UNITS/2.
        IF (IBREM.AND..NOT.IBEAM) THEN
          SIG=SIG*ESTRUC(X1,QSQ)*ESTRUC(X2,QSQ)*JAC
        ELSE IF (IBEAM) THEN
          SIG=SIG*SSFEL(X1,0)*SSFEL(X2,0)*JAC
        END IF
        CALL SIGFIL(SIG,0,0,29,29)
200   CONTINUE
C           W W Cross section
      IF(.NOT.(GOQ(27,1).AND.GOQ(28,2))) GO TO 300
        PCM=.5*SQRT(SH-4.*AMW**2)
        THT=AMW**2-SH/2.+RSH*PCM*CTH(2)
        UH=2*AMW**2-SH-THT
        UT=UH*THT-AMW**4
        PHIRL=4*(AE+BE)**2*TNTHW**2/SH/SH/PROPZ*
     ,        (UT*(PCM**2*SH+3*AMW**4)+4*AMW**2*PCM**2*SH*SH)
        PHILR=UT/SH/SH*(3.+2*(AE-BE)*TNTHW*(SH-6*AMW**2)*REDZ+
     ,      4*(AE-BE)**2*TNTHW**2*(PCM**2*SH+3*AMW**4)/PROPZ)+
     ,      8*(AE-BE)*TNTHW*AMW**2*REDZ+16*(AE-BE)**2*TNTHW**2*
     ,      AMW**2*PCM**2/PROPZ+2*(1.-2*(AE-BE)*TNTHW*AMW**2*REDZ)*
     ,      (UT/SH/THT-2*AMW**2/THT)+UT/THT**2
        SIGLR=4*E**4*PCM/64./PI/SH/RSH/SIN2W**2*PHILR
        SIGRL=4*E**4*PCM/64./PI/SH/RSH/SIN2W**2*PHIRL
        SIG=(FLEM*FREP*SIGLR+FREM*FLEP*SIGRL)*UNITS/2.
        IF (IBREM.AND..NOT.IBEAM) THEN
          SIG=SIG*ESTRUC(X1,QSQ)*ESTRUC(X2,QSQ)*JAC
        ELSE IF (IBEAM) THEN
          SIG=SIG*SSFEL(X1,0)*SSFEL(X2,0)*JAC
        END IF
        CALL SIGFIL(SIG,0,0,27,28)
300   CONTINUE
      IF(.NOT.(GOQ(28,1).AND.GOQ(27,2))) GO TO 400
        PCM=.5*SQRT(SH-4.*AMW**2)
        THT=AMW**2-SH/2.+RSH*PCM*CTH(1)
        UH=2*AMW**2-SH-THT
        UT=UH*THT-AMW**4
        PHIRL=4*(AE+BE)**2*TNTHW**2/SH/SH/PROPZ*
     ,        (UT*(PCM**2*SH+3*AMW**4)+4*AMW**2*PCM**2*SH*SH)
        PHILR=UT/SH/SH*(3.+2*(AE-BE)*TNTHW*(SH-6*AMW**2)*REDZ+
     ,      4*(AE-BE)**2*TNTHW**2*(PCM**2*SH+3*AMW**4)/PROPZ)+
     ,      8*(AE-BE)*TNTHW*AMW**2*REDZ+16*(AE-BE)**2*TNTHW**2*
     ,      AMW**2*PCM**2/PROPZ+2*(1.-2*(AE-BE)*TNTHW*AMW**2*REDZ)*
     ,      (UT/SH/THT-2*AMW**2/THT)+UT/THT**2
        SIGLR=4*E**4*PCM/64./PI/SH/RSH/SIN2W**2*PHILR
        SIGRL=4*E**4*PCM/64./PI/SH/RSH/SIN2W**2*PHIRL
        SIG=(FLEM*FREP*SIGLR+FREM*FLEP*SIGRL)*UNITS/2.
        IF (IBREM.AND..NOT.IBEAM) THEN
          SIG=SIG*ESTRUC(X1,QSQ)*ESTRUC(X2,QSQ)*JAC
        ELSE IF (IBEAM) THEN
          SIG=SIG*SSFEL(X1,0)*SSFEL(X2,0)*JAC
        END IF
        CALL SIGFIL(SIG,0,0,28,27)
400   CONTINUE
C
C          Higgs boson mechanisms
C          E+ E- --> Z H_SM; symmetric in cos(theta)
C
      IF(AMH.GT.0.AND.(AMZ+AMH).LT.RSH) THEN
        FACLR=E**2*G**2*(AE-BE)**2/COS2W
        FACRL=E**2*G**2*(AE+BE)**2/COS2W
        Z=CTH(1)
        PCM=SQRT(SSXLAM(SH,AMZ**2,AMH**2))/4./EE
        EZ0=SQRT(PCM**2+AMZ**2)
        FAC1=AMZ**2+EZ0**2-PCM**2*Z**2
        SIGLR=2*FACLR/32./PI/PROPZ/SQRT(SH)*PCM*FAC1
        SIGRL=2*FACRL/32./PI/PROPZ/SQRT(SH)*PCM*FAC1
        SIG=(FLEM*FREP*SIGLR+FREM*FLEP*SIGRL)*UNITS/2.
        IF (IBREM.AND..NOT.IBEAM) THEN
          SIG=SIG*ESTRUC(X1,QSQ)*ESTRUC(X2,QSQ)*JAC
        ELSE IF (IBEAM) THEN
          SIG=SIG*SSFEL(X1,0)*SSFEL(X2,0)*JAC
        END IF
        IF(GOQ(29,1).AND.GOQ(30,2)) CALL SIGFIL(SIG,0,0,29,30)
        IF(GOQ(30,1).AND.GOQ(29,2)) CALL SIGFIL(SIG,0,0,30,29)
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
