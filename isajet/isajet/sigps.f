#include "PILOT.inc"
        REAL FUNCTION SIGPS(P2,I,J,G1,G2,CHW)
C-----------------------------------------------------------------------
C          SIGPS: Chargino mass matrix correction
C     Taken from Damien M. Pierce, Jonathan A. Bagger, Konstantin T. Matchev,
C     Ren-jie Zhang, Nucl.Phys.B491:3-67,1997, hep-ph/9606211
C     Programmed by Tadas Krupovnickas
C          P2 = 4-momentum squared
C          CHW = Cos(theta_W) in DR bar scheme
C     Ordering: u=1,s=2,t=3,d=4,c=5,b=6,e=7,mu=8,tau=9,nue=10,num=11,nut=12
C     I and J are indexes in Pierce's base. To convert to ISAJET's basis
C     transform the matrix elements in the following way:
C     11->11, 12->-21, 21->-12, 22->22
C-----------------------------------------------------------------------
#ifdef IMPNONE_X
      IMPLICIT NONE
#endif
#include "sssm.inc"
#include "sspar.inc"
#include "sugmg.inc"
#include "ssinf.inc"
      COMPLEX*16 SSB0,AC0PPW(4,2),BC0PPW(4,2),AC0PPH(4,2,2)
     $,BC0PPH(4,2,2)
      DOUBLE PRECISION TEMP,BPPFF(2,12,2),BPPCPZ(2,2)
     $,APPFF(2,12,2),APPCPZ(2,2)
      DOUBLE PRECISION BPPCPG(2,2),BPPCPH(2,2,4)
     $,APPCPG(2,2),APPCPH(2,2,4)
      REAL APPR(2,12),APPL(2,12),AP0PPW(4,2),APPPPZ(2,2),APPPPH(2,2,4)
     $,BPPR(2,12),BPPL(2,12),BP0PPW(4,2),BPPPPZ(2,2),BPPPPH(2,2,4)
     $,AP0PPH(4,2,2),BP0PPH(4,2,2)
      REAL P2,G1,G2,CHW,COST,SINT,COSB,SINB,COSL,SINL,GG,GGP
      REAL THX,THY,VMAT(2,2),UMAT(2,2),BE,FB,FT,FMIX(12,2,2)
      INTEGER THIW1,THIW2
      INTEGER I,J,II,III
      COMPLEX IMAG,N(4,4)
      PARAMETER (IMAG=(0.,1.))
      REAL PI,SR2
      PI=4*ATAN(1.)
      SR2=SQRT(2.)
      COST=COS(THETAT)
      SINT=SIN(THETAT)
      COSB=COS(THETAB)
      SINB=SIN(THETAB)
      COSL=COS(THETAL)
      SINL=SIN(THETAL)
      GG=G2
      GGP=SQRT(3./5.)*G1
      BE=ATAN(VUQ/VDQ)
      FB=MBQ/VDQ
      FT=MTQ/VUQ
      THX=SIGN(1.,TAN(GAMMAL))
      THY=SIGN(1.,TAN(GAMMAR))
      IF (SIGN(1.,AMW1SS).EQ.1.) THEN
         THIW1=0
      ELSE
         THIW1=1
      END IF
      IF (SIGN(1.,AMW2SS).EQ.1.) THEN
         THIW2=0
      ELSE
         THIW2=1
      END IF
      UMAT(1,1)=-(-1)**THIW1*SIN(GAMMAR)
      UMAT(1,2)=(-1)**THIW1*COS(GAMMAR)
      UMAT(2,1)=-(-1)**THIW2*THY*COS(GAMMAR)
      UMAT(2,2)=-(-1)**THIW2*THY*SIN(GAMMAR)
      VMAT(1,1)=-SIN(GAMMAL)
      VMAT(1,2)=COS(GAMMAL)
      VMAT(2,1)=-THX*COS(GAMMAL)
      VMAT(2,2)=-THX*SIN(GAMMAL)
      DO II=1,4
        IF (SIGN(1.,AMZISS(II)).EQ.1.) THEN
          III=0
        ELSE
          III=1
        END IF
        N(II,1)=-IMAG**III*ZMIXSS(4,II)
        N(II,2)=-IMAG**III*ZMIXSS(3,II)
        N(II,3)=IMAG**III*ZMIXSS(2,II)
        N(II,4)=IMAG**III*ZMIXSS(1,II)
      ENDDO
      DO II=1,2
        DO III=1,12
          APPR(II,III)=0.
          APPL(II,III)=0.
          BPPR(II,III)=0.
          BPPL(II,III)=0.
        ENDDO
      ENDDO
      APPL(1,4)=GG
      APPL(1,5)=GG
      APPL(1,6)=GG
      APPL(1,7)=GG
      APPL(1,8)=GG
      APPL(1,9)=GG
      BPPL(1,1)=GG
      BPPL(1,2)=GG
      BPPL(1,3)=GG
      BPPL(1,10)=GG
      BPPL(1,11)=GG
      BPPL(1,12)=GG
      BPPL(2,6)=-FB
      BPPR(2,3)=-FB
      APPL(2,3)=-FT
      APPR(2,6)=-FT
      DO II=1,12
        FMIX(II,1,1)=1.
        FMIX(II,1,2)=0.
        FMIX(II,2,1)=0.
        FMIX(II,2,2)=1.
      ENDDO
      FMIX(3,1,1)=COST
      FMIX(3,1,2)=-SINT
      FMIX(3,2,1)=SINT
      FMIX(3,2,2)=COST
      FMIX(6,1,1)=COSB
      FMIX(6,1,2)=-SINB
      FMIX(6,2,1)=SINB
      FMIX(6,2,2)=COSB
      FMIX(9,1,1)=COSL
      FMIX(9,1,2)=-SINL
      FMIX(9,2,1)=SINL
      FMIX(9,2,2)=COSL
      DO II=1,2
        DO III=1,12
          APPFF(II,III,1)=FMIX(III,1,1)*APPL(II,III)
     $+FMIX(III,1,2)*APPR(II,III)
          APPFF(II,III,2)=FMIX(III,2,1)*APPL(II,III)
     $+FMIX(III,2,2)*APPR(II,III)
          BPPFF(II,III,1)=FMIX(III,1,1)*BPPL(II,III)
     $+FMIX(III,1,2)*BPPR(II,III)
          BPPFF(II,III,2)=FMIX(III,2,1)*BPPL(II,III)
     $+FMIX(III,2,2)*BPPR(II,III)
        ENDDO
      ENDDO
      DO II=1,4
        DO III=1,2
          AP0PPW(II,III)=0.
          BP0PPW(II,III)=0.
        ENDDO
      ENDDO
      AP0PPW(2,1)=-GG
      AP0PPW(4,2)=GG/SR2
      BP0PPW(2,1)=-GG
      BP0PPW(3,2)=-GG/SR2
      DO II=1,4
        DO III=1,2
          AC0PPW(II,III)=CONJG(N(II,1))*AP0PPW(1,III)
     $+CONJG(N(II,2))*AP0PPW(2,III)+CONJG(N(II,3))*AP0PPW(3,III)
     $+CONJG(N(II,4))*AP0PPW(4,III)
          BC0PPW(II,III)=N(II,1)*BP0PPW(1,III)
     $+N(II,2)*BP0PPW(2,III)+N(II,3)*BP0PPW(3,III)
     $+N(II,4)*BP0PPW(4,III)
        ENDDO
      ENDDO
      DO II=1,2
        DO III=1,2
          APPPPZ(II,III)=0.
          BPPPPZ(II,III)=0.
        ENDDO
      ENDDO
      APPPPZ(1,1)=GG*CHW
      APPPPZ(2,2)=GG*(2.*CHW**2-1.)/2./CHW
      BPPPPZ(1,1)=GG*CHW
      BPPPPZ(2,2)=GG*(2.*CHW**2-1.)/2./CHW
      DO II=1,2
        DO III=1,2
          APPCPZ(II,III)=VMAT(II,1)*APPPPZ(1,III)
     $+VMAT(II,2)*APPPPZ(2,III)
          BPPCPZ(II,III)=UMAT(II,1)*BPPPPZ(1,III)
     $+UMAT(II,2)*BPPPPZ(2,III)
        ENDDO
      ENDDO
      DO II=1,2
        DO III=1,2
          APPCPG(II,III)=SQRT(4.*PI/137.036)*VMAT(III,II)
          BPPCPG(II,III)=SQRT(4.*PI/137.036)*UMAT(III,II)
        ENDDO
      ENDDO
      DO II=1,4
        DO III=1,2
          AP0PPH(II,III,1)=0.
          AP0PPH(II,III,2)=0.
          BP0PPH(II,III,1)=0.
          BP0PPH(II,III,2)=0.
        ENDDO
      ENDDO
      AP0PPH(1,2,1)=GGP/SR2
      AP0PPH(2,2,1)=GG/SR2
      AP0PPH(3,1,1)=-GG
      BP0PPH(1,2,2)=GGP/SR2
      BP0PPH(2,2,2)=GG/SR2
      BP0PPH(4,1,2)=GG
      DO II=1,4
        DO III=1,2
          AC0PPH(II,III,1)=COS(BE)*(CONJG(N(II,1))*AP0PPH(1,III,1)
     $+CONJG(N(II,2))*AP0PPH(2,III,1)+CONJG(N(II,3))*AP0PPH(3,III,1)
     $+CONJG(N(II,4))*AP0PPH(4,III,1))+SIN(BE)
     $*(CONJG(N(II,1))*AP0PPH(1,III,2)
     $+CONJG(N(II,2))*AP0PPH(2,III,2)+CONJG(N(II,3))*AP0PPH(3,III,2)
     $+CONJG(N(II,4))*AP0PPH(4,III,2))
          AC0PPH(II,III,2)=-SIN(BE)*(CONJG(N(II,1))*AP0PPH(1,III,1)
     $+CONJG(N(II,2))*AP0PPH(2,III,1)+CONJG(N(II,3))*AP0PPH(3,III,1)
     $+CONJG(N(II,4))*AP0PPH(4,III,1))+COS(BE)
     $*(CONJG(N(II,1))*AP0PPH(1,III,2)
     $+CONJG(N(II,2))*AP0PPH(2,III,2)+CONJG(N(II,3))*AP0PPH(3,III,2)
     $+CONJG(N(II,4))*AP0PPH(4,III,2))
          BC0PPH(II,III,1)=COS(BE)*(N(II,1)*BP0PPH(1,III,1)
     $+N(II,2)*BP0PPH(2,III,1)+N(II,3)*BP0PPH(3,III,1)
     $+N(II,4)*BP0PPH(4,III,1))+SIN(BE)*(N(II,1)*BP0PPH(1,III,2)
     $+N(II,2)*BP0PPH(2,III,2)+N(II,3)*BP0PPH(3,III,2)
     $+N(II,4)*BP0PPH(4,III,2))
          BC0PPH(II,III,2)=-SIN(BE)*(N(II,1)*BP0PPH(1,III,1)
     $+N(II,2)*BP0PPH(2,III,1)+N(II,3)*BP0PPH(3,III,1)
     $+N(II,4)*BP0PPH(4,III,1))+COS(BE)*(N(II,1)*BP0PPH(1,III,2)
     $+N(II,2)*BP0PPH(2,III,2)+N(II,3)*BP0PPH(3,III,2)
     $+N(II,4)*BP0PPH(4,III,2))
        ENDDO
      ENDDO
      DO II=1,2
        DO III=1,2
          APPPPH(II,III,1)=0.
          APPPPH(II,III,2)=0.
          APPPPH(II,III,3)=0.
          APPPPH(II,III,4)=0.
          BPPPPH(II,III,1)=0.
          BPPPPH(II,III,2)=0.
          BPPPPH(II,III,3)=0.
          BPPPPH(II,III,4)=0.
        ENDDO
      ENDDO
      APPPPH(1,2,1)=GG/SR2
      APPPPH(2,1,2)=GG/SR2
      APPPPH(1,2,3)=GG/SR2
      APPPPH(2,1,4)=-GG/SR2
      BPPPPH(2,1,1)=GG/SR2
      BPPPPH(1,2,2)=GG/SR2
      BPPPPH(2,1,3)=-GG/SR2
      BPPPPH(1,2,4)=GG/SR2
      DO II=1,2
        DO III=1,2
          APPCPH(II,III,1)=(UMAT(III,1)*APPPPH(II,1,1)
     $+UMAT(III,2)*APPPPH(II,2,1))*COS(ALFAH)
     $-(UMAT(III,1)*APPPPH(II,1,2)+UMAT(III,2)*APPPPH(II,2,2))
     $*SIN(ALFAH)
          APPCPH(II,III,2)=(UMAT(III,1)*APPPPH(II,1,1)
     $+UMAT(III,2)*APPPPH(II,2,1))*SIN(ALFAH)
     $+(UMAT(III,1)*APPPPH(II,1,2)+UMAT(III,2)*APPPPH(II,2,2))
     $*COS(ALFAH)
          APPCPH(II,III,3)=(UMAT(III,1)*APPPPH(II,1,3)
     $+UMAT(III,2)*APPPPH(II,2,3))*COS(BE)
     $+(UMAT(III,1)*APPPPH(II,1,4)+UMAT(III,2)*APPPPH(II,2,4))*SIN(BE)
          APPCPH(II,III,4)=-(UMAT(III,1)*APPPPH(II,1,3)
     $+UMAT(III,2)*APPPPH(II,2,3))*SIN(BE)
     $+(UMAT(III,1)*APPPPH(II,1,4)+UMAT(III,2)*APPPPH(II,2,4))*COS(BE)
          BPPCPH(II,III,1)=(VMAT(III,1)*BPPPPH(II,1,1)
     $+VMAT(III,2)*BPPPPH(II,2,1))*COS(ALFAH)
     $-(VMAT(III,1)*BPPPPH(II,1,2)+VMAT(III,2)*BPPPPH(II,2,2))
     $*SIN(ALFAH)
          BPPCPH(II,III,2)=(VMAT(III,1)*BPPPPH(II,1,1)
     $+VMAT(III,2)*BPPPPH(II,2,1))*SIN(ALFAH)
     $+(VMAT(III,1)*BPPPPH(II,1,2)+VMAT(III,2)*BPPPPH(II,2,2))
     $*COS(ALFAH)
          BPPCPH(II,III,3)=(VMAT(III,1)*BPPPPH(II,1,3)
     $+VMAT(III,2)*BPPPPH(II,2,3))*COS(BE)
     $+(VMAT(III,1)*BPPPPH(II,1,4)+VMAT(III,2)*BPPPPH(II,2,4))*SIN(BE)
          BPPCPH(II,III,4)=-(VMAT(III,1)*BPPPPH(II,1,3)
     $+VMAT(III,2)*BPPPPH(II,2,3))*SIN(BE)
     $+(VMAT(III,1)*BPPPPH(II,1,4)+VMAT(III,2)*BPPPPH(II,2,4))*COS(BE)
        ENDDO
      ENDDO
      TEMP=DBLE(3.*(BPPFF(I,1,1)*APPFF(J,1,1)*AMUP
     $*SSB0(P2,AMUP,ABS(MSS(4)))
     $+BPPFF(I,1,2)*APPFF(J,1,2)*AMUP*SSB0(P2,AMUP,ABS(MSS(5)))
     $+BPPFF(I,2,1)*APPFF(J,2,1)*AMST*SSB0(P2,AMST,ABS(MSS(8)))
     $+BPPFF(I,2,2)*APPFF(J,2,2)*AMST*SSB0(P2,AMST,ABS(MSS(9)))
     $+BPPFF(I,3,1)*APPFF(J,3,1)*MTQ*SSB0(P2,AMTP,ABS(MSS(10)))
     $+BPPFF(I,3,2)*APPFF(J,3,2)*MTQ*SSB0(P2,AMTP,ABS(MSS(11)))
     $+BPPFF(I,4,1)*APPFF(J,4,1)*AMDN*SSB0(P2,AMDN,ABS(MSS(2)))
     $+BPPFF(I,4,2)*APPFF(J,4,2)*AMDN*SSB0(P2,AMDN,ABS(MSS(3)))
     $+BPPFF(I,5,1)*APPFF(J,5,1)*AMCH*SSB0(P2,AMCH,ABS(MSS(6)))
     $+BPPFF(I,5,2)*APPFF(J,5,2)*AMCH*SSB0(P2,AMCH,ABS(MSS(7)))
     $+BPPFF(I,6,1)*APPFF(J,6,1)*MBQ*SSB0(P2,AMBT,ABS(MSS(13)))
     $+BPPFF(I,6,2)*APPFF(J,6,2)*MBQ*SSB0(P2,AMBT,ABS(MSS(12))))
     $+BPPFF(I,7,1)*APPFF(J,7,1)*AME*SSB0(P2,AME,ABS(MSS(14)))
     $+BPPFF(I,8,1)*APPFF(J,8,1)*AMMU*SSB0(P2,AMMU,ABS(MSS(15)))
     $+BPPFF(I,9,1)*APPFF(J,9,1)*MLQ*SSB0(P2,AMTAU,ABS(MSS(16)))
     $-4.*(CONJG(BC0PPW(1,I))*AC0PPW(1,J)*ABS(MSS(23))
     $*SSB0(P2,ABS(MSS(23)),AMW)
     $+CONJG(BC0PPW(2,I))*AC0PPW(2,J)*ABS(MSS(24))
     $*SSB0(P2,ABS(MSS(24)),AMW)
     $+CONJG(BC0PPW(3,I))*AC0PPW(3,J)*ABS(MSS(25))
     $*SSB0(P2,ABS(MSS(25)),AMW)
     $+CONJG(BC0PPW(4,I))*AC0PPW(4,J)*ABS(MSS(26))
     $*SSB0(P2,ABS(MSS(26)),AMW)
     $+BPPCPZ(I,1)*APPCPZ(J,1)*ABS(MSS(27))*SSB0(P2,ABS(MSS(27)),AMZ)
     $+BPPCPZ(I,2)*APPCPZ(J,2)*ABS(MSS(28))*SSB0(P2,ABS(MSS(28)),AMZ)
     $+BPPCPG(I,1)*APPCPG(J,1)*ABS(MSS(27))*SSB0(P2,ABS(MSS(27)),0.)
     $+BPPCPG(I,2)*APPCPG(J,2)*ABS(MSS(28))*SSB0(P2,ABS(MSS(28)),0.))
     $+CONJG(BC0PPH(1,I,1))*AC0PPH(1,J,1)*ABS(MSS(23))
     $*SSB0(P2,ABS(MSS(23)),AMW)
     $+CONJG(BC0PPH(1,I,2))*AC0PPH(1,J,2)*ABS(MSS(23))
     $*SSB0(P2,ABS(MSS(23)),ABS(MSS(32)))
     $+CONJG(BC0PPH(2,I,1))*AC0PPH(2,J,1)*ABS(MSS(24))
     $*SSB0(P2,ABS(MSS(24)),AMW)
     $+CONJG(BC0PPH(2,I,2))*AC0PPH(2,J,2)*ABS(MSS(24))
     $*SSB0(P2,ABS(MSS(24)),ABS(MSS(32)))
     $+CONJG(BC0PPH(3,I,1))*AC0PPH(3,J,1)*ABS(MSS(25))
     $*SSB0(P2,ABS(MSS(25)),AMW)
     $+CONJG(BC0PPH(3,I,2))*AC0PPH(3,J,2)*ABS(MSS(25))
     $*SSB0(P2,ABS(MSS(25)),ABS(MSS(32)))
     $+CONJG(BC0PPH(4,I,1))*AC0PPH(4,J,1)*ABS(MSS(26))
     $*SSB0(P2,ABS(MSS(26)),AMW)
     $+CONJG(BC0PPH(4,I,2))*AC0PPH(4,J,2)*ABS(MSS(26))
     $*SSB0(P2,ABS(MSS(26)),ABS(MSS(32)))
     $+BPPCPH(I,1,1)*APPCPH(J,1,1)*ABS(MSS(27))
     $*SSB0(P2,ABS(MSS(27)),ABS(MSS(30)))
     $+BPPCPH(I,1,2)*APPCPH(J,1,2)*ABS(MSS(27))
     $*SSB0(P2,ABS(MSS(27)),ABS(MSS(29)))
     $+BPPCPH(I,1,3)*APPCPH(J,1,3)*ABS(MSS(27))
     $*SSB0(P2,ABS(MSS(27)),AMZ)
     $+BPPCPH(I,1,4)*APPCPH(J,1,4)*ABS(MSS(27))
     $*SSB0(P2,ABS(MSS(27)),ABS(MSS(31)))
     $+BPPCPH(I,2,1)*APPCPH(J,2,1)*ABS(MSS(28))
     $*SSB0(P2,ABS(MSS(28)),ABS(MSS(30)))
     $+BPPCPH(I,2,2)*APPCPH(J,2,2)*ABS(MSS(28))
     $*SSB0(P2,ABS(MSS(28)),ABS(MSS(29)))
     $+BPPCPH(I,2,3)*APPCPH(J,2,3)*ABS(MSS(28))
     $*SSB0(P2,ABS(MSS(28)),AMZ)
     $+BPPCPH(I,2,4)*APPCPH(J,2,4)*ABS(MSS(28))
     $*SSB0(P2,ABS(MSS(28)),ABS(MSS(31)))
     $)/16./PI**2
      SIGPS=TEMP
      RETURN
      END
