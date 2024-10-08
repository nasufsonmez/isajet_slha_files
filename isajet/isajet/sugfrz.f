#include "PILOT.inc"
C------------------------------------------------------------------
      SUBROUTINE SUGFRZ(Q,G,G0,IG)
C------------------------------------------------------------------
C
C     Freeze out final soft breaking parameters
C
#ifdef IMPNONE_X
      IMPLICIT NONE
#endif
#include "sslun.inc"
#include "sugpas.inc"
#include "sspar.inc"
C     Common blocks for A. Box RGE code
      COMMON/RGEMS/VEVMH,RGEMS,RGEMU
      DOUBLE COMPLEX VEVMH
      DOUBLE PRECISION RGEMS,RGEMU
      SAVE/RGEMS/
C
      INTEGER IG(31)
      REAL Q,MT,PI,TMZ1
      REAL*8 G(31),DPI
      REAL G0(31)
      INTEGER I
C
      MT=AMT
      PI=4.*ATAN(1.)
      DPI=4.D0*DATAN(1.D0)
      TMZ1=2*ABS(AMZ1SS)
      DO 200 I=1,3
        G0(I)=SNGL(G(I))
200   CONTINUE
C          Freeze out Yukawa couplings and A-terms at HIGFRZ
      DO 205 I=4,6
        IF (Q.LT.HIGFRZ.AND.IG(I).EQ.0) THEN
          G0(I)=SNGL(G(I))
          IG(I)=1
          G0(I+6)=SNGL(G(I+6))
          IG(I+6)=1
C         The following line needed to interface with A. Box RGE code
          RGEMS=DBLE(Q)
C
        ELSE IF (IG(I).EQ.0) THEN
          G0(I)=SNGL(G(I))
          G0(I+6)=SNGL(G(I+6))
        END IF
205   CONTINUE
C         Extract b Yukawa at mA for use in Higgs decay rates
      IF (Q.GT.AMHA) THEN
        FBMA=SNGL(G(5))
      END IF
      IF (Q.GT.TMZ1) THEN
        FT2Z1=SNGL(G(6))
        FB2Z1=SNGL(G(5))
        FL2Z1=SNGL(G(4))
      END IF
C         Extract vu, vd at mt for mt calculation...
      IF (Q.GT.AMT) THEN
        VUMT=SNGL(G(30))
        VDMT=SNGL(G(31))
        ASMTP=SNGL(G(3)**2/4.D0/DPI)
      END IF
      IF (Q.GT.HIGFRZ) THEN
        ASMSS=SNGL(G(3)**2/4.D0/DPI)
        M3Q=SNGL(G(9))
      END IF
C          Freeze out running gluino mass at MGL
        IF (Q.LT.ABS(SNGL(G(9))).AND.IG(9).EQ.0) THEN
          G0(9)=SNGL(G(9))
          IG(9)=1
            ASM3=SNGL(G(3)**2/4.D0/DPI)
        ELSE IF (IG(9).EQ.0) THEN
          G0(9)=SNGL(G(9))
        END IF
C          Freeze out Higgs paremeters at HIGFRZ
      DO 211 I=13,14
        IF (Q.LT.HIGFRZ.AND.IG(I).EQ.0) THEN
          G0(I)=SNGL(G(I))
          IG(I)=1
          G0(I-6)=SNGL(G(I-6))
          IG(I-6)=1
          G0(I+12)=SNGL(G(I+12))
          IG(I+12)=1
          G0(I+17)=SNGL(G(I+17))
          IG(I+17)=1
        ELSE IF (IG(I).EQ.0) THEN
          G0(I)=SNGL(G(I))
          G0(I-6)=SNGL(G(I-6))
          G0(I+12)=SNGL(G(I+12))
          G0(I+17)=SNGL(G(I+17))
        END IF
211   CONTINUE
C          Freeze out 1st/2nd gen. soft terms at own masses
      DO 220 I=15,19
        IF (Q.LT.SQRT(ABS(SNGL(G(I)))).AND.IG(I).EQ.0) THEN
          G0(I)=SNGL(G(I))
          IG(I)=1
        ELSE IF (IG(I).EQ.0) THEN
          G0(I)=SNGL(G(I))
        END IF
220   CONTINUE
C          Freeze out third gen. soft terms at HIGFRZ
      DO 221 I=20,24
        IF (Q.LT.HIGFRZ.AND.IG(I).EQ.0) THEN
          G0(I)=SNGL(G(I))
          IG(I)=1
        ELSE IF (IG(I).EQ.0) THEN
          G0(I)=SNGL(G(I))
        END IF
221   CONTINUE
C          Freeze our N_R parameters at Majorana mass scale
      DO 230 I=27,29
        IF (G(I).NE.0.D0) G0(I)=SNGL(G(I))
        IF (Q.LT.AMNRMJ.AND.IG(I).EQ.0.) THEN
          IG(I)=1
        END IF
230   CONTINUE
100   RETURN
      END
