      SUBROUTINE D1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(6)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(93)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(93)**2-A(6)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(93)**2-A(6)**2+A(95)**2-2*(-P4)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(93)**2-A(6)**2+A(95)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(93)**2-A(93)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(93)**2-A(93)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(6)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(6)**2-2*(-P2)
      DO 2 I=1,10
      IF (RWIDTH) THEN
        Q2(I)=1/(Q1(I)**2+((DMASS(I)-Q1(I)/DMASS(I))*DWIDTH(I))**2)
      ELSE
        Q2(I)=1/(Q1(I)**2+(DMASS(I)*DWIDTH(I))**2)
      ENDIF
      IF (GWIDTH) THEN
        Q0(I)=Q2(I)*Q1(I)**2
      ELSE
        Q0(I)=1 
      ENDIF
        Q1(I)=Q2(I)*Q1(I)
2     CONTINUE
      RETURN
      END

