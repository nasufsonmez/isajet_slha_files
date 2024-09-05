      SUBROUTINE D800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U800/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(87)
      DWIDTH(5)=A(88)
      Q1(5)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(4)=A(137)
      DWIDTH(4)=A(111)
      Q1(4)=-A(134)**2-A(87)**2+A(137)**2-2*(-P4)
      DMASS(3)=A(151)
      DWIDTH(3)=A(16)
      Q1(3)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
      DMASS(2)=A(135)
      DWIDTH(2)=A(105)
      Q1(2)=-A(134)**2-A(84)**2+A(135)**2-2*(-P2)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
      Q1(1)=-A(84)**2-2*(-P2)
      DO 2 I=1,5
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

      SUBROUTINE D801
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U801/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
      DMASS(1)=A(135)
      DWIDTH(1)=A(105)
      Q1(1)=-A(134)**2-A(7)**2+A(135)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D802
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U802/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D803
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U803/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D804
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U804/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D805
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U805/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D806
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U806/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D807
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U807/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D808
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U808/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D809
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U809/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D80
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U80/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(132)
      DWIDTH(5)=A(102)
      Q1(5)=-A(93)**2+A(132)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(93)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(93)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(93)**2-A(84)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(84)**2-2*(-P4)
      DO 2 I=1,5
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

      SUBROUTINE D810
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U810/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D811
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U811/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D812
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U812/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D813
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U813/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D814
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U814/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D815
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U815/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D816
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U816/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D817
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U817/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D818
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U818/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D819
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U819/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D81
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U81/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(93)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(93)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(93)**2-A(7)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(7)**2-2*(-P4)
      DO 2 I=1,4
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

      SUBROUTINE D820
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U820/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D821
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U821/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D822
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U822/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D823
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U823/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D824
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U824/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D825
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U825/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D826
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U826/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D827
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U827/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D828
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U828/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D829
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U829/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D82
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U82/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(93)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
      Q1(1)=-A(93)**2-A(8)**2+A(134)**2-2*(-P4)
      DO 2 I=1,1
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
      DO 3 I=2,2
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D830
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U830/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D831
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U831/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D832
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U832/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D833
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U833/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D834
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U834/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D835
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U835/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(106)**2-A(9)**2+A(99)**2-2*(-P4)
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(106)**2-A(9)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(106)**2-A(9)**2+A(97)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(106)**2-A(9)**2+A(97)**2-2*(-P2)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(106)**2-A(9)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(106)**2-A(9)**2+A(95)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(106)**2-A(9)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(106)**2-A(9)**2+A(93)**2-2*(-P2)
      DO 2 I=1,8
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

      SUBROUTINE D836
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U836/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(106)
      DWIDTH(2)=A(107)
      Q1(2)=-2*(-P4)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D837
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U837/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(106)
      DWIDTH(2)=A(107)
      Q1(2)=-A(6)**2-2*(-P4)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D838
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U838/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(106)
      DWIDTH(2)=A(107)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D839
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U839/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(106)
      DWIDTH(2)=A(107)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D83
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U83/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(93)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(5)=A(134)
      DWIDTH(5)=A(104)
      Q1(5)=-A(93)**2-A(8)**2+A(134)**2-2*(-P4)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(93)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(93)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(93)**2-A(6)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(6)**2-2*(-P2)
      DO 2 I=1,5
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
      DO 3 I=6,6
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D840
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U840/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(108)
      DWIDTH(6)=A(109)
      Q1(6)=-A(106)**2-A(6)**2+A(108)**2-2*(-P4)
      DMASS(5)=A(108)
      DWIDTH(5)=A(109)
      Q1(5)=-A(106)**2-A(6)**2+A(108)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(106)
      DWIDTH(2)=A(107)
      Q1(2)=-A(6)**2-2*(-P4)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(6)**2-2*(-P2)
      DO 2 I=1,6
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

      SUBROUTINE D841
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U841/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(108)
      DWIDTH(5)=A(109)
      Q1(5)=-A(106)**2-A(82)**2+A(108)**2-2*(-P4)
      DMASS(4)=A(108)
      DWIDTH(4)=A(109)
      Q1(4)=-A(106)**2-A(6)**2+A(108)**2-2*(-P2)
      DMASS(3)=A(106)
      DWIDTH(3)=A(107)
      Q1(3)=-A(82)**2-2*(-P4)
      DMASS(2)=A(6)
      DWIDTH(2)=A(15)
      Q1(2)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(6)**2-2*(-P2)
      DO 2 I=1,5
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

      SUBROUTINE D842
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U842/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(108)
      DWIDTH(5)=A(109)
      Q1(5)=-A(106)**2-A(84)**2+A(108)**2-2*(-P4)
      DMASS(4)=A(108)
      DWIDTH(4)=A(109)
      Q1(4)=-A(106)**2-A(6)**2+A(108)**2-2*(-P2)
      DMASS(3)=A(106)
      DWIDTH(3)=A(107)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(6)
      DWIDTH(2)=A(15)
      Q1(2)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(6)**2-2*(-P2)
      DO 2 I=1,5
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

      SUBROUTINE D843
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U843/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(108)
      DWIDTH(4)=A(109)
      Q1(4)=-A(106)**2-A(7)**2+A(108)**2-2*(-P4)
      DMASS(3)=A(108)
      DWIDTH(3)=A(109)
      Q1(3)=-A(106)**2-A(6)**2+A(108)**2-2*(-P2)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DO 2 I=1,4
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

      SUBROUTINE D844
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U844/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(112)
      DWIDTH(4)=A(113)
      Q1(4)=-A(106)**2-A(151)**2+A(112)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(5)=-A(106)**2-A(106)**2-2*(+P1)
      DO 2 I=1,4
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
      DO 3 I=5,5
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D845
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U845/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(112)
      DWIDTH(3)=A(113)
      Q1(3)=-A(106)**2-A(87)**2+A(112)**2-2*(-P4)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DO 2 I=1,3
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

      SUBROUTINE D846
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U846/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(112)
      DWIDTH(3)=A(113)
      Q1(3)=-A(106)**2-A(151)**2+A(112)**2-2*(-P2)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DO 2 I=1,3
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

      SUBROUTINE D847
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U847/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      DO 2 I=1,1
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

      SUBROUTINE D848
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U848/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      DO 2 I=1,1
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

      SUBROUTINE D849
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U849/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(6)
      DWIDTH(3)=A(15)
      Q1(3)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(106)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(106)**2+A(89)**2-2*(-P2)
      DO 2 I=1,3
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

      SUBROUTINE D84
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U84/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(137)
      DWIDTH(3)=A(111)
      Q1(3)=-A(93)**2+A(137)**2-2*(-P4)
      Q1(4)=-A(93)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(93)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(93)**2-A(151)**2+A(89)**2-2*(-P2)
      DO 2 I=1,3
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
      DO 3 I=4,4
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D850
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U850/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(2)=-A(106)**2-A(106)**2-2*(+P1)
      DO 2 I=1,1
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
      DO 3 I=2,2
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D851
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U851/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(106)**2-A(106)**2-2*(+P1)
      DO 2 I=1,3
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
      DO 3 I=4,4
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D852
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U852/ Q0(7),Q1(8),Q2(8)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(84)
      DWIDTH(7)=A(85)
      Q1(7)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(6)=A(82)
      DWIDTH(6)=A(83)
      Q1(6)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(5)=A(6)
      DWIDTH(5)=A(15)
      Q1(5)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(8)=-A(106)**2-A(106)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(106)**2-A(9)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(106)**2-A(9)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(106)**2-A(9)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(106)**2-A(9)**2+A(93)**2-2*(-P2)
      DO 2 I=1,7
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
      DO 3 I=8,8
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D853
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U853/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(2)=-A(106)**2-A(106)**2-2*(+P1)
      DO 2 I=1,1
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
      DO 3 I=2,2
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D854
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U854/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(2)=-A(106)**2-A(106)**2-2*(+P1)
      DO 2 I=1,1
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
      DO 3 I=2,2
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D855
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U855/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(106)**2-A(106)**2-2*(+P1)
      DO 2 I=1,3
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
      DO 3 I=4,4
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D856
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U856/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(106)**2-A(106)**2-2*(+P1)
      DO 2 I=1,3
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
      DO 3 I=4,4
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D857
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U857/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(106)**2-A(106)**2-2*(+P1)
      DO 2 I=1,3
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
      DO 3 I=4,4
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D858
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U858/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(106)**2-A(106)**2-2*(+P1)
      DO 2 I=1,3
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
      DO 3 I=4,4
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D859
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U859/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(108)
      DWIDTH(6)=A(109)
      Q1(6)=-A(106)**2-A(82)**2+A(108)**2-2*(-P4)
      DMASS(5)=A(108)
      DWIDTH(5)=A(109)
      Q1(5)=-A(106)**2-A(82)**2+A(108)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(106)
      DWIDTH(2)=A(107)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(82)**2-2*(-P2)
      DO 2 I=1,6
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

      SUBROUTINE D85
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U85/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(137)
      DWIDTH(3)=A(111)
      Q1(3)=-A(93)**2+A(137)**2-2*(-P2)
      Q1(4)=-A(93)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(93)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(93)**2-A(87)**2+A(89)**2-2*(-P4)
      DO 2 I=1,3
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
      DO 3 I=4,4
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D860
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U860/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(108)
      DWIDTH(6)=A(109)
      Q1(6)=-A(106)**2-A(84)**2+A(108)**2-2*(-P4)
      DMASS(5)=A(108)
      DWIDTH(5)=A(109)
      Q1(5)=-A(106)**2-A(82)**2+A(108)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(106)
      DWIDTH(3)=A(107)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(82)**2-2*(-P2)
      DO 2 I=1,6
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

      SUBROUTINE D861
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U861/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(108)
      DWIDTH(3)=A(109)
      Q1(3)=-A(106)**2-A(7)**2+A(108)**2-2*(-P4)
      DMASS(2)=A(108)
      DWIDTH(2)=A(109)
      Q1(2)=-A(106)**2-A(82)**2+A(108)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      DO 2 I=1,3
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

      SUBROUTINE D862
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U862/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(108)
      DWIDTH(6)=A(109)
      Q1(6)=-A(106)**2-A(84)**2+A(108)**2-2*(-P2)
      DMASS(5)=A(106)
      DWIDTH(5)=A(107)
      Q1(5)=-A(84)**2-2*(-P2)
      DMASS(4)=A(108)
      DWIDTH(4)=A(109)
      Q1(4)=-A(106)**2-A(84)**2+A(108)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(106)
      DWIDTH(2)=A(107)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DO 2 I=1,6
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

      SUBROUTINE D863
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U863/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(108)
      DWIDTH(3)=A(109)
      Q1(3)=-A(106)**2-A(7)**2+A(108)**2-2*(-P4)
      DMASS(2)=A(108)
      DWIDTH(2)=A(109)
      Q1(2)=-A(106)**2-A(84)**2+A(108)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      DO 2 I=1,3
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

      SUBROUTINE D864
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U864/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(108)
      DWIDTH(4)=A(109)
      Q1(4)=-A(106)**2-A(7)**2+A(108)**2-2*(-P2)
      DMASS(3)=A(108)
      DWIDTH(3)=A(109)
      Q1(3)=-A(106)**2-A(7)**2+A(108)**2-2*(-P4)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DO 2 I=1,4
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

      SUBROUTINE D865
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U865/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(112)
      DWIDTH(4)=A(113)
      Q1(4)=-A(106)**2-A(87)**2+A(112)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(106)**2-A(106)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(106)**2-A(106)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(106)**2-A(106)**2+A(6)**2-2*(+P1)
      Q1(5)=-A(106)**2-A(106)**2-2*(+P1)
      DO 2 I=1,4
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
      DO 3 I=5,5
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D866
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U866/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(106)**2-A(9)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(106)**2-A(9)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(106)**2-A(9)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(106)**2-A(9)**2+A(93)**2-2*(-P4)
      DO 2 I=1,4
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

      SUBROUTINE D867
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U867/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(106)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(106)**2+A(89)**2-2*(-P4)
      DO 2 I=1,2
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

      SUBROUTINE D868
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U868/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(106)**2-A(9)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(106)**2-A(9)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(106)**2-A(9)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(106)**2-A(9)**2+A(93)**2-2*(-P4)
      DO 2 I=1,4
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

      SUBROUTINE D869
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U869/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(106)**2-A(9)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(106)**2-A(9)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(106)**2-A(9)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(106)**2-A(9)**2+A(93)**2-2*(-P4)
      DO 2 I=1,4
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

      SUBROUTINE D86
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U86/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(135)
      DWIDTH(6)=A(105)
      Q1(6)=-A(93)**2-A(8)**2+A(135)**2-2*(-P2)
      Q1(7)=-A(93)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(5)=A(134)
      DWIDTH(5)=A(104)
      Q1(5)=-A(93)**2-A(8)**2+A(134)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(93)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(93)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(93)**2-A(82)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(82)**2-2*(-P4)
      DO 2 I=1,6
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
      DO 3 I=7,7
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D870
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U870/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(106)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(106)**2+A(89)**2-2*(-P4)
      DO 2 I=1,2
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

      SUBROUTINE D871
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U871/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(106)**2-A(9)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(106)**2-A(9)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(106)**2-A(9)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(106)**2-A(9)**2+A(93)**2-2*(-P4)
      DO 2 I=1,4
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

      SUBROUTINE D872
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U872/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(99)
      DWIDTH(6)=A(100)
      Q1(6)=-A(106)**2-A(9)**2+A(99)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(106)**2-A(9)**2+A(97)**2-2*(-P4)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(106)**2-A(9)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(106)**2-A(9)**2+A(93)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(106)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(106)**2+A(89)**2-2*(-P2)
      DO 2 I=1,6
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

      SUBROUTINE D873
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U873/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D874
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U874/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D875
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U875/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(151)
      DWIDTH(4)=A(16)
      Q1(4)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DMASS(3)=A(112)
      DWIDTH(3)=A(113)
      Q1(3)=-A(106)**2-A(151)**2+A(112)**2-2*(-P4)
      DMASS(2)=A(108)
      DWIDTH(2)=A(109)
      Q1(2)=-A(106)**2-A(6)**2+A(108)**2-2*(-P2)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(6)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D876
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U876/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(87)
      DWIDTH(4)=A(88)
      Q1(4)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(3)=A(112)
      DWIDTH(3)=A(113)
      Q1(3)=-A(106)**2-A(87)**2+A(112)**2-2*(-P4)
      DMASS(2)=A(108)
      DWIDTH(2)=A(109)
      Q1(2)=-A(106)**2-A(6)**2+A(108)**2-2*(-P2)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(6)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D877
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U877/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(87)
      DWIDTH(5)=A(88)
      Q1(5)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(4)=A(151)
      DWIDTH(4)=A(16)
      Q1(4)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DMASS(3)=A(112)
      DWIDTH(3)=A(113)
      Q1(3)=-A(106)**2-A(151)**2+A(112)**2-2*(-P2)
      DMASS(2)=A(108)
      DWIDTH(2)=A(109)
      Q1(2)=-A(106)**2-A(82)**2+A(108)**2-2*(-P4)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(82)**2-2*(-P4)
      DO 2 I=1,5
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

      SUBROUTINE D878
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U878/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(87)
      DWIDTH(5)=A(88)
      Q1(5)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(4)=A(151)
      DWIDTH(4)=A(16)
      Q1(4)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DMASS(3)=A(112)
      DWIDTH(3)=A(113)
      Q1(3)=-A(106)**2-A(151)**2+A(112)**2-2*(-P2)
      DMASS(2)=A(108)
      DWIDTH(2)=A(109)
      Q1(2)=-A(106)**2-A(84)**2+A(108)**2-2*(-P4)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(84)**2-2*(-P4)
      DO 2 I=1,5
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

      SUBROUTINE D879
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U879/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(108)
      DWIDTH(1)=A(109)
      Q1(1)=-A(106)**2-A(7)**2+A(108)**2-2*(-P4)
      DO 2 I=1,2
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

      SUBROUTINE D87
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U87/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(135)
      DWIDTH(6)=A(105)
      Q1(6)=-A(93)**2-A(8)**2+A(135)**2-2*(-P2)
      Q1(7)=-A(93)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(5)=A(134)
      DWIDTH(5)=A(104)
      Q1(5)=-A(93)**2-A(8)**2+A(134)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(93)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(93)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(93)**2-A(84)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(84)**2-2*(-P4)
      DO 2 I=1,6
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
      DO 3 I=7,7
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D880
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U880/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,1
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

      SUBROUTINE D881
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U881/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,2
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

      SUBROUTINE D882
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U882/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(87)
      DWIDTH(6)=A(88)
      Q1(6)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(5)=A(151)
      DWIDTH(5)=A(16)
      Q1(5)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(106)**2-A(9)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(106)**2-A(9)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(106)**2-A(9)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(106)**2-A(9)**2+A(93)**2-2*(-P4)
      DO 2 I=1,6
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

      SUBROUTINE D883
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U883/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,1
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

      SUBROUTINE D884
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U884/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,2
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

      SUBROUTINE D885
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U885/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,2
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

      SUBROUTINE D886
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U886/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,2
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

      SUBROUTINE D887
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U887/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,2
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

      SUBROUTINE D888
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U888/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,2
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

      SUBROUTINE D889
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U889/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,2
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

      SUBROUTINE D88
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U88/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(135)
      DWIDTH(5)=A(105)
      Q1(5)=-A(93)**2-A(8)**2+A(135)**2-2*(-P2)
      Q1(6)=-A(93)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(93)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(93)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(93)**2-A(7)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(7)**2-2*(-P4)
      DO 2 I=1,5
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
      DO 3 I=6,6
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D890
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U890/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,2
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

      SUBROUTINE D891
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U891/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DO 2 I=1,2
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

      SUBROUTINE D892
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U892/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(87)
      DWIDTH(5)=A(88)
      Q1(5)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(4)=A(112)
      DWIDTH(4)=A(113)
      Q1(4)=-A(106)**2-A(87)**2+A(112)**2-2*(-P4)
      DMASS(3)=A(151)
      DWIDTH(3)=A(16)
      Q1(3)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DMASS(2)=A(108)
      DWIDTH(2)=A(109)
      Q1(2)=-A(106)**2-A(82)**2+A(108)**2-2*(-P2)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(82)**2-2*(-P2)
      DO 2 I=1,5
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

      SUBROUTINE D893
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U893/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(87)
      DWIDTH(5)=A(88)
      Q1(5)=-A(106)**2-A(112)**2+A(87)**2-2*(+P1)
      DMASS(4)=A(112)
      DWIDTH(4)=A(113)
      Q1(4)=-A(106)**2-A(87)**2+A(112)**2-2*(-P4)
      DMASS(3)=A(151)
      DWIDTH(3)=A(16)
      Q1(3)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DMASS(2)=A(108)
      DWIDTH(2)=A(109)
      Q1(2)=-A(106)**2-A(84)**2+A(108)**2-2*(-P2)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(84)**2-2*(-P2)
      DO 2 I=1,5
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

      SUBROUTINE D894
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U894/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(106)**2-A(112)**2+A(151)**2-2*(+P1)
      DMASS(1)=A(108)
      DWIDTH(1)=A(109)
      Q1(1)=-A(106)**2-A(7)**2+A(108)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D895
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U895/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(106)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(106)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D896
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U896/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(106)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(106)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D897
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U897/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(106)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(106)**2+A(89)**2-2*(-P2)
      DO 2 I=1,2
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

      SUBROUTINE D898
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U898/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(106)**2-A(9)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(106)**2-A(9)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(106)**2-A(9)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(106)**2-A(9)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D899
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U899/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(106)**2-A(9)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(106)**2-A(9)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(106)**2-A(9)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(106)**2-A(9)**2+A(93)**2-2*(-P2)
      DO 2 I=1,4
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

      SUBROUTINE D89
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U89/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(93)**2-A(106)**2+A(9)**2-2*(+P1)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(93)**2-A(9)**2+A(106)**2-2*(-P4)
      DO 2 I=1,1
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
      DO 3 I=2,2
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D8
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U8/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(136)
      DWIDTH(3)=A(110)
      Q1(3)=-A(93)**2+A(136)**2-2*(-P2)
      DMASS(2)=A(136)
      DWIDTH(2)=A(110)
      Q1(2)=-A(93)**2+A(136)**2-2*(-P4)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(93)**2-A(93)**2+A(6)**2-2*(+P1)
      DO 2 I=1,3
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

