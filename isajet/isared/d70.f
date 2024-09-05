      SUBROUTINE D700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U700/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(132)**2-A(136)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(132)**2-A(136)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D701
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U701/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(132)**2-A(136)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(132)**2-A(136)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D702
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U702/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(132)**2-A(136)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(132)**2-A(136)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D703
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U703/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(87)
      DWIDTH(4)=A(88)
      Q1(4)=-A(132)**2-A(136)**2+A(87)**2-2*(+P1)
      DMASS(3)=A(136)
      DWIDTH(3)=A(110)
      Q1(3)=-A(132)**2-A(87)**2+A(136)**2-2*(-P4)
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(132)**2-A(136)**2+A(151)**2-2*(+P1)
      DMASS(1)=A(132)
      DWIDTH(1)=A(102)
      Q1(1)=-A(82)**2-2*(-P2)
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

      SUBROUTINE D704
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U704/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(87)
      DWIDTH(4)=A(88)
      Q1(4)=-A(132)**2-A(136)**2+A(87)**2-2*(+P1)
      DMASS(3)=A(136)
      DWIDTH(3)=A(110)
      Q1(3)=-A(132)**2-A(87)**2+A(136)**2-2*(-P4)
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(132)**2-A(136)**2+A(151)**2-2*(+P1)
      DMASS(1)=A(132)
      DWIDTH(1)=A(102)
      Q1(1)=-A(84)**2-2*(-P2)
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

      SUBROUTINE D705
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U705/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(132)**2-A(136)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D706
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U706/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D707
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U707/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D708
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U708/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D709
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U709/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D70
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(124)
      DWIDTH(6)=A(125)
      Q1(6)=-A(93)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(5)=A(122)
      DWIDTH(5)=A(123)
      Q1(5)=-A(93)**2-A(12)**2+A(122)**2-2*(-P4)
      DMASS(4)=A(145)
      DWIDTH(4)=A(121)
      Q1(4)=-A(93)**2-A(11)**2+A(145)**2-2*(-P2)
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(93)**2-A(11)**2+A(144)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(93)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(93)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D710
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U710/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D711
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U711/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D712
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U712/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D713
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U713/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D714
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U714/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D715
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U715/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D716
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U716/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D717
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U717/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D718
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U718/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D719
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U719/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D71
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(93)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(5)=A(126)
      DWIDTH(5)=A(127)
      Q1(5)=-A(93)**2-A(13)**2+A(126)**2-2*(-P4)
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(93)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(93)**2-A(12)**2+A(122)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(93)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(93)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D720
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U720/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D721
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U721/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D722
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U722/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D723
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U723/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D724
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U724/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D725
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U725/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D726
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U726/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D727
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U727/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D728
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U728/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D729
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U729/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D72
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(93)**2-A(82)**2+A(99)**2-2*(-P2)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(82)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(93)**2-A(82)**2+A(95)**2-2*(-P2)
      DMASS(5)=A(93)
      DWIDTH(5)=A(94)
      Q1(5)=-A(82)**2-2*(-P2)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(93)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(3)=A(87)
      DWIDTH(3)=A(88)
      Q1(3)=-A(93)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(93)**2-A(87)**2+A(89)**2-2*(-P4)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(93)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D730
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U730/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D731
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U731/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D732
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U732/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D733
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U733/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D734
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U734/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D735
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U735/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D736
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U736/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D737
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U737/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D738
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U738/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D739
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U739/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D73
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U73/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(93)**2-A(84)**2+A(99)**2-2*(-P2)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(84)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(93)**2-A(84)**2+A(95)**2-2*(-P2)
      DMASS(5)=A(93)
      DWIDTH(5)=A(94)
      Q1(5)=-A(84)**2-2*(-P2)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(93)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(3)=A(87)
      DWIDTH(3)=A(88)
      Q1(3)=-A(93)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(93)**2-A(87)**2+A(89)**2-2*(-P4)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(93)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D740
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U740/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(132)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(132)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D741
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U741/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(132)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(132)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(132)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(132)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D742
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U742/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(134)**2-A(8)**2+A(99)**2-2*(-P4)
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(134)**2-A(8)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(134)**2-A(8)**2+A(97)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(134)**2-A(8)**2+A(97)**2-2*(-P2)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(134)**2-A(8)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(134)**2-A(8)**2+A(95)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(134)**2-A(8)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D743
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U743/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(134)
      DWIDTH(2)=A(104)
      Q1(2)=-2*(-P4)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
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

      SUBROUTINE D744
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U744/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(134)
      DWIDTH(2)=A(104)
      Q1(2)=-A(6)**2-2*(-P4)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
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

      SUBROUTINE D745
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U745/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(134)
      DWIDTH(2)=A(104)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
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

      SUBROUTINE D746
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U746/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(134)
      DWIDTH(2)=A(104)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
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

      SUBROUTINE D747
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U747/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(134)
      DWIDTH(2)=A(104)
      Q1(2)=-A(6)**2-2*(-P4)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
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

      SUBROUTINE D748
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U748/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(134)
      DWIDTH(3)=A(104)
      Q1(3)=-A(82)**2-2*(-P4)
      DMASS(2)=A(6)
      DWIDTH(2)=A(15)
      Q1(2)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
      Q1(1)=-A(6)**2-2*(-P2)
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

      SUBROUTINE D749
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U749/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(134)
      DWIDTH(3)=A(104)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(6)
      DWIDTH(2)=A(15)
      Q1(2)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
      Q1(1)=-A(6)**2-2*(-P2)
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

      SUBROUTINE D74
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U74/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(93)**2-A(7)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(93)**2-A(7)**2+A(97)**2-2*(-P2)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(93)**2-A(7)**2+A(95)**2-2*(-P2)
      DMASS(4)=A(93)
      DWIDTH(4)=A(94)
      Q1(4)=-A(7)**2-2*(-P2)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(93)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(93)**2-A(87)**2+A(89)**2-2*(-P4)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(93)**2-A(89)**2+A(151)**2-2*(+P1)
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
      RETURN
      END

      SUBROUTINE D750
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U750/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D751
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U751/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(137)
      DWIDTH(4)=A(111)
      Q1(4)=-A(134)**2-A(151)**2+A(137)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(5)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D752
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U752/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(137)
      DWIDTH(3)=A(111)
      Q1(3)=-A(134)**2-A(87)**2+A(137)**2-2*(-P4)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D753
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U753/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(137)
      DWIDTH(3)=A(111)
      Q1(3)=-A(134)**2-A(151)**2+A(137)**2-2*(-P2)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D754
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U754/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D755
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U755/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(6)
      DWIDTH(3)=A(15)
      Q1(3)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D756
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U756/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D757
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U757/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(2)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D758
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U758/ Q0(7),Q1(8),Q2(8)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(84)
      DWIDTH(7)=A(85)
      Q1(7)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(6)=A(82)
      DWIDTH(6)=A(83)
      Q1(6)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(5)=A(6)
      DWIDTH(5)=A(15)
      Q1(5)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(8)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D759
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U759/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D75
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U75/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(93)**2-A(132)**2-2*(+P1)
      DMASS(1)=A(132)
      DWIDTH(1)=A(102)
      Q1(1)=-A(93)**2+A(132)**2-2*(-P4)
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

      SUBROUTINE D760
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U760/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(2)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D761
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U761/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(2)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D762
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U762/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D763
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U763/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D764
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U764/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D765
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U765/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D766
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U766/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(135)
      DWIDTH(6)=A(105)
      Q1(6)=-A(134)**2-A(82)**2+A(135)**2-2*(-P4)
      DMASS(5)=A(135)
      DWIDTH(5)=A(105)
      Q1(5)=-A(134)**2-A(82)**2+A(135)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(134)
      DWIDTH(2)=A(104)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
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

      SUBROUTINE D767
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U767/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(135)
      DWIDTH(6)=A(105)
      Q1(6)=-A(134)**2-A(84)**2+A(135)**2-2*(-P4)
      DMASS(5)=A(135)
      DWIDTH(5)=A(105)
      Q1(5)=-A(134)**2-A(82)**2+A(135)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(134)
      DWIDTH(3)=A(104)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
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

      SUBROUTINE D768
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U768/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(135)
      DWIDTH(3)=A(105)
      Q1(3)=-A(134)**2-A(7)**2+A(135)**2-2*(-P4)
      DMASS(2)=A(135)
      DWIDTH(2)=A(105)
      Q1(2)=-A(134)**2-A(82)**2+A(135)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D769
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U769/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(135)
      DWIDTH(6)=A(105)
      Q1(6)=-A(134)**2-A(84)**2+A(135)**2-2*(-P2)
      DMASS(5)=A(134)
      DWIDTH(5)=A(104)
      Q1(5)=-A(84)**2-2*(-P2)
      DMASS(4)=A(135)
      DWIDTH(4)=A(105)
      Q1(4)=-A(134)**2-A(84)**2+A(135)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(134)
      DWIDTH(2)=A(104)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D76
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U76/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(93)**2-A(132)**2-2*(+P1)
      DMASS(5)=A(132)
      DWIDTH(5)=A(102)
      Q1(5)=-A(93)**2+A(132)**2-2*(-P4)
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

      SUBROUTINE D770
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U770/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(135)
      DWIDTH(3)=A(105)
      Q1(3)=-A(134)**2-A(7)**2+A(135)**2-2*(-P4)
      DMASS(2)=A(135)
      DWIDTH(2)=A(105)
      Q1(2)=-A(134)**2-A(84)**2+A(135)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D771
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U771/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(135)
      DWIDTH(4)=A(105)
      Q1(4)=-A(134)**2-A(7)**2+A(135)**2-2*(-P2)
      DMASS(3)=A(135)
      DWIDTH(3)=A(105)
      Q1(3)=-A(134)**2-A(7)**2+A(135)**2-2*(-P4)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D772
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U772/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(137)
      DWIDTH(4)=A(111)
      Q1(4)=-A(134)**2-A(87)**2+A(137)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(134)**2-A(134)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(134)**2-A(134)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(134)**2-A(134)**2+A(6)**2-2*(+P1)
      Q1(5)=-A(134)**2-A(134)**2-2*(+P1)
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

      SUBROUTINE D773
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U773/ Q0(4),Q1(4),Q2(4)
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

      SUBROUTINE D774
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U774/ Q0(2),Q1(2),Q2(2)
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

      SUBROUTINE D775
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U775/ Q0(4),Q1(4),Q2(4)
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

      SUBROUTINE D776
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U776/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D777
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U777/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D778
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U778/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D779
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U779/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(99)
      DWIDTH(6)=A(100)
      Q1(6)=-A(134)**2-A(8)**2+A(99)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(134)**2-A(8)**2+A(97)**2-2*(-P4)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(134)**2-A(8)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(134)**2-A(8)**2+A(93)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(134)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(134)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D77
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U77/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(136)
      DWIDTH(3)=A(110)
      Q1(3)=-A(93)**2+A(136)**2-2*(-P4)
      Q1(4)=-A(93)**2-A(132)**2-2*(+P1)
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

      SUBROUTINE D780
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U780/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
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

      SUBROUTINE D781
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U781/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
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

      SUBROUTINE D782
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U782/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(151)
      DWIDTH(3)=A(16)
      Q1(3)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
      DMASS(2)=A(137)
      DWIDTH(2)=A(111)
      Q1(2)=-A(134)**2-A(151)**2+A(137)**2-2*(-P4)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
      Q1(1)=-A(6)**2-2*(-P2)
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

      SUBROUTINE D783
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U783/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(87)
      DWIDTH(3)=A(88)
      Q1(3)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(2)=A(137)
      DWIDTH(2)=A(111)
      Q1(2)=-A(134)**2-A(87)**2+A(137)**2-2*(-P4)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
      Q1(1)=-A(6)**2-2*(-P2)
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

      SUBROUTINE D784
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U784/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(87)
      DWIDTH(4)=A(88)
      Q1(4)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(3)=A(151)
      DWIDTH(3)=A(16)
      Q1(3)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
      DMASS(2)=A(137)
      DWIDTH(2)=A(111)
      Q1(2)=-A(134)**2-A(151)**2+A(137)**2-2*(-P2)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
      Q1(1)=-A(82)**2-2*(-P4)
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

      SUBROUTINE D785
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U785/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(87)
      DWIDTH(4)=A(88)
      Q1(4)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(3)=A(151)
      DWIDTH(3)=A(16)
      Q1(3)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
      DMASS(2)=A(137)
      DWIDTH(2)=A(111)
      Q1(2)=-A(134)**2-A(151)**2+A(137)**2-2*(-P2)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
      Q1(1)=-A(84)**2-2*(-P4)
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

      SUBROUTINE D786
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U786/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(87)
      DWIDTH(1)=A(88)
      Q1(1)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
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

      SUBROUTINE D787
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U787/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D788
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U788/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(87)
      DWIDTH(6)=A(88)
      Q1(6)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(5)=A(151)
      DWIDTH(5)=A(16)
      Q1(5)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(134)**2-A(8)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(134)**2-A(8)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(134)**2-A(8)**2+A(95)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(134)**2-A(8)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D789
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U789/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D78
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U78/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(136)
      DWIDTH(3)=A(110)
      Q1(3)=-A(93)**2+A(136)**2-2*(-P2)
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
      RETURN
      END

      SUBROUTINE D790
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U790/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D791
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U791/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D792
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U792/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D793
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U793/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D794
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U794/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D795
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U795/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D796
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U796/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D797
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U797/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D798
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U798/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(134)**2-A(137)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(134)**2-A(137)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D799
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U799/ Q0(5),Q1(5),Q2(5)
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
      Q1(2)=-A(134)**2-A(82)**2+A(135)**2-2*(-P2)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
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

      SUBROUTINE D79
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U79/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(132)
      DWIDTH(5)=A(102)
      Q1(5)=-A(93)**2+A(132)**2-2*(-P2)
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

      SUBROUTINE D7
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U7/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(7)
      DWIDTH(7)=A(86)
      Q1(7)=-A(93)**2-A(93)**2+A(7)**2-2*(+P1)
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(93)**2-A(93)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(93)**2-A(93)**2+A(82)**2-2*(+P1)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(93)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(93)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(93)**2-A(87)**2+A(89)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(93)**2-A(151)**2+A(89)**2-2*(-P2)
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
      RETURN
      END

