      SUBROUTINE D190
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U190/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(93)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(3)=A(124)
      DWIDTH(3)=A(125)
      Q1(3)=-A(93)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(2)=A(122)
      DWIDTH(2)=A(123)
      Q1(2)=-A(93)**2-A(12)**2+A(122)**2-2*(-P2)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
      Q1(1)=-A(93)**2-A(12)**2+A(122)**2-2*(-P4)
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

      SUBROUTINE D191
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U191/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(128)
      DWIDTH(4)=A(129)
      Q1(4)=-A(93)**2-A(13)**2+A(128)**2-2*(-P2)
      DMASS(3)=A(128)
      DWIDTH(3)=A(129)
      Q1(3)=-A(93)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(93)**2-A(13)**2+A(126)**2-2*(-P2)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
      Q1(1)=-A(93)**2-A(13)**2+A(126)**2-2*(-P4)
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

      SUBROUTINE D192
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U192/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(95)**2-A(6)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(95)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(95)**2-A(6)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(6)**2-2*(-P4)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(6)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(95)**2-A(6)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(6)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D193
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U193/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(95)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(95)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(95)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(7)
      DWIDTH(6)=A(86)
      Q1(6)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(82)**2-2*(-P4)
      DMASS(4)=A(6)
      DWIDTH(4)=A(15)
      Q1(4)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(6)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(95)**2-A(82)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(6)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D194
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U194/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(95)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(95)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(95)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(7)
      DWIDTH(6)=A(86)
      Q1(6)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(84)**2-2*(-P4)
      DMASS(4)=A(6)
      DWIDTH(4)=A(15)
      Q1(4)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(6)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(95)**2-A(84)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(6)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D195
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U195/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(95)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(95)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(95)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(7)**2-2*(-P4)
      DMASS(5)=A(84)
      DWIDTH(5)=A(85)
      Q1(5)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(4)=A(82)
      DWIDTH(4)=A(83)
      Q1(4)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(6)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(95)**2-A(7)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(6)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D196
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U196/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(84)
      DWIDTH(7)=A(85)
      Q1(7)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(6)=A(82)
      DWIDTH(6)=A(83)
      Q1(6)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(5)=A(6)
      DWIDTH(5)=A(15)
      Q1(5)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(95)**2-A(151)**2+A(91)**2-2*(-P4)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D197
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U197/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(7)
      DWIDTH(7)=A(86)
      Q1(7)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D198
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U198/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(7)
      DWIDTH(7)=A(86)
      Q1(7)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D199
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U199/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(136)
      DWIDTH(3)=A(110)
      Q1(3)=-A(95)**2+A(136)**2-2*(-P2)
      DMASS(2)=A(136)
      DWIDTH(2)=A(110)
      Q1(2)=-A(95)**2+A(136)**2-2*(-P4)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D19
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U19/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(84)
      DWIDTH(8)=A(85)
      Q1(8)=-A(93)**2-A(93)**2+A(84)**2-2*(+P1)
      DMASS(7)=A(82)
      DWIDTH(7)=A(83)
      Q1(7)=-A(93)**2-A(93)**2+A(82)**2-2*(+P1)
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(93)**2-A(13)**2+A(128)**2-2*(-P2)
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(93)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(93)**2-A(13)**2+A(126)**2-2*(-P2)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(93)**2-A(13)**2+A(126)**2-2*(-P4)
      DMASS(2)=A(7)
      DWIDTH(2)=A(86)
      Q1(2)=-A(93)**2-A(93)**2+A(7)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(93)**2-A(93)**2+A(6)**2-2*(+P1)
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

