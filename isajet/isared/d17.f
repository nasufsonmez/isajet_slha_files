      SUBROUTINE D1700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1700/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(11)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(11)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1701
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1701/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(13)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(13)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1702
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1702/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(122)**2-A(122)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(122)**2-A(122)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(122)**2-A(122)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(122)**2-A(122)**2-2*(+P1)
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

      SUBROUTINE D1703
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1703/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(84)
      DWIDTH(5)=A(85)
      Q1(5)=-A(122)**2-A(122)**2+A(84)**2-2*(+P1)
      DMASS(4)=A(82)
      DWIDTH(4)=A(83)
      Q1(4)=-A(122)**2-A(122)**2+A(82)**2-2*(+P1)
      DMASS(3)=A(6)
      DWIDTH(3)=A(15)
      Q1(3)=-A(122)**2-A(122)**2+A(6)**2-2*(+P1)
      Q1(6)=-A(122)**2-A(122)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(11)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(11)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1704
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1704/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(11)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(11)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1705
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1705/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(13)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(13)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1706
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1706/ Q0(8),Q1(9),Q2(9)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(131)
      DWIDTH(8)=A(101)
      Q1(8)=-A(122)**2-A(12)**2+A(131)**2-2*(-P2)
      DMASS(7)=A(84)
      DWIDTH(7)=A(85)
      Q1(7)=-A(122)**2-A(122)**2+A(84)**2-2*(+P1)
      DMASS(6)=A(82)
      DWIDTH(6)=A(83)
      Q1(6)=-A(122)**2-A(122)**2+A(82)**2-2*(+P1)
      DMASS(5)=A(6)
      DWIDTH(5)=A(15)
      Q1(5)=-A(122)**2-A(122)**2+A(6)**2-2*(+P1)
      Q1(9)=-A(122)**2-A(122)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(122)**2-A(12)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(122)**2-A(12)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(122)**2-A(12)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(122)**2-A(12)**2+A(93)**2-2*(-P2)
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
      DO 3 I=9,9
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D1707
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1707/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(84)
      DWIDTH(5)=A(85)
      Q1(5)=-A(122)**2-A(122)**2+A(84)**2-2*(+P1)
      DMASS(4)=A(82)
      DWIDTH(4)=A(83)
      Q1(4)=-A(122)**2-A(122)**2+A(82)**2-2*(+P1)
      DMASS(3)=A(6)
      DWIDTH(3)=A(15)
      Q1(3)=-A(122)**2-A(122)**2+A(6)**2-2*(+P1)
      Q1(6)=-A(122)**2-A(122)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(13)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(13)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1708
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1708/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(124)
      DWIDTH(6)=A(125)
      Q1(6)=-A(122)**2-A(82)**2+A(124)**2-2*(-P4)
      DMASS(5)=A(124)
      DWIDTH(5)=A(125)
      Q1(5)=-A(122)**2-A(82)**2+A(124)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(122)**2-A(122)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(122)**2-A(122)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(122)
      DWIDTH(2)=A(123)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
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

      SUBROUTINE D1709
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1709/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(124)
      DWIDTH(6)=A(125)
      Q1(6)=-A(122)**2-A(84)**2+A(124)**2-2*(-P4)
      DMASS(5)=A(124)
      DWIDTH(5)=A(125)
      Q1(5)=-A(122)**2-A(82)**2+A(124)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(122)**2-A(122)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(122)**2-A(122)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
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

      SUBROUTINE D170
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U170/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(124)
      DWIDTH(7)=A(125)
      Q1(7)=-A(93)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(6)=A(12)
      DWIDTH(6)=A(14)
      Q1(6)=-A(93)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(5)=A(122)
      DWIDTH(5)=A(123)
      Q1(5)=-A(93)**2-A(12)**2+A(122)**2-2*(-P2)
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

      SUBROUTINE D1710
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1710/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(124)
      DWIDTH(3)=A(125)
      Q1(3)=-A(122)**2-A(7)**2+A(124)**2-2*(-P4)
      DMASS(2)=A(124)
      DWIDTH(2)=A(125)
      Q1(2)=-A(122)**2-A(82)**2+A(124)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(122)**2-A(122)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D1711
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1711/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(124)
      DWIDTH(6)=A(125)
      Q1(6)=-A(122)**2-A(84)**2+A(124)**2-2*(-P2)
      DMASS(5)=A(122)
      DWIDTH(5)=A(123)
      Q1(5)=-A(84)**2-2*(-P2)
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(122)**2-A(84)**2+A(124)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(122)**2-A(122)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(122)
      DWIDTH(2)=A(123)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(122)**2-A(122)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D1712
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1712/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(124)
      DWIDTH(3)=A(125)
      Q1(3)=-A(122)**2-A(7)**2+A(124)**2-2*(-P4)
      DMASS(2)=A(124)
      DWIDTH(2)=A(125)
      Q1(2)=-A(122)**2-A(84)**2+A(124)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(122)**2-A(122)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D1713
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1713/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(122)**2-A(7)**2+A(124)**2-2*(-P2)
      DMASS(3)=A(124)
      DWIDTH(3)=A(125)
      Q1(3)=-A(122)**2-A(7)**2+A(124)**2-2*(-P4)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(122)**2-A(122)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(122)**2-A(122)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D1714
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1714/ Q0(8),Q1(9),Q2(9)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(128)
      DWIDTH(8)=A(129)
      Q1(8)=-A(122)**2-A(87)**2+A(128)**2-2*(-P2)
      DMASS(7)=A(126)
      DWIDTH(7)=A(127)
      Q1(7)=-A(122)**2-A(87)**2+A(126)**2-2*(-P2)
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(122)**2-A(122)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(122)**2-A(122)**2+A(82)**2-2*(+P1)
      DMASS(4)=A(6)
      DWIDTH(4)=A(15)
      Q1(4)=-A(122)**2-A(122)**2+A(6)**2-2*(+P1)
      Q1(9)=-A(122)**2-A(122)**2-2*(+P1)
      DMASS(3)=A(145)
      DWIDTH(3)=A(121)
      Q1(3)=-A(122)**2-A(87)**2+A(145)**2-2*(-P2)
      DMASS(2)=A(144)
      DWIDTH(2)=A(120)
      Q1(2)=-A(122)**2-A(87)**2+A(144)**2-2*(-P2)
      DMASS(1)=A(140)
      DWIDTH(1)=A(116)
      Q1(1)=-A(122)**2-A(87)**2+A(140)**2-2*(-P2)
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
      DO 3 I=9,9
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D1715
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1715/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1716
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1716/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(11)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(11)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1717
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1717/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(13)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(13)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1718
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1718/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1719
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1719/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D171
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U171/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(124)
      DWIDTH(7)=A(125)
      Q1(7)=-A(93)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(6)=A(12)
      DWIDTH(6)=A(14)
      Q1(6)=-A(93)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(5)=A(122)
      DWIDTH(5)=A(123)
      Q1(5)=-A(93)**2-A(12)**2+A(122)**2-2*(-P2)
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

      SUBROUTINE D1720
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1720/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(11)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(11)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1721
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1721/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(13)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(13)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1722
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1722/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(11)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(11)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1723
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1723/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(131)
      DWIDTH(7)=A(101)
      Q1(7)=-A(122)**2-A(12)**2+A(131)**2-2*(-P2)
      DMASS(6)=A(99)
      DWIDTH(6)=A(100)
      Q1(6)=-A(122)**2-A(12)**2+A(99)**2-2*(-P2)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(122)**2-A(12)**2+A(97)**2-2*(-P2)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(122)**2-A(12)**2+A(95)**2-2*(-P2)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(122)**2-A(12)**2+A(93)**2-2*(-P2)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(122)**2-A(13)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(122)**2-A(13)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1724
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1724/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(151)
      DWIDTH(3)=A(16)
      Q1(3)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(122)**2-A(151)**2+A(126)**2-2*(-P4)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
      Q1(1)=-2*(-P2)
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

      SUBROUTINE D1725
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1725/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(87)
      DWIDTH(3)=A(88)
      Q1(3)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(122)**2-A(87)**2+A(126)**2-2*(-P4)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
      Q1(1)=-2*(-P2)
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

      SUBROUTINE D1726
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1726/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(122)**2-A(151)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(151)
      DWIDTH(4)=A(16)
      Q1(4)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(122)**2-A(151)**2+A(126)**2-2*(-P4)
      DMASS(2)=A(124)
      DWIDTH(2)=A(125)
      Q1(2)=-A(122)**2-A(6)**2+A(124)**2-2*(-P2)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
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

      SUBROUTINE D1727
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1727/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(122)**2-A(87)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(87)
      DWIDTH(4)=A(88)
      Q1(4)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(122)**2-A(87)**2+A(126)**2-2*(-P4)
      DMASS(2)=A(124)
      DWIDTH(2)=A(125)
      Q1(2)=-A(122)**2-A(6)**2+A(124)**2-2*(-P2)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
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

      SUBROUTINE D1728
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1728/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(122)**2-A(151)**2+A(126)**2-2*(-P2)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
      Q1(1)=-2*(-P4)
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

      SUBROUTINE D1729
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1729/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(122)**2-A(151)**2+A(128)**2-2*(-P2)
      DMASS(5)=A(87)
      DWIDTH(5)=A(88)
      Q1(5)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(4)=A(151)
      DWIDTH(4)=A(16)
      Q1(4)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(122)**2-A(151)**2+A(126)**2-2*(-P2)
      DMASS(2)=A(124)
      DWIDTH(2)=A(125)
      Q1(2)=-A(122)**2-A(82)**2+A(124)**2-2*(-P4)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
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
      RETURN
      END

      SUBROUTINE D172
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U172/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(124)
      DWIDTH(6)=A(125)
      Q1(6)=-A(93)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(5)=A(12)
      DWIDTH(5)=A(14)
      Q1(5)=-A(93)**2-A(122)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D1730
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1730/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(122)**2-A(151)**2+A(128)**2-2*(-P2)
      DMASS(5)=A(87)
      DWIDTH(5)=A(88)
      Q1(5)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(4)=A(151)
      DWIDTH(4)=A(16)
      Q1(4)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(122)**2-A(151)**2+A(126)**2-2*(-P2)
      DMASS(2)=A(124)
      DWIDTH(2)=A(125)
      Q1(2)=-A(122)**2-A(84)**2+A(124)**2-2*(-P4)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
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
      RETURN
      END

      SUBROUTINE D1731
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1731/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(128)
      DWIDTH(3)=A(129)
      Q1(3)=-A(122)**2-A(151)**2+A(128)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(124)
      DWIDTH(1)=A(125)
      Q1(1)=-A(122)**2-A(7)**2+A(124)**2-2*(-P4)
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

      SUBROUTINE D1732
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1732/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(122)**2-A(87)**2+A(126)**2-2*(-P4)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
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

      SUBROUTINE D1733
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1733/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D1734
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1734/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D1735
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1735/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D1736
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1736/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D1737
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1737/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D1738
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1738/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D1739
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1739/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D173
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U173/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(93)**2-A(13)**2+A(128)**2-2*(-P2)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(93)**2-A(13)**2+A(126)**2-2*(-P2)
      DMASS(3)=A(12)
      DWIDTH(3)=A(14)
      Q1(3)=-A(93)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(93)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(93)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1740
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1740/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D1741
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1741/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D1742
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1742/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D1743
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1743/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D1744
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1744/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(131)
      DWIDTH(7)=A(101)
      Q1(7)=-A(122)**2-A(12)**2+A(131)**2-2*(-P2)
      DMASS(6)=A(87)
      DWIDTH(6)=A(88)
      Q1(6)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(5)=A(151)
      DWIDTH(5)=A(16)
      Q1(5)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(122)**2-A(12)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(122)**2-A(12)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(122)**2-A(12)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(122)**2-A(12)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D1745
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1745/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(122)**2-A(87)**2+A(128)**2-2*(-P4)
      DMASS(5)=A(87)
      DWIDTH(5)=A(88)
      Q1(5)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(122)**2-A(87)**2+A(126)**2-2*(-P4)
      DMASS(3)=A(151)
      DWIDTH(3)=A(16)
      Q1(3)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
      DMASS(2)=A(124)
      DWIDTH(2)=A(125)
      Q1(2)=-A(122)**2-A(82)**2+A(124)**2-2*(-P2)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
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

      SUBROUTINE D1746
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1746/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(122)**2-A(87)**2+A(128)**2-2*(-P4)
      DMASS(5)=A(87)
      DWIDTH(5)=A(88)
      Q1(5)=-A(122)**2-A(126)**2+A(87)**2-2*(+P1)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(122)**2-A(87)**2+A(126)**2-2*(-P4)
      DMASS(3)=A(151)
      DWIDTH(3)=A(16)
      Q1(3)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
      DMASS(2)=A(124)
      DWIDTH(2)=A(125)
      Q1(2)=-A(122)**2-A(84)**2+A(124)**2-2*(-P2)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
      Q1(1)=-A(84)**2-2*(-P2)
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

      SUBROUTINE D1747
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1747/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(128)
      DWIDTH(3)=A(129)
      Q1(3)=-A(122)**2-A(87)**2+A(128)**2-2*(-P4)
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(122)**2-A(126)**2+A(151)**2-2*(+P1)
      DMASS(1)=A(124)
      DWIDTH(1)=A(125)
      Q1(1)=-A(122)**2-A(7)**2+A(124)**2-2*(-P2)
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

      SUBROUTINE D1748
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1748/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(122)
      DWIDTH(2)=A(123)
      Q1(2)=-2*(-P2)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D1749
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1749/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(124)
      DWIDTH(3)=A(125)
      Q1(3)=-A(122)**2-A(6)**2+A(124)**2-2*(-P2)
      DMASS(2)=A(122)
      DWIDTH(2)=A(123)
      Q1(2)=-A(6)**2-2*(-P2)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D174
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U174/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(93)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
      Q1(1)=-A(93)**2-A(13)**2+A(126)**2-2*(-P4)
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

      SUBROUTINE D1750
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1750/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(140)
      DWIDTH(2)=A(116)
      Q1(2)=-A(122)**2-A(151)**2+A(140)**2-2*(-P2)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D1751
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1751/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(144)
      DWIDTH(2)=A(120)
      Q1(2)=-A(122)**2-A(151)**2+A(144)**2-2*(-P2)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D1752
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1752/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(128)
      DWIDTH(3)=A(129)
      Q1(3)=-A(122)**2-A(151)**2+A(128)**2-2*(-P2)
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(122)**2-A(151)**2+A(126)**2-2*(-P2)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D1753
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1753/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-2*(-P2)
      DMASS(2)=A(12)
      DWIDTH(2)=A(14)
      Q1(2)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
      DMASS(1)=A(131)
      DWIDTH(1)=A(101)
      Q1(1)=-A(122)**2-A(12)**2+A(131)**2-2*(-P4)
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

      SUBROUTINE D1754
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1754/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(140)
      DWIDTH(2)=A(116)
      Q1(2)=-A(122)**2-A(87)**2+A(140)**2-2*(-P4)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D1755
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1755/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(145)
      DWIDTH(3)=A(121)
      Q1(3)=-A(122)**2-A(87)**2+A(145)**2-2*(-P4)
      DMASS(2)=A(144)
      DWIDTH(2)=A(120)
      Q1(2)=-A(122)**2-A(87)**2+A(144)**2-2*(-P4)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D1756
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1756/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(124)
      DWIDTH(3)=A(125)
      Q1(3)=-A(122)**2-A(82)**2+A(124)**2-2*(-P4)
      DMASS(2)=A(122)
      DWIDTH(2)=A(123)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D1757
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1757/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(124)
      DWIDTH(3)=A(125)
      Q1(3)=-A(122)**2-A(84)**2+A(124)**2-2*(-P4)
      DMASS(2)=A(122)
      DWIDTH(2)=A(123)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D1758
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1758/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(124)
      DWIDTH(2)=A(125)
      Q1(2)=-A(122)**2-A(7)**2+A(124)**2-2*(-P4)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D1759
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1759/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(128)
      DWIDTH(3)=A(129)
      Q1(3)=-A(122)**2-A(87)**2+A(128)**2-2*(-P4)
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(122)**2-A(87)**2+A(126)**2-2*(-P4)
      DMASS(1)=A(12)
      DWIDTH(1)=A(14)
      Q1(1)=-A(122)**2-A(131)**2+A(12)**2-2*(+P1)
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

      SUBROUTINE D175
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U175/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(93)**2-A(13)**2+A(128)**2-2*(-P4)
      Q1(7)=-A(93)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(5)=A(126)
      DWIDTH(5)=A(127)
      Q1(5)=-A(93)**2-A(13)**2+A(126)**2-2*(-P4)
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

      SUBROUTINE D1760
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(131)
      DWIDTH(10)=A(101)
      Q1(10)=-A(126)**2-A(13)**2+A(131)**2-2*(-P2)
      DMASS(9)=A(131)
      DWIDTH(9)=A(101)
      Q1(9)=-A(126)**2-A(13)**2+A(131)**2-2*(-P4)
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(126)**2-A(13)**2+A(99)**2-2*(-P4)
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(126)**2-A(13)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(126)**2-A(13)**2+A(97)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(126)**2-A(13)**2+A(97)**2-2*(-P2)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(126)**2-A(13)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(126)**2-A(13)**2+A(95)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(126)**2-A(13)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(126)**2-A(13)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D1761
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1761/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1762
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1762/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(6)**2-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1763
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1763/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1764
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1764/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1765
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1765/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1766
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(126)**2-A(6)**2+A(128)**2-2*(-P4)
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(126)**2-A(6)**2+A(128)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(6)**2-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1767
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1767/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
      Q1(1)=-A(6)**2-2*(-P2)
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

      SUBROUTINE D1768
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(126)**2-A(82)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(128)
      DWIDTH(4)=A(129)
      Q1(4)=-A(126)**2-A(6)**2+A(128)**2-2*(-P2)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(82)**2-2*(-P4)
      DMASS(2)=A(6)
      DWIDTH(2)=A(15)
      Q1(2)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1769
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(126)**2-A(84)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(128)
      DWIDTH(4)=A(129)
      Q1(4)=-A(126)**2-A(6)**2+A(128)**2-2*(-P2)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(6)
      DWIDTH(2)=A(15)
      Q1(2)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D176
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U176/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(93)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(3)=A(138)
      DWIDTH(3)=A(114)
      Q1(3)=-A(93)**2+A(138)**2-2*(-P4)
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

      SUBROUTINE D1770
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(128)
      DWIDTH(4)=A(129)
      Q1(4)=-A(126)**2-A(7)**2+A(128)**2-2*(-P4)
      DMASS(3)=A(128)
      DWIDTH(3)=A(129)
      Q1(3)=-A(126)**2-A(6)**2+A(128)**2-2*(-P2)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D1771
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(84)
      DWIDTH(7)=A(85)
      Q1(7)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(6)=A(82)
      DWIDTH(6)=A(83)
      Q1(6)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(5)=A(6)
      DWIDTH(5)=A(15)
      Q1(5)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      Q1(8)=-A(126)**2-A(126)**2-2*(+P1)
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(126)**2-A(151)**2+A(124)**2-2*(-P4)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(126)**2-A(151)**2+A(122)**2-2*(-P4)
      DMASS(2)=A(142)
      DWIDTH(2)=A(118)
      Q1(2)=-A(126)**2-A(151)**2+A(142)**2-2*(-P4)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(126)**2-A(151)**2+A(138)**2-2*(-P4)
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

      SUBROUTINE D1772
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(126)**2-A(87)**2+A(124)**2-2*(-P4)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(126)**2-A(87)**2+A(122)**2-2*(-P4)
      DMASS(2)=A(142)
      DWIDTH(2)=A(118)
      Q1(2)=-A(126)**2-A(87)**2+A(142)**2-2*(-P4)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(126)**2-A(87)**2+A(138)**2-2*(-P4)
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

      SUBROUTINE D1773
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(126)**2-A(151)**2+A(124)**2-2*(-P2)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(126)**2-A(151)**2+A(122)**2-2*(-P2)
      DMASS(2)=A(142)
      DWIDTH(2)=A(118)
      Q1(2)=-A(126)**2-A(151)**2+A(142)**2-2*(-P2)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(126)**2-A(151)**2+A(138)**2-2*(-P2)
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

      SUBROUTINE D1774
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1774/ Q0(2),Q1(3),Q2(3)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      Q1(3)=-A(126)**2-A(126)**2-2*(+P1)
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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
      DO 3 I=3,3
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D1775
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1775/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1776
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1776/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1777
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1777/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D1778
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1778/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D1779
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1779/ Q0(1),Q1(1),Q2(1)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D177
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U177/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(93)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(93)**2-A(10)**2+A(142)**2-2*(-P4)
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

      SUBROUTINE D1780
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1780/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      Q1(2)=-A(126)**2-A(126)**2-2*(+P1)
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

      SUBROUTINE D1781
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(126)**2-A(126)**2-2*(+P1)
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

      SUBROUTINE D1782
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(126)**2-A(126)**2-2*(+P1)
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

      SUBROUTINE D1783
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(6)
      DWIDTH(3)=A(15)
      Q1(3)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(126)**2-A(126)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(126)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(126)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1784
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1784/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(126)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(126)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1785
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1785/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(126)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(126)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1786
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1786/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(126)**2-A(10)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(126)**2-A(10)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1787
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1787/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(126)**2-A(12)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(126)**2-A(12)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1788
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1788/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      Q1(2)=-A(126)**2-A(126)**2-2*(+P1)
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

      SUBROUTINE D1789
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(84)
      DWIDTH(5)=A(85)
      Q1(5)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(4)=A(82)
      DWIDTH(4)=A(83)
      Q1(4)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(3)=A(6)
      DWIDTH(3)=A(15)
      Q1(3)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      Q1(6)=-A(126)**2-A(126)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(126)**2-A(10)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(126)**2-A(10)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D178
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U178/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      Q1(5)=-A(93)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(93)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(93)**2-A(12)**2+A(122)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(93)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(93)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1790
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1790/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(126)**2-A(10)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(126)**2-A(10)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1791
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1791/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(126)**2-A(12)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(126)**2-A(12)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D1792
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      Q1(4)=-A(126)**2-A(126)**2-2*(+P1)
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

      SUBROUTINE D1793
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(84)
      DWIDTH(5)=A(85)
      Q1(5)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(4)=A(82)
      DWIDTH(4)=A(83)
      Q1(4)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(3)=A(6)
      DWIDTH(3)=A(15)
      Q1(3)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      Q1(6)=-A(126)**2-A(126)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(126)**2-A(12)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(126)**2-A(12)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D1794
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(131)
      DWIDTH(8)=A(101)
      Q1(8)=-A(126)**2-A(13)**2+A(131)**2-2*(-P2)
      DMASS(7)=A(84)
      DWIDTH(7)=A(85)
      Q1(7)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(6)=A(82)
      DWIDTH(6)=A(83)
      Q1(6)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(5)=A(6)
      DWIDTH(5)=A(15)
      Q1(5)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
      Q1(9)=-A(126)**2-A(126)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(126)**2-A(13)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(126)**2-A(13)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(126)**2-A(13)**2+A(95)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(126)**2-A(13)**2+A(93)**2-2*(-P2)
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
      DO 3 I=9,9
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D1795
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(126)**2-A(82)**2+A(128)**2-2*(-P4)
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(126)**2-A(82)**2+A(128)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1796
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(126)**2-A(84)**2+A(128)**2-2*(-P4)
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(126)**2-A(82)**2+A(128)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
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

      SUBROUTINE D1797
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1797/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(128)
      DWIDTH(3)=A(129)
      Q1(3)=-A(126)**2-A(7)**2+A(128)**2-2*(-P4)
      DMASS(2)=A(128)
      DWIDTH(2)=A(129)
      Q1(2)=-A(126)**2-A(82)**2+A(128)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D1798
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(126)**2-A(84)**2+A(128)**2-2*(-P2)
      DMASS(5)=A(126)
      DWIDTH(5)=A(127)
      Q1(5)=-A(84)**2-2*(-P2)
      DMASS(4)=A(128)
      DWIDTH(4)=A(129)
      Q1(4)=-A(126)**2-A(84)**2+A(128)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(126)**2-A(126)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(126)**2-A(126)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D1799
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1799/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(128)
      DWIDTH(3)=A(129)
      Q1(3)=-A(126)**2-A(7)**2+A(128)**2-2*(-P4)
      DMASS(2)=A(128)
      DWIDTH(2)=A(129)
      Q1(2)=-A(126)**2-A(84)**2+A(128)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(126)**2-A(126)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D179
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U179/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(93)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
      Q1(1)=-A(93)**2-A(13)**2+A(126)**2-2*(-P4)
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

      SUBROUTINE D17
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U17/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(84)
      DWIDTH(8)=A(85)
      Q1(8)=-A(93)**2-A(93)**2+A(84)**2-2*(+P1)
      DMASS(7)=A(82)
      DWIDTH(7)=A(83)
      Q1(7)=-A(93)**2-A(93)**2+A(82)**2-2*(+P1)
      DMASS(6)=A(145)
      DWIDTH(6)=A(121)
      Q1(6)=-A(93)**2-A(11)**2+A(145)**2-2*(-P2)
      DMASS(5)=A(145)
      DWIDTH(5)=A(121)
      Q1(5)=-A(93)**2-A(11)**2+A(145)**2-2*(-P4)
      DMASS(4)=A(144)
      DWIDTH(4)=A(120)
      Q1(4)=-A(93)**2-A(11)**2+A(144)**2-2*(-P2)
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(93)**2-A(11)**2+A(144)**2-2*(-P4)
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

