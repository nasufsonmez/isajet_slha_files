      SUBROUTINE D300
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U300/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(7)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(7)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D301
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U301/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(143)
      DWIDTH(4)=A(119)
      Q1(4)=-A(95)**2-A(10)**2+A(143)**2-2*(-P2)
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(95)**2-A(10)**2+A(142)**2-2*(-P2)
      Q1(5)=-A(95)**2-A(140)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D302
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U302/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(95)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(95)**2-A(12)**2+A(122)**2-2*(-P2)
      Q1(5)=-A(95)**2-A(140)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D303
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U303/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(1)=A(142)
      DWIDTH(1)=A(118)
      Q1(1)=-A(95)**2-A(10)**2+A(142)**2-2*(-P4)
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

      SUBROUTINE D304
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U304/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(5)=A(142)
      DWIDTH(5)=A(118)
      Q1(5)=-A(95)**2-A(10)**2+A(142)**2-2*(-P4)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(6)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(6)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D305
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U305/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(3)=A(140)
      DWIDTH(3)=A(116)
      Q1(3)=-A(95)**2+A(140)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D306
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U306/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(95)**2-A(11)**2+A(144)**2-2*(-P4)
      Q1(4)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D307
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U307/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(128)
      DWIDTH(4)=A(129)
      Q1(4)=-A(95)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
      Q1(5)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D308
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U308/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(1)=A(142)
      DWIDTH(1)=A(118)
      Q1(1)=-A(95)**2-A(10)**2+A(142)**2-2*(-P4)
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

      SUBROUTINE D309
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U309/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(3)=A(140)
      DWIDTH(3)=A(116)
      Q1(3)=-A(95)**2+A(140)**2-2*(-P2)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D30
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U30/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(93)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(93)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(93)**2-A(7)**2+A(95)**2-2*(-P4)
      DMASS(5)=A(84)
      DWIDTH(5)=A(85)
      Q1(5)=-A(93)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(4)=A(82)
      DWIDTH(4)=A(83)
      Q1(4)=-A(93)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(93)**2-A(6)**2+A(95)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(7)**2-2*(-P4)
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

      SUBROUTINE D310
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U310/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(143)
      DWIDTH(6)=A(119)
      Q1(6)=-A(95)**2-A(10)**2+A(143)**2-2*(-P2)
      Q1(7)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(5)=A(142)
      DWIDTH(5)=A(118)
      Q1(5)=-A(95)**2-A(10)**2+A(142)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(82)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D311
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U311/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(143)
      DWIDTH(6)=A(119)
      Q1(6)=-A(95)**2-A(10)**2+A(143)**2-2*(-P2)
      Q1(7)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(5)=A(142)
      DWIDTH(5)=A(118)
      Q1(5)=-A(95)**2-A(10)**2+A(142)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(84)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D312
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U312/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(143)
      DWIDTH(5)=A(119)
      Q1(5)=-A(95)**2-A(10)**2+A(143)**2-2*(-P2)
      Q1(6)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(7)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(7)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D313
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U313/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(145)
      DWIDTH(4)=A(121)
      Q1(4)=-A(95)**2-A(11)**2+A(145)**2-2*(-P2)
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(95)**2-A(11)**2+A(144)**2-2*(-P2)
      Q1(5)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D314
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U314/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(128)
      DWIDTH(4)=A(129)
      Q1(4)=-A(95)**2-A(13)**2+A(128)**2-2*(-P2)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(95)**2-A(13)**2+A(126)**2-2*(-P2)
      Q1(5)=-A(95)**2-A(142)**2+A(10)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D315
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U315/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(1)=A(144)
      DWIDTH(1)=A(120)
      Q1(1)=-A(95)**2-A(11)**2+A(144)**2-2*(-P4)
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

      SUBROUTINE D316
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U316/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(5)=A(144)
      DWIDTH(5)=A(120)
      Q1(5)=-A(95)**2-A(11)**2+A(144)**2-2*(-P4)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(6)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(6)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D317
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U317/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(3)=A(138)
      DWIDTH(3)=A(114)
      Q1(3)=-A(95)**2+A(138)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D318
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U318/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(95)**2-A(10)**2+A(142)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D319
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U319/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(95)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(95)**2-A(12)**2+A(122)**2-2*(-P4)
      Q1(5)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D31
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U31/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(84)
      DWIDTH(7)=A(85)
      Q1(7)=-A(93)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(6)=A(82)
      DWIDTH(6)=A(83)
      Q1(6)=-A(93)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(5)=A(6)
      DWIDTH(5)=A(15)
      Q1(5)=-A(93)**2-A(95)**2+A(6)**2-2*(+P1)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(93)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(93)**2-A(151)**2+A(91)**2-2*(-P4)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(93)**2-A(151)**2+A(89)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(93)**2-A(151)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D320
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U320/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(1)=A(144)
      DWIDTH(1)=A(120)
      Q1(1)=-A(95)**2-A(11)**2+A(144)**2-2*(-P4)
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

      SUBROUTINE D321
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U321/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(3)=A(138)
      DWIDTH(3)=A(114)
      Q1(3)=-A(95)**2+A(138)**2-2*(-P2)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D322
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U322/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      Q1(5)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(4)=A(143)
      DWIDTH(4)=A(119)
      Q1(4)=-A(95)**2-A(10)**2+A(143)**2-2*(-P2)
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(95)**2-A(10)**2+A(142)**2-2*(-P2)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D323
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U323/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(145)
      DWIDTH(6)=A(121)
      Q1(6)=-A(95)**2-A(11)**2+A(145)**2-2*(-P2)
      Q1(7)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(5)=A(144)
      DWIDTH(5)=A(120)
      Q1(5)=-A(95)**2-A(11)**2+A(144)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(82)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D324
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U324/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(145)
      DWIDTH(6)=A(121)
      Q1(6)=-A(95)**2-A(11)**2+A(145)**2-2*(-P2)
      Q1(7)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(5)=A(144)
      DWIDTH(5)=A(120)
      Q1(5)=-A(95)**2-A(11)**2+A(144)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(84)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D325
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U325/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(145)
      DWIDTH(5)=A(121)
      Q1(5)=-A(95)**2-A(11)**2+A(145)**2-2*(-P2)
      Q1(6)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(7)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(7)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D326
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U326/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(95)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(95)**2-A(12)**2+A(122)**2-2*(-P2)
      Q1(5)=-A(95)**2-A(144)**2+A(11)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D327
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U327/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(12)
      DWIDTH(2)=A(14)
      Q1(2)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
      Q1(1)=-A(95)**2-A(12)**2+A(122)**2-2*(-P4)
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

      SUBROUTINE D328
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U328/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(124)
      DWIDTH(7)=A(125)
      Q1(7)=-A(95)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(6)=A(12)
      DWIDTH(6)=A(14)
      Q1(6)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(5)=A(122)
      DWIDTH(5)=A(123)
      Q1(5)=-A(95)**2-A(12)**2+A(122)**2-2*(-P4)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(6)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(6)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D329
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U329/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(12)
      DWIDTH(4)=A(14)
      Q1(4)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(3)=A(140)
      DWIDTH(3)=A(116)
      Q1(3)=-A(95)**2+A(140)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D32
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U32/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(7)
      DWIDTH(7)=A(86)
      Q1(7)=-A(93)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(93)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(93)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(93)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(93)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(93)**2-A(151)**2+A(89)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(93)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D330
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U330/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(12)
      DWIDTH(4)=A(14)
      Q1(4)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(95)**2-A(11)**2+A(144)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D331
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U331/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(95)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
      DMASS(3)=A(12)
      DWIDTH(3)=A(14)
      Q1(3)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D332
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U332/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(12)
      DWIDTH(2)=A(14)
      Q1(2)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
      Q1(1)=-A(95)**2-A(12)**2+A(122)**2-2*(-P4)
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

      SUBROUTINE D333
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U333/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(12)
      DWIDTH(4)=A(14)
      Q1(4)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(3)=A(140)
      DWIDTH(3)=A(116)
      Q1(3)=-A(95)**2+A(140)**2-2*(-P2)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D334
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U334/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(12)
      DWIDTH(5)=A(14)
      Q1(5)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(4)=A(145)
      DWIDTH(4)=A(121)
      Q1(4)=-A(95)**2-A(11)**2+A(145)**2-2*(-P2)
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(95)**2-A(11)**2+A(144)**2-2*(-P2)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D335
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U335/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(124)
      DWIDTH(7)=A(125)
      Q1(7)=-A(95)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(6)=A(12)
      DWIDTH(6)=A(14)
      Q1(6)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(5)=A(122)
      DWIDTH(5)=A(123)
      Q1(5)=-A(95)**2-A(12)**2+A(122)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(82)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D336
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U336/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(124)
      DWIDTH(7)=A(125)
      Q1(7)=-A(95)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(6)=A(12)
      DWIDTH(6)=A(14)
      Q1(6)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(5)=A(122)
      DWIDTH(5)=A(123)
      Q1(5)=-A(95)**2-A(12)**2+A(122)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(84)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D337
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U337/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(124)
      DWIDTH(6)=A(125)
      Q1(6)=-A(95)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(5)=A(12)
      DWIDTH(5)=A(14)
      Q1(5)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(7)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(7)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D338
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U338/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(95)**2-A(13)**2+A(128)**2-2*(-P2)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(95)**2-A(13)**2+A(126)**2-2*(-P2)
      DMASS(3)=A(12)
      DWIDTH(3)=A(14)
      Q1(3)=-A(95)**2-A(122)**2+A(12)**2-2*(+P1)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D339
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U339/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
      Q1(1)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
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

      SUBROUTINE D33
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U33/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(7)
      DWIDTH(7)=A(86)
      Q1(7)=-A(93)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(93)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(93)**2-A(95)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D340
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U340/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(95)**2-A(13)**2+A(128)**2-2*(-P4)
      Q1(7)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(5)=A(126)
      DWIDTH(5)=A(127)
      Q1(5)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(6)**2-2*(-P2)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(6)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D341
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U341/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(3)=A(138)
      DWIDTH(3)=A(114)
      Q1(3)=-A(95)**2+A(138)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D342
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U342/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(95)**2-A(10)**2+A(142)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D343
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U343/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      Q1(5)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(95)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(95)**2-A(12)**2+A(122)**2-2*(-P4)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D344
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U344/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
      Q1(1)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
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

      SUBROUTINE D345
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U345/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(3)=A(138)
      DWIDTH(3)=A(114)
      Q1(3)=-A(95)**2+A(138)**2-2*(-P2)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D346
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U346/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      Q1(5)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(4)=A(143)
      DWIDTH(4)=A(119)
      Q1(4)=-A(95)**2-A(10)**2+A(143)**2-2*(-P2)
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(95)**2-A(10)**2+A(142)**2-2*(-P2)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D347
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U347/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      Q1(5)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(95)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(95)**2-A(12)**2+A(122)**2-2*(-P2)
      DMASS(2)=A(91)
      DWIDTH(2)=A(92)
      Q1(2)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D348
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U348/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(95)**2-A(13)**2+A(128)**2-2*(-P2)
      Q1(7)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(5)=A(126)
      DWIDTH(5)=A(127)
      Q1(5)=-A(95)**2-A(13)**2+A(126)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(82)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D349
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U349/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(95)**2-A(13)**2+A(128)**2-2*(-P2)
      Q1(7)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(5)=A(126)
      DWIDTH(5)=A(127)
      Q1(5)=-A(95)**2-A(13)**2+A(126)**2-2*(-P2)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(84)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D34
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U34/ Q0(3),Q1(3),Q2(3)
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
      Q1(1)=-A(93)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D350
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U350/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(95)**2-A(13)**2+A(128)**2-2*(-P2)
      Q1(6)=-A(95)**2-A(126)**2+A(13)**2-2*(+P1)
      DMASS(4)=A(99)
      DWIDTH(4)=A(100)
      Q1(4)=-A(95)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(3)=A(97)
      DWIDTH(3)=A(98)
      Q1(3)=-A(95)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(2)=A(95)
      DWIDTH(2)=A(96)
      Q1(2)=-A(7)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(7)**2+A(93)**2-2*(-P4)
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

      SUBROUTINE D351
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U351/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(139)
      DWIDTH(4)=A(115)
      Q1(4)=-A(95)**2+A(139)**2-2*(-P2)
      DMASS(3)=A(139)
      DWIDTH(3)=A(115)
      Q1(3)=-A(95)**2+A(139)**2-2*(-P4)
      DMASS(2)=A(138)
      DWIDTH(2)=A(114)
      Q1(2)=-A(95)**2+A(138)**2-2*(-P2)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(95)**2+A(138)**2-2*(-P4)
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

      SUBROUTINE D352
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U352/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(141)
      DWIDTH(4)=A(117)
      Q1(4)=-A(95)**2+A(141)**2-2*(-P2)
      DMASS(3)=A(141)
      DWIDTH(3)=A(117)
      Q1(3)=-A(95)**2+A(141)**2-2*(-P4)
      DMASS(2)=A(140)
      DWIDTH(2)=A(116)
      Q1(2)=-A(95)**2+A(140)**2-2*(-P2)
      DMASS(1)=A(140)
      DWIDTH(1)=A(116)
      Q1(1)=-A(95)**2+A(140)**2-2*(-P4)
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

      SUBROUTINE D353
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U353/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(143)
      DWIDTH(4)=A(119)
      Q1(4)=-A(95)**2-A(10)**2+A(143)**2-2*(-P2)
      DMASS(3)=A(143)
      DWIDTH(3)=A(119)
      Q1(3)=-A(95)**2-A(10)**2+A(143)**2-2*(-P4)
      DMASS(2)=A(142)
      DWIDTH(2)=A(118)
      Q1(2)=-A(95)**2-A(10)**2+A(142)**2-2*(-P2)
      DMASS(1)=A(142)
      DWIDTH(1)=A(118)
      Q1(1)=-A(95)**2-A(10)**2+A(142)**2-2*(-P4)
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

      SUBROUTINE D354
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U354/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(145)
      DWIDTH(4)=A(121)
      Q1(4)=-A(95)**2-A(11)**2+A(145)**2-2*(-P2)
      DMASS(3)=A(145)
      DWIDTH(3)=A(121)
      Q1(3)=-A(95)**2-A(11)**2+A(145)**2-2*(-P4)
      DMASS(2)=A(144)
      DWIDTH(2)=A(120)
      Q1(2)=-A(95)**2-A(11)**2+A(144)**2-2*(-P2)
      DMASS(1)=A(144)
      DWIDTH(1)=A(120)
      Q1(1)=-A(95)**2-A(11)**2+A(144)**2-2*(-P4)
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

      SUBROUTINE D355
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U355/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(95)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(3)=A(124)
      DWIDTH(3)=A(125)
      Q1(3)=-A(95)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(2)=A(122)
      DWIDTH(2)=A(123)
      Q1(2)=-A(95)**2-A(12)**2+A(122)**2-2*(-P2)
      DMASS(1)=A(122)
      DWIDTH(1)=A(123)
      Q1(1)=-A(95)**2-A(12)**2+A(122)**2-2*(-P4)
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

      SUBROUTINE D356
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U356/ Q0(4),Q1(4),Q2(4)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(128)
      DWIDTH(4)=A(129)
      Q1(4)=-A(95)**2-A(13)**2+A(128)**2-2*(-P2)
      DMASS(3)=A(128)
      DWIDTH(3)=A(129)
      Q1(3)=-A(95)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(2)=A(126)
      DWIDTH(2)=A(127)
      Q1(2)=-A(95)**2-A(13)**2+A(126)**2-2*(-P2)
      DMASS(1)=A(126)
      DWIDTH(1)=A(127)
      Q1(1)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
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

      SUBROUTINE D357
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U357/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(89)**2-A(151)**2+A(99)**2-2*(-P4)
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(89)**2-A(151)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(89)**2-A(151)**2+A(97)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(89)**2-A(151)**2+A(97)**2-2*(-P2)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(89)**2-A(151)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(89)**2-A(151)**2+A(95)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(89)**2-A(151)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(89)**2-A(151)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D358
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U358/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(89)**2-A(87)**2+A(99)**2-2*(-P4)
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(89)**2-A(151)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(89)**2-A(87)**2+A(97)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(89)**2-A(151)**2+A(97)**2-2*(-P2)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(89)**2-A(87)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(89)**2-A(151)**2+A(95)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(89)**2-A(87)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(89)**2-A(151)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D359
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U359/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(89)**2-A(87)**2+A(99)**2-2*(-P4)
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(89)**2-A(87)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(89)**2-A(87)**2+A(97)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(89)**2-A(87)**2+A(97)**2-2*(-P2)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(89)**2-A(87)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(89)**2-A(87)**2+A(95)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(89)**2-A(87)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(89)**2-A(87)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D35
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U35/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(137)
      DWIDTH(3)=A(111)
      Q1(3)=-A(93)**2+A(137)**2-2*(-P2)
      DMASS(2)=A(137)
      DWIDTH(2)=A(111)
      Q1(2)=-A(93)**2+A(137)**2-2*(-P4)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(93)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D360
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U360/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D361
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U361/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(6)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D362
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U362/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D363
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U363/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D364
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U364/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(7)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D365
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U365/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(91)
      DWIDTH(6)=A(92)
      Q1(6)=-A(89)**2-A(6)**2+A(91)**2-2*(-P4)
      DMASS(5)=A(91)
      DWIDTH(5)=A(92)
      Q1(5)=-A(89)**2-A(6)**2+A(91)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(6)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D366
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U366/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(91)
      DWIDTH(6)=A(92)
      Q1(6)=-A(89)**2-A(82)**2+A(91)**2-2*(-P4)
      DMASS(5)=A(91)
      DWIDTH(5)=A(92)
      Q1(5)=-A(89)**2-A(6)**2+A(91)**2-2*(-P2)
      DMASS(4)=A(7)
      DWIDTH(4)=A(86)
      Q1(4)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(3)=A(89)
      DWIDTH(3)=A(90)
      Q1(3)=-A(82)**2-2*(-P4)
      DMASS(2)=A(6)
      DWIDTH(2)=A(15)
      Q1(2)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D367
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U367/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(91)
      DWIDTH(6)=A(92)
      Q1(6)=-A(89)**2-A(84)**2+A(91)**2-2*(-P4)
      DMASS(5)=A(91)
      DWIDTH(5)=A(92)
      Q1(5)=-A(89)**2-A(6)**2+A(91)**2-2*(-P2)
      DMASS(4)=A(7)
      DWIDTH(4)=A(86)
      Q1(4)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(3)=A(89)
      DWIDTH(3)=A(90)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(6)
      DWIDTH(2)=A(15)
      Q1(2)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D368
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U368/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(91)
      DWIDTH(6)=A(92)
      Q1(6)=-A(89)**2-A(7)**2+A(91)**2-2*(-P4)
      DMASS(5)=A(91)
      DWIDTH(5)=A(92)
      Q1(5)=-A(89)**2-A(6)**2+A(91)**2-2*(-P2)
      DMASS(4)=A(89)
      DWIDTH(4)=A(90)
      Q1(4)=-A(7)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D369
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U369/ Q0(7),Q1(8),Q2(8)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(89)**2-A(151)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(89)**2-A(151)**2+A(97)**2-2*(-P2)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(89)**2-A(151)**2+A(95)**2-2*(-P2)
      DMASS(4)=A(93)
      DWIDTH(4)=A(94)
      Q1(4)=-A(89)**2-A(151)**2+A(93)**2-2*(-P2)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      Q1(8)=-A(89)**2-A(89)**2-2*(+P1)
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

      SUBROUTINE D36
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U36/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(112)
      DWIDTH(3)=A(113)
      Q1(3)=-A(93)**2+A(112)**2-2*(-P2)
      DMASS(2)=A(112)
      DWIDTH(2)=A(113)
      Q1(2)=-A(93)**2+A(112)**2-2*(-P4)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(93)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D370
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U370/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(7)
      DWIDTH(7)=A(86)
      Q1(7)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(6)=A(99)
      DWIDTH(6)=A(100)
      Q1(6)=-A(89)**2-A(151)**2+A(99)**2-2*(-P2)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(89)**2-A(151)**2+A(97)**2-2*(-P2)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(89)**2-A(151)**2+A(95)**2-2*(-P2)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(89)**2-A(151)**2+A(93)**2-2*(-P2)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D371
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U371/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(7)
      DWIDTH(7)=A(86)
      Q1(7)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(6)=A(99)
      DWIDTH(6)=A(100)
      Q1(6)=-A(89)**2-A(87)**2+A(99)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(89)**2-A(87)**2+A(97)**2-2*(-P4)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(89)**2-A(87)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(89)**2-A(87)**2+A(93)**2-2*(-P4)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D372
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U372/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(132)
      DWIDTH(2)=A(102)
      Q1(2)=-A(89)**2+A(132)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D373
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U373/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(135)
      DWIDTH(3)=A(105)
      Q1(3)=-A(89)**2+A(135)**2-2*(-P2)
      DMASS(2)=A(134)
      DWIDTH(2)=A(104)
      Q1(2)=-A(89)**2+A(134)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D374
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U374/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(108)
      DWIDTH(3)=A(109)
      Q1(3)=-A(89)**2+A(108)**2-2*(-P2)
      DMASS(2)=A(106)
      DWIDTH(2)=A(107)
      Q1(2)=-A(89)**2+A(106)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D375
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U375/ Q0(2),Q1(3),Q2(3)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(136)
      DWIDTH(2)=A(110)
      Q1(2)=-A(89)**2+A(136)**2-2*(-P4)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      Q1(3)=-A(89)**2-A(89)**2-2*(+P1)
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

      SUBROUTINE D376
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U376/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(7)
      DWIDTH(5)=A(86)
      Q1(5)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(4)=A(137)
      DWIDTH(4)=A(111)
      Q1(4)=-A(89)**2-A(8)**2+A(137)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      Q1(6)=-A(89)**2-A(89)**2-2*(+P1)
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

      SUBROUTINE D377
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U377/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(7)
      DWIDTH(5)=A(86)
      Q1(5)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(4)=A(112)
      DWIDTH(4)=A(113)
      Q1(4)=-A(89)**2-A(9)**2+A(112)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      Q1(6)=-A(89)**2-A(89)**2-2*(+P1)
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

      SUBROUTINE D378
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U378/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(89)**2+A(128)**2-2*(-P2)
      DMASS(5)=A(126)
      DWIDTH(5)=A(127)
      Q1(5)=-A(89)**2+A(126)**2-2*(-P2)
      DMASS(4)=A(145)
      DWIDTH(4)=A(121)
      Q1(4)=-A(89)**2+A(145)**2-2*(-P2)
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(89)**2+A(144)**2-2*(-P2)
      DMASS(2)=A(140)
      DWIDTH(2)=A(116)
      Q1(2)=-A(89)**2+A(140)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      Q1(7)=-A(89)**2-A(89)**2-2*(+P1)
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

      SUBROUTINE D379
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U379/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(89)**2+A(128)**2-2*(-P2)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(89)**2+A(126)**2-2*(-P2)
      DMASS(3)=A(145)
      DWIDTH(3)=A(121)
      Q1(3)=-A(89)**2+A(145)**2-2*(-P2)
      DMASS(2)=A(144)
      DWIDTH(2)=A(120)
      Q1(2)=-A(89)**2+A(144)**2-2*(-P2)
      DMASS(1)=A(140)
      DWIDTH(1)=A(116)
      Q1(1)=-A(89)**2+A(140)**2-2*(-P2)
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

      SUBROUTINE D37
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U37/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(133)
      DWIDTH(5)=A(103)
      Q1(5)=-A(93)**2+A(133)**2-2*(-P2)
      DMASS(4)=A(133)
      DWIDTH(4)=A(103)
      Q1(4)=-A(93)**2+A(133)**2-2*(-P4)
      DMASS(3)=A(132)
      DWIDTH(3)=A(102)
      Q1(3)=-A(93)**2+A(132)**2-2*(-P2)
      DMASS(2)=A(132)
      DWIDTH(2)=A(102)
      Q1(2)=-A(93)**2+A(132)**2-2*(-P4)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(93)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D380
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U380/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(89)**2+A(128)**2-2*(-P2)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(89)**2+A(126)**2-2*(-P2)
      DMASS(3)=A(145)
      DWIDTH(3)=A(121)
      Q1(3)=-A(89)**2+A(145)**2-2*(-P2)
      DMASS(2)=A(144)
      DWIDTH(2)=A(120)
      Q1(2)=-A(89)**2+A(144)**2-2*(-P2)
      DMASS(1)=A(140)
      DWIDTH(1)=A(116)
      Q1(1)=-A(89)**2+A(140)**2-2*(-P2)
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

      SUBROUTINE D381
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U381/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(89)**2-A(10)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(89)**2-A(10)**2+A(126)**2-2*(-P4)
      DMASS(3)=A(145)
      DWIDTH(3)=A(121)
      Q1(3)=-A(89)**2-A(10)**2+A(145)**2-2*(-P4)
      DMASS(2)=A(144)
      DWIDTH(2)=A(120)
      Q1(2)=-A(89)**2-A(10)**2+A(144)**2-2*(-P4)
      DMASS(1)=A(140)
      DWIDTH(1)=A(116)
      Q1(1)=-A(89)**2-A(10)**2+A(140)**2-2*(-P4)
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

      SUBROUTINE D382
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U382/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(89)**2-A(12)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(89)**2-A(12)**2+A(126)**2-2*(-P4)
      DMASS(3)=A(145)
      DWIDTH(3)=A(121)
      Q1(3)=-A(89)**2-A(12)**2+A(145)**2-2*(-P4)
      DMASS(2)=A(144)
      DWIDTH(2)=A(120)
      Q1(2)=-A(89)**2-A(12)**2+A(144)**2-2*(-P4)
      DMASS(1)=A(140)
      DWIDTH(1)=A(116)
      Q1(1)=-A(89)**2-A(12)**2+A(140)**2-2*(-P4)
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

      SUBROUTINE D383
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U383/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(124)
      DWIDTH(6)=A(125)
      Q1(6)=-A(89)**2+A(124)**2-2*(-P4)
      DMASS(5)=A(122)
      DWIDTH(5)=A(123)
      Q1(5)=-A(89)**2+A(122)**2-2*(-P4)
      DMASS(4)=A(143)
      DWIDTH(4)=A(119)
      Q1(4)=-A(89)**2+A(143)**2-2*(-P4)
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(89)**2+A(142)**2-2*(-P4)
      DMASS(2)=A(138)
      DWIDTH(2)=A(114)
      Q1(2)=-A(89)**2+A(138)**2-2*(-P4)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      Q1(7)=-A(89)**2-A(89)**2-2*(+P1)
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

      SUBROUTINE D384
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U384/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(124)
      DWIDTH(5)=A(125)
      Q1(5)=-A(89)**2-A(11)**2+A(124)**2-2*(-P4)
      DMASS(4)=A(122)
      DWIDTH(4)=A(123)
      Q1(4)=-A(89)**2-A(11)**2+A(122)**2-2*(-P4)
      DMASS(3)=A(143)
      DWIDTH(3)=A(119)
      Q1(3)=-A(89)**2-A(11)**2+A(143)**2-2*(-P4)
      DMASS(2)=A(142)
      DWIDTH(2)=A(118)
      Q1(2)=-A(89)**2-A(11)**2+A(142)**2-2*(-P4)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(89)**2-A(11)**2+A(138)**2-2*(-P4)
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

      SUBROUTINE D385
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U385/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(124)
      DWIDTH(5)=A(125)
      Q1(5)=-A(89)**2-A(13)**2+A(124)**2-2*(-P4)
      DMASS(4)=A(122)
      DWIDTH(4)=A(123)
      Q1(4)=-A(89)**2-A(13)**2+A(122)**2-2*(-P4)
      DMASS(3)=A(143)
      DWIDTH(3)=A(119)
      Q1(3)=-A(89)**2-A(13)**2+A(143)**2-2*(-P4)
      DMASS(2)=A(142)
      DWIDTH(2)=A(118)
      Q1(2)=-A(89)**2-A(13)**2+A(142)**2-2*(-P4)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(89)**2-A(13)**2+A(138)**2-2*(-P4)
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

      SUBROUTINE D386
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U386/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(124)
      DWIDTH(5)=A(125)
      Q1(5)=-A(89)**2+A(124)**2-2*(-P2)
      DMASS(4)=A(122)
      DWIDTH(4)=A(123)
      Q1(4)=-A(89)**2+A(122)**2-2*(-P2)
      DMASS(3)=A(143)
      DWIDTH(3)=A(119)
      Q1(3)=-A(89)**2+A(143)**2-2*(-P2)
      DMASS(2)=A(142)
      DWIDTH(2)=A(118)
      Q1(2)=-A(89)**2+A(142)**2-2*(-P2)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(89)**2+A(138)**2-2*(-P2)
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

      SUBROUTINE D387
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U387/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(124)
      DWIDTH(5)=A(125)
      Q1(5)=-A(89)**2+A(124)**2-2*(-P2)
      DMASS(4)=A(122)
      DWIDTH(4)=A(123)
      Q1(4)=-A(89)**2+A(122)**2-2*(-P2)
      DMASS(3)=A(143)
      DWIDTH(3)=A(119)
      Q1(3)=-A(89)**2+A(143)**2-2*(-P2)
      DMASS(2)=A(142)
      DWIDTH(2)=A(118)
      Q1(2)=-A(89)**2+A(142)**2-2*(-P2)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(89)**2+A(138)**2-2*(-P2)
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

      SUBROUTINE D388
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U388/ Q0(9),Q1(10),Q2(10)
      DIMENSION DMASS(9),DWIDTH(9)
      SAVE
      DMASS(9)=A(7)
      DWIDTH(9)=A(86)
      Q1(9)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(8)=A(128)
      DWIDTH(8)=A(129)
      Q1(8)=-A(89)**2-A(10)**2+A(128)**2-2*(-P2)
      DMASS(7)=A(126)
      DWIDTH(7)=A(127)
      Q1(7)=-A(89)**2-A(10)**2+A(126)**2-2*(-P2)
      DMASS(6)=A(145)
      DWIDTH(6)=A(121)
      Q1(6)=-A(89)**2-A(10)**2+A(145)**2-2*(-P2)
      DMASS(5)=A(144)
      DWIDTH(5)=A(120)
      Q1(5)=-A(89)**2-A(10)**2+A(144)**2-2*(-P2)
      DMASS(4)=A(140)
      DWIDTH(4)=A(116)
      Q1(4)=-A(89)**2-A(10)**2+A(140)**2-2*(-P2)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      Q1(10)=-A(89)**2-A(89)**2-2*(+P1)
      DO 2 I=1,9
      IF (RWIDTH) THEN
        Q2(I)=1/(Q1(I)**2+((DMASS(I)-Q1(I)/DMASS(I))*DWIDTH(I))**2)
      ELSE
        Q2(I)=1/(Q1(I)**2+(DMASS(I)*DWIDTH(I))**2)
      ENDIF
      IF (GWIDTH) THEN
        Q0(I)=Q2(I)*Q1(I)**2
      ELSE
        Q0(I)=1 
      ENDIF
        Q1(I)=Q2(I)*Q1(I)
2     CONTINUE
      DO 3 I=10,10
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D389
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U389/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(89)**2-A(10)**2+A(128)**2-2*(-P2)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(89)**2-A(10)**2+A(126)**2-2*(-P2)
      DMASS(3)=A(145)
      DWIDTH(3)=A(121)
      Q1(3)=-A(89)**2-A(10)**2+A(145)**2-2*(-P2)
      DMASS(2)=A(144)
      DWIDTH(2)=A(120)
      Q1(2)=-A(89)**2-A(10)**2+A(144)**2-2*(-P2)
      DMASS(1)=A(140)
      DWIDTH(1)=A(116)
      Q1(1)=-A(89)**2-A(10)**2+A(140)**2-2*(-P2)
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

      SUBROUTINE D38
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U38/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(84)
      DWIDTH(8)=A(85)
      Q1(8)=-A(93)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(7)=A(82)
      DWIDTH(7)=A(83)
      Q1(7)=-A(93)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(6)=A(135)
      DWIDTH(6)=A(105)
      Q1(6)=-A(93)**2-A(8)**2+A(135)**2-2*(-P2)
      DMASS(5)=A(135)
      DWIDTH(5)=A(105)
      Q1(5)=-A(93)**2-A(8)**2+A(135)**2-2*(-P4)
      DMASS(4)=A(134)
      DWIDTH(4)=A(104)
      Q1(4)=-A(93)**2-A(8)**2+A(134)**2-2*(-P2)
      DMASS(3)=A(134)
      DWIDTH(3)=A(104)
      Q1(3)=-A(93)**2-A(8)**2+A(134)**2-2*(-P4)
      DMASS(2)=A(7)
      DWIDTH(2)=A(86)
      Q1(2)=-A(93)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(93)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D390
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U390/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(89)**2-A(12)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(89)**2-A(12)**2+A(126)**2-2*(-P4)
      DMASS(3)=A(145)
      DWIDTH(3)=A(121)
      Q1(3)=-A(89)**2-A(12)**2+A(145)**2-2*(-P4)
      DMASS(2)=A(144)
      DWIDTH(2)=A(120)
      Q1(2)=-A(89)**2-A(12)**2+A(144)**2-2*(-P4)
      DMASS(1)=A(140)
      DWIDTH(1)=A(116)
      Q1(1)=-A(89)**2-A(12)**2+A(140)**2-2*(-P4)
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

      SUBROUTINE D391
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U391/ Q0(9),Q1(10),Q2(10)
      DIMENSION DMASS(9),DWIDTH(9)
      SAVE
      DMASS(9)=A(7)
      DWIDTH(9)=A(86)
      Q1(9)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(8)=A(124)
      DWIDTH(8)=A(125)
      Q1(8)=-A(89)**2-A(11)**2+A(124)**2-2*(-P4)
      DMASS(7)=A(122)
      DWIDTH(7)=A(123)
      Q1(7)=-A(89)**2-A(11)**2+A(122)**2-2*(-P4)
      DMASS(6)=A(143)
      DWIDTH(6)=A(119)
      Q1(6)=-A(89)**2-A(11)**2+A(143)**2-2*(-P4)
      DMASS(5)=A(142)
      DWIDTH(5)=A(118)
      Q1(5)=-A(89)**2-A(11)**2+A(142)**2-2*(-P4)
      DMASS(4)=A(138)
      DWIDTH(4)=A(114)
      Q1(4)=-A(89)**2-A(11)**2+A(138)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      Q1(10)=-A(89)**2-A(89)**2-2*(+P1)
      DO 2 I=1,9
      IF (RWIDTH) THEN
        Q2(I)=1/(Q1(I)**2+((DMASS(I)-Q1(I)/DMASS(I))*DWIDTH(I))**2)
      ELSE
        Q2(I)=1/(Q1(I)**2+(DMASS(I)*DWIDTH(I))**2)
      ENDIF
      IF (GWIDTH) THEN
        Q0(I)=Q2(I)*Q1(I)**2
      ELSE
        Q0(I)=1 
      ENDIF
        Q1(I)=Q2(I)*Q1(I)
2     CONTINUE
      DO 3 I=10,10
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D392
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U392/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(124)
      DWIDTH(5)=A(125)
      Q1(5)=-A(89)**2-A(13)**2+A(124)**2-2*(-P4)
      DMASS(4)=A(122)
      DWIDTH(4)=A(123)
      Q1(4)=-A(89)**2-A(13)**2+A(122)**2-2*(-P4)
      DMASS(3)=A(143)
      DWIDTH(3)=A(119)
      Q1(3)=-A(89)**2-A(13)**2+A(143)**2-2*(-P4)
      DMASS(2)=A(142)
      DWIDTH(2)=A(118)
      Q1(2)=-A(89)**2-A(13)**2+A(142)**2-2*(-P4)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(89)**2-A(13)**2+A(138)**2-2*(-P4)
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

      SUBROUTINE D393
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U393/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(124)
      DWIDTH(5)=A(125)
      Q1(5)=-A(89)**2-A(11)**2+A(124)**2-2*(-P2)
      DMASS(4)=A(122)
      DWIDTH(4)=A(123)
      Q1(4)=-A(89)**2-A(11)**2+A(122)**2-2*(-P2)
      DMASS(3)=A(143)
      DWIDTH(3)=A(119)
      Q1(3)=-A(89)**2-A(11)**2+A(143)**2-2*(-P2)
      DMASS(2)=A(142)
      DWIDTH(2)=A(118)
      Q1(2)=-A(89)**2-A(11)**2+A(142)**2-2*(-P2)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(89)**2-A(11)**2+A(138)**2-2*(-P2)
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

      SUBROUTINE D394
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U394/ Q0(9),Q1(10),Q2(10)
      DIMENSION DMASS(9),DWIDTH(9)
      SAVE
      DMASS(9)=A(7)
      DWIDTH(9)=A(86)
      Q1(9)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(8)=A(128)
      DWIDTH(8)=A(129)
      Q1(8)=-A(89)**2-A(12)**2+A(128)**2-2*(-P2)
      DMASS(7)=A(126)
      DWIDTH(7)=A(127)
      Q1(7)=-A(89)**2-A(12)**2+A(126)**2-2*(-P2)
      DMASS(6)=A(145)
      DWIDTH(6)=A(121)
      Q1(6)=-A(89)**2-A(12)**2+A(145)**2-2*(-P2)
      DMASS(5)=A(144)
      DWIDTH(5)=A(120)
      Q1(5)=-A(89)**2-A(12)**2+A(144)**2-2*(-P2)
      DMASS(4)=A(140)
      DWIDTH(4)=A(116)
      Q1(4)=-A(89)**2-A(12)**2+A(140)**2-2*(-P2)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      Q1(10)=-A(89)**2-A(89)**2-2*(+P1)
      DO 2 I=1,9
      IF (RWIDTH) THEN
        Q2(I)=1/(Q1(I)**2+((DMASS(I)-Q1(I)/DMASS(I))*DWIDTH(I))**2)
      ELSE
        Q2(I)=1/(Q1(I)**2+(DMASS(I)*DWIDTH(I))**2)
      ENDIF
      IF (GWIDTH) THEN
        Q0(I)=Q2(I)*Q1(I)**2
      ELSE
        Q0(I)=1 
      ENDIF
        Q1(I)=Q2(I)*Q1(I)
2     CONTINUE
      DO 3 I=10,10
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D395
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U395/ Q0(9),Q1(10),Q2(10)
      DIMENSION DMASS(9),DWIDTH(9)
      SAVE
      DMASS(9)=A(7)
      DWIDTH(9)=A(86)
      Q1(9)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(8)=A(124)
      DWIDTH(8)=A(125)
      Q1(8)=-A(89)**2-A(13)**2+A(124)**2-2*(-P4)
      DMASS(7)=A(122)
      DWIDTH(7)=A(123)
      Q1(7)=-A(89)**2-A(13)**2+A(122)**2-2*(-P4)
      DMASS(6)=A(143)
      DWIDTH(6)=A(119)
      Q1(6)=-A(89)**2-A(13)**2+A(143)**2-2*(-P4)
      DMASS(5)=A(142)
      DWIDTH(5)=A(118)
      Q1(5)=-A(89)**2-A(13)**2+A(142)**2-2*(-P4)
      DMASS(4)=A(138)
      DWIDTH(4)=A(114)
      Q1(4)=-A(89)**2-A(13)**2+A(138)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
      Q1(10)=-A(89)**2-A(89)**2-2*(+P1)
      DO 2 I=1,9
      IF (RWIDTH) THEN
        Q2(I)=1/(Q1(I)**2+((DMASS(I)-Q1(I)/DMASS(I))*DWIDTH(I))**2)
      ELSE
        Q2(I)=1/(Q1(I)**2+(DMASS(I)*DWIDTH(I))**2)
      ENDIF
      IF (GWIDTH) THEN
        Q0(I)=Q2(I)*Q1(I)**2
      ELSE
        Q0(I)=1 
      ENDIF
        Q1(I)=Q2(I)*Q1(I)
2     CONTINUE
      DO 3 I=10,10
        Q1(I)=1/Q1(I)
        Q2(I)=Q1(I)**2
3     CONTINUE
      RETURN
      END

      SUBROUTINE D396
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U396/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(91)
      DWIDTH(6)=A(92)
      Q1(6)=-A(89)**2-A(82)**2+A(91)**2-2*(-P4)
      DMASS(5)=A(91)
      DWIDTH(5)=A(92)
      Q1(5)=-A(89)**2-A(82)**2+A(91)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D397
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U397/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(91)
      DWIDTH(6)=A(92)
      Q1(6)=-A(89)**2-A(84)**2+A(91)**2-2*(-P4)
      DMASS(5)=A(91)
      DWIDTH(5)=A(92)
      Q1(5)=-A(89)**2-A(82)**2+A(91)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(89)
      DWIDTH(3)=A(90)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
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

      SUBROUTINE D398
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U398/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(91)
      DWIDTH(6)=A(92)
      Q1(6)=-A(89)**2-A(7)**2+A(91)**2-2*(-P4)
      DMASS(5)=A(91)
      DWIDTH(5)=A(92)
      Q1(5)=-A(89)**2-A(82)**2+A(91)**2-2*(-P2)
      DMASS(4)=A(7)
      DWIDTH(4)=A(86)
      Q1(4)=-A(89)**2-A(89)**2+A(7)**2-2*(+P1)
      DMASS(3)=A(89)
      DWIDTH(3)=A(90)
      Q1(3)=-A(7)**2-2*(-P4)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(82)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(89)**2-A(89)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D399
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U399/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(91)
      DWIDTH(6)=A(92)
      Q1(6)=-A(89)**2-A(84)**2+A(91)**2-2*(-P2)
      DMASS(5)=A(89)
      DWIDTH(5)=A(90)
      Q1(5)=-A(84)**2-2*(-P2)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(89)**2-A(84)**2+A(91)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(89)**2-A(89)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(89)**2-A(89)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D39
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U39/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(84)
      DWIDTH(8)=A(85)
      Q1(8)=-A(93)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(7)=A(82)
      DWIDTH(7)=A(83)
      Q1(7)=-A(93)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(6)=A(108)
      DWIDTH(6)=A(109)
      Q1(6)=-A(93)**2-A(9)**2+A(108)**2-2*(-P2)
      DMASS(5)=A(108)
      DWIDTH(5)=A(109)
      Q1(5)=-A(93)**2-A(9)**2+A(108)**2-2*(-P4)
      DMASS(4)=A(106)
      DWIDTH(4)=A(107)
      Q1(4)=-A(93)**2-A(9)**2+A(106)**2-2*(-P2)
      DMASS(3)=A(106)
      DWIDTH(3)=A(107)
      Q1(3)=-A(93)**2-A(9)**2+A(106)**2-2*(-P4)
      DMASS(2)=A(7)
      DWIDTH(2)=A(86)
      Q1(2)=-A(93)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(93)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D3
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U3/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(93)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(93)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(93)**2-A(84)**2+A(95)**2-2*(-P4)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(93)**2-A(6)**2+A(95)**2-2*(-P2)
      DMASS(4)=A(7)
      DWIDTH(4)=A(86)
      Q1(4)=-A(93)**2-A(93)**2+A(7)**2-2*(+P1)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(6)
      DWIDTH(2)=A(15)
      Q1(2)=-A(93)**2-A(93)**2+A(6)**2-2*(+P1)
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

