      SUBROUTINE D200
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U200/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(137)
      DWIDTH(3)=A(111)
      Q1(3)=-A(95)**2+A(137)**2-2*(-P2)
      DMASS(2)=A(137)
      DWIDTH(2)=A(111)
      Q1(2)=-A(95)**2+A(137)**2-2*(-P4)
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

      SUBROUTINE D201
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U201/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(112)
      DWIDTH(3)=A(113)
      Q1(3)=-A(95)**2+A(112)**2-2*(-P2)
      DMASS(2)=A(112)
      DWIDTH(2)=A(113)
      Q1(2)=-A(95)**2+A(112)**2-2*(-P4)
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

      SUBROUTINE D202
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U202/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(133)
      DWIDTH(5)=A(103)
      Q1(5)=-A(95)**2+A(133)**2-2*(-P2)
      DMASS(4)=A(133)
      DWIDTH(4)=A(103)
      Q1(4)=-A(95)**2+A(133)**2-2*(-P4)
      DMASS(3)=A(132)
      DWIDTH(3)=A(102)
      Q1(3)=-A(95)**2+A(132)**2-2*(-P2)
      DMASS(2)=A(132)
      DWIDTH(2)=A(102)
      Q1(2)=-A(95)**2+A(132)**2-2*(-P4)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D203
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U203/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(84)
      DWIDTH(8)=A(85)
      Q1(8)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(7)=A(82)
      DWIDTH(7)=A(83)
      Q1(7)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(6)=A(135)
      DWIDTH(6)=A(105)
      Q1(6)=-A(95)**2-A(8)**2+A(135)**2-2*(-P2)
      DMASS(5)=A(135)
      DWIDTH(5)=A(105)
      Q1(5)=-A(95)**2-A(8)**2+A(135)**2-2*(-P4)
      DMASS(4)=A(134)
      DWIDTH(4)=A(104)
      Q1(4)=-A(95)**2-A(8)**2+A(134)**2-2*(-P2)
      DMASS(3)=A(134)
      DWIDTH(3)=A(104)
      Q1(3)=-A(95)**2-A(8)**2+A(134)**2-2*(-P4)
      DMASS(2)=A(7)
      DWIDTH(2)=A(86)
      Q1(2)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D204
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U204/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(84)
      DWIDTH(8)=A(85)
      Q1(8)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(7)=A(82)
      DWIDTH(7)=A(83)
      Q1(7)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(6)=A(108)
      DWIDTH(6)=A(109)
      Q1(6)=-A(95)**2-A(9)**2+A(108)**2-2*(-P2)
      DMASS(5)=A(108)
      DWIDTH(5)=A(109)
      Q1(5)=-A(95)**2-A(9)**2+A(108)**2-2*(-P4)
      DMASS(4)=A(106)
      DWIDTH(4)=A(107)
      Q1(4)=-A(95)**2-A(9)**2+A(106)**2-2*(-P2)
      DMASS(3)=A(106)
      DWIDTH(3)=A(107)
      Q1(3)=-A(95)**2-A(9)**2+A(106)**2-2*(-P4)
      DMASS(2)=A(7)
      DWIDTH(2)=A(86)
      Q1(2)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D205
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U205/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(139)
      DWIDTH(5)=A(115)
      Q1(5)=-A(95)**2+A(139)**2-2*(-P2)
      DMASS(4)=A(139)
      DWIDTH(4)=A(115)
      Q1(4)=-A(95)**2+A(139)**2-2*(-P4)
      DMASS(3)=A(138)
      DWIDTH(3)=A(114)
      Q1(3)=-A(95)**2+A(138)**2-2*(-P2)
      DMASS(2)=A(138)
      DWIDTH(2)=A(114)
      Q1(2)=-A(95)**2+A(138)**2-2*(-P4)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D206
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U206/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(141)
      DWIDTH(5)=A(117)
      Q1(5)=-A(95)**2+A(141)**2-2*(-P2)
      DMASS(4)=A(141)
      DWIDTH(4)=A(117)
      Q1(4)=-A(95)**2+A(141)**2-2*(-P4)
      DMASS(3)=A(140)
      DWIDTH(3)=A(116)
      Q1(3)=-A(95)**2+A(140)**2-2*(-P2)
      DMASS(2)=A(140)
      DWIDTH(2)=A(116)
      Q1(2)=-A(95)**2+A(140)**2-2*(-P4)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D207
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U207/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(84)
      DWIDTH(8)=A(85)
      Q1(8)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(7)=A(82)
      DWIDTH(7)=A(83)
      Q1(7)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(6)=A(143)
      DWIDTH(6)=A(119)
      Q1(6)=-A(95)**2-A(10)**2+A(143)**2-2*(-P2)
      DMASS(5)=A(143)
      DWIDTH(5)=A(119)
      Q1(5)=-A(95)**2-A(10)**2+A(143)**2-2*(-P4)
      DMASS(4)=A(142)
      DWIDTH(4)=A(118)
      Q1(4)=-A(95)**2-A(10)**2+A(142)**2-2*(-P2)
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(95)**2-A(10)**2+A(142)**2-2*(-P4)
      DMASS(2)=A(7)
      DWIDTH(2)=A(86)
      Q1(2)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D208
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U208/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(84)
      DWIDTH(8)=A(85)
      Q1(8)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(7)=A(82)
      DWIDTH(7)=A(83)
      Q1(7)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(6)=A(145)
      DWIDTH(6)=A(121)
      Q1(6)=-A(95)**2-A(11)**2+A(145)**2-2*(-P2)
      DMASS(5)=A(145)
      DWIDTH(5)=A(121)
      Q1(5)=-A(95)**2-A(11)**2+A(145)**2-2*(-P4)
      DMASS(4)=A(144)
      DWIDTH(4)=A(120)
      Q1(4)=-A(95)**2-A(11)**2+A(144)**2-2*(-P2)
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(95)**2-A(11)**2+A(144)**2-2*(-P4)
      DMASS(2)=A(7)
      DWIDTH(2)=A(86)
      Q1(2)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D209
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U209/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(84)
      DWIDTH(8)=A(85)
      Q1(8)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(7)=A(82)
      DWIDTH(7)=A(83)
      Q1(7)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(6)=A(124)
      DWIDTH(6)=A(125)
      Q1(6)=-A(95)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(5)=A(124)
      DWIDTH(5)=A(125)
      Q1(5)=-A(95)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(4)=A(122)
      DWIDTH(4)=A(123)
      Q1(4)=-A(95)**2-A(12)**2+A(122)**2-2*(-P2)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(95)**2-A(12)**2+A(122)**2-2*(-P4)
      DMASS(2)=A(7)
      DWIDTH(2)=A(86)
      Q1(2)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D20
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U20/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(93)**2-A(82)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(93)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(82)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(93)**2-A(82)**2+A(95)**2-2*(-P4)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(93)**2-A(82)**2+A(95)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(93)**2-A(93)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(93)**2-A(93)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(82)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(82)**2-2*(-P2)
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

      SUBROUTINE D210
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U210/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(84)
      DWIDTH(8)=A(85)
      Q1(8)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(7)=A(82)
      DWIDTH(7)=A(83)
      Q1(7)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(95)**2-A(13)**2+A(128)**2-2*(-P2)
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(95)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(95)**2-A(13)**2+A(126)**2-2*(-P2)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
      DMASS(2)=A(7)
      DWIDTH(2)=A(86)
      Q1(2)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D211
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U211/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(95)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(95)**2-A(82)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(95)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(82)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(82)**2-2*(-P4)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(82)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(95)**2-A(82)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(82)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D212
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U212/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(95)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(95)**2-A(82)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(95)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(82)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(84)**2-2*(-P4)
      DMASS(4)=A(82)
      DWIDTH(4)=A(83)
      Q1(4)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(82)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(95)**2-A(84)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(82)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D213
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U213/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(95)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(95)**2-A(82)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(95)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(82)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(7)
      DWIDTH(6)=A(86)
      Q1(6)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(7)**2-2*(-P4)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(82)**2-2*(-P2)
      DMASS(3)=A(6)
      DWIDTH(3)=A(15)
      Q1(3)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(95)**2-A(7)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(82)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D214
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U214/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(95)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(95)**2-A(84)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(95)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(84)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(84)**2-2*(-P4)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(84)**2-2*(-P2)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(95)**2-A(84)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(84)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D215
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U215/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(95)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(95)**2-A(84)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(95)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(84)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(7)
      DWIDTH(6)=A(86)
      Q1(6)=-A(95)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(7)**2-2*(-P4)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(84)**2-2*(-P2)
      DMASS(3)=A(6)
      DWIDTH(3)=A(15)
      Q1(3)=-A(95)**2-A(95)**2+A(6)**2-2*(+P1)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(95)**2-A(7)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(84)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D216
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U216/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(95)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(95)**2-A(7)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(95)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(7)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(7)**2-2*(-P4)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(7)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(95)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(82)
      DWIDTH(3)=A(83)
      Q1(3)=-A(95)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(95)**2-A(7)**2+A(93)**2-2*(-P4)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(95)**2-A(7)**2+A(93)**2-2*(-P2)
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

      SUBROUTINE D217
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U217/ Q0(7),Q1(7),Q2(7)
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
      Q1(4)=-A(95)**2-A(87)**2+A(91)**2-2*(-P2)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(95)**2-A(87)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D218
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U218/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D219
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U219/ Q0(2),Q1(2),Q2(2)
      DIMENSION DMASS(2),DWIDTH(2)
      SAVE
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
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

      SUBROUTINE D21
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U21/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(93)**2-A(82)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(93)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(82)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(93)**2-A(84)**2+A(95)**2-2*(-P4)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(93)**2-A(82)**2+A(95)**2-2*(-P2)
      DMASS(4)=A(84)
      DWIDTH(4)=A(85)
      Q1(4)=-A(93)**2-A(93)**2+A(84)**2-2*(+P1)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(84)**2-2*(-P4)
      DMASS(2)=A(82)
      DWIDTH(2)=A(83)
      Q1(2)=-A(93)**2-A(93)**2+A(82)**2-2*(+P1)
      DMASS(1)=A(93)
      DWIDTH(1)=A(94)
      Q1(1)=-A(82)**2-2*(-P2)
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

      SUBROUTINE D220
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U220/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(95)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(95)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(6)**2-2*(-P2)
      DMASS(4)=A(93)
      DWIDTH(4)=A(94)
      Q1(4)=-A(95)**2-A(6)**2+A(93)**2-2*(-P2)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(95)**2-A(151)**2+A(91)**2-2*(-P4)
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D221
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U221/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(95)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(95)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(6)**2-2*(-P2)
      DMASS(4)=A(93)
      DWIDTH(4)=A(94)
      Q1(4)=-A(95)**2-A(6)**2+A(93)**2-2*(-P2)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
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

      SUBROUTINE D222
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U222/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(95)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(82)**2-2*(-P4)
      DMASS(5)=A(93)
      DWIDTH(5)=A(94)
      Q1(5)=-A(95)**2-A(82)**2+A(93)**2-2*(-P4)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(3)=A(87)
      DWIDTH(3)=A(88)
      Q1(3)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D223
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U223/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(95)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(84)**2-2*(-P4)
      DMASS(5)=A(93)
      DWIDTH(5)=A(94)
      Q1(5)=-A(95)**2-A(84)**2+A(93)**2-2*(-P4)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(3)=A(87)
      DWIDTH(3)=A(88)
      Q1(3)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(2)=A(151)
      DWIDTH(2)=A(16)
      Q1(2)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
      DMASS(1)=A(89)
      DWIDTH(1)=A(90)
      Q1(1)=-A(95)**2-A(151)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D224
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U224/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(95)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(95)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(7)**2-2*(-P4)
      DMASS(4)=A(93)
      DWIDTH(4)=A(94)
      Q1(4)=-A(95)**2-A(7)**2+A(93)**2-2*(-P4)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(95)**2-A(151)**2+A(91)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
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

      SUBROUTINE D225
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U225/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(136)
      DWIDTH(3)=A(110)
      Q1(3)=-A(95)**2+A(136)**2-2*(-P2)
      DMASS(2)=A(132)
      DWIDTH(2)=A(102)
      Q1(2)=-A(95)**2+A(132)**2-2*(-P4)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D226
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U226/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(137)
      DWIDTH(5)=A(111)
      Q1(5)=-A(95)**2+A(137)**2-2*(-P2)
      DMASS(4)=A(135)
      DWIDTH(4)=A(105)
      Q1(4)=-A(95)**2-A(8)**2+A(135)**2-2*(-P4)
      DMASS(3)=A(134)
      DWIDTH(3)=A(104)
      Q1(3)=-A(95)**2-A(8)**2+A(134)**2-2*(-P4)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D227
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U227/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(112)
      DWIDTH(5)=A(113)
      Q1(5)=-A(95)**2+A(112)**2-2*(-P2)
      DMASS(4)=A(108)
      DWIDTH(4)=A(109)
      Q1(4)=-A(95)**2-A(9)**2+A(108)**2-2*(-P4)
      DMASS(3)=A(106)
      DWIDTH(3)=A(107)
      Q1(3)=-A(95)**2-A(9)**2+A(106)**2-2*(-P4)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D228
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U228/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(140)
      DWIDTH(3)=A(116)
      Q1(3)=-A(95)**2+A(140)**2-2*(-P4)
      DMASS(2)=A(138)
      DWIDTH(2)=A(114)
      Q1(2)=-A(95)**2+A(138)**2-2*(-P2)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D229
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U229/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(145)
      DWIDTH(5)=A(121)
      Q1(5)=-A(95)**2-A(11)**2+A(145)**2-2*(-P4)
      DMASS(4)=A(144)
      DWIDTH(4)=A(120)
      Q1(4)=-A(95)**2-A(11)**2+A(144)**2-2*(-P4)
      DMASS(3)=A(138)
      DWIDTH(3)=A(114)
      Q1(3)=-A(95)**2+A(138)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D22
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U22/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(93)**2-A(82)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(93)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(82)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(93)**2-A(7)**2+A(95)**2-2*(-P4)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(93)**2-A(82)**2+A(95)**2-2*(-P2)
      DMASS(4)=A(7)
      DWIDTH(4)=A(86)
      Q1(4)=-A(93)**2-A(93)**2+A(7)**2-2*(+P1)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(7)**2-2*(-P4)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(82)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(93)**2-A(93)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D230
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U230/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(128)
      DWIDTH(5)=A(129)
      Q1(5)=-A(95)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(4)=A(126)
      DWIDTH(4)=A(127)
      Q1(4)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
      DMASS(3)=A(138)
      DWIDTH(3)=A(114)
      Q1(3)=-A(95)**2+A(138)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D231
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U231/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(143)
      DWIDTH(5)=A(119)
      Q1(5)=-A(95)**2-A(10)**2+A(143)**2-2*(-P4)
      DMASS(4)=A(142)
      DWIDTH(4)=A(118)
      Q1(4)=-A(95)**2-A(10)**2+A(142)**2-2*(-P4)
      DMASS(3)=A(140)
      DWIDTH(3)=A(116)
      Q1(3)=-A(95)**2+A(140)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D232
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U232/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(124)
      DWIDTH(5)=A(125)
      Q1(5)=-A(95)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(4)=A(122)
      DWIDTH(4)=A(123)
      Q1(4)=-A(95)**2-A(12)**2+A(122)**2-2*(-P4)
      DMASS(3)=A(140)
      DWIDTH(3)=A(116)
      Q1(3)=-A(95)**2+A(140)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D233
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U233/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(145)
      DWIDTH(6)=A(121)
      Q1(6)=-A(95)**2-A(11)**2+A(145)**2-2*(-P4)
      DMASS(5)=A(144)
      DWIDTH(5)=A(120)
      Q1(5)=-A(95)**2-A(11)**2+A(144)**2-2*(-P4)
      DMASS(4)=A(143)
      DWIDTH(4)=A(119)
      Q1(4)=-A(95)**2-A(10)**2+A(143)**2-2*(-P2)
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(95)**2-A(10)**2+A(142)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D234
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U234/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(95)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(5)=A(126)
      DWIDTH(5)=A(127)
      Q1(5)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
      DMASS(4)=A(143)
      DWIDTH(4)=A(119)
      Q1(4)=-A(95)**2-A(10)**2+A(143)**2-2*(-P2)
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(95)**2-A(10)**2+A(142)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D235
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U235/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(124)
      DWIDTH(6)=A(125)
      Q1(6)=-A(95)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(5)=A(122)
      DWIDTH(5)=A(123)
      Q1(5)=-A(95)**2-A(12)**2+A(122)**2-2*(-P4)
      DMASS(4)=A(145)
      DWIDTH(4)=A(121)
      Q1(4)=-A(95)**2-A(11)**2+A(145)**2-2*(-P2)
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(95)**2-A(11)**2+A(144)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D236
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U236/ Q0(6),Q1(6),Q2(6)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(128)
      DWIDTH(6)=A(129)
      Q1(6)=-A(95)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(5)=A(126)
      DWIDTH(5)=A(127)
      Q1(5)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(95)**2-A(12)**2+A(124)**2-2*(-P2)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(95)**2-A(12)**2+A(122)**2-2*(-P2)
      DMASS(2)=A(87)
      DWIDTH(2)=A(88)
      Q1(2)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D237
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U237/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(95)**2-A(82)**2+A(99)**2-2*(-P2)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(82)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(82)**2-2*(-P2)
      DMASS(5)=A(93)
      DWIDTH(5)=A(94)
      Q1(5)=-A(95)**2-A(82)**2+A(93)**2-2*(-P2)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(3)=A(87)
      DWIDTH(3)=A(88)
      Q1(3)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D238
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U238/ Q0(8),Q1(8),Q2(8)
      DIMENSION DMASS(8),DWIDTH(8)
      SAVE
      DMASS(8)=A(99)
      DWIDTH(8)=A(100)
      Q1(8)=-A(95)**2-A(84)**2+A(99)**2-2*(-P2)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(95)**2-A(84)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(84)**2-2*(-P2)
      DMASS(5)=A(93)
      DWIDTH(5)=A(94)
      Q1(5)=-A(95)**2-A(84)**2+A(93)**2-2*(-P2)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(3)=A(87)
      DWIDTH(3)=A(88)
      Q1(3)=-A(95)**2-A(89)**2+A(87)**2-2*(+P1)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D239
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U239/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(99)
      DWIDTH(7)=A(100)
      Q1(7)=-A(95)**2-A(7)**2+A(99)**2-2*(-P2)
      DMASS(6)=A(97)
      DWIDTH(6)=A(98)
      Q1(6)=-A(95)**2-A(7)**2+A(97)**2-2*(-P2)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(7)**2-2*(-P2)
      DMASS(4)=A(93)
      DWIDTH(4)=A(94)
      Q1(4)=-A(95)**2-A(7)**2+A(93)**2-2*(-P2)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(95)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(95)**2-A(87)**2+A(89)**2-2*(-P4)
      DMASS(1)=A(151)
      DWIDTH(1)=A(16)
      Q1(1)=-A(95)**2-A(89)**2+A(151)**2-2*(+P1)
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

      SUBROUTINE D23
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U23/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(84)**2+A(99)**2-2*(-P2)
      DMASS(9)=A(97)
      DWIDTH(9)=A(98)
      Q1(9)=-A(93)**2-A(84)**2+A(97)**2-2*(-P2)
      DMASS(8)=A(95)
      DWIDTH(8)=A(96)
      Q1(8)=-A(93)**2-A(84)**2+A(95)**2-2*(-P2)
      DMASS(7)=A(93)
      DWIDTH(7)=A(94)
      Q1(7)=-A(84)**2-2*(-P2)
      DMASS(6)=A(99)
      DWIDTH(6)=A(100)
      Q1(6)=-A(93)**2-A(84)**2+A(99)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(93)**2-A(84)**2+A(97)**2-2*(-P4)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(93)**2-A(84)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(84)
      DWIDTH(3)=A(85)
      Q1(3)=-A(93)**2-A(93)**2+A(84)**2-2*(+P1)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(84)**2-2*(-P4)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(93)**2-A(93)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D240
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U240/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(132)**2-2*(+P1)
      DMASS(1)=A(132)
      DWIDTH(1)=A(102)
      Q1(1)=-A(95)**2+A(132)**2-2*(-P4)
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

      SUBROUTINE D241
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U241/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(95)**2-A(132)**2-2*(+P1)
      DMASS(5)=A(132)
      DWIDTH(5)=A(102)
      Q1(5)=-A(95)**2+A(132)**2-2*(-P4)
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

      SUBROUTINE D242
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U242/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(136)
      DWIDTH(3)=A(110)
      Q1(3)=-A(95)**2+A(136)**2-2*(-P4)
      Q1(4)=-A(95)**2-A(132)**2-2*(+P1)
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

      SUBROUTINE D243
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U243/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(136)
      DWIDTH(3)=A(110)
      Q1(3)=-A(95)**2+A(136)**2-2*(-P2)
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
      RETURN
      END

      SUBROUTINE D244
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U244/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(132)
      DWIDTH(5)=A(102)
      Q1(5)=-A(95)**2+A(132)**2-2*(-P2)
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

      SUBROUTINE D245
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U245/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(132)
      DWIDTH(5)=A(102)
      Q1(5)=-A(95)**2+A(132)**2-2*(-P2)
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

      SUBROUTINE D246
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U246/ Q0(4),Q1(4),Q2(4)
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

      SUBROUTINE D247
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U247/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(1)=A(134)
      DWIDTH(1)=A(104)
      Q1(1)=-A(95)**2-A(8)**2+A(134)**2-2*(-P4)
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

      SUBROUTINE D248
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U248/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(95)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(5)=A(134)
      DWIDTH(5)=A(104)
      Q1(5)=-A(95)**2-A(8)**2+A(134)**2-2*(-P4)
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

      SUBROUTINE D249
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U249/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(137)
      DWIDTH(3)=A(111)
      Q1(3)=-A(95)**2+A(137)**2-2*(-P4)
      Q1(4)=-A(95)**2-A(134)**2+A(8)**2-2*(+P1)
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

      SUBROUTINE D24
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U24/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(93)**2-A(84)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(93)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(84)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(93)**2-A(7)**2+A(95)**2-2*(-P4)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(93)**2-A(84)**2+A(95)**2-2*(-P2)
      DMASS(4)=A(7)
      DWIDTH(4)=A(86)
      Q1(4)=-A(93)**2-A(93)**2+A(7)**2-2*(+P1)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(7)**2-2*(-P4)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(84)**2-2*(-P2)
      DMASS(1)=A(6)
      DWIDTH(1)=A(15)
      Q1(1)=-A(93)**2-A(93)**2+A(6)**2-2*(+P1)
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

      SUBROUTINE D250
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U250/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(137)
      DWIDTH(3)=A(111)
      Q1(3)=-A(95)**2+A(137)**2-2*(-P2)
      Q1(4)=-A(95)**2-A(134)**2+A(8)**2-2*(+P1)
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

      SUBROUTINE D251
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U251/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(135)
      DWIDTH(6)=A(105)
      Q1(6)=-A(95)**2-A(8)**2+A(135)**2-2*(-P2)
      Q1(7)=-A(95)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(5)=A(134)
      DWIDTH(5)=A(104)
      Q1(5)=-A(95)**2-A(8)**2+A(134)**2-2*(-P2)
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

      SUBROUTINE D252
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U252/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(135)
      DWIDTH(6)=A(105)
      Q1(6)=-A(95)**2-A(8)**2+A(135)**2-2*(-P2)
      Q1(7)=-A(95)**2-A(134)**2+A(8)**2-2*(+P1)
      DMASS(5)=A(134)
      DWIDTH(5)=A(104)
      Q1(5)=-A(95)**2-A(8)**2+A(134)**2-2*(-P2)
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

      SUBROUTINE D253
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U253/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(135)
      DWIDTH(5)=A(105)
      Q1(5)=-A(95)**2-A(8)**2+A(135)**2-2*(-P2)
      Q1(6)=-A(95)**2-A(134)**2+A(8)**2-2*(+P1)
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

      SUBROUTINE D254
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U254/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(106)**2+A(9)**2-2*(+P1)
      DMASS(1)=A(106)
      DWIDTH(1)=A(107)
      Q1(1)=-A(95)**2-A(9)**2+A(106)**2-2*(-P4)
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

      SUBROUTINE D255
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U255/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(108)
      DWIDTH(6)=A(109)
      Q1(6)=-A(95)**2-A(9)**2+A(108)**2-2*(-P4)
      Q1(7)=-A(95)**2-A(106)**2+A(9)**2-2*(+P1)
      DMASS(5)=A(106)
      DWIDTH(5)=A(107)
      Q1(5)=-A(95)**2-A(9)**2+A(106)**2-2*(-P4)
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

      SUBROUTINE D256
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U256/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(112)
      DWIDTH(3)=A(113)
      Q1(3)=-A(95)**2+A(112)**2-2*(-P4)
      Q1(4)=-A(95)**2-A(106)**2+A(9)**2-2*(+P1)
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

      SUBROUTINE D257
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U257/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(112)
      DWIDTH(3)=A(113)
      Q1(3)=-A(95)**2+A(112)**2-2*(-P2)
      Q1(4)=-A(95)**2-A(106)**2+A(9)**2-2*(+P1)
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

      SUBROUTINE D258
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U258/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(108)
      DWIDTH(6)=A(109)
      Q1(6)=-A(95)**2-A(9)**2+A(108)**2-2*(-P2)
      Q1(7)=-A(95)**2-A(106)**2+A(9)**2-2*(+P1)
      DMASS(5)=A(106)
      DWIDTH(5)=A(107)
      Q1(5)=-A(95)**2-A(9)**2+A(106)**2-2*(-P2)
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

      SUBROUTINE D259
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U259/ Q0(6),Q1(7),Q2(7)
      DIMENSION DMASS(6),DWIDTH(6)
      SAVE
      DMASS(6)=A(108)
      DWIDTH(6)=A(109)
      Q1(6)=-A(95)**2-A(9)**2+A(108)**2-2*(-P2)
      Q1(7)=-A(95)**2-A(106)**2+A(9)**2-2*(+P1)
      DMASS(5)=A(106)
      DWIDTH(5)=A(107)
      Q1(5)=-A(95)**2-A(9)**2+A(106)**2-2*(-P2)
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

      SUBROUTINE D25
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U25/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(7)**2+A(99)**2-2*(-P2)
      DMASS(9)=A(97)
      DWIDTH(9)=A(98)
      Q1(9)=-A(93)**2-A(7)**2+A(97)**2-2*(-P2)
      DMASS(8)=A(95)
      DWIDTH(8)=A(96)
      Q1(8)=-A(93)**2-A(7)**2+A(95)**2-2*(-P2)
      DMASS(7)=A(93)
      DWIDTH(7)=A(94)
      Q1(7)=-A(7)**2-2*(-P2)
      DMASS(6)=A(99)
      DWIDTH(6)=A(100)
      Q1(6)=-A(93)**2-A(7)**2+A(99)**2-2*(-P4)
      DMASS(5)=A(97)
      DWIDTH(5)=A(98)
      Q1(5)=-A(93)**2-A(7)**2+A(97)**2-2*(-P4)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(93)**2-A(7)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(7)**2-2*(-P4)
      DMASS(2)=A(84)
      DWIDTH(2)=A(85)
      Q1(2)=-A(93)**2-A(93)**2+A(84)**2-2*(+P1)
      DMASS(1)=A(82)
      DWIDTH(1)=A(83)
      Q1(1)=-A(93)**2-A(93)**2+A(82)**2-2*(+P1)
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

      SUBROUTINE D260
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U260/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(108)
      DWIDTH(5)=A(109)
      Q1(5)=-A(95)**2-A(9)**2+A(108)**2-2*(-P2)
      Q1(6)=-A(95)**2-A(106)**2+A(9)**2-2*(+P1)
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

      SUBROUTINE D261
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U261/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(95)**2-A(136)**2-2*(+P1)
      DMASS(5)=A(136)
      DWIDTH(5)=A(110)
      Q1(5)=-A(95)**2+A(136)**2-2*(-P4)
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

      SUBROUTINE D262
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U262/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(136)**2-2*(+P1)
      DMASS(3)=A(132)
      DWIDTH(3)=A(102)
      Q1(3)=-A(95)**2+A(132)**2-2*(-P4)
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

      SUBROUTINE D263
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U263/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(136)
      DWIDTH(5)=A(110)
      Q1(5)=-A(95)**2+A(136)**2-2*(-P2)
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

      SUBROUTINE D264
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U264/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(136)
      DWIDTH(5)=A(110)
      Q1(5)=-A(95)**2+A(136)**2-2*(-P2)
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

      SUBROUTINE D265
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U265/ Q0(4),Q1(4),Q2(4)
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

      SUBROUTINE D266
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U266/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(132)
      DWIDTH(3)=A(102)
      Q1(3)=-A(95)**2+A(132)**2-2*(-P2)
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
      RETURN
      END

      SUBROUTINE D267
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U267/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(95)**2-A(137)**2-2*(+P1)
      DMASS(5)=A(137)
      DWIDTH(5)=A(111)
      Q1(5)=-A(95)**2+A(137)**2-2*(-P4)
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

      SUBROUTINE D268
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U268/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(137)**2-2*(+P1)
      DMASS(3)=A(134)
      DWIDTH(3)=A(104)
      Q1(3)=-A(95)**2-A(8)**2+A(134)**2-2*(-P4)
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

      SUBROUTINE D269
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U269/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(137)
      DWIDTH(5)=A(111)
      Q1(5)=-A(95)**2+A(137)**2-2*(-P2)
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

      SUBROUTINE D26
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U26/ Q0(7),Q1(7),Q2(7)
      DIMENSION DMASS(7),DWIDTH(7)
      SAVE
      DMASS(7)=A(84)
      DWIDTH(7)=A(85)
      Q1(7)=-A(93)**2-A(93)**2+A(84)**2-2*(+P1)
      DMASS(6)=A(82)
      DWIDTH(6)=A(83)
      Q1(6)=-A(93)**2-A(93)**2+A(82)**2-2*(+P1)
      DMASS(5)=A(6)
      DWIDTH(5)=A(15)
      Q1(5)=-A(93)**2-A(93)**2+A(6)**2-2*(+P1)
      DMASS(4)=A(91)
      DWIDTH(4)=A(92)
      Q1(4)=-A(93)**2-A(87)**2+A(91)**2-2*(-P2)
      DMASS(3)=A(91)
      DWIDTH(3)=A(92)
      Q1(3)=-A(93)**2-A(87)**2+A(91)**2-2*(-P4)
      DMASS(2)=A(89)
      DWIDTH(2)=A(90)
      Q1(2)=-A(93)**2-A(87)**2+A(89)**2-2*(-P2)
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

      SUBROUTINE D270
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U270/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(137)
      DWIDTH(5)=A(111)
      Q1(5)=-A(95)**2+A(137)**2-2*(-P2)
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

      SUBROUTINE D271
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U271/ Q0(4),Q1(4),Q2(4)
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

      SUBROUTINE D272
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U272/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      Q1(5)=-A(95)**2-A(137)**2-2*(+P1)
      DMASS(4)=A(135)
      DWIDTH(4)=A(105)
      Q1(4)=-A(95)**2-A(8)**2+A(135)**2-2*(-P2)
      DMASS(3)=A(134)
      DWIDTH(3)=A(104)
      Q1(3)=-A(95)**2-A(8)**2+A(134)**2-2*(-P2)
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

      SUBROUTINE D273
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U273/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(95)**2-A(112)**2-2*(+P1)
      DMASS(5)=A(112)
      DWIDTH(5)=A(113)
      Q1(5)=-A(95)**2+A(112)**2-2*(-P4)
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

      SUBROUTINE D274
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U274/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      Q1(5)=-A(95)**2-A(112)**2-2*(+P1)
      DMASS(4)=A(108)
      DWIDTH(4)=A(109)
      Q1(4)=-A(95)**2-A(9)**2+A(108)**2-2*(-P4)
      DMASS(3)=A(106)
      DWIDTH(3)=A(107)
      Q1(3)=-A(95)**2-A(9)**2+A(106)**2-2*(-P4)
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

      SUBROUTINE D275
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U275/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(112)
      DWIDTH(5)=A(113)
      Q1(5)=-A(95)**2+A(112)**2-2*(-P2)
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

      SUBROUTINE D276
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U276/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(112)
      DWIDTH(5)=A(113)
      Q1(5)=-A(95)**2+A(112)**2-2*(-P2)
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

      SUBROUTINE D277
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U277/ Q0(4),Q1(4),Q2(4)
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

      SUBROUTINE D278
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U278/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      Q1(5)=-A(95)**2-A(112)**2-2*(+P1)
      DMASS(4)=A(108)
      DWIDTH(4)=A(109)
      Q1(4)=-A(95)**2-A(9)**2+A(108)**2-2*(-P2)
      DMASS(3)=A(106)
      DWIDTH(3)=A(107)
      Q1(3)=-A(95)**2-A(9)**2+A(106)**2-2*(-P2)
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

      SUBROUTINE D279
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U279/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(138)**2-2*(+P1)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(95)**2+A(138)**2-2*(-P4)
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

      SUBROUTINE D27
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U27/ Q0(10),Q1(10),Q2(10)
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
      DMASS(6)=A(84)
      DWIDTH(6)=A(85)
      Q1(6)=-A(93)**2-A(95)**2+A(84)**2-2*(+P1)
      DMASS(5)=A(82)
      DWIDTH(5)=A(83)
      Q1(5)=-A(93)**2-A(95)**2+A(82)**2-2*(+P1)
      DMASS(4)=A(95)
      DWIDTH(4)=A(96)
      Q1(4)=-A(93)**2-A(6)**2+A(95)**2-2*(-P4)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(93)**2-A(6)**2+A(95)**2-2*(-P2)
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

      SUBROUTINE D280
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U280/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(95)**2-A(138)**2-2*(+P1)
      DMASS(5)=A(138)
      DWIDTH(5)=A(114)
      Q1(5)=-A(95)**2+A(138)**2-2*(-P4)
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

      SUBROUTINE D281
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U281/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(140)
      DWIDTH(3)=A(116)
      Q1(3)=-A(95)**2+A(140)**2-2*(-P4)
      Q1(4)=-A(95)**2-A(138)**2-2*(+P1)
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

      SUBROUTINE D282
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U282/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(95)**2-A(11)**2+A(144)**2-2*(-P4)
      Q1(4)=-A(95)**2-A(138)**2-2*(+P1)
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

      SUBROUTINE D283
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U283/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(128)
      DWIDTH(4)=A(129)
      Q1(4)=-A(95)**2-A(13)**2+A(128)**2-2*(-P4)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(95)**2-A(13)**2+A(126)**2-2*(-P4)
      Q1(5)=-A(95)**2-A(138)**2-2*(+P1)
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

      SUBROUTINE D284
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U284/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(138)**2-2*(+P1)
      DMASS(1)=A(138)
      DWIDTH(1)=A(114)
      Q1(1)=-A(95)**2+A(138)**2-2*(-P4)
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

      SUBROUTINE D285
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U285/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(138)
      DWIDTH(5)=A(114)
      Q1(5)=-A(95)**2+A(138)**2-2*(-P2)
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

      SUBROUTINE D286
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U286/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(138)
      DWIDTH(5)=A(114)
      Q1(5)=-A(95)**2+A(138)**2-2*(-P2)
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

      SUBROUTINE D287
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U287/ Q0(4),Q1(4),Q2(4)
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

      SUBROUTINE D288
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U288/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
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
      RETURN
      END

      SUBROUTINE D289
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U289/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(145)
      DWIDTH(4)=A(121)
      Q1(4)=-A(95)**2-A(11)**2+A(145)**2-2*(-P2)
      DMASS(3)=A(144)
      DWIDTH(3)=A(120)
      Q1(3)=-A(95)**2-A(11)**2+A(144)**2-2*(-P2)
      Q1(5)=-A(95)**2-A(138)**2-2*(+P1)
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

      SUBROUTINE D28
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U28/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(93)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(93)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(7)
      DWIDTH(6)=A(86)
      Q1(6)=-A(93)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(93)**2-A(82)**2+A(95)**2-2*(-P4)
      DMASS(4)=A(6)
      DWIDTH(4)=A(15)
      Q1(4)=-A(93)**2-A(95)**2+A(6)**2-2*(+P1)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(93)**2-A(6)**2+A(95)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(82)**2-2*(-P4)
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

      SUBROUTINE D290
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U290/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(128)
      DWIDTH(4)=A(129)
      Q1(4)=-A(95)**2-A(13)**2+A(128)**2-2*(-P2)
      DMASS(3)=A(126)
      DWIDTH(3)=A(127)
      Q1(3)=-A(95)**2-A(13)**2+A(126)**2-2*(-P2)
      Q1(5)=-A(95)**2-A(138)**2-2*(+P1)
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

      SUBROUTINE D291
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U291/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(140)**2-2*(+P1)
      DMASS(1)=A(140)
      DWIDTH(1)=A(116)
      Q1(1)=-A(95)**2+A(140)**2-2*(-P4)
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

      SUBROUTINE D292
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U292/ Q0(5),Q1(6),Q2(6)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      Q1(6)=-A(95)**2-A(140)**2-2*(+P1)
      DMASS(5)=A(140)
      DWIDTH(5)=A(116)
      Q1(5)=-A(95)**2+A(140)**2-2*(-P4)
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

      SUBROUTINE D293
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U293/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      Q1(4)=-A(95)**2-A(140)**2-2*(+P1)
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

      SUBROUTINE D294
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U294/ Q0(3),Q1(4),Q2(4)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
      DMASS(3)=A(142)
      DWIDTH(3)=A(118)
      Q1(3)=-A(95)**2-A(10)**2+A(142)**2-2*(-P4)
      Q1(4)=-A(95)**2-A(140)**2-2*(+P1)
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

      SUBROUTINE D295
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U295/ Q0(4),Q1(5),Q2(5)
      DIMENSION DMASS(4),DWIDTH(4)
      SAVE
      DMASS(4)=A(124)
      DWIDTH(4)=A(125)
      Q1(4)=-A(95)**2-A(12)**2+A(124)**2-2*(-P4)
      DMASS(3)=A(122)
      DWIDTH(3)=A(123)
      Q1(3)=-A(95)**2-A(12)**2+A(122)**2-2*(-P4)
      Q1(5)=-A(95)**2-A(140)**2-2*(+P1)
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

      SUBROUTINE D296
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U296/ Q0(1),Q1(2),Q2(2)
      DIMENSION DMASS(1),DWIDTH(1)
      SAVE
      Q1(2)=-A(95)**2-A(140)**2-2*(+P1)
      DMASS(1)=A(140)
      DWIDTH(1)=A(116)
      Q1(1)=-A(95)**2+A(140)**2-2*(-P4)
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

      SUBROUTINE D297
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U297/ Q0(3),Q1(3),Q2(3)
      DIMENSION DMASS(3),DWIDTH(3)
      SAVE
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
      RETURN
      END

      SUBROUTINE D298
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U298/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(140)
      DWIDTH(5)=A(116)
      Q1(5)=-A(95)**2+A(140)**2-2*(-P2)
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

      SUBROUTINE D299
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U299/ Q0(5),Q1(5),Q2(5)
      DIMENSION DMASS(5),DWIDTH(5)
      SAVE
      DMASS(5)=A(140)
      DWIDTH(5)=A(116)
      Q1(5)=-A(95)**2+A(140)**2-2*(-P2)
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

      SUBROUTINE D29
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U29/ Q0(10),Q1(10),Q2(10)
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
      DMASS(6)=A(7)
      DWIDTH(6)=A(86)
      Q1(6)=-A(93)**2-A(95)**2+A(7)**2-2*(+P1)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(93)**2-A(84)**2+A(95)**2-2*(-P4)
      DMASS(4)=A(6)
      DWIDTH(4)=A(15)
      Q1(4)=-A(93)**2-A(95)**2+A(6)**2-2*(+P1)
      DMASS(3)=A(95)
      DWIDTH(3)=A(96)
      Q1(3)=-A(93)**2-A(6)**2+A(95)**2-2*(-P2)
      DMASS(2)=A(93)
      DWIDTH(2)=A(94)
      Q1(2)=-A(84)**2-2*(-P4)
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

      SUBROUTINE D2
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U2/ Q0(10),Q1(10),Q2(10)
      DIMENSION DMASS(10),DWIDTH(10)
      SAVE
      DMASS(10)=A(99)
      DWIDTH(10)=A(100)
      Q1(10)=-A(93)**2-A(82)**2+A(99)**2-2*(-P4)
      DMASS(9)=A(99)
      DWIDTH(9)=A(100)
      Q1(9)=-A(93)**2-A(6)**2+A(99)**2-2*(-P2)
      DMASS(8)=A(97)
      DWIDTH(8)=A(98)
      Q1(8)=-A(93)**2-A(82)**2+A(97)**2-2*(-P4)
      DMASS(7)=A(97)
      DWIDTH(7)=A(98)
      Q1(7)=-A(93)**2-A(6)**2+A(97)**2-2*(-P2)
      DMASS(6)=A(95)
      DWIDTH(6)=A(96)
      Q1(6)=-A(93)**2-A(82)**2+A(95)**2-2*(-P4)
      DMASS(5)=A(95)
      DWIDTH(5)=A(96)
      Q1(5)=-A(93)**2-A(6)**2+A(95)**2-2*(-P2)
      DMASS(4)=A(7)
      DWIDTH(4)=A(86)
      Q1(4)=-A(93)**2-A(93)**2+A(7)**2-2*(+P1)
      DMASS(3)=A(93)
      DWIDTH(3)=A(94)
      Q1(3)=-A(82)**2-2*(-P4)
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

