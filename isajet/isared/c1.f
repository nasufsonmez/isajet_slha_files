*******************************
*    CompHEP version 33.24    *
*******************************
      SUBROUTINE CC1(C)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      DIMENSION C(9)                                                    
      SAVE
      S1=A(93)**2
      S2=A(6)**2
      S3=A(6)**4
      C(1)=+S3*(S1*(32*S1+9*S2))
      S4=A(6)**6
      C(2)=+S4
      C(3)=+62*S1*S3
      C(4)=+2*S3
      C(5)=+S2*(28*S1+10*S2)
      C(6)=+4*S2
      C(7)=+8*(S1+S2)
      C(8)=+S3
      S5=A(1459)**4
      C(9)=+S5
      RETURN
      END

