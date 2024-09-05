*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1()
*                    ~o1    Z    !  Z    ~o1                         
*                   =====@-1-----!---1-@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o1|P5     !  ~o1|-P6                          
*                    ~o1 |  Z    !  Z  | ~o1                         
*                   =====@-2-----!---2-@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1).EQ.0) CALL CC1(C)
      TOTNUM=-C(9)
      TOTDEN=+C(8)
      RNUM=+P2*(P2*(P2*(8*(P2-P1)-C(7))+C(5)-C(6)*P1)+C(4)*P1-C(3))+C(1)
     >+C(2)*P1
      F1=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0(7)*
     >Q0(8)*Q0(9)*Q0(10)
      RETURN
      END

