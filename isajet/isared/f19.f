*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19000()
*                                !  b    ~b1                         
*                               /!==>==@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~o2|-P6                          
*                    ~b1    b   |!  b  | ~b1                         
*                   -->--@==>===+!==>==@-->--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o2|P5    |!                                   
*                    ~b1 |  b   |!                                   
*                   -->--@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19000(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19000=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19001()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o2|P5     !  ~o3|-P6                          
*                    ~b1 |  b    !  b  | ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19001(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19001=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19002()
*                                !  b    ~b1                         
*                               /!==>==@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~o3|-P6                          
*                    ~b1    b   |!  b  | ~b1                         
*                   -->--@==>===+!==>==@-->--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o2|P5    |!                                   
*                    ~b1 |  b   |!                                   
*                   -->--@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19002(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19002=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(7)*Q0(8)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19003()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o2|P5     !  ~o4|-P6                          
*                    ~b1 |  b    !  b  | ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19003(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19003=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(7)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19004()
*                                !  b    ~b1                         
*                               /!==>==@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~o4|-P6                          
*                    ~b1    b   |!  b  | ~b1                         
*                   -->--@==>===+!==>==@-->--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o2|P5    |!                                   
*                    ~b1 |  b   |!                                   
*                   -->--@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19004(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19004=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(8)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19005()
*                                !  b    ~b1                         
*                               /!==>==@-->--                        
*                               |!  P4 |  P1                         
*                               |!   ~g|-P6                          
*                    ~b1    b   |!  b  | ~b1                         
*                   -->--@==>===+!==>==@-->--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o2|P5    |!                                   
*                    ~b1 |  b   |!                                   
*                   -->--@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19005(C)
      S1=A(146)**2
      TOTNUM=-C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))-C(1)-C(2)*P1
      F19005=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(9)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19006()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o3|P5     !  ~o3|-P6                          
*                    ~b1 |  b    !  b  | ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19006(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19006=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19007()
*                                !  b    ~b1                         
*                               /!==>==@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~o3|-P6                          
*                    ~b1    b   |!  b  | ~b1                         
*                   -->--@==>===+!==>==@-->--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o3|P5    |!                                   
*                    ~b1 |  b   |!                                   
*                   -->--@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19007(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19007=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(7)*Q0(8)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19008()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o3|P5     !  ~o4|-P6                          
*                    ~b1 |  b    !  b  | ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19008(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19008=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)*Q0(8)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19009()
*                                !  b    ~b1                         
*                               /!==>==@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~o4|-P6                          
*                    ~b1    b   |!  b  | ~b1                         
*                   -->--@==>===+!==>==@-->--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o3|P5    |!                                   
*                    ~b1 |  b   |!                                   
*                   -->--@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19009(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19009=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)*Q0(7)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1900()
*                                   !  S    ~o1                      
*                                  /!==<==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~s1|-P6                       
*                 ~o1          c   |!  c  | ~1+                      
*                =====\     /==>===+!==>==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  H+ |  S   |!                                
*                ==>==@-->--@==<===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1900(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F1900=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19010()
*                                !  b    ~b1                         
*                               /!==>==@-->--                        
*                               |!  P4 |  P1                         
*                               |!   ~g|-P6                          
*                    ~b1    b   |!  b  | ~b1                         
*                   -->--@==>===+!==>==@-->--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o3|P5    |!                                   
*                    ~b1 |  b   |!                                   
*                   -->--@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19010(C)
      S1=A(146)**2
      TOTNUM=-C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))-C(1)-C(2)*P1
      F19010=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)*Q0(7)*Q0(8)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19011()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o4|P5     !  ~o4|-P6                          
*                    ~b1 |  b    !  b  | ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19011(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19011=RNUM*(TOTNUM/TOTDEN)*Q2(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19012()
*                                !  b    ~b1                         
*                               /!==>==@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~o4|-P6                          
*                    ~b1    b   |!  b  | ~b1                         
*                   -->--@==>===+!==>==@-->--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o4|P5    |!                                   
*                    ~b1 |  b   |!                                   
*                   -->--@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19012(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19012=RNUM*(TOTNUM/TOTDEN)*Q1(7)*Q1(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(6)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19013()
*                                !  b    ~b1                         
*                               /!==>==@-->--                        
*                               |!  P4 |  P1                         
*                               |!   ~g|-P6                          
*                    ~b1    b   |!  b  | ~b1                         
*                   -->--@==>===+!==>==@-->--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o4|P5    |!                                   
*                    ~b1 |  b   |!                                   
*                   -->--@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19013(C)
      S1=A(146)**2
      TOTNUM=-C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))-C(1)-C(2)*P1
      F19013=RNUM*(TOTNUM/TOTDEN)*Q1(7)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(6)*Q0(8)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19014()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                      ~g|P5     !   ~g|-P6                          
*                    ~b1 |  b    !  b  | ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19014(C)
      S1=A(146)**4
      TOTNUM=-16*S1
      TOTDEN=+9
      RNUM=+P2*(C(4)*(P2-P1)+C(3))-C(1)-C(2)*P1
      F19014=RNUM*(TOTNUM/TOTDEN)*Q2(10)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q
     >0(6)*Q0(7)*Q0(8)*Q0(9)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19015()
*                                !  b    ~b1                         
*                               /!==>==@-->--                        
*                               |!  P4 |  P1                         
*                               |!   ~g|-P6                          
*                    ~b1    b   |!  b  | ~b1                         
*                   -->--@==>===+!==>==@-->--                        
*                     P1 |  P3  |!  P3    P2                         
*                      ~g|P5    |!                                   
*                    ~b1 |  b   |!                                   
*                   -->--@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1760/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1760).EQ.0) CALL CC19015(C)
      S1=A(146)**4
      TOTNUM=-16*S1
      TOTDEN=+27
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19015=RNUM*(TOTNUM/TOTDEN)*Q1(10)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q
     >0(5)*Q0(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19016()
*                    ~b1    A    !  A    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  A    !  A  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1761/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1761).EQ.0) CALL CC19016(C)
      TOTNUM=+C(3)
      TOTDEN=+243
      RNUM=+P2*(P2-C(2))+C(1)
      F19016=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19017()
*                                !  A    ~b1                         
*                               /!---2-@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    A   |!  A  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  A   |!                                   
*                   --<--@-2----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1761/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1761).EQ.0) CALL CC19017(C)
      TOTNUM=+C(3)
      TOTDEN=+243
      RNUM=+P1*(P1-C(2))+C(1)
      F19017=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19018()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    A    !  A  | ~B1                         
*                   -->--@-1-----!---1-@--<--                        
*                     P1 |  P3   !  P3 |  P2                         
*                     ~b1|P5     !     |                             
*                    ~B1 |  A    !  A  |                             
*                   --<--@-2-----!---2-/                             
*                     P2    P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1761/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1761).EQ.0) CALL CC19018(C)
      TOTNUM=-C(2)
      TOTDEN=+243
      RNUM=+P1-C(1)+4*P2
      F19018=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19019()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  A    !  A  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2 |  P4   !  P4 |  P2                         
*                        |       !     |                             
*                        |  A    !  A  |                             
*                        \-1-----!---1-/                             
*                           P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1761/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1761).EQ.0) CALL CC19019(C)
      TOTNUM=+C(1)
      TOTDEN=+243
      RNUM=+1
      F19019=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1901()
*                                   !  S    ~o1                      
*                                  /!==<==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~s2|-P6                       
*                 ~o1          c   |!  c  | ~1+                      
*                =====\     /==>===+!==>==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  H+ |  S   |!                                
*                ==>==@-->--@==<===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1901(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F1901=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19020()
*                    ~b1    A    !  A    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1762/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1762).EQ.0) CALL CC19020(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(P2*(C(4)*P2-C(3))-C(2))+C(1)
      F19020=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19021()
*                                !  Z    ~b1                         
*                               /!---2-@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    A   |!  A  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  Z   |!                                   
*                   --<--@-2----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1762/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1762).EQ.0) CALL CC19021(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      S1=P2**2
      RNUM=+P1*(P1*(C(3)-C(6)*P2)+C(6)*S1-C(2))+P2*(C(4)-C(5)*P2)+C(1)
      F19021=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19022()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    A    !  A  | ~B1                         
*                   -->--@-1-----!---1-@--<--                        
*                     P1 |  P3   !  P3 |  P2                         
*                     ~b1|P5     !     |                             
*                    ~B1 |  Z    !  Z  |                             
*                   --<--@-2-----!---2-/                             
*                     P2    P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1762/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1762).EQ.0) CALL CC19022(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(C(5)*P2-C(3)-C(4)*P1)+C(2)*P1-C(1)
      F19022=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19023()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  A    !  A  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1762/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1762).EQ.0) CALL CC19023(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(5)*P2-C(4)-C(6)*P1)+P2*(C(3)-C(5)*P2)-C(2))+P2*(P2
     >*(C(6)*P2-C(4))+C(2))+C(1)
      F19023=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19024()
*                    ~b1    Z    !       ~b1                         
*                   -->--@-1---\ !     /-->--                        
*                     P1 |  P4 | !     |  P1                         
*                     ~b1|P5   | !     |                             
*                    ~B1 |  A  | !  A  | ~B1                         
*                   --<--@-2---+-!---2-@--<--                        
*                     P2    P3 | !  P3 |  P2                         
*                              | !     |                             
*                              | !  Z  |                             
*                              \-!---1-/                             
*                                !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1762/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1762).EQ.0) CALL CC19024(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F19024=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19025()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  A    !  A  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2 |  P3   !  P3 |  P2                         
*                        |       !     |                             
*                        |  Z    !  Z  |                             
*                        \-1-----!---1-/                             
*                           P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1762/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1762).EQ.0) CALL CC19025(C)
      TOTNUM=+C(1)
      TOTDEN=+1
      RNUM=+1
      F19025=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19026()
*                    ~b1    A    !  A    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  G    !  G  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1763/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1763).EQ.0) CALL CC19026(C)
      S1=A(146)**2
      TOTNUM=+C(3)*S1
      TOTDEN=+81
      RNUM=+P2*(P2-C(2))+C(1)
      F19026=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19027()
*                                !  G    ~b1                         
*                               /!---2-@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    A   |!  A  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  G   |!                                   
*                   --<--@-2----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1763/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1763).EQ.0) CALL CC19027(C)
      S1=A(146)**2
      TOTNUM=+C(3)*S1
      TOTDEN=+81
      RNUM=+P1*(P1-C(2))+C(1)
      F19027=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19028()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    A    !  A  | ~B1                         
*                   -->--@-1-----!---1-@--<--                        
*                     P1 |  P3   !  P3 |  P2                         
*                     ~b1|P5     !     |                             
*                    ~B1 |  G    !  G  |                             
*                   --<--@-2-----!---2-/                             
*                     P2    P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1763/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1763).EQ.0) CALL CC19028(C)
      S1=A(146)**2
      TOTNUM=-C(2)*S1
      TOTDEN=+81
      RNUM=+P1-C(1)+4*P2
      F19028=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19029()
*                    ~b1    G    !  G    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  A    !  A  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1763/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1763).EQ.0) CALL CC19029(C)
      S1=A(146)**2
      TOTNUM=+C(1)*S1
      TOTDEN=+81
      S2=P2**2
      RNUM=+P1*(P1-2*P2)+S2
      F19029=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1902()
*                    ~o1    c    !  c    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c1|P5     !  ~c1|-P6                          
*                    ~1+ |  S    !  S  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1902(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1902=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19030()
*                    ~b1    G    !       ~b1                         
*                   -->--@-1---\ !     /-->--                        
*                     P1 |  P4 | !     |  P1                         
*                     ~b1|P5   | !     |                             
*                    ~B1 |  A  | !  A  | ~B1                         
*                   --<--@-2---+-!---2-@--<--                        
*                     P2    P3 | !  P3 |  P2                         
*                              | !     |                             
*                              | !  G  |                             
*                              \-!---1-/                             
*                                !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1763/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1763).EQ.0) CALL CC19030(C)
      S1=A(146)**2
      TOTNUM=+C(2)*S1
      TOTDEN=+81
      RNUM=+4*P2-C(1)-5*P1
      F19030=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19031()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  A    !  A  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2 |  P3   !  P3 |  P2                         
*                        |       !     |                             
*                        |  G    !  G  |                             
*                        \-1-----!---1-/                             
*                           P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1763/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1763).EQ.0) CALL CC19031(C)
      S1=A(146)**2
      TOTNUM=+C(1)*S1
      TOTDEN=+81
      RNUM=+1
      F19031=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19032()
*                    ~b1    A    !  A    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1764/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1764).EQ.0) CALL CC19032(C)
      TOTNUM=+C(2)
      TOTDEN=+27
      RNUM=+P2-C(1)
      F19032=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19033()
*                                !  h    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    A   |!  A  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  h   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1764/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1764).EQ.0) CALL CC19033(C)
      TOTNUM=+C(2)
      TOTDEN=+27
      RNUM=+2*P1-C(1)
      F19033=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19034()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  A    !  A  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1764/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1764).EQ.0) CALL CC19034(C)
      TOTNUM=-C(2)
      TOTDEN=+27
      RNUM=+2*(P2-P1)+C(1)
      F19034=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19035()
*                    ~b1    A    !  A    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1765/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1765).EQ.0) CALL CC19035(C)
      TOTNUM=+C(2)
      TOTDEN=+27
      RNUM=+P2-C(1)
      F19035=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19036()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    A   |!  A  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1765/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1765).EQ.0) CALL CC19036(C)
      TOTNUM=+C(2)
      TOTDEN=+27
      RNUM=+2*P1-C(1)
      F19036=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19037()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  A    !  A  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1765/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1765).EQ.0) CALL CC19037(C)
      TOTNUM=-C(2)
      TOTDEN=+27
      RNUM=+2*(P2-P1)+C(1)
      F19037=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19038()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19038(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      S1=P2**2
      RNUM=+S1*(C(3)*S1-C(2))+C(1)
      F19038=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19039()
*                                !  Z    ~b1                         
*                               /!---2-@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  Z   |!                                   
*                   --<--@-2----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(10)                                                   
      SAVE
      IF(L(1766).EQ.0) CALL CC19039(C)
      TOTNUM=+C(10)
      TOTDEN=+C(9)
      S1=P1**2
      RNUM=+P2*(P2*(P1*(C(5)+C(8)*P1-C(7)*P2)+P2*(C(8)*P2-C(6))+C(4))+P1
     >*(-C(2)-C(3)*P1))+C(1)*S1
      F19039=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1903()
*                    ~o1    c    !  c    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c1|P5     !  ~c2|-P6                          
*                    ~1+ |  S    !  S  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1903(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1903=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19040()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    Z    !  Z  | ~B1                         
*                   -->--@-1-----!---1-@--<--                        
*                     P1 |  P3   !  P3 |  P2                         
*                     ~b1|P5     !     |                             
*                    ~B1 |  Z    !  Z  |                             
*                   --<--@-2-----!---2-/                             
*                     P2    P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19040(C)
      TOTNUM=-C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(P1*(C(5)*P2-C(3))+C(4)*P2-C(2))+C(1)*P1
      F19040=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19041()
*                 ~b1    Z    !  Z          ~b1                      
*                -->--@-1-----!---1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~b1|P5     !     |     |                          
*                 ~B1 |  Z    !  Z  |  h  | ~B1                      
*                --<--@-2-----!---2-@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19041(C)
      TOTNUM=-C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(P1*(C(5)*P2-C(3))+C(4)*P2-C(2))+C(1)*P1
      F19041=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19042()
*                 ~b1    Z    !  Z          ~b1                      
*                -->--@-1-----!---1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~b1|P5     !     |     |                          
*                 ~B1 |  Z    !  Z  |  H  | ~B1                      
*                --<--@-2-----!---2-@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19042(C)
      TOTNUM=-C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(P1*(C(5)*P2-C(3))+C(4)*P2-C(2))+C(1)*P1
      F19042=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19043()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19043(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      S1=P2**2
      RNUM=+S1*(C(3)*S1-C(2))+C(1)
      F19043=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19044()
*                                !  Z    ~b1                         
*                               /!---2-@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  Z   |!                                   
*                   --<--@-2----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(10)                                                   
      SAVE
      IF(L(1766).EQ.0) CALL CC19044(C)
      TOTNUM=+C(10)
      TOTDEN=+C(9)
      S1=P1**2
      RNUM=+P2*(P2*(P1*(C(5)+C(8)*P1-C(7)*P2)+P2*(C(8)*P2-C(6))+C(4))+P1
     >*(-C(2)-C(3)*P1))+C(1)*S1
      F19044=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19045()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2 |  P4   !  P4 |  P2                         
*                        |       !     |                             
*                        |  Z    !  Z  |                             
*                        \-1-----!---1-/                             
*                           P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19045(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+P1)+C(1)
      F19045=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19046()
*                 ~b1         !                                      
*                -->--\       !                                      
*                  P1 |       !                                      
*                     |       !                                      
*                 ~B1 |  Z    !  Z          ~b1                      
*                --<--@-2-----!---2-\     /-->--                     
*                  P2 |  P4   !  P4 |     |  P1                      
*                     |       !     |     |                          
*                     |  Z    !  Z  |  h  | ~B1                      
*                     \-1-----!---1-@-----@--<--                     
*                        P3   !  P3   -P5    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19046(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+P1)+C(1)
      F19046=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19047()
*                 ~b1         !                                      
*                -->--\       !                                      
*                  P1 |       !                                      
*                     |       !                                      
*                 ~B1 |  Z    !  Z          ~b1                      
*                --<--@-2-----!---2-\     /-->--                     
*                  P2 |  P4   !  P4 |     |  P1                      
*                     |       !     |     |                          
*                     |  Z    !  Z  |  H  | ~B1                      
*                     \-1-----!---1-@-----@--<--                     
*                        P3   !  P3   -P5    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19047(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+P1)+C(1)
      F19047=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19048()
*                    ~b1         !                                   
*                   -->--\       !                                   
*                     P1 |       !                                   
*                        |       !                                   
*                    ~B1 |  Z    !  Z    ~b1                         
*                   --<--@-2-----!---2-@-->--                        
*                     P2 |  P4   !  P4 |  P1                         
*                        |       !  ~b2|-P5                          
*                        |  Z    !  Z  | ~B1                         
*                        \-1-----!---1-@--<--                        
*                           P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19048(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+P1-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))+C
     >(1)
      F19048=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19049()
*              ~b1          Z    !  Z          ~b1                   
*             -->--\     /-2-----!---2-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  Z    !  Z  |  h  | ~B1                   
*             --<--@-----@-1-----!---1-@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19049(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+P1)+C(1)
      F19049=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1904()
*                                !  S    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~s1|-P6                          
*                    ~o1    c   |!  c  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~c1|P5    |!                                   
*                    ~1+ |  S   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1904(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1904=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19050()
*              ~b1          Z    !  Z          ~b1                   
*             -->--\     /-2-----!---2-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  Z    !  Z  |  H  | ~B1                   
*             --<--@-----@-1-----!---1-@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19050(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+P1)+C(1)
      F19050=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19051()
*                 ~b1          Z    !  Z    ~b1                      
*                -->--\     /-2-----!---2-@-->--                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  h  |  Z    !  Z  | ~B1                      
*                --<--@-----@-1-----!---1-@--<--                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19051(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+P1-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))+C
     >(1)
      F19051=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19052()
*              ~b1          Z    !  Z          ~b1                   
*             -->--\     /-2-----!---2-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  Z    !  Z  |  H  | ~B1                   
*             --<--@-----@-1-----!---1-@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19052(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+P1)+C(1)
      F19052=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19053()
*                 ~b1          Z    !  Z    ~b1                      
*                -->--\     /-2-----!---2-@-->--                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  H  |  Z    !  Z  | ~B1                      
*                --<--@-----@-1-----!---1-@--<--                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19053(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+P1-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))+C
     >(1)
      F19053=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19054()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19054(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      S1=P2**2
      RNUM=+S1*(S1-C(2))+C(1)
      F19054=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19055()
*                                !  Z    ~b1                         
*                               /!---2-@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b2|P5    |!                                   
*                    ~B1 |  Z   |!                                   
*                   --<--@-2----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1766/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1766).EQ.0) CALL CC19055(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      S1=P1**2
      RNUM=+P2*(P2*(P1*(C(5)+P1-2*P2)+P2*(P2-C(6))+C(4))+P1*(-C(2)-C(3)*
     >P1))+C(1)*S1
      F19055=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19056()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  G    !  G  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1767/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1767).EQ.0) CALL CC19056(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(P2*(C(4)*P2-C(3))-C(2))+C(1)
      F19056=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19057()
*                                !  G    ~b1                         
*                               /!---2-@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  G   |!                                   
*                   --<--@-2----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1767/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1767).EQ.0) CALL CC19057(C)
      S1=A(146)**2
      TOTNUM=+C(7)*S1
      TOTDEN=+C(6)
      RNUM=+P1*(P2*(C(5)*(P2-P1)-C(3))+C(3)*P1-C(1))+P2*(C(2)-C(4)*P2)
      F19057=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19058()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    Z    !  Z  | ~B1                         
*                   -->--@-1-----!---1-@--<--                        
*                     P1 |  P3   !  P3 |  P2                         
*                     ~b1|P5     !     |                             
*                    ~B1 |  G    !  G  |                             
*                   --<--@-2-----!---2-/                             
*                     P2    P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1767/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1767).EQ.0) CALL CC19058(C)
      S1=A(146)**2
      TOTNUM=+C(7)*S1
      TOTDEN=+C(6)
      RNUM=+P2*(C(5)*P2-C(3)-C(4)*P1)+C(2)*P1-C(1)
      F19058=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19059()
*                    ~b1    G    !  G    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1767/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1767).EQ.0) CALL CC19059(C)
      S1=A(146)**2
      TOTNUM=-C(7)*S1
      TOTDEN=+C(6)
      RNUM=+P1*(P1*(C(4)*P2-C(3)-C(5)*P1)+P2*(C(2)-C(4)*P2)-C(1))+P2*(P2
     >*(C(5)*P2-C(3))+C(1))
      F19059=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1905()
*                                !  S    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~s2|-P6                          
*                    ~o1    c   |!  c  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~c1|P5    |!                                   
*                    ~1+ |  S   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1905(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1905=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19060()
*                    ~b1    G    !       ~b1                         
*                   -->--@-1---\ !     /-->--                        
*                     P1 |  P4 | !     |  P1                         
*                     ~b1|P5   | !     |                             
*                    ~B1 |  Z  | !  Z  | ~B1                         
*                   --<--@-2---+-!---2-@--<--                        
*                     P2    P3 | !  P3 |  P2                         
*                              | !     |                             
*                              | !  G  |                             
*                              \-!---1-/                             
*                                !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1767/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1767).EQ.0) CALL CC19060(C)
      S1=A(146)**2
      TOTNUM=+C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F19060=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19061()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2 |  P3   !  P3 |  P2                         
*                        |       !     |                             
*                        |  G    !  G  |                             
*                        \-1-----!---1-/                             
*                           P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1767/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1767).EQ.0) CALL CC19061(C)
      S1=A(146)**2
      TOTNUM=+C(3)*S1
      TOTDEN=+C(2)
      RNUM=+C(1)
      F19061=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19062()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19062(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      S1=P2**2
      RNUM=+C(2)*S1-C(1)
      F19062=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19063()
*                 ~b1    Z    !  Z          ~b1                      
*                -->--@-1-----!---1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~b1|P5     !     |     |                          
*                 ~B1 |  h    !  h  |  Z  | ~B1                      
*                --<--@-------!-----@---2-@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19063(C)
      TOTNUM=-C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(3)-C(6)*P2)+C(2)-C(5)*P2)+P2*(C(7)*P2-C(4))+C(1)
      F19063=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19064()
*                                !  h    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  h   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19064(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*(P2-P1)-C(2))+C(1)*P1
      F19064=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19065()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19065(C)
      TOTNUM=-C(4)
      TOTDEN=+C(3)
      S1=P2**2
      RNUM=+C(2)*S1-C(1)
      F19065=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19066()
*                                !  h    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  h   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19066(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*(P2-P1)-C(2))+C(1)*P1
      F19066=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19067()
*              ~b1          Z    !  Z          ~b1                   
*             -->--\     /-2-----!---2-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  h    !  h  |  Z  | ~B1                   
*             --<--@-1---@-------!-----@---3-@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19067(C)
      TOTNUM=+C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(P1*(-C(4)-C(5)*P1)-C(3))-C(2)-C(7)*P2)+P2*(C(7)*P2-C
     >(6))-C(1)
      F19067=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19068()
*                                   !  h    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b1|-P6                       
*                 ~b1          Z   |!  Z  | ~B1                      
*                -->--\     /-2----+!---2-@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  Z  |  h   |!                                
*                --<--@-1---@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19068(C)
      TOTNUM=-C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(6)*(P2-P1)-C(3))+C(5)*P2-C(2))+P2*(C(4)+C(7)*P2)-C
     >(1)
      F19068=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19069()
*                 ~b1          Z    !  Z    ~b1                      
*                -->--\     /-2-----!---2-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  Z  |  h    !  h  | ~B1                      
*                --<--@-1---@-------!-----@--<--                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19069(C)
      TOTNUM=+C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(6)*P2-C(3))+C(5)*P2-C(2))+P2*(C(4)+C(7)*P2)-C(1)
      F19069=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1906()
*                    ~o1    c    !  c    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c2|P5     !  ~c2|-P6                          
*                    ~1+ |  S    !  S  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1906(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1906=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19070()
*                                   !  h    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          Z   |!  Z  | ~B1                      
*                -->--\     /-2----+!---2-@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  Z  |  h   |!                                
*                --<--@-1---@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19070(C)
      TOTNUM=+C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(6)*(P2-P1)-C(3))+C(5)*P2-C(2))+P2*(C(4)+C(7)*P2)-C
     >(1)
      F19070=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19071()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19071(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19071=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19072()
*                                !  Z    ~b1                         
*                               /!---1-@-->--                        
*                               |!  P3 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    h   |!  h  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  Z   |!                                   
*                   --<--@-1----/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19072(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*(P2-P1)-C(2))+C(1)*P1
      F19072=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19073()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19073(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19073=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19074()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19074(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19074=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19075()
*                                !  h    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b2|P5    |!                                   
*                    ~B1 |  h   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19075(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P2*(2*(P2-P1)-C(2))+C(1)*P1
      F19075=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19076()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1768/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1768).EQ.0) CALL CC19076(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19076=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19077()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19077(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      S1=P2**2
      RNUM=+C(2)*S1-C(1)
      F19077=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19078()
*                 ~b1    Z    !  Z          ~b1                      
*                -->--@-1-----!---1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~b1|P5     !     |     |                          
*                 ~B1 |  H    !  H  |  Z  | ~B1                      
*                --<--@-------!-----@---2-@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19078(C)
      TOTNUM=-C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(3)-C(6)*P2)+C(2)-C(5)*P2)+P2*(C(7)*P2-C(4))+C(1)
      F19078=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19079()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19079(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*(P2-P1)-C(2))+C(1)*P1
      F19079=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1907()
*                                !  S    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~s1|-P6                          
*                    ~o1    c   |!  c  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~c2|P5    |!                                   
*                    ~1+ |  S   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1907(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1907=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19080()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19080(C)
      TOTNUM=-C(4)
      TOTDEN=+C(3)
      S1=P2**2
      RNUM=+C(2)*S1-C(1)
      F19080=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19081()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19081(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*(P2-P1)-C(2))+C(1)*P1
      F19081=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19082()
*              ~b1          Z    !  Z          ~b1                   
*             -->--\     /-2-----!---2-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  H    !  H  |  Z  | ~B1                   
*             --<--@-1---@-------!-----@---3-@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19082(C)
      TOTNUM=+C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(P1*(-C(4)-C(5)*P1)-C(3))-C(2)-C(7)*P2)+P2*(C(7)*P2-C
     >(6))-C(1)
      F19082=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19083()
*                                   !  H    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b1|-P6                       
*                 ~b1          Z   |!  Z  | ~B1                      
*                -->--\     /-2----+!---2-@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  Z  |  H   |!                                
*                --<--@-1---@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19083(C)
      TOTNUM=-C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(6)*(P2-P1)-C(3))+C(5)*P2-C(2))+P2*(C(4)+C(7)*P2)-C
     >(1)
      F19083=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19084()
*                 ~b1          Z    !  Z    ~b1                      
*                -->--\     /-2-----!---2-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  Z  |  H    !  H  | ~B1                      
*                --<--@-1---@-------!-----@--<--                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19084(C)
      TOTNUM=+C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(6)*P2-C(3))+C(5)*P2-C(2))+P2*(C(4)+C(7)*P2)-C(1)
      F19084=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19085()
*                                   !  H    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          Z   |!  Z  | ~B1                      
*                -->--\     /-2----+!---2-@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  Z  |  H   |!                                
*                --<--@-1---@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19085(C)
      TOTNUM=+C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(6)*(P2-P1)-C(3))+C(5)*P2-C(2))+P2*(C(4)+C(7)*P2)-C
     >(1)
      F19085=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19086()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19086(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19086=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19087()
*                                !  Z    ~b1                         
*                               /!---1-@-->--                        
*                               |!  P3 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    H   |!  H  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  Z   |!                                   
*                   --<--@-1----/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19087(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*(P2-P1)-C(2))+C(1)*P1
      F19087=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19088()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19088(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19088=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19089()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19089(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19089=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1908()
*                                !  S    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~s2|-P6                          
*                    ~o1    c   |!  c  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~c2|P5    |!                                   
*                    ~1+ |  S   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1908(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1908=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19090()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b2|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19090(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P2*(2*(P2-P1)-C(2))+C(1)*P1
      F19090=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19091()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1769/ Q0(5),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1769).EQ.0) CALL CC19091(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19091=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19092()
*              ~b1          Z    !  Z          ~b1                   
*             -->--\     /-1-----!---1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H3   !  H3 |  h  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1770).EQ.0) CALL CC19092(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1)+C(1)
      F19092=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19093()
*              ~b1          Z    !  Z          ~b1                   
*             -->--\     /-1-----!---1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H3   !  H3 |  H  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1770).EQ.0) CALL CC19093(C)
      TOTNUM=-C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1)+C(1)
      F19093=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19094()
*                 ~b1          Z    !  Z    ~b1                      
*                -->--\     /-1-----!---1-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  h  |  H3   !  H3 | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1770).EQ.0) CALL CC19094(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(2*P2-C(2))+C(3)*P2-C(1)
      F19094=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19095()
*                                   !  H3   ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          Z   |!  Z  | ~B1                      
*                -->--\     /-1----+!---1-@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  h  |  H3  |!                                
*                --<--@-----@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1770).EQ.0) CALL CC19095(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(4*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19095=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19096()
*              ~b1          Z    !  Z          ~b1                   
*             -->--\     /-1-----!---1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  H3   !  H3 |  H  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1770).EQ.0) CALL CC19096(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1)+C(1)
      F19096=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19097()
*                 ~b1          Z    !  Z    ~b1                      
*                -->--\     /-1-----!---1-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  H  |  H3   !  H3 | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1770).EQ.0) CALL CC19097(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(2*P2-C(2))+C(3)*P2-C(1)
      F19097=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19098()
*                                   !  H3   ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          Z   |!  Z  | ~B1                      
*                -->--\     /-1----+!---1-@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  H  |  H3  |!                                
*                --<--@-----@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1770).EQ.0) CALL CC19098(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(4*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19098=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19099()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  H3   !  H3 | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1770).EQ.0) CALL CC19099(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19099=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1909()
*                    ~o1    S    !  S    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~s1|P5     !  ~s1|-P6                          
*                    ~1+ |  c    !  c  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1909(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1909=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F190()
*                                !  H3   ~o1                         
*                               /!-----@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~o4|-P6                          
*                    ~o1    Z   |!  Z  | ~o1                         
*                   =====@-1----+!---1-@=====                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o3|P5    |!                                   
*                    ~o1 |  H3  |!                                   
*                   =====@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U4/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(4).EQ.0) CALL CC190(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P2*(8*(P2-P1)+C(5))+C(2)+C(3)*P1)+P2*(C(4)-C(6)*P2)+C(1)
      F190=RNUM*(TOTNUM/TOTDEN)*Q1(7)*Q1(10)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(
     >5)*Q0(6)*Q0(8)*Q0(9)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19100()
*                                !  H3   ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    Z   |!  Z  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b2|P5    |!                                   
*                    ~B1 |  H3  |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1770).EQ.0) CALL CC19100(C)
      TOTNUM=-C(4)
      TOTDEN=+C(3)
      RNUM=+P2*(2*(P2-P1)-C(2))+C(1)*P1
      F19100=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19101()
*                    ~b1    H3   !  H3   ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  Z    !  Z  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1770/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1770).EQ.0) CALL CC19101(C)
      TOTNUM=-C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19101=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19102()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~u1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-2>----!-->2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19102(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1*(C(6)+P1-4*P2)+P2*(6*P2-C(5))-C(4))+P2*(P2*(C(5)-
     >4*P2)+C(3))-C(2))+P2*(P2*(P2*(P2-C(6))-C(4))+C(2))+C(1)
      F19102=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19103()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~c1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-2>----!-->2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19103(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1*(C(6)+P1-4*P2)+P2*(6*P2-C(5))-C(4))+P2*(P2*(C(5)-
     >4*P2)+C(3))-C(2))+P2*(P2*(P2*(P2-C(6))-C(4))+C(2))+C(1)
      F19103=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19104()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~t1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-2>----!-->2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19104(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1*(C(6)+P1-4*P2)+P2*(6*P2-C(5))-C(4))+P2*(P2*(C(5)-
     >4*P2)+C(3))-C(2))+P2*(P2*(P2*(P2-C(6))-C(4))+C(2))+C(1)
      F19104=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19105()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~t2|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-2>----!-->2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19105(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1*(C(6)+P1-4*P2)+P2*(6*P2-C(5))-C(4))+P2*(P2*(C(5)-
     >4*P2)+C(3))-C(2))+P2*(P2*(P2*(P2-C(6))-C(4))+C(2))+C(1)
      F19105=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19106()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~u1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  A  | ~B1                      
*                --<--@-2>----!-->2-@---3-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(12)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19106(C)
      TOTNUM=-C(12)
      TOTDEN=+C(11)
      RNUM=+P1*(P1*(P1*(4*P2-C(4)-P1)+P2*(-C(7)-5*P2)+C(3))+P2*(P2*(C(9)
     >+2*P2)-C(6))+C(2))+P2*(P2*(C(8)-C(10)*P2)-C(5))-C(1)
      F19106=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(8)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19107()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~u1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  Z  | ~B1                      
*                --<--@-2>----!-->2-@---3-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(16)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19107(C)
      TOTNUM=+C(16)
      TOTDEN=+C(15)
      RNUM=+P1*(P1*(P1*(C(9)*P2-C(4)-C(5)*P1)+P2*(-C(8)-C(12)*P2)+C(3))+
     >P2*(P2*(C(11)+C(14)*P2)-C(7))+C(2))+P2*(P2*(C(10)-C(13)*P2)-C(6))-
     >C(1)
      F19107=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19108()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    W-   !  W- | ~B1                         
*                   -->--@-1<----!--<1-@--<--                        
*                     P1 |  P4   !  P4 |  P2                         
*                     ~u1|P5     !     |                             
*                    ~B1 |  W+   !  W+ |                             
*                   --<--@-2>----!-->2-/                             
*                     P2    P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19108(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19108=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19109()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~u1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  h  | ~B1                      
*                --<--@-2>----!-->2-@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19109(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19109=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1910()
*                    ~o1    S    !  S    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~s1|P5     !  ~s2|-P6                          
*                    ~1+ |  c    !  c  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1910(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1910=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19110()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~u1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  H  | ~B1                      
*                --<--@-2>----!-->2-@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19110(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19110=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(7)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19111()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~c1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-2>----!-->2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19111(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1*(C(6)+P1-4*P2)+P2*(6*P2-C(5))-C(4))+P2*(P2*(C(5)-
     >4*P2)+C(3))-C(2))+P2*(P2*(P2*(P2-C(6))-C(4))+C(2))+C(1)
      F19111=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19112()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~t1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-2>----!-->2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19112(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1*(C(6)+P1-4*P2)+P2*(6*P2-C(5))-C(4))+P2*(P2*(C(5)-
     >4*P2)+C(3))-C(2))+P2*(P2*(P2*(P2-C(6))-C(4))+C(2))+C(1)
      F19112=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19113()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~t2|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-2>----!-->2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19113(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1*(C(6)+P1-4*P2)+P2*(6*P2-C(5))-C(4))+P2*(P2*(C(5)-
     >4*P2)+C(3))-C(2))+P2*(P2*(P2*(P2-C(6))-C(4))+C(2))+C(1)
      F19113=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19114()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  A  | ~B1                      
*                --<--@-2>----!-->2-@---3-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(12)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19114(C)
      TOTNUM=-C(12)
      TOTDEN=+C(11)
      RNUM=+P1*(P1*(P1*(4*P2-C(4)-P1)+P2*(-C(7)-5*P2)+C(3))+P2*(P2*(C(9)
     >+2*P2)-C(6))+C(2))+P2*(P2*(C(8)-C(10)*P2)-C(5))-C(1)
      F19114=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(8)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19115()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  Z  | ~B1                      
*                --<--@-2>----!-->2-@---3-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(16)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19115(C)
      TOTNUM=+C(16)
      TOTDEN=+C(15)
      RNUM=+P1*(P1*(P1*(C(9)*P2-C(4)-C(5)*P1)+P2*(-C(8)-C(12)*P2)+C(3))+
     >P2*(P2*(C(11)+C(14)*P2)-C(7))+C(2))+P2*(P2*(C(10)-C(13)*P2)-C(6))-
     >C(1)
      F19115=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19116()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    W-   !  W- | ~B1                         
*                   -->--@-1<----!--<1-@--<--                        
*                     P1 |  P4   !  P4 |  P2                         
*                     ~c1|P5     !     |                             
*                    ~B1 |  W+   !  W+ |                             
*                   --<--@-2>----!-->2-/                             
*                     P2    P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19116(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19116=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19117()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  h  | ~B1                      
*                --<--@-2>----!-->2-@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19117(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19117=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19118()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  H  | ~B1                      
*                --<--@-2>----!-->2-@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19118(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19118=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(7)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19119()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-2>----!-->2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19119(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1*(C(6)+P1-4*P2)+P2*(6*P2-C(5))-C(4))+P2*(P2*(C(5)-
     >4*P2)+C(3))-C(2))+P2*(P2*(P2*(P2-C(6))-C(4))+C(2))+C(1)
      F19119=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1911()
*                    ~o1    S    !  S    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~s2|P5     !  ~s2|-P6                          
*                    ~1+ |  c    !  c  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U68/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(68).EQ.0) CALL CC1911(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1911=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19120()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t2|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-2>----!-->2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19120(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1*(C(6)+P1-4*P2)+P2*(6*P2-C(5))-C(4))+P2*(P2*(C(5)-
     >4*P2)+C(3))-C(2))+P2*(P2*(P2*(P2-C(6))-C(4))+C(2))+C(1)
      F19120=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19121()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  A  | ~B1                      
*                --<--@-2>----!-->2-@---3-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(12)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19121(C)
      TOTNUM=-C(12)
      TOTDEN=+C(11)
      RNUM=+P1*(P1*(P1*(4*P2-C(4)-P1)+P2*(-C(7)-5*P2)+C(3))+P2*(P2*(C(9)
     >+2*P2)-C(6))+C(2))+P2*(P2*(C(8)-C(10)*P2)-C(5))-C(1)
      F19121=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(8)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19122()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  Z  | ~B1                      
*                --<--@-2>----!-->2-@---3-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(16)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19122(C)
      TOTNUM=+C(16)
      TOTDEN=+C(15)
      RNUM=+P1*(P1*(P1*(C(9)*P2-C(4)-C(5)*P1)+P2*(-C(8)-C(12)*P2)+C(3))+
     >P2*(P2*(C(11)+C(14)*P2)-C(7))+C(2))+P2*(P2*(C(10)-C(13)*P2)-C(6))-
     >C(1)
      F19122=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19123()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    W-   !  W- | ~B1                         
*                   -->--@-1<----!--<1-@--<--                        
*                     P1 |  P4   !  P4 |  P2                         
*                     ~t1|P5     !     |                             
*                    ~B1 |  W+   !  W+ |                             
*                   --<--@-2>----!-->2-/                             
*                     P2    P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19123(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19123=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19124()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  h  | ~B1                      
*                --<--@-2>----!-->2-@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19124(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19124=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19125()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t1|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  H  | ~B1                      
*                --<--@-2>----!-->2-@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19125(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19125=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(7)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19126()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t2|P5     !  ~t2|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-2>----!-->2-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19126(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1*(C(6)+P1-4*P2)+P2*(6*P2-C(5))-C(4))+P2*(P2*(C(5)-
     >4*P2)+C(3))-C(2))+P2*(P2*(P2*(P2-C(6))-C(4))+C(2))+C(1)
      F19126=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19127()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t2|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  A  | ~B1                      
*                --<--@-2>----!-->2-@---3-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(12)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19127(C)
      TOTNUM=-C(12)
      TOTDEN=+C(11)
      RNUM=+P1*(P1*(P1*(4*P2-C(4)-P1)+P2*(-C(7)-5*P2)+C(3))+P2*(P2*(C(9)
     >+2*P2)-C(6))+C(2))+P2*(P2*(C(8)-C(10)*P2)-C(5))-C(1)
      F19127=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(8)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19128()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t2|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  Z  | ~B1                      
*                --<--@-2>----!-->2-@---3-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(16)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19128(C)
      TOTNUM=+C(16)
      TOTDEN=+C(15)
      RNUM=+P1*(P1*(P1*(C(9)*P2-C(4)-C(5)*P1)+P2*(-C(8)-C(12)*P2)+C(3))+
     >P2*(P2*(C(11)+C(14)*P2)-C(7))+C(2))+P2*(P2*(C(10)-C(13)*P2)-C(6))-
     >C(1)
      F19128=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19129()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    W-   !  W- | ~B1                         
*                   -->--@-1<----!--<1-@--<--                        
*                     P1 |  P4   !  P4 |  P2                         
*                     ~t2|P5     !     |                             
*                    ~B1 |  W+   !  W+ |                             
*                   --<--@-2>----!-->2-/                             
*                     P2    P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19129(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19129=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1912()
*              ~o1          c    !  c          ~o1                   
*             =====\     /==>====!==>==\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  W+ |  B    !  B  |  W+ | ~1+                   
*             ==>==@-1>--@==<====!==<==@-->2-@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1912(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F1912=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19130()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t2|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  h  | ~B1                      
*                --<--@-2>----!-->2-@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19130(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19130=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19131()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t2|P5     !     |     |                          
*                 ~B1 |  W+   !  W+ |  H  | ~B1                      
*                --<--@-2>----!-->2-@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19131(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(P1-C(3)-2*P2)+P2*(P2-C(5))+C(2))+P2*(C(6)*P2-C(4))-C
     >(1)
      F19131=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19132()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-3<----!--<3-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  W+   !  W+ |  A  | ~B1                   
*             --<--@-1---@-2>----!-->2-@---4-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(11)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19132(C)
      TOTNUM=+C(11)
      TOTDEN=+C(10)
      RNUM=+P1*(P1*(P1*(C(4)+P1-4*P2)+P2*(C(7)+4*P2)-C(3))+P2*(-C(6)-C(9
     >)*P2)+C(2))+P2*(C(8)*P2-C(5))+C(1)
      F19132=RNUM*(TOTNUM/TOTDEN)*Q2(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19133()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-3<----!--<3-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  W+   !  W+ |  Z  | ~B1                   
*             --<--@-1---@-2>----!-->2-@---4-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(13)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19133(C)
      TOTNUM=-C(13)
      TOTDEN=+C(12)
      RNUM=+P1*(P1*(P1*(C(4)+C(5)*P1-C(11)*P2)+P2*(C(8)+C(11)*P2)-C(3))+
     >P2*(-C(7)-C(10)*P2)+C(2))+P2*(C(9)*P2-C(6))+C(1)
      F19133=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19134()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          W-   !  W- | ~B1                      
*                -->--\     /-3<----!--<3-@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  A  |  W+   !  W+ |                          
*                --<--@-1---@-2>----!-->2-/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19134(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(P1*(2*P2-C(3)-P1)+C(2)+C(5)*P2)+C(1)-C(4)*P2
      F19134=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19135()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-3<----!--<3-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  W+   !  W+ |  h  | ~B1                   
*             --<--@-1---@-2>----!-->2-@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19135(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(P1*(2*P2-C(3)-P1)+C(2)+C(5)*P2)+C(1)-C(4)*P2
      F19135=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19136()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-3<----!--<3-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  W+   !  W+ |  H  | ~B1                   
*             --<--@-1---@-2>----!-->2-@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19136(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(P1*(2*P2-C(3)-P1)+C(2)+C(5)*P2)+C(1)-C(4)*P2
      F19136=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19137()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-3<----!--<3-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  W+   !  W+ |  Z  | ~B1                   
*             --<--@-1---@-2>----!-->2-@---4-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(13)                                                   
      SAVE
      IF(L(1771).EQ.0) CALL CC19137(C)
      TOTNUM=+C(13)
      TOTDEN=+C(12)
      RNUM=+P1*(P1*(P1*(C(4)+C(5)*P1-C(11)*P2)+P2*(C(8)+C(11)*P2)-C(3))+
     >P2*(-C(7)-C(10)*P2)+C(2))+P2*(C(9)*P2-C(6))+C(1)
      F19137=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19138()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          W-   !  W- | ~B1                      
*                -->--\     /-3<----!--<3-@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  Z  |  W+   !  W+ |                          
*                --<--@-1---@-2>----!-->2-/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19138(C)
      TOTNUM=-C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(7)*P2-C(3)-C(4)*P1)+C(2)+C(6)*P2)+C(1)-C(5)*P2
      F19138=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19139()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-3<----!--<3-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  W+   !  W+ |  h  | ~B1                   
*             --<--@-1---@-2>----!-->2-@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19139(C)
      TOTNUM=-C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(7)*P2-C(3)-C(4)*P1)+C(2)+C(6)*P2)+C(1)-C(5)*P2
      F19139=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1913()
*              ~o1          c    !  c          ~o1                   
*             =====\     /==>====!==>==\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  W+ |  B    !  B  |  H+ | ~1+                   
*             ==>==@-1>--@==<====!==<==@-->--@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1913(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(3)*P1-C(2))+C(4)*P2-C(1)
      F1913=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19140()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-3<----!--<3-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  W+   !  W+ |  H  | ~B1                   
*             --<--@-1---@-2>----!-->2-@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(9)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19140(C)
      TOTNUM=-C(9)
      TOTDEN=+C(8)
      RNUM=+P1*(P1*(C(7)*P2-C(3)-C(4)*P1)+C(2)+C(6)*P2)+C(1)-C(5)*P2
      F19140=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19141()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  W-   !  W- | ~B1                         
*                   --<--@-2<----!--<2-@--<--                        
*                     P2 |  P4   !  P4 |  P2                         
*                        |       !     |                             
*                        |  W+   !  W+ |                             
*                        \-1>----!-->1-/                             
*                           P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19141(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(P1-C(2))+C(1)
      F19141=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19142()
*                 ~b1         !                                      
*                -->--\       !                                      
*                  P1 |       !                                      
*                     |       !                                      
*                 ~B1 |  W-   !  W-         ~b1                      
*                --<--@-2<----!--<2-\     /-->--                     
*                  P2 |  P4   !  P4 |     |  P1                      
*                     |       !     |     |                          
*                     |  W+   !  W+ |  h  | ~B1                      
*                     \-1>----!-->1-@-----@--<--                     
*                        P3   !  P3   -P5    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19142(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(P1-C(2))+C(1)
      F19142=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19143()
*                 ~b1         !                                      
*                -->--\       !                                      
*                  P1 |       !                                      
*                     |       !                                      
*                 ~B1 |  W-   !  W-         ~b1                      
*                --<--@-2<----!--<2-\     /-->--                     
*                  P2 |  P4   !  P4 |     |  P1                      
*                     |       !     |     |                          
*                     |  W+   !  W+ |  H  | ~B1                      
*                     \-1>----!-->1-@-----@--<--                     
*                        P3   !  P3   -P5    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19143(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(P1-C(2))+C(1)
      F19143=RNUM*(TOTNUM/TOTDEN)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19144()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-2<----!--<2-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  W+   !  W+ |  h  | ~B1                   
*             --<--@-----@-1>----!-->1-@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19144(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(P1-C(2))+C(1)
      F19144=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19145()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-2<----!--<2-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  W+   !  W+ |  H  | ~B1                   
*             --<--@-----@-1>----!-->1-@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19145(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(P1-C(2))+C(1)
      F19145=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19146()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-2<----!--<2-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  W+   !  W+ |  H  | ~B1                   
*             --<--@-----@-1>----!-->1-@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1771/ Q0(7),Q1(8),Q2(8)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1771).EQ.0) CALL CC19146(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(P1-C(2))+C(1)
      F19146=RNUM*(TOTNUM/TOTDEN)*Q2(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19147()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~u1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-1>----!-->1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19147(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19147=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19148()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~c1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-1>----!-->1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19148(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19148=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19149()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~t1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-1>----!-->1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19149(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19149=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1914()
*                 ~o1          c    !  c    ~o1                      
*                =====\     /==>====!==>==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~c1|-P6                       
*                 ~1+ |  W+ |  B    !  B  | ~1+                      
*                ==>==@-1>--@==<====!==<==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1914(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(C(4)*P1-C(3)+C(5)*P2)+C(1)+C(2)*P1
      F1914=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19150()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~t2|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-1>----!-->1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19150(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19150=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19151()
*                             !  W+         ~b1                      
*                            /!-->1-\     /-->--                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    H-  |!  H- |  h  | ~B1                      
*                -->--@--<---+!--<--@-----@--<--                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                  ~u1|P5    |!                                      
*                 ~B1 |  W+  |!                                      
*                --<--@-1>---/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19151(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(4*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19151=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19152()
*                             !  W+         ~b1                      
*                            /!-->1-\     /-->--                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    H-  |!  H- |  H  | ~B1                      
*                -->--@--<---+!--<--@-----@--<--                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                  ~u1|P5    |!                                      
*                 ~B1 |  W+  |!                                      
*                --<--@-1>---/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19152(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(4*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19152=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19153()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~c1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-1>----!-->1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19153(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19153=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19154()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~t1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-1>----!-->1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19154(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19154=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19155()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~t2|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-1>----!-->1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19155(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19155=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19156()
*                             !  W+         ~b1                      
*                            /!-->1-\     /-->--                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    H-  |!  H- |  h  | ~B1                      
*                -->--@--<---+!--<--@-----@--<--                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                  ~c1|P5    |!                                      
*                 ~B1 |  W+  |!                                      
*                --<--@-1>---/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19156(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(4*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19156=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19157()
*                             !  W+         ~b1                      
*                            /!-->1-\     /-->--                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    H-  |!  H- |  H  | ~B1                      
*                -->--@--<---+!--<--@-----@--<--                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                  ~c1|P5    |!                                      
*                 ~B1 |  W+  |!                                      
*                --<--@-1>---/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19157(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(4*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19157=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19158()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t1|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-1>----!-->1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19158(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19158=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19159()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t2|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-1>----!-->1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19159(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19159=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1915()
*                 ~o1          c    !  c    ~o1                      
*                =====\     /==>====!==>==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~c2|-P6                       
*                 ~1+ |  W+ |  B    !  B  | ~1+                      
*                ==>==@-1>--@==<====!==<==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1915(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(C(4)*P1-C(3)+C(5)*P2)+C(1)+C(2)*P1
      F1915=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19160()
*                             !  W+         ~b1                      
*                            /!-->1-\     /-->--                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    H-  |!  H- |  h  | ~B1                      
*                -->--@--<---+!--<--@-----@--<--                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                  ~t1|P5    |!                                      
*                 ~B1 |  W+  |!                                      
*                --<--@-1>---/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19160(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(4*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19160=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19161()
*                             !  W+         ~b1                      
*                            /!-->1-\     /-->--                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    H-  |!  H- |  H  | ~B1                      
*                -->--@--<---+!--<--@-----@--<--                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                  ~t1|P5    |!                                      
*                 ~B1 |  W+  |!                                      
*                --<--@-1>---/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19161(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(4*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19161=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19162()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t2|P5     !  ~t2|-P6                          
*                    ~B1 |  W+   !  W+ | ~B1                         
*                   --<--@-1>----!-->1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19162(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(C(2)+4*P1-8*P2)+P2*(4*P2-C(2))+C(1)
      F19162=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19163()
*                             !  W+         ~b1                      
*                            /!-->1-\     /-->--                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    H-  |!  H- |  h  | ~B1                      
*                -->--@--<---+!--<--@-----@--<--                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                  ~t2|P5    |!                                      
*                 ~B1 |  W+  |!                                      
*                --<--@-1>---/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19163(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(4*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19163=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19164()
*                             !  W+         ~b1                      
*                            /!-->1-\     /-->--                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    H-  |!  H- |  H  | ~B1                      
*                -->--@--<---+!--<--@-----@--<--                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                  ~t2|P5    |!                                      
*                 ~B1 |  W+  |!                                      
*                --<--@-1>---/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19164(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(4*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19164=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19165()
*              ~b1          W+   !  W+         ~b1                   
*             -->--\     /-1>----!-->1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H-   !  H- |  h  | ~B1                   
*             --<--@-----@--<----!--<--@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19165(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(4*P1-C(2))+C(1)
      F19165=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19166()
*              ~b1          W+   !  W+         ~b1                   
*             -->--\     /-1>----!-->1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H-   !  H- |  H  | ~B1                   
*             --<--@-----@--<----!--<--@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19166(C)
      TOTNUM=-C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(4*P1-C(2))+C(1)
      F19166=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19167()
*              ~b1          W+   !  W+         ~b1                   
*             -->--\     /-1>----!-->1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  H-   !  H- |  H  | ~B1                   
*             --<--@-----@--<----!--<--@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1772/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1772).EQ.0) CALL CC19167(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(4*P1-C(2))+C(1)
      F19167=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19168()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~u1|P5     !  ~u1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19168(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19168=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19169()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~u1|P5     !  ~c1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19169(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19169=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1916()
*                                   !  B    ~o1                      
*                                  /!==<==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b1|-P6                       
*                 ~o1          c   |!  c  | ~1+                      
*                =====\     /==>===+!==>==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  W+ |  B   |!                                
*                ==>==@-1>--@==<===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1916(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F1916=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19170()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~u1|P5     !  ~t1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19170(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19170=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19171()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~u1|P5     !  ~t2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19171(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19171=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19172()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~u1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  h  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19172(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(2*P2-C(2))+C(3)*P2-C(1)
      F19172=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19173()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~u1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  H  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19173(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(2*P2-C(2))+C(3)*P2-C(1)
      F19173=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19174()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c1|P5     !  ~c1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19174(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19174=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19175()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c1|P5     !  ~t1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19175(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19175=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19176()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c1|P5     !  ~t2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19176(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19176=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19177()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~c1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  h  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19177(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(2*P2-C(2))+C(3)*P2-C(1)
      F19177=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19178()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~c1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  H  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19178(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(2*P2-C(2))+C(3)*P2-C(1)
      F19178=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19179()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t1|P5     !  ~t1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19179(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19179=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1917()
*                                   !  B    ~o1                      
*                                  /!==<==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~o1          c   |!  c  | ~1+                      
*                =====\     /==>===+!==>==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  W+ |  B   |!                                
*                ==>==@-1>--@==<===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1917(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F1917=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19180()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t1|P5     !  ~t2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19180(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19180=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19181()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~t1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  h  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19181(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(2*P2-C(2))+C(3)*P2-C(1)
      F19181=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19182()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~t1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  H  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19182(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(2*P2-C(2))+C(3)*P2-C(1)
      F19182=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19183()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t2|P5     !  ~t2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19183(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      S1=P2**2
      RNUM=+S1-C(1)
      F19183=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19184()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~t2|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  h  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19184(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(2*P2-C(2))+C(3)*P2-C(1)
      F19184=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19185()
*                 ~b1    W-   !  W-         ~b1                      
*                -->--@-1<----!--<1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~t2|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  H  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19185(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(2*P2-C(2))+C(3)*P2-C(1)
      F19185=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19186()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-1<----!--<1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H+   !  H+ |  h  | ~B1                   
*             --<--@-----@-->----!-->--@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19186(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(4*P1-C(2))+C(1)
      F19186=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19187()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-1<----!--<1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H+   !  H+ |  H  | ~B1                   
*             --<--@-----@-->----!-->--@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19187(C)
      TOTNUM=-C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(4*P1-C(2))+C(1)
      F19187=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19188()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-1<----!--<1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  H+   !  H+ |  H  | ~B1                   
*             --<--@-----@-->----!-->--@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1773/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1773).EQ.0) CALL CC19188(C)
      TOTNUM=+C(4)
      TOTDEN=+C(3)
      RNUM=+P1*(4*P1-C(2))+C(1)
      F19188=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19189()
*                    ~b1    G    !  G    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  G    !  G  | ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1774/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1774).EQ.0) CALL CC19189(C)
      S1=A(146)**4
      TOTNUM=+256*S1
      TOTDEN=+27
      RNUM=+P2*(P2-C(2))+C(1)
      F19189=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1918()
*              ~o1          c    !  c          ~o1                   
*             =====\     /==>====!==>==\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  H+ |  B    !  B  |  H+ | ~1+                   
*             ==>==@-->--@==<====!==<==@-->--@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1918(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(C(2)+C(3)*P1)-C(1)
      F1918=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19190()
*                                !  G    ~b1                         
*                               /!---2-@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    G   |!  G  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  G   |!                                   
*                   --<--@-2----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1774/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1774).EQ.0) CALL CC19190(C)
      S1=A(146)**4
      TOTNUM=-8*S1
      TOTDEN=+27
      RNUM=+P1*(P1-C(2))+C(1)
      F19190=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19191()
*                 ~b1    G    !  G          ~b1                      
*                -->--@-1-----!---1-\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~b1|P5     !     |     |                          
*                 ~B1 |  G    !  G  |  G  | ~B1                      
*                --<--@-2-----!---2-@---3-@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1774/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1774).EQ.0) CALL CC19191(C)
      S1=A(146)**4
      TOTNUM=-8*S1
      TOTDEN=+3
      S2=P2**2
      RNUM=+P1*(P1-C(2))+4*S2-C(1)
      F19191=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19192()
*                    ~b1    G    !  G    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~0D|-P6                          
*                    ~B1 |  G    !  G  3 ~B1                         
*                   --<--@-2-----!---2-@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1774/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1774).EQ.0) CALL CC19192(C)
      S1=A(146)**4
      TOTNUM=-32*S1
      TOTDEN=+27
      RNUM=+P1-C(1)+4*P2
      F19192=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19193()
*                                !  G    ~b1                         
*                               /!---2-@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~0D|-P6                          
*                    ~b1    G   |!  G  3 ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  G   |!                                   
*                   --<--@-2----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1774/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1774).EQ.0) CALL CC19193(C)
      S1=A(146)**4
      TOTNUM=+4*S1
      TOTDEN=+27
      RNUM=+P1-C(1)+4*P2
      F19193=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19194()
*              ~b1          G    !  G          ~b1                   
*             -->--\     /-3-----!---3-\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  G  |  G    !  G  |  G  | ~B1                   
*             --<--@-1---@-2-----!---2-@---4-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1774/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1774).EQ.0) CALL CC19194(C)
      S1=A(146)**4
      TOTNUM=+16*S1
      TOTDEN=+3
      RNUM=+P1*(C(2)-P1-4*P2)+P2*(4*P2-C(3))+C(1)
      F19194=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19195()
*                 ~b1          G    !  G    ~b1                      
*                -->--\     /-3-----!---3-@-->--                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~0D|-P6                       
*                 ~B1 |  G  |  G    !  G  4 ~B1                      
*                --<--@-1---@-2-----!---2-@--<--                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1774/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1774).EQ.0) CALL CC19195(C)
      S1=A(146)**4
      TOTNUM=-8*S1
      TOTDEN=+1
      RNUM=+2*P2-C(1)-P1
      F19195=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19196()
*                    ~b1    G    !  G    ~b1                         
*                   -->--@-2-----!---2-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~0D|P5     !  ~0D|-P6                          
*                    ~B1 1  G    !  G  4 ~B1                         
*                   --<--@-3-----!---3-@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1774/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1774).EQ.0) CALL CC19196(C)
      S1=A(146)**4
      TOTNUM=+64*S1
      TOTDEN=+27
      RNUM=+1
      F19196=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19197()
*                                !  G    ~b1                         
*                               /!---3-@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~0D|-P6                          
*                    ~b1    G   |!  G  4 ~B1                         
*                   -->--@-2----+!---2-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~0D|P5    |!                                   
*                    ~B1 1  G   |!                                   
*                   --<--@-3----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1774/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1774).EQ.0) CALL CC19197(C)
      S1=A(146)**4
      TOTNUM=-8*S1
      TOTDEN=+27
      RNUM=+1
      F19197=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19198()
*                    ~b1    G    !  G    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1775/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1775).EQ.0) CALL CC19198(C)
      S1=A(146)**2
      TOTNUM=+C(2)*S1
      TOTDEN=+9
      RNUM=+P2-C(1)
      F19198=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19199()
*                                !  h    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    G   |!  G  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  h   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1775/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1775).EQ.0) CALL CC19199(C)
      S1=A(146)**2
      TOTNUM=+C(2)*S1
      TOTDEN=+9
      RNUM=+2*P1-C(1)
      F19199=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1919()
*                 ~o1          c    !  c    ~o1                      
*                =====\     /==>====!==>==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~c1|-P6                       
*                 ~1+ |  H+ |  B    !  B  | ~1+                      
*                ==>==@-->--@==<====!==<==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1919(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*P2-C(2))+C(3)*P2-C(1)
      F1919=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F191()
*                    ~o1    H3   !  H3   ~o1                         
*                   =====@-------!-----@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~o3|P5     !  ~o3|-P6                          
*                    ~o1 |  Z    !  Z  | ~o1                         
*                   =====@-1-----!---1-@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U4/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(4).EQ.0) CALL CC191(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+4*P1-8*P2)+P2*(C(5)+4*P2)+C(2))+P2*(C(4)-C(6)*P
     >2)+C(1)
      F191=RNUM*(TOTNUM/TOTDEN)*Q2(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6
     >)*Q0(7)*Q0(9)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19200()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  G    !  G  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1775/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1775).EQ.0) CALL CC19200(C)
      S1=A(146)**2
      TOTNUM=-C(2)*S1
      TOTDEN=+9
      RNUM=+2*(P2-P1)+C(1)
      F19200=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19201()
*                    ~b1    G    !  G    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1776/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1776).EQ.0) CALL CC19201(C)
      S1=A(146)**2
      TOTNUM=+C(2)*S1
      TOTDEN=+9
      RNUM=+P2-C(1)
      F19201=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19202()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    G   |!  G  | ~B1                         
*                   -->--@-1----+!---1-@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1776/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1776).EQ.0) CALL CC19202(C)
      S1=A(146)**2
      TOTNUM=+C(2)*S1
      TOTDEN=+9
      RNUM=+2*P1-C(1)
      F19202=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19203()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  G    !  G  | ~B1                         
*                   --<--@-1-----!---1-@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1776/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1776).EQ.0) CALL CC19203(C)
      S1=A(146)**2
      TOTNUM=-C(2)*S1
      TOTDEN=+9
      RNUM=+2*(P2-P1)+C(1)
      F19203=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19204()
*              ~b1          N1   !  N1         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  n1   !  n1 |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1777/ Q0(1),Q1(1),Q2(1)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1777).EQ.0) CALL CC19204(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19204=RNUM*(TOTNUM/TOTDEN)*Q2(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19205()
*              ~b1          N2   !  N2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  n2   !  n2 |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1778/ Q0(1),Q1(1),Q2(1)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1778).EQ.0) CALL CC19205(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19205=RNUM*(TOTNUM/TOTDEN)*Q2(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19206()
*              ~b1          N3   !  N3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  n3   !  n3 |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1779/ Q0(1),Q1(1),Q2(1)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1779).EQ.0) CALL CC19206(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19206=RNUM*(TOTNUM/TOTDEN)*Q2(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19207()
*              ~b1          E1   !  E1         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  e1   !  e1 |  A  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1780/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1780).EQ.0) CALL CC19207(C)
      TOTNUM=-C(4)
      TOTDEN=+27
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19207=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19208()
*              ~b1          E1   !  E1         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  e1   !  e1 |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1780/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1780).EQ.0) CALL CC19208(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19208=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19209()
*              ~b1          E1   !  E1         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  e1   !  e1 |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1780/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1780).EQ.0) CALL CC19209(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19209=RNUM*(TOTNUM/TOTDEN)*Q2(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1920()
*                 ~o1          c    !  c    ~o1                      
*                =====\     /==>====!==>==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~c2|-P6                       
*                 ~1+ |  H+ |  B    !  B  | ~1+                      
*                ==>==@-->--@==<====!==<==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1920(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*P2-C(2))+C(3)*P2-C(1)
      F1920=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19210()
*              ~b1          E2   !  E2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  e2   !  e2 |  A  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1781).EQ.0) CALL CC19210(C)
      TOTNUM=-C(4)
      TOTDEN=+27
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19210=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19211()
*              ~b1          E2   !  E2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  e2   !  e2 |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1781).EQ.0) CALL CC19211(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19211=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19212()
*              ~b1          E2   !  E2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  e2   !  e2 |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1781).EQ.0) CALL CC19212(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19212=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(2)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19213()
*              ~b1          E2   !  E2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  e2   !  e2 |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1781).EQ.0) CALL CC19213(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19213=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19214()
*              ~b1          E2   !  E2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  e2   !  e2 |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1781).EQ.0) CALL CC19214(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+C(4)*P1)+C(2)-C(6)*P2)+P2*(C(6)*P2-C(5))+C(1)
      F19214=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19215()
*              ~b1          E2   !  E2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  e2   !  e2 |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1781).EQ.0) CALL CC19215(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19215=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19216()
*              ~b1          E2   !  E2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  e2   !  e2 |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1781).EQ.0) CALL CC19216(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19216=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19217()
*              ~b1          E2   !  E2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  e2   !  e2 |  h  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1781).EQ.0) CALL CC19217(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19217=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19218()
*              ~b1          E2   !  E2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  e2   !  e2 |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1781).EQ.0) CALL CC19218(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19218=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19219()
*              ~b1          E2   !  E2         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  e2   !  e2 |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1781/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1781).EQ.0) CALL CC19219(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19219=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1921()
*                                   !  B    ~o1                      
*                                  /!==<==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b1|-P6                       
*                 ~o1          c   |!  c  | ~1+                      
*                =====\     /==>===+!==>==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  H+ |  B   |!                                
*                ==>==@-->--@==<===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1921(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F1921=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19220()
*              ~b1          E3   !  E3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  e3   !  e3 |  A  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1782).EQ.0) CALL CC19220(C)
      TOTNUM=-C(4)
      TOTDEN=+27
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19220=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19221()
*              ~b1          E3   !  E3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  e3   !  e3 |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1782).EQ.0) CALL CC19221(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19221=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19222()
*              ~b1          E3   !  E3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  e3   !  e3 |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1782).EQ.0) CALL CC19222(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19222=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(2)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19223()
*              ~b1          E3   !  E3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  e3   !  e3 |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1782).EQ.0) CALL CC19223(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19223=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19224()
*              ~b1          E3   !  E3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  e3   !  e3 |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1782).EQ.0) CALL CC19224(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+C(4)*P1)+C(2)-C(6)*P2)+P2*(C(6)*P2-C(5))+C(1)
      F19224=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19225()
*              ~b1          E3   !  E3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  e3   !  e3 |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1782).EQ.0) CALL CC19225(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19225=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19226()
*              ~b1          E3   !  E3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  e3   !  e3 |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1782).EQ.0) CALL CC19226(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19226=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19227()
*              ~b1          E3   !  E3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  e3   !  e3 |  h  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1782).EQ.0) CALL CC19227(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19227=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19228()
*              ~b1          E3   !  E3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  e3   !  e3 |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1782).EQ.0) CALL CC19228(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19228=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19229()
*              ~b1          E3   !  E3         ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  e3   !  e3 |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1782/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1782).EQ.0) CALL CC19229(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19229=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1922()
*                                   !  B    ~o1                      
*                                  /!==<==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~o1          c   |!  c  | ~1+                      
*                =====\     /==>===+!==>==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  H+ |  B   |!                                
*                ==>==@-->--@==<===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1922(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F1922=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19230()
*                    ~b1    u    !  u    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~B1 |  U    !  U  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19230(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19230=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19231()
*                    ~b1    u    !  u    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~B1 |  U    !  U  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19231(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19231=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19232()
*                             !  U          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    u   |!  u  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  U   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19232(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19232=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19233()
*                             !  U          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    u   |!  u  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  U   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19233(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19233=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19234()
*                             !  U          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    u   |!  u  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  U   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19234(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19234=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19235()
*                    ~b1    u    !  u    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~2+|P5     !  ~2+|-P6                          
*                    ~B1 |  U    !  U  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19235(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19235=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19236()
*                             !  U          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    u   |!  u  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  U   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19236(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19236=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19237()
*                             !  U          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    u   |!  u  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  U   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19237(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19237=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19238()
*                             !  U          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    u   |!  u  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  U   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19238(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19238=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19239()
*              ~b1          U    !  U          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  u    !  u  |  A  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19239(C)
      TOTNUM=-C(4)
      TOTDEN=+81
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19239=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1923()
*                    ~o1    c    !  c    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c1|P5     !  ~c1|-P6                          
*                    ~1+ |  B    !  B  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1923(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1923=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19240()
*              ~b1          U    !  U          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  u    !  u  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19240(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19240=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19241()
*              ~b1          U    !  U          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  u    !  u  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19241(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19241=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19242()
*              ~b1          U    !  U          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  G  |  u    !  u  |  G  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1783/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1783).EQ.0) CALL CC19242(C)
      S1=A(146)**4
      TOTNUM=-32*S1
      TOTDEN=+9
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19242=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19243()
*                    ~b1    u    !  u    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~B1 |  C    !  C  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1784/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1784).EQ.0) CALL CC19243(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19243=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19244()
*                    ~b1    u    !  u    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~B1 |  C    !  C  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1784/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1784).EQ.0) CALL CC19244(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19244=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19245()
*                    ~b1    u    !  u    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~2+|P5     !  ~2+|-P6                          
*                    ~B1 |  C    !  C  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1784/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1784).EQ.0) CALL CC19245(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19245=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19246()
*                    ~b1    u    !  u    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~B1 |  T    !  T  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1785/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1785).EQ.0) CALL CC19246(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19246=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19247()
*                    ~b1    u    !  u    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~B1 |  T    !  T  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1785/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1785).EQ.0) CALL CC19247(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19247=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19248()
*                    ~b1    u    !  u    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~2+|P5     !  ~2+|-P6                          
*                    ~B1 |  T    !  T  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1785/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1785).EQ.0) CALL CC19248(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19248=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19249()
*                    ~b1    c    !  c    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~B1 |  U    !  U  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1786/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1786).EQ.0) CALL CC19249(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19249=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1924()
*                    ~o1    c    !  c    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c1|P5     !  ~c2|-P6                          
*                    ~1+ |  B    !  B  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1924(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1924=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19250()
*                    ~b1    c    !  c    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~B1 |  U    !  U  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1786/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1786).EQ.0) CALL CC19250(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19250=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19251()
*                    ~b1    c    !  c    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~2+|P5     !  ~2+|-P6                          
*                    ~B1 |  U    !  U  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1786/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1786).EQ.0) CALL CC19251(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19251=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19252()
*                    ~b1    t    !  t    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~B1 |  U    !  U  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1787/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1787).EQ.0) CALL CC19252(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19252=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19253()
*                    ~b1    t    !  t    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~B1 |  U    !  U  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1787/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1787).EQ.0) CALL CC19253(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19253=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19254()
*                    ~b1    t    !  t    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~2+|P5     !  ~2+|-P6                          
*                    ~B1 |  U    !  U  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1787/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1787).EQ.0) CALL CC19254(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19254=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19255()
*              ~b1          D    !  D          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  d    !  d  |  A  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1788/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1788).EQ.0) CALL CC19255(C)
      TOTNUM=-C(4)
      TOTDEN=+81
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19255=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19256()
*              ~b1          D    !  D          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  d    !  d  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1788/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1788).EQ.0) CALL CC19256(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19256=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19257()
*              ~b1          D    !  D          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  d    !  d  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1788/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1788).EQ.0) CALL CC19257(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19257=RNUM*(TOTNUM/TOTDEN)*Q2(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19258()
*              ~b1          D    !  D          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  G  |  d    !  d  |  G  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1788/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1788).EQ.0) CALL CC19258(C)
      S1=A(146)**4
      TOTNUM=-32*S1
      TOTDEN=+9
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19258=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19259()
*                    ~b1    c    !  c    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~B1 |  C    !  C  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19259(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19259=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1925()
*                                !  B    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~o1    c   |!  c  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~c1|P5    |!                                   
*                    ~1+ |  B   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1925(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1925=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19260()
*                    ~b1    c    !  c    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~B1 |  C    !  C  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19260(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19260=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19261()
*                             !  C          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    c   |!  c  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  C   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19261(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19261=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19262()
*                             !  C          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    c   |!  c  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  C   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19262(C)
      TOTNUM=-C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(C(2)-C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))+C(1)
      F19262=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19263()
*                             !  C          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    c   |!  c  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  C   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19263(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19263=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19264()
*                             !  C          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    c   |!  c  |  h  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  C   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19264(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19264=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19265()
*                             !  C          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    c   |!  c  |  H  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  C   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19265(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19265=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19266()
*                    ~b1    c    !  c    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~2+|P5     !  ~2+|-P6                          
*                    ~B1 |  C    !  C  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19266(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19266=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19267()
*                             !  C          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    c   |!  c  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  C   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19267(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19267=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19268()
*                             !  C          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    c   |!  c  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  C   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19268(C)
      TOTNUM=-C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(C(2)-C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))+C(1)
      F19268=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19269()
*                             !  C          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    c   |!  c  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  C   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19269(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19269=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1926()
*                                !  B    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~o1    c   |!  c  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~c1|P5    |!                                   
*                    ~1+ |  B   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1926(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1926=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19270()
*                             !  C          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    c   |!  c  |  h  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  C   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19270(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19270=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19271()
*                             !  C          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    c   |!  c  |  H  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  C   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19271(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19271=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19272()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  c    !  c  |  A  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19272(C)
      TOTNUM=-C(4)
      TOTDEN=+81
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19272=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19273()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  c    !  c  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19273(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19273=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19274()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  c    !  c  |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19274(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19274=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19275()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  c    !  c  |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19275(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19275=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19276()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  c    !  c  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19276(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+C(4)*P1)+C(2)-C(6)*P2)+P2*(C(6)*P2-C(5))+C(1)
      F19276=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19277()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  c    !  c  |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19277(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19277=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19278()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  c    !  c  |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19278(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19278=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19279()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  G  |  c    !  c  |  G  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19279(C)
      S1=A(146)**4
      TOTNUM=-32*S1
      TOTDEN=+9
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19279=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1927()
*                    ~o1    c    !  c    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c2|P5     !  ~c2|-P6                          
*                    ~1+ |  B    !  B  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1927(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1927=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19280()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  c    !  c  |  h  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19280(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19280=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19281()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  c    !  c  |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19281(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19281=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19282()
*              ~b1          C    !  C          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  c    !  c  |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1789/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1789).EQ.0) CALL CC19282(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19282=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19283()
*                    ~b1    c    !  c    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~B1 |  T    !  T  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1790/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1790).EQ.0) CALL CC19283(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19283=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19284()
*                    ~b1    c    !  c    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~B1 |  T    !  T  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1790/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1790).EQ.0) CALL CC19284(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19284=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19285()
*                    ~b1    c    !  c    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~2+|P5     !  ~2+|-P6                          
*                    ~B1 |  T    !  T  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1790/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1790).EQ.0) CALL CC19285(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19285=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19286()
*                    ~b1    t    !  t    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~B1 |  C    !  C  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1791/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1791).EQ.0) CALL CC19286(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19286=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19287()
*                    ~b1    t    !  t    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~B1 |  C    !  C  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1791/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1791).EQ.0) CALL CC19287(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19287=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19288()
*                    ~b1    t    !  t    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~2+|P5     !  ~2+|-P6                          
*                    ~B1 |  C    !  C  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1791/ Q0(2),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1791).EQ.0) CALL CC19288(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19288=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19289()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  s    !  s  |  A  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19289(C)
      TOTNUM=-C(4)
      TOTDEN=+81
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19289=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1928()
*                                !  B    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~o1    c   |!  c  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~c2|P5    |!                                   
*                    ~1+ |  B   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1928(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1928=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19290()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  s    !  s  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19290(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19290=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19291()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  s    !  s  |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19291(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19291=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(2)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19292()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  s    !  s  |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19292(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19292=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19293()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  s    !  s  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19293(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+C(4)*P1)+C(2)-C(6)*P2)+P2*(C(6)*P2-C(5))+C(1)
      F19293=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19294()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  s    !  s  |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19294(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19294=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19295()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  s    !  s  |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19295(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19295=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19296()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  G  |  s    !  s  |  G  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19296(C)
      S1=A(146)**4
      TOTNUM=-32*S1
      TOTDEN=+9
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19296=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19297()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  s    !  s  |  h  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19297(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19297=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19298()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  s    !  s  |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19298(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19298=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19299()
*              ~b1          S    !  S          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  s    !  s  |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1792/ Q0(3),Q1(4),Q2(4)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1792).EQ.0) CALL CC19299(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19299=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1929()
*                                !  B    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~o1    c   |!  c  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~c2|P5    |!                                   
*                    ~1+ |  B   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1929(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1929=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F192()
*                                !  Z    ~o1                         
*                               /!---1-@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~o4|-P6                          
*                    ~o1    H3  |!  H3 | ~o1                         
*                   =====@------+!-----@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~o3|P5    |!                                   
*                    ~o1 |  Z   |!                                   
*                   =====@-1----/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U4/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(4).EQ.0) CALL CC192(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P2*(8*(P2-P1)+C(5))+C(2)+C(3)*P1)+P2*(C(4)-C(6)*P2)+C(1)
      F192=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5
     >)*Q0(6)*Q0(7)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19300()
*                    ~b1    t    !  t    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~B1 |  T    !  T  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19300(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19300=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19301()
*                    ~b1    t    !  t    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~B1 |  T    !  T  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19301(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19301=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19302()
*                             !  T          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    t   |!  t  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  T   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19302(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19302=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19303()
*                             !  T          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    t   |!  t  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  T   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19303(C)
      TOTNUM=-C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(C(2)-C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))+C(1)
      F19303=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19304()
*                             !  T          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    t   |!  t  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  T   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19304(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19304=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19305()
*                             !  T          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    t   |!  t  |  h  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  T   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19305(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19305=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19306()
*                             !  T          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    t   |!  t  |  H  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~B1 |  T   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19306(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19306=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19307()
*                    ~b1    t    !  t    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~2+|P5     !  ~2+|-P6                          
*                    ~B1 |  T    !  T  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19307(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19307=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19308()
*                             !  T          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    t   |!  t  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  T   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19308(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19308=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19309()
*                             !  T          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    t   |!  t  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  T   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19309(C)
      TOTNUM=-C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(C(2)-C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))+C(1)
      F19309=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1930()
*                    ~o1    B    !  B    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~1+ |  c    !  c  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1930(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1930=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19310()
*                             !  T          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    t   |!  t  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  T   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19310(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19310=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19311()
*                             !  T          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    t   |!  t  |  h  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  T   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19311(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19311=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19312()
*                             !  T          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    t   |!  t  |  H  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~2+|P5    |!                                      
*                 ~B1 |  T   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19312(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19312=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19313()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  t    !  t  |  A  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19313(C)
      TOTNUM=-C(4)
      TOTDEN=+81
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19313=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19314()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  t    !  t  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19314(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19314=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19315()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  t    !  t  |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19315(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19315=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19316()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  t    !  t  |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19316(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19316=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19317()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  t    !  t  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19317(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+C(4)*P1)+C(2)-C(6)*P2)+P2*(C(6)*P2-C(5))+C(1)
      F19317=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19318()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  t    !  t  |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19318(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19318=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19319()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  t    !  t  |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19319(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19319=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1931()
*                    ~o1    B    !  B    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~1+ |  c    !  c  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1931(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1931=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19320()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  G  |  t    !  t  |  G  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19320(C)
      S1=A(146)**4
      TOTNUM=-32*S1
      TOTDEN=+9
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19320=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19321()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  t    !  t  |  h  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19321(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19321=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19322()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  t    !  t  |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19322(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19322=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19323()
*              ~b1          T    !  T          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  t    !  t  |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1793/ Q0(5),Q1(6),Q2(6)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1793).EQ.0) CALL CC19323(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19323=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19324()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o1|P5     !  ~o1|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19324(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19324=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19325()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o1|P5     !  ~o2|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19325(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19325=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19326()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o1|P5     !  ~o3|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19326(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19326=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19327()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o1|P5     !  ~o4|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19327(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19327=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19328()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o1|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19328(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19328=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(9)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19329()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o1|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19329(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(C(2)-C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))+C(1)
      F19329=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1932()
*                    ~o1    B    !  B    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~1+ |  c    !  c  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U69/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(69).EQ.0) CALL CC1932(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1932=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19330()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o1|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19330(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19330=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(9)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19331()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  h  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o1|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19331(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19331=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19332()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  H  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o1|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19332(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19332=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(7)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19333()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o2|P5     !  ~o2|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19333(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19333=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19334()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o2|P5     !  ~o3|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19334(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19334=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19335()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o2|P5     !  ~o4|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19335(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19335=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19336()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o2|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19336(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19336=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(9)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19337()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o2|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19337(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(C(2)-C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))+C(1)
      F19337=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19338()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o2|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19338(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19338=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(9)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19339()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  h  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o2|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19339(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19339=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1933()
*              ~o1          S    !  S          ~o1                   
*             =====\     /==<====!==<==\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  W+ |  t    !  t  |  W+ | ~1+                   
*             ==>==@-1>--@==>====!==>==@-->2-@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1933(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F1933=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19340()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  H  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o2|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19340(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19340=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(7)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19341()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o3|P5     !  ~o3|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19341(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19341=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19342()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o3|P5     !  ~o4|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19342(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19342=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19343()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o3|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19343(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19343=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(9)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19344()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o3|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19344(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(C(2)-C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))+C(1)
      F19344=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19345()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o3|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19345(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19345=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(9)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19346()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  h  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o3|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19346(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19346=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19347()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  H  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o3|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19347(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19347=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(7)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19348()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o4|P5     !  ~o4|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19348(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19348=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19349()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o4|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19349(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19349=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1934()
*              ~o1          S    !  S          ~o1                   
*             =====\     /==<====!==<==\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  W+ |  t    !  t  |  H+ | ~1+                   
*             ==>==@-1>--@==>====!==>==@-->--@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1934(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(3)*P1-C(2))+C(4)*P2-C(1)
      F1934=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19350()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o4|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19350(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(C(2)-C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))+C(1)
      F19350=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19351()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o4|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19351(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19351=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19352()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  h  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o4|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19352(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19352=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19353()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  H  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                  ~o4|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19353(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19353=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19354()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                      ~g|P5     !   ~g|-P6                          
*                    ~B1 |  B    !  B  | ~B1                         
*                   --<--@==<====!==<==@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19354(C)
      S1=A(146)**4
      TOTNUM=-16*S1
      TOTDEN=+9
      RNUM=+P2*(C(4)*(P2-P1)+C(3))-C(1)-C(2)*P1
      F19354=RNUM*(TOTNUM/TOTDEN)*Q2(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19355()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  A  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                   ~g|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19355(C)
      S1=A(146)**2
      TOTNUM=-C(5)*S1
      TOTDEN=+81
      RNUM=+P2*(C(4)*(P2-P1)+C(3))-C(1)-C(2)*P1
      F19355=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19356()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  Z  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                   ~g|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19356(C)
      S1=A(146)**2
      TOTNUM=-C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)+C(3))-C(1)-C(2)*P1
      F19356=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19357()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  G  | ~B1                      
*                -->--@==>===+!==>==@---1-@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                   ~g|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19357(C)
      S1=A(146)**4
      TOTNUM=+32*S1
      TOTDEN=+27
      RNUM=+P2*(C(4)*(P2-P1)+C(3))-C(1)-C(2)*P1
      F19357=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19358()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  h  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                   ~g|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19358(C)
      S1=A(146)**2
      TOTNUM=-C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19358=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19359()
*                             !  B          ~b1                      
*                            /!==<==\     /-->--                     
*                            |!  P4 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  H  | ~B1                      
*                -->--@==>===+!==>==@-----@--<--                     
*                  P1 |  P3  |!  P3   -P6    P2                      
*                   ~g|P5    |!                                      
*                 ~B1 |  B   |!                                      
*                --<--@==<===/!                                      
*                  P2    P4   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19359(C)
      S1=A(146)**2
      TOTNUM=-C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19359=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1935()
*                 ~o1          S    !  S    ~o1                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~s1|-P6                       
*                 ~1+ |  W+ |  t    !  t  | ~1+                      
*                ==>==@-1>--@==>====!==>==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1935(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(C(4)*P1-C(3)+C(5)*P2)+C(1)+C(2)*P1
      F1935=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19360()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  b    !  b  |  A  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19360(C)
      TOTNUM=-C(4)
      TOTDEN=+81
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19360=RNUM*(TOTNUM/TOTDEN)*Q2(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19361()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  b    !  b  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19361(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19361=RNUM*(TOTNUM/TOTDEN)*Q1(9)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19362()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  b    !  b  |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19362(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19362=RNUM*(TOTNUM/TOTDEN)*Q1(9)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19363()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  b    !  b  |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19363(C)
      TOTNUM=-C(3)
      TOTDEN=+C(2)
      RNUM=+2*P2-C(1)-P1
      F19363=RNUM*(TOTNUM/TOTDEN)*Q1(9)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19364()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  b    !  b  |  Z  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19364(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+C(4)*P1)+C(2)-C(6)*P2)+P2*(C(6)*P2-C(5))+C(1)
      F19364=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19365()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  b    !  b  |  h  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19365(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19365=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19366()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  b    !  b  |  H  | ~B1                   
*             --<--@-1---@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19366(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19366=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19367()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  G  |  b    !  b  |  G  | ~B1                   
*             --<--@-1---@==>====!==>==@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19367(C)
      S1=A(146)**4
      TOTNUM=-32*S1
      TOTDEN=+9
      RNUM=+P2*(2*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19367=RNUM*(TOTNUM/TOTDEN)*Q2(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19368()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  b    !  b  |  h  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19368(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19368=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19369()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  b    !  b  |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19369(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19369=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1936()
*                 ~o1          S    !  S    ~o1                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~s2|-P6                       
*                 ~1+ |  W+ |  t    !  t  | ~1+                      
*                ==>==@-1>--@==>====!==>==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1936(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(C(4)*P1-C(3)+C(5)*P2)+C(1)+C(2)*P1
      F1936=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19370()
*              ~b1          B    !  B          ~b1                   
*             -->--\     /==<====!==<==\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  b    !  b  |  H  | ~B1                   
*             --<--@-----@==>====!==>==@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1794/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1794).EQ.0) CALL CC19370(C)
      TOTNUM=+C(3)
      TOTDEN=+C(2)
      RNUM=+C(1)+P1
      F19370=RNUM*(TOTNUM/TOTDEN)*Q2(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19371()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19371(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19371=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19372()
*                                !  h    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    h   |!  h  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  h   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19372(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19372=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19373()
*                 ~b1    h    !  h          ~b1                      
*                -->--@-------!-----\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~b1|P5     !     |     |                          
*                 ~B1 |  h    !  h  |  h  | ~B1                      
*                --<--@-------!-----@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19373(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19373=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19374()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    h    !  h  | ~B1                         
*                   -->--@-------!-----@--<--                        
*                     P1 |  P3   !  P3 |  P2                         
*                     ~b1|P5     !     |                             
*                    ~B1 |  h    !  h  |                             
*                   --<--@-------!-----/                             
*                     P2    P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19374(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19374=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19375()
*                 ~b1    h    !  h          ~b1                      
*                -->--@-------!-----\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~b1|P5     !     |     |                          
*                 ~B1 |  h    !  h  |  H  | ~B1                      
*                --<--@-------!-----@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19375(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19375=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19376()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19376(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19376=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19377()
*                                !  h    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    h   |!  h  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  h   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19377(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19377=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19378()
*              ~b1          h    !  h          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  h    !  h  |  h  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19378(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19378=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19379()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          h    !  h  | ~B1                      
*                -->--\     /-------!-----@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  h  |  h    !  h  |                          
*                --<--@-----@-------!-----/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19379(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19379=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1937()
*                                   !  t    ~o1                      
*                                  /!==>==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~t1|-P6                       
*                 ~o1          S   |!  S  | ~1+                      
*                =====\     /==<===+!==<==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  W+ |  t   |!                                
*                ==>==@-1>--@==>===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1937(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F1937=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19380()
*              ~b1          h    !  h          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  h    !  h  |  H  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19380(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19380=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19381()
*                 ~b1          h    !  h    ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  h  |  h    !  h  | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19381(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19381=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19382()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2 |  P4   !  P4 |  P2                         
*                        |       !     |                             
*                        |  h    !  h  |                             
*                        \-------!-----/                             
*                           P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19382(C)
      TOTNUM=+C(1)
      TOTDEN=+6
      RNUM=+1
      F19382=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19383()
*                 ~b1         !                                      
*                -->--\       !                                      
*                  P1 |       !                                      
*                     |       !                                      
*                 ~B1 |  h    !  h          ~b1                      
*                --<--@-------!-----\     /-->--                     
*                  P2 |  P4   !  P4 |     |  P1                      
*                     |       !     |     |                          
*                     |  h    !  h  |  H  | ~B1                      
*                     \-------!-----@-----@--<--                     
*                        P3   !  P3   -P5    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19383(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19383=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19384()
*                    ~b1         !                                   
*                   -->--\       !                                   
*                     P1 |       !                                   
*                        |       !                                   
*                    ~B1 |  h    !  h    ~b1                         
*                   --<--@-------!-----@-->--                        
*                     P2 |  P4   !  P4 |  P1                         
*                        |       !  ~b2|-P5                          
*                        |  h    !  h  | ~B1                         
*                        \-------!-----@--<--                        
*                           P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19384(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19384=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19385()
*              ~b1          h    !  h          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  h    !  h  |  H  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19385(C)
      TOTNUM=+C(1)
      TOTDEN=+6
      RNUM=+1
      F19385=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19386()
*                 ~b1          h    !  h    ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  H  |  h    !  h  | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19386(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19386=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19387()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19387(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19387=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19388()
*                                !  h    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    h   |!  h  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b2|P5    |!                                   
*                    ~B1 |  h   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1795/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1795).EQ.0) CALL CC19388(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19388=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19389()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19389(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19389=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1938()
*                                   !  t    ~o1                      
*                                  /!==>==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~t2|-P6                       
*                 ~o1          S   |!  S  | ~1+                      
*                =====\     /==<===+!==<==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  W+ |  t   |!                                
*                ==>==@-1>--@==>===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1938(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F1938=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19390()
*                 ~b1    h    !  h          ~b1                      
*                -->--@-------!-----\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~b1|P5     !     |     |                          
*                 ~B1 |  H    !  H  |  h  | ~B1                      
*                --<--@-------!-----@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19390(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19390=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19391()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    h   |!  h  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19391(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19391=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19392()
*                 ~b1    h    !  h          ~b1                      
*                -->--@-------!-----\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~b1|P5     !     |     |                          
*                 ~B1 |  H    !  H  |  H  | ~B1                      
*                --<--@-------!-----@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19392(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19392=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19393()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    h    !  h  | ~B1                         
*                   -->--@-------!-----@--<--                        
*                     P1 |  P3   !  P3 |  P2                         
*                     ~b1|P5     !     |                             
*                    ~B1 |  H    !  H  |                             
*                   --<--@-------!-----/                             
*                     P2    P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19393(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19393=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19394()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19394(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19394=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19395()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    h   |!  h  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19395(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19395=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19396()
*              ~b1          h    !  h          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H    !  H  |  h  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19396(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19396=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19397()
*                                   !  H    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b1|-P6                       
*                 ~b1          h   |!  h  | ~B1                      
*                -->--\     /------+!-----@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  h  |  H   |!                                
*                --<--@-----@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19397(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19397=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19398()
*              ~b1          h    !  h          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H    !  H  |  H  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19398(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19398=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19399()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          h    !  h  | ~B1                      
*                -->--\     /-------!-----@--<--                     
*                  P1 |     |  P3   !  P3 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  h  |  H    !  H  |                          
*                --<--@-----@-------!-----/                          
*                  P2    P5    P4   !  P4                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19399(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19399=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1939()
*              ~o1          S    !  S          ~o1                   
*             =====\     /==<====!==<==\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  H+ |  t    !  t  |  H+ | ~1+                   
*             ==>==@-->--@==>====!==>==@-->--@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1939(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(C(2)+C(3)*P1)-C(1)
      F1939=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F193()
*                    ~o1    H3   !  H3   ~o1                         
*                   =====@-------!-----@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~o3|P5     !  ~o4|-P6                          
*                    ~o1 |  Z    !  Z  | ~o1                         
*                   =====@-1-----!---1-@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U4/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(4).EQ.0) CALL CC193(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+8*P1-16*P2)+P2*(C(5)+8*P2)+C(2))+P2*(C(4)-C(6)*
     >P2)+C(1)
      F193=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q1(10)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(
     >5)*Q0(6)*Q0(7)*Q0(9)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19400()
*                 ~b1          h    !  h    ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  h  |  H    !  H  | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19400(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19400=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19401()
*                                   !  H    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          h   |!  h  | ~B1                      
*                -->--\     /------+!-----@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  h  |  H   |!                                
*                --<--@-----@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19401(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19401=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19402()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19402(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19402=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19403()
*                             !  h          ~b1                      
*                            /!-----\     /-->--                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    H   |!  H  |  H  | ~B1                      
*                -->--@------+!-----@-----@--<--                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                  ~b1|P5    |!                                      
*                 ~B1 |  h   |!                                      
*                --<--@------/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19403(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19403=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19404()
*                    ~b1    H    !       ~b1                         
*                   -->--@-----\ !     /-->--                        
*                     P1 |  P4 | !     |  P1                         
*                     ~b1|P5   | !     |                             
*                    ~B1 |  h  | !  h  | ~B1                         
*                   --<--@-----+-!-----@--<--                        
*                     P2    P3 | !  P3 |  P2                         
*                              | !     |                             
*                              | !  H  |                             
*                              \-!-----/                             
*                                !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19404(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19404=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19405()
*                                !  h    ~b1                         
*                               /!-----@-->--                        
*                               |!  P3 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    H   |!  H  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  h   |!                                   
*                   --<--@------/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19405(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19405=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19406()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19406(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19406=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19407()
*              ~b1          h    !  h          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  H    !  H  |  H  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19407(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19407=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19408()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          h    !  h  | ~B1                      
*                -->--\     /-------!-----@--<--                     
*                  P1 |     |  P3   !  P3 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  H  |  H    !  H  |                          
*                --<--@-----@-------!-----/                          
*                  P2    P5    P4   !  P4                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19408(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19408=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19409()
*                 ~b1          h    !  h    ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  H  |  H    !  H  | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19409(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19409=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1940()
*                 ~o1          S    !  S    ~o1                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~s1|-P6                       
*                 ~1+ |  H+ |  t    !  t  | ~1+                      
*                ==>==@-->--@==>====!==>==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1940(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*P2-C(2))+C(3)*P2-C(1)
      F1940=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19410()
*                                   !  H    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          h   |!  h  | ~B1                      
*                -->--\     /------+!-----@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  H  |  H   |!                                
*                --<--@-----@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19410(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19410=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19411()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2 |  P3   !  P3 |  P2                         
*                        |       !     |                             
*                        |  H    !  H  |                             
*                        \-------!-----/                             
*                           P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19411(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19411=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19412()
*                    ~b1         !                                   
*                   -->--\       !                                   
*                     P1 |       !                                   
*                        |       !                                   
*                    ~B1 |  h    !  h    ~b1                         
*                   --<--@-------!-----@-->--                        
*                     P2 |  P3   !  P3 |  P1                         
*                        |       !  ~b2|-P5                          
*                        |  H    !  H  | ~B1                         
*                        \-------!-----@--<--                        
*                           P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19412(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19412=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19413()
*                    ~b1         !  H    ~b1                         
*                   -->--\      /!-----@-->--                        
*                     P1 |      |!  P4 |  P1                         
*                        |      |!  ~b2|-P5                          
*                    ~B1 |  h   |!  h  | ~B1                         
*                   --<--@------+!-----@--<--                        
*                     P2 |  P3  |!  P3    P2                         
*                        |      |!                                   
*                        |  H   |!                                   
*                        \------/!                                   
*                           P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19413(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19413=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19414()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19414(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19414=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19415()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    h   |!  h  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b2|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19415(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19415=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19416()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1796/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1796).EQ.0) CALL CC19416(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19416=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19417()
*              ~b1          h    !  h          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  H3   !  H3 |  Z  | ~B1                   
*             --<--@-1---@-------!-----@---2-@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1797/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1797).EQ.0) CALL CC19417(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(-C(2)-C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))-C(1)
      F19417=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19418()
*                 ~b1          h    !  h    ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  Z  |  H3   !  H3 | ~B1                      
*                --<--@-1---@-------!-----@--<--                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1797/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1797).EQ.0) CALL CC19418(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19418=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19419()
*                                   !  H3   ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          h   |!  h  | ~B1                      
*                -->--\     /------+!-----@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  Z  |  H3  |!                                
*                --<--@-1---@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1797/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1797).EQ.0) CALL CC19419(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19419=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1941()
*                 ~o1          S    !  S    ~o1                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~s2|-P6                       
*                 ~1+ |  H+ |  t    !  t  | ~1+                      
*                ==>==@-->--@==>====!==>==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1941(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*P2-C(2))+C(3)*P2-C(1)
      F1941=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19420()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  H3   !  H3 | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1797/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1797).EQ.0) CALL CC19420(C)
      TOTNUM=-C(1)
      TOTDEN=+3
      RNUM=+1
      F19420=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19421()
*                                !  H3   ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    h   |!  h  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b2|P5    |!                                   
*                    ~B1 |  H3  |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1797/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1797).EQ.0) CALL CC19421(C)
      TOTNUM=-C(1)
      TOTDEN=+3
      RNUM=+1
      F19421=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19422()
*                    ~b1    H3   !  H3   ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  h    !  h  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1797/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1797).EQ.0) CALL CC19422(C)
      TOTNUM=-C(1)
      TOTDEN=+3
      RNUM=+1
      F19422=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19423()
*              ~b1          H    !  H          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H    !  H  |  h  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19423(C)
      TOTNUM=+C(1)
      TOTDEN=+6
      RNUM=+1
      F19423=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19424()
*                 ~b1          H    !  H    ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b1|-P6                       
*                 ~B1 |  h  |  H    !  H  | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19424(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19424=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19425()
*              ~b1          H    !  H          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H    !  H  |  H  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19425(C)
      TOTNUM=-C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19425=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19426()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          H    !  H  | ~B1                      
*                -->--\     /-------!-----@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  h  |  H    !  H  |                          
*                --<--@-----@-------!-----/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19426(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19426=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19427()
*                 ~b1          H    !  H    ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  h  |  H    !  H  | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19427(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19427=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19428()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19428(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19428=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19429()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    H   |!  H  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19429(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19429=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(2)*Q0(1)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1942()
*                                   !  t    ~o1                      
*                                  /!==>==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~t1|-P6                       
*                 ~o1          S   |!  S  | ~1+                      
*                =====\     /==<===+!==<==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  H+ |  t   |!                                
*                ==>==@-->--@==>===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1942(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F1942=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19430()
*                 ~b1    H    !  H          ~b1                      
*                -->--@-------!-----\     /-->--                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                  ~b1|P5     !     |     |                          
*                 ~B1 |  H    !  H  |  H  | ~B1                      
*                --<--@-------!-----@-----@--<--                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19430(C)
      TOTNUM=-C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19430=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(3)*Q0(1)*Q0(2)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19431()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    H    !  H  | ~B1                         
*                   -->--@-------!-----@--<--                        
*                     P1 |  P3   !  P3 |  P2                         
*                     ~b1|P5     !     |                             
*                    ~B1 |  H    !  H  |                             
*                   --<--@-------!-----/                             
*                     P2    P4   !  P4                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19431(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19431=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19432()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19432(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19432=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19433()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    H   |!  H  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19433(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19433=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(4)*Q0(1)*Q0(2)*Q0(3)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19434()
*              ~b1          H    !  H          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  H    !  H  |  H  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19434(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19434=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19435()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          H    !  H  | ~B1                      
*                -->--\     /-------!-----@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  H  |  H    !  H  |                          
*                --<--@-----@-------!-----/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19435(C)
      TOTNUM=-C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19435=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19436()
*                 ~b1          H    !  H    ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  H  |  H    !  H  | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19436(C)
      TOTNUM=-C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19436=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19437()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2 |  P4   !  P4 |  P2                         
*                        |       !     |                             
*                        |  H    !  H  |                             
*                        \-------!-----/                             
*                           P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19437(C)
      TOTNUM=+C(1)
      TOTDEN=+6
      RNUM=+1
      F19437=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19438()
*                    ~b1         !                                   
*                   -->--\       !                                   
*                     P1 |       !                                   
*                        |       !                                   
*                    ~B1 |  H    !  H    ~b1                         
*                   --<--@-------!-----@-->--                        
*                     P2 |  P4   !  P4 |  P1                         
*                        |       !  ~b2|-P5                          
*                        |  H    !  H  | ~B1                         
*                        \-------!-----@--<--                        
*                           P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19438(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19438=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19439()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19439(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19439=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1943()
*                                   !  t    ~o1                      
*                                  /!==>==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~t2|-P6                       
*                 ~o1          S   |!  S  | ~1+                      
*                =====\     /==<===+!==<==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  H+ |  t   |!                                
*                ==>==@-->--@==>===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1943(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F1943=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19440()
*                                !  H    ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    H   |!  H  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b2|P5    |!                                   
*                    ~B1 |  H   |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1798/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1798).EQ.0) CALL CC19440(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19440=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19441()
*              ~b1          H    !  H          ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  H3   !  H3 |  Z  | ~B1                   
*             --<--@-1---@-------!-----@---2-@--<--                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1799/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1799).EQ.0) CALL CC19441(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(-C(2)-C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))-C(1)
      F19441=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19442()
*                 ~b1          H    !  H    ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  Z  |  H3   !  H3 | ~B1                      
*                --<--@-1---@-------!-----@--<--                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1799/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1799).EQ.0) CALL CC19442(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19442=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19443()
*                                   !  H3   ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          H   |!  H  | ~B1                      
*                -->--\     /------+!-----@--<--                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~B1 |  Z  |  H3  |!                                
*                --<--@-1---@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1799/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1799).EQ.0) CALL CC19443(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19443=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19444()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  H3   !  H3 | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1799/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1799).EQ.0) CALL CC19444(C)
      TOTNUM=-C(1)
      TOTDEN=+3
      RNUM=+1
      F19444=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19445()
*                                !  H3   ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    H   |!  H  | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b2|P5    |!                                   
*                    ~B1 |  H3  |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1799/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1799).EQ.0) CALL CC19445(C)
      TOTNUM=-C(1)
      TOTDEN=+3
      RNUM=+1
      F19445=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19446()
*                    ~b1    H3   !  H3   ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  H    !  H  | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1799/ Q0(3),Q1(3),Q2(3)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1799).EQ.0) CALL CC19446(C)
      TOTNUM=-C(1)
      TOTDEN=+3
      RNUM=+1
      F19446=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19447()
*              ~b1          H3   !  H3         ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H3   !  H3 |  h  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19447(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19447=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19448()
*              ~b1          H3   !  H3         ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H3   !  H3 |  H  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19448(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19448=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19449()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          H3   !  H3 | ~B1                      
*                -->--\     /-------!-----@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  h  |  H3   !  H3 |                          
*                --<--@-----@-------!-----/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19449(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19449=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1944()
*                    ~o1    S    !  S    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~s1|P5     !  ~s1|-P6                          
*                    ~1+ |  t    !  t  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1944(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1944=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19450()
*                 ~b1          H3   !  H3   ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  h  |  H3   !  H3 | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19450(C)
      TOTNUM=-C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19450=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19451()
*              ~b1          H3   !  H3         ~b1                   
*             -->--\     /-------!-----\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  H3   !  H3 |  H  | ~B1                   
*             --<--@-----@-------!-----@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19451(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19451=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19452()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          H3   !  H3 | ~B1                      
*                -->--\     /-------!-----@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  H  |  H3   !  H3 |                          
*                --<--@-----@-------!-----/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19452(C)
      TOTNUM=+C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19452=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19453()
*                 ~b1          H3   !  H3   ~b1                      
*                -->--\     /-------!-----@-->--                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                 ~B1 |  H  |  H3   !  H3 | ~B1                      
*                --<--@-----@-------!-----@--<--                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19453(C)
      TOTNUM=-C(2)
      TOTDEN=+C(1)
      RNUM=+1
      F19453=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19454()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  H3   !  H3 | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2 |  P4   !  P4 |  P2                         
*                        |       !     |                             
*                        |  H3   !  H3 |                             
*                        \-------!-----/                             
*                           P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19454(C)
      TOTNUM=+C(1)
      TOTDEN=+6
      RNUM=+1
      F19454=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19455()
*                    ~b1         !                                   
*                   -->--\       !                                   
*                     P1 |       !                                   
*                        |       !                                   
*                    ~B1 |  H3   !  H3   ~b1                         
*                   --<--@-------!-----@-->--                        
*                     P2 |  P4   !  P4 |  P1                         
*                        |       !  ~b2|-P5                          
*                        |  H3   !  H3 | ~B1                         
*                        \-------!-----@--<--                        
*                           P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19455(C)
      TOTNUM=-C(1)
      TOTDEN=+3
      RNUM=+1
      F19455=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19456()
*                    ~b1    H3   !  H3   ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~B1 |  H3   !  H3 | ~B1                         
*                   --<--@-------!-----@--<--                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19456(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19456=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19457()
*                                !  H3   ~b1                         
*                               /!-----@-->--                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~b1    H3  |!  H3 | ~B1                         
*                   -->--@------+!-----@--<--                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b2|P5    |!                                   
*                    ~B1 |  H3  |!                                   
*                   --<--@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1800/ Q0(4),Q1(4),Q2(4)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1800).EQ.0) CALL CC19457(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19457=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19458()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~u1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19458(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19458=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19459()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~c1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19459(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19459=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1945()
*                    ~o1    S    !  S    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~s1|P5     !  ~s2|-P6                          
*                    ~1+ |  t    !  t  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1945(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1945=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19460()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~c2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19460(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19460=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19461()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~t1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19461(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19461=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19462()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~t2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19462(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19462=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19463()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~u1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  A  | ~B1                      
*                --<--@-->----!-->--@---1-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19463(C)
      TOTNUM=+C(2)
      TOTDEN=+9
      RNUM=+2*P2-C(1)-P1
      F19463=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(9)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19464()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~u1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  Z  | ~B1                      
*                --<--@-->----!-->--@---1-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19464(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19464=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19465()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~u1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  h  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19465(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19465=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(7)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19466()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~u1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  H  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19466(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19466=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(8)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19467()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    H-   !  H- | ~B1                         
*                   -->--@--<----!--<--@--<--                        
*                     P1 |  P4   !  P4 |  P2                         
*                     ~u1|P5     !     |                             
*                    ~B1 |  H+   !  H+ |                             
*                   --<--@-->----!-->--/                             
*                     P2    P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19467(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19467=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19468()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~c1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19468(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19468=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19469()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~c2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19469(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19469=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1946()
*                                !  t    ~o1                         
*                               /!==>==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~t1|-P6                          
*                    ~o1    S   |!  S  | ~1+                         
*                   =====@==<===+!==<==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~s1|P5    |!                                   
*                    ~1+ |  t   |!                                   
*                   ==>==@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1946(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1946=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19470()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~t1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19470(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19470=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19471()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~t2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19471(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19471=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19472()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  A  | ~B1                      
*                --<--@-->----!-->--@---1-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19472(C)
      TOTNUM=+C(2)
      TOTDEN=+9
      RNUM=+2*P2-C(1)-P1
      F19472=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(9)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19473()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  Z  | ~B1                      
*                --<--@-->----!-->--@---1-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19473(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19473=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19474()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  h  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19474(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19474=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(7)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19475()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  H  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19475(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19475=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(8)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19476()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    H-   !  H- | ~B1                         
*                   -->--@--<----!--<--@--<--                        
*                     P1 |  P4   !  P4 |  P2                         
*                     ~c1|P5     !     |                             
*                    ~B1 |  H+   !  H+ |                             
*                   --<--@-->----!-->--/                             
*                     P2    P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19476(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19476=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19477()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c2|P5     !  ~c2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19477(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19477=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19478()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c2|P5     !  ~t1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19478(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19478=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19479()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c2|P5     !  ~t2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19479(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19479=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1947()
*                                !  t    ~o1                         
*                               /!==>==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~t2|-P6                          
*                    ~o1    S   |!  S  | ~1+                         
*                   =====@==<===+!==<==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~s1|P5    |!                                   
*                    ~1+ |  t   |!                                   
*                   ==>==@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1947(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1947=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19480()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c2|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  A  | ~B1                      
*                --<--@-->----!-->--@---1-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19480(C)
      TOTNUM=+C(2)
      TOTDEN=+9
      RNUM=+2*P2-C(1)-P1
      F19480=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(9)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19481()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c2|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  Z  | ~B1                      
*                --<--@-->----!-->--@---1-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19481(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19481=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19482()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c2|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  h  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19482(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19482=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(7)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19483()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~c2|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  H  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19483(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19483=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(8)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19484()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    H-   !  H- | ~B1                         
*                   -->--@--<----!--<--@--<--                        
*                     P1 |  P4   !  P4 |  P2                         
*                     ~c2|P5     !     |                             
*                    ~B1 |  H+   !  H+ |                             
*                   --<--@-->----!-->--/                             
*                     P2    P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19484(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19484=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19485()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t1|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19485(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19485=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19486()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19486(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19486=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19487()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  A  | ~B1                      
*                --<--@-->----!-->--@---1-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19487(C)
      TOTNUM=+C(2)
      TOTDEN=+9
      RNUM=+2*P2-C(1)-P1
      F19487=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19488()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  Z  | ~B1                      
*                --<--@-->----!-->--@---1-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19488(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19488=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19489()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  h  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19489(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19489=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1948()
*                    ~o1    S    !  S    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~s2|P5     !  ~s2|-P6                          
*                    ~1+ |  t    !  t  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1948(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1948=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19490()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t1|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  H  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19490(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19490=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(8)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19491()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    H-   !  H- | ~B1                         
*                   -->--@--<----!--<--@--<--                        
*                     P1 |  P4   !  P4 |  P2                         
*                     ~t1|P5     !     |                             
*                    ~B1 |  H+   !  H+ |                             
*                   --<--@-->----!-->--/                             
*                     P2    P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19491(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19491=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19492()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t2|P5     !  ~t2|-P6                          
*                    ~B1 |  H+   !  H+ | ~B1                         
*                   --<--@-->----!-->--@--<--                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19492(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19492=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19493()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t2|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  A  | ~B1                      
*                --<--@-->----!-->--@---1-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19493(C)
      TOTNUM=+C(2)
      TOTDEN=+9
      RNUM=+2*P2-C(1)-P1
      F19493=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19494()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t2|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  Z  | ~B1                      
*                --<--@-->----!-->--@---1-@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19494(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19494=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19495()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t2|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  h  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19495(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19495=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19496()
*                 ~b1    H-   !  H-         ~b1                      
*                -->--@--<----!--<--\     /-->--                     
*                  P1 |  P4   !  P4 |     |  P1                      
*                  ~t2|P5     !     |     |                          
*                 ~B1 |  H+   !  H+ |  H  | ~B1                      
*                --<--@-->----!-->--@-----@--<--                     
*                  P2    P3   !  P3   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19496(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19496=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19497()
*                                !       ~b1                         
*                                !     /-->--                        
*                                !     |  P1                         
*                                !     |                             
*                    ~b1    H-   !  H- | ~B1                         
*                   -->--@--<----!--<--@--<--                        
*                     P1 |  P4   !  P4 |  P2                         
*                     ~t2|P5     !     |                             
*                    ~B1 |  H+   !  H+ |                             
*                   --<--@-->----!-->--/                             
*                     P2    P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19497(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19497=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19498()
*              ~b1          H-   !  H-         ~b1                   
*             -->--\     /--<----!--<--\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  H+   !  H+ |  A  | ~B1                   
*             --<--@-1---@-->----!-->--@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19498(C)
      TOTNUM=+C(4)
      TOTDEN=+27
      RNUM=+P1*(C(2)+P1-4*P2)+P2*(4*P2-C(3))+C(1)
      F19498=RNUM*(TOTNUM/TOTDEN)*Q2(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19499()
*              ~b1          H-   !  H-         ~b1                   
*             -->--\     /--<----!--<--\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  H+   !  H+ |  Z  | ~B1                   
*             --<--@-1---@-->----!-->--@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19499(C)
      TOTNUM=-C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))+C(1)
      F19499=RNUM*(TOTNUM/TOTDEN)*Q1(9)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1949()
*                                !  t    ~o1                         
*                               /!==>==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~t1|-P6                          
*                    ~o1    S   |!  S  | ~1+                         
*                   =====@==<===+!==<==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~s2|P5    |!                                   
*                    ~1+ |  t   |!                                   
*                   ==>==@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1949(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1949=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F194()
*                    ~o1    Z    !  Z    ~o1                         
*                   =====@-1-----!---1-@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~o4|P5     !  ~o4|-P6                          
*                    ~o1 |  H3   !  H3 | ~o1                         
*                   =====@-------!-----@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U4/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(4).EQ.0) CALL CC194(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(P1*(C(4)+4*P2)+C(3)-C(5)*P2)+C(1)+C(2)*P1
      F194=RNUM*(TOTNUM/TOTDEN)*Q2(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6
     >)*Q0(7)*Q0(8)*Q0(10)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19500()
*              ~b1          H-   !  H-         ~b1                   
*             -->--\     /--<----!--<--\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  H+   !  H+ |  h  | ~B1                   
*             --<--@-1---@-->----!-->--@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19500(C)
      TOTNUM=+C(2)
      TOTDEN=+9
      RNUM=+2*P2-C(1)-P1
      F19500=RNUM*(TOTNUM/TOTDEN)*Q1(9)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19501()
*              ~b1          H-   !  H-         ~b1                   
*             -->--\     /--<----!--<--\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  A  |  H+   !  H+ |  H  | ~B1                   
*             --<--@-1---@-->----!-->--@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19501(C)
      TOTNUM=+C(2)
      TOTDEN=+9
      RNUM=+2*P2-C(1)-P1
      F19501=RNUM*(TOTNUM/TOTDEN)*Q1(9)*Q1(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19502()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          H-   !  H- | ~B1                      
*                -->--\     /--<----!--<--@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  A  |  H+   !  H+ |                          
*                --<--@-1---@-->----!-->--/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19502(C)
      TOTNUM=+C(2)
      TOTDEN=+9
      RNUM=+2*P2-C(1)-P1
      F19502=RNUM*(TOTNUM/TOTDEN)*Q1(9)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19503()
*              ~b1          H-   !  H-         ~b1                   
*             -->--\     /--<----!--<--\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  H+   !  H+ |  Z  | ~B1                   
*             --<--@-1---@-->----!-->--@---2-@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19503(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(5)*P2-C(4))+C(1)
      F19503=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19504()
*              ~b1          H-   !  H-         ~b1                   
*             -->--\     /--<----!--<--\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  H+   !  H+ |  h  | ~B1                   
*             --<--@-1---@-->----!-->--@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19504(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19504=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19505()
*              ~b1          H-   !  H-         ~b1                   
*             -->--\     /--<----!--<--\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  Z  |  H+   !  H+ |  H  | ~B1                   
*             --<--@-1---@-->----!-->--@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19505(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19505=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q1(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19506()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          H-   !  H- | ~B1                      
*                -->--\     /--<----!--<--@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  Z  |  H+   !  H+ |                          
*                --<--@-1---@-->----!-->--/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19506(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F19506=RNUM*(TOTNUM/TOTDEN)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19507()
*              ~b1          H-   !  H-         ~b1                   
*             -->--\     /--<----!--<--\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H+   !  H+ |  h  | ~B1                   
*             --<--@-----@-->----!-->--@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19507(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19507=RNUM*(TOTNUM/TOTDEN)*Q2(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19508()
*              ~b1          H-   !  H-         ~b1                   
*             -->--\     /--<----!--<--\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  h  |  H+   !  H+ |  H  | ~B1                   
*             --<--@-----@-->----!-->--@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19508(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19508=RNUM*(TOTNUM/TOTDEN)*Q1(7)*Q1(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0
     >(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19509()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          H-   !  H- | ~B1                      
*                -->--\     /--<----!--<--@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  h  |  H+   !  H+ |                          
*                --<--@-----@-->----!-->--/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19509(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19509=RNUM*(TOTNUM/TOTDEN)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1950()
*                                !  t    ~o1                         
*                               /!==>==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~t2|-P6                          
*                    ~o1    S   |!  S  | ~1+                         
*                   =====@==<===+!==<==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~s2|P5    |!                                   
*                    ~1+ |  t   |!                                   
*                   ==>==@==>===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1950(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1950=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19510()
*              ~b1          H-   !  H-         ~b1                   
*             -->--\     /--<----!--<--\     /-->--                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*              ~B1 |  H  |  H+   !  H+ |  H  | ~B1                   
*             --<--@-----@-->----!-->--@-----@--<--                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19510(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19510=RNUM*(TOTNUM/TOTDEN)*Q2(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19511()
*                                   !       ~b1                      
*                                   !     /-->--                     
*                                   !     |  P1                      
*                                   !     |                          
*                 ~b1          H-   !  H- | ~B1                      
*                -->--\     /--<----!--<--@--<--                     
*                  P1 |     |  P4   !  P4 |  P2                      
*                     |     |       !     |                          
*                 ~B1 |  H  |  H+   !  H+ |                          
*                --<--@-----@-->----!-->--/                          
*                  P2    P5    P3   !  P3                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19511(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19511=RNUM*(TOTNUM/TOTDEN)*Q1(8)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0
     >(6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19512()
*                    ~b1         !       ~b1                         
*                   -->--\       !     /-->--                        
*                     P1 |       !     |  P1                         
*                        |       !     |                             
*                    ~B1 |  H-   !  H- | ~B1                         
*                   --<--@--<----!--<--@--<--                        
*                     P2 |  P4   !  P4 |  P2                         
*                        |       !     |                             
*                        |  H+   !  H+ |                             
*                        \-->----!-->--/                             
*                           P3   !  P3                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1801/ Q0(8),Q1(9),Q2(9)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1801).EQ.0) CALL CC19512(C)
      TOTNUM=+C(1)
      TOTDEN=+3
      RNUM=+1
      F19512=RNUM*(TOTNUM/TOTDEN)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0
     >(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19513()
*              ~b1          A    !  A          ~b1                   
*             -->--\     /-1-----!---1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  b    !  b  |  b  |  ~g                   
*             =====@==>==@==>====!==>==@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1802/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1802).EQ.0) CALL CC19513(C)
      S1=A(146)**2
      TOTNUM=-C(5)*S1
      TOTDEN=+27
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19513=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19514()
*                 ~b1          A    !  A    ~b1                      
*                -->--\     /-1-----!---1-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b1|-P6                       
*                  ~g |  b  |  b    !  b  |  ~g                      
*                =====@==>==@==>====!==>==@=====                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1802/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1802).EQ.0) CALL CC19514(C)
      S1=A(146)**2
      TOTNUM=+C(5)*S1
      TOTDEN=+27
      RNUM=+P1*(C(4)*P2-C(2))+C(1)+C(3)*P2
      F19514=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19515()
*                    ~b1    A    !  A    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1802/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1802).EQ.0) CALL CC19515(C)
      S1=A(146)**2
      TOTNUM=+C(4)*S1
      TOTDEN=+27
      RNUM=+P2*(C(2)+C(3)*P2)-C(1)
      F19515=RNUM*(TOTNUM/TOTDEN)*Q2(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19516()
*              ~b1          Z    !  Z          ~b1                   
*             -->--\     /-1-----!---1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  b    !  b  |  b  |  ~g                   
*             =====@==>==@==>====!==>==@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1803/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1803).EQ.0) CALL CC19516(C)
      S1=A(146)**2
      TOTNUM=+C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+C(6)*P2)+C(2)+C(5)*P2)+C(1)+C(4)*P2
      F19516=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19517()
*                 ~b1          Z    !  Z    ~b1                      
*                -->--\     /-1-----!---1-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b1|-P6                       
*                  ~g |  b  |  b    !  b  |  ~g                      
*                =====@==>==@==>====!==>==@=====                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1803/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1803).EQ.0) CALL CC19517(C)
      S1=A(146)**2
      TOTNUM=+C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P2*(P1*(C(4)+C(6)*P2)+C(3)+C(5)*P2)-C(1)-C(2)*P1
      F19517=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19518()
*                 ~b1          Z    !  Z    ~b1                      
*                -->--\     /-1-----!---1-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                  ~g |  b  |  b    !  b  |  ~g                      
*                =====@==>==@==>====!==>==@=====                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1803/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1803).EQ.0) CALL CC19518(C)
      S1=A(146)**2
      TOTNUM=-C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P2*(P1*(C(4)+C(6)*P2)+C(3)+C(5)*P2)-C(1)-C(2)*P1
      F19518=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19519()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1803/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1803).EQ.0) CALL CC19519(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(P2*(C(3)+C(4)*P2)-C(2))-C(1)
      F19519=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1951()
*                    ~o1    t    !  t    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t1|-P6                          
*                    ~1+ |  S    !  S  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1951(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1951=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19520()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1803/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1803).EQ.0) CALL CC19520(C)
      S1=A(146)**2
      TOTNUM=-C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(P2*(C(3)+C(4)*P2)-C(2))-C(1)
      F19520=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19521()
*                    ~b1    Z    !  Z    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1803/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1803).EQ.0) CALL CC19521(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(P2*(C(3)+C(4)*P2)-C(2))-C(1)
      F19521=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19522()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-1<----!--<1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  u    !  u  |  b  |  ~g                   
*             =====@==>==@==>====!==>==@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1804/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1804).EQ.0) CALL CC19522(C)
      S1=A(146)**2
      TOTNUM=+C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+C(6)*P2)+C(2)-C(5)*P2)-C(1)-C(4)*P2
      F19522=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19523()
*                 ~b1          W-   !  W-   ~b1                      
*                -->--\     /-1<----!--<1-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~u1|-P6                       
*                  ~g |  b  |  u    !  u  |  ~g                      
*                =====@==>==@==>====!==>==@=====                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1804/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1804).EQ.0) CALL CC19523(C)
      S1=A(146)**2
      TOTNUM=+C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P2*(P1*(C(6)*P2-C(4))+C(3)+C(5)*P2)+C(1)-C(2)*P1
      F19523=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19524()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~u1|P5     !  ~u1|-P6                          
*                     ~g |  u    !  u  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1804/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1804).EQ.0) CALL CC19524(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(P2*(C(4)*P2-C(3))-C(2))+C(1)
      F19524=RNUM*(TOTNUM/TOTDEN)*Q2(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19525()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-1<----!--<1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  c    !  c  |  b  |  ~g                   
*             =====@==>==@==>====!==>==@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1805/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1805).EQ.0) CALL CC19525(C)
      S1=A(146)**2
      TOTNUM=+C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+C(6)*P2)+C(2)-C(5)*P2)-C(1)-C(4)*P2
      F19525=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19526()
*                 ~b1          W-   !  W-   ~b1                      
*                -->--\     /-1<----!--<1-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~c1|-P6                       
*                  ~g |  b  |  c    !  c  |  ~g                      
*                =====@==>==@==>====!==>==@=====                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1805/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1805).EQ.0) CALL CC19526(C)
      S1=A(146)**2
      TOTNUM=+C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P2*(P1*(C(6)*P2-C(4))+C(3)+C(5)*P2)+C(1)-C(2)*P1
      F19526=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19527()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c1|P5     !  ~c1|-P6                          
*                     ~g |  c    !  c  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1805/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1805).EQ.0) CALL CC19527(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(P2*(C(4)*P2-C(3))-C(2))+C(1)
      F19527=RNUM*(TOTNUM/TOTDEN)*Q2(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19528()
*              ~b1          W-   !  W-         ~b1                   
*             -->--\     /-1<----!--<1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  t    !  t  |  b  |  ~g                   
*             =====@==>==@==>====!==>==@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1806/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1806).EQ.0) CALL CC19528(C)
      S1=A(146)**2
      TOTNUM=+C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+C(6)*P2)+C(2)-C(5)*P2)-C(1)-C(4)*P2
      F19528=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19529()
*                 ~b1          W-   !  W-   ~b1                      
*                -->--\     /-1<----!--<1-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~t1|-P6                       
*                  ~g |  b  |  t    !  t  |  ~g                      
*                =====@==>==@==>====!==>==@=====                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1806/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1806).EQ.0) CALL CC19529(C)
      S1=A(146)**2
      TOTNUM=+C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P2*(P1*(C(6)*P2-C(4))+C(3)+C(5)*P2)+C(1)-C(2)*P1
      F19529=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1952()
*                    ~o1    t    !  t    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t2|-P6                          
*                    ~1+ |  S    !  S  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1952(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1952=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19530()
*                 ~b1          W-   !  W-   ~b1                      
*                -->--\     /-1<----!--<1-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~t2|-P6                       
*                  ~g |  b  |  t    !  t  |  ~g                      
*                =====@==>==@==>====!==>==@=====                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1806/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(1806).EQ.0) CALL CC19530(C)
      S1=A(146)**2
      TOTNUM=+C(8)*S1
      TOTDEN=+C(7)
      RNUM=+P2*(P1*(C(6)*P2-C(4))+C(3)+C(5)*P2)+C(1)-C(2)*P1
      F19530=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19531()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t1|P5     !  ~t1|-P6                          
*                     ~g |  t    !  t  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1806/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1806).EQ.0) CALL CC19531(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(P2*(C(4)*P2-C(3))-C(2))+C(1)
      F19531=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19532()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t1|P5     !  ~t2|-P6                          
*                     ~g |  t    !  t  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1806/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1806).EQ.0) CALL CC19532(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(P2*(C(4)*P2-C(3))-C(2))+C(1)
      F19532=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19533()
*                    ~b1    W-   !  W-   ~b1                         
*                   -->--@-1<----!--<1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t2|P5     !  ~t2|-P6                          
*                     ~g |  t    !  t  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1806/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1806).EQ.0) CALL CC19533(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P2*(P2*(C(4)*P2-C(3))-C(2))+C(1)
      F19533=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19534()
*                    ~b1    b    !  b    ~b1                         
*                   -->--@==>====!==>==@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                      ~g|P5     !   ~g|-P6                          
*                     ~g |  G    !  G  |  ~g                         
*                   =====@-1-----!---1-@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1807/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1807).EQ.0) CALL CC19534(C)
      S1=A(146)**4
      TOTNUM=-S1
      TOTDEN=+1
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19534=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19535()
*                             !  G          ~b1                      
*                            /!---1-\     /-->--                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~b1    b   |!  b  |  b  |  ~g                      
*                -->--@==>===+!==>==@==>==@=====                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                   ~g|P5    |!                                      
*                  ~g |  G   |!                                      
*                =====@-1----/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1807/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1807).EQ.0) CALL CC19535(C)
      S1=A(146)**4
      TOTNUM=-S1
      TOTDEN=+1
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19535=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19536()
*                                !  G    ~b1                         
*                               /!---1-@-->--                        
*                               |!  P3 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~b1    b   |!  b  |  ~g                         
*                   -->--@==>===+!==>==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                      ~g|P5    |!                                   
*                     ~g |  G   |!                                   
*                   =====@-1----/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1807/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1807).EQ.0) CALL CC19536(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+2
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(2)*P1-C(1)
      F19536=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19537()
*              ~b1          G    !  G          ~b1                   
*             -->--\     /-1-----!---1-\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  b    !  b  |  b  |  ~g                   
*             =====@==>==@==>====!==>==@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1807/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1807).EQ.0) CALL CC19537(C)
      S1=A(146)**4
      TOTNUM=-4*S1
      TOTDEN=+9
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F19537=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19538()
*                 ~b1          G    !  G    ~b1                      
*                -->--\     /-1-----!---1-@-->--                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~b1|-P6                       
*                  ~g |  b  |  b    !  b  |  ~g                      
*                =====@==>==@==>====!==>==@=====                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1807/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1807).EQ.0) CALL CC19538(C)
      S1=A(146)**4
      TOTNUM=-S1
      TOTDEN=+18
      RNUM=+P1*(C(4)*P2-C(2))+C(1)+C(3)*P2
      F19538=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19539()
*                    ~b1    G    !  G    ~b1                         
*                   -->--@-1-----!---1-@-->--                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1807/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1807).EQ.0) CALL CC19539(C)
      S1=A(146)**4
      TOTNUM=+8*S1
      TOTDEN=+9
      RNUM=+P2*(C(2)+C(3)*P2)-C(1)
      F19539=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1953()
*                    ~o1    t    !  t    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t2|P5     !  ~t2|-P6                          
*                    ~1+ |  S    !  S  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U70/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(70).EQ.0) CALL CC1953(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1953=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19540()
*              ~b1          u    !  u          ~b1                   
*             -->--\     /==>====!==>==\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  H-   !  H- |  b  |  ~g                   
*             =====@==>==@--<----!--<--@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1808/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1808).EQ.0) CALL CC19540(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P2)+C(1)+C(3)*P2
      F19540=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19541()
*                                   !  H-   ~b1                      
*                                  /!--<--@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~u1|-P6                       
*                 ~b1          u   |!  u  |  ~g                      
*                -->--\     /==>===+!==>==@=====                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                  ~g |  b  |  H-  |!                                
*                =====@==>==@--<---/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1808/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1808).EQ.0) CALL CC19541(C)
      S1=A(146)**2
      TOTNUM=+C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19541=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19542()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~u1|-P6                          
*                     ~g |  u    !  u  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1808/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1808).EQ.0) CALL CC19542(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+6
      RNUM=+C(2)*(P2-P1)-C(1)
      F19542=RNUM*(TOTNUM/TOTDEN)*Q2(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19543()
*              ~b1          c    !  c          ~b1                   
*             -->--\     /==>====!==>==\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  H-   !  H- |  b  |  ~g                   
*             =====@==>==@--<----!--<--@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1809/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1809).EQ.0) CALL CC19543(C)
      S1=A(146)**2
      TOTNUM=-C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P2)+C(1)+C(3)*P2
      F19543=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19544()
*                                   !  H-   ~b1                      
*                                  /!--<--@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~c1|-P6                       
*                 ~b1          c   |!  c  |  ~g                      
*                -->--\     /==>===+!==>==@=====                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                  ~g |  b  |  H-  |!                                
*                =====@==>==@--<---/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1809/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1809).EQ.0) CALL CC19544(C)
      S1=A(146)**2
      TOTNUM=+C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19544=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19545()
*                                   !  H-   ~b1                      
*                                  /!--<--@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~c2|-P6                       
*                 ~b1          c   |!  c  |  ~g                      
*                -->--\     /==>===+!==>==@=====                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                  ~g |  b  |  H-  |!                                
*                =====@==>==@--<---/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1809/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1809).EQ.0) CALL CC19545(C)
      S1=A(146)**2
      TOTNUM=+C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19545=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19546()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~c1|-P6                          
*                     ~g |  c    !  c  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1809/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1809).EQ.0) CALL CC19546(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+6
      RNUM=+C(2)*(P2-P1)-C(1)
      F19546=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19547()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~c2|-P6                          
*                     ~g |  c    !  c  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1809/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1809).EQ.0) CALL CC19547(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+3
      RNUM=+C(2)*(P2-P1)-C(1)
      F19547=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19548()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c2|P5     !  ~c2|-P6                          
*                     ~g |  c    !  c  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1809/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1809).EQ.0) CALL CC19548(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+6
      RNUM=+C(2)*(P2-P1)-C(1)
      F19548=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19549()
*              ~b1          t    !  t          ~b1                   
*             -->--\     /==>====!==>==\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  H-   !  H- |  b  |  ~g                   
*             =====@==>==@--<----!--<--@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1810/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1810).EQ.0) CALL CC19549(C)
      S1=A(146)**2
      TOTNUM=-C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P2)+C(1)+C(3)*P2
      F19549=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1954()
*              ~o1          t    !  t          ~o1                   
*             =====\     /==>====!==>==\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  W+ |  B    !  B  |  W+ | ~1+                   
*             ==>==@-1>--@==<====!==<==@-->2-@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1954(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F1954=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19550()
*                                   !  H-   ~b1                      
*                                  /!--<--@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~t1|-P6                       
*                 ~b1          t   |!  t  |  ~g                      
*                -->--\     /==>===+!==>==@=====                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                  ~g |  b  |  H-  |!                                
*                =====@==>==@--<---/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1810/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1810).EQ.0) CALL CC19550(C)
      S1=A(146)**2
      TOTNUM=+C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19550=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19551()
*                                   !  H-   ~b1                      
*                                  /!--<--@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~t2|-P6                       
*                 ~b1          t   |!  t  |  ~g                      
*                -->--\     /==>===+!==>==@=====                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                  ~g |  b  |  H-  |!                                
*                =====@==>==@--<---/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1810/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1810).EQ.0) CALL CC19551(C)
      S1=A(146)**2
      TOTNUM=+C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19551=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19552()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t1|-P6                          
*                     ~g |  t    !  t  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1810/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1810).EQ.0) CALL CC19552(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+6
      RNUM=+C(2)*(P2-P1)-C(1)
      F19552=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19553()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t2|-P6                          
*                     ~g |  t    !  t  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1810/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1810).EQ.0) CALL CC19553(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+3
      RNUM=+C(2)*(P2-P1)-C(1)
      F19553=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19554()
*                    ~b1    H-   !  H-   ~b1                         
*                   -->--@--<----!--<--@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t2|P5     !  ~t2|-P6                          
*                     ~g |  t    !  t  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1810/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1810).EQ.0) CALL CC19554(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+6
      RNUM=+C(2)*(P2-P1)-C(1)
      F19554=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19555()
*              ~b1          b    !  b          ~b1                   
*             -->--\     /==>====!==>==\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  h    !  h  |  b  |  ~g                   
*             =====@==>==@-------!-----@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1811/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1811).EQ.0) CALL CC19555(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P2)+C(1)+C(3)*P2
      F19555=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19556()
*                                   !  h    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b1|-P6                       
*                 ~b1          b   |!  b  |  ~g                      
*                -->--\     /==>===+!==>==@=====                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                  ~g |  b  |  h   |!                                
*                =====@==>==@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1811/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1811).EQ.0) CALL CC19556(C)
      S1=A(146)**2
      TOTNUM=+C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19556=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19557()
*                                   !  h    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          b   |!  b  |  ~g                      
*                -->--\     /==>===+!==>==@=====                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                  ~g |  b  |  h   |!                                
*                =====@==>==@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1811/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1811).EQ.0) CALL CC19557(C)
      S1=A(146)**2
      TOTNUM=+C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19557=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19558()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1811/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1811).EQ.0) CALL CC19558(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+6
      RNUM=+C(2)*(P2-P1)-C(1)
      F19558=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19559()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1811/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1811).EQ.0) CALL CC19559(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+3
      RNUM=+C(2)*(P2-P1)-C(1)
      F19559=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1955()
*              ~o1          t    !  t          ~o1                   
*             =====\     /==>====!==>==\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  W+ |  B    !  B  |  H+ | ~1+                   
*             ==>==@-1>--@==<====!==<==@-->--@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1955(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(3)*P1-C(2))+C(4)*P2-C(1)
      F1955=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19560()
*                    ~b1    h    !  h    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1811/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1811).EQ.0) CALL CC19560(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+6
      RNUM=+C(2)*(P2-P1)-C(1)
      F19560=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19561()
*              ~b1          b    !  b          ~b1                   
*             -->--\     /==>====!==>==\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  H    !  H  |  b  |  ~g                   
*             =====@==>==@-------!-----@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1812/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1812).EQ.0) CALL CC19561(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P2)+C(1)+C(3)*P2
      F19561=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19562()
*                                   !  H    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b1|-P6                       
*                 ~b1          b   |!  b  |  ~g                      
*                -->--\     /==>===+!==>==@=====                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                  ~g |  b  |  H   |!                                
*                =====@==>==@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1812/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1812).EQ.0) CALL CC19562(C)
      S1=A(146)**2
      TOTNUM=+C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19562=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19563()
*                                   !  H    ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          b   |!  b  |  ~g                      
*                -->--\     /==>===+!==>==@=====                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                  ~g |  b  |  H   |!                                
*                =====@==>==@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1812/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1812).EQ.0) CALL CC19563(C)
      S1=A(146)**2
      TOTNUM=+C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19563=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19564()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1812/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1812).EQ.0) CALL CC19564(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+6
      RNUM=+C(2)*(P2-P1)-C(1)
      F19564=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19565()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1812/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1812).EQ.0) CALL CC19565(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+3
      RNUM=+C(2)*(P2-P1)-C(1)
      F19565=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19566()
*                    ~b1    H    !  H    ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1812/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1812).EQ.0) CALL CC19566(C)
      S1=A(146)**2
      TOTNUM=-C(3)*S1
      TOTDEN=+6
      RNUM=+C(2)*(P2-P1)-C(1)
      F19566=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19567()
*              ~b1          b    !  b          ~b1                   
*             -->--\     /==>====!==>==\     /-->--                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  b  |  H3   !  H3 |  b  |  ~g                   
*             =====@==>==@-------!-----@==>==@=====                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1813/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(1813).EQ.0) CALL CC19567(C)
      S1=A(146)**2
      TOTNUM=+C(6)*S1
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P2)+C(1)+C(3)*P2
      F19567=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19568()
*                                   !  H3   ~b1                      
*                                  /!-----@-->--                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~b1          b   |!  b  |  ~g                      
*                -->--\     /==>===+!==>==@=====                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                  ~g |  b  |  H3  |!                                
*                =====@==>==@------/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1813/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1813).EQ.0) CALL CC19568(C)
      S1=A(146)**2
      TOTNUM=-C(5)*S1
      TOTDEN=+C(4)
      RNUM=+C(1)+C(2)*P1+C(3)*P2
      F19568=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19569()
*                    ~b1    H3   !  H3   ~b1                         
*                   -->--@-------!-----@-->--                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1813/ Q0(1),Q1(2),Q2(2)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1813).EQ.0) CALL CC19569(C)
      S1=A(146)**2
      TOTNUM=+C(3)*S1
      TOTDEN=+6
      RNUM=+C(2)*(P2-P1)-C(1)
      F19569=RNUM*(TOTNUM/TOTDEN)*Q2(1)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1956()
*                 ~o1          t    !  t    ~o1                      
*                =====\     /==>====!==>==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~t1|-P6                       
*                 ~1+ |  W+ |  B    !  B  | ~1+                      
*                ==>==@-1>--@==<====!==<==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1956(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(C(4)*P1-C(3)+C(5)*P2)+C(1)+C(2)*P1
      F1956=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19570()
*                     ~g    G    !  G     ~g                         
*                   =====@-1-----!---1-@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                      ~g|P5     !   ~g|-P6                          
*                     ~g |  G    !  G  |  ~g                         
*                   =====@-2-----!---2-@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1814/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1814).EQ.0) CALL CC19570(C)
      S1=A(146)**4
      TOTNUM=-9*S1
      TOTDEN=+1
      RNUM=+P2*(P2-C(2)-P1)+C(1)
      F19570=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19571()
*                                !  G     ~g                         
*                               /!---2-@=====                        
*                               |!  P4 |  P1                         
*                               |!   ~g|-P6                          
*                     ~g    G   |!  G  |  ~g                         
*                   =====@-1----+!---1-@=====                        
*                     P1 |  P3  |!  P3    P2                         
*                      ~g|P5    |!                                   
*                     ~g |  G   |!                                   
*                   =====@-2----/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1814/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1814).EQ.0) CALL CC19571(C)
      S1=A(146)**4
      TOTNUM=+C(2)*S1
      TOTDEN=+4
      RNUM=+P1-C(1)
      F19571=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19572()
*                  ~g    G    !  G           ~g                      
*                =====@-1-----!---1-\     /=====                     
*                  P1 |  P3   !  P3 |     |  P1                      
*                   ~g|P5     !     |     |                          
*                  ~g |  G    !  G  |  G  |  ~g                      
*                =====@-2-----!---2-@---3-@=====                     
*                  P2    P4   !  P4   -P6    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1814/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(2)                                                    
      SAVE
      IF(L(1814).EQ.0) CALL CC19572(C)
      S1=A(146)**4
      TOTNUM=-9*S1
      TOTDEN=+2
      S2=P2**2
      RNUM=+C(1)+C(2)*P1+2*S2
      F19572=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19573()
*               ~g          G    !  G           ~g                   
*             =====\     /-3-----!---3-\     /=====                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  G  |  G    !  G  |  G  |  ~g                   
*             =====@-1---@-2-----!---2-@---4-@=====                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1814/ Q0(2),Q1(3),Q2(3)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1814).EQ.0) CALL CC19573(C)
      S1=A(146)**4
      TOTNUM=-9*S1
      TOTDEN=+1
      RNUM=+P1*(C(2)+P1-P2)+P2*(P2-C(3))+C(1)
      F19573=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19574()
*               ~g          U    !  U           ~g                   
*             =====\     /==<====!==<==\     /=====                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  G  |  u    !  u  |  G  |  ~g                   
*             =====@-1---@==>====!==>==@---2-@=====                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19574(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+2
      RNUM=+P1*(C(2)+P1-2*P2)+P2*(2*P2-C(3))+C(1)
      F19574=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19575()
*                  ~g          U    !  U     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~u1|-P6                       
*                  ~g |  G  |  u    !  u  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19575(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19575=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19576()
*                                   !  u     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~u1|-P6                       
*                  ~g          U   |!  U  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  u   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19576(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      S2=P2**2
      RNUM=+C(1)+C(2)*P1+C(3)*S2
      F19576=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19577()
*                  ~g          U    !  U     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~u2|-P6                       
*                  ~g |  G  |  u    !  u  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19577(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19577=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19578()
*                                   !  u     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~u2|-P6                       
*                  ~g          U   |!  U  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  u   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19578(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      S2=P2**2
      RNUM=+C(1)+C(2)*P1+C(3)*S2
      F19578=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19579()
*                     ~g    U    !  U     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~u1|-P6                          
*                     ~g |  u    !  u  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19579(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19579=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1957()
*                 ~o1          t    !  t    ~o1                      
*                =====\     /==>====!==>==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~t2|-P6                       
*                 ~1+ |  W+ |  B    !  B  | ~1+                      
*                ==>==@-1>--@==<====!==<==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(7)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1957(C)
      TOTNUM=+C(7)
      TOTDEN=+C(6)
      RNUM=+P2*(C(4)*P1-C(3)+C(5)*P2)+C(1)+C(2)*P1
      F1957=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19580()
*                                !  u     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~u1|-P6                          
*                     ~g    U   |!  U  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~u1|P5    |!                                   
*                     ~g |  u   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19580(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19580=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19581()
*                     ~g    U    !  U     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u1|P5     !  ~u2|-P6                          
*                     ~g |  u    !  u  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19581(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19581=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19582()
*                                !  u     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~u2|-P6                          
*                     ~g    U   |!  U  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~u1|P5    |!                                   
*                     ~g |  u   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19582(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19582=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19583()
*                     ~g    u    !  u     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~u1|P5     !  ~u1|-P6                          
*                     ~g |  U    !  U  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19583(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      S2=P2**2
      RNUM=+C(1)*S2
      F19583=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19584()
*                                !  U     ~g                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~u2|-P6                          
*                     ~g    u   |!  u  |  ~g                         
*                   =====@==>===+!==>==@=====                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~u1|P5    |!                                   
*                     ~g |  U   |!                                   
*                   =====@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19584(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19584=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19585()
*                     ~g    u    !  u     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~u1|P5     !  ~u2|-P6                          
*                     ~g |  U    !  U  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19585(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      S2=P2**2
      RNUM=+C(1)*S2
      F19585=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19586()
*                     ~g    U    !  U     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~u2|P5     !  ~u2|-P6                          
*                     ~g |  u    !  u  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19586(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19586=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19587()
*                                !  u     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~u2|-P6                          
*                     ~g    U   |!  U  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~u2|P5    |!                                   
*                     ~g |  u   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19587(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19587=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19588()
*                     ~g    u    !  u     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~u2|P5     !  ~u2|-P6                          
*                     ~g |  U    !  U  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1815/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1815).EQ.0) CALL CC19588(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      S2=P2**2
      RNUM=+C(1)*S2
      F19588=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19589()
*               ~g          D    !  D           ~g                   
*             =====\     /==<====!==<==\     /=====                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  G  |  d    !  d  |  G  |  ~g                   
*             =====@-1---@==>====!==>==@---2-@=====                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19589(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+2
      RNUM=+P1*(C(2)+P1-2*P2)+P2*(2*P2-C(3))+C(1)
      F19589=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1958()
*                                   !  B    ~o1                      
*                                  /!==<==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b1|-P6                       
*                 ~o1          t   |!  t  | ~1+                      
*                =====\     /==>===+!==>==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  W+ |  B   |!                                
*                ==>==@-1>--@==<===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1958(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F1958=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19590()
*                  ~g          D    !  D     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~d1|-P6                       
*                  ~g |  G  |  d    !  d  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19590(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19590=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19591()
*                                   !  d     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~d1|-P6                       
*                  ~g          D   |!  D  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  d   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19591(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      S2=P2**2
      RNUM=+C(1)+C(2)*P1+C(3)*S2
      F19591=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19592()
*                  ~g          D    !  D     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~d2|-P6                       
*                  ~g |  G  |  d    !  d  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19592(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19592=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19593()
*                                   !  d     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~d2|-P6                       
*                  ~g          D   |!  D  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  d   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19593(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      S2=P2**2
      RNUM=+C(1)+C(2)*P1+C(3)*S2
      F19593=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19594()
*                     ~g    D    !  D     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~d1|P5     !  ~d1|-P6                          
*                     ~g |  d    !  d  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19594(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19594=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19595()
*                                !  d     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~d1|-P6                          
*                     ~g    D   |!  D  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~d1|P5    |!                                   
*                     ~g |  d   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19595(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19595=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19596()
*                     ~g    D    !  D     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~d1|P5     !  ~d2|-P6                          
*                     ~g |  d    !  d  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19596(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19596=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19597()
*                                !  d     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~d2|-P6                          
*                     ~g    D   |!  D  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~d1|P5    |!                                   
*                     ~g |  d   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19597(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19597=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19598()
*                     ~g    d    !  d     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~d1|P5     !  ~d1|-P6                          
*                     ~g |  D    !  D  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19598(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      S2=P2**2
      RNUM=+C(1)*S2
      F19598=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19599()
*                                !  D     ~g                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~d2|-P6                          
*                     ~g    d   |!  d  |  ~g                         
*                   =====@==>===+!==>==@=====                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~d1|P5    |!                                   
*                     ~g |  D   |!                                   
*                   =====@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19599(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19599=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1959()
*                                   !  B    ~o1                      
*                                  /!==<==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~o1          t   |!  t  | ~1+                      
*                =====\     /==>===+!==>==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  W+ |  B   |!                                
*                ==>==@-1>--@==<===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1959(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(C(2)+C(3)*P1-C(5)*P2)+P2*(C(6)*P2-C(4))+C(1)
      F1959=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F195()
*                                !  H3   ~o1                         
*                               /!-----@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~o4|-P6                          
*                    ~o1    Z   |!  Z  | ~o1                         
*                   =====@-1----+!---1-@=====                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~o4|P5    |!                                   
*                    ~o1 |  H3  |!                                   
*                   =====@------/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U4/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(4).EQ.0) CALL CC195(C)
      TOTNUM=-C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P2*(8*(P2-P1)+C(5))+C(2)+C(3)*P1)+P2*(C(4)-C(6)*P2)+C(1)
      F195=RNUM*(TOTNUM/TOTDEN)*Q1(9)*Q1(10)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(
     >5)*Q0(6)*Q0(7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19600()
*                     ~g    d    !  d     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~d1|P5     !  ~d2|-P6                          
*                     ~g |  D    !  D  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19600(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      S2=P2**2
      RNUM=+C(1)*S2
      F19600=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19601()
*                     ~g    D    !  D     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~d2|P5     !  ~d2|-P6                          
*                     ~g |  d    !  d  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19601(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19601=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19602()
*                                !  d     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~d2|-P6                          
*                     ~g    D   |!  D  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~d2|P5    |!                                   
*                     ~g |  d   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19602(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19602=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19603()
*                     ~g    d    !  d     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~d2|P5     !  ~d2|-P6                          
*                     ~g |  D    !  D  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1816/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(1)                                                    
      SAVE
      IF(L(1816).EQ.0) CALL CC19603(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      S2=P2**2
      RNUM=+C(1)*S2
      F19603=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19604()
*               ~g          C    !  C           ~g                   
*             =====\     /==<====!==<==\     /=====                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  G  |  c    !  c  |  G  |  ~g                   
*             =====@-1---@==>====!==>==@---2-@=====                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19604(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+2
      RNUM=+P1*(C(2)+P1-2*P2)+P2*(2*P2-C(3))+C(1)
      F19604=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19605()
*                  ~g          C    !  C     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~c1|-P6                       
*                  ~g |  G  |  c    !  c  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19605(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19605=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19606()
*                                   !  c     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~c1|-P6                       
*                  ~g          C   |!  C  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  c   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19606(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P2*(C(4)*P2-C(3))+C(1)+C(2)*P1
      F19606=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19607()
*                  ~g          C    !  C     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~c2|-P6                       
*                  ~g |  G  |  c    !  c  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19607(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19607=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19608()
*                                   !  c     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~c2|-P6                       
*                  ~g          C   |!  C  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  c   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19608(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P2*(C(4)*P2-C(3))+C(1)+C(2)*P1
      F19608=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19609()
*                     ~g    C    !  C     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~c1|-P6                          
*                     ~g |  c    !  c  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19609(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19609=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1960()
*              ~o1          t    !  t          ~o1                   
*             =====\     /==>====!==>==\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  H+ |  B    !  B  |  H+ | ~1+                   
*             ==>==@-->--@==<====!==<==@-->--@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1960(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P1*(C(2)+C(3)*P1)-C(1)
      F1960=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19610()
*                                !  c     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~c1|-P6                          
*                     ~g    C   |!  C  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~c1|P5    |!                                   
*                     ~g |  c   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19610(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19610=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19611()
*                     ~g    C    !  C     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c1|P5     !  ~c2|-P6                          
*                     ~g |  c    !  c  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19611(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19611=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19612()
*                                !  c     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~c2|-P6                          
*                     ~g    C   |!  C  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~c1|P5    |!                                   
*                     ~g |  c   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19612(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19612=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19613()
*                     ~g    c    !  c     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c1|P5     !  ~c1|-P6                          
*                     ~g |  C    !  C  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19613(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19613=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19614()
*                                !  C     ~g                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~c2|-P6                          
*                     ~g    c   |!  c  |  ~g                         
*                   =====@==>===+!==>==@=====                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~c1|P5    |!                                   
*                     ~g |  C   |!                                   
*                   =====@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19614(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19614=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19615()
*                     ~g    c    !  c     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c1|P5     !  ~c2|-P6                          
*                     ~g |  C    !  C  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19615(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19615=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19616()
*                     ~g    C    !  C     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~c2|P5     !  ~c2|-P6                          
*                     ~g |  c    !  c  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19616(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19616=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19617()
*                                !  c     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~c2|-P6                          
*                     ~g    C   |!  C  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~c2|P5    |!                                   
*                     ~g |  c   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19617(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19617=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19618()
*                     ~g    c    !  c     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~c2|P5     !  ~c2|-P6                          
*                     ~g |  C    !  C  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1817/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1817).EQ.0) CALL CC19618(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19618=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19619()
*               ~g          S    !  S           ~g                   
*             =====\     /==<====!==<==\     /=====                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  G  |  s    !  s  |  G  |  ~g                   
*             =====@-1---@==>====!==>==@---2-@=====                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19619(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+2
      RNUM=+P1*(C(2)+P1-2*P2)+P2*(2*P2-C(3))+C(1)
      F19619=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1961()
*                 ~o1          t    !  t    ~o1                      
*                =====\     /==>====!==>==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~t1|-P6                       
*                 ~1+ |  H+ |  B    !  B  | ~1+                      
*                ==>==@-->--@==<====!==<==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1961(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*P2-C(2))+C(3)*P2-C(1)
      F1961=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19620()
*                  ~g          S    !  S     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~s1|-P6                       
*                  ~g |  G  |  s    !  s  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19620(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19620=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19621()
*                                   !  s     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~s1|-P6                       
*                  ~g          S   |!  S  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  s   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19621(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P2*(C(4)*P2-C(3))+C(1)+C(2)*P1
      F19621=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19622()
*                  ~g          S    !  S     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~s2|-P6                       
*                  ~g |  G  |  s    !  s  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19622(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19622=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19623()
*                                   !  s     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~s2|-P6                       
*                  ~g          S   |!  S  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  s   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19623(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P2*(C(4)*P2-C(3))+C(1)+C(2)*P1
      F19623=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19624()
*                     ~g    S    !  S     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~s1|P5     !  ~s1|-P6                          
*                     ~g |  s    !  s  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19624(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19624=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19625()
*                                !  s     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~s1|-P6                          
*                     ~g    S   |!  S  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~s1|P5    |!                                   
*                     ~g |  s   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19625(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19625=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19626()
*                     ~g    S    !  S     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~s1|P5     !  ~s2|-P6                          
*                     ~g |  s    !  s  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19626(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19626=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19627()
*                                !  s     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~s2|-P6                          
*                     ~g    S   |!  S  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~s1|P5    |!                                   
*                     ~g |  s   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19627(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19627=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19628()
*                     ~g    s    !  s     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~s1|P5     !  ~s1|-P6                          
*                     ~g |  S    !  S  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19628(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19628=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19629()
*                                !  S     ~g                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~s2|-P6                          
*                     ~g    s   |!  s  |  ~g                         
*                   =====@==>===+!==>==@=====                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~s1|P5    |!                                   
*                     ~g |  S   |!                                   
*                   =====@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19629(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19629=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1962()
*                 ~o1          t    !  t    ~o1                      
*                =====\     /==>====!==>==@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~t2|-P6                       
*                 ~1+ |  H+ |  B    !  B  | ~1+                      
*                ==>==@-->--@==<====!==<==@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1962(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*P2-C(2))+C(3)*P2-C(1)
      F1962=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19630()
*                     ~g    s    !  s     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~s1|P5     !  ~s2|-P6                          
*                     ~g |  S    !  S  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19630(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19630=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19631()
*                     ~g    S    !  S     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~s2|P5     !  ~s2|-P6                          
*                     ~g |  s    !  s  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19631(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19631=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19632()
*                                !  s     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~s2|-P6                          
*                     ~g    S   |!  S  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~s2|P5    |!                                   
*                     ~g |  s   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19632(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19632=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19633()
*                     ~g    s    !  s     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~s2|P5     !  ~s2|-P6                          
*                     ~g |  S    !  S  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1818/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1818).EQ.0) CALL CC19633(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19633=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19634()
*               ~g          T    !  T           ~g                   
*             =====\     /==<====!==<==\     /=====                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  G  |  t    !  t  |  G  |  ~g                   
*             =====@-1---@==>====!==>==@---2-@=====                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19634(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+2
      RNUM=+P1*(C(2)+P1-2*P2)+P2*(2*P2-C(3))+C(1)
      F19634=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19635()
*                  ~g          T    !  T     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~t1|-P6                       
*                  ~g |  G  |  t    !  t  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19635(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19635=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19636()
*                                   !  t     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~t1|-P6                       
*                  ~g          T   |!  T  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  t   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19636(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P2*(C(4)*P2-C(3))+C(1)+C(2)*P1
      F19636=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19637()
*                  ~g          T    !  T     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~t2|-P6                       
*                  ~g |  G  |  t    !  t  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19637(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19637=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19638()
*                                   !  t     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~t2|-P6                       
*                  ~g          T   |!  T  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  t   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19638(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P2*(C(4)*P2-C(3))+C(1)+C(2)*P1
      F19638=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19639()
*                     ~g    T    !  T     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t1|-P6                          
*                     ~g |  t    !  t  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19639(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19639=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1963()
*                                   !  B    ~o1                      
*                                  /!==<==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b1|-P6                       
*                 ~o1          t   |!  t  | ~1+                      
*                =====\     /==>===+!==>==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  H+ |  B   |!                                
*                ==>==@-->--@==<===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1963(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F1963=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19640()
*                                !  t     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~t1|-P6                          
*                     ~g    T   |!  T  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~t1|P5    |!                                   
*                     ~g |  t   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19640(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19640=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19641()
*                     ~g    T    !  T     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t1|P5     !  ~t2|-P6                          
*                     ~g |  t    !  t  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19641(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19641=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19642()
*                                !  t     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~t2|-P6                          
*                     ~g    T   |!  T  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~t1|P5    |!                                   
*                     ~g |  t   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19642(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19642=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19643()
*                     ~g    t    !  t     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t1|P5     !  ~t1|-P6                          
*                     ~g |  T    !  T  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19643(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19643=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19644()
*                                !  T     ~g                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~t2|-P6                          
*                     ~g    t   |!  t  |  ~g                         
*                   =====@==>===+!==>==@=====                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~t1|P5    |!                                   
*                     ~g |  T   |!                                   
*                   =====@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19644(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19644=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19645()
*                     ~g    t    !  t     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t1|P5     !  ~t2|-P6                          
*                     ~g |  T    !  T  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19645(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19645=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19646()
*                     ~g    T    !  T     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~t2|P5     !  ~t2|-P6                          
*                     ~g |  t    !  t  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19646(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19646=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19647()
*                                !  t     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~t2|-P6                          
*                     ~g    T   |!  T  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~t2|P5    |!                                   
*                     ~g |  t   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19647(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19647=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19648()
*                     ~g    t    !  t     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t2|P5     !  ~t2|-P6                          
*                     ~g |  T    !  T  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1819/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1819).EQ.0) CALL CC19648(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19648=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19649()
*               ~g          B    !  B           ~g                   
*             =====\     /==<====!==<==\     /=====                  
*               P1 |     |  P4   !  P4 |     |  P1                   
*                  |     |       !     |     |                       
*               ~g |  G  |  b    !  b  |  G  |  ~g                   
*             =====@-1---@==>====!==>==@---2-@=====                  
*               P2    P5    P3   !  P3   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19649(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+2
      RNUM=+P1*(C(2)+P1-2*P2)+P2*(2*P2-C(3))+C(1)
      F19649=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1964()
*                                   !  B    ~o1                      
*                                  /!==<==@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~b2|-P6                       
*                 ~o1          t   |!  t  | ~1+                      
*                =====\     /==>===+!==>==@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  H+ |  B   |!                                
*                ==>==@-->--@==<===/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1964(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(4)*(P2-P1)-C(2))+C(3)*P2-C(1)
      F1964=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19650()
*                  ~g          B    !  B     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b1|-P6                       
*                  ~g |  G  |  b    !  b  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19650(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19650=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19651()
*                                   !  b     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~b1|-P6                       
*                  ~g          B   |!  B  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  b   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19651(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P2*(C(4)*P2-C(3))+C(1)+C(2)*P1
      F19651=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19652()
*                  ~g          B    !  B     ~g                      
*                =====\     /==<====!==<==@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~b2|-P6                       
*                  ~g |  G  |  b    !  b  |  ~g                      
*                =====@-1---@==>====!==>==@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19652(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P1*(C(2)+C(5)*P1-C(4)*P2)+P2*(C(5)*P2-C(3))+C(1)
      F19652=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19653()
*                                   !  b     ~g                      
*                                  /!==>==@=====                     
*                                  |!  P3 |  P1                      
*                                  |!  ~b2|-P6                       
*                  ~g          B   |!  B  |  ~g                      
*                =====\     /==<===+!==<==@=====                     
*                  P1 |     |  P4  |!  P4    P2                      
*                     |     |      |!                                
*                  ~g |  G  |  b   |!                                
*                =====@-1---@==>===/!                                
*                  P2    P5    P3   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19653(C)
      S1=A(146)**4
      TOTNUM=+3*S1
      TOTDEN=+8
      RNUM=+P2*(C(4)*P2-C(3))+C(1)+C(2)*P1
      F19653=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19654()
*                     ~g    B    !  B     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19654(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19654=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19655()
*                                !  b     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~b1|-P6                          
*                     ~g    B   |!  B  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~b1|P5    |!                                   
*                     ~g |  b   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19655(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19655=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19656()
*                     ~g    B    !  B     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19656(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19656=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19657()
*                                !  b     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~b2|-P6                          
*                     ~g    B   |!  B  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~b1|P5    |!                                   
*                     ~g |  b   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19657(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19657=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19658()
*                     ~g    b    !  b     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                     ~g |  B    !  B  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19658(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19658=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19659()
*                                !  B     ~g                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                     ~g    b   |!  b  |  ~g                         
*                   =====@==>===+!==>==@=====                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~b1|P5    |!                                   
*                     ~g |  B   |!                                   
*                   =====@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19659(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19659=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1965()
*                    ~o1    t    !  t    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t1|P5     !  ~t1|-P6                          
*                    ~1+ |  B    !  B  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1965(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1965=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19660()
*                     ~g    b    !  b     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                     ~g |  B    !  B  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19660(C)
      S1=A(146)**4
      TOTNUM=+2*S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19660=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19661()
*                     ~g    B    !  B     ~g                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                     ~g |  b    !  b  |  ~g                         
*                   =====@==>====!==>==@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19661(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F19661=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19662()
*                                !  b     ~g                         
*                               /!==>==@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~b2|-P6                          
*                     ~g    B   |!  B  |  ~g                         
*                   =====@==<===+!==<==@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~b2|P5    |!                                   
*                     ~g |  b   |!                                   
*                   =====@==>===/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19662(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+24
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F19662=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19663()
*                     ~g    b    !  b     ~g                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                     ~g |  B    !  B  |  ~g                         
*                   =====@==<====!==<==@=====                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1820/ Q0(4),Q1(5),Q2(5)
      DIMENSION C(3)                                                    
      SAVE
      IF(L(1820).EQ.0) CALL CC19663(C)
      S1=A(146)**4
      TOTNUM=+S1
      TOTDEN=+3
      RNUM=+P2*(C(3)*P2-C(2))+C(1)
      F19663=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1966()
*                    ~o1    t    !  t    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t1|P5     !  ~t2|-P6                          
*                    ~1+ |  B    !  B  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1966(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1966=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1967()
*                                !  B    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~o1    t   |!  t  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~t1|P5    |!                                   
*                    ~1+ |  B   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1967(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1967=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1968()
*                                !  B    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~o1    t   |!  t  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~t1|P5    |!                                   
*                    ~1+ |  B   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1968(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1968=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1969()
*                    ~o1    t    !  t    ~o1                         
*                   =====@==>====!==>==@=====                        
*                     P1 |  P3   !  P3 |  P1                         
*                     ~t2|P5     !  ~t2|-P6                          
*                    ~1+ |  B    !  B  | ~1+                         
*                   ==>==@==<====!==<==@==>==                        
*                     P2    P4   !  P4    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1969(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+P2*(C(3)*P2-C(2))-C(1)
      F1969=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F196()
*                    ~o1    H3   !  H3   ~o1                         
*                   =====@-------!-----@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~o4|P5     !  ~o4|-P6                          
*                    ~o1 |  Z    !  Z  | ~o1                         
*                   =====@-1-----!---1-@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U4/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(8)                                                    
      SAVE
      IF(L(4).EQ.0) CALL CC196(C)
      TOTNUM=+C(8)
      TOTDEN=+C(7)
      RNUM=+P1*(P1*(C(3)+4*P1-8*P2)+P2*(C(5)+4*P2)+C(2))+P2*(C(4)-C(6)*P
     >2)+C(1)
      F196=RNUM*(TOTNUM/TOTDEN)*Q2(10)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(
     >6)*Q0(7)*Q0(8)*Q0(9)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1970()
*                                !  B    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~b1|-P6                          
*                    ~o1    t   |!  t  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~t2|P5    |!                                   
*                    ~1+ |  B   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1970(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1970=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1971()
*                                !  B    ~o1                         
*                               /!==<==@=====                        
*                               |!  P4 |  P1                         
*                               |!  ~b2|-P6                          
*                    ~o1    t   |!  t  | ~1+                         
*                   =====@==>===+!==>==@==>==                        
*                     P1 |  P3  |!  P3    P2                         
*                     ~t2|P5    |!                                   
*                    ~1+ |  B   |!                                   
*                   ==>==@==<===/!                                   
*                     P2    P4   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1971(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1971=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1972()
*                    ~o1    B    !  B    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b1|-P6                          
*                    ~1+ |  t    !  t  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1972(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1972=RNUM*(TOTNUM/TOTDEN)*Q2(5)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(6)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1973()
*                    ~o1    B    !  B    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b1|P5     !  ~b2|-P6                          
*                    ~1+ |  t    !  t  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1973(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1973=RNUM*(TOTNUM/TOTDEN)*Q1(5)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1974()
*                    ~o1    B    !  B    ~o1                         
*                   =====@==<====!==<==@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~b2|P5     !  ~b2|-P6                          
*                    ~1+ |  t    !  t  | ~1+                         
*                   ==>==@==>====!==>==@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U71/ Q0(6),Q1(6),Q2(6)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(71).EQ.0) CALL CC1974(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P1*(C(2)+C(4)*P1-C(3)*P2)+P2*(C(4)*P2-C(2))+C(1)
      F1974=RNUM*(TOTNUM/TOTDEN)*Q2(6)*Q0(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1975()
*              ~o1          h    !  h          ~o1                   
*             =====\     /-------!-----\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  W+ |  H+   !  H+ |  W+ | ~1+                   
*             ==>==@-1>--@-->----!-->--@-->2-@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1975(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1975=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1976()
*                                   !  H+   ~o1                      
*                                  /!-->--@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~1+|-P6                       
*                 ~o1          h   |!  h  | ~1+                      
*                =====\     /------+!-----@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  W+ |  H+  |!                                
*                ==>==@-1>--@-->---/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1976(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1976=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1977()
*              ~o1          h    !  h          ~o1                   
*             =====\     /-------!-----\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  W+ |  H+   !  H+ |  H+ | ~1+                   
*             ==>==@-1>--@-->----!-->--@-->--@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1977(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F1977=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1978()
*                                   !  H+   ~o1                      
*                                  /!-->--@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~2+|-P6                       
*                 ~o1          h   |!  h  | ~1+                      
*                =====\     /------+!-----@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  W+ |  H+  |!                                
*                ==>==@-1>--@-->---/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1978(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1978=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(4)*Q0(2)*Q0(3)*Q0(5)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1979()
*                 ~o1          h    !  h    ~o1                      
*                =====\     /-------!-----@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~o1|-P6                       
*                 ~1+ |  W+ |  H+   !  H+ | ~1+                      
*                ==>==@-1>--@-->----!-->--@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1979(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1979=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(5)*Q0(2)*Q0(3)*Q0(4)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F197()
*                    ~o1    W-   !  W-   ~o1                         
*                   =====@-1<----!--<1-@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~o1 |  W+   !  W+ | ~o1                         
*                   =====@-2>----!-->2-@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U5/ Q0(7),Q1(7),Q2(7)
      DIMENSION C(14)                                                   
      SAVE
      IF(L(5).EQ.0) CALL CC197(C)
      TOTNUM=-C(14)
      TOTDEN=+C(13)
      RNUM=+P2*(P1*(P1*(C(7)-C(12)*P1+C(11)*P2)+P2*(-C(9)-C(11)*P2)-C(6)
     >)+P2*(P2*(C(10)+C(12)*P2)+C(8))+C(5))+P1*(P1*(C(3)-C(4)*P1)+C(2))+
     >C(1)
      F197=RNUM*(TOTNUM/TOTDEN)*Q2(1)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0(7
     >)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1980()
*                 ~o1          h    !  h    ~o1                      
*                =====\     /-------!-----@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~o2|-P6                       
*                 ~1+ |  W+ |  H+   !  H+ | ~1+                      
*                ==>==@-1>--@-->----!-->--@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1980(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1980=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(6)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1981()
*                 ~o1          h    !  h    ~o1                      
*                =====\     /-------!-----@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~o3|-P6                       
*                 ~1+ |  W+ |  H+   !  H+ | ~1+                      
*                ==>==@-1>--@-->----!-->--@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1981(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1981=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(7)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(
     >6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1982()
*                 ~o1          h    !  h    ~o1                      
*                =====\     /-------!-----@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~o4|-P6                       
*                 ~1+ |  W+ |  H+   !  H+ | ~1+                      
*                ==>==@-1>--@-->----!-->--@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1982(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1982=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(8)*Q0(2)*Q0(3)*Q0(4)*Q0(5)*Q0(
     >6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1983()
*                    ~o1    H+   !  H+   ~o1                         
*                   =====@-->----!-->--@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~1+|P5     !  ~1+|-P6                          
*                    ~1+ |  h    !  h  | ~1+                         
*                   ==>==@-------!-----@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1983(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))-C(1)-C(2)*P1
      F1983=RNUM*(TOTNUM/TOTDEN)*Q2(2)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1984()
*                             !  h          ~o1                      
*                            /!-----\     /=====                     
*                            |!  P3 |     |  P1                      
*                            |!     |     |                          
*                 ~o1    H+  |!  H+ |  H+ | ~1+                      
*                =====@-->---+!-->--@-->--@==>==                     
*                  P1 |  P4  |!  P4   -P6    P2                      
*                  ~1+|P5    |!                                      
*                 ~1+ |  h   |!                                      
*                ==>==@------/!                                      
*                  P2    P3   !                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1984(C)
      TOTNUM=+C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F1984=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(3)*Q0(1)*Q0(4)*Q0(5)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1985()
*                    ~o1    H+   !  H+   ~o1                         
*                   =====@-->----!-->--@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~1+ |  h    !  h  | ~1+                         
*                   ==>==@-------!-----@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1985(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))-C(1)-C(2)*P1
      F1985=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(4)*Q0(1)*Q0(3)*Q0(5)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1986()
*                                !  h    ~o1                         
*                               /!-----@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~o1|-P6                          
*                    ~o1    H+  |!  H+ | ~1+                         
*                   =====@-->---+!-->--@==>==                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~1+|P5    |!                                   
*                    ~1+ |  h   |!                                   
*                   ==>==@------/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1986(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1986=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(5)*Q0(1)*Q0(3)*Q0(4)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1987()
*                                !  h    ~o1                         
*                               /!-----@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~o2|-P6                          
*                    ~o1    H+  |!  H+ | ~1+                         
*                   =====@-->---+!-->--@==>==                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~1+|P5    |!                                   
*                    ~1+ |  h   |!                                   
*                   ==>==@------/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1987(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1987=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(6)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1988()
*                                !  h    ~o1                         
*                               /!-----@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~o3|-P6                          
*                    ~o1    H+  |!  H+ | ~1+                         
*                   =====@-->---+!-->--@==>==                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~1+|P5    |!                                   
*                    ~1+ |  h   |!                                   
*                   ==>==@------/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1988(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1988=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(7)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(
     >6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1989()
*                                !  h    ~o1                         
*                               /!-----@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~o4|-P6                          
*                    ~o1    H+  |!  H+ | ~1+                         
*                   =====@-->---+!-->--@==>==                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~1+|P5    |!                                   
*                    ~1+ |  h   |!                                   
*                   ==>==@------/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1989(C)
      TOTNUM=-C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1989=RNUM*(TOTNUM/TOTDEN)*Q1(2)*Q1(8)*Q0(1)*Q0(3)*Q0(4)*Q0(5)*Q0(
     >6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F198()
*                                !  W+   ~o1                         
*                               /!-->2-@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~1+|-P6                          
*                    ~o1    W-  |!  W- | ~o1                         
*                   =====@-1<---+!--<1-@=====                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~1+|P5    |!                                   
*                    ~o1 |  W+  |!                                   
*                   =====@-2>---/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U5/ Q0(7),Q1(7),Q2(7)
      DIMENSION C(13)                                                   
      SAVE
      IF(L(5).EQ.0) CALL CC198(C)
      TOTNUM=-C(13)
      TOTDEN=+C(12)
      RNUM=+P2*(P2*(P1*(C(11)*P1-C(8)-C(10)*P2)+P2*(C(11)*P2-C(9))-C(7))
     >+P1*(C(5)+C(6)*P1)+C(4))+P1*(-C(2)-C(3)*P1)+C(1)
      F198=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(2)*Q0(3)*Q0(4)*Q0(5)*Q0(6)*Q0(7
     >)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1990()
*              ~o1          h    !  h          ~o1                   
*             =====\     /-------!-----\     /=====                  
*               P1 |     |  P3   !  P3 |     |  P1                   
*                  |     |       !     |     |                       
*              ~1+ |  H+ |  H+   !  H+ |  H+ | ~1+                   
*             ==>==@-->--@-->----!-->--@-->--@==>==                  
*               P2    P5    P4   !  P4   -P6    P2                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(4)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1990(C)
      TOTNUM=-C(4)
      TOTDEN=+C(3)
      RNUM=+C(2)*P1-C(1)
      F1990=RNUM*(TOTNUM/TOTDEN)*Q2(3)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1991()
*                                   !  H+   ~o1                      
*                                  /!-->--@=====                     
*                                  |!  P4 |  P1                      
*                                  |!  ~2+|-P6                       
*                 ~o1          h   |!  h  | ~1+                      
*                =====\     /------+!-----@==>==                     
*                  P1 |     |  P3  |!  P3    P2                      
*                     |     |      |!                                
*                 ~1+ |  H+ |  H+  |!                                
*                ==>==@-->--@-->---/!                                
*                  P2    P5    P4   !                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1991(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(3)*P2-C(1)-C(2)*P1
      F1991=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(4)*Q0(1)*Q0(2)*Q0(5)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1992()
*                 ~o1          h    !  h    ~o1                      
*                =====\     /-------!-----@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~o1|-P6                       
*                 ~1+ |  H+ |  H+   !  H+ | ~1+                      
*                ==>==@-->--@-->----!-->--@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1992(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(2)*P1-C(1)+C(3)*P2
      F1992=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(5)*Q0(1)*Q0(2)*Q0(4)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1993()
*                 ~o1          h    !  h    ~o1                      
*                =====\     /-------!-----@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~o2|-P6                       
*                 ~1+ |  H+ |  H+   !  H+ | ~1+                      
*                ==>==@-->--@-->----!-->--@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1993(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(2)*P1-C(1)+C(3)*P2
      F1993=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(6)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1994()
*                 ~o1          h    !  h    ~o1                      
*                =====\     /-------!-----@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~o3|-P6                       
*                 ~1+ |  H+ |  H+   !  H+ | ~1+                      
*                ==>==@-->--@-->----!-->--@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1994(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(2)*P1-C(1)+C(3)*P2
      F1994=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(7)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(
     >6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1995()
*                 ~o1          h    !  h    ~o1                      
*                =====\     /-------!-----@=====                     
*                  P1 |     |  P3   !  P3 |  P1                      
*                     |     |       !  ~o4|-P6                       
*                 ~1+ |  H+ |  H+   !  H+ | ~1+                      
*                ==>==@-->--@-->----!-->--@==>==                     
*                  P2    P5    P4   !  P4    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(5)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1995(C)
      TOTNUM=-C(5)
      TOTDEN=+C(4)
      RNUM=+C(2)*P1-C(1)+C(3)*P2
      F1995=RNUM*(TOTNUM/TOTDEN)*Q1(3)*Q1(8)*Q0(1)*Q0(2)*Q0(4)*Q0(5)*Q0(
     >6)*Q0(7)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1996()
*                    ~o1    H+   !  H+   ~o1                         
*                   =====@-->----!-->--@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~2+|P5     !  ~2+|-P6                          
*                    ~1+ |  h    !  h  | ~1+                         
*                   ==>==@-------!-----@==>==                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1996(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))-C(1)-C(2)*P1
      F1996=RNUM*(TOTNUM/TOTDEN)*Q2(4)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1997()
*                                !  h    ~o1                         
*                               /!-----@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~o1|-P6                          
*                    ~o1    H+  |!  H+ | ~1+                         
*                   =====@-->---+!-->--@==>==                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~2+|P5    |!                                   
*                    ~1+ |  h   |!                                   
*                   ==>==@------/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1997(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1997=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(5)*Q0(1)*Q0(2)*Q0(3)*Q0(6)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1998()
*                                !  h    ~o1                         
*                               /!-----@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~o2|-P6                          
*                    ~o1    H+  |!  H+ | ~1+                         
*                   =====@-->---+!-->--@==>==                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~2+|P5    |!                                   
*                    ~1+ |  h   |!                                   
*                   ==>==@------/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1998(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1998=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(6)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(
     >7)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F1999()
*                                !  h    ~o1                         
*                               /!-----@=====                        
*                               |!  P3 |  P1                         
*                               |!  ~o3|-P6                          
*                    ~o1    H+  |!  H+ | ~1+                         
*                   =====@-->---+!-->--@==>==                        
*                     P1 |  P4  |!  P4    P2                         
*                     ~2+|P5    |!                                   
*                    ~1+ |  h   |!                                   
*                   ==>==@------/!                                   
*                     P2    P3   !                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U72/ Q0(8),Q1(8),Q2(8)
      DIMENSION C(6)                                                    
      SAVE
      IF(L(72).EQ.0) CALL CC1999(C)
      TOTNUM=+C(6)
      TOTDEN=+C(5)
      RNUM=+P2*(C(4)*(P2-P1)-C(3))+C(1)+C(2)*P1
      F1999=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(7)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(
     >6)*Q0(8)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F199()
*                    ~o1    W-   !  W-   ~o1                         
*                   =====@-1<----!--<1-@=====                        
*                     P1 |  P4   !  P4 |  P1                         
*                     ~1+|P5     !  ~2+|-P6                          
*                    ~o1 |  W+   !  W+ | ~o1                         
*                   =====@-2>----!-->2-@=====                        
*                     P2    P3   !  P3    P2                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U5/ Q0(7),Q1(7),Q2(7)
      DIMENSION C(14)                                                   
      SAVE
      IF(L(5).EQ.0) CALL CC199(C)
      TOTNUM=-C(14)
      TOTDEN=+C(13)
      RNUM=+P2*(P1*(P1*(C(7)-C(12)*P1+C(11)*P2)+P2*(-C(9)-C(11)*P2)-C(6)
     >)+P2*(P2*(C(10)+C(12)*P2)+C(8))+C(5))+P1*(P1*(C(3)-C(4)*P1)+C(2))+
     >C(1)
      F199=RNUM*(TOTNUM/TOTDEN)*Q1(1)*Q1(3)*Q0(2)*Q0(4)*Q0(5)*Q0(6)*Q0(7
     >)
      RETURN
      END

*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION F19()
*                 ~o1          Z    !  Z    ~o1                      
*                =====\     /-2-----!---2-@=====                     
*                  P1 |     |  P4   !  P4 |  P1                      
*                     |     |       !  ~o4|-P6                       
*                 ~o1 |  H  |  Z    !  Z  | ~o1                      
*                =====@-----@-1-----!---1-@=====                     
*                  P2    P5    P3   !  P3    P2                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      COMMON/VARS/A(1800)
      COMMON/SCLR/P1,P2,P3,P4,P5,P6,P7,P8,P9,PA,PB,PC,PD,PE,PF,PG,PH
     .,PI,PJ,PK,PL,PM,PN,PO,PP,PQ,PR,PS
      COMMON /U1/ Q0(10),Q1(10),Q2(10)
      DIMENSION C(11)                                                   
      SAVE
      IF(L(1).EQ.0) CALL CC19(C)
      TOTNUM=+C(11)
      TOTDEN=+C(10)
      RNUM=+P1*(P1*(C(7)*P2-C(3)-C(4)*P1)+P2*(C(6)+C(9)*P2)-C(2))+P2*(C(
     >5)+C(8)*P2)-C(1)
      F19=RNUM*(TOTNUM/TOTDEN)*Q1(4)*Q1(10)*Q0(1)*Q0(2)*Q0(3)*Q0(5)*Q0(6
     >)*Q0(7)*Q0(8)*Q0(9)
      RETURN
      END

