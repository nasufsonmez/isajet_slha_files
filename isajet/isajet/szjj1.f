#include "PILOT.inc"
      REAL*8 FUNCTION SZJJ1(P1, P2, P3, P4, P5,IM1,IM2)
C
C Function generated by Madgraph + hand coding 
C Returns amplitude squared summed/avg over colors
C and helicities
C for the point in phase space P1,P2,P3,P4,...
C for process : q(im1) q~(im1)  -> z q(im2) q~(im2)
C with Madgraph codes IM1 != IM2  
C
#ifdef IMPNONE_X
      IMPLICIT NONE
#endif
C  
C CONSTANTS
C  
      INTEGER    NEXTERNAL,   NCOMB                     
      PARAMETER (NEXTERNAL=5, NCOMB= 48)
C  
C ARGUMENTS 
C  
      REAL*8 P1(0:3),P2(0:3),P3(0:3),P4(0:3),P5(0:3)                             
      INTEGER IM1,IM2
C  
C LOCAL VARIABLES 
C  
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY                                         
      REAL*8 T
      REAL*8 ZJJ1                                                            
      INTEGER IHEL
      LOGICAL GOODHEL(NCOMB)
      DATA GOODHEL/NCOMB*.FALSE./
      DATA NTRY/0/
      DATA (NHEL(IHEL,  1),IHEL=1,5) / -1, -1, -1, -1, -1/
      DATA (NHEL(IHEL,  2),IHEL=1,5) / -1, -1, -1, -1,  1/
      DATA (NHEL(IHEL,  3),IHEL=1,5) / -1, -1, -1,  1, -1/
      DATA (NHEL(IHEL,  4),IHEL=1,5) / -1, -1, -1,  1,  1/
      DATA (NHEL(IHEL,  5),IHEL=1,5) / -1, -1,  0, -1, -1/
      DATA (NHEL(IHEL,  6),IHEL=1,5) / -1, -1,  0, -1,  1/
      DATA (NHEL(IHEL,  7),IHEL=1,5) / -1, -1,  0,  1, -1/
      DATA (NHEL(IHEL,  8),IHEL=1,5) / -1, -1,  0,  1,  1/
      DATA (NHEL(IHEL,  9),IHEL=1,5) / -1, -1,  1, -1, -1/
      DATA (NHEL(IHEL, 10),IHEL=1,5) / -1, -1,  1, -1,  1/
      DATA (NHEL(IHEL, 11),IHEL=1,5) / -1, -1,  1,  1, -1/
      DATA (NHEL(IHEL, 12),IHEL=1,5) / -1, -1,  1,  1,  1/
      DATA (NHEL(IHEL, 13),IHEL=1,5) / -1,  1, -1, -1, -1/
      DATA (NHEL(IHEL, 14),IHEL=1,5) / -1,  1, -1, -1,  1/
      DATA (NHEL(IHEL, 15),IHEL=1,5) / -1,  1, -1,  1, -1/
      DATA (NHEL(IHEL, 16),IHEL=1,5) / -1,  1, -1,  1,  1/
      DATA (NHEL(IHEL, 17),IHEL=1,5) / -1,  1,  0, -1, -1/
      DATA (NHEL(IHEL, 18),IHEL=1,5) / -1,  1,  0, -1,  1/
      DATA (NHEL(IHEL, 19),IHEL=1,5) / -1,  1,  0,  1, -1/
      DATA (NHEL(IHEL, 20),IHEL=1,5) / -1,  1,  0,  1,  1/
      DATA (NHEL(IHEL, 21),IHEL=1,5) / -1,  1,  1, -1, -1/
      DATA (NHEL(IHEL, 22),IHEL=1,5) / -1,  1,  1, -1,  1/
      DATA (NHEL(IHEL, 23),IHEL=1,5) / -1,  1,  1,  1, -1/
      DATA (NHEL(IHEL, 24),IHEL=1,5) / -1,  1,  1,  1,  1/
      DATA (NHEL(IHEL, 25),IHEL=1,5) /  1, -1, -1, -1, -1/
      DATA (NHEL(IHEL, 26),IHEL=1,5) /  1, -1, -1, -1,  1/
      DATA (NHEL(IHEL, 27),IHEL=1,5) /  1, -1, -1,  1, -1/
      DATA (NHEL(IHEL, 28),IHEL=1,5) /  1, -1, -1,  1,  1/
      DATA (NHEL(IHEL, 29),IHEL=1,5) /  1, -1,  0, -1, -1/
      DATA (NHEL(IHEL, 30),IHEL=1,5) /  1, -1,  0, -1,  1/
      DATA (NHEL(IHEL, 31),IHEL=1,5) /  1, -1,  0,  1, -1/
      DATA (NHEL(IHEL, 32),IHEL=1,5) /  1, -1,  0,  1,  1/
      DATA (NHEL(IHEL, 33),IHEL=1,5) /  1, -1,  1, -1, -1/
      DATA (NHEL(IHEL, 34),IHEL=1,5) /  1, -1,  1, -1,  1/
      DATA (NHEL(IHEL, 35),IHEL=1,5) /  1, -1,  1,  1, -1/
      DATA (NHEL(IHEL, 36),IHEL=1,5) /  1, -1,  1,  1,  1/
      DATA (NHEL(IHEL, 37),IHEL=1,5) /  1,  1, -1, -1, -1/
      DATA (NHEL(IHEL, 38),IHEL=1,5) /  1,  1, -1, -1,  1/
      DATA (NHEL(IHEL, 39),IHEL=1,5) /  1,  1, -1,  1, -1/
      DATA (NHEL(IHEL, 40),IHEL=1,5) /  1,  1, -1,  1,  1/
      DATA (NHEL(IHEL, 41),IHEL=1,5) /  1,  1,  0, -1, -1/
      DATA (NHEL(IHEL, 42),IHEL=1,5) /  1,  1,  0, -1,  1/
      DATA (NHEL(IHEL, 43),IHEL=1,5) /  1,  1,  0,  1, -1/
      DATA (NHEL(IHEL, 44),IHEL=1,5) /  1,  1,  0,  1,  1/
      DATA (NHEL(IHEL, 45),IHEL=1,5) /  1,  1,  1, -1, -1/
      DATA (NHEL(IHEL, 46),IHEL=1,5) /  1,  1,  1, -1,  1/
      DATA (NHEL(IHEL, 47),IHEL=1,5) /  1,  1,  1,  1, -1/
      DATA (NHEL(IHEL, 48),IHEL=1,5) /  1,  1,  1,  1,  1/
C ----------
C BEGIN CODE
C ----------
      SZJJ1 = 0D0
      NTRY=NTRY+1
      DO IHEL=1,NCOMB
          IF (GOODHEL(IHEL) .OR. NTRY .LT. 10) THEN
             T=ZJJ1(P1, P2, P3, P4, P5,NHEL(1,IHEL),IM1,IM2) 
             SZJJ1 = SZJJ1 + T
              IF (T .GT. 0D0 .AND. .NOT. GOODHEL(IHEL)) THEN
                  GOODHEL(IHEL)=.TRUE.
              ENDIF
          ENDIF
      ENDDO
      SZJJ1 = SZJJ1 /  4D0 
      END
