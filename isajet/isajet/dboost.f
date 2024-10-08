#include "PILOT.inc"
      SUBROUTINE DBOOST(ISIGN,F,P)
C
C          DOUBLE PRECISION BOOST OF 5-VECTOR P BY 5-VECTOR F WITH SIGN
C          OF ISIGN. EXACT COMPONENTS ARE 1,2,5 AND LARGER OF +,-
C
      DIMENSION F(5),P(5)
      DOUBLE PRECISION DF(5),DFPL,DFMN,DP(5),DPPL,DPMN,DBP,DSIGN
C          COPY TO DOUBLE PRECISION
      DO 100 K=1,5
      DF(K)=F(K)
100   DP(K)=P(K)
      IF(ISIGN.GT.0) THEN
        DSIGN=1.D0
      ELSE
        DSIGN=-1.D0
      ENDIF
C          PUT ON DOUBLE PRECISION SHELL
      CALL DBLVEC(P,DP)
C          BOOST
      DBP=0.D0
      DO 110 K=1,3
110   DBP=DBP+DF(K)*DP(K)
      DBP=DBP/DF(5)
      DO 120 K=1,3
120   DP(K)=DP(K)+DSIGN*DF(K)*DP(4)/DF(5)+DF(K)*DBP/(DF(4)+DF(5))
      DP(4)=DF(4)*DP(4)/DF(5)+DSIGN*DBP
C          COPY BACK
      DO 130 K=1,4
130   P(K)=DP(K)
      RETURN
      END
