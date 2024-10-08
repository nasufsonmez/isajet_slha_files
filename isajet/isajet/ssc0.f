#include "PILOT.inc"
      DOUBLE PRECISION FUNCTION SSC0(M1,M2,M3)
#ifdef IMPNONE_X
      IMPLICIT NONE
#endif
      REAL M1,M2,M3
      DOUBLE PRECISION M1SQ,M2SQ,M3SQ
      M1SQ=M1*M1
      M2SQ=M2*M2
      M3SQ=M3*M3
      IF(M1.NE.M2.AND.M1.NE.M3) THEN
        SSC0=(M2SQ/(M1SQ-M2SQ)*DLOG(M2SQ/M1SQ)-M3SQ/(M1SQ-M3SQ)
     $*DLOG(M3SQ/M1SQ))/(M2SQ-M3SQ)
      ELSEIF(M1.EQ.M2.AND.M1.NE.M3) THEN
        SSC0=(-1.D0-M3SQ/(M1SQ-M3SQ)*DLOG(M3SQ/M1SQ))/(M2SQ-M3SQ)
      ELSEIF(M1.NE.M2.AND.M1.EQ.M3) THEN
        SSC0=(M2SQ/(M1SQ-M2SQ)*DLOG(M2SQ/M1SQ)+1.D0)/(M2SQ-M3SQ)
      ELSEIF(M1.EQ.M2.AND.M1.EQ.M3) THEN
        SSC0=0.D0
      ENDIF
      RETURN
      END
