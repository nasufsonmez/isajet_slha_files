#include "PILOT.inc"
      DOUBLE PRECISION FUNCTION SSD0(M1,M2,M3,M4)
#ifdef IMPNONE_X
      IMPLICIT NONE
#endif
      REAL M1,M2,M3,M4
      DOUBLE PRECISION M1SQ,M2SQ,M3SQ,M4SQ,SSC134,SSC234
      M1SQ=M1*M1
      M2SQ=M2*M2
      M3SQ=M3*M3
      M4SQ=M4*M4
      IF(M1.NE.M3.AND.M1.NE.M4) THEN
        SSC134=(M3SQ/(M1SQ-M3SQ)*DLOG(M3SQ/M1SQ)-M4SQ/(M1SQ-M4SQ)
     $*DLOG(M4SQ/M1SQ))/(M3SQ-M4SQ)
      ELSEIF(M1.EQ.M3.AND.M1.NE.M4) THEN
        SSC134=(-1.D0-M4SQ/(M1SQ-M4SQ)*DLOG(M4SQ/M1SQ))/(M3SQ-M4SQ)
      ELSEIF(M1.NE.M3.AND.M1.EQ.M4) THEN
        SSC134=(M3SQ/(M1SQ-M3SQ)*DLOG(M3SQ/M1SQ)+1.D0)/(M3SQ-M4SQ)
      ELSEIF(M1.EQ.M3.AND.M1.EQ.M4) THEN
        SSC134=0.D0
      ENDIF
      IF(M2.NE.M3.AND.M2.NE.M4) THEN
        SSC234=(M3SQ/(M2SQ-M3SQ)*DLOG(M3SQ/M2SQ)-M4SQ/(M2SQ-M4SQ)
     $*DLOG(M4SQ/M2SQ))/(M3SQ-M4SQ)
      ELSEIF(M2.EQ.M3.AND.M2.NE.M4) THEN
        SSC234=(-1.D0-M4SQ/(M2SQ-M4SQ)*DLOG(M4SQ/M2SQ))/(M3SQ-M4SQ)
      ELSEIF(M2.NE.M3.AND.M2.EQ.M4) THEN
        SSC234=(M3SQ/(M2SQ-M3SQ)*DLOG(M3SQ/M2SQ)+1.D0)/(M3SQ-M4SQ)
      ELSEIF(M2.EQ.M3.AND.M2.EQ.M4) THEN
        SSC234=0.D0
      ENDIF
      IF(M1.NE.M2) THEN
        SSD0=(SSC134-SSC234)/(M1SQ-M2SQ)
      ELSE
        SSD0=0.D0
      ENDIF
      RETURN
      END
