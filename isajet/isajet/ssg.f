#include "PILOT.inc"
      COMPLEX*16 FUNCTION SSG(P,M1,M2)
#ifdef IMPNONE_X
      IMPLICIT NONE
#endif
#include "ssinf.inc"
      COMPLEX*16 SSB0
      DOUBLE PRECISION SSA0,M1SQ,M2SQ,P2
      REAL P,M1,M2
      M1SQ=M1*M1
      M2SQ=M2*M2
      P2=P*P
      SSG=(P2-M1SQ-M2SQ)*SSB0(P**2,M1,M2)-SSA0(M1)-SSA0(M2)
      RETURN
      END
