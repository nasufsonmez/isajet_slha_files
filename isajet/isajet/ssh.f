#include "PILOT.inc"
      COMPLEX*16 FUNCTION SSH(P,M1,M2)
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
      SSH=4.D0*(((SSA0(M1)+SSA0(M2))/2.D0*(1.D0,0.D0)
     $+(M1SQ+M2SQ-P2/2.D0)*SSB0(P**2,M1,M2)+(M2SQ-M1SQ)/2.D0/P2
     $*((SSA0(M2)-SSA0(M1))*(1.D0,0.D0)
     $-(M2SQ-M1SQ)*SSB0(P**2,M1,M2))
     $+(M1SQ+M2SQ-P2/3.D0)*(1.D0,0.D0))/6.D0)
     $+(P2-M1SQ-M2SQ)*SSB0(P**2,M1,M2)
     $-(SSA0(M1)+SSA0(M2))*(1.D0,0.D0)
      RETURN
      END
