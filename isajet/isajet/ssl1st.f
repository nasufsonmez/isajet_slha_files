#include "PILOT.inc"
        REAL FUNCTION SSL1ST(SS)
C-----------------------------------------------------------------------
C          SSL1ST: l_1 -> stau_1+nu_l+nutaubar: TATA F FUNCTION
C-----------------------------------------------------------------------
#ifdef IMPNONE_X
      IMPLICIT NONE
#endif
#include "sssm.inc"
#include "sspar.inc"
#include "sstmp.inc"
        REAL SS
        DOUBLE PRECISION S,M1,M2,MST1,ML1,WID
        S=SS
        M1=TMP(1)
        M2=TMP(2)
        MST1=TMP(3)
        ML1=TMP(4)
        WID=(S-MST1**2)**2/(S-M1**2)/(S-M2**2)*(S-ML1**2)**2
     $       /S/ML1**2
        SSL1ST=WID
        RETURN
        END
