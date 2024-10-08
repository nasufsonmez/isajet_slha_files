#include "PILOT.inc"
        REAL FUNCTION SSGWT7(E)
C-----------------------------------------------------------------------
C          SSGLBF: glss -> wiss + tp + bb
C          Baer's FTBW23/Drees' G_7
C-----------------------------------------------------------------------
#ifdef IMPNONE_X
      IMPLICIT NONE
#endif
#include "sssm.inc"
#include "sspar.inc"
#include "sstmp.inc"
        REAL E
        DOUBLE PRECISION ET,MWI,MG,MT,MSB,MST,PT,EBMX
        DOUBLE PRECISION DEN,T1,SN,DFTBW,EBMN,XX,XL
        DOUBLE PRECISION MB,Z,RDL,DENO,R1,R2,R3,SSDLAM
C
        ET=E
        MWI=TMP(1)
        MG=TMP(2)
        MT=TMP(3)
        MSB=TMP(4)
        MST=TMP(6)
        SN=TMP(8)
        MB=AMBT
C
C            Rewrite PT=DSQRT(ET**2-MT**2)
        PT=DSQRT((ET-MT)*(ET+MT))
        Z=(MG**2+MT**2-2*MG*ET+MB**2-MWI**2)/2.
        R1=1.D0+MT**2/MG**2-2.D0*ET/MG
        R2=MB**2/MG**2
        R3=MWI**2/MG**2
        RDL=DSQRT(DMAX1(0.D0,SSDLAM(R1,R2,R3)))
        DENO=MG**2+MT**2-2*ET*MG
        EBMX=(2*Z*(1.D0-ET/MG)+PT*MG*RDL)*MG/2.D0/DENO
        EBMN=(2*Z*(1.D0-ET/MG)-PT*MG*RDL)*MG/2.D0/DENO
        XX=(MSB**2+2*MG*EBMX-MG**2)/(MSB**2+2*MG*EBMN-MG**2)
        XL=DLOG(XX)
        DEN=MG**2-2*MG*ET+MT**2-MST**2
        T1=2*(EBMX-EBMN)-(MSB**2-MG**2)*XL/MG
        DFTBW=.5*SN*MWI*MT*T1/DEN
        SSGWT7=DFTBW
        RETURN
        END
