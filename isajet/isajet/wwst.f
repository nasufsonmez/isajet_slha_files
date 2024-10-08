#include "PILOT.inc"
      FUNCTION WWST(T,U,T1,U1,T3,U3,P1,P2)
C          DECAY DISTRIBUTION FOR W+ W- PAIRS FROM SCHOONSCHIP(1980).
C          INTERFERENCE OF T AND S GRAPHS.
#include "itapes.inc"
#include "wwpar.inc"
      DIMENSION P1(4),P2(4)
#ifdef DOUBLE_X
      DOUBLE PRECISION WWST
      DOUBLE PRECISION T,U,T1,U1,T3,U3,P1,P2
      DOUBLE PRECISION CVACQ,EPF
#endif
      CVACQ=(CV+CA)*CQ
      WWST=
     1 +CVACQ*(-256.*T1*U1*T3*U3+256.*T1**2*T3**2)
     1 +CVACQ*T*(256.*T1*U1*T3+256.*T1*T3*S13-256.*T1*T3**2+128.*U1*T3*
     1 U3-128.*U1*U3*S13-128.*U1**2*U3)
     1 +CVACQ*T*U*(-256.*U1*T3-128.*U1*S13-128.*T3*S13-128.*S13**2)
     1 +CVACQ*T*WM2*(384.*U1*T3-128.*U1*S13-128.*U1**2+256.*T3*S13-256.
     1 *T3**2)
     1 +CVACQ*T**2*(-256.*U1*T3-256.*T3*S13)
     1 +CVACQ*U*(128.*T1*U1*T3-128.*T1*T3*S13-128.*T1*T3**2)
      WWST=WWST
     1 +CVACQ*U*WM2*(128.*U1*T3-128.*T3*S13-128.*T3**2)
     1 +CVACQ*WM2*(-256.*T1*U1*T3+512.*T1*T3**2-256.*U1*T3*U3)
     1 +CVACQ*WM2**2*(256.*U1*S13+128.*U1**2+256.*T3*S13+384.*T3**2+128
     1 .*S13**2)
     1 +EPF(P1,P2,P3,Q1)*CVACQ*(128.*T3*U3+128.*T3*S13+64.*T3**2+128.*U
     1 3*S13+64.*U3**2)
     1 +EPF(P1,P2,P3,Q1)*CVACQ*T*(-32.*T3-32.*U3-64.*S13)
     1 -32.*EPF(P1,P2,P3,Q1)*CVACQ*T*WM2
      WWST=WWST
     1 +EPF(P1,P2,P3,Q1)*CVACQ*U*(-32.*T3-32.*U3-64.*S13)
     1 -32.*EPF(P1,P2,P3,Q1)*CVACQ*U*WM2
     1 +EPF(P1,P2,P3,Q1)*CVACQ*WM2*(128.*T3+128.*U3+128.*S13)
     1 +64.*EPF(P1,P2,P3,Q1)*CVACQ*WM2**2
     1 -32.*EPF(P1,P2,P3,Q3)*CVACQ*T*WM2
     1 -32.*EPF(P1,P2,P3,Q3)*CVACQ*U*WM2
     1 +EPF(P1,P2,P3,Q3)*CVACQ*WM2*(64.*T3+64.*U3)
     1 +64.*EPF(P1,P2,P3,Q3)*CVACQ*WM2**2
      WWST=WWST
     1 +EPF(P1,P3,Q1,Q3)*CVACQ*(128.*U1*T3+128.*U1*U3)
     1 +EPF(P1,P3,Q1,Q3)*CVACQ*T*(-64.*U1)
     1 +32.*EPF(P1,P3,Q1,Q3)*CVACQ*T*U
     1 -32.*EPF(P1,P3,Q1,Q3)*CVACQ*T*WM2
     1 +EPF(P1,P3,Q1,Q3)*CVACQ*U*(-64.*U1-64.*T3-64.*U3)
     1 -96.*EPF(P1,P3,Q1,Q3)*CVACQ*U*WM2
     1 +32.*EPF(P1,P3,Q1,Q3)*CVACQ*U**2
     1 +EPF(P1,P3,Q1,Q3)*CVACQ*WM2*(128.*U1+64.*T3+64.*U3)
      WWST=WWST
     1 +64.*EPF(P1,P3,Q1,Q3)*CVACQ*WM2**2
     1 +EPF(P2,P3,Q1,Q3)*CVACQ*(-128.*T1*T3-128.*T1*U3)
     1 +EPF(P2,P3,Q1,Q3)*CVACQ*T*(64.*T1+64.*T3+64.*U3)
     1 -32.*EPF(P2,P3,Q1,Q3)*CVACQ*T*U
     1 +96.*EPF(P2,P3,Q1,Q3)*CVACQ*T*WM2
     1 -32.*EPF(P2,P3,Q1,Q3)*CVACQ*T**2
     1 +EPF(P2,P3,Q1,Q3)*CVACQ*U*(64.*T1)
     1 +32.*EPF(P2,P3,Q1,Q3)*CVACQ*U*WM2
     1 +EPF(P2,P3,Q1,Q3)*CVACQ*WM2*(-128.*T1-64.*T3-64.*U3)
     1 -64.*EPF(P2,P3,Q1,Q3)*CVACQ*WM2**2
      WWST=WWST/T
      WWST=2.*WWST
      RETURN
      END
