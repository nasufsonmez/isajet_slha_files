#include "PILOT.inc"
      FUNCTION GAMMA(X)
#include "itapes.inc"
      DIMENSION C(13)
      DATA C
     1/ 0.00053 96989 58808, 0.00261 93072 82746, 0.02044 96308 23590,
     2  0.07309 48364 14370, 0.27964 36915 78538, 0.55338 76923 85769,
     3  0.99999 99999 99998,-0.00083 27247 08684, 0.00469 86580 79622,
     4  0.02252 38347 47260,-0.17044 79328 74746,-0.05681 03350 86194,
     5  1.13060 33572 86556/
      Z=X
      IF(X .GT. 0.0) GO TO 1
      IF(X .EQ. AINT(X)) GO TO 5
      Z=1.0-Z
    1 F=1.0/Z
      IF(Z .LE. 1.0) GO TO 4
      F=1.0
    2 IF(Z .LT. 2.0) GO TO 3
      Z=Z-1.0
      F=F*Z
      GO TO 2
    3 Z=Z-1.0
    4 GAMMA=
     1 F*((((((C(1)*Z+C(2))*Z+C(3))*Z+C(4))*Z+C(5))*Z+C(6))*Z+C(7))/
     2   ((((((C(8)*Z+C(9))*Z+C(10))*Z+C(11))*Z+C(12))*Z+C(13))*Z+1.0)
      IF(X .GT. 0.0) RETURN
      GAMMA=3.141592653589793/(SIN(3.141592653589793*X)*GAMMA)
      RETURN
    5 GAMMA=0.
      WRITE(ITLIS,10) X
      RETURN
   10 FORMAT(1X,'GAMMA ... ARGUMENT IS NON-POSITIVE INTEGER = ',E20.5)
      END
