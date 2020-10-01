#include "PILOT.inc"
      SUBROUTINE RDTAPE(IDEV,IFL)
C
C          CALL ROUTINES TO READ AND UNPACK ISAJET DATA
C          RGENS FOR EVENTS
C          RDBEG FOR BEGINNING RECORD
C          REND FOR END RECORD
C
#include "itapes.inc"
#include "ita.inc"
#include "rectp.inc"
#include "zevel.inc"
      ITB=IABS(IDEV)
      CALL RGENS(IFL)
      IF(IFL.NE.0) RETURN
      IF(IRECTP.EQ.200) CALL RDBEG
      IF(IRECTP.EQ.300) CALL REND
      RETURN
      END
