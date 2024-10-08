#include "PILOT.inc"
      SUBROUTINE BUFIN(IL,IFLAG)
C
C          INVERSE OF BUFOUT.
C          IF CDCPACK IS USED, READ INPUT RECORD INTO ZVOUT AND
C          UNPACK EACH ZVOUT WORD INTO TWO ZEVEL WORDS BY CALL EXPAIR.
C          OTHERWISE, READ ONE INPUT RECORD INTO ZEVEL.
C
#include "itapes.inc"
#include "ita.inc"
#include "zevel.inc"
#include "zvout.inc"
      DIMENSION W(2),IW(2)
      EQUIVALENCE(W(1),IW(1))
      DATA NPARR/0/
    1 CONTINUE
#ifdef CDCPACK_X
C          USE CDC BUFFER IN TO READ PACKED RECORD.
      BUFFER IN(ITB,1) (ZVOUT(1),ZVOUT(512))
      IF(UNIT(ITB,ZVOUT(1),ZVOUT(512))) 300,200,100
#elif defined(STDIO_X)
C          STANDARD FORTRAN 77 READ.
      CALL ZEROL(ZEVEL,MAXLEN)
      READ(ITB,ERR=100,END=200) IZVL1,IZVL2,(ZEVEL(JJ),JJ=3,IZVL2)
      GO TO 300
#endif
C            TAPE READ ERROR
  100 WRITE(ITLIS,10) ITB
      NPARR=NPARR+1
   10 FORMAT(1X,' TAPE READ ERROR ON TAPE',I3)
      IFLAG=1
      IF(NPARR.LT.20) GOTO 1
C            END OF FILE
  200 IFLAG=-1
      RETURN
C            GOOD RECORD
  300 IFLAG=0
#ifdef CDCPACK_X
C          USE CDC ASSEMBLY LANGUAGE ROUTINE EXPAIR TO UNPACK 1 ZVOUT
C          WORDS INTO 2 ZEVEL WORDS.
      WOUT=ZVOUT(1)
      CALL EXPAIR(W(1),W(2),WOUT,IFL)
      IZEVEL(1)=IW(1)
      IZEVEL(2)=IW(2)
      IL=IW(2)
      NW=IL/2+MOD(IL,2)
      DO 310 I=2,NW
      WOUT=ZVOUT(I)
      II=2*I-1
      CALL EXPAIR(W(1),W(2),WOUT,IFL)
      CALL MOVLEV(W,IZEVEL(II),2)
  310 CONTINUE
#endif
      IL=3
      RETURN
      END
