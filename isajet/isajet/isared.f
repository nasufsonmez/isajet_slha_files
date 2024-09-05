#include "PILOT.inc"
#ifdef ISATOOLS_X
cccccccccccccccccccccccccccccccccccccccccccccc
c      AUTHORS: Baer,Balazs,Belyaev
c
c      Last modification -> 10/27/2005 A.Belyaev
c      Last modification -> 10/04/2007 A.Belyaev
c      Last modification -> 12/06/2007 A.Belyaev -> sigma*v (v->0) added
c
cccccccccccccccccccccccccccccccccccccccccccccc
     
      SUBROUTINE ISARED(IPRT)
c      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT NONE
      
	
c----------USER--------------------------------      
      COMMON /CTRL/
     &AS_MAX,XFINI,XF,COS_MIN,COS_MAX,CS_MIN,SUPPEXP, SUPPEXPMAX,
     &NDIMUSER,NDIMUSER_EFF,ISUM,NST_MAX,
     &NPROC_MIN,NPROC_MAX,NPROC_STEP

      REAL*8 AS_MAX,XFINI,XF,COS_MIN,COS_MAX,CS_MIN,SUPPEXP, SUPPEXPMAX
      INTEGER NDIMUSER,NDIMUSER_EFF,ISUM,NST_MAX,
     &NPROC_MIN,NPROC_MAX,NPROC_STEP
     

      INTEGER IPRINT
      
      DATA   AS_MAX / 3D0 /, XFINI/0.05D0/, NDIMUSER/3/, NST_MAX/10/,
     &      NPROC_MIN/1/,   NPROC_MAX/1820/, NPROC_STEP/1/, IPRINT/1/,
     &      COS_MIN / -0.999999d0 / , COS_MAX / 0.999999d0 /,
     &      CS_MIN  /0d0/, ISUM/1/, SUPPEXPMAX/2d0/
     

      common/printlevel/NPRINT
      INTEGER NPRINT
c-----------------------------------------------      
      COMMON   /GOOD/    NNOGOOD,IALLOW 
      INTEGER  NNOGOOD,IALLOW                                                                                                       
c------BASES------------------------------------\
     
      INTEGER NDIM,IG,MXDIM,NWILD,NCALL
      REAL*8 XL,XU
   
      PARAMETER (MXDIM = 50 )                                           
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,IG(MXDIM),NCALL    
      DATA NWILD/2/,NCALL/1000/      
      
      INTEGER INTV, IPNT, NLOOP, MLOOP,ITMX1,ITMX2
      REAL*8 ACC1,ACC2
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2                             
      DATA  ITMX1/5/,ITMX2/5/,ACC1/1d0/,ACC2/1d0/
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
c-----------------------------------------------
      REAL*8 xb,g,pm
      CHARACTER*6 names
      INTEGER J,idof,IPTOT
      COMMON /INPART/ xb(29),g(29),pm(29),idof(29),IPTOT
      COMMON /NAMES/ names(29)

      DATA IPTOT/29/
      DATA  (IDOF(J),J=1,29)
     _/  2,     2 ,     16,
     _   2 ,	1 ,     1,      1,      3,     3, 
     _  	1 ,     1 ,     1,      3,     3,    3,    3,
     _   2 ,    1 ,     1,      1,      3,     3,
     _  	1 ,     1 ,     1,      3,     3,    3,    3/

     
      DATA  (NAMES(J),J=1,29)
     _/'~o1',  '~o2' ,'~g',
     _  '~1-' ,'~e1' , '~e2' , '~e3' ,'~t1','~b1', 
     _         '~n1' , '~n2' , '~n3' ,'~u1','~d1','~c1','~s1',
     _  '~1+' ,'~E1' , '~E2' , '~E3' ,'~T1','~B1', 
     _         '~N1' , '~N2' , '~N3' ,'~U1','~D1','~C1','~S1'/
c-----------------------------------------------
      REAL*4 OMGH2,SIGMA,XFREEZ,FFF_V
      INTEGER NSTEPS
      COMMON/SUGRED/ OMGH2,SIGMA,XFREEZ,NSTEPS,FFF_V
c-----ISAJET-----------------------------------   
#include "sslun.inc"
#include "sugxin.inc"
#include "sugmg.inc"
#include "sugpas.inc"
c----------COMPHEP------------------------------      
      COMMON/VARS/A(1800)
      REAL*8 A
c-----------------------------------------------      
c-----------------------------------------------      
      CHARACTER*6 PINF
      REAL*8 PBNTOGEV,PI,SQRT_GN,FFF,XFI,XF_OLD,SUMM,GEFF,
     &FUNC_INT,FFF_TMP,OMEGA,FFF_OLD
      
      INTEGER IPRT,NPROC_MAX_S,NPROC_MIN_S,IPM,IDEL,
     &NDIMUSER_SAVE,NST,NDIMUSER_EFF_SAVE, NCALL_SAV,I
    
      
c-----------------------------------------------      
      REAL*8 del(29)
    
      REAL*8 vvv,AS_MAX_SAVE,conv1,conv2,CS_V
      
      
      IPRINT=IPRT
      NPRINT=IPRINT
      
      XF=XFINI
      FFF=0d0
	
      SQRT_GN=dsqrt(67.07d0)/1d+20
      PBNTOGEV=1d0/0.3893796623/1d+09	
       
      Pi=DACOS(-1d0)
      
      FFF=0
      NNOGOOD=0
      
      call vini
***************************

      call isachp

*****************************
! Z1,Z2,W1, e1, e2, e3, n1,n2,n3,u1,d1,c1,s1,t1,b1,gss
****************************
	PM(1)=abs(A(93))   	!neutr1
        PM(2)=abs(A(95))   	!neutr2
        PM(3)=abs(A(131))  	!gluino
	
        PM(4)=abs(A(89))   	!charg
        PM(5)=abs(A(132))  	!sel
        PM(6)=abs(A(134))  	!smu
        PM(7)=abs(A(106))  	!sta
        PM(8)=abs(A(122))   	!stop
        PM(9)=abs(A(126))  	!sbot
	
   	
        PM(10)=abs(A(136))  	!snu_e
        PM(11)=abs(A(137))  	!snu_l
        PM(12)=abs(A(112))  	!snu_tau
        PM(13)=abs(A(138))  	!sup1
        PM(14)=abs(A(140))  	!sd_1
        PM(15)=abs(A(142))  	!sc1
        PM(16)=abs(A(144))  	!ss_1
	
        PM(17)=	PM(4)		!charg
        PM(18)=	PM(5)  		!sel
        PM(19)=	PM(6)  		!smu
        PM(20)=	PM(7)  		!sta
        PM(21)=	PM(8)		!stop
        PM(22)=	PM(9)  		!sbot
	
 
        PM(23)=	PM(10)  	!snu_e
        PM(24)=	PM(11)  	!snu_l
        PM(25)=	PM(12)  	!snu_tau
        PM(26)=	PM(13)  	!sup1
        PM(27)=	PM(14)  	!sd_1
        PM(28)=	PM(15)  	!sc1
        PM(29)=	PM(17)  	!ss_1
***********************************************      

 
      SUPPEXP=1E+20
 
      DO IPM=2,IPTOT
        if(min(pm(IPM),PM(1)).ne.pm(1).and.iallow.eq.0) then
          iallow=-11 !NEUTR IS NOT THE LIGHTEST
          if(iprt.gt.2) print *,'Z1 IS NOT LSP',IPM,PM(IPM),PM(1)
          NNOGOOD=5
          goto 555
          ENDIF 
          SUPPEXP=min(SUPPEXP,pm(IPM)/pm(1))
      ENDDO

      geff=0d0
      DO IDEL=1,IPTOT
        del(idel)=(pm(idel)-pm(1))/pm(1)
        geff=geff+ 2. * (1.+del(IDEL))**(3./2.) * Exp(-del(idel)/XF)
        if(iprint.gt.2) 
     &print *,  (1.+del(IDEL))**(3./2.) * Exp(-del(idel)/XF), del(idel)
      ENDDO
      IF(IPRINT.gt.1) print *,'GEFF=', GEFF,'   ','SUPPEXP=',SUPPEXP
      NDIMUSER_EFF=NDIMUSER
      IF(SUPPEXP.ge.SUPPEXPMAX.and.NDIMUSER.eq.3)  NDIMUSER_EFF=2 
      if(iprint.gt.2) 
     & print *, 'NDIMUSER_EFF=', NDIMUSER_EFF,SUPPEXP
     

cccccccccccccccccc    
c      goto 111

      NST=0
      NDIMUSER_SAVE=NDIMUSER
      NDIMUSER_EFF_SAVE=NDIMUSER_EFF
c       print *,'ISARED:', NPROC_MIN,NPROC_MAX

      IF(NDIMUSER.ge.2) NDIMUSER=2
      NDIMUSER_EFF=NDIMUSER
      NCALL_SAV=NCALL
      NCALL=NCALL/2.
888   continue
      NST=NST+1
      IF(NST.GT.NST_MAX) goto 666
      
      IPRINT=IPRT
c--------------------	
      FFF_OLD=FFF  
      FFF=FUNC_INT(IPRINT)
      IF(FFF.lt.CS_MIN) GOTO  666
c--------------------	  

      IF(NDIMUSER_SAVE.eq.2) GOTO 777
      if(IPRT.gt.1) print *,'FFF0=', FFF

        
      IF(FFF.le.0) then
         FFF=0.
         goto 666
      ENDIF


       XFI=LOG(
     &     PM(1)/(2d0*Pi**3)*geff/2d0*sqrt(45d0/(2d0*81d0))/SQRT_GN
     &     *FFF*PBNTOGEV*SQRT(XF)
     &     )
       

       XF_OLD=XF
       XF=1D0/XFI
       


       IF(iprint.gt.1) then
       print *,'======================='
       print *,'XF = ',XF,1d0/XF
       print *,'CS  = ',FFF
       print *,'NST = ',NST
       print *,'======================='
       endif

       IF(XF.le.0.) then
         XF=XFINI
         FFF=0
         goto 666
       ENDIF

ccc       IF(abs(XF-XF_OLD)/XF.gt.0.01) goto 888
       IF(abs(FFF-FFF_OLD)/FFF.gt.0.03) goto 888
ccc    print *,'xxxxxxxx 3d Integration xxxxxxxxxx'
 777   CONTINUE
 666   NCALL=NCALL*2.
       NDIMUSER=NDIMUSER_SAVE
       NDIMUSER_EFF=NDIMUSER_EFF_SAVE
       IF(NDIMUSER.eq.2    ) goto 999
       IF(NDIMUSER_EFF.eq.2) NCALL=NCALL/2.

       IF(FFF.lt.CS_MIN) THEN
          FFF=1.E-20
          GOTO 999
       ENDIF


       FFF=FUNC_INT(IPRINT)
 999   continue
  
c      print *,'IPRINT=',IPRINT
       IF(IPRINT.ge.3) then
         NPROC_MAX_S=NPROC_MAX
         NPROC_MIN_S=NPROC_MIN
         SUMM=0.
         
         
         DO I =NPROC_MIN_S,NPROC_MAX_S,NPROC_STEP
           if(SUPPEXP.ge.SUPPEXPMAX.and.I.gt.26) GOTO 444
           NPROC_MIN=I
           NPROC_MAX=I
           FFF_TMP=FUNC_INT(-1)
           IF(FFF_TMP/FFF.gt.0.01) THEN
             print '(I6,A4,F6.2,A2,4A8)',
     &       I,'  ',FFF_TMP/FFF*100,' %',
     &       (PINF(I,J),J=1,4)
             SUMM=SUMM+FFF_TMP/FFF
           ENDIF
         ENDDO
 444     continue	 

         NPROC_MAX=NPROC_MAX_S
         NPROC_MIN=NPROC_MIN_S
       endif

      
 
 555   continue
       if(iprint.gt.1) then
       print *,'===========FINAL======='
       print *, 'freez out temp=',1d0/XF
       print *, 'n steps       =',NST
       print *, 'CS (fb)       =',FFF*1000d0
       print *, 'OMEGA H^2     =',OMEGA(FFF)
       print *,'======================='
       endif

       OMGH2 =OMEGA(FFF)
       SIGMA =FFF
       XFREEZ=XF
       NSTEPS =NST

       NCALL=NCALL_SAV
      
cccccccccccccccccccccccccccccccccccccccccccc
       if(NNOGOOD.eq.5) RETURN
 111   continue
       NDIMUSER_SAVE=NDIMUSER
       AS_MAX_SAVE  =AS_MAX
       NDIMUSER=1
       NDIMUSER_EFF=1

       
       VVV=1.E-03
       AS_MAX=2.*sqrt(1.+(VVV/2.)**2)  
       CONV1 =  2.998E+10  ! speed of light cm/sec
       CONV2 =  1.000E-36  ! 1pb ==> cm^2

       CS_V  =  FUNC_INT(IPRINT)
       FFF_V =  CS_V*VVV*CONV1*CONV2

       NDIMUSER=NDIMUSER_SAVE
       AS_MAX=AS_MAX_SAVE
       

cccccccccccccccccccccccccccccccccccccccccccc       

      RETURN
      END

******************************************************************
******************************************************************
******************************************************************
      FUNCTION FUNC(X)


      IMPLICIT NONE
      
      CHARACTER*6 PINF,PN1,PN2
      REAL*8 A
      COMMON/VARS/A(1800)
 
      COMMON /CTRL/
     &AS_MAX,XFINI,XF,COS_MIN,COS_MAX,CS_MIN,SUPPEXP, SUPPEXPMAX,
     &NDIMUSER,NDIMUSER_EFF,ISUM,NST_MAX,
     &NPROC_MIN,NPROC_MAX,NPROC_STEP

      REAL*8 AS_MAX,XFINI,XF,COS_MIN,COS_MAX,CS_MIN,SUPPEXP, SUPPEXPMAX
      INTEGER NDIMUSER,NDIMUSER_EFF,ISUM,NST_MAX,
     &NPROC_MIN,NPROC_MAX,NPROC_STEP
      
      REAL*8 X(25)
      REAL*8 P(0:3,4)

      REAL*8 RR,SN,PI,GEVTOPBN,FUNC,ds_da_dc,AS_MIN,PM,E,XMASS,Z,
     &COS1, PM1,PM2,PM3,PM4,SIN1,SFACT,CFACT,TFACT,B1,B2,FA,AS,DJACOB,
     &ECM,XFA,FA12,E1,E2,R1,R2,E3,R3,E4,R4,F_PS,FA12_integr,FUNC_TMP,
     &SQME
     
      INTEGER I
      
      RR(E,XMASS)  = SQRT(MAX(0d0,(E-XMASS)*(E+XMASS)))
      SN(Z)        = SQRT(MAX(0d0,(1-z)*(1+z)))
      
      
      PI=DACOS(-1d0)
      GEVTOPBN= 0.3893796623*1.E+09      
      
      FUNC=0.
      ds_da_dc=0.
      

 
      AS_MIN=2d0
      PM=abs(A(93))
C------------------------------------------ 
      COS1=1d0-2d0*X(1)


      IF(COS1.gt.COS_MAX) goto 999
      IF(COS1.lt.cos_min) goto 999
      SIN1=SN(COS1)


C------------------------------------------      
	
      DO I =NPROC_MIN,NPROC_MAX,NPROC_STEP
        
        CALL PMAS(I,1,PM1)
        CALL PMAS(I,2,PM2)
	IF(ABS(PM1/PM).gt.SUPPEXPMAX.or.ABS(PM2/PM).gt.SUPPEXPMAX) 
     >  goto 900
        CALL PMAS(I,3,PM3)
        CALL PMAS(I,4,PM4)
	
	PN1=PINF(I,1)
        PN2=PINF(I,2)
	SFACT=1.
	CFACT=1.

	IF(PN1.ne.PN2) SFACT=2.

	IF(PN1.ne.'~o1'.and.PN1.ne.'~o2'.and.PN1.ne.'~g')  CFACT=2.
	IF(PN2.ne.'~o1'.and.PN2.ne.'~o2'.and.PN2.ne.'~g')  CFACT=2.
	
	IF(PN1.eq.'~1+'.and.PN2.eq.'~1-')  CFACT=1.
	
	IF(PN1.eq.'~e1'.and.PN2.eq.'~E1')  CFACT=1.
	IF(PN1.eq.'~e2'.and.PN2.eq.'~E2')  CFACT=1.
	IF(PN1.eq.'~e3'.and.PN2.eq.'~E3')  CFACT=1.
	IF(PN1.eq.'~n1'.and.PN2.eq.'~N1')  CFACT=1.
	IF(PN1.eq.'~n2'.and.PN2.eq.'~N2')  CFACT=1.
	IF(PN1.eq.'~n3'.and.PN2.eq.'~N3')  CFACT=1.
	IF(PN1.eq.'~u1'.and.PN2.eq.'~U1')  CFACT=1.
	IF(PN1.eq.'~d1'.and.PN2.eq.'~D1')  CFACT=1.
	IF(PN1.eq.'~c1'.and.PN2.eq.'~C1')  CFACT=1.
	IF(PN1.eq.'~s1'.and.PN2.eq.'~S1')  CFACT=1.
	IF(PN1.eq.'~t1'.and.PN2.eq.'~T1')  CFACT=1.
	IF(PN1.eq.'~b1'.and.PN2.eq.'~B1')  CFACT=1.
	
	TFACT=SFACT*CFACT
	
     
      
	
	pm1=abs(pm1)
	pm2=abs(pm2)
	pm3=abs(pm3)
	pm4=abs(pm4)
	
      
	
	B1=pm1/pm
	B2=pm2/pm
        
	
     
        IF(NDIMUSER.eq.1) then
          FA=1.
          AS=AS_MAX
          DJACOB=2.
        ELSEIF(NDIMUSER.ge.2) then
	
          AS=AS_MIN+(AS_MAX-AS_MIN)*X(2)
          DJACOB=(AS_MAX-AS_MIN)*2.
	  IF(AS.le.b1+b2) goto 900
	  
	  IF(NDIMUSER.eq.2) THEN
	      FA=FA12(AS,I,XF)
	      
	    
	  ELSEIF(NDIMUSER.eq.3.and.NDIMUSER_EFF.eq.2) THEN
	
   	      FA=FA12_integr(AS,b1,b2,XF)*
     &        ((AS**2-b1**2-b2**2)**2-4.*b1**2*b2**2)
     &        /sqrt(AS)/(2.*sqrt(2.*pi)*b1**(3./2.)*b2**(3./2.))

         
          ELSEIF(NDIMUSER.eq.3.and.NDIMUSER_EFF.eq.3) THEN
	      XFA=X(3)
	      FA=FA12(AS,I,XFA)
	      
          ENDIF
      
        ENDIF
	
         
        ECM=AS*PM
        IF(ECM.GT.max(PM1+PM2,PM3+PM4)) then
	
	
          E1=(ECM+(PM1-PM2)*(PM1+PM2)/ECM)/2D0
          E2=(ECM+(PM2-PM1)*(PM1+PM2)/ECM)/2D0
          R1=RR(E1,PM1)
          R2=R1
 	
          P(0,1)=E1
          P(1,1)=0.
          P(2,1)=0.
          P(3,1)=-R1
          P(0,2)=E2
          P(1,2)=0.
          P(2,2)=0.
          P(3,2)=R1
	
          E3=(ECM+(PM3-PM4)*(PM3+PM4)/ECM)/2D0
          R3=RR(E3,PM3)
          E4=(ECM+(PM4-PM3)*(PM3+PM4)/ECM)/2D0
          R4=R3

          P(0,3)=E3
          P(1,3)=R3*SIN1
          P(2,3)=0.
          P(3,3)=R3*COS1
         
	  P(0,4)=E4
          P(1,4)=-P(1,3)
          P(2,4)=0.
          P(3,4)=-P(3,3)
           
       	 
	  F_PS=R3/R1/(32d0*Pi*ECM**2)
	  FUNC_TMP =TFACT*SQME(I,P)*F_PS*FA*DJACOB*GEVTOPBN
          ds_da_dc=FUNC_TMP+ds_da_dc
	  IF(I.eq.26.and.NDIMUSER.eq.3.and.NDIMUSER_EFF.eq.2) goto 999
	  
	  
        ENDIF
  900   CONTINUE     

      ENDDO
  999 FUNC=ds_da_dc
       

      IF(FUNC.le.1D-16) FUNC=1D-16
 
      return
      end
      
C-------------------------------------------------------
C#######################################################
C-------------------------------------------------------

      FUNCTION SQRLAM(x,y,z) 
      IMPLICIT  NONE
      REAL*8     SQRLAM,x,y,z  
      SQRLAM=sqrt(max(1.E-06,
     &       (x+(y+z))*(x-(y+z))*(x+(y-z))*(x-(y-z))))
      return
      END

C-------------------------------------------------------
C#######################################################
C-------------------------------------------------------
C-------------------------------------------------------
      Function BesK1(z)
C-----------------------------------------------------------------------
C     The modified Bessel function of the second kind K(n, z)
C     for large arguments z.
C     These functions have a precision better then 1(.1) % for z > 1.5(3).
C     For more details see my notes on thermal average. CsB 12/2001
C-----------------------------------------------------------------------
      Implicit none
      Real*8 BesK1,BesK2,z,Pi
      Integer n
      Data Pi / 3.14159265358979323846D0 /

      BesK1 = (Sqrt(Pi/2.)*(1./z)**3.5*
     -    (105. - 120.*z + 384.*z**2 + 1024.*z**3))/(1024.)

      ENTRY BesK2(z)
      BesK2 = (Sqrt(Pi/2.)*(1./z)**3.5*
     -    (-315. + 840.*z + 1920.*z**2 + 1024.*z**3))/(1024.)

      If (z.LT.1.5) Print*, '!!! WARNING: Precision < 1% in BesK !!!'
      If (z.LT.3.) Print*,  '!!! WARNING: Precision < .1% in BesK !!!'
     
      Return
      End
cccccccccccccccccccccccc===================================cccccccccccc

      Function FA12(as,I,x)
      IMPLICIT NONE
      REAL*8 FA12
      
      REAL*8 A
      COMMON/VARS/A(1800)

      COMMON /CTRL/
     &AS_MAX,XFINI,XF,COS_MIN,COS_MAX,CS_MIN,SUPPEXP, SUPPEXPMAX,
     &NDIMUSER,NDIMUSER_EFF,ISUM,NST_MAX,
     &NPROC_MIN,NPROC_MAX,NPROC_STEP

      REAL*8 AS_MAX,XFINI,XF,COS_MIN,COS_MAX,CS_MIN,SUPPEXP, SUPPEXPMAX
      INTEGER NDIMUSER,NDIMUSER_EFF,ISUM,NST_MAX,
     &NPROC_MIN,NPROC_MAX,NPROC_STEP
 
      REAL*8 PI,BesK1,BesK2,bj,bi,sqrlam,as,x,gj,gi,BF
      INTEGER ip1,ip2,JSUM,I

      REAL*8 b,g,pm
      CHARACTER*6 names
      INTEGER J,idof,IPTOT
      COMMON /INPART/ b(29),g(29),pm(29),idof(29),IPTOT
      COMMON /NAMES/ names(29)
      
      CHARACTER*6 PINF1,PINF2,PINF
     
     
      Data Pi / 3.14159265358979323846D0 /

      DO J=1,IPTOT
        IF(PINF(I,1).eq.NAMES(J)) IP1=J
        IF(PINF(I,2).eq.NAMES(J)) IP2=J
        b(J)=PM(J)/PM(1)
 	g(J)=IDOF(J)
      ENDDO
     
  
        BF=0d0
      
       JSUM=IPTOT
       if(NPROC_MIN.ge.1.and.NPROC_MAX.le.26.and.ISUM.eq.0) JSUM=1   
       if(NDIMUSER_EFF.eq.2) JSUM=1
       DO J=1,JSUM
 	BF=BF+b(J)**2*g(J)*BesK2(b(J)/x)*Exp((-b(J)+b(1))/x)
      ENDDO
      
C Changed from /Exp((-2d0*b(1)+as)/x) to avoid floating overflow. FEP

        FA12= 
     _ sqrlam(as,b(ip1),b(ip2))**2*
     _ g(ip1)*g(ip2)*BesK1(as/x) /4d0*Exp(-(-2d0*b(1)+as)/x)/(x*(BF)**2)

       End   

C-------------------------------------------------------
C#######################################################
C-------------------------------------------------------
      Function FA12_integr(a,b1,b2,xF)
C-----------------------------------------------------------------------
C     Function to calculate F_{12} for the velocity averaged
C     co-annihilation cross section.
C       x  = T/m is already integrated over analytically.
C       a  = Sqrt(s)/m.
C       bi = m_i/m.
C       xF is the upper limit of the x-integral.
C       m is a convenient scale (can be choosen e.g. as (m_1+m_2)/2).
C-----------------------------------------------------------------------
      Implicit none
      Real*8 FA12_integr,a,b1,b2,xF, a12,A1, Pi, dErfc
      Data Pi / 3.14159265358979323846D0 /

      a12 = a - b1 - b2
      A1  = dErfc(Sqrt(a12/xF))

      FA12_integr = A1*Sqrt(Pi/a12) + 
     -  2.*(3./(8.*a) - 15./(8.*b1) - 15./(8.*b2))*
     -   (-A1*Sqrt(a12*Pi) + Sqrt(xF)/Exp(a12/xF)) + 
     -  (2.*(-15./(128.*a**2) + 345./(128.*b1**2) - 45./(64.*a*b1) + 
     -       345./(128.*b2**2) - 45./(64.*a*b2) + 225./(64.*b1*b2))*
     -     (2.*A1*a12**1.5*Sqrt(Pi) + 
     -       (-2.*a12*Sqrt(xF) + xF**1.5)/Exp(a12/xF)))/3.
     

      Return
      End   
C-------------------------------------------------------
C#######################################################
C-------------------------------------------------------

c      SUBROUTINE SPEVNT
c      RETURN
c      END

C-------------------------------------------------------
C#######################################################
C-------------------------------------------------------
      
      SUBROUTINE CPUTIM(ISECON)
      IMPLICIT NONE
      INTEGER ISECON
      REAL*4    SEC
      ISECON=0
#ifdef CERN_X
      CALL TIMEX( SEC)
#endif
      ISECON = SEC 
      RETURN
      END


C-------------------------------------------------------
C#######################################################
C-------------------------------------------------------

      FUNCTION FUNC_INT(IPRINT)
      IMPLICIT NONE
C------- BASES COMMON BLOCKS ---------------------
      
      EXTERNAL FUNC
      REAL*8  FUNC_INT,FUNC
      
      INTEGER NDIM,IG,MXDIM,NWILD,NCALL
      REAL*8 XL,XU
      PARAMETER (MXDIM = 50 )                                           
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,IG(MXDIM),NCALL    
c      DATA NWILD/2/,NCALL/1000/      
      INTEGER INTV, IPNT, NLOOP, MLOOP,ITMX1,ITMX2
      REAL*8 ACC1,ACC2
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2                             
c      DATA  ITMX1/5/,ITMX2/5/,ACC1/0.1d0/,ACC2/0.1d0/
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP      
c---------------------------------------------------
      REAL*8 ESTIM, SIGMA, CTIME

     
      COMMON /CTRL/
     &AS_MAX,XFINI,XF,COS_MIN,COS_MAX,CS_MIN,SUPPEXP, SUPPEXPMAX,
     &NDIMUSER,NDIMUSER_EFF,ISUM,NST_MAX,
     &NPROC_MIN,NPROC_MAX,NPROC_STEP

      REAL*8 AS_MAX,XFINI,XF,COS_MIN,COS_MAX,CS_MIN,SUPPEXP, SUPPEXPMAX
      INTEGER NDIMUSER,NDIMUSER_EFF,ISUM,NST_MAX,
     &NPROC_MIN,NPROC_MAX,NPROC_STEP
     
      INTEGER I,IPRINT,IT1,IT2
      
      CALL BSINIT 
      
      ITMX1=5
      ITMX2=5
      NDIM =NDIMUSER_EFF
      NWILD=NDIM
      ACC1=1.
      ACC2=1.
      
      DO 1 I=1,NDIMUSER_EFF
        XL(I)=0d0
        XU(I)=1d0
    1 CONTINUE
      XL(3)=0.000001
      XU(3)=XF
   
c-------------------------------------------------

      INTV=IPRINT
      
c       print *,'NCALLS=', NCALL,IPRINT,INTV,NPROC_MIN,NPROC_MAX
      
c      CALL BSINIT                                                  
      CALL BASES( FUNC, ESTIM, SIGMA, CTIME, IT1, IT2 )             
      
      FUNC_INT=ESTIM
      
     
      
      RETURN
      END
C-------------------------------------------------------
C#######################################################
c ==========================================================================
C   
C   ISAJET - CompHEP interface
C
      SUBROUTINE ISACHP  
C-----------------------------------------------------------------------
C          A primitive interface between ISAJET and CompHEP generated code.
C          Fills the CompHEP common blocks from ISAJET common blocks.
C-----------------------------------------------------------------------
      IMPLICIT NONE

#include "wcon.inc"
#include "sssm.inc"
#include "sspar.inc"
#include "sugmg.inc"
#include "sugpas.inc"
C          SM ident code definitions. These are standard ISAJET but
C          can be changed.
      INTEGER IDUP,IDDN,IDST,IDCH,IDBT,IDTP
      INTEGER IDNE,IDE,IDNM,IDMU,IDNT,IDTAU
      INTEGER IDGL,IDGM,IDW,IDZ,IDH
      PARAMETER (IDUP=1,IDDN=2,IDST=3,IDCH=4,IDBT=5,IDTP=6)
      PARAMETER (IDNE=11,IDE=12,IDNM=13,IDMU=14,IDNT=15,IDTAU=16)
      PARAMETER (IDGL=9,IDGM=10,IDW=80,IDZ=90,IDH=81)
C          SUSY ident code definitions. They are chosen to be similar
C          to those in versions < 6.50 but may be changed.
      INTEGER ISUPL,ISDNL,ISSTL,ISCHL,ISBT1,ISTP1
      INTEGER ISNEL,ISEL,ISNML,ISMUL,ISNTL,ISTAU1
      INTEGER ISUPR,ISDNR,ISSTR,ISCHR,ISBT2,ISTP2
      INTEGER ISNER,ISER,ISNMR,ISMUR,ISNTR,ISTAU2
      INTEGER ISZ1,ISZ2,ISZ3,ISZ4,ISW1,ISW2,ISGL
      INTEGER ISHL,ISHH,ISHA,ISHC
      INTEGER ISGRAV
      INTEGER IDTAUL,IDTAUR
      PARAMETER (ISUPL=21,ISDNL=22,ISSTL=23,ISCHL=24,ISBT1=25,ISTP1=26)
      PARAMETER (ISNEL=31,ISEL=32,ISNML=33,ISMUL=34,ISNTL=35,ISTAU1=36)
      PARAMETER (ISUPR=41,ISDNR=42,ISSTR=43,ISCHR=44,ISBT2=45,ISTP2=46)
      PARAMETER (ISNER=51,ISER=52,ISNMR=53,ISMUR=54,ISNTR=55,ISTAU2=56)
      PARAMETER (ISGL=29)
      PARAMETER (ISZ1=30,ISZ2=40,ISZ3=50,ISZ4=60,ISW1=39,ISW2=49)
      PARAMETER (ISHL=82,ISHH=83,ISHA=84,ISHC=86)
      PARAMETER (ISGRAV=91)
      PARAMETER (IDTAUL=10016,IDTAUR=20016)

      INTEGER NOUT
      PARAMETER (NOUT=33)
      INTEGER IDOUT(NOUT)
      REAL AMASS,AMPL
      REAL AMI,SUMGAM,SUMMJ,WIDMX
      REAL QSUSY,ASMB,MBMB,ASMT,MTMT,SUALFS,GG
      DOUBLE PRECISION SSMQCD
      INTEGER I,J,K,IFL1,IFL2,IFL3,JSPIN,INDEX,IITEST
C
      DATA IDOUT/
     $IDTP,ISGL,ISUPL,ISDNL,ISSTL,ISCHL,ISBT1,ISTP1,ISUPR,ISDNR,
     $ISSTR,ISCHR,ISBT2,ISTP2,ISEL,ISMUL,ISTAU1,ISNEL,ISNML,ISNTL,
     $ISER,ISMUR,ISTAU2,ISZ1,ISZ2,ISZ3,ISZ4,ISW1,ISW2,
     $ISHL,ISHH,ISHA,ISHC/
      
      REAL FTAMZ1,FBOMZ1,FTOMZ1,VUMZ1,VDMZ1       
      

************************
c   CompHEP common blocks
************************
      Real*8 A
      COMMON/VARS/A(1800)
      CHARACTER*6 xxxxx(146)
      DATA xxxxx/
     >'EE',	'SW',  	's12',	's23',	's13',           !1
     >'MZ',	'MH3',	'Mm',	'Mt',	'Mc',		 !2 
     >'Ms',	'Mtop',	'Mb',	'wtop',	'wZ',		 !3 
     >'wW',	'sa',	'MU',	'Zp11',	'Zp12', 	 !4 
     >'Zp21',	'Zp22',	'Zm11',	'Zm12',	'Zm21', 	 !5 
     >'Zm22',	'Zn11',	'Zn12',	'Zn13',	'Zn14', 	 !6 
     >'Zn21',	'Zn22',	'Zn23',	'Zn24',	'Zn31', 	 !7 
     >'Zn32',	'Zn33',	'Zn34',	'Zn41',	'Zn42', 	 !8 
     >'Zn43',	'Zn44',	'Atau',	'Zl33',	'Zl36', 	 !9 
     >'Zl63',	'Zl66',	'Zl11',	'Zl14',	'Zl41', 	 !10 
     >'Zl44',	'Zl22',	'Zl25',	'Zl52',	'Zl55', 	 !11 
     >'Atop',	'Abot',	'Zu33',	'Zu36',	'Zu63', 	 !12 
     >'Zu66',	'Zu11',	'Zu14',	'Zu41',	'Zu44', 	 !13 
     >'Zu22',	'Zu25',	'Zu52',	'Zu55',	'Zd33', 	 !14 
     >'Zd36',	'Zd63',	'Zd66',	'Zd11',	'Zd14', 	 !15 
     >'Zd41',	'Zd44',	'Zd22',	'Zd25',	'Zd52', 	 !16 
     >'Zd55',	'Mh',	'wh',	'MHH',	'wHh',		 !17 
     >'wH3',	'MHc',	'wHc',	'MC1',	'wC1',		 !18 
     >'MC2',	'wC2',	'MNE1',	'wNE1',	'MNE2', 	 !19 
     >'wNE2',	'MNE3',	'wNE3',	'MNE4',	'wNE4', 	 !20 
     >'wSG',	'wSe1',	'wSe2',	'wSmu1','wSmu2',	 !21 
     >'MStau1',	'wStau1','MStau2','wStau2','wSne',	 !22 
     >'wSnmu',	'MSntau','wSntau','wSu1','wSu2',	 !23 
     >'wSd1',	'wSd2',	'wSc1',	'wSc2',	'wSs1', 	 !24 
     >'wSs2',	'MStop1','wStop1','MStop2','wStop2'	 !25 
     >,'MSbot1','wSbot1','MSbot2','wSbot2','tb',	 !26
     >'MSG',	'MSe1',	'MSe2',	'MSmu1','MSmu2',	 !27
     >'MSne',	'MSnmu','MSu1',	'MSu2',	'MSd1', 	 !28 
     >'MSd2',	'MSc1',	'MSc2',	'MSs1',	'MSs2', 	 !29 
     >'GG'/						 !30 
************************
c      Bases common blocks
************************
      INTEGER NPRINT
      common/printlevel/NPRINT
******************
c   Local variables
******************
      INTEGER ii
      
      
      REAL TANB,MT,PI,GPX,THX,THY,THETAX,THETAY,GAMTOT
      PI=4.*ATAN(1.)

CsB   sin of the Weinberg angle
      GPX=SQRT(.6)*GSS(1)
      IF((GSS(2)**2+GPX**2).eq.0) THEN 
        SIN2W=SN2THW
      ELSE
        SIN2W=GPX**2/(GSS(2)**2+GPX**2)
      ENDIF

C   Set constants & EW parameters
C   This *must* be done after defining SIN2W above
      CALL SETCON
      CALL SETW
      
CsB   theta_x and theta_y in the chargino mixing matrices
      CALL THETAXY(THX,THY)
      THETAX = THX
      THETAY = THY

C     CompHEP = ISAJET              ! name
      A(1) = SQRT(4.D0*PI/128.D0)   ! SQRT(4*Pi/128)
      A(2) = SQRT(SIN2W)            ! sin of the Weinberg angle
      A(3) = 0.221                  ! CKM matrix element (= 0.221 PDG'94)
      A(4) = 0.040                  ! CKM matrix element (= 0.040 PDG'94)
      A(5) = .0035                  ! CKM matrix element (= .0035 PDG'94)
     
      A(6) = AMZ                    ! Z boson mass
      A(7) = MSS(31)                ! mass of CP-odd Higgs
      A(8) = AMMU                   ! muon mass
C---------------------------------------------------C
      A(10) = AMCH                  ! charm mass
    
      A(11) = AMST                  ! strange mass

      
      A(9)  = FL2Z1*VDQ		! tau mass
      A(12) = FT2Z1*VUQ		! top mass
      A(13) = FB2Z1*VDQ		! bottom mass

c      A(9)  = MLQ             ! tau mass
c      A(12) = MTQ             ! top mass
c      A(13) = MBQ            ! bottom mas
     


      if(nprint.eq.3) then
      
 	if(nprint.ge.3) print*,'MB=',FB2Z1*VDQ
	if(nprint.ge.3) print*,'MT=',FT2Z1*VUQ
	if(nprint.ge.3) print*,'ML=',FL2Z1*VDQ
      ENDIF
        
c      print *,'MTAZMZ1,MBMZ1,MTOMZ1',
c     $      FTAMZ1*VDMZ1, FBOMZ1*VDMZ1, FTOMZ1*VUMZ1
c      print *,'FTAZMZ1,FBOZ1,FTOMZ1',FTAMZ1,FTOMZ1,FBOMZ1
c      print *,'VDMZ1,VUMZ1',VDMZ1,VUMZ1
      
c      print *,'MLQ,MBQ,MTQ',MLQ,MBQ,MTQ
       
     
ccc      FT=MTQ/VUQ
ccc      FB=MBQ/VDQ
ccc      FL=MLQ/VDQ


C---------------------------------------------------C
       A(14) = GAMTOT(IDTP)          ! top width
       A(15) = GAMZ                  ! Z boson width

       A(16) = GAMW                   ! W boson width
       A(17) = SIN(ALFAH)             ! sin(alpha) Higgs mixing angle
       A(18) = MU                     ! mu parameter

       A(19) = -SIN(GAMMAR)           ! chargino mixing angle
       A(20) =  COS(GAMMAR)           ! chargino mixing angle
       A(21) = -THETAY*COS(GAMMAR)    ! chargino mixing angle
       A(22) = -THETAY*SIN(GAMMAR)    ! chargino mixing angle

       A(23) = -SIN(GAMMAL)           ! chargino mixing angle
       A(24) = -THETAX*COS(GAMMAL)    ! chargino mixing angle
       A(25) = -COS(GAMMAL)           ! chargino mixing angle
       A(26) =  THETAX*SIN(GAMMAL)    ! chargino mixing angle

       A(27) =   ZMIXSS(4,1)          ! neutralino mixing angle 1,1
       A(28) =   ZMIXSS(4,2)          ! neutralino mixing angle 1,2
       A(29) =   ZMIXSS(4,3)	     ! neutralino mixing angle 1,3
       A(30) =   ZMIXSS(4,4)	     ! neutralino mixing angle 1,4

       A(31) =   ZMIXSS(3,1)          ! neutralino mixing angle
       A(32) =   ZMIXSS(3,2)          ! neutralino mixing angle
       A(33) =   ZMIXSS(3,3)	     ! neutralino mixing angle
       A(34) =   ZMIXSS(3,4)	     ! neutralino mixing angle

       A(35) =  -ZMIXSS(2,1)	    ! neutralino mixing angle
       A(36) =  -ZMIXSS(2,2)	    ! neutralino mixing angle
       A(37) =  -ZMIXSS(2,3)	  ! neutralino mixing angle
       A(38) =  -ZMIXSS(2,4)	  ! neutralino mixing angle

       A(39) =  -ZMIXSS(1,1)	  ! neutralino mixing angle
       A(40) =  -ZMIXSS(1,2)	  ! neutralino mixing angle
       A(41) =  -ZMIXSS(1,3)	  ! neutralino mixing angle
       A(42) =  -ZMIXSS(1,4)	  ! neutralino mixing angle

       A(43) = GSS(10)               ! Tau soft coupling

       A(44) =  COS(THETAL)          ! stau mixing angle Zl33
       A(45) =  -SIN(THETAL)          ! stau mixing angle Zl36
       A(46) =   SIN(THETAL)          ! stau mixing angle Zl63
       A(47) =  COS(THETAL)          ! stau mixing angle Zl66

       A(56) = GSS(12)               ! top tril soft coupling
       A(57) = GSS(11)               ! bottom tril  soft coupling

       A(58) =  COS(THETAT)           ! squark mixing angle
       A(59) = -SIN(THETAT)          ! squark mixing angle
       A(60) =  SIN(THETAT)          ! squark mixing angle
       A(61) =  COS(THETAT)           ! squark mixing angle

       A(70) =  COS(THETAB)           ! squark mixing angle
       A(71) =  -SIN(THETAB)           ! squark mixing angle
       A(72) =   SIN(THETAB)          ! squark mixing angle
       A(73) =  COS(THETAB)           ! squark mixing angle

       A(82) = MSS(29)               ! Mh
       A(83) = GAMTOT(ISHL)          ! width of light Higgs
       A(84) = MSS(30)               ! MHh
       A(85) = GAMTOT(ISHH)          ! width of heavy higgs
       A(86) = GAMTOT(ISHA)          ! width of CP-odd Higgs
       A(87) = MSS(32)               ! MHC
       A(88) = GAMTOT(ISHC)          ! Width of charged Higgs

       A(89) = ABS(MSS(27))          ! MC1
       A(90) = GAMTOT(ISW1)          ! width of chargino 1
       A(91) = ABS(MSS(28))          ! mass of chargino 2
       A(92) = GAMTOT(ISW2)          ! width of chargino 2
       A(93) = -MSS(23)              ! MNE1

       A(94) = GAMTOT(ISZ1)          ! width of neutralino 1
       A(94) = abs(A(93))/100.

       A(95) = -MSS(24)              ! MNe2
       A(96) = GAMTOT(ISZ2)          ! width of neutralino 2
       A(97) = -MSS(25)              ! mass of neutralino 3
       A(98) = GAMTOT(ISZ3)          ! width of neutralino 3
       A(99) = -MSS(26)              ! mass of neutralino 4
       A(100) = GAMTOT(ISZ4)         ! width of neutralino 4
       A(101) = GAMTOT(ISGL)         ! gluino width

       A(102) = GAMTOT(ISER)         ! 1st selectron width (ISAJET: e_r)
       A(103) = GAMTOT(ISEL)         ! 2nd selectron width (ISAJET: e_l)

       A(104) = GAMTOT(ISMUR)        ! 1st smuon width (ISAJET: mu_r)
       A(105) = GAMTOT(ISMUL)        ! 2nd smuon width (ISAJET: mu_l)

       A(106) = MSS(21)              ! MSTAU1
       A(107) = GAMTOT(ISTAU1)       ! 1st stau width
       A(108) = MSS(22)              ! MSTAU2
       A(109) = GAMTOT(ISTAU2)       ! 2nd stau width
       A(110) = GAMTOT(ISNEL)        ! e-sneutrino width
       A(111) = GAMTOT(ISNML)        ! mu-sneutrino width
       A(112) = MSS(16)              ! tau-sneutrino mass
       A(113) = GAMTOT(ISNTL)        ! tau-sneutrino width

       A(114) = GAMTOT(ISUPL)        ! width of u-squark 1 (ISAJET: u_l)
       A(115) = GAMTOT(ISUPR)        ! width of u-squark 2 (ISAJET: u_r)

       A(116) = GAMTOT(ISDNL)        ! width of d-squark 1 (ISAJET: d_l)
       A(117) = GAMTOT(ISDNR)        ! width of d-squark 2 (ISAJET: d_r)
       A(118) = GAMTOT(ISCHL)        ! width of c-squark 1 (ISAJET: c_l)
       A(119) = GAMTOT(ISCHR)        ! width of c-squark 1 (ISAJET: c_r)
       A(120) = GAMTOT(ISSTL)        ! width of s-squark 1 (ISAJET: s_l)
       A(121) = GAMTOT(ISSTR)        ! width of s-squark 1 (ISAJET: s_r)

       A(122) = MSS(12)              ! MST1
       A(123) = GAMTOT(ISTP1)        ! width of t-squark 1
       A(124) = MSS(13)              ! mass of t-squark 2
       A(125) = GAMTOT(ISTP2)        ! width of t-squark 2
       A(126) = MSS(10)              ! MSB1
       A(127) = GAMTOT(ISBT1)        ! width of b-squark 1
       A(128) = MSS(11)              ! mass of b-squark 2
       A(129) = GAMTOT(ISBT2)        ! width of b-squark 2
       A(130) = XTANB                ! tan beta
       A(131) = MSS(1)               ! gluino mass

       IF(MSS(17).lt.MSS(18)) THEN
         A(132) = MSS(17)              ! MSE1
         A(133) = MSS(18)              ! MSE2
         A(48) =  1
         A(49) =  0
         A(50) =  0
         A(51) =  1
       ELSE
       A(103) = GAMTOT(ISER)         ! 1st selectron width (ISAJET: e_r)
       A(104) = GAMTOT(ISEL)         ! 2nd selectron width (ISAJET: e_l)
         A(132) = MSS(18)
         A(133) = MSS(17)
         A(48) =  0                     ! slepton mixing angle
         A(49) =  1                     ! slepton mixing angle
         A(50) =  -1                     ! slepton mixing angle
         A(51) =  0                     ! slepton mixing angle
       ENDIF

       IF(MSS(19).lt.MSS(20)) THEN
         A(134) = MSS(19)              ! MSMU1
         A(135) = MSS(20)              ! MSMU2
         A(52) =  1                     ! slepton mixing angle
         A(53) =  0                     ! slepton mixing angle
         A(54) =  0                     ! slepton mixing angle
         A(55) =  1                    ! slepton mixing angle
        ELSE
       A(105) = GAMTOT(ISMUR)        ! 1st smuon width (ISAJET: mu_r)
       A(104) = GAMTOT(ISMUL)        ! 2nd smuon width (ISAJET: mu_l)
         A(134) = MSS(20)
         A(135) = MSS(19)
         A(52) =  0                     ! slepton mixing angle
         A(53) =  1                     ! slepton mixing angle
         A(54) =   -1                     ! slepton mixing angle
         A(55) =  0                    ! slepton mixing angle
       ENDIF

       A(136) = MSS(14)              ! e-sneutrino mass
       A(137) = MSS(15)              ! mu-sneutrino mass

       A(62) =  1                     ! squark mixing angle
       A(63) =  0                     ! squark mixing angle
       A(64) =  0                     ! squark mixing angle
       A(65) =  1                     ! squark mixing angle
       A(138) = MSS(2)               ! mass of u-squark 1 (ISAJET: u_l)
       A(139) = MSS(3)               ! mass of u-squark 2 (ISAJET: u_r)
       IF(MSS(2).gt.MSS(3)) then
        A(62) =  0                     ! squark mixing angle
        A(63) =  1                     ! squark mixing angle
        A(64) =  -1                     ! squark mixing angle
        A(65) =  0                     ! squark mixing angle
        A(138) = MSS(3)               ! mass of u-squark 1 (ISAJET: u_l)
        A(139) = MSS(2)               ! mass of u-squark 2 (ISAJET: u_r)
       A(115) = GAMTOT(ISUPL)        ! width of u-squark 1 (ISAJET: u_l)
       A(114) = GAMTOT(ISUPR)        ! width of u-squark 2 (ISAJET: u_r)
       ENDIF

       A(66) =  1                     ! squark mixing angle
       A(67) =  0                     ! squark mixing angle
       A(68) =  0                     ! squark mixing angle
       A(69) =  1                     ! squark mixing angle
       A(142) = MSS(8)               ! mass of c-squark 1 (ISAJET: c_l)
       A(143) = MSS(9)               ! mass of c-squark 1 (ISAJET: c_r)
       IF(MSS(8).gt.MSS(9)) then
       A(66) =  0                     ! squark mixing angle
       A(67) =  1                     ! squark mixing angle
       A(68) =  -1                     ! squark mixing angle
       A(69) =  0                     ! squark mixing angle
       A(142) = MSS(9)               ! mass of c-squark 1 (ISAJET: c_l)
       A(143) = MSS(8)               ! mass of c-squark 1 (ISAJET: c_r)
       A(119) = GAMTOT(ISCHL)        ! width of c-squark 1 (ISAJET: c_l)
       A(118) = GAMTOT(ISCHR)        ! width of c-squark 1 (ISAJET: c_r)
       ENDIF

       A(74) = 1                     ! squark mixing angle
       A(75) = 0                     ! squark mixing angle
       A(76) = 0                     ! squark mixing angle
       A(77) = 1                     ! squark mixing angle
       A(140) = MSS(4)               ! mass of d-squark 1 (ISAJET: d_l)
       A(141) = MSS(5)               ! mass of d-squark 2 (ISAJET: d_r)
       IF(MSS(4).gt.MSS(5)) THEN
       A(74) = 0                     ! squark mixing angle
       A(75) = 1                     ! squark mixing angle
       A(76) = -1                     ! squark mixing angle
       A(77) = 0                     ! squark mixing angle
       A(140) = MSS(5)               ! mass of d-squark 1 (ISAJET: d_l)
       A(141) = MSS(4)               ! mass of d-squark 2 (ISAJET: d_r)
       A(117) = GAMTOT(ISDNL)        ! width of d-squark 1 (ISAJET: d_l)
       A(116) = GAMTOT(ISDNR)        ! width of d-squark 2 (ISAJET: d_r)
       ENDIF

       A(78) = 1                     ! squark mixing angle
       A(79) = 0                     ! squark mixing angle
       A(80) = 0                     ! squark mixing angle
       A(81) = 1                     ! squark mixing angle
       A(144) = MSS(6)               ! mass of s-squark 1 (ISAJET: s_l)
       A(145) = MSS(7)               ! mass of s-squark 1 (ISAJET: s_r)
       IF(MSS(6).gt.MSS(7)) then
       A(78) = 0                     ! squark mixing angle
       A(79) = 1                     ! squark mixing angle
       A(80) = -1                     ! squark mixing angle
       A(81) = 0                     ! squark mixing angle
       A(144) = MSS(7)               ! mass of s-squark 1 (ISAJET: s_l)
       A(145) = MSS(6)               ! mass of s-squark 1 (ISAJET: s_r)
       A(121) = GAMTOT(ISSTL)        ! width of s-squark 1 (ISAJET: s_l)
       A(120) = GAMTOT(ISSTR)        ! width of s-squark 1 (ISAJET: s_r)
       ENDIF
       A(146) = GSS(3)               ! g_s(M_Z) in CompHEP, g_s(Q) in ISAJET
      IF(NPRINT.ge.2) then
      
        Do ii=1,144,3
	  Print '(3(I6,E16.3))', 
     &     ii,A(ii),ii+1,A(ii+1),ii+2,A(ii+2)
        End do
        Do ii=145,146,2   
	  Print '(2(I6,E16.3))', 
     &     ii,A(ii),ii+1,A(ii+1)
        End do
	
!      Print*, GAMTOT(IDW), GAMTOT(IDZ)
!      Stop
      ENDIF

      RETURN
      END
 
      FUNCTION GAMTOT(ID)
C-----------------------------------------------------------------------
C          Calculate total width (in GeV) for ID
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
#include "ssmode.inc"
#include "sslun.inc"
C
      INTEGER ID,I
      REAL GAMTOT
C
      GAMTOT=0
      DO 100 I=1,NSSMOD
        IF(ISSMOD(I).EQ.ID) GAMTOT=GAMTOT+GSSMOD(I)
100   CONTINUE
      RETURN
      END 

      SUBROUTINE THETAXY(THX,THY)
C-----------------------------------------------------------------------
C          Calculate theta_x and theta_y in the chargino mixing matrices.
C-----------------------------------------------------------------------
      IMPLICIT NONE

#include "sssm.inc"
#include "sspar.inc"
#include "sugmg.inc"
CsB
      REAL MU2,AL,SINA,COSA,SIN2A,COS2A,ZETA,ZETAS,XM,YM,THX,THY

      REAL SR2
      SR2=SQRT(2.)

CsB   M_2(M_weak)
      MU2 = GSS(8)

      AL=ATAN(RV2V1)
      SINA=SIN(AL)
      COSA=COS(AL)
      SIN2A=SIN(2.*AL)
      COS2A=COS(2.*AL)
      ZETAS=(TWOM1**2-MU2**2)**2
     $+4*AMW**2*(AMW**2*COS2A**2+TWOM1**2+MU2**2+2*TWOM1*MU2*SIN2A)
      ZETA=SQRT(ZETAS)
      XM=-(TWOM1**2-MU2**2-2*AMW**2*COS2A-ZETA)
     $/(2*SR2*AMW*(MU2*SINA+TWOM1*COSA))
      YM=-(TWOM1**2-MU2**2+2*AMW**2*COS2A-ZETA)
     $/(2*SR2*AMW*(MU2*COSA+TWOM1*SINA))
      THX=SIGN(1.,XM)
      THY=SIGN(1.,YM)

      RETURN
      END
      
c####################################################################
********************************************************************
c####################################################################
      REAL*8 FUNCTION omega(FF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

*----------------------------------------------------
*OMEH2=CISLO*PLANCK*VOLNO*TEMP*(TGAMMA**3)/SJINT/CRIT

	CISLO=1.66
	VOLNO=9.

*PLANCK=0.818933748
* MP     = 2.1767E-08      kg  
* 1  eV  = 1.782661731E-36 kg
* 1 GeV  = 1.782661731E-27 kg
* MP     = 2.1767/1.782661731 *1.E+19 GeV
	PLANCK=1.782661731/2.1767    
*1.E-19 left

*TEMP=(T_x/T_gamma)^3=2./(2.+7./8.*(6.*2.+5.*3.*2.))
	TEMP=2./(2.+7./8.*(6.*2.+5.*3.*2.))

 

* k boltz= 1.3806503E-23 J/K
*        = 8.617342E-05 ev/K
*        = 8.617342E-14 GeV/K
* T_gamma^3 = (2.726*0.8617342)^3  ! 10^(-39) left
	TG3=(2.726*0.8617342)**3     
* 10^(-39) left

* ro_crit= 8.0992E-47 GeV^(4)
	CRIT = 8.0992                
* 10^47 left

*CS: GeV^(-2) =0.3893796623  *1.E+09 pb
*    1pb      =1./0.3893796623*1.E-09 GeV^2
*    1fb      =1./0.3893796623*1.E-12 GeV^2
*

	CSCONV = 3.893796623               
* 10^11 left
 
*  -19-39+47+11  =0 

        omega=10000
        IF(FF.ne.0.) then
	  omega=CISLO*PLANCK*VOLNO*TEMP*TG3/CRIT*CSCONV/1000./FF
	endif
      
      return
      
*------------------------------------------------------
      END
c####################################################################
************************************************************************
*	BASES/SPRING library                                           *      
************************************************************************
*    ====================================================              *
      SUBROUTINE BASES( FXN, S, SIGMA, CTIME, IT1, IT2 )                
*    ====================================================              *
*      Subroutine BASES for the Numerical integration.                 *
*      In terms of this program Integration can be done, furthermore   *
*      a probability distribution can be made for the event generation.*
*      The event with weight one is generated by program SPRING.       *
* ((Input))                                                            *
*    from the arguement                                                *
*      FXN    : Name of function program                               *
*    from the labeled common /BASE1/                                   *
*      XL(50) : Lower limits of the integration variabels              *
*      XU(50) : upper limits of the integration variabels              *
*      NDIM   : Dimension of the integration                           *
*      NCALL  : Number of sampling points per iteration                *
*    from the lebeled common /BASE2/                                   *
*      ITMX*  : Number of iteration                                    *
*      ACC*   : Required accuracies                                    *
* ((Output))                                                           *
*      S      : Estimate of the integral                               *
*      SIGMA  : Standard deviation of the estimate                     *
*      CTIME  : Computing time required for integration                *
*      IT1    : Number of iterations for the grid defining step        *
*      IT2    : Number of iterations for the integration step          *
C*                                                                     *
C*       Coded by S.Kawabata         April '94                         *
C*                                                                     *
C***********************************************************************
C                                                                       
C                                                                       
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      EXTERNAL FXN                                                      
      PARAMETER (MXDIM = 50)                                            
*                                                                       
*     JFLAG =  0 : First trial for defining grids.                      
*     JFLAG =  1 : First trial for event accumulation.                  
*     JFLAG =  2 : Second or more trial for defining grids.             
*     JFLAG =  3 : Second or more trial for accumulation.               
*                                                                      *
      COMMON /BASE0/ JFLAG,IBASES                                       
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                    
     .               IG(MXDIM),NCALL                                    
      COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2                              
      REAL*4 STIME                                                      
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF                        
      CHARACTER*80 ERROR                                                
      COMMON /BWARN1/ NERROR                                            
      COMMON /BWARN2/ ERROR(3,3)                                        
*        INTV = ( 0 / 1 / any ) = ( Batch / Batch(Unix) / Interactive ) 
*        IPNT = ( 0 / any ) = ( IBM Type / Ascii printer )              
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
                                                                        
       COMMON/NINFO/ NODEID, NUMNOD                                     
       COMMON /BDATE/ IDATE(3),ITIME(2)                                 
*            IDATE(1) : year        ITIME(1) : hour                     
*            IDATE(2) : month       ITIME(2) : minute                   
*            IDATE(3) : day                                             
      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1      
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1                  
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)                    
                                                                        
*-------------------------------------------------                      
*     Check the parameters defined by user                              
*------------------------------------------------------                 
                                                                        
      CALL BSCHCK                                                       
                                                                        
* ---------------------------------------------------------------       
*          Initialize timer                                             
* ---------------------------------------------------------------       
                                                                        
       CALL BSDATE                                                      
                                                                        
       JFLAG  = 0                                                       
       LU     = 6                                                       
       IF( INTV .GT. 1 ) THEN                                           
           CALL BSPRNT( LU, 1, IDUM1, IDUM2 )                           
       ENDIF                                                            
                                                                        
C  -----------------------------------------------------                
C     Defining grids                                                    
C  -----------------------------------------------------                
*                                                                       
       DO 100 I = 1, NWILD                                              
          IG(I) = 1                                                     
  100  CONTINUE                                                         
                                                                        
       CALL BSETGU                                                      
                                                                        
       IF( INTV .GT. 1 ) THEN                                           
           CALL BSPRNT( LU, 4, IDUM1, IDUM2 )                           
       ENDIF                                                            
                                                                        
       CALL BSUTIM( 0, 2 )                                              
                                                                        
*     ===================                                               
       CALL BSINTG( FXN )                                               
*     ===================        For a parallel computer                
C                                      CALL BSCAST( JFLAG, 1 )          
                                                                        
*  ----------------------------------------------------                 
*     Accumulation to make probability distribution                     
*  ----------------------------------------------------                 
*     ===================                                               
       CALL BSINTG( FXN )                                               
*     ===================        For a parallel computer                
C                                      CALL BSCAST( JFLAG, 1 )          
       S     = AVGI                                                     
       SIGMA = SD                                                       
       CTIME = STIME                                                    
       IT1   = ITG                                                      
       IT2   = ITF                                                      
                                                                        
       CALL BSUTIM( 0, 2 )                                              
       TIMEB2 = RTIME                                                   
                                                                        
       IF( NERROR .GT. 0.and. INTV.ge.3 ) THEN                                         
CAB       IF( NERROR .GT. 0. ) THEN                                         
           WRITE(6,9900)                                                
 9900      FORMAT(1X,'****************************************',        
     .               '***************************************',         
     .           /1X,'* (((( Warning in the integration step ',         
     .               '))))                                   *',        
     .           /1X,'*                                      ',         
     .               '                                       *')        
           DO 990 J = 1,NERROR                                          
           DO 990 I = 1,3                                               
              WRITE(6,9901) ERROR(I,J)                                  
 9901         FORMAT(1X,A79)                                            
  990      CONTINUE                                                     
           WRITE(6,9902)                                                
 9902      FORMAT(1X,'*                                      ',         
     .               '                                       *',        
     .           /1X,'*(( Suggestion ))                      ',         
     .               '                                       *',        
     .           /1X,'* (1) Try integration again with larger ',        
     .               'number of sample points than this job.*',         
     .           /1X,'* or                                   ',         
     .               '                                       *',        
     .           /1X,'* (2) The integral variables are not sui',        
     .               'ted for the function.                 *',         
     .           /1X,'*     Take another integral variables !!',        
     .               '                                      *',         
     .           /1X,'*                                       ',        
     .               '                                      *',         
     .           /1X,'****************************************',        
     .               '***************************************')         
       ENDIF                                                            
                                                                        
       IF( INTV .GT. 1 ) THEN                                           
           CALL BSPRNT( LU, 2, IDUM1, IDUM2 )                           
       ENDIF                                                            
                                                                        
       RETURN                                                           
       END                                                              
************************************************************************
*     ==========================                                       *
       SUBROUTINE BHINIT( LUN )                                         
*     ==========================                                       *
*                                                                      *
* ((Purpose))                                                          *
*    Initialization program for  histograms and scatter plots.         *
*    This program is called by USERIN.                                 *
* ((Arguments))                                                        *
*    LUN    : logical unit number for message print                    *
* !! Caution!!                                                         *
*    When LUN is set equal to 0, the message print is suppressed.      *
* (( Common /PLOTH/ ))                                                 *
*                                                                      *
*    NW                     : Total number of words of used buffer     *
*                                                                      *
*    NHIST                  : Number of Histograms                     *
*    NSCAT                  : Number of Scat_Plots                     *
*                                                                      *
*   -----------------                                                  *
*     Hashing Table                                                    *
*   -----------------                                                  *
*                                                                      *
*     XHASH(   1,i)      : NH Number of histograms for the i-th class  *
*     XHASH(   2,i) = K  : Serial number of histograms                 *
*              :                     :                                 *
*     XHASH(NH+1,i) = K  : Serial number of histograms                 *
*                     |                                                *
*              MAPL(1,K) = ID  : Histogram ID                          *
*              MAPL(2,K) = IP1 : the 1st pointer to the K-th buffer    *
*              MAPL(3,K) = IP2 : the 2nd pointer to the K-th buffer    *
*              MAPL(4,K) = IP3 : the 3rd pointer to the K-th buffer    *
*                                                                      *
* (( Common /PLOTB/ ))                                                 *
*                                                                      *
*   --------------------                                               *
*     Histogram buffer                                                 *
*   --------------------                                               *
*                                                                      *
*    IP1  = NW + 1                                                     *
*           NW = NW + 281    : Updated NW                              *
*       BUFF( IP1 )          = Xmin                                    *
*       BUFF( IP1 + 1 )      = Xmax                                    *
*       IBUF( IP1 + 2 )      = No. of bins                             *
*       BUFF( IP1 + 3 )      = Bin width                               *
*    IP2  = IP1 + 4                                                    *
*       IBUF(   IP2       )                                            *
*          => IBUF( +  51 )  = No. of sampling points                  *
*       BUFF(   IP2 +  52)                                             *
*          => BUFF( + 103 )  = Sum of Fi for the current IT            *
*       BUFF(   IP2 + 104)                                             *
*          => BUFF( + 155 )  = Sum of Fi**2 for the current IT         *
*       BUFF(   IP2 + 156)                                             *
*          => BUFF( + 207 )  = Sum of Fi for total                     *
*       BUFF(   IP2 + 208)                                             *
*          => BUFF( + 259 )  = Sum of Fi**2 for total                  *
*    IP3  = IP1 + 264                                                  *
*       IBUF( IP3 )          = Tag for spring                          *
*       IBUF( IP3   +  1 )                                             *
*          => IBUF( + 16 )   = Title of this histogram                 *
*                                                                      *
*   --------------------                                               *
*     Scat_Plot buffer                                                 *
*   --------------------                                               *
*                                                                      *
* IP1   = NW + 1                                                       *
*         NW  = NW + 2527                                              *
*       BUFF( IP1 )          = Xmin                                    *
*       BUFF( IP1 + 1 )      = Xmax                                    *
*       IBUF( IP1 + 2 )      = No. of bins for X                       *
*       BUFF( IP1 + 3 )      = Bin width for X                         *
*       BUFF( IP1 + 4 )      = Ymin                                    *
*       BUFF( IP1 + 5 )      = Ymax                                    *
*       IBUF( IP1 + 6 )      = No. of bins for Y                       *
*       BUFF( IP1 + 7 )      = Bin width for Y                         *
* IP2   = IP1 + 8                                                      *
*       BUFF(   IP2       )  = No. of sampling points                  *
*       BUFF(   IP2 +   1 )                                            *
*          => BUFF( +2500 )  = Sum of Fi                               *
* IP3   = IP1 + 2509                                                   *
*       IBUF( IP3 )          = X-Tag for spring                        *
*       IBUF( IP3   +  1 )   = Y-Tag for spring                        *
*       IBUF( IP3   +  2 )                                             *
*          => IBUF( + 17 )   = Title of this histogram                 *
*                                                                      *
*  ((Author))                                                          *
*    S.Kawabata    June '90 at KEK                                     *
*                                                                      *
************************************************************************
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
                                                                        
      COMMON/PLOTLU/ LU                                                 
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
         LU   = LUN                                                     
                                                                        
         NW     = 0                                                     
                                                                        
         DO 50 I = 1, 13                                                
           XHASH(1,I) = 0                                               
           DHASH(1,I) = 0                                               
   50    CONTINUE                                                       
         NHIST    = 0                                                   
         NSCAT    = 0                                                   
         DO 100 I = 1, NHS                                              
           MAPL(1,I)= 0                                                 
  100    CONTINUE                                                       
         DO 200 I = 1, NSC                                              
           MAPD(1,I)= 0                                                 
  200    CONTINUE                                                       
C                                                                       
      RETURN                                                            
      END                                                               
************************************************************************
*     =========================                                        *
       SUBROUTINE BHPLOT( LU )                                          
*     =========================                                        *
* ((Purpose))                                                          *
*     Interface routine to print histograms and scatter plots.         *
*     Routines XHPLOT and DHPLOT are called to print them.             *
* ((Author))                                                           *
*     S.Kawabata  June '90  at KEK                                     *
************************************************************************
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      REAL*8 AVGI,SD,CHI2A                                              
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF                        
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                       
      IF( ITF .LE. 0 ) RETURN                                           
*    ===================                                               *
      CALL XHCHCK( LU )                                                 
*    ===================                                                
                                                                        
      IF( NHIST .LE. 0 ) THEN                                           
         WRITE(LU,9000)                                                 
 9000    FORMAT(1X,'No Histogram')                                      
      ELSE                                                              
         DO 500 J = 1, NHIST                                            
            IFBASE(J) = 1                                               
*          =====================                                        
            CALL XHPLOT(LU, 0, J )                                      
*          =====================                                        
  500    CONTINUE                                                       
      ENDIF                                                             
                                                                        
*    ===================                                                
      CALL DHPLOT( LU )                                                 
*    ===================                                                
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*    ====================                                              *
      SUBROUTINE BHRSET                                                 
*    ====================                                              *
* ((Purpose))                                                          *
*     To reset contents of histograms and scatter plots.               *
* ((Author))                                                           *
*     S.Kawabata  June '90 at KEK                                      *
************************************************************************
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
*-------------------------- Clear Histograms --------------------------*
*                                                                      *
         DO 200 J    = 1, NHIST                                         
           IP2       = MAPL(3,J)                                        
           DO 100 I  = IP2,IP2+259                                      
             IBUF(I) = 0                                                
  100      CONTINUE                                                     
           IFBASE(J) = 0                                                
  200    CONTINUE                                                       
*                                                                      *
*-------------------------- Clear Scat. Plots -------------------------*
*                                                                      *
         DO 500  J   = 1, NSCAT                                         
           IP2       = MAPD(3,J)                                        
           DO 400  I = IP2,IP2+2500                                     
             IBUF(I) = 0.0                                              
  400      CONTINUE                                                     
  500    CONTINUE                                                       
*                                                                      *
      RETURN                                                            
      END                                                               
************************************************************************
*     ====================                                             *
       SUBROUTINE BHSAVE                                                
*     ====================                                             *
* ((Purpose))                                                          *
*     To save contents of temporary buffers to the histogram buffers,  *
*     in order to avoid the precision problem.                         *
* ((Author))                                                           *
*     S.Kawabata  June '90 at KEK                                      *
************************************************************************
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
      DO 200 J = 1, NHIST                                               
         IP2   = MAPL(3,J)                                              
         NC    = IBUF( MAPL(2,J)+2 ) + 1                                
         IB1   = IP2 + 52                                               
         IB2   = IB1 + 52                                               
         DO 100 I = 0,NC                                                
            I1    = I + IB1                                             
            I2    = I1 + 104                                            
            BUFF(I2)  = BUFF(I2) + BUFF(I1)                             
            BUFF(I1)  = 0.0                                             
            I1    = I + IB2                                             
            I2    = I1 + 104                                            
            BUFF(I2)  = BUFF(I2) + BUFF(I1)                             
            BUFF(I1)  = 0.0                                             
  100    CONTINUE                                                       
  200 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
************************************************************************
*    ===================                                               *
      SUBROUTINE BSCHCK                                                 
*    ===================                                               *
* ((Purpose))                                                          *
*     To check user's initialization parameters.                       *
*                                                                      *
*        Coded by S.Kawabata        Oct. '85                           *
*                                                                      *
************************************************************************
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      PARAMETER ( MXDIM = 50)                                           
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                   
     .               IG(MXDIM),NCALL                                    
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2                             
                                                                        
      COMMON /BASE0/ JFLAG,IBASES                                       
      COMMON /BASE1/ XLT(MXDIM),XUT(MXDIM),NDIMT,NWILDT,                
     .               IGT(MXDIM),NCALLT                                  
      COMMON /BASE2/ ACC1T,ACC2T,ITMX1T,ITMX2T                          
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
      COMMON /XHCNTL/ LOCK                                              
                                                                        
      LOCK  = 1                                                         
                                                                        
      IF( IBASES .NE.  1 ) THEN                                         
          WRITE(6,9000)                                                 
 9000     FORMAT(                                                       
     .     5X,'*************************************************',      
     .    /5X,'*                                               *',      
     .    /5X,'*   BSINIT was not called before calling BASES  *',      
     .    /5X,'*                                               *',      
     .    /5X,'*   Process was terminated due to this error.   *',      
     .    /5X,'*                                               *',      
     .    /5X,'*************************************************')      
          STOP                                                          
      ENDIF                                                             
                                                                        
                                                                        
      IF( NDIM .LT. 1) THEN                                             
          WRITE(6,9100)                                                 
 9100     FORMAT(                                                       
     .     5X,'*************************************************',      
     .    /5X,'*                                               *',      
     .    /5X,'*   NDIM was not set before calling BASES.      *',      
     .    /5X,'*                                               *',      
     .    /5X,'*   Process was terminated due to this error.   *',      
     .    /5X,'*                                               *',      
     .    /5X,'*************************************************')      
          STOP                                                          
      ENDIF                                                             
                                                                        
      NDIMT = NDIM                                                      
                                                                        
      DO 200 I = 1,NDIM     
         IF( XU(I) .LE. -1.0D37) THEN                                   
             WRITE(6,9200) I,I                                          
 9200        FORMAT(                                                    
     .        5X,'*************************************************',   
     .       /5X,'*                                               *',   
     .       /5X,'*   XL(',I6,' ).  XU(',I6,' ) were not set      *',   
     .       /5X,'*    before calling BASES.                      *',   
     .       /5X,'*   Process was terminated due to this error.   *',   
     .       /5X,'*                                               *',   
     .       /5X,'*************************************************')   
             STOP                                                       
         ENDIF                                                          
                                                                        
         IGT(I)  = IG(I)                                                
         XLT(I)  = XL(I)                                                
         XUT(I)  = XU(I)                                                
                                                                        
  200 CONTINUE                                                          
C                                                                       
C  Change the maximum number of the wild variables                      
C 10 ===> 15                                                            
      IF( NWILD .LT.  0) THEN                                           
          NWILD = MIN( NDIM, 15)                                        
          WRITE(6,9300) NWILD                                           
 9300     FORMAT(                                                       
     .     5X,'*************************************************',      
     .    /5X,'*                                               *',      
     .    /5X,'*   NWILD was not set before calling BASES.     *',      
     .    /5X,'*                                               *',      
     .    /5X,'*   NWILD is set equal to the value(',I6,' ).   *',      
     .    /5X,'*                                               *',      
     .    /5X,'*************************************************')      
      ELSE                                                              
     .IF( NWILD .GT. 15) THEN                                           
          NWILDO = NWILD                                                
          NWILD  = MIN( NDIM, 15)                                       
          WRITE(6,9400) NWILDO, NWILD                                   
 9400     FORMAT(                                                       
     .     5X,'*************************************************',      
     .    /5X,'*                                               *',      
     .    /5X,'*   NWILD(',I6,' ) was too large number.        *',      
     .    /5X,'*                                               *',      
     .    /5X,'*   NWILD is set equal to the value(',I6,' ).   *',      
     .    /5X,'*                                               *',      
     .    /5X,'*************************************************')      
      ENDIF                                                             
                                                                        
      NWILDT = NWILD                                                    
      NCALLT = NCALL                                                    
                                                                        
      ITMX1T = ITMX1                                                    
      ITMX2T = ITMX2                                                    
      ACC1T  = ACC1                                                     
      ACC2T  = ACC2                                                     
C                                                                       
      RETURN                                                            
      END                                                               
C***********************************************************************
C*=======================                                              *
       SUBROUTINE BSDATE                                                
C*=======================                                              *
C*((Purpose))                                                          *
C*    Changethe format of the time stamp.                              *
C*    This program should be modified according to the machine.        *
C*((Author))                                                           *
C*    S.Kawabata  Nov. '91 at KEK                                      *
C*    For HP      Jul. '92 at KEK                                      *
C***********************************************************************
       COMMON /BDATE/ IDATE(3),ITIME(2)                                 
       COMMON /SLATE/ IS(40)                                            
*            IDATE(1) : year        ITIME(1) : hour                     
*            IDATE(2) : month       ITIME(2) : minute                   
*            IDATE(3) : day                                             
                                                                        
*      CALL UXDATE(IY,IM,ID,IHH,IMM)                                    
#ifdef CERN_X
       CALL DATIME(ID,IT)
       CALL UCOPY(IS(1),IDATE(1),5)                                     
#endif
       IDATE(1) = MOD(IDATE(1),1900)                                    
*      IDATE(1) = IY                                                    
*      IDATE(2) = IM                                                    
*      IDATE(3) = ID                                                    
*      ITIME(1) = IHH                                                   
*      ITIME(2) = IMM                                                   
       RETURN                                                           
       END                                                              
************************************************************************
*    ===========================================                       *
      SUBROUTINE BSDIMS( MDIM, MWILD, XLL, XUU )                        
*    ===========================================                       *
* ((Purpose))                                                          *
*     To set the BASES parameters.                                     *
* ((Input))                                                            *
*     MDIM   : The number of dimension of integral                     *
*     MWILD  : The number of wild variables                            *
*     XLL(i) : The lower value of the i-th integral variable           *
*     XUU(i) : The upper value of the i-th integral variable           *
* ((Output))                                                           *
*     These parameters are to be set in the labeled common /BPARM1/    *
* (Caution)                                                            *
*     The parameter IG(i) is not able to set by this routine.          *
*     If some of parameters IG(i) are required to be changed,          *
*     it is done by calling the subroutine BSGRID.                     *
*                                                                      *
*        Coded by S.Kawabata         August '94                        *
*                                                                      *
************************************************************************
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      PARAMETER (MXDIM = 50, NDMX = 50 )                                
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                   
     .               IG(MXDIM),NCALL                                    
                                                                        
      DIMENSION XLL(MDIM), XUU(MDIM)                                    
                                                                        
*=========================================================              
                                                                        
      NDIM   = MDIM                                                     
      NWILD  = MWILD                                                    
      DO 100 I= 1, NDIM                                                 
         XL(I) = XLL(I)                                                 
         XU(I) = XUU(I)                                                 
  100 CONTINUE                                                          
                                                                        
       RETURN                                                           
       END                                                              
C***********************************************************************
C*                                                                     *
C*========================                                             *
C*    SUBROUTINE BSETGU                                                *
C*========================                                             *
C*((Function))                                                         *
C*     Initialization of Bases progam                                  *
C*     This is called only when IFLAG=0.                               *
C*     ( IFLAG = 0 ; First Trial of Defining Grid step )               *
C*                                                                     *
C*    Changed by S.Kawabata    Aug. 1984 at Nagoya Univ.               *
C*    Last update              Oct. 1985 at KEK                        *
C*                                                                     *
C***********************************************************************
C                                                                       
      SUBROUTINE BSETGU                                                 
C                                                                       
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)                   
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                    
     .               IG(MXDIM),NCALL                                    
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),      
     .               ND,NG,NPG,MA(MXDIM)                                
      COMMON /BASE6/ D(NDMX,MXDIM),                                     
     .               ALPH,XSAVE(NDMX,MXDIM),XTI,XTSI,XACC,ITSX          
                                                                        
      DIMENSION  XIN(NDMX)                                              
      DATA  ONE/ 1.0D0/                                                 
C                                                                       
C---------------------------------------------------------------        
C           Define the number of grids and sub-regions                  
C---------------------------------------------------------------        
C==> Determine NG : Number of grids                                     
          NG    = (NCALL/2.)**(1./NWILD)                                
         IF(NG .GT. 25) NG  = 25                                        
  100    IF(NG .LT.  2) NG  =  1                                        
         IF(NG**NWILD .GT. LENG) THEN                                   
            NG  = NG - 1                                                
            GO TO 100                                                   
         ENDIF                                                          
C                                                                       
C==> Determine ND : Number of sub-regions                               
          M     = NDMX/NG                                               
          ND    = M*NG                                                  
C                                                                       
C==> Determine NPG: Number of sampling points per subhypercube          
          NSP   = NG**NWILD                                             
          NPG   = NCALL/NSP                                             
                                                                        
          XI(1,1)= ONE                                                  
          MA(1)  = 1                                                    
          DX(1)  = XU(1)-XL(1)                                          
                                                                        
          IF( NDIM .GT. 1 ) THEN                                        
              DO 130 J = 2,NDIM                                         
                 XI(1,J)= ONE                                           
                 DX(J)  = XU(J)-XL(J)                                   
                 IF( J .LE. NWILD ) THEN                                
                    MA(J)  = NG*MA(J-1)                                 
                 ENDIF                                                  
  130         CONTINUE                                                  
          ENDIF                                                         
C                                                                       
C---------------------------------------------------------------        
C           Set size of subregions uniform                              
C---------------------------------------------------------------        
          NDM   = ND-1                                                  
          RC    = ONE/ND                                                
          DO 155 J =1,NDIM                                              
             K     = 0                                                  
             XN    = 0.D0                                               
             DR    = XN                                                 
             I     = K                                                  
  140        K     = K+1                                                
             DR    = DR+ONE                                             
             XO    = XN                                                 
             XN    = XI(K,J)                                            
  145       IF(RC .GT. DR) GO TO 140                                    
             I     = I+1                                                
             DR    = DR-RC                                              
             XIN(I)= XN-(XN-XO)*DR                                      
            IF(I .LT. NDM) GO TO 145                                    
             DO 150 I  = 1,NDM                                          
                XI(I,J)= XIN(I)                                         
  150        CONTINUE                                                   
             XI(ND,J)  = ONE                                            
  155     CONTINUE                                                      
********************************************* Updated Feb.08 '94        
          IF( ITSX .GT. 0 ) THEN                                        
              IPSAVE = 1                                                
              XACC    = 1.0D37                                          
              XTI     = 0.0D0                                           
              XTSI    = XACC                                            
              ITSX    = 1                                               
              DO 200 J = 1, NDIM                                        
              DO 200 I = 1, ND                                          
                 XSAVE(I,J) = XI(I,J)                                   
  200         CONTINUE                                                  
          ENDIF                                                         
C                                                                       
      RETURN                                                            
      END                                                               
C***********************************************************************
C*                                                                     *
C*========================                                             *
C*    SUBROUTINE BSETGV( IFLAG )                                       *
C*========================                                             *
C*((Function))                                                         *
C*    Refine the grid sizes                                            *
C*                                                                     *
C*    Coded   by S.Kawabata    Aug. 1984 at Nagoya Univ.               *
C*    Last update              Oct. 1985 at KEK                        *
C*                                                                     *
C***********************************************************************
C                                                                       
      SUBROUTINE BSETGV( IFLAG )                                        
C                                                                       
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)                   
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                    
     .               IG(MXDIM),NCALL                                    
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),      
     .               ND,NG,NPG,MA(MXDIM)                                
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT                          
      COMMON /BASE6/ D(NDMX,MXDIM),                                     
     .               ALPH,XSAVE(NDMX,MXDIM),XTI,XTSI,XACC,ITSX          
      REAL*4 STIME                                                      
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF                        
*                                                                       
                                                                        
      DIMENSION  XIN(NDMX),R(NDMX),DT(MXDIM),DDX(NDMX)                  
      DATA  ONE/1.0D0/,ZERO/0.0D0/,N0/0/,N1/1/                          
*                                                                       
*========= Save the grid information for the best accuracy ===========  
*                                                                       
      IF( ITSX .GT. 0 ) THEN                                            
          IF( IFLAG .EQ. 0 ) THEN                                       
              IF( IT .GE. 5 ) THEN                                      
                  IF( ( TI .GT. AVGI+SD) .AND. TSI .LT. XTSI ) THEN     
                      DO 400 J = 1, NDIM                                
                      DO 400 I = 1, ND                                  
                         XSAVE(I,J) = XI(I,J)                           
  400                 CONTINUE                                          
                      XACC         = TACC                               
                      ITSX         = IT                                 
                      XTI          = TI                                 
                      XTSI         = TSI                                
                  ENDIF                                                 
              ENDIF                                                     
          ELSE                                                          
              IF( ( XTI .GT. TI) .AND. XTSI .LT. TSI ) THEN             
                  DO 500 J = 1, NDIM                                    
                  DO 500 I = 1, ND                                      
                     XI(I,J) = XSAVE(I,J)                               
  500             CONTINUE                                              
*                ==========                                             
                   RETURN                                               
*                ==========                                             
              ENDIF                                                     
          ENDIF                                                         
      ENDIF                                                             
                                                                        
C======= SMOOTHING THE FUNCTION D(I,J)                                  
C                                                                       
        CLOGE   = 1.0D0/LOG(10.0D0)                                     
                                                                        
        NDM     = ND-1                                                  
        DO 780 J= N1,NDIM                                               
         IF( IG(J) .EQ. 1 ) THEN                                        
          DDX(1)= 0.5D0*(D(1,J) + D(2,J))                               
          DO 710 I=2,NDM                                                
            DDX(I)= (D(I+1,J) + D(I,J) + D(I-1,J))/3.D0                 
  710     CONTINUE                                                      
          DDX(ND) = 0.5D0*(D(NDM,J) + D(ND,J))                          
          DT(J) = 0.D0                                                  
          DO 720 I = 1, ND                                              
             D(I,J) = DDX(I)                                            
             DT(J)  = DT(J)+D(I,J)                                      
  720     CONTINUE                                                      
C                                                                       
C=========== REDEFINE THE GRID                                          
C                                                                       
                                                                        
          DTLOG   = LOG(DT(J))                                          
          DT10    = CLOGE*DTLOG                                         
          RC    = ZERO                                                  
          DO 730 I= N1,ND                                               
            R(I)  = ZERO                                                
            IF(D(I,J) .GT. ZERO) THEN                                   
               DILOG = LOG(D(I,J))                                      
               IF( DT10 - CLOGE*DILOG  .LE. 70.0D0 ) THEN               
                   XO    = DT(J)/D(I,J)                                 
                   R(I)  = ((XO-ONE)/(XO*(DTLOG-DILOG)))**ALPH          
               ELSE                                                     
C                  XO    = DT(J)/D(I,J)                                 
                   R(I)  = (DTLOG-DILOG)**(-ALPH)                       
               ENDIF                                                    
            ENDIF                                                       
            RC    = RC+R(I)                                             
  730     CONTINUE                                                      
          RC    = RC/ND                                                 
          K     = N0                                                    
          XN    = N0                                                    
          DR    = XN                                                    
          I     = K                                                     
  740  K     = K + N1                                                   
          DR    = DR+R(K)                                               
          XO    = XN                                                    
          XN    = XI(K,J)                                               
  750 IF(RC.GT.DR)GO TO 740                                             
          I     = I + N1                                                
          DR    = DR-RC                                                 
          XIN(I)= XN-(XN-XO)*DR/R(K)                                    
      IF(I.LT.NDM)GO TO 750                                             
          DO 760 I= N1,NDM                                              
            XI(I,J)= XIN(I)                                             
  760     CONTINUE                                                      
          XI(ND,J)= ONE                                                 
         ENDIF                                                          
  780   CONTINUE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
************************************************************************
*    ==================================                                *
      SUBROUTINE BSGRID( MDIM, IGG )                                    
*    ==================================                                *
* ((Purpose))                                                          *
*     To change the grid optimizing flag.                              *
* ((Input))                                                            *
*     MDIM   : The number of dimension of integral                     *
*     IGG(i) : The flag switches whether the grid of i-th variable     *
*              is to be optimized ( 1 ) or kept uniform ( 0 ).         *
* ((Output))                                                           *
*     These parameters are to be set in the labeled common /BPARM1/    *
*                                                                      *
*        Coded by S.Kawabata         August '94                        *
*                                                                      *
************************************************************************
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      PARAMETER (MXDIM = 50, NDMX = 50 )                                
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                   
     .               IG(MXDIM),NCALL                                    
                                                                        
      DIMENSION IGG(MDIM)                                               
                                                                        
*=========================================================              
                                                                        
      NDIM   = MDIM                                                     
      DO 100 I= 1, NDIM                                                 
         IG(I) = IGG(I)                                                 
  100 CONTINUE                                                          
                                                                        
       RETURN                                                           
       END                                                              
*********************************************************************** 
*============================                                         * 
      SUBROUTINE BSINFO( LU )                                           
*============================                                         * 
*((Purpose))                                                          * 
*    Print the information for                                        * 
*        (1) BASES parameters                                         * 
*        (2) Computer time information                                * 
*        (3) Convergency behavior of the Grid optimization step       * 
*        (4) Convergency behavior of the integration step             * 
*(( Input ))                                                          * 
*    LU  :  Logical unit number of printer                            * 
*                                                                     * 
*           by S.Kawabata    March 1994 at KEK                          
*                                                                     * 
*********************************************************************** 
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      REAL*4 STIME                                                      
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF                        
                                                                        
*  Print Title                                                          
                                                                        
      CALL BSPRNT( LU, 1, IDUM1, IDUM2 )                                
                                                                        
*  Print Bases parameters                                               
                                                                        
      CALL BSPRNT( LU, 4, IDUM1, IDUM2 )                                
                                                                        
*  Print Computing time information                                     
                                                                        
      CALL BSPRNT( LU, 3, IDUM1, IDUM2 )                                
                                                                        
*  Print Convergency Behaviors                                          
                                                                        
      DO 100 ISTEP = 0, 1                                               
         ITX  = ITG                                                     
         IF( ISTEP .EQ. 1 ) ITX = ITF                                   
                                                                        
      IF( ITX .GT. 0 ) THEN                                             
                                                                        
         CALL BSPRNT( LU, 8, ITX, ISTEP )
	                                                                
                                                                        
      ENDIF                                                             
  100 CONTINUE                                                          
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*    ===================                                               *
      SUBROUTINE BSINIT                                                 
*    ===================                                               *
* ((Purpose))                                                          *
*     Initialization of BASE50/SPRING50.                               *
*     Function of this routine is                                      *
*       (0) Set the size of histogram and scatter plot buffers         *
*       (1) Set the parameters INTV and IPNT                           *
*             INTV = ( 0 / 1 / any )                                   *
*                  = ( Batch / Batch(Unix) / Interactive )             *
*             IPNT = ( 0 / any )                                       *
*                  = ( IBM Type / Ascii printer )                      *
*       (2) Set the acceleration factor ALPHA by 1.5                   *
*            The range of this value is from 0.0 to 2.0.               *
*            ALPHA = 0.0 results in no grid-optimization.              *
*       (3) Set the grid-optimization flag IGOPT ( Default value 0 )   *
*             IGOPT = 0  :  The grid is optimized by VEGAS algorithm   *
*             IGOPT = 1  :  The grid is optimized so that the accuracy *
*                           of each iteration be minimized.            *
*       (4) Set Node-ID number NODEID and the number of nodes NUMNOD   *
*       (5) Set seed of radom number                                   *
*       (6) Set the values of BASES paremeters with default ones.      *
*       (7) Set the values of parameters with non-sense values,        *
*            which should be set again with the true values by User    *
*            before running BASES.                                     *
*                                                                      *
*        Coded by S.Kawabata         March '94                         *
*                                                                      *
************************************************************************
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      PARAMETER (MXDIM = 50, NDMX = 50 )                                
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                   
     .               IG(MXDIM),NCALL                                    
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2                             
                                                                        
      COMMON /BASE0/ JFLAG,IBASES                                       
      COMMON /BASE6/ D(NDMX,MXDIM),                                     
     .               ALPH,XSAVE(NDMX,MXDIM),XTI,XTSI,XACC,IGOPT         
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
       COMMON/NINFO/ NODEID, NUMNOD                                     
       COMMON /BDATE/ IDATE(3),ITIME(2)                                 
*            IDATE(1) : year        ITIME(1) : hour                     
*            IDATE(2) : month       ITIME(2) : minute                   
*            IDATE(3) : day                                             
      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1      
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1                  
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)                    
*=========================================================              
* (0) Initialization of timer and Histogram buffer                      
*     Timer initialization                                              
       CALL BSTIME( TIME0, 0 )                                          
       TIMEB1 = TIME0                                                   
       TIMINT = 0                                                       
                                                                        
*     Histogram buffer initialization                                   
       LU  = 6                                                          
       CALL BHINIT( LU )                                                
                                                                        
*=========================================================              
                                                                        
* (1) Set the parameters INTV and IPNT                                  
       INTV  = 0                                                        
       IPNT  = 1                                                        
* (2) Set the acceleration factor ALPHA by 1.5                          
       ALPH  = 1.5D0                                                    
* (3) Set the grid-optimization flag IGOPT                              
       IGOPT = 0                                                        
* (4) Set Node-ID number NODEID and the number of nodes NUMNOD          
*      IF( INTV .EQ. 0 ) THEN                                           
           NODEID = 0                                                   
           NUMNOD = 1                                                   
*      ELSE                                                             
*          NODEID = 0                                                   
*          NUMNOD = 1                                                   
*      ENDIF                                                            
                                                                        
C---------------------------------------------------------------        
C (5)  Set initial seeds of random number generator                     
C---------------------------------------------------------------        
       ISEED = 12345                                                    
C                                                                       
       CALL DRNSET( ISEED )                                             
C ---------------------------------------------------------------       
C (6),(7)  Set BASES parameters equal to default values                 
C ---------------------------------------------------------------       
C                                                                       
cccAB       NDIM   = -1                                                      
cccAB        NWILD  =  1                                                      
cccAB        ITMX1  = 15                                                      
cccAB        ITMX2  = 100                                                     
cccAB        NCALL  = 1000                                                    
cccAB        ACC1   = 0.2D0                                                   
cccAB        ACC2   = 0.01D0                                                  
       DO 100 I = 1,MXDIM                                               
          IG(I) = 1                                                     
          XU(I)  = -1.0D37                                              
  100  CONTINUE                                                         
                                                                        
*    Initialization of computing time table of BASES                    
       DO 200 I = 0, 2                                                  
          TIMEBS(I) = 0.0                                               
  200  CONTINUE                                                         
                                                                        
*-------------------------------------------                            
*      Don't change IBASES from this value                              
*-------------------------------------------                            
       IBASES =  1                                                      
                                                                        
       RETURN                                                           
       END                                                              
*********************************************************************** 
*                                                                     * 
*    ==========================                                       * 
      SUBROUTINE BSINTG( FXN )                                          
*    ==========================                                       * 
*((Function))                                                         * 
*    Subroutine performs N-dimensional Monte Carlo integration        * 
*    for four vector generation of simulated events                   * 
*                                                                     * 
*       JFLAG = 0 ; First Trial of Defining Grid                      * 
*       JFLAG = 1 ; First Trial of Data Accumulation                  * 
*       JFLAG = 2 ; Second Trial of Defining Grid                     * 
*       JFLAG = 3 ; Second Trial of Data Accumulation                 * 
*                                                                     * 
*    Coded   by S.Kawabata    July 1980 at DESY, Hamburg              * 
*    Last update              March 1994                              * 
*                                                                     * 
*********************************************************************** 
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
                                                                        
      EXTERNAL FXN                                                      
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)                   
      COMMON /BASE0/ JFLAG,IBASES                                       
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                    
     .               IG(MXDIM),NCALL                                    
      COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2                              
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT                          
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),      
     .               ND,NG,NPG,MA(MXDIM)                                
      PARAMETER (ITM = 50)                                              
      REAL*4 TIME, EFF, WRONG, TRSLT, TSTD, PCNT                        
      COMMON /BASE5/ ITRAT(ITM,0:1),TIME(ITM,0:2),EFF(ITM,0:1),         
     .               WRONG(ITM,0:1),RESLT(ITM,0:1),ACSTD(ITM,0:1),      
     .               TRSLT(ITM,0:1),TSTD(ITM,0:1),PCNT(ITM,0:1)         
      COMMON /BASE6/ D(NDMX,MXDIM),                                     
     .               ALPH,XSAVE(NDMX,MXDIM),XTI,XTSI,XACC,ITSX          
      REAL*4 STIME                                                      
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF                        
      CHARACTER*80 ERROR                                                
      COMMON /BWARN1/ NERROR                                            
      COMMON /BWARN2/ ERROR(3,3)                                        
*                                                                       
*        INTV = ( 0 / 1 / any ) = ( Batch / Batch(Unix) / Interactive ) 
*        IPNT = ( 0 / any ) = ( IBM Type / Ascii printer )              
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
                                                                        
      REAL*8  X(MXDIM)                                                  
      INTEGER KG(MXDIM),IA(MXDIM)                                       
                                                                        
      COMMON/NINFO/ NODEID, NUMNOD                                      
      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1      
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1                  
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)                    
*                                                                       
*     Parameters for checking convergency                               
*                                                                       
      DATA ACLMT,FC / 25.0D0, 5.0D0 /                                   
                                                                        
C     REAL*8  TX(2)                                                     
      INTEGER NCNODE(2,512),NPNODE(2,512)                               
C     INTEGER NEFF(2)                                                   
                                                                        
      DATA  ONE/ 1.0D0/, ZERO/0.0D0/, LU / 6/                           
      DATA  N0/0/, N1/1/, HUNDRT/100.0D0/                               
                                                                        
************************************************************************
*                       Initialization Part                             
************************************************************************
*=======================================================================
*          Determine the number of hypercubes NSP                       
*=======================================================================
                                                                        
      XND     = ND                                                      
      NSP     = NG**NWILD                                               
      XJAC    = 1.0D0                                                   
      DO  5 I = 1, NDIM                                                 
         XJAC = XJAC*DX(I)                                              
    5 CONTINUE                                                          
      CALLS   = NSP*NPG                                                 
      DXG     = 1.0D0/NG                                                
      DV2G    = DXG**(2*NWILD)/NPG/NPG/(NPG-1)                          
      DXG     = DXG*XND                                                 
                                                                        
      IF( NSP .EQ. 1 ) THEN                                             
*=======================================================================
*           Determination of the number of sampling points              
*               per node in the single hypercube case                   
*=======================================================================
          MEX     = MOD(NPG,NUMNOD)                                     
          NPERCP  = NPG/NUMNOD                                          
          NPGT    = 0                                                   
          DO  12 NODEX = 1,NUMNOD                                       
             NPGS  = NPGT + 1                                           
             NPGT  = NPGT + NPERCP                                      
             IF( NODEX .LE. MEX ) NPGT = NPGT + 1                       
             NCNODE(1,NODEX) = 1                                        
             NCNODE(2,NODEX) = 1                                        
             NPNODE(1,NODEX) = NPGS                                     
             NPNODE(2,NODEX) = NPGT                                     
   12     CONTINUE                                                      
      ELSE                                                              
*=======================================================================
*          Determination of the number of hypercubes                    
*              per node in many hypercubes case                         
*=======================================================================
          MEX     = MOD(NSP,NUMNOD)                                     
          NPERCP  = NSP/NUMNOD                                          
          NSPT    = 0                                                   
          DO  15 NODEX = 1,NUMNOD                                       
             NSPS  = NSPT + 1                                           
             NSPT  = NSPT + NPERCP                                      
             IF( NODEX .LE. MEX ) NSPT = NSPT + 1                       
             NCNODE(1,NODEX) = NSPS                                     
             NCNODE(2,NODEX) = NSPT                                     
             NPNODE(1,NODEX) = 1                                        
             NPNODE(2,NODEX) = NPG                                      
   15     CONTINUE                                                      
      ENDIF                                                             
*=======================================================================
      NEND    = N0                                                      
      ATACC   = ZERO                                                    
      NERROR  = N0                                                      
      NER1    = N0                                                      
      NER2    = N0                                                      
      NER3    = N0                                                      
      SUMTI   = ZERO                                                    
      SUMTSI  = ZERO                                                    
                                                                        
      IF(JFLAG .EQ. N0 .OR. JFLAG .EQ. N1 ) THEN                        
*-----------------------------------------------------------------------
*        JFLAG = 0  : The first trial of the grid optim. step           
*        JFLAG = 1  : The first trial of the integration step           
*-----------------------------------------------------------------------
         DO 10 J  = N1,NSP                                              
           DXD(J) = ZERO                                                
           DXP(J) = ZERO                                                
   10    CONTINUE                                                       
*       -----------------                                               
         ISTEP   = JFLAG                                                
*       -----------------                                               
         IT1   = N1                                                     
         SI    = ZERO                                                   
         SI2   = ZERO                                                   
         SWGT  = ZERO                                                   
         SCHI  = ZERO                                                   
*       =============                                                   
         CALL BHRSET                                                    
*       =============                                                   
         NSU     = N0                                                   
         SCALLS= ZERO                                                   
      ELSE                                                              
*-----------------------------------------------------------------------
*        JFLAG = 2  : The continuation of the grid optim. step          
*        JFLAG = 3  : The continuation of the integration step          
*-----------------------------------------------------------------------
C        IF( JFLAG .EQ. 2 ) THEN                                        
*           -------------                                               
C            ISTEP  = N0                                                
*           -------------                                               
C        ELSE                                                           
C    .   IF( JFLAG .EQ. 3 ) THEN                                        
*           -------------                                               
C            ISTEP  = N1                                                
*           -------------                                               
C        ELSE                                                           
C                *****************                                      
C                      STOP                                             
C                *****************                                      
C         ENDIF                                                         
C                                                                       
C         IT1   = IT + 1                                                
      ENDIF                                                             
                                                                        
*------- Set the expected accuracy and the max. iteration number -------
                                                                        
      ITMX   = ITMX1                                                    
      ACC    = ACC1*0.01D0                                              
      IF( ISTEP .EQ. N1 ) THEN                                          
         ITMX = ITMX2                                                   
         ACC  = ACC2*0.01D0                                             
      ENDIF                                                             
                                                                        
*-------- Print the title of the convergency behavior table ----------- 
*                  in the interactive mode                              
      IF( INTV .GT. 1 ) THEN                                            
*         -----------------------------------                           
           CALL BSPRNT( LU, 5, ISTEP, IDUM2 )                           
*         -----------------------------------                           
      ENDIF                                                             
      NEGFLG     = 0                                                    
                                                                        
*    =====================                                              
      CALL BSUTIM( 0, 2 )                                               
*    =====================                                              
                                                                        
*********************************************************************   
*               Main Integration Loop                                   
*********************************************************************   
*    ========                                                           
      DO 500  IT = IT1,ITMX                                             
*    ========                                                           
*=======================================================================
*                 Initialization for the iteration                      
*=======================================================================
                                                                        
         SCALLS  = SCALLS + CALLS                                       
         NGOOD   = N0                                                   
         NEGTIV  = N0                                                   
         TI      = ZERO                                                 
         TSI     = TI                                                   
                                                                        
         IF( ISTEP .EQ. N0 ) THEN                                       
             DO 200 J= N1,NDIM                                          
             DO 200 I=1,ND                                              
                D(I,J)= TI                                              
  200        CONTINUE                                                   
         ENDIF                                                          
                                                                        
         NODEX  = NODEID                                                
         IF( NODEID .EQ. 0 )  NODEX = NUMNOD                            
                                                                        
*---------------------------------------------------------------------  
*        Distributing hyper cubes to NumNode nodes                      
*           NCNODE(1,NODEX)   : 1st cube number for the node NODEX      
*           NCNODE(2,NODEX)   : Last cube number for the node NODEX     
*                    NODEX    : node number 1 => NumNode(=0)            
*                    NODEX    : node number 1 => NumNode(=0)            
*---------------------------------------------------------------------  
                                                                        
         NSP1  = NCNODE(1,NODEX)                                        
         NSP2  = NCNODE(2,NODEX)                                        
*                                 Dummy loopfor a parallel processor    
C                                 IF( NSP1 .GT. 1 ) THEN                
C                                     CALL DRLOOP( NDIM*NPG*(NSP1-1) )  
C                                 ENDIF                                 
                                                                        
*=====================================================================  
*      Loop for hypercube from NSP1 to NSP2 in the NodeX-th node        
*=====================================================================  
*       ========                                                        
         DO 400 NCB = NSP1, NSP2                                        
*       ========                                                        
            FB      = 0.0                                               
            F2B     = 0.0                                               
            NP      = NCB - 1                                           
            IF( NWILD .GT. 1 ) THEN                                     
                DO 210 J = 1,NWILD-1                                    
                   NUM   = MOD(NP,MA(J+1))                              
                   KG(J) = NUM/MA(J) + 1                                
  210           CONTINUE                                                
            ENDIF                                                       
            KG(NWILD)     = NP/MA(NWILD) + 1                            
                                                                        
*---------------------------------------------------------------------  
*       If number of hypercubes is only one,                            
*        Distributing sampling points to NumNode nodes                  
*           NPNODE(1,NODEX)   : 1st sample point for the node NODEX     
*           NPNODE(2,NODEX)   : Last sample point for the node NODEX    
*                    NODEX    : node number 1 => NumNode(=0)            
*---------------------------------------------------------------------  
                                                                        
            NPG1  = NPNODE(1,NODEX)                                     
            NPG2  = NPNODE(2,NODEX)                                     
*                                 Dummy loop for a parallel processor   
C                                 IF( NPG1 .GT. 1 ) THEN                
C                                     CALL DRLOOP( NDIM*(NPG1-1) )      
C                                 ENDIF                                 
                                                                        
*=====================================================================  
*          Loop for sampling points from NPG1 to NPG2                   
*                in the single hypercube case                           
*=====================================================================  
*          ========                                                     
            DO 300 NTY = NPG1,NPG2                                      
*          ========                                                     
*---------------------------------------------------------------------  
*        Determine the integration variables by random numbers          
*---------------------------------------------------------------------  
                                                                        
               WGT   = XJAC                                             
               DO 250 J= 1,NDIM                                         
                  IF( J .LE. NWILD ) THEN                               
                      XN  = (KG(J)-DRN(IDUMY))*DXG+1.D0                 
                  ELSE                                                  
                      XN  = ND*DRN(IDUMY)+1.D0                           
                  ENDIF                                                 
                  IA(J)   = XN                                          
                  IAJ     = IA(J)                                       
                  IF( IAJ .EQ. 1) THEN                                  
                      XO  = XI(IAJ,J)                                   
                      RC  = (XN-IA(J))*XO                               
                  ELSE                                                  
                      XO  = XI(IAJ,J)-XI(IAJ-1,J)                       
                      RC  = XI(IAJ-1,J)+(XN-IAJ)*XO                     
                  ENDIF                                                 
                  X(J)    = XL(J)+RC*DX(J)                              
                  WGT     = WGT*XO*XND                                  
  250          CONTINUE                                                 
*-----------------------------------------------------------------------
*                     =======                                           
               FXG  =  FXN(X)*WGT                                       
*                     =======                                           
*-----------------------------------------------------------------------
*             Check the value of the integrand                          
*-----------------------------------------------------------------------
                                                                        
               IF( FXG .NE. 0.0 ) THEN                                  
                   NGOOD = NGOOD + 1                                    
                   IF( ISTEP .EQ. 1 ) THEN                              
                       DXD(NCB) = DXD(NCB) + FXG                        
                       IF( FXG .GT. DXP(NCB) ) DXP(NCB) = FXG           
                   ENDIF                                                
                   IF( FXG .LT. 0.0 ) THEN                              
                       NEGTIV= NEGTIV+ 1                                
                       IF( NEGFLG .EQ. 0 ) THEN                         
                          WRITE(6,9200) IT,NODEID                       
 9200                     FORMAT(1X,                                    
     .                       '******* WARNING FROM BASES ********',     
     .                       '***********',                             
     .                       /1X,'*  Negative FUNCTION at IT =',I3,1X,  
     .                       ', node = ',I3,1X,'*',                     
     .                       /1X,'***********************************', 
     .                       '***********')                             
                          NEGFLG  = 1                                   
                       ENDIF                                            
                   ENDIF                                                
               ENDIF                                                    
                                                                        
*-----------------------------------------------------------------------
*              Accumulation of FXG and FXG*FXG                          
*-----------------------------------------------------------------------
                                                                        
               F2    = FXG*FXG                                          
               FB    = FB + FXG                                         
               F2B   = F2B + F2                                         
                                                                        
               IF( ISTEP .EQ. 0 ) THEN                                  
                   DO 260  J = 1,NDIM                                   
                      D(IA(J),J)= D(IA(J),J)+F2                         
  260              CONTINUE                                             
               ENDIF                                                    
*======                                                                 
  300       CONTINUE                                                    
*======                                                                 
*------------------------------------------- for a parallel processor   
*                                 Dummy loop for a parallel processor   
C                                 IF( NPG2 .LT. NPG ) THEN              
C                                     CALL DRLOOP(NDIM*(NPG-NPG1))      
C                                 ENDIF                                 
*                                 Global sum of FB and F2B              
C                                 IF( NSP .EQ. 1 ) THEN                 
C                                     CALL BSDSUM(  FB, 1 )             
C                                     CALL BSDSUM( F2B, 1 )             
C                                 ENDIF                                 
*-----------------------------------------------------------------------
                                                                        
*-----------------------------------------------------------------------
*         Calculate the estimate and variance in the hypercube          
*-----------------------------------------------------------------------
                                                                        
            F2B   = DSQRT(F2B*NPG)                                      
            F2S   = (F2B-FB)*(F2B+FB)                                   
            TI    = TI+FB                                               
            TSI   = TSI + F2S                                           
                                                                        
*======                                                                 
  400    CONTINUE                                                       
*======                                                                 
*------------------------------------------- for a parallel processor   
*                                 Dummy loop                            
C                                 IF( NSP2 .LT. NSP ) THEN              
C                                     CALL DRLOOP(NDIM*NPG*(NSP-NSP2))  
C                                 ENDIF                                 
                                                                        
*                                 Global sum of efficiency and frequency
*                                     of negative valued function       
C                                 NEFF(1) = NGOOD                       
C                                 NEFF(2) = NEGTIV                      
C                                 CALL BSISUM( NEFF, 2 )                
                                                                        
C                                 TX(1) = TI                            
C                                 TX(2) = TSI                           
C                                 IF( NSP .EQ. 1 ) THEN                 
C                                     CALL BSDSUM(   TX, 2 )            
C                                 ENDIF                                 
                                                                        
*                                 Global sum of grid information        
C                                 IF( ISTEP .EQ. 0 ) THEN               
C                                     NOWORK = NDMX*NDIM                
C                                     CALL BSDSUM(    D, NOWORK )       
C                                 ENDIF                                 
                                                                        
*=====================================================================  
*           Compute Result of this Iteration                            
*=====================================================================  
*--------------------------------------------------------------------   
*           Accumulate the histogram entries                            
*--------------------------------------------------------------------   
*       -------------                                                   
         CALL BHSAVE                                                    
*       -------------                                                   
*--------------------------------------------------------------------   
                                                                        
C        TI     = TX(1)                                                 
C        TSI    = TX(2)                                                 
C        NGOOD  = NEFF(1)                                               
C        NEGTIV = NEFF(2)                                               
                                                                        
         TI    = TI/CALLS                                               
         TSI   = TSI*DV2G                                               
**                                                                      
         IF( TSI .LE. 1.0D-37 ) TSI = 1.0D-37                           
**                                                                      
         TI2   = TI*TI                                                  
                                                                        
         IF( NGOOD .LE. 10 ) THEN                                       
*           --------------------------------                            
             CALL BSPRNT( LU, 9, IDUM1, IDUM2 )                         
*           --------------------------------                            
*            *****************                                          
                   STOP                                                 
*            *****************                                          
                                                                        
         ENDIF                                                          
                                                                        
*--------------------------------------------------------------------   
*               Calculate the cumulative result                         
*--------------------------------------------------------------------   
                                                                        
         WGT   = ONE/TSI                                                
         SI    = SI+TI*WGT                                              
         SWGT  = SWGT+WGT                                               
         SCHI  = SCHI+TI2*WGT                                           
         AVGI  = SI/SWGT                                                
         CHI2A = ZERO                                                   
         IF(IT .GT. N1 ) CHI2A = (SCHI - SI*AVGI)/(IT-.999D0)           
         SD    = DSQRT(ONE/SWGT)                                        
                                                                        
*---------------------------------------------------------------------  
*             Save the results in the buffer                            
*---------------------------------------------------------------------  
                                                                        
         TSI   = DSQRT(TSI)                                             
         ITX         = MOD( IT, ITM)                                    
         IF( ITX .EQ. 0 ) ITX = ITM                                     
         ITRAT(ITX,ISTEP)  = IT                                         
         EFF  (ITX,ISTEP)  = NGOOD/CALLS*HUNDRT                         
         WRONG(ITX,ISTEP)  = NEGTIV/CALLS*HUNDRT                        
         RESLT(ITX,ISTEP)  = AVGI                                       
         ACSTD(ITX,ISTEP)  = SD                                         
         TRSLT(ITX,ISTEP)  = TI                                         
         TACC              = ABS(TSI/TI*HUNDRT)                         
         TSTD (ITX,ISTEP)  = TACC                                       
         PCNT (ITX,ISTEP)  = ABS(SD/AVGI*HUNDRT)                        
                                                                        
*---------------------------------------------------------------------- 
*                  Check cumulative accuracy                            
*---------------------------------------------------------------------- 
                                                                        
         IF( NODEID .EQ. 0 ) THEN                                       
                                                                        
*-------------------  Check cumulative accuracy ----------------------- 
                                                                        
             SDAV  = SD/AVGI                                            
             IF((ABS(SDAV) .LE. ACC)) NEND = N1                         
                                                                        
             IF( ISTEP .EQ. N1 ) THEN                                   
                 IF( TACC .GT. ACLMT ) THEN                             
                     IF( NER1 .EQ. 0 ) THEN                             
                         NERROR = NERROR + 1                            
                         WRITE(ERROR(1,NERROR),9900) NERROR,IT,ACLMT    
 9900                    FORMAT('* (',I1,') Temp. accuracy of it-#',    
     .                         I3,' is too large comparing to',         
     .                         F6.2,' percent.',6X,'*')                 
                         WRITE(ERROR(2,NERROR),9901) TACC,ACLMT         
 9901                    FORMAT('*',8X,'Temp. accuracy (',              
     .                         F7.4,' % )  >>   (',                     
     .                         F7.4,' % )',23X,'*')                     
                         WRITE(ERROR(3,NERROR),9902)                    
 9902                    FORMAT('*',77X,'*')                            
                         NER1  = 1                                      
                     ENDIF                                              
                 ENDIF                                                  
                 IF( IT .GT. 1 ) THEN                                   
                     IF(( TI .GT. AVTI+FDEVI ) .OR.                     
     .                  ( TI .LT. AVTI-FDEVI )      ) THEN              
                          IF( NER2 .EQ. 0 ) THEN                        
                              NERROR = NERROR + 1                       
                              WRITE(ERROR(1,NERROR),9910) NERROR,IT,FC  
 9910                         FORMAT('* (',I1,') Temp. estimate of ',   
     .                        'it-#',I3,' fluctuates more than ',       
     .                               F4.1,'*average-sigma.',6X,'*')     
                              RE = TI                                   
                              CALL BSORDR( RE, FX2, ORDER, IORDR )      
                              RE = TI/ORDER                             
                              RE1 = AVTI                                
                              AC  = FDEVI                               
                              IF( RE1 .GE. AC ) THEN                    
                                  CALL BSORDR( RE1, FX2, ORDR1, IORDR1) 
                              ELSE                                      
                                  CALL BSORDR( AC, FX2, ORDR1, IORDR1)  
                              ENDIF                                     
                              RE1 = AVTI/ORDR1                          
                              AC  = AC/ORDR1                            
                              WRITE(ERROR(2,NERROR),9911) RE,IORDR,     
     .                                          RE1,AC,IORDR1           
 9911                         FORMAT('*        Temp. Estimate (',       
     .                         F10.6,' E',I3,')  >  (',F10.6,'+',F8.6,  
     .                         ' ) E',I3,', or',1X,'*')                 
                              WRITE(ERROR(3,NERROR),9912) RE,IORDR,     
     .                                          RE1,AC,IORDR1           
 9912                         FORMAT('*        Temp. Estimate (',       
     .                         F10.6,' E',I3,')  <  (',F10.6,'-',F8.6,  
     .                         ' ) E',I3,5X,'*')                        
                              NER2 = 1                                  
                          ENDIF                                         
                     ENDIF                                              
                     IF( TSI .GT. FDEVI ) THEN                          
                         IF( NER3 .EQ. 0 ) THEN                         
                             NERROR = NERROR + 1                        
                             WRITE(ERROR(1,NERROR),9920) NERROR,IT,FC   
 9920                        FORMAT('* (',I1,') Error of it-#',         
     .                              I3,' fluctuates more than',F4.1,    
     .                              '*average-sigma.',16X,'*')          
                             RE1 = TSI                                  
                             CALL BSORDR( RE1, FX2, ORDER, IORDR)       
                             RE1 = TSI/ORDER                            
                             AC  = FDEVI                                
                             CALL BSORDR( AC, FX2, ORDR1, IORDR1)       
                             AC  = AC/ORDR1                             
                             WRITE(ERROR(2,NERROR),9921) RE1,IORDR,     
     .                                         AC,IORDR1                
 9921                        FORMAT('*        Temp. Error (',           
     .                         F10.6,' E',I3,')  >  (',F10.6,           
     .                         ' E',I3,')',18X,'*')                     
                             WRITE(ERROR(3,NERROR),9902)                
                             NER3  = 1                                  
                         ENDIF                                          
                     ENDIF                                              
                 ENDIF                                                  
                 SUMTSI = SUMTSI + TSI                                  
                 SUMTI  = SUMTI  + TI                                   
                 AVTSI  = SUMTSI/FLOAT(IT)                              
                 AVTI   = SUMTI/FLOAT(IT)                               
                 FDEVI  = FC*AVTSI                                      
             ENDIF                                                      
         ENDIF                                                          
                                                                        
*------------------------------------------- for a parallel processor   
                                                                        
*                                  Broadcast                            
C                                  CALL BSCAST( NEND, 1 )               
                                                                        
*---------------------------------------------------------------------- 
*        Smoothing the Distribution D(I,J) and refine the grids         
*---------------------------------------------------------------------- 
                                                                        
         IF( ISTEP .LE. N0 ) THEN                                       
             IF( IT .EQ. ITMX ) NEND = N1                               
*           ---------------------                                       
             CALL BSETGV( NEND )                                        
*           ---------------------                                       
         ENDIF                                                          
*       ==========================                                      
         CALL BSUTIM( 0, ISTEP )                                        
*       ==========================                                      
                                                                        
         TIME (ITX,ISTEP)  = TIMINT                                     
         STIME             = TIMINT                                     
                                                                        
*---- Print the convergency behavior table in the interactive mode ---- 
         IF( INTV .GT. 1 ) THEN                                         
*            ---------------------------------                          
              CALL BSPRNT ( LU, 6, ISTEP, IDUM2 )                       
*            ---------------------------------                          
         ENDIF                                                          
                                                                        
         IF( NEND .EQ. N1 ) GO TO 600                                   
                                                                        
*       ======================                                          
         CALL BSUTIM( 0, 2 )                                            
*       ======================                                          
*======                                                                 
  500 CONTINUE                                                          
*======                                                                 
      IT    = IT - N1                                                   
      NEND  = N1                                                        
                                                                        
*********************************************************************** 
*                   Termination of BASES                                
*********************************************************************** 
*======                                                                 
  600 CONTINUE                                                          
*======                                                                 
*---------------------------------------------- For a parallel computer 
                                                                        
*                                 Global sum of histograms              
C                                 CALL BHSUM                            
*                                 Global sum of probabilities           
C                                 CALL BSDSUM(  DXD, NSP )              
*                                 Global sum of the max.value in each HC
C                                 CALL BSDSUM(  DXP, NSP )              
                                                                        
                                                                        
*======================= End of the step ? ============================ 
                                                                        
      IF( NEND .EQ. N1 ) THEN                                           
          IF( INTV .GT. 1 ) THEN                                        
*            ---------------------------------                          
              CALL BSPRNT ( LU, 7, IDUM1, IDUM2 )                       
*            ---------------------------------                          
          ENDIF                                                         
          IF( ISTEP .EQ. N0) THEN                                       
              JFLAG   = N1                                              
              ITG     = IT                                              
          ELSE                                                          
              JFLAG   = N0                                              
              ITF     = IT                                              
          ENDIF                                                         
      ENDIF                                                             
*    ======================                                             
       CALL BSUTIM( 0, 2 )                                              
*    ======================                                             
                                                                        
      RETURN                                                            
      END                                                               
*********************************************************************** 
*    ===================================                              * 
      SUBROUTINE BSLIST( LU, I, ISTEP )                                 
*    ===================================                              * 
* ((purpose))                                                         * 
*     Print out results of each iteration and cumulative result       * 
* ((Argument))                                                        * 
*  (Input)                                                            * 
*     LU      : Logical unit number for the printer                   * 
*     I       : Address in the arrays of common /BASE5/               * 
*     ISTEP   : The Set-Identifier                                    * 
*               ISTEP = ( 0 / 1 ) = ( Grid opt. / Integration step )  * 
*                                                                     * 
*     S. Kawabata   March '94                                         * 
*********************************************************************** 
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      PARAMETER (ITM = 50)                                              
      REAL*4 TIME, EFF, WRONG, TRSLT, TSTD, PCNT                        
      COMMON /BASE5/ ITRAT(ITM,0:1),TIME(ITM,0:2),EFF(ITM,0:1),         
     .               WRONG(ITM,0:1),RESLT(ITM,0:1),ACSTD(ITM,0:1),      
     .               TRSLT(ITM,0:1),TSTD(ITM,0:1),PCNT(ITM,0:1)         
                                                                        
      CALL BSTCNV( TIME(I,ISTEP), IH, MN, IS1, IS2 )                    
                                                                        
      RE  = RESLT(I,ISTEP)                                              
      AC  = ABS(ACSTD(I,ISTEP))                                         
      ARE = ABS(RE)                                                     
      IF( ARE .GE. AC) THEN                                             
          CALL BSORDR( ARE, F2, ORDER, IORDR)                           
      ELSE                                                              
          CALL BSORDR(  AC, F2, ORDER, IORDR )                          
      ENDIF                                                             
      RE  = RE/ORDER                                                    
      AC  = AC/ORDER                                                    
      IEFF = EFF(I,ISTEP)                                               
      WRITE(LU,9631) ITRAT(I,ISTEP),IEFF,WRONG(I,ISTEP),                
     .              TRSLT(I,ISTEP),TSTD(I,ISTEP),                       
     .              RE,AC,IORDR,PCNT(I,ISTEP),IH,MN,IS1,IS2             
 9631 FORMAT(I4,I4,F6.2,1P,E11.3, 0P,1X,F6.3,                           
     .              F10.6,'(+-',F8.6,')E',I3.2,1X,F6.3,                 
     .          1X,I3,':',I2,':',I2,'.',I2.2)                           
                                                                        
                                                                        
      RETURN                                                            
      END                                                               
C***********************************************************************
C*                                                                     *
C*=============================================                        *
C*    SUBROUTINE BSORDR( VAL, F2, ORDER, IORDR)                        *
C*=============================================                        *
C*((Function))                                                         *
C*    To resolve the real number VAL into mantester and exponent parts.*
C*  When VAL = 1230.0 is given, output are                             *
C*        F2 = 1.2  and ORDER = 4.0.                                   *
C*((Input))                                                            *
C*  VAL  : Real*8 value                                                *
C*((Output))                                                           *
C*  F2   : The upper two digits is given                               *
C*  ORDER: Order is given                                              *
C*  IORDR: Exponent is given                                           *
C*((Author))                                                           *
C*  S.Kawabata                                                         *
C*                                                                     *
C***********************************************************************
                                                                        
      SUBROUTINE BSORDR(VAL, F2, ORDER, IORDR)                          
      IMPLICIT REAL*8 (A-H,O-Z)                                         
                                                                        
      IF( VAL .NE. 0.0 ) THEN                                           
          ORDER    =  LOG10( VAL )                                      
          IORDR    =  INT( ORDER )                                      
          IF( ORDER .LT. 0.0D0 ) IORDR = IORDR - 1                      
          ORDER  = 10.D0**IORDR                                         
          F2     = VAL/ORDER                                            
      ELSE                                                              
          IORDR  = 0                                                    
          ORDER  = 1.0D0                                                
          F2    = 0.0D0                                                 
      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*    ================================================                  *
      SUBROUTINE BSPARM( MCALL, AC1, AC2, IT1, IT2 )                    
*    ================================================                  *
* ((Purpose))                                                          *
*     To set the BASES parameters.                                     *
* ((Input))                                                            *
*     MCALL  : The number of sample points per iteration.              *
*              This actual number is to be determined by taking the    *
*              number of dimensions into account.                      *
*     AC1 %  : The required accuracy at the grid optimization step     *
*     AC2 %  : The required accuracy at the integration step.          *
*     IT1    : The max. number of iteration at the grid opt. step.     *
*     IT2    : Thr max. number of iteration at the integration step.   *
* ((Output))                                                           *
*     These parameters are set in the labeled common /BPARM1/ and      *
*     /BPARM2/.                                                         
*                                                                      *
*        Coded by S.Kawabata         August '94                        *
*                                                                      *
************************************************************************
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      PARAMETER (MXDIM = 50, NDMX = 50 )                                
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                   
     .               IG(MXDIM),NCALL                                    
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2                             
                                                                        
      INTEGER MCALL, IT1, IT2                                           
      REAL*8 AC1, AC2                                                   
                                                                        
      NCALL = MCALL                                                     
      ACC1  = AC1                                                       
      ACC2  = AC2                                                       
      ITMX1 = IT1                                                       
      ITMX2 = IT2                                                       
                                                                        
      RETURN                                                            
      END                                                               
*********************************************************************** 
*    =======================================                          * 
      SUBROUTINE BSPRNT( LU, ID, IP1, IP2 )                             
*    =======================================                          * 
* ((purpose))                                                         * 
*     Print out routine of BASES.                                     * 
*  (Argument)                                                         * 
*     ID  : Identity number of printouts.                             * 
*     IP1... IP2 : Integer                                            * 
*  (Author)                                                           * 
*     S. Kawabata   May 1992                                          * 
*     Last update   March 1994                                        * 
*********************************************************************** 
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)                   
      COMMON /BASE0/ JFLAG,IBASES                                       
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                    
     .               IG(MXDIM),NCALL                                    
      COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2                              
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT                          
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),      
     .               ND,NG,NPG,MA(MXDIM)                                
      PARAMETER (ITM = 50)                                              
      REAL*4 TIME, EFF, WRONG, TRSLT, TSTD, PCNT                        
      COMMON /BASE5/ ITRAT(ITM,0:1),TIME(ITM,0:2),EFF(ITM,0:1),         
     .               WRONG(ITM,0:1),RESLT(ITM,0:1),ACSTD(ITM,0:1),      
     .               TRSLT(ITM,0:1),TSTD(ITM,0:1),PCNT(ITM,0:1)         
      REAL*4 STIME                                                      
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,IT1,ITF                        
      CHARACTER*51 ICH(0:1)                                             
      CHARACTER*1 CN                                                    
*        INTV = ( 0 / 1 / any ) = ( Batch / Batch(Unix) / Interactive ) 
*        IPNT = ( 0 / any ) = ( IBM Type / Ascii printer )              
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
*                                                                       
       COMMON /BDATE/ IDATE(3),ITIME(2)                                 
*            IDATE(1) : year        ITIME(1) : hour                     
*            IDATE(2) : month       ITIME(2) : minute                   
*            IDATE(3) : day                                             
      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1      
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1                  
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)                    
      REAL*4 XTIME                                                      
*                                                                       
       COMMON/NINFO/ NODEID, NUMNOD                                     
*                                                                       
      DATA  ICH / 'Convergency Behavior for the Grid Optimization Step',
     .            'Convergency Behavior for the Integration Step      '/
                                                                        
      IF( NODEID .NE. 0 ) RETURN                                        
      CN = CHAR(12)                                                     
                                                                        
      GO TO ( 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ), ID   
C----------------------------------------------------------- BSMAIN     
                                                                        
  100 IF( IPNT .EQ. 0 ) THEN                                            
          WRITE(LU,9600)                                                
 9600     FORMAT(/1H1,/1H )                                             
      ELSE                                                              
          WRITE(LU,9610) CN                                             
 9610     FORMAT(A1)                                                    
      ENDIF                                                             
      WRITE(LU,9620) (IDATE(I),I=1,3),(ITIME(J),J=1,2)                  
 9620 FORMAT(55X,'Date: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)            
      WRITE(LU,9050)                                                    
 9050 FORMAT(                                                           
     . 8X,'**********************************************************', 
     ./8X,'*                                                        *', 
     ./8X,'*     BBBBBBB     AAAA     SSSSSS   EEEEEE   SSSSSS      *', 
     ./8X,'*     BB    BB   AA  AA   SS    SS  EE      SS    SS     *', 
     ./8X,'*     BB    BB  AA    AA  SS        EE      SS           *', 
     ./8X,'*     BBBBBBB   AAAAAAAA   SSSSSS   EEEEEE   SSSSSS      *', 
     ./8X,'*     BB    BB  AA    AA        SS  EE            SS     *', 
     ./8X,'*     BB    BB  AA    AA  SS    SS  EE      SS    SS     *', 
     ./8X,'*     BBBB BB   AA    AA   SSSSSS   EEEEEE   SSSSSS      *', 
     ./8X,'*                                                        *', 
     ./8X,'*                   BASES Version 5.1                    *', 
     ./8X,'*           coded by S.Kawabata KEK, March 1994          *', 
     ./8X,'**********************************************************') 
                                                                        
          RETURN                                                        
C----------------------------------------------------------- BSMAIN     
                                                                        
  200     IF( IPNT .EQ. 0 ) THEN                                        
              WRITE(LU,9600)                                            
          ELSE                                                          
              WRITE(LU,9610) CN                                         
          ENDIF                                                         
          WRITE(LU,9300)                                                
 9300     FORMAT(20X,                                                   
     .         '****** END OF BASES *********')                         
                                                                        
C----------------------------------------------------------- BSMAIN     
                                                                        
  300 CONTINUE                                                          
      WRITE(LU,9305)                                                    
 9305 FORMAT(                                                           
     .//5X,'<<   Computing Time Information   >>')                      
                                                                        
*     WRITE(LU,9310) (IDATE(I),I=1,3),(ITIME(J),J=1,2)                  
*9310 FORMAT(/15X,'Start at: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)       
*     CALL BSDATE                                                       
*     WRITE(LU,9320) (IDATE(I),I=1,3),(ITIME(J),J=1,2)                  
*9320 FORMAT(15X,'End   at: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)        
      WRITE(LU,9330)                                                    
 9330 FORMAT(/15X,'(1) For BASES              H: M:  Sec')              
      CALL BSTCNV(TIMEBS(2),IH,MN,IS1,IS2)                              
      WRITE(LU,9340) IH, MN, IS1, IS2                                   
 9340 FORMAT(19X,'Overhead           : ',I3,':',I2,':',I2,'.',I2.2)     
      CALL BSTCNV(TIMEBS(0),IH,MN,IS1,IS2)                              
      WRITE(LU,9350) IH, MN, IS1, IS2                                   
 9350 FORMAT(19X,'Grid Optim. Step   : ',I3,':',I2,':',I2,'.',I2.2)     
      CALL BSTCNV(TIMEBS(1),IH,MN,IS1,IS2)                              
      WRITE(LU,9360) IH, MN, IS1, IS2                                   
 9360 FORMAT(19X,'Integration Step   : ',I3,':',I2,':',I2,'.',I2.2)     
      XTIME = TIMEB2 - TIMEB1                                           
      CALL BSTCNV(XTIME,IH,MN,IS1,IS2)                                  
      WRITE(LU,9365) IH, MN, IS1, IS2                                   
 9365 FORMAT(19X,'Go time for all    : ',I3,':',I2,':',I2,'.',I2.2)     
      EXTIM  = TIMEBS(1)*1000.0/SCALLS/0.7                              
      WRITE(LU,9375)                                                    
 9375 FORMAT(/15X,'(2) Expected event generation time')                 
      WRITE(LU,9376) EXTIM                                              
 9376 FORMAT(19X,'Expected time for 1000 events :',F10.2,' Sec')        
      RETURN                                                            
                                                                        
C----------------------------------------------------------- BASES      
                                                                        
  400 NSP   = NG**NWILD                                                 
      MCALL = NSP*NPG                                                   
      WRITE(LU,9400) NDIM,NWILD,MCALL,NCALL,ND,NG,NSP                   
 9400 FORMAT(                                                           
     .//5X,'<<   Parameters for BASES    >>',                           
     .//5X,' (1) Dimensions of integration etc.',                       
     . /5X,'     # of dimensions :    Ndim    =',I9,3X,'( 50 at max.)', 
     . /5X,'     # of Wilds      :    Nwild   =',I9,3X,'( 15 at max.)', 
     . /5X,'     # of sample points : Ncall   =',I9,'(real)',           
     .                                         I9,'(given)',            
     . /5X,'     # of subregions    : Ng      =',I9,' / variable',      
     . /5X,'     # of regions       : Nregion =',I9,' / variable',      
     . /5X,'     # of Hypercubes    : Ncube   =',I9,                    
     .//5X,' (2) About the integration variables')                      
      WRITE(LU,9405)                                                    
 9405 FORMAT(10X,'------',2('+---------------'),'+-------+-------')     
      WRITE(LU,9410)                                                    
 9410 FORMAT(10X,'    i       XL(i)           XU(i)     ',              
     .           '  IG(i)   Wild')                                      
      WRITE(LU,9405)                                                    
       DO 450 I = 1,NDIM                                                
          IF( I .LE. NWILD ) THEN                                       
          WRITE(LU,9420) I,XL(I),XU(I),IG(I)                            
 9420     FORMAT(10X,I5,1P,2('  ',E14.6),'  ',3X,0P,I1,3X,              
     .                       '   yes')                                  
          ELSE                                                          
          WRITE(LU,9421) I,XL(I),XU(I),IG(I)                            
 9421     FORMAT(10X,I5,1P,2('  ',E14.6),'  ',3X,0P,I1,3X,              
     .                        '    no')                                 
          ENDIF                                                         
  450  CONTINUE                                                         
       WRITE(LU,9405)                                                   
       WRITE(LU,9450) ITMX1,ACC1,ITMX2,ACC2                             
 9450  FORMAT(                                                          
     . /5X,' (3) Parameters for the grid optimization step',            
     . /5X,'     Max.# of iterations: ITMX1 =',I9,                      
     . /5X,'     Expected accuracy  : Acc1  =',F9.4,' %',               
     .//5X,' (4) Parameters for the integration step',                  
     . /5X,'     Max.# of iterations: ITMX2 =',I9,                      
     . /5X,'     Expected accuracy  : Acc2  =',F9.4,' %')               
                                                                        
          RETURN                                                        
C----------------------------------------------------------- BASES      
                                                                        
  500    IF( INTV .LE. 1 )    RETURN                                    
         ISTEP  = IP1                                                   
         IF( IPNT .EQ. 0 ) THEN                                         
             WRITE(LU,9600)                                             
         ELSE                                                           
             WRITE(LU,9610) CN                                          
         ENDIF                                                          
         WRITE(LU,9620) (IDATE(I),I=1,3),(ITIME(J),J=1,2)               
         WRITE(LU,9500) ICH(ISTEP)                                      
 9500    FORMAT(15X,A)                                                  
         WRITE(LU,9570)                                                 
         WRITE(LU,9550)                                                 
 9550    FORMAT(1X,'<- Result of  each iteration ->',                   
     .          2X,'<-     Cumulative Result     ->',                   
     .          1X,'< CPU  time >',                                     
     .         /1X,' IT Eff R_Neg   Estimate  Acc %',                   
     .          2X,'Estimate(+- Error )order  Acc %',                   
     .          1X,'( H: M: Sec )')                                     
         WRITE(LU,9570)                                                 
 9570    FORMAT(1X,7('----------'),'--------')                          
         RETURN                                                         
                                                                        
C----------------------------------------------------------- BASES      
                                                                        
  600    IF( INTV .LE. 1 ) RETURN                                       
         ISTEP  = IP1                                                   
         ITX = MOD( IT, ITM)                                            
         IF( ITX .EQ. 0 ) ITX = ITM                                     
                                                                        
         CALL BSLIST( LU, ITX, ISTEP )                                  
                                                                        
         RETURN                                                         
                                                                        
  700    IF( INTV .LE. 1 ) RETURN                                       
         WRITE(LU,9570)                                                 
                                                                        
         RETURN                                                         
C----------------------------------------------------------- BASES      
                                                                        
  800    ITJ    = IP1                                                   
         ISTEP  = IP2                                                   
         ITX  = MOD( ITJ, ITM )                                         
         IF( ITX .EQ. 0 ) ITX = ITM                                     
                                                                        
         IF( ITRAT(1,ISTEP) .EQ. 1 ) THEN                               
             NDEV   = 1                                                 
         ELSE                                                           
             NDEV   = 2                                                 
             ITFN   = ITM                                               
             ITMN   = 10000                                             
             DO 610 I = 1,ITM                                           
                IF( ITRAT(I,ISTEP) .LT. ITMN ) THEN                     
                    ITST = I                                            
                    ITMN = ITRAT(I,ISTEP)                               
                ENDIF                                                   
  610        CONTINUE                                                   
             IF( ITST .EQ. 1 ) NDEV = 1                                 
         ENDIF                                                          
                                                                        
         IF( IPNT .EQ. 0 ) THEN                                         
             WRITE(LU,9600)                                             
         ELSE                                                           
             WRITE(LU,9610) CN                                          
         ENDIF                                                          
         WRITE(LU,9620) (IDATE(I),I=1,3),(ITIME(J),J=1,2)               
         WRITE(LU,9500) ICH(ISTEP)                                      
         WRITE(LU,9570)                                                 
         WRITE(LU,9550)                                                 
         WRITE(LU,9570)                                                 
                                                                        
  625    IF( NDEV .EQ. 1 ) THEN                                         
             ITST = 1                                                   
             ITFN = ITX                                                 
         ENDIF                                                          
                                                                        
         DO 650 I = ITST, ITFN                                          
                                                                        
            CALL BSLIST( LU, I, ISTEP )                                 
                                                                        
  650    CONTINUE                                                       
         NDEV  = NDEV - 1                                               
         IF( NDEV .GT. 0 ) GO TO 625                                    
         WRITE(LU,9570)                                                 
                                                                        
      RETURN                                                            
                                                                        
C----------------------------------------------------------- BASES      
                                                                        
  900 WRITE(LU,9950)                                                    
 9950 FORMAT(1X,'******** FATAL ERROR IN BASES **************',         
     .      /1X,'There are no enough good points in this iteration.',   
     .      /1X,'Process was terminated due to this error.')            
                                                                        
      RETURN                                                            
                                                                        
C-----------------------------------------------------------------      
 1000 LOOP = IP1                                                        
      IF( IP2 .NE. 0 ) THEN                                             
          IF( IPNT .EQ. 0 ) THEN                                        
              WRITE(LU,9600)                                            
           ELSE                                                         
              WRITE(LU,9610) CN                                         
           ENDIF                                                        
           WRITE(LU,9620) (IDATE(I),I=1,3),(ITIME(J),J=1,2)             
           WRITE(LU,9650)                                               
 9650      FORMAT(                                                      
     .      20X,'Results of Integration',                               
     .     /10X,5('----------'),'------',                               
     .     /10X,' Loop#  Estimate(+- Error )order',                     
     .                     '  It1  It2 ( H: M: Sec )',                  
     .     /10X,5('----------'),'------')                               
      ENDIF                                                             
                                                                        
      RE  = AVGI                                                        
      AC  = ABS(SD)                                                     
      ARE = ABS(RE)                                                     
      IF( ARE .GE. AC) THEN                                             
          CALL BSORDR( ARE, F2, ORDER, IORDR)                           
      ELSE                                                              
          CALL BSORDR(  AC, F2, ORDER, IORDR )                          
      ENDIF                                                             
      RE  = RE/ORDER                                                    
      AC  = AC/ORDER                                                    
      CALL BSTCNV( STIME, IH, MN, IS1, IS2)                             
      WRITE(LU,9660) LOOP,RE,AC,IORDR,IT1,IT,IH,MN,IS1,IS2              
 9660 FORMAT(10X,I6,F10.6,'(+-',F8.6,')E',I3.2,2I5,                     
     .        1X,I3,':',I2,':',I2,'.',I2.2,                             
     .      /10X,5('----------'),'------')                              
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*=================================================                      
      SUBROUTINE BSTCNV( TIME, IH, MN, IS1, IS2 )                       
*=================================================                      
* (Purpose)                                                             
*    Resolve TIME in second into IH, MN, IS1, IS2                       
* (Input)                                                               
*    TIME : in the unit of second                                       
* (Output)                                                              
*    IH   : Hours                                                       
*    MN   : Minute                                                      
*    IS1  : Second                                                      
*    IS2  : 0.xx Second                                                 
* (Author)                                                              
*    S.Kawabata 1992 June 15                                            
************************************************************************
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      REAL*4 TIME                                                       
      INTEGER  HOUR                                                     
      DATA HOUR, MINUT, N100/ 360000, 6000, 100 /                       
                                                                        
      ISEC  = TIME*N100                                                 
      IH    = 0                                                         
      MN    = IH                                                        
      IF( ISEC .GE. MINUT ) THEN                                        
          ITIME = ISEC                                                  
          IF( ISEC .GE. HOUR ) THEN                                     
              IH    = ITIME/HOUR                                        
              IHX   = IH*HOUR                                           
              ITIME = ITIME - IHX                                       
              ISEC  = ISEC - IHX                                        
          ENDIF                                                         
          MN    = ITIME/MINUT                                           
          ISEC  = ISEC - MN*MINUT                                       
      ENDIF                                                             
      IS1  = ISEC/N100                                                  
      IS2  = MOD( ISEC, N100)                                           
                                                                        
      RETURN                                                            
      END                                                               
*CMZ :          24/06/94  10.51.47  by  Unknown                         
*-- Author :                                                            
C                                                                       
C***********************************************************************
C*=================================                                    *
C* SUBROUTINE BSTIME( TIME, IFLG )                                     *
C*=================================                                    *
C*((Purpose))                                                          *
C*        Interface routine to get used CPU time from FORTRAN          *
C*        Library routine CLOCK etc.                                   *
C*((Input))                                                            *
C*        IFLG  : Flag                                                 *
C*          IFLG = 0 : Initialization of clock routine.                *
C*          IFLG = 1 : Get used CPU time.                              *
C*((Output))                                                           *
C*        TIME  : Used CPU time in second.                             *
C*                                                                     *
C*       Coded by S.Kawabata        Oct. '85                           *
C*                                                                     *
C***********************************************************************
C                                                                       
      SUBROUTINE BSTIME( TIME, IFLG )                                   
C                                                                       
*     save time_init                                                    
C                                                                       
                                                                        
      IF( IFLG .NE. 0 ) THEN                                            
C                                                                       
C         iutime.c should be compiled.                                  
C                                                                       
*         TIME = uxtime() - time_init                                   
          TIME=0
#ifdef CERN_X
          CALL TIMEX(TIME)                                              
#endif
C                                                                       
      ELSE                                                              
                                                                        
*         time_init = uxtime()                                          
#ifdef CERN_X
          CALL TIMEST(999.)                                             
#endif
          TIME      = 0.0                                               
                                                                        
      ENDIF                                                             
C                                                                       
      RETURN                                                            
      END   

      SUBROUTINE BSUTIM( JOB, ID )                                      
                                                                        
C     COMMON/NINFO/ NODEID, NUMNOD                                      
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1                  
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)                    
                                                                        
*  Prior to call thisroutine, BSTIME( TIME0, 1 ) should be called       
*  for initialize the time offset TIME0.                                
*                                                                       
*     print *,'bsutim .. job, id ',job,id                               
      CALL BSTIME( RTIME, 1)                                            
      DTIME      = RTIME - TIME0                                        
                                                                        
      IF( JOB .EQ. 0 ) THEN                                             
*       For BASES computing time                                        
*         ID  = 0  : Grid defining step                                 
*               1  : Integration step                                   
*               2  : Others                                             
                                                                        
          TIMEBS(ID) = TIMEBS(ID) + DTIME                               
                                                                        
          IF( ID .LE. 1 ) THEN                                          
              TIMINT = TIMINT + DTIME                                   
          ENDIF                                                         
      ELSE                                                              
*       For SPRING computing time                                       
*         ID  = 0  : Event generation                                   
*               1  : Overhead                                           
*               2  : Others                                             
                                                                        
          TIMESP(ID) = TIMESP(ID) + DTIME                               
                                                                        
      ENDIF                                                             
                                                                        
      TIME0      = RTIME                                                
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*    =======================================                           *
       SUBROUTINE DHFILL( ID, DX, DY, FX )                              
*    =======================================                           *
* ((Function))                                                         *
*     To fill scatter plot                                             *
*   This routine identifies the bin number which is to be updated      *
*   with weight FX*WGT.  Up to five points per plot are able to        *
*   be stacked before calling BHUPDT or SHUPDT.                        *
* ((Input))                                                            *
*   ID    : Histogram identification number                            *
*   DX    : Input x value                                              *
*   DY    : Input y value                                              *
*   FX    : Input value of the function                                *
* ((Author))                                                           *
*   S.Kawabata         June '90 at KEK                                 *
*                                                                      *
************************************************************************
                                                                        
      REAL*8 DX, DY, FX                                                 
      COMMON /BASE0/ IFLAG,IBASES                                       
      REAL*8 SCALLS,WGT,TI,TSI,TACC                                     
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT                          
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
C     COMMON /PLOTLU/ LU                                                
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
*======================================================================*
*               Find the scatter plot ID in the table                  *
*======================================================================*
*                                                                      *
      IF( NSCAT .GT. 0 ) THEN                                           
          I  = IABS(MOD( ID, 13 )) + 1                                  
          IF( DHASH(1, I) .EQ. 1 ) THEN                                 
            IF( ID .EQ. MAPD( 1, DHASH(2,I))) THEN                      
                ISCAT = DHASH(2,I)                                      
                GO TO 200                                               
            ENDIF                                                       
          ELSEIF( DHASH(1, I) .GT. 1 ) THEN                             
            DO 100 K = 2, DHASH(1,I)+1                                  
               IF( ID .EQ. MAPD( 1, DHASH(K,I))) THEN                   
                   ISCAT = DHASH(K,I)                                   
                   GO TO 200                                            
               ENDIF                                                    
  100       CONTINUE                                                    
          ENDIF                                                         
      ENDIF                                                             
C     IF( LU .GT. 0 ) THEN                                              
C         WRITE(LU,9000) ID                                             
C9000     FORMAT(1X,'No Scat_Plot corresponds to ID =',I5,              
C    .          /1X,' This call is neglected ]]]')                      
C     ENDIF                                                             
      RETURN                                                            
                                                                        
*                                                                      *
*======================================================================*
*               Determine the bin numbers for x and y                  *
*======================================================================*
*                                                                      *
  200 X     = DX*1.0                                                    
      Y     = DY*1.0                                                    
                                                                        
          IP1   = MAPD(2,ISCAT)                                         
          XMIN  = BUFF(IP1)                                             
          XMAX  = BUFF(IP1+1)                                           
          MXBIN = IBUF(IP1+2)                                           
          DEV   = BUFF(IP1+3)                                           
          IX    =   0                                                   
          IY    =   0                                                   
          IF( X .GE. XMIN .AND. X .LE. XMAX ) THEN                      
              IX   = INT( (X - XMIN)/DEV+ 1.0 )                         
              IF( IX .GT. MXBIN ) IX =   0                              
          ENDIF                                                         
C                                                                       
          IF( IX .GT. 0 ) THEN                                          
              YMIN  = BUFF(IP1+4)                                       
              YMAX  = BUFF(IP1+5)                                       
              MYBIN = IBUF(IP1+6)                                       
              DEV   = BUFF(IP1+7)                                       
              IF( Y .GE. YMIN .AND. Y .LE. YMAX ) THEN                  
                  IY   = INT((Y - YMIN)/DEV + 1.0)                      
                 IF( IY .GT. MYBIN ) THEN                               
                     IX  =  0                                           
                     IY  =  0                                           
                 ENDIF                                                  
              ENDIF                                                     
          ENDIF                                                         
*                                                                      *
*======================================================================*
*               Fill the scatter plot ID                               *
*======================================================================*
*----------------------------------------------------------------------*
*               For BASES                                              *
*----------------------------------------------------------------------*
*                                                                      *
      IF( IBASES .EQ. 1 ) THEN                                          
          IF( IY .GT. 0 ) THEN                                          
                                                                        
              IP2       = MAPD(3,ISCAT)                                 
              IBUF(IP2) = SCALLS                                        
              IP2       = IX + MXBIN*(IY - 1) + IP2                     
              BUFF(IP2) = BUFF(IP2) + FX*WGT                            
                                                                        
          ENDIF                                                         
                                                                        
*----------------------------------------------------------------------*
*               For SPRING                                             *
*----------------------------------------------------------------------*
*                                                                      *
      ELSE                                                              
                                                                        
          IP3         = MAPD(4,ISCAT)                                   
          IBUF(IP3)   = IX                                              
          IBUF(IP3+1) = IY                                              
                                                                        
      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*  =================================================================== *
      SUBROUTINE DHINIT(ID,DXMIN,DXMAX,NXBIN,DYMIN,DYMAX,NYBIN,TNAME)   
*  =================================================================== *
* ((Function))                                                         *
*     To define a scatter plot                                         *
* ((Input))                                                            *
*    ID   : scatter plot identification number                         *
*    DXMIN: Lower limit of X for the scatter plot                      *
*    DXMAX: Upper limit of X for the scatter plot                      *
*    NXBIN: Number of bins of X for the plot (Max. is 50 )             *
*    DYMIN: Lower limit of Y for the scatter plot                      *
*    DYMAX: Upper limit of Y for the scatter plot                      *
*    NYBIN: Number of bins of Y for the plot (Max. is 50 )             *
*    TNAME: Title of the plot in the character string (upto 64         *
*            characters)                                               *
* ((Author))                                                           *
*    S.Kawabata     June '90 at KEK                                    *
*                                                                      *
************************************************************************
                                                                        
      REAL*8 DXMIN,DXMAX,DYMIN,DYMAX                                    
      CHARACTER*(*) TNAME                                               
      CHARACTER*64 NAME                                                 
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
*     COMMON/XHCNTL/ LOCK                                               
      COMMON/PLOTLU/ LU                                                 
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
*======================================================================*
*               Find the scatter plot ID in the table                  *
*======================================================================*
*                                                                      *
      IF( NSCAT .GE. NSC ) THEN                                         
*         IF( LOCK .NE. 0 ) RETURN                                      
          IF( LU .GT. 0 ) THEN                                          
            WRITE(LU,9000) NSCAT,ID                                     
 9000       FORMAT(1X,'Numberof Scat_plots exceeds ',I3,' at ID = ',I3, 
     .            /1X,'This call is neglected.')                        
          ENDIF                                                         
          RETURN                                                        
      ENDIF                                                             
                                                                        
      IEXIST = 0                                                        
      I  = IABS(MOD( ID, 13 )) + 1                                      
      NS     = DHASH(1, I)                                              
                                                                        
      IF( NS .EQ. 1 ) THEN                                              
            IF( ID .EQ. MAPD( 1, DHASH(2,I))) THEN                      
*               IF( LOCK .NE. 0 ) RETURN                                
                IEXIST = DHASH(2,I)                                     
            ENDIF                                                       
      ELSEIF( NS .GT. 1 ) THEN                                          
          DO 100 K = 2, DHASH(1,I)+1                                    
            IF( ID .EQ. MAPD( 1, DHASH(K,I))) THEN                      
*               IF( LOCK .NE. 0 ) RETURN                                
                IEXIST = DHASH(K,I)                                     
                GO TO 110                                               
            ENDIF                                                       
  100    CONTINUE                                                       
  110    CONTINUE                                                       
      ENDIF                                                             
      XMIN  = DXMIN*1.0                                                 
      XMAX  = DXMAX*1.0                                                 
      YMIN  = DYMIN*1.0                                                 
      YMAX  = DYMAX*1.0                                                 
                                                                        
      IF( IEXIST .GT. 0 ) THEN                                          
          IF( LU .GT. 0 ) THEN                                          
            WRITE(LU,9100) ID                                           
          ENDIF                                                         
 9100     FORMAT(1X,'Scat_Plot ID (',I3,' ) exists already.')           
          IP1    =  MAPD(2,IEXIST)                                      
          IF(( XMIN .EQ. BUFF(IP1))   .AND.                             
     .       ( XMAX .EQ. BUFF(IP1+1)) .AND.                             
     .       ( NXBIN .EQ. IBUF(IP1+2)) )    THEN                        
             IF(( YMIN .EQ. BUFF(IP1+4))   .AND.                        
     .          ( YMAX .EQ. BUFF(IP1+5)) .AND.                          
     .          ( NYBIN .EQ. IBUF(IP1+6)) )    THEN                     
                  IF( LU .GT. 0 ) THEN                                  
                      WRITE(LU,9110)                                    
                  ENDIF                                                 
 9110             FORMAT(1X,' This call is neglected.')                 
                  RETURN                                                
             ENDIF                                                      
          ENDIF                                                         
          IF( LU .GT. 0 ) THEN                                          
              WRITE(LU,9120) ID,XMIN,XMAX,NXBIN,YMIN,YMAX,NYBIN         
          ENDIF                                                         
 9120     FORMAT(1X,'Scat_Plot ( ID =',I3,' ) parameters are replaced', 
     .          /1X,'by the following new parameters :',                
     .          /1X,' XMIN(',E12.5,')  XMAX(',E12.5,' )  XBIN(',I4,' )',
     .          /1X,' YMIN(',E12.5,')  YMAX(',E12.5,' )  YBIN(',I4,' )')
      ENDIF                                                             
      IF(NXBIN .GT. 50 .OR. NYBIN .GT. 50 ) THEN                        
         IF( LU .GT. 0 ) THEN                                           
             WRITE(LU,9300) NXBIN,NYBIN,ID                              
         ENDIF                                                          
 9300    FORMAT(1X,'Bin size (',2I3,' )  exceeds 50 at ID =',I5,        
     .         /1X,' This call is neglected .')                         
         RETURN                                                         
      ELSEIF((XMIN .GE. XMAX) .OR. (YMIN .GE. YMAX)) THEN               
         IF( LU .GT. 0 ) THEN                                           
             WRITE(LU,9400) ID,XMIN,XMAX,YMIN,YMAX                      
         ENDIF                                                          
 9400    FORMAT(1X,'Lower limit is larger than upper at SC_PL ID =',I5, 
     .         /1X,' This call is neglected .',                         
     .         /1X,' XMIN =',G13.4,' XMAX =',G13.4,                     
     .         /1X,' YMIN =',G13.4,' YMAX =',G13.4)                     
         RETURN                                                         
      ENDIF                                                             
      IF(DHASH(1,I) .GE. NSC ) THEN                                     
         IF( LU .GT. 0 ) THEN                                           
             WRITE(LU,9500) I                                           
         ENDIF                                                          
 9500    FORMAT(1X,I5,'-th Hash table overflow',                        
     .         /1X,' This call is neglected.')                          
         RETURN                                                         
      ENDIF                                                             
                                                                        
      IF( IEXIST .GT. 0 ) THEN                                          
          NSCT     = IEXIST                                             
      ELSE                                                              
          NSCAT        = NSCAT + 1                                      
          DHASH(1,I)   = DHASH(1,I) + 1                                 
          K            = DHASH(1,I) + 1                                 
          DHASH(K,I)   = NSCAT                                          
          NSCT         = NSCAT                                          
          IP1    = NW + 1                                               
          NW  = NW + 2527                                               
          MAPD(1,NSCT)  = ID                                            
          MAPD(2,NSCT)  = IP1                                           
      ENDIF                                                             
                                                                        
         BUFF(IP1     ) = XMIN                                          
         BUFF(IP1 +  1) = XMAX                                          
         IBUF(IP1 +  2) = NXBIN                                         
         DEV            = XMAX - XMIN                                   
         BUFF(IP1 +  3) = DEV/NXBIN                                     
         BUFF(IP1 +  4) = YMIN                                          
         BUFF(IP1 +  5) = YMAX                                          
         IBUF(IP1 +  6) = NYBIN                                         
         DEV            = YMAX - YMIN                                   
         BUFF(IP1 +  7) = DEV/NYBIN                                     
      IP2   = IP1 + 8                                                   
         MAPD(3,NSCT)  = IP2                                            
         IBUF(IP2     ) = 0                                             
      IP3   = IP1 + 2509                                                
         MAPD(4,NSCT)  = IP3                                            
         IBUF(IP3     ) =  0                                            
         IBUF(IP3 +  1) =  0                                            
                                                                        
         I1   = IP3 + 2                                                 
         I2   = I1 + 15                                                 
         NAME = TNAME                                                   
         READ(NAME,9800) (BUFF(I),I=I1,I2)                              
 9800    FORMAT(16A4)                                                   
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*     =========================                                        *
       SUBROUTINE DHPLOT( LU )                                          
*     =========================                                        *
* ((Purpose))                                                          *
*      To print scatter plots for BASES and SPRING                     *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata    June '90                                         *
*                                                                      *
************************************************************************
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
                                                                        
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
                                                                        
      CHARACTER*1  PLUS,MINUS,BLNK,STAR,NUM(0:9),NEG(0:9),SHARP,PNT     
      REAL*4       X(50)                                                
      CHARACTER*1 CHARR(50), CN                                         
      CHARACTER*80 FORM1,FORM                                           
      DATA  PLUS /'+'/, MINUS /'-'/, BLNK /' '/, STAR /'*'/             
      DATA  SHARP /'#'/,  PNT /'.'/                                     
      DATA  NUM  / '0','1','2','3','4','5','6','7','8','9'/             
      DATA  NEG  / '-','a','b','c','d','e','f','g','h','i'/             
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
      CN   = CHAR(12)                                                   
                                                                        
      IF( NSCAT .GT. 0 ) THEN                                           
         DO 900 ISCAT = 1, NSCAT                                        
            IP3   = MAPD(4,ISCAT)                                       
            IF( IPNT .EQ. 0 ) THEN                                      
                WRITE(LU,9010)                                          
            ELSE                                                        
                WRITE(LU,9020) CN                                       
            ENDIF                                                       
 9010       FORMAT(/1H1)                                                
 9020       FORMAT(A1)                                                  
            WRITE(LU,9100) MAPD(1,ISCAT),(BUFF(I), I=IP3+2,IP3+17)      
 9100       FORMAT(/5X,'Scat_Plot (ID =',I3,' ) for ',16A4,/)           
                                                                        
            IP1   = MAPD(2,ISCAT)                                       
            XL    = BUFF(IP1)                                           
            XU    = BUFF(IP1+1)                                         
            NX    = IBUF(IP1+2)                                         
            DX    = BUFF(IP1+3)                                         
            XM    = ABS(XU)                                             
            XX    = ABS(XL)                                             
            IF( XX .GT. XM ) XM = XX                                    
            CALL XHORDR( XU, FX, XORD, IXORD)                           
            YL    = BUFF(IP1+4)                                         
            YU    = BUFF(IP1+5)                                         
            NY    = IBUF(IP1+6)                                         
            DY    = BUFF(IP1+7)                                         
            MIDY  = NY/2                                                
            IF( MIDY .EQ. 0 ) MIDY = 1                                  
            YM    = ABS(YU)                                             
            YY    = ABS(YL)                                             
            IF( YY .GT. YM ) YM = YY                                    
            CALL XHORDR( YM, FY, YORD, IYORD)                           
            IP2   = MAPD(3,ISCAT)                                       
            NTOTAL= IBUF(IP2)                                           
            VMAX  = BUFF(IP2+1)                                         
            VMIN  = VMAX                                                
            DO 100 J = 0,NY-1                                           
               IB    = NX*J + IP2                                       
               DO 100 I = 1,NX                                          
                  VLS    = BUFF( I + IB )                               
                  IF( VLS .GT. VMAX ) VMAX = VLS                        
                  IF( VLS .LT. VMIN ) VMIN = VLS                        
  100       CONTINUE                                                    
***                                                                     
            IF( VMAX .EQ. 0.0 .AND. VMIN .EQ. 0.0 ) THEN                
                VMAX  = 10.0                                            
                VMIN  = 0.0                                             
            ENDIF                                                       
***                                                                     
            IF( VMAX .GT. -VMIN ) THEN                                  
                UNIT = ABS(VMAX)/11.0                                   
            ELSE                                                        
                UNIT = ABS(VMIN)/11.0                                   
            ENDIF                                                       
            WRITE(FORM1,9200) NX                                        
*9200       FORMAT('(7X,''E'',I3,3X,''+'',',I2,'(''--''),''-+'')')      
 9200       FORMAT('(7X,''E'',I3,3X,''+'',',I2,'(''-''),''+'')')        
            WRITE(LU,FORM1) IYORD                                       
            DO 300 L = NY-1,0,-1                                        
               IB     = NX*L + IP2                                      
               DO 200 I = 1,NX                                          
                 XNUM   = BUFF( I + IB )/UNIT                           
                 IF( XNUM .LT. 0 0 ) THEN                               
                     NUMB   = XNUM - 1.0                                
                     IF(     NUMB .GE. -1 )THEN                         
                             CHARR(I) = MINUS                           
                     ELSEIF( NUMB .GE. -10 ) THEN                       
                            CHARR(I) = NEG(-NUMB-1)                     
                     ELSE                                               
                            CHARR(I) = SHARP                            
                     ENDIF                                              
                 ELSE                                                   
                     NUMB   = XNUM + 1.0                                
                     IF(     XNUM .EQ. 0.0 ) THEN                       
                             CHARR(I) = BLNK                            
                     ELSEIF( NUMB .LE.  1 ) THEN                        
                             CHARR(I) = PLUS                            
                             IF( VMIN .GE. 0.0 ) CHARR(I) = PNT         
                     ELSEIF( NUMB .LE. 10 ) THEN                        
                             CHARR(I) = NUM(NUMB-1)                     
                     ELSE                                               
                             CHARR(I) = STAR                            
                     ENDIF                                              
                 ENDIF                                                  
  200          CONTINUE                                                 
                                                                        
               Y   = (L*DY + YL)/YORD                                   
               IF( L .EQ. MIDY ) THEN                                   
                   WRITE(FORM,9300) NX                                  
*9300              FORMAT('(5X,F6.3,'' Y I'',',I2,'(1X,A1),'' I'')')    
 9300              FORMAT('(5X,F6.3,'' Y I'',',I2,'A1,''I'')')          
               ELSE                                                     
                   WRITE(FORM,9310) NX                                  
*9310              FORMAT('(5X,F6.3,''   I'',',I2,'(1X,A1),'' I'')')    
 9310              FORMAT('(5X,F6.3,''   I'',',I2,'A1,''I'')')          
               ENDIF                                                    
               WRITE(LU,FORM) Y,(CHARR(M),M=1,NX)                       
                                                                        
  300       CONTINUE                                                    
                                                                        
            WRITE(LU,FORM1) IYORD                                       
                                                                        
            NXH   = NX/2                                                
            IF( NXH .EQ. 0 ) NXH = 1                                    
            WRITE(FORM,9400) NXH                                        
                                                                        
*           WRITE(FORM,9400) NX                                         
 9400       FORMAT('(6X,''Low-'',5X,',I2,'X,''X'')')                    
            WRITE(LU,FORM)                                              
                                                                        
            XORD     = XORD*10.                                         
            DO 400 I = 1, NX                                            
               X(I)  = ((I-1)*DX + XL)/XORD                             
               IF( X(I) .LT. 0.0 ) THEN                                 
                   CHARR(I)  = MINUS                                    
                   X(I)      = -X(I)                                    
               ELSE                                                     
                   CHARR(I)  = BLNK                                     
               ENDIF                                                    
  400       CONTINUE                                                    
            WRITE(FORM1,9500) NX                                        
*9500       FORMAT('(6X,''Edge'',5X,',I2,'(1X,A1))')                    
 9500       FORMAT('(6X,''Edge'',5X,',I2,'A1)')                         
            WRITE(LU,FORM1) (CHARR(M),M=1,NX)                           
                                                                        
            XORD      = 1.0                                             
            DO 600 I  = 1,5                                             
               IF( I .EQ. 2 ) THEN                                      
                   WRITE(FORM,9602) NX                                  
 9602              FORMAT('(7X,''E'',I3,4X',I2,                         
     .                    '(''.''))')                                   
                   WRITE(LU,FORM) IXORD                                 
               ELSE                                                     
                   DO 500 J = 1, NX                                     
                      XX        = X(J)*10.0                             
                      NUMB      = XX                                    
                      CHARR(J)  = NUM(NUMB)                             
                      X(J)      = XX - FLOAT(NUMB)                      
  500              CONTINUE                                             
                   IF(     I .EQ. 4 ) THEN                              
                           WRITE(FORM,9604) NX                          
 9604                      FORMAT('(7X,''Low-'',4X,',I2,                
     .                            'A1)')                                
                   ELSEIF( I .EQ. 5 ) THEN                              
                           WRITE(FORM,9605) NX                          
 9605                      FORMAT('(7X,''Edge'',4X,',I2,                
     .                            'A1)')                                
                   ELSE                                                 
                           WRITE(FORM,9601) NX                          
 9601                      FORMAT('(15X,',I2,                           
     .                            'A1)')                                
                   ENDIF                                                
                   WRITE(LU,FORM) (CHARR(M),M=1,NX)                     
               ENDIF                                                    
  600       CONTINUE                                                    
                                                                        
  900    CONTINUE                                                       
      ENDIF                                                             
C                                                                       
      RETURN                                                            
      END                                                               
C********************************************************************** 
C*======================                                              * 
C* FUNCTION DRN( ISEED)                                               * 
C*======================                                              * 
C*  Machine-independent Random number generator                       * 
C*     General purpose Version,  OK as long as >= 32 bits             * 
C*((Arguement))                                                       * 
C*  ISEED: Seed                                                       * 
C*                                                                    * 
C********************************************************************** 
                                                                        
      REAL*8 FUNCTION DRN(ISEED)                                        
                                                                        
      COMMON/RANDM/RDM(31),RM1,RM2,IA1,IC1,M1,IX1,                      
     .                             IA2,IC2,M2,IX2,                      
     .                             IA3,IC3,M3,IX3                       
                                                                        
C Generate Next number in sequence                                      
                                                                        
      IX1    = MOD( IA1*IX1+IC1, M1 )                                   
      IX2    = MOD( IA2*IX2+IC2, M2 )                                   
      IX3    = MOD( IA3*IX3+IC3, M3 )                                   
      J      = 1 + (31*IX3)/M3                                          
      DRN    = RDM(J)                                                   
      RDM(J) = ( FLOAT(IX1)+FLOAT(IX2)*RM2 )*RM1                        
                                                                        
C Omit following statement if function arguement passed by value:       
                                                                        
      ISEED = IX1                                                       
      RETURN                                                            
      END                                                               
C********************************************************************** 
C*============================                                        * 
C* Subroutine DRNSET( ISEED )                                         * 
C*============================                                        * 
C*((Purpose))                                                         * 
C*  Initialization routine of                                         * 
C*         Machine-independent Random number generator                * 
C*         General purpose Version,  OK as long as >= 32 bits         * 
C*((Arguement))                                                       * 
C*  ISEED: SEED                                                       * 
C*                                                                    * 
C********************************************************************** 
                                                                        
      SUBROUTINE DRNSET( ISEED )                                        
                                                                        
      COMMON/RANDM/RDM(31),RM1,RM2,IA1,IC1,M1,IX1,                      
     .                             IA2,IC2,M2,IX2,                      
     .                             IA3,IC3,M3,IX3                       
                                                                        
      IA1 =    1279                                                     
      IC1 =  351762                                                     
      M1  = 1664557                                                     
      IA2 =    2011                                                     
      IC2 =  221592                                                     
      M2  = 1048583                                                     
      IA3 =   15091                                                     
      IC3 =    6171                                                     
      M3  =   29201                                                     
                                                                        
C Initialization                                                        
                                                                        
      IX1  = MOD( ISEED, M1 )                                           
      IX1  = MOD( IA1*IX1+IC1, M1 )                                     
      IX2  = MOD( IX1, M2 )                                             
      IX1  = MOD( IA1*IX1+IC1, M1 )                                     
      IX3  = MOD( IX1,M3)                                               
      RM1  = 1./FLOAT(M1)                                               
      RM2  = 1./FLOAT(M2)                                               
      DO 100 J = 1,31                                                   
         IX1   = MOD( IA1*IX1+IC1, M1 )                                 
         IX2   = MOD( IA2*IX2+IC2, M2 )                                 
         RDM(J)= ( FLOAT(IX1)+FLOAT(IX2)*RM2 )*RM1                      
  100 CONTINUE                                                          
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*    ===================                                               *
      SUBROUTINE SHCLER                                                 
*    ===================                                               *
* ((FUNCTION))                                                         *
*     To cancel the update of histograms and scatter plots in case     *
*   of the trial was rejected.                                         *
* ((Author))                                                           *
*     S.Kawabata June '90 at KEK                                       *
*                                                                      *
************************************************************************
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
                                                                        
      IF( NHIST .GT. 0 ) THEN                                           
         DO 200  J   = 1, NHIST                                         
           IP3       = MAPL(3,J)                                        
           IBUF(IP3) = -1                                               
  200    CONTINUE                                                       
      ENDIF                                                             
C                                                                       
      IF( NSCAT .GT. 0 ) THEN                                           
         DO 500   K    = 1, NSCAT                                       
           IP3         = MAPD(4,K)                                      
           IBUF(IP3)   =  0                                             
           IBUF(IP3+1) =  0                                             
  500    CONTINUE                                                       
      ENDIF                                                             
C                                                                       
      RETURN                                                            
      END                                                               
************************************************************************
*    ===========================                                       *
      SUBROUTINE SHFILL( NTRY )                                         
*    ===========================                                       *
* ((Function))                                                         *
*     To fill the number of trials for a event generation              *
* ((Input))                                                            *
*    NTYR : the number of trials for the current event                 *
* ((Author))                                                           *
*    S.Kawabata    April 1994                                          *
*                                                                      *
************************************************************************
                                                                        
      PARAMETER ( MXBIN = 51 )                                          
      COMMON/PLOTSP/ NBIN,IBUFSP( MXBIN )                               
                                                                        
      IF( NTRY .LE. NBIN ) THEN                                         
          IBUFSP( NTRY ) = IBUFSP( NTRY ) + 1                           
      ELSE                                                              
          IBUFSP( NBIN+1 ) = IBUFSP( NBIN+1 ) + 1                       
      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*    ============================                                      *
      SUBROUTINE SHINIT( MXTRY )                                        
*    ============================                                      *
* ((Function))                                                         *
*     To clear the histogram buffer for generation efficiency          *
* ((Input))                                                            *
*    MXTRY: Maximum number of trials for one event generation          *
* ((Author))                                                           *
*    S.Kawabata    April 1994                                          *
*                                                                      *
************************************************************************
                                                                        
      INTEGER MXTRY                                                     
      PARAMETER ( MXBIN = 51 )                                          
      COMMON/PLOTSP/ NBIN,IBUFSP( MXBIN )                               
                                                                        
      IF( MXTRY .GT. 50 ) THEN                                          
          NBIN  = 50                                                    
      ELSE                                                              
          NBIN  = MXTRY                                                 
      ENDIF                                                             
                                                                        
      DO 100 I = 1,NBIN+1                                               
         IBUFSP(I) = 0                                                  
  100 CONTINUE                                                          
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*    =========================                                         *
      SUBROUTINE SHPLOT( LU )                                           
*    =========================                                         *
C*((Function))                                                         *
C*    To print histograms and scatter plots defined by XHINIT and      *
C*  DHINIT.                                                            *
C*    For the original histograms, a special histograms are printd     *
C*  by this routine. For the additional histograms and scatter plots   *
C*  routines XHPLOT and DHPLOT are called.                             *
C*((Author))                                                           *
C*    S.Kawabata   June '90 at KEK                                     *
C*                                                                     *
C***********************************************************************
                                                                        
      REAL*8         SCALLS,WGT,TI,TSI,TACC                             
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT                          
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
                                                                        
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
                                                                        
      CHARACTER*50 CHARR,CHR1                                           
      CHARACTER*52 SCALE                                                
      REAL  VAL(0:51),VLOG(0:51)                                        
      REAL  VERR(0:51)                                                  
      CHARACTER*1  BLNK,STAR,CROS,AI,CN                                 
      DATA  YMAX / 50/, BLNK /' '/, STAR /'*'/, CROS /'O'/              
      DATA  AI /'I'/                                                    
                                                                        
      CN  = CHAR(12)                                                    
                                                                        
      CALL XHCHCK( LU )                                                 
                                                                        
      IF( NHIST .GT. 0 ) THEN                                           
*                 add March 1994                                        
         CALL SHUPDT                                                    
*                                                                       
C        NTOTAL= SCALLS                                                 
         DO 500 IHIST = 1, NHIST                                        
          IF(IFBASE(IHIST) .EQ. 1 ) THEN                                
            IP3  = MAPL(4,IHIST)                                        
            IF( IPNT .EQ. 0 ) THEN                                      
                WRITE(LU,9010)                                          
            ELSE                                                        
                WRITE(LU,9020) CN                                       
            ENDIF                                                       
 9010       FORMAT(/1H1)                                                
 9020       FORMAT(A1)                                                  
            WRITE(LU,9050) MAPL(1,IHIST),(BUFF(I), I=IP3+1,IP3+15)      
 9050       FORMAT(1X,'Original Histogram (ID =',I3,' ) for ',15A4)     
                                                                        
            IP1   = MAPL(2,IHIST)                                       
            XMIN  = BUFF(IP1)                                           
            XMAX  = BUFF(IP1+1)                                         
            NXBIN = IBUF(IP1+2) + 1                                     
            DEV   = BUFF(IP1+3)                                         
            VMAX  = 0.0                                                 
            VORG  = 0.0                                                 
            VEVT  = 0.0                                                 
C           FACT       = 1./(NTOTAL*DEV)                                
            FACT       = 1./(SCALLS*DEV)                                
            IP2   = MAPL(3,IHIST)                                       
            IPX   = IP2 + 52                                            
            IPF   = IP2 + 156                                           
            IPF2  = IPF + 52                                            
C           VAL(0)     = BUFF(IPF)/NTOTAL                               
            VAL(0)     = BUFF(IPF)/SCALLS                               
C           VAL(NXBIN) = BUFF(IPF+NXBIN)/NTOTAL                         
            VAL(NXBIN) = BUFF(IPF+NXBIN)/SCALLS                         
            VEVT1 = BUFF(IPX) + BUFF(IPX+NXBIN)                         
            DO  50 I   = 1,NXBIN-1                                      
                TX     = BUFF(I+IPF)                                    
                NX     = IBUF(I+IP2)                                    
                VLS    = TX*FACT                                        
                IF( VMAX .LT. VLS ) VMAX = VLS                          
                VAL(I) = VLS                                            
                IF( NX .GT. 1 ) THEN                                    
                  DEV2   =  NX*BUFF(I+IPF2)-TX*TX                       
                  IF( DEV2 .LE. 0.0 ) THEN                              
                      VERR(I)= 0.0                                      
                  ELSE                                                  
                      VERR(I)= FACT*SQRT( DEV2/( NX-1 ))                
                  ENDIF                                                 
                ELSEIF( NX .EQ. 1 ) THEN                                
                  VERR(I)= VLS                                          
                ELSE                                                    
                  VERR(I)= 0.0                                          
                ENDIF                                                   
                VORG   = VLS + VORG                                     
                VEVT   = BUFF(I+IPX) + VEVT                             
   50       CONTINUE                                                    
            NTOT   = INT(VEVT+VEVT1)                                    
            IF( VMAX .LE. 0.0 .AND. VEVT .GT. 0.0 ) THEN                
                  WRITE(LU,9060) MAPL(1,IHIST)                          
 9060             FORMAT(/5X,'***************************************', 
     .                   /5X,'* Since BASES has no entry            *', 
     .                   /5X,'*     in the histogram ID(',I6,' ),   *', 
     .                   /5X,'*  an additional hist. is given       *', 
     .                   /5X,'*     in the next page in stead.      *', 
     .                   /5X,'***************************************') 
C                                                                       
                  CALL XHPLOT( LU, 1, IHIST )                           
C                                                                       
                  GO TO 500                                             
            ELSEIF( VEVT .LE. 0) THEN                                   
                  WRITE(LU,9070) IHIST                                  
 9070             FORMAT(/5X,'***************************************', 
     .                   /5X,'*    SPRING has no entry              *', 
     .                   /5X,'*     in the histogram ID(',I6,' )    *', 
     .                   /5X,'***************************************') 
                  GO TO 500                                             
            ENDIF                                                       
            VNORM = VORG/VEVT                                           
            XNORM = VNORM*DEV                                           
            VLMAX = ALOG10(VMAX)                                        
            VLMIN = VLMAX                                               
            DO  60 I = 0,NXBIN                                          
              IF( VAL(I) .GT. 0.0 ) THEN                                
                  VLS   = ALOG10( VAL(I) )                              
                 IF( I .GT. 0 .AND. I .LT. NXBIN ) THEN                 
                    IF( VLS .LT. VLMIN ) VLMIN = VLS                    
                 ENDIF                                                  
                 VLOG(I)  = VLS                                         
              ELSE                                                      
                 VLOG(I)  = 0.0                                         
              ENDIF                                                     
   60       CONTINUE                                                    
C                                                                       
             VXMAX = VLMAX                                              
             IF( VLMIN .LT. 0.0) THEN                                   
                VXMIN = IFIX(VLMIN) - 1.0                               
             ELSE                                                       
                VXMIN = IFIX(VLMIN)                                     
             ENDIF                                                      
             CALL XHRNGE( 1, VXMIN, VXMAX, VLMIN, VLMAX, VLSTP)         
             UNITL = (VLMAX-VLMIN)/YMAX                                 
C                                                                       
             CALL XHSCLE( 1, VLMIN, VLMAX, VLSTP, UNITL, SCALE, CHR1)   
C                                                                       
C                                                                       
             WRITE(LU,9150) NTOT                                        
 9150        FORMAT(1X,'Total =',I10,' events',                         
     .              3X,'"*" : Orig. Dist. in Log Scale.')               
             VXMIN = 10.0**VLMIN                                        
             WRITE(LU,9200) SCALE                                       
 9200        FORMAT(1X,'   x      d(Sig/dx)  dN/dx',A52)                
             WRITE(LU,9250) CHR1                                        
 9250        FORMAT(1X,                                                 
     .             '+-------+----------+-------+',                      
     .       A50 )                                                      
C                                                                       
                                                                        
            VX    = ABS(XMAX)                                           
            XM    = ABS(XMIN)                                           
            IF( XM .GT. VX ) VX = XM                                    
                                                                        
            CALL XHORDR( VX, F2, ORD, IORD )                            
                                                                        
            DO 200 I = 0,NXBIN                                          
              RNORM = VNORM                                             
              IF( I .EQ. 0 .OR. I .EQ. NXBIN ) RNORM = XNORM            
              VX    = VAL(I)                                            
              XL     = BUFF( I + IPX )                                  
              NX     = XL                                               
              IF( VX .GT. 0.0 ) THEN                                    
                 NUMBL  = (VLOG(I) - VLMIN)/UNITL + 1.0                 
              ELSE                                                      
                 NUMBL  = 0                                             
              ENDIF                                                     
              IF( NX .GT. 0 ) THEN                                      
                 NUMB   = ( LOG10( XL*RNORM ) - VLMIN)/UNITL + 1.0      
                 ERL    = SQRT(XL)                                      
                 DERL   = (XL + ERL)*RNORM                              
                 NERUP  = ( LOG10( DERL ) - VLMIN)/UNITL + 1.0          
                 DERL   = (XL - ERL)*RNORM                              
                 IF( DERL .GT. 0.0 ) THEN                               
                     NERLW  = ( LOG10( DERL ) - VLMIN)/UNITL + 1.0      
                 ELSE                                                   
                     NERLW  = 0                                         
                 ENDIF                                                  
              ELSE                                                      
                 NUMB   = 0                                             
                 NERUP  = 0                                             
                 NERLW  = 0                                             
              ENDIF                                                     
              IF( NUMB  .GT. 50 ) NUMB = 50                             
              IF( NUMBL .GT. 50 ) NUMBL= 50                             
              DO 100 K = 1,50                                           
                IF( K .LE. NUMBL) THEN                                  
                  CHARR(K:K) = STAR                                     
                ELSE                                                    
                  IF( K .EQ. 50 ) THEN                                  
                    CHARR(K:K) = AI                                     
                  ELSE                                                  
                    CHARR(K:K) = BLNK                                   
                  ENDIF                                                 
                ENDIF                                                   
C                                                                       
                IF(     K .EQ. NUMB ) THEN                              
                        CHARR(K:K) = CROS                               
                        IF( K .EQ. NERUP .AND. K .EQ. NERLW ) GO TO 100 
                ENDIF                                                   
                IF(     K .EQ. NERUP ) THEN                             
                        CHARR(K:K) = '>'                                
                ELSEIF( K .EQ. NERLW ) THEN                             
                        CHARR(K:K) = '<'                                
                ENDIF                                                   
                                                                        
  100         CONTINUE                                                  
                                                                        
              CALL XHORDR( VX, F2, ORDER, IORDR )                       
                                                                        
             IF( I .EQ. 0 .OR. I .EQ. NXBIN ) THEN                      
                 WRITE(LU,9300) IORD,F2,IORDR,NX,CHARR                  
 9300            FORMAT(1X,'I  E',I3,' I',F6.3,'E',I3,'I',              
     .                                            I7,'I',A50)           
             ELSE                                                       
                   XM    = (XMIN + DEV*(I-1))/ORD                       
                   WRITE(LU,9340) XM,F2,IORDR,NX,CHARR                  
 9340              FORMAT(1X,'I',F6.3,' I',F6.3,'E',I3,'I',             
     .                                        I7,'I',A50)               
             ENDIF                                                      
  200       CONTINUE                                                    
             WRITE(LU,9250) CHR1                                        
             WRITE(LU,9260)                                             
 9260    FORMAT(1X,                                                     
     .       '   x      d(Sig/dx)  dN/dx',4X,                           
     .       '"O" : Generated Events.',                                 
     .       '( Arbitrary unit in Log )')                               
C                                                                       
           ELSE                                                         
C                                                                       
              CALL XHPLOT( LU, 1, IHIST )                               
C                                                                       
           ENDIF                                                        
  500    CONTINUE                                                       
      ENDIF                                                             
C                                                                       
      CALL DHPLOT( LU )                                                 
C                                                                       
      RETURN                                                            
      END                                                               
************************************************************************
*    ====================                                              *
      SUBROUTINE SHRSET                                                 
*    ====================                                              *
* ((Function))                                                         *
*     To reset the content of histograms and scatter plots.            *
* ((Author))                                                           *
*     S.Kawabata   June '90 at KEK                                     *
*                                                                      *
* **********************************************************************
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
                                                                        
      IF( NHIST .GT. 0 ) THEN                                           
         DO 100 IHIST = 1, NHIST                                        
            IP2       = MAPL(3,IHIST) + 52                              
            IP3       = MAPL(4,IHIST)                                   
            IBUF(IP3) = -1                                              
            DO 100 I = 0,51                                             
               BUFF(I+IP2) = 0.0                                        
  100      CONTINUE                                                     
      ENDIF                                                             
C                                                                       
      IF( NSCAT .GT. 0 ) THEN                                           
         DO 400   ISCAT = 1, NSCAT                                      
            IP3         = MAPD(4,ISCAT)                                 
            IBUF(IP3)   = 0                                             
            IBUF(IP3+1) = 0                                             
            IP2         = MAPD(3,ISCAT)                                 
            IBUF(IP2)   = 0                                             
            DO 400   I  = IP2+1,IP2+2500                                
               BUFF(I)  = 0.0                                           
  400      CONTINUE                                                     
      ENDIF                                                             
C                                                                       
      RETURN                                                            
      END                                                               
************************************************************************
*    ====================                                              *
      SUBROUTINE SHUPDT                                                 
*    ====================                                              *
* ((Function))                                                         *
*     To update histograms and scatter plots with unit weight.         *
*   The bin number to be updated is marked by XHFILL and DHFILL.       *
* ((Author))                                                           *
*     S.Kawabata  June '90 at KEK                                      *
*                                                                      *
************************************************************************
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
                                                                        
      IF( NHIST .GT. 0 ) THEN                                           
         DO 150   IHIST   = 1, NHIST                                    
            IP3       = MAPL(4,IHIST)                                   
            IX        = IBUF(IP3)                                       
            IF( IX .GE. 0 ) THEN                                        
                IP       = IX + MAPL(3,IHIST) + 52                      
                BUFF(IP) = BUFF(IP) + 1.                                
                                                                        
                IBUF(IP3)  = -1                                         
            ENDIF                                                       
  150    CONTINUE                                                       
      ENDIF                                                             
C                                                                       
      IF( NSCAT .GT. 0 ) THEN                                           
         DO 250   ISCAT   = 1, NSCAT                                    
            IP3         = MAPD(4,ISCAT)                                 
            IX          = IBUF(IP3)                                     
            IF( IX .GT. 0 ) THEN                                        
                IP1   = MAPD(2,ISCAT)                                   
                MXBIN = IBUF(IP1+2)                                     
                MYBIN = IBUF(IP1+6)                                     
                IP2       = MAPD(3,ISCAT)                               
                IBUF(IP2) = IBUF(IP2) + 1                               
                IY        = IBUF(IP3+1)                                 
                IF( IX .GT. 0 .AND. IX .LE. MXBIN .AND.                 
     .              IY .GT. 0 .AND. IY .LE. MYBIN ) THEN                
                    IP       = IX + MXBIN*(IY-1) + IP2                  
                    BUFF(IP) = BUFF(IP) + 1.0                           
                ENDIF                                                   
                IBUF(IP3)   =  0                                        
                IBUF(IP3+1) =  0                                        
           ENDIF                                                        
C                                                                       
  250    CONTINUE                                                       
      ENDIF                                                             
C                                                                       
      RETURN                                                            
      END                                                               
************************************************************************
*    ===================                                               *
      SUBROUTINE SPCHCK                                                 
*    ===================                                               *
* ((Purpose))                                                          *
*     To check user's initialization parameters.                       *
*                                                                      *
*        Coded by S.Kawabata      April  '94                           *
*                                                                      *
************************************************************************
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      PARAMETER ( MXDIM = 50)                                           
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                   
     .               IG(MXDIM),NCALL                                    
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2                             
                                                                        
      COMMON /BASE0/ JFLAG,IBASES                                       
      COMMON /BASE1/ XLT(MXDIM),XUT(MXDIM),NDIMT,NWILDT,                
     .               IGT(MXDIM),NCALLT                                  
      COMMON /BASE2/ ACC1T,ACC2T,ITMX1T,ITMX2T                          
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
                                                                        
      IF( NDIM .NE. NDIMT ) THEN                                        
          WRITE(6,9100) NDIM,NDIMT                                      
 9100     FORMAT(                                                       
     .     5X,'*************************************************',      
     .    /5X,'*                                               *',      
     .    /5X,'*   Given NDIM(',I6,' ) does not match          *',      
     .    /5X,'*      to NDIM(',I6,' ) in BASES.               *',      
     .    /5X,'*                                               *',      
     .    /5X,'*   Process was terminated due to this error.   *',      
     .    /5X,'*                                               *',      
     .    /5X,'*************************************************')      
          STOP                                                          
      ENDIF                                                             
                                                                        
      IF( NWILD .NE. NWILDT ) THEN                                      
          WRITE(6,9110) NWILD,NWILDT                                    
 9110     FORMAT(                                                       
     .     5X,'*************************************************',      
     .    /5X,'*                                               *',      
     .    /5X,'*   Given NWILD(',I6,' ) does not match         *',      
     .    /5X,'*      to NWILD(',I6,' ) in BASES.              *',      
     .    /5X,'*                                               *',      
     .    /5X,'*   Process was terminated due to this error.   *',      
     .    /5X,'*                                               *',      
     .    /5X,'*************************************************')      
          STOP                                                          
      ENDIF                                                             
                                                                        
      DO 200 I = 1,NDIM                                                 
         IF( XL(I) .NE. XLT(I) ) THEN                                   
             WRITE(6,9200) I,XL(I),I,XLT(I)                             
 9200        FORMAT(                                                    
     .     5X,'*************************************************',      
     .    /5X,'*                                               *',      
     .    /5X,'*   Given XL(',I3,' ) = ',D15.8,'            *',         
     .    /5X,'*      does not match to                        *',      
     .    /5X,'*      to XL(',I3,' ) = ',D15.8,' in BASES   *',         
     .    /5X,'*                                               *',      
     .    /5X,'*   Process was terminated due to this error.   *',      
     .    /5X,'*                                               *',      
     .    /5X,'*************************************************')      
             STOP                                                       
         ENDIF                                                          
         IF( XU(I) .NE. XUT(I) ) THEN                                   
             WRITE(6,9210) I,XU(I),I,XUT(I)                             
 9210        FORMAT(                                                    
     .     5X,'*************************************************',      
     .    /5X,'*                                               *',      
     .    /5X,'*   Given XU(',I3,' ) = ',D15.8,'            *',         
     .    /5X,'*      does not match to                        *',      
     .    /5X,'*      to XU(',I3,' ) = ',D15.8,' in BASES   *',         
     .    /5X,'*                                               *',      
     .    /5X,'*   Process was terminated due to this error.   *',      
     .    /5X,'*                                               *',      
     .    /5X,'*************************************************')      
             STOP                                                       
         ENDIF                                                          
  200 CONTINUE                                                          
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*    =========================                                         *
      SUBROUTINE SPHIST( LU )                                           
*    =========================                                         *
* ((Purpose))                                                          *
*      To print the histogram for event generation                     *
* ((Input))                                                            *
*      LU   : logical unit number for the printer to be printed        *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata    April 1994                                       *
*                                                                      *
************************************************************************
                                                                        
      PARAMETER ( MXBIN = 51 )                                          
      COMMON/PLOTSP/ NBIN,IBUFSP( MXBIN )                               
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
                                                                        
      REAL  VAL(MXBIN),VLOG(MXBIN)                                      
      CHARACTER*50 CHARR,CHAR1                                          
      CHARACTER*52 SCALE                                                
      CHARACTER*1  BLNK,STAR,OO,AI,CN                                   
      DATA  YMAX / 50/                                                  
      DATA  BLNK /' '/, STAR /'*'/, OO /'O'/, AI /'I'/                  
                                                                        
      CN    = CHAR(12)                                                  
      IF( IPNT .EQ. 0 ) THEN                                            
          WRITE(LU,9000)                                                
 9000     FORMAT(/1H1,/1H )                                             
      ELSE                                                              
          WRITE(LU,9005) CN                                             
 9005     FORMAT(A1)                                                    
      ENDIF                                                             
          WRITE(LU,9102)                                                
 9102     FORMAT(5X,                                                    
     .  '************* Number of trials to get an event *************') 
                                                                        
      XMIN  = 1.0                                                       
      XMAX  = NBIN                                                      
      DEV   = 1.0                                                       
      NBIN1 = NBIN + 1                                                  
                                                                        
          NTOTAL     = IBUFSP(NBIN1)                                    
          VAL(NBIN1) = FLOAT(NTOTAL)                                    
          VMIN       = 0.0                                              
          VMAX       = VMIN                                             
          DO  55 I   = 1,NBIN                                           
              NTR    = IBUFSP(I)                                        
              VLS    = FLOAT( NTR )                                     
              NTOTAL = NTR + NTOTAL                                     
              IF( VMAX .LT. VLS ) VMAX = VLS                            
              VAL(I) = VLS                                              
   55     CONTINUE                                                      
                                                                        
          VLMAX = LOG10(VMAX)                                           
          VLMIN = VLMAX                                                 
                                                                        
           DO  60 I = 1,NBIN1                                           
               IF( VAL(I) .GT. 0.0 ) THEN                               
                   VLS   = LOG10( VAL(I) )                              
                   IF( I .LE. NBIN ) THEN                               
                       IF( VLS .LT. VLMIN ) VLMIN = VLS                 
                   ENDIF                                                
                   VLOG(I)  = VLS                                       
               ENDIF                                                    
   60      CONTINUE                                                     
                                                                        
           IF( VLMIN .LT. 0.0) THEN                                     
               VXMIN = IFIX(VLMIN) - 1.0                                
           ELSE                                                         
               VXMIN = IFIX(VLMIN)                                      
           ENDIF                                                        
           VXMAX = VLMAX                                                
           IFLG  = 1                                                    
           CALL XHRNGE( IFLG, VXMIN, VXMAX, VLMIN, VLMAX, VLSTP )       
           UNITL = (VLMAX-VLMIN)/YMAX                                   
                                                                        
       IFLG   = 0                                                       
           IF( VMIN .GE. 0.0 ) THEN                                     
               VXMAX  = 1.2*VMAX                                        
               VXMIN  = 0.0                                             
               CALL XHRNGE( IFLG, VXMIN, VXMAX, VMIN, VMAX, VSTP )      
           ELSE                                                         
               VXMAX  = 1.1*VMAX                                        
               VXMIN  = 1.1*VMIN                                        
               CALL XHRNGE( IFLG, VXMIN, VXMAX, VMIN, VMAX, VSTP )      
           ENDIF                                                        
                                                                        
       UNIT  = (VMAX-VMIN)/YMAX                                         
                                                                        
       CALL XHSCLE( IFLG, VMIN, VMAX, VSTP, UNIT, SCALE, CHAR1 )        
                                                                        
             WRITE(LU,9210) NTOTAL                                      
 9210        FORMAT(1X,'Total =',I10,' events',                         
     .        3X,'"*" : No. of events in Linear scale.')                
             WRITE(LU,9205) SCALE                                       
 9205        FORMAT(1X,'   x      Lg(dN/dx)  dN/dx',A52)                
             WRITE(LU,9251) CHAR1                                       
 9251        FORMAT(1X,                                                 
     .             '+-------+----------+-------+',                      
     .       A50 )                                                      
                                                                        
       VX    = ABS(XMAX)                                                
       XM    = ABS(XMIN)                                                
       IF( XM .GT. VX ) VX = XM                                         
                                                                        
       CALL XHORDR( VX, F2, ORD, IORD )                                 
                                                                        
       IF( VMIN .LT. 0.0 ) THEN                                         
           V1    = VMIN                                                 
           NUMBL = 1                                                    
           DO 150 I = 1, 80                                             
              V2    = V1 + UNIT                                         
              IF( V1 .LE. 0.0 .AND. V2 .GE. 0.0 ) THEN                  
                  NUMBL  = I                                            
                  GO TO 180                                             
              ENDIF                                                     
              V1    = V2                                                
  150      CONTINUE                                                     
       ENDIF                                                            
                                                                        
  180  DO 300 I = 1,NBIN1                                               
          VX   = VAL(I)                                                 
          IF( VMIN .GE. 0.0 ) THEN                                      
              IF( VX .GT. 0.0 ) THEN                                    
                  NUMBL  = (VLOG(I) - VLMIN)/UNITL + 1.0                
                  NUMB   = VX/UNIT + 1.0                                
              ELSE                                                      
                  NUMBL  = 0                                            
                  NUMB   = 0                                            
              ENDIF                                                     
              IF( NUMB .GT. 50 ) NUMB = 50                              
              IF( NUMBL.GT. 50 ) NUMBL= 50                              
              DO 200 K = 1,50                                           
                 IF(     ( K .GT. NUMBL) .AND. (K .GT. NUMB ) ) THEN    
                           IF( K .EQ. 50 ) THEN                         
                               CHARR(K:K) = AI                          
                           ELSE                                         
                               CHARR(K:K) = BLNK                        
                           ENDIF                                        
                 ELSEIF( ( K .LE. NUMBL) .AND. (K .GT. NUMB )) THEN     
                           CHARR(K:K) = OO                              
                 ELSEIF( ( K .GT. NUMBL) .AND. (K .LE. NUMB )) THEN     
                           CHARR(K:K) = STAR                            
                 ELSEIF( ( K .LE. NUMBL) .AND. (K .LE. NUMB)) THEN      
                           IF( NUMB .GE. NUMBL ) THEN                   
                               CHARR(K:K) = OO                          
                           ELSE                                         
                               CHARR(K:K) = STAR                        
                           ENDIF                                        
                 ENDIF                                                  
  200         CONTINUE                                                  
          ELSE                                                          
                                                                        
              V1          = VMIN                                        
              NHIG        = 1                                           
              DO 220  J = 1, 50                                         
                 V2     = V1 + UNIT                                     
                 IF( VX .GE. V1 .AND. VX .LT. V2 ) THEN                 
                     NHIG   = J                                         
                     GO TO 240                                          
                 ENDIF                                                  
                 V1    = V2                                             
  220         CONTINUE                                                  
  240         NLOW   = NUMBL                                            
              IF( NHIG .LT. NLOW) THEN                                  
                  NX    = NHIG                                          
                  NHIG  = NLOW                                          
                  NLOW  = NX                                            
              ENDIF                                                     
                                                                        
              DO 250 K = 1, 49                                          
                 IF(     K .EQ. NUMBL ) THEN                            
                         CHARR(K:K) = AI                                
                 ELSEIF( K .GT. NHIG ) THEN                             
                         CHARR(K:K) = BLNK                              
                 ELSEIF( K .LT. NLOW ) THEN                             
                         CHARR(K:K) = BLNK                              
                 ELSE                                                   
                     IF( K .EQ. NHIG .AND. K .EQ. NLOW) THEN            
                         CHARR(K:K) = AI                                
                     ELSE                                               
                         CHARR(K:K) = STAR                              
                     ENDIF                                              
                 ENDIF                                                  
  250         CONTINUE                                                  
              CHARR(50:50) = AI                                         
          ENDIF                                                         
                                                                        
             NX  = VAL(I)                                               
             VX     = VAL(I)                                            
             VX1    = VX                                                
             IF( VX .LT. 0.0 ) VX1 = -VX                                
             CALL XHORDR( VX1, F2, ORDER, IORDR )                       
             F2     = VX/ORDER                                          
             IF( I .EQ. NBIN1 ) THEN                                    
                 WRITE(LU,9400) IORD,F2,IORDR,NX,CHARR                  
 9400            FORMAT(1X,'I  E',I3,' I',F6.3,'E',I3,'I',              
     .                                            I7,'I',A50)           
             ELSE                                                       
                   XM  = (XMIN + DEV*(I - 1))/ORD                       
                   WRITE(LU,9440) XM,F2,IORDR,NX,CHARR                  
 9440              FORMAT(1X,'I',F6.3,' I',F6.3,'E',I3,'I',             
     .                                        I7,'I',A50)               
             ENDIF                                                      
                                                                        
  300  CONTINUE                                                         
                                                                        
       IF( VMIN .GE. 0.0 ) THEN                                         
           CALL XHSCLE( 1, VLMIN, VLMAX, VLSTP, UNITL, SCALE, CHAR1)    
           VXMIN  = 10**VLMIN                                           
       ENDIF                                                            
                                                                        
           WRITE(LU,9251) CHAR1                                         
           WRITE(LU,9205) SCALE                                         
           WRITE(LU,9360)                                               
 9360      FORMAT(30X,'"O" : No. of Events in Log. scale.')             
                                                                        
C                                                                       
                                                                        
      RETURN                                                            
      END                                                               
*********************************************************************** 
*============================                                         * 
      SUBROUTINE SPINFO( LU )                                           
*============================                                         * 
*((Purpose))                                                          * 
*    Print the information for                                        * 
*        (1) BASES parameters                                         * 
*        (2) Computer time information                                * 
*        (3) Convergency behavior of the Grid optimization step       * 
*        (4) Convergency behavior of the integration step             * 
*(( Input ))                                                          * 
*    LU  :  Logical unit number of printer                            * 
*                                                                     * 
*           by S.Kawabata    March 1994 at KEK                          
*                                                                     * 
*********************************************************************** 
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      COMMON /BDATE/ IDATE(3),ITIME(2)                                  
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
                                                                        
      COMMON /SPRNG2/ MXTRY,NEVENT, NTRIAL, MISS                        
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
*     COMMON/PLOTH/ XHASH(ILH,13),DHASH(IDH,14),IFBASE(ILH),            
*    .              MAXL, NHIST, MAPL(4,ILH),                           
*    .              MAXD, NSCAT, MAPD(4,IDH),                           
*    .              NW                                                  
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
                                                                        
      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1      
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1                  
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)                    
      REAL*4 XTIME                                                      
                                                                        
      CHARACTER*1 CN                                                    
                                                                        
       IF( IPNT .EQ. 0 ) THEN                                           
           WRITE(LU,9300)                                               
       ELSE                                                             
           CN     = CHAR(12)                                            
           WRITE(LU,9350) CN                                            
       ENDIF                                                            
 9300  FORMAT(/1H1,////1H )                                             
 9350  FORMAT(A1,////1X)                                                
       WRITE(LU,9360) (IDATE(I),I=1,3),(ITIME(J),J=1,2)                 
 9360  FORMAT(55X,'Date: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)           
       WRITE(LU,9400)                                                   
 9400 FORMAT(                                                           
     . 8X,'**********************************************************', 
     ./8X,'*                                                        *', 
     ./8X,'*    SSSSS   PPPPPP   RRRRRR   IIIII  N    NN   GGGGG    *', 
     ./8X,'*   SS   SS  PP   PP  RR   RR   III   NN   NN  GG   GG   *', 
     ./8X,'*   SS       PP   PP  RR   RR   III   NNN  NN  GG        *', 
     ./8X,'*    SSSSS   PPPPPP   RRRRR     III   NNNN NN  GG  GGGG  *', 
     ./8X,'*        SS  PP       RR  RR    III   NN NNNN  GG   GG   *', 
     ./8X,'*   SS   SS  PP       RR   RR   III   NN  NNN  GG   GG   *', 
     ./8X,'*    SSSSS   PP       RR    RR IIIII  NN   NN   GGGGG    *', 
     ./8X,'*                                                        *', 
     ./8X,'*                  SPRING Version 5.1                    *', 
     ./8X,'*           coded by S.Kawabata KEK, March 1994          *', 
     ./8X,'**********************************************************') 
*                                                                      *
          EFF   = FLOAT(NEVENT)/FLOAT(NTRIAL)*100.D0                    
          CALL BSTIME( RTIME, 1 )                                       
          XTIME = RTIME - TIMES1                                        
          WRITE(LU,9500) NEVENT,EFF,(TIMESP(I),I=0,2),XTIME,MXTRY,MISS  
 9500     FORMAT(/5X,'Number of generated events    =',I10,             
     .         /5X,'Generation efficiency         =',F10.3,' Percent',  
     .         /5X,'Computing time for generation =',F10.3,' Seconds',  
     .         /5X,'               for Overhead   =',F10.3,' Seconds',  
     .         /5X,'               for Others     =',F10.3,' Seconds',  
     .         /5X,'GO time for event generation  =',F10.3,' Seconds',  
     .         /5X,'Max. number of trials MXTRY   =',I10,' per event',  
     .         /5X,'Number of miss-generation     =',I10,' times')      
                                                                        
      CALL SPHIST( LU )                                                 
                                                                        
      RETURN                                                            
      END                                                               
C***********************************************************************
C*====================================                                 *
C* SUBROUTINE SPRGEN( F, MXTRY, NTRY )                                 *
C*====================================                                 *
C*                                                                     *
C*     Generation of events according to the probability density       *
C*     which is stored in a disk file.                                 *
C*                                                                     *
C*    Coded   by S.Kawabata   at July,1980                             *
C*    Update     S.Kawabata   September '84                            *
C*                                                                     *
C***********************************************************************
C                                                                       
       SUBROUTINE SPRGEN(F,MXTRY,NTRY)                                  
C                                                                       
      IMPLICIT REAL*8 (A-H,O-Z)                                         
C                                                                       
      EXTERNAL F                                                        
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)                   
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                    
     .               IG(MXDIM),NCALL                                    
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),      
     .               ND,NG,NPG,MA(MXDIM)                                
                                                                        
      COMMON /SPRNG1/ XND, DXG, XJAC, DXMAX, NSP                        
                                                                        
      DIMENSION Y(MXDIM),KG(MXDIM)                                      
      DATA ONE/1.0D0/                                                   
C                                                                       
C                                                                       
      RX    = DRN(IDUMY)*DXMAX                                           
C                                                                       
C  -------------- Binary Search  --------------------------------       
C                                                                       
      IPMIN = 1                                                         
      IPMAX = NSP                                                       
C                                                                       
 300  IC    = (IPMIN+IPMAX)/2                                           
        IF(RX .LT. DXD(IC)) THEN                                        
          IPMAX = IC                                                    
        ELSE                                                            
          IPMIN = IC                                                    
        ENDIF                                                           
      IF(IPMAX-IPMIN .GT.  2) GO TO 300                                 
C                                                                       
      IC    = IPMIN-1                                                   
 350  IC    = IC+1                                                      
      IF(DXD(IC) .LT. RX) GO TO 350                                     
C                                                                       
C --------------------------------------------------------------------  
C      Identify the hypecube number from sequential number IC           
C --------------------------------------------------------------------  
C                                                                       
       FMAX  = DXP(IC)                                                  
C                                                                       
       IX    = IC-1                                                     
                                                                        
       KG(NWILD) = IX/MA(NWILD) + 1                                     
       IF( NWILD .GT. 1 ) THEN                                          
           DO 400 J = 1,NWILD-1                                         
              NUM   = MOD(IX,MA(J+1))                                   
              KG(J) = NUM/MA(J) + 1                                     
  400      CONTINUE                                                     
       ENDIF                                                            
C                                                                       
C  ------------------------------------------------------------------   
C                     Sample and test a event                           
C  ------------------------------------------------------------------   
C                                                                       
      DO 600 NTRY = 1,MXTRY                                             
        WGT   = XJAC                                                    
        DO 550 J=1,NDIM                                                 
          IF( J .LE. NWILD) THEN                                        
             XN    = (KG(J)-DRN(IDUMY))*DXG+ONE                          
          ELSE                                                          
             XN    = ND*DRN(IDUMY) + ONE                                 
          ENDIF                                                         
          IAJ   = XN                                                    
          IF(IAJ .EQ. 1) THEN                                           
            XO    = XI(IAJ,J)                                           
            RC    = (XN-IAJ)*XO                                         
          ELSE                                                          
            XO    = XI(IAJ,J)-XI(IAJ-1,J)                               
            RC    = XI(IAJ-1,J)+(XN-IAJ)*XO                             
          ENDIF                                                         
          Y(J)  = XL(J) + RC*DX(J)                                      
          WGT   = WGT*XO*XND                                            
  550   CONTINUE                                                        
C                                                                       
*       FX    = F(Y)*WGT                                                
        FF    = F(Y)                                                    
        FX    = FF*WGT                                                  
        FUNCT = FX/FMAX                                                 
C                                                                       
        IF( FX .GT. 0.0D0 ) THEN                                        
*           IF( DRN(IDUMY) .LE. FUNCT ) GO TO 700                        
            XJ = DRN(IDUMY)                                              
            IF( XJ .LE. FUNCT ) GO TO 700                               
*           IF( XJ .LE. FUNCT ) THEN                                    
*               WRITE(6,9999) NTRY,IC,FF,WGT,XJ,FUNCT                   
*9999           FORMAT(1X,'NTRY,IC,FF,WGT,XJ,FUNCT = ',2I5,4E12.4)      
*               GO TO 700                                               
*           ENDIF                                                       
        ELSE                                                            
     .  IF( FX .LT. 0.0D0 ) THEN                                        
            WRITE(6,9100) IC                                            
 9100       FORMAT(                                                     
     .      /5X,'********** FATAL ERROR IN SPRING **********',          
     .      /5X,'* A negative value of function was found  *',          
     .      /5X,'*        in the ',I6,'-th Hypercube.      *',          
     .      /5X,'*******************************************')          
            WRITE(6,9405)                                               
 9405       FORMAT(5X,'------',3('+---------------'),'+')               
            WRITE(6,9410)                                               
 9410       FORMAT(5X,'    i       XL(i)             X       ',         
     .                '     XU(i)')                                     
            WRITE(6,9405)                                               
            DO 450 I = 1,NDIM                                           
                WRITE(6,9420) I,XL(I),Y(I),XU(I)                        
 9420           FORMAT(5X,I5,1P,3('  ',E14.6))                          
  450       CONTINUE                                                    
            WRITE(6,9405)                                               
            STOP                                                        
        ENDIF                                                           
C                                                                       
        CALL SHCLER                                                     
C                                                                       
  600 CONTINUE                                                          
                                                                        
      NTRY  = MXTRY + 1                                                 
                                                                        
  700 RETURN                                                            
      END                                                               
************************************************************************
*    ==================================                                *
      SUBROUTINE SPRING(FUNC, MXTRY )                                   
*    ==================================                                *
*         Main Program for the Event generation program SPRING.        *
*                                                                      *
*        Coded by S.Kawabata        September '84                      *
*                                                                      *
************************************************************************
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      EXTERNAL FUNC                                                     
      COMMON /BASE0/ NDUM,IBASES                                        
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)                   
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,                    
     .               IG(MXDIM),NCALL                                    
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),      
     .               ND,NG,NPG,MA(MXDIM)                                
      COMMON /BDATE/ IDATE(3),ITIME(2)                                  
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
                                                                        
      COMMON /SPRNG1/ XND, DXG, XJAC, DXMAX, NSP                        
      COMMON /SPRNG2/ MXTRYP,NEVENT, NTRIAL,MISS                        
                                                                        
      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1      
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1                  
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)                    
*                                                                      *
*----------------------------- Entry point ----------------------------*
*                                                                      *
*======================================================================*
*                  Initialization of the program                       *
*======================================================================*
*----------------------------------------------------------------------*
*                     initialize timer etc.                            *
*----------------------------------------------------------------------*
*                                                                      *
       IF( IBASES .GT. 0 ) THEN                                         
                                                                        
           CALL SPCHCK                                                  
                                                                        
           CALL BSTIME( TIME0, 0 )                                      
           TIMES1 = TIME0                                               
                                                                        
           MXTRYP = MXTRY                                               
           INTV   = 0                                                   
           IBASES = 0                                                   
           MISFLG = 0                                                   
                                                                        
           CALL BSDATE                                                  
                                                                        
           DO 10 I = 0,2                                                
              TIMESP(I) = 0.0                                           
   10      CONTINUE                                                     
*                                                                      *
            IF( MXTRY .LT. 10 ) MXTRY = 50                              
            NBIN    = MXTRY                                             
            IF( MXTRY .GT. 50) NBIN = 50                                
            MXTRY1  = MXTRY + 1                                         
            MISS    = 0                                                 
            NEVENT  = 0                                                 
            NTRIAL  = 0                                                 
                                                                        
            CALL SHINIT( MXTRY1 )                                       
                                                                        
*           -------------                                               
             CALL SHRSET                                                
*            -------------                                              
*----------------------------------------------------------------------*
*             Make the cumulative probability distribution             *
*----------------------------------------------------------------------*
*                                                                      *
            XND     = ND                                                
            DXG     = XND/NG                                            
            NSP     = NG**NWILD                                         
                                                                        
*///// DEBUG                                                            
*       MCALL   = NSP*NPG                                               
*       CALL BSPRNT( 4, MCALL, IDUM2, IDUM3, IDUM4 )                    
*                                                                       
            XJAC    = 1.0                                               
            DO 50 I = 1, NDIM                                           
               XJAC = XJAC*DX(I)                                        
   50       CONTINUE                                                    
            DXMAX   = 0.0D0                                             
            DO 100  I = 1,NSP                                           
               IF( DXD( I ) .LT. 0.0D0 ) THEN                           
                   WRITE(6,9100) I                                      
 9100              FORMAT(                                              
     .             /5X,'********** FATAL ERROR IN SPRING **********',   
     .             /5X,'*     Negative probability was found      *',   
     .             /5X,'*        in the ',I6,'-th Hypercube.      *',   
     .             /5X,'*******************************************')   
                   STOP                                                 
               ENDIF                                                    
                                                                        
               DXMAX    = DXMAX + DXD( I )                              
               DXD(I)   = DXMAX                                         
  100       CONTINUE                                                    
*        =====================                                          
          CALL BSUTIM( 1, 1 )                                           
*        =====================                                          
      ENDIF                                                             
*     =====================                                             
       CALL BSUTIM( 1, 2 )                                              
*     =====================                                             
      IF( IBASES .EQ. 1 ) THEN                                          
          WRITE(6,9000)                                                 
 9000     FORMAT(                                                       
     .      1X,'**************************************************',    
     .     /1X,'*    Flag IBASES was not equal to "0".           *',    
     .     /1X,'*                                                *',    
     .     /1X,'*   Process was terminated by this error.        *',    
     .     /1X,'*   Call S.Kawabata.                             *',    
     .     /1X,'**************************************************')    
           STOP                                                         
       ENDIF                                                            
*                                                                      *
*======================================================================*
*                       Event generation                               *
*======================================================================*
*     =====================                                             
  500  CALL BSUTIM( 1, 1 )                                              
*     =====================                                             
                                                                        
*     ==================================                                
        CALL SPRGEN( FUNC, MXTRY, IRET)                                 
*     ==================================                                
                                                                        
*     =====================                                             
       CALL BSUTIM( 1, 0 )                                              
*     =====================                                             
                                                                        
      CALL SHFILL( IRET )                                               
                                                                        
      IF( IRET .LE. MXTRY ) THEN                                        
          NTRIAL =NTRIAL + IRET                                         
          NEVENT = NEVENT + 1                                           
          CALL SHUPDT                                                   
      ELSE                                                              
          NTRIAL =NTRIAL + IRET - 1                                     
          MISS = MISS + 1                                               
          IF( MISFLG .EQ. 0 .AND. MISS .GT. MXTRY ) THEN                
              WRITE(6,9600) MXTRY                                       
 9600         FORMAT(1X,'****************************************',     
     .                  '****************************************',     
     .              /1X,'* (((( Warning ))))                     ',     
     .                  '                                       *',     
     .              /1X,'*                                       ',     
     .                  '                                       *',     
     .              /1X,'*  The number of mis-generations is foun',     
     .                  'd more than',I3,' times.                  *')  
              WRITE(6,9610)                                             
 9610         FORMAT(1X,'*                                       ',     
     .                  '                                       *',     
     .              /1X,'*(( Suggestion ))                       ',     
     .                  '                                       *',     
     .              /1X,'* (1) Try integration again with larger ',     
     .                  'number of sample points than this job. *',     
     .              /1X,'* or                                    ',     
     .                  '                                       *',     
     .              /1X,'* (2) The integral variables are not sui',     
     .                  'ted for the function.                  *',     
     .              /1X,'*     Take another integral variables !!',     
     .                  '                                       *',     
     .              /1X,'*                                       ',     
     .                  '                                       *',     
     .              /1X,'****************************************',     
     .                  '****************************************')     
            MISFLG = 1                                                  
          ENDIF                                                         
          GO TO 500                                                     
      ENDIF                                                             
*     =====================                                             
  600  CALL BSUTIM( 1, 1 )                                              
*     =====================                                             
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*    =======================                                           *
      SUBROUTINE XHCHCK(LU)                                             
*    =======================                                           *
* ((Purpose))                                                          *
*      To check the contents of the histogram table                    *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata    June '90                                         *
*                                                                      *
************************************************************************
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
                                                                        
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
      CHARACTER*1 CN                                                    
                                                                        
      CN  = CHAR(12)                                                    
                                                                        
      IF( IPNT .EQ. 0 ) THEN                                            
          WRITE(LU,9000)                                                
      ELSE                                                              
          WRITE(LU,9010) CN                                             
      ENDIF                                                             
 9000 FORMAT(/1H1)                                                      
 9010 FORMAT(A1)                                                        
                                                                        
      WRITE(LU,9050) NW                                                 
 9050 FORMAT(                                                           
     . //5X,'*********  Contents of the histogram Header *********',    
     .     //1X,'(1) Actual Buffer size     = ',I6,' Words')            
      WRITE(LU,9100) NHS,NHIST                                          
 9100 FORMAT(1X,'(2) Contents of Histograms ',                          
     .      /1X,'    Max. No. of Histograms = ',I6,                     
     .      /1X,'    Number   of Histograms = ',I6)                     
                                                                        
      IF( NHIST .GT. 0 ) THEN                                           
          WRITE(LU,9200)                                                
 9200     FORMAT(1X,'   ID     X_min        X_max    X_bin',            
     .              ' Hash Hst#')                                       
          DO 200 I = 1, 13                                              
             NT    = XHASH(1,I)                                         
             IF( NT .GT. 0 ) THEN                                       
                 DO 100 J = 2, NT+1                                     
                    K     = XHASH(J,I)                                  
                    IP1   = MAPL(2,K)                                   
                    IP3   = MAPL(4,K)                                   
                    XMIN  = BUFF(IP1)                                   
                    XMAX  = BUFF(IP1+1)                                 
                    NBIN  = IBUF(IP1+2)                                 
                    WRITE(LU,9300) MAPL(1,K),XMIN,XMAX,NBIN,I,NT,K      
 9300               FORMAT(1X,I5,1X,1PE12.4,1X,E12.4,I5,2I3,I5)         
  100            CONTINUE                                               
             ENDIF                                                      
  200     CONTINUE                                                      
      ENDIF                                                             
                                                                        
      WRITE(LU,9400) NSC,NSCAT                                          
 9400 FORMAT(1X,'(3) Contents of Scatter Plots',                        
     .      /1X,'    Max. No. of Scat_Plots = ',I6,                     
     .      /1X,'    Number   of Scat_Plots = ',I6)                     
                                                                        
      IF( NSCAT .GT. 0 ) THEN                                           
          WRITE(LU,9500)                                                
 9500     FORMAT(1X,'   ID      X_min   ',                              
     .              '     X_max   X-Bin    Y_min   ',                   
     .              '     Y_max   Y_Bin Hash Hst#')                     
          DO 400 I = 1, 13                                              
             NT    = DHASH(1,I)                                         
             IF( NT .GT. 0 ) THEN                                       
                 DO 300 J = 2, NT+1                                     
                    K     = DHASH(J,I)                                  
                    IP1   = MAPD(2,K)                                   
                    IP3   = MAPD(4,K)                                   
                    XMIN  = BUFF(IP1)                                   
                    XMAX  = BUFF(IP1+1)                                 
                    NXBN  = IBUF(IP1+2)                                 
                    YMIN  = BUFF(IP1+4)                                 
                    YMAX  = BUFF(IP1+5)                                 
                    NYBN  = IBUF(IP1+6)                                 
                    WRITE(LU,9600) MAPD(1,K),XMIN,XMAX,NXBN,            
     .                            YMIN,YMAX,NYBN,I,NT,K                 
 9600               FORMAT(1X,I5,1X,1PE12.4,1X,E12.4,I5,                
     .                                 E12.4,1X,E12.4,I5,2I3,I5)        
  300            CONTINUE                                               
             ENDIF                                                      
  400     CONTINUE                                                      
      ENDIF                                                             
      RETURN                                                            
      END                                                               
************************************************************************
*    ==============================                                    *
      SUBROUTINE XHFILL(ID, DX, FX )                                    
*    ==============================                                    *
* ((Function))                                                         *
*     To fill histograms.                                              *
*   This routine identifies the bin number which is to be updated      *
*   with weight FX*WGT.  Up to five points per histogram are able      *
*   to be stacked before calling BHUPDT or SHUPDT.                     *
* ((Input))                                                            *
*   ID    : Histogram identification number                            *
*   DX    : Input value                                                *
*   FX    : Input value of the function                                *
* ((Author))                                                           *
*   S.Kawabata         June '90 at KEK                                 *
*                                                                      *
************************************************************************
                                                                        
      REAL*8 DX,FX                                                      
      COMMON /BASE0/ IFLAG,IBASES                                       
      REAL*8         SCALLS,WGT,TI,TSI,TACC                             
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT                          
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
                                                                        
C     COMMON/PLOTLU/ LU                                                 
C                                                                       
      IF( NHIST .GT. 0 ) THEN                                           
C                                                                       
          I  = IABS(MOD( ID, 13 )) + 1                                  
          IF( XHASH(1, I) .EQ. 1 ) THEN                                 
            IF( ID .EQ. MAPL( 1, XHASH(2,I))) THEN                      
                IHIST = XHASH(2,I)                                      
                GO TO 200                                               
            ENDIF                                                       
          ELSEIF( XHASH(1, I) .GT. 1 ) THEN                             
            DO 100 K = 2, XHASH(1,I)+1                                  
               IF( ID .EQ. MAPL( 1, XHASH(K,I))) THEN                   
                   IHIST = XHASH(K,I)                                   
                   GO TO 200                                            
               ENDIF                                                    
  100       CONTINUE                                                    
          ENDIF                                                         
      ENDIF                                                             
C     IF( LU .GT. 0 ) THEN                                              
C         WRITE(LU,9000) ID                                             
C     ENDIF                                                             
C9000 FORMAT(1X,'No Histogram corresponds to ID =',I5,                  
C    .      /1X,' This call is neglected.')                             
      RETURN                                                            
C                                                                       
                                                                        
  200 X     = DX*1.0                                                    
                                                                        
          IX    = -1                                                    
          IP1   = MAPL(2,IHIST)                                         
          XMIN  = BUFF(IP1)                                             
          XMAX  = BUFF(IP1+1)                                           
          NXBIN = IBUF(IP1+2)                                           
          DEV   = BUFF(IP1+3)                                           
          IF(     X .LT. XMIN ) THEN                                    
                  IX   = 0                                              
          ELSEIF( X .GT. XMAX ) THEN                                    
                 IX   = NXBIN + 1                                       
          ELSE                                                          
                 IX   = INT((X - XMIN)/DEV + 1.0)                       
                 IF( IX .GT. NXBIN ) IX = NXBIN                         
          ENDIF                                                         
C        PRINT*,'ID, IHIST, IFBASE =',ID,IHIST,(IFBASE(I),I=1,NHIST)    
                                                                        
      IF( IBASES .EQ. 1 ) THEN                                          
                                                                        
          IP2       = MAPL(3,IHIST) + IX                                
          IBUF(IP2) = IBUF(IP2) + 1                                     
          FXWGT     = FX*WGT                                            
          IP2       = IP2 + 52                                          
          BUFF(IP2) = BUFF(IP2) + FXWGT                                 
          IP2       = IP2 + 52                                          
          BUFF(IP2) = BUFF(IP2) + FXWGT*FXWGT                           
*   Add March 1994                                                      
          IFBASE(IHIST) = 1                                             
                                                                        
      ELSE                                                              
C        PRINT*,'ID, IHIST, IFBASE =',ID,IHIST,(IFBASE(I),I=1,NHIST)    
                                                                        
         IP3        =  MAPL(4,IHIST)                                    
         IBUF(IP3)  = IX                                                
                                                                        
      ENDIF                                                             
                                                                        
C                                                                       
      RETURN                                                            
      END                                                               
************************************************************************
*    ============================================                      *
      SUBROUTINE XHINIT(ID,DXMIN,DXMAX,NBIN,TNAME)                      
*    ============================================                      *
* ((Function))                                                         *
*     To define a histogram.                                           *
* ((Input))                                                            *
*    ID   : Histogram identification number                            *
*    DXMIN: Lower limit of the histogram                               *
*    DXMAX: Upper limit of the histogram                               *
*    NBIN : Number of bins for the histogram (Max. is 50 )             *
*    TNAME: Title of the histogram in the character string (upto 64    *
*            characters)                                               *
* ((Author))                                                           *
*    S.Kawabata    June '90                                            *
*                                                                      *
************************************************************************
                                                                        
      REAL*8 DXMIN, DXMAX                                               
      CHARACTER*(*) TNAME                                               
      CHARACTER*68  NAME                                                
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
                                                                        
*     COMMON/XHCNTL/ LOCK                                               
      COMMON/PLOTLU/ LU                                                 
                                                                        
      IF( NHIST .GE. NHS ) THEN                                         
*         IF( LOCK .NE. 0 ) RETURN                                      
          IF( LU .GT. 0 ) THEN                                          
              WRITE(LU,9000) NHIST,ID                                   
          ENDIF                                                         
 9000     FORMAT(1X,'Number of Histograms exceeds ',I3,' at ID = ',I3,  
     .            /1X,'This call is neglected.')                        
          RETURN                                                        
      ENDIF                                                             
                                                                        
      IEXIST = 0                                                        
      I  = IABS(MOD( ID, 13 )) + 1                                      
      NH = XHASH(1,I)                                                   
                                                                        
      IF( NH .EQ. 1 ) THEN                                              
            IF( ID .EQ. MAPL( 1, XHASH(2,I))) THEN                      
*               IF( LOCK .NE. 0 ) RETURN                                
                IEXIST = XHASH(2,I)                                     
            ENDIF                                                       
      ELSEIF( NH .GT. 1 ) THEN                                          
          DO 100 K = 2, NH+1                                            
            IF( ID .EQ. MAPL( 1, XHASH(K,I))) THEN                      
*               IF( LOCK .NE. 0 ) RETURN                                
                IEXIST = XHASH(K,I)                                     
                GO TO 110                                               
            ENDIF                                                       
  100    CONTINUE                                                       
  110    CONTINUE                                                       
      ENDIF                                                             
      XMIN  = DXMIN*1.0                                                 
      XMAX  = DXMAX*1.0                                                 
                                                                        
      IF( IEXIST .GT. 0 ) THEN                                          
          IF( LU .GT. 0 ) THEN                                          
              WRITE(LU,9100) ID                                         
          ENDIF                                                         
 9100     FORMAT(1X,'Histogram ID (',I3,' ) exists already.')           
          IP1    =  MAPL(2,IEXIST)                                      
          IF(( XMIN .EQ. BUFF(IP1))   .AND.                             
     .       ( XMAX .EQ. BUFF(IP1+1)) .AND.                             
     .       ( NBIN .EQ. IBUF(IP1+2)) )    THEN                         
               IF( LU .GT. 0 ) THEN                                     
                   WRITE(LU,9110)                                       
               ENDIF                                                    
 9110          FORMAT(1X,' This call is neglected.')                    
               RETURN                                                   
          ENDIF                                                         
          IF( LU .GT. 0 ) THEN                                          
              WRITE(LU,9120) ID,XMIN,XMAX,NBIN                          
          ENDIF                                                         
 9120     FORMAT(1X,'Histogram ( ID =',I3,' ) parameters are replaced', 
     .          /1X,'by the following new parameters :',                
     .          /1X,' XMIN(',E12.5,')  XMAX(',E12.5,' )  NBIN(',I4,' )')
      ENDIF                                                             
                                                                        
      IF((NHIST .GE. NHS) .AND. (ID .GT. 0) ) THEN                      
*         IF( LOCK .NE. 0 ) RETURN                                      
          IF( LU .GT. 0 ) THEN                                          
              WRITE(LU,9000) NHS,ID                                     
          ENDIF                                                         
         RETURN                                                         
      ENDIF                                                             
                                                                        
      IF(NBIN  .GT. 50 ) THEN                                           
         IF( LU .GT. 0 ) THEN                                           
             WRITE(LU,9200) NBIN,ID                                     
         ENDIF                                                          
 9200    FORMAT(1X,'Bin size (',I3,' )  exceeds 50 at ID =',I5,         
     .         /1X,' This call is neglected.')                          
         RETURN                                                         
      ENDIF                                                             
      IF(XMIN  .GE. XMAX ) THEN                                         
         IF( LU .GT. 0 ) THEN                                           
             WRITE(LU,9300) ID                                          
         ENDIF                                                          
 9300    FORMAT(1X,'Lower limit is larger than upper at ID =',I5,       
     .         /1X,' This call is neglected.')                          
         RETURN                                                         
      ENDIF                                                             
      IF(XHASH(1,I) .GE. NHS) THEN                                      
         IF( LU .GT. 0 ) THEN                                           
             WRITE(LU,9400) I                                           
         ENDIF                                                          
 9400    FORMAT(1X,I5,'-th Hash table overflow',                        
     .         /1X,' This call is neglected.')                          
         RETURN                                                         
      ENDIF                                                             
                                                                        
      IF( IEXIST .GT. 0 ) THEN                                          
          NHST     = IEXIST                                             
      ELSE                                                              
          NHIST        = NHIST + 1                                      
          XHASH(1,I)   = XHASH(1,I) + 1                                 
          K            = XHASH(1,I) + 1                                 
          XHASH(K,I)   = NHIST                                          
          NHST         = NHIST                                          
          IP1    = NW + 1                                               
          NW  = NW + 281                                                
          MAPL(1,NHST)  = ID                                            
          MAPL(2,NHST)  = IP1                                           
      ENDIF                                                             
         BUFF(IP1     ) = XMIN                                          
         BUFF(IP1 +  1) = XMAX                                          
         IBUF(IP1 +  2) = NBIN                                          
         DEV            = XMAX - XMIN                                   
         BUFF(IP1 +  3) = DEV/NBIN                                      
      IP2   = IP1 + 4                                                   
         MAPL(3,NHST)  = IP2                                            
      IP3   = IP1 + 264                                                 
         MAPL(4,NHST)  = IP3                                            
         IBUF(IP3)     = -1                                             
                                                                        
         I1   = IP3 + 1                                                 
         I2   = I1 + 15                                                 
         NAME = TNAME                                                   
         READ(NAME,9800) (BUFF(I),I=I1,I2)                              
 9800    FORMAT(16A4)                                                   
                                                                        
C                                                                       
 1000 CONTINUE                                                          
      RETURN                                                            
      END                                                               
C***********************************************************************
C*                                                                     *
C*=============================================                        *
C*    SUBROUTINE XHORDR( VAL, F2, ORDER, IORDR)                        *
C*=============================================                        *
C*((Function))                                                         *
C*    To resolve the real number VAL into mantester and exponent parts.*
C*  When VAL = 1230.0 is given, output are                             *
C*        F2 = 1.2  and ORDER = 4.0.                                   *
C*((Input))                                                            *
C*  VAL  : Real*4 value                                                *
C*((Output))                                                           *
C*  F2   : The upper two digits is given                               *
C*  ORDER: Order is given                                              *
C*  IORDR: Exponent is given                                           *
C*((Author))                                                           *
C*  S.Kawabata                                                         *
C*                                                                     *
C***********************************************************************
                                                                        
      SUBROUTINE XHORDR(VAL, F2, ORDER, IORDR)                          
                                                                        
      IF( VAL .NE. 0.0 ) THEN                                           
          ORDER    =  LOG10( VAL )                                      
          IORDR    =  INT( ORDER )                                      
          IF( ORDER .LT. 0.0 ) IORDR = IORDR - 1                        
          ORDER  = 10.0**IORDR                                          
          F2     = VAL/ORDER                                            
      ELSE                                                              
          IORDR  = 0                                                    
          ORDER  = 1.0                                                  
          F2    = 0.0                                                   
      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*    =====================================                             *
      SUBROUTINE XHPLOT( LU, IFG, IHIST )                               
*    =====================================                             *
* ((Purpose))                                                          *
*      To print histograms for BASES and SPRING.                       *
* ((Input))                                                            *
*      IFG  : Flag which indicats whether this is called by BASES      *
*             or SPRING.  IFG = ( 0 / anyother) = ( By BASES/ SPRING)  *
*      IHIST: Serial number of the histogram                           *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata    June '90 at KEK                                  *
*     Last update     March '94                                        *
*                                                                      *
************************************************************************
                                                                        
      REAL*8         SCALLS,WGT,TI,TSI,TACC                             
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT                          
                                                                        
      PARAMETER ( NHS = 50, NSC = 50 )                                  
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD                
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),        
     .              NHIST, MAPL(4,NHS),                                 
     .              NSCAT, MAPD(4,NSC),                                 
     .              NW                                                  
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )                         
      REAL*4         BUFF( 281*NHS + 2527*NSC )                         
      EQUIVALENCE (IBUF(1),BUFF(1))                                     
                                                                        
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP                          
                                                                        
      REAL  VAL(0:51),VLOG(0:51),VERR(0:51)                             
      CHARACTER*50 CHARR,CHAR1                                          
      CHARACTER*52 SCALE                                                
      CHARACTER*1  BLNK,STAR,OO,AI,CN                                   
      DATA  YMAX / 50/                                                  
      DATA  BLNK /' '/, STAR /'*'/, OO /'O'/, AI /'I'/                  
                                                                        
      CN    = CHAR(12)                                                  
      IP3   = MAPL(4,IHIST)                                             
      IF(     IFG .EQ. 0 ) THEN                                         
            IF( IPNT .EQ. 0 ) THEN                                      
                WRITE(LU,9000)                                          
            ELSE                                                        
                WRITE(LU,9005) CN                                       
            ENDIF                                                       
 9000       FORMAT(/1H1)                                                
 9005       FORMAT(A1)                                                  
            WRITE(LU,9100) MAPL(1,IHIST),(BUFF(I),I=IP3+1,IP3+16)       
 9100       FORMAT(1X,'Histogram (ID =',I3,' ) for ',16A4)              
      ELSEIF( IFG .EQ. -10 ) THEN                                       
            IF( IPNT .EQ. 0 ) THEN                                      
                WRITE(LU,9000)                                          
            ELSE                                                        
                WRITE(LU,9005) CN                                       
            ENDIF                                                       
            WRITE(LU,9102) (BUFF(I),I=IP3+1,IP3+16)                     
 9102       FORMAT(5X,16A4)                                             
      ELSE                                                              
            IF( IPNT .EQ. 0 ) THEN                                      
                WRITE(LU,9000)                                          
            ELSE                                                        
                WRITE(LU,9005) CN                                       
            ENDIF                                                       
            WRITE(LU,9105) MAPL(1,IHIST),(BUFF(I),I=IP3+1,IP3+16)       
 9105       FORMAT(                                                     
     .      1X,'Additional Histogram (ID =',I3,' ) for ',16A4)          
      ENDIF                                                             
                                                                        
      IP1   = MAPL(2,IHIST)                                             
      XMIN  = BUFF(IP1)                                                 
      XMAX  = BUFF(IP1+1)                                               
      NXBIN = IBUF(IP1+2) + 1                                           
      DEV   = BUFF(IP1+3)                                               
      IP2   = MAPL(3,IHIST)                                             
                                                                        
      IF( IFG .EQ. 0 ) THEN                                             
C         NTOTAL     = SCALLS                                           
          FACT       = 1./(SCALLS*DEV)                                  
          IPF   = IP2 + 156                                             
          IPF2  = IPF + 52                                              
          VAL(0)     = BUFF(IPF)/SCALLS                                 
          VAL(NXBIN) = BUFF(IPF+NXBIN)/SCALLS                           
          VMAX       = FACT*BUFF(IPF+1)                                 
          VMIN       = VMAX                                             
          DO  50 I   = 1,NXBIN-1                                        
              TX     = BUFF(I+IPF)                                      
              NX     = IBUF(I+IP2)                                      
              VLS    = TX*FACT                                          
              IF( VMAX .LT. VLS ) VMAX = VLS                            
              IF( VMIN .GT. VLS ) VMIN = VLS                            
              VAL(I) = VLS                                              
              IF( NX .GT. 1 ) THEN                                      
                  DEV2   =  NX*BUFF(I+IPF2)-TX*TX                       
                  IF( DEV2 .LE. 0.0 ) THEN                              
                      VERR(I)= 0.0                                      
                  ELSE                                                  
                      VERR(I)= FACT*SQRT( DEV2/( NX-1 ))                
                  ENDIF                                                 
              ELSEIF( NX .EQ. 1 ) THEN                                  
                  VERR(I)= VLS                                          
              ELSE                                                      
                  VERR(I)= 0.0                                          
              ENDIF                                                     
                                                                        
   50     CONTINUE                                                      
      ELSE                                                              
          IPX   = IP2 + 52                                              
          VAL(0)     = BUFF(IPX)                                        
          VAL(NXBIN) = BUFF(IPX+NXBIN)                                  
          NTOTAL     = INT(VAL(0)) + INT(VAL(NXBIN))                    
          VMIN       = 0.0                                              
          VMAX       = VMIN                                             
          DO  55 I   = 1,NXBIN-1                                        
              VLS    = BUFF(I+IPX)                                      
              NTOTAL = INT(VLS) + NTOTAL                                
              IF( VMAX .LT. VLS ) VMAX = VLS                            
              VAL(I) = VLS                                              
              IF( VLS .GT. 0.0 ) THEN                                   
                  VERR(I) = SQRT(VLS)                                   
              ELSE                                                      
                  VERR(I) = 0.0                                         
              ENDIF                                                     
   55     CONTINUE                                                      
       ENDIF                                                            
***                                                                     
       IF( VMAX .EQ. 0.0 .AND. VMIN .EQ. 0.0) THEN                      
           V0 = VAL(0)                                                  
           VM = VAL(NXBIN)                                              
           IF( V0 .GE. 0.0 .AND. VM .GE. 0.0 ) THEN                     
               VMIN  = 0.0                                              
               IF( V0 .GT. VM  ) THEN                                   
                   VMAX = V0                                            
               ELSE                                                     
                   VMAX = VM                                            
               ENDIF                                                    
           ELSEIF( V0 .LT. 0.0 .AND. VM .LT. 0.0 ) THEN                 
               VMAX  = 0.0                                              
               IF( V0 .LT. VM ) THEN                                    
                   VMIN  = V0                                           
               ELSE                                                     
                   VMIN  = VM                                           
               ENDIF                                                    
           ELSEIF( V0 .GT. VM ) THEN                                    
               VMAX  = V0                                               
               VMIN  = VM                                               
           ELSE                                                         
               VMAX  = VM                                               
               VMIN  = V0                                               
           ENDIF                                                        
       ENDIF                                                            
***                                                                     
       IF( VMIN .GE. 0.0 ) THEN                                         
C//VV                                                                   
           IF( VMAX .GT. 0.0 ) THEN                                     
               VLMAX = LOG10(VMAX)                                      
           ELSE                                                         
               VLMAX = 2.0                                              
           ENDIF                                                        
C//                                                                     
           VLMIN = VLMAX                                                
           DO  60 I = 0,NXBIN                                           
               IF( VAL(I) .GT. 0.0 ) THEN                               
                   VLS   = LOG10( VAL(I) )                              
                   IF( I .GT. 0 .AND. I .LT. NXBIN ) THEN               
                       IF( VLS .LT. VLMIN ) VLMIN = VLS                 
                   ENDIF                                                
                   VLOG(I)  = VLS                                       
C//VV                                                                   
C              ELSE                                                     
C                  VLOG(I)  = 0.0                                       
               ENDIF                                                    
   60      CONTINUE                                                     
                                                                        
           IF( VLMIN .LT. 0.0) THEN                                     
               VXMIN = IFIX(VLMIN) - 1.0                                
           ELSE                                                         
               VXMIN = IFIX(VLMIN)                                      
           ENDIF                                                        
           VXMAX = VLMAX                                                
           IFLG  = 1                                                    
           CALL XHRNGE( IFLG, VXMIN, VXMAX, VLMIN, VLMAX, VLSTP )       
           UNITL = (VLMAX-VLMIN)/YMAX                                   
                                                                        
       ENDIF                                                            
                                                                        
       IFLG   = 0                                                       
       IF( VMAX .GT. 0.0 ) THEN                                         
           IF( VMIN .GE. 0.0 ) THEN                                     
               VXMAX  = 1.2*VMAX                                        
               VXMIN  = 0.0                                             
               CALL XHRNGE( IFLG, VXMIN, VXMAX, VMIN, VMAX, VSTP )      
           ELSE                                                         
               VXMAX  = 1.1*VMAX                                        
               VXMIN  = 1.1*VMIN                                        
               CALL XHRNGE( IFLG, VXMIN, VXMAX, VMIN, VMAX, VSTP )      
           ENDIF                                                        
       ELSE                                                             
          VXMAX  = 0.0                                                  
          VXMIN  = 1.1*VMIN                                             
          CALL XHRNGE( IFLG, VXMIN, VXMAX, VMIN, VMAX, VSTP )           
       ENDIF                                                            
                                                                        
       UNIT  = (VMAX-VMIN)/YMAX                                         
                                                                        
       CALL XHSCLE( IFLG, VMIN, VMAX, VSTP, UNIT, SCALE, CHAR1 )        
C                                                                       
C                                                                       
       IF( IFG .EQ. 0 ) THEN                                            
           WRITE(LU,9150)                                               
 9150      FORMAT(30X,'Linear Scale indicated by "*"')                  
           WRITE(LU,9200) SCALE                                         
 9200      FORMAT(1X,'    x      d(Sigma)/dx    ',A52)                  
           WRITE(LU,9250) CHAR1                                         
 9250      FORMAT(1X,                                                   
     .                '+-------+------------------+',                   
     .           A50 )                                                  
       ELSE                                                             
             WRITE(LU,9210) NTOTAL                                      
 9210        FORMAT(1X,'Total =',I10,' events',                         
     .        3X,'"*" : No. of events in Linear scale.')                
             WRITE(LU,9205) SCALE                                       
 9205        FORMAT(1X,'   x      Lg(dN/dx)  dN/dx',A52)                
             WRITE(LU,9251) CHAR1                                       
 9251        FORMAT(1X,                                                 
     .             '+-------+----------+-------+',                      
     .       A50 )                                                      
       ENDIF                                                            
                                                                        
       VX    = ABS(XMAX)                                                
       XM    = ABS(XMIN)                                                
       IF( XM .GT. VX ) VX = XM                                         
                                                                        
       CALL XHORDR( VX, F2, ORD, IORD )                                 
                                                                        
       IF( VMIN .LT. 0.0 ) THEN                                         
           V1    = VMIN                                                 
           NUMBL = 1                                                    
           DO 150 I = 1, 80                                             
              V2    = V1 + UNIT                                         
              IF( V1 .LE. 0.0 .AND. V2 .GE. 0.0 ) THEN                  
                  NUMBL  = I                                            
                  GO TO 180                                             
              ENDIF                                                     
              V1    = V2                                                
  150      CONTINUE                                                     
       ENDIF                                                            
                                                                        
  180  DO 300 I = 0,NXBIN                                               
          VX   = VAL(I)                                                 
          IF( VMIN .GE. 0.0 ) THEN                                      
              IF( VX .GT. 0.0 ) THEN                                    
                  NUMBL  = (VLOG(I) - VLMIN)/UNITL + 1.0                
                  NUMB   = VX/UNIT + 1.0                                
              ELSE                                                      
                  NUMBL  = 0                                            
                  NUMB   = 0                                            
              ENDIF                                                     
              IF( NUMB .GT. 50 ) NUMB = 50                              
              IF( NUMBL.GT. 50 ) NUMBL= 50                              
              DO 200 K = 1,50                                           
                 IF(     ( K .GT. NUMBL) .AND. (K .GT. NUMB ) ) THEN    
                           IF( K .EQ. 50 ) THEN                         
                               CHARR(K:K) = AI                          
                           ELSE                                         
                               CHARR(K:K) = BLNK                        
                           ENDIF                                        
                 ELSEIF( ( K .LE. NUMBL) .AND. (K .GT. NUMB )) THEN     
                           CHARR(K:K) = OO                              
                 ELSEIF( ( K .GT. NUMBL) .AND. (K .LE. NUMB )) THEN     
                           CHARR(K:K) = STAR                            
                 ELSEIF( ( K .LE. NUMBL) .AND. (K .LE. NUMB)) THEN      
                           IF( NUMB .GE. NUMBL ) THEN                   
                               CHARR(K:K) = OO                          
                           ELSE                                         
                               CHARR(K:K) = STAR                        
                           ENDIF                                        
                 ENDIF                                                  
  200         CONTINUE                                                  
          ELSE                                                          
                                                                        
              V1          = VMIN                                        
              NHIG        = 1                                           
              DO 220  J = 1, 50                                         
                 V2     = V1 + UNIT                                     
                 IF( VX .GE. V1 .AND. VX .LT. V2 ) THEN                 
                     NHIG   = J                                         
                     GO TO 240                                          
                 ENDIF                                                  
                 V1    = V2                                             
  220         CONTINUE                                                  
  240         NLOW   = NUMBL                                            
              IF( NHIG .LT. NLOW) THEN                                  
                  NX    = NHIG                                          
                  NHIG  = NLOW                                          
                  NLOW  = NX                                            
              ENDIF                                                     
                                                                        
              DO 250 K = 1, 49                                          
                 IF(     K .EQ. NUMBL ) THEN                            
                         CHARR(K:K) = AI                                
                 ELSEIF( K .GT. NHIG ) THEN                             
                         CHARR(K:K) = BLNK                              
                 ELSEIF( K .LT. NLOW ) THEN                             
                         CHARR(K:K) = BLNK                              
                 ELSE                                                   
                     IF( K .EQ. NHIG .AND. K .EQ. NLOW) THEN            
                         CHARR(K:K) = AI                                
                     ELSE                                               
                         CHARR(K:K) = STAR                              
                     ENDIF                                              
                 ENDIF                                                  
  250         CONTINUE                                                  
              CHARR(50:50) = AI                                         
          ENDIF                                                         
                                                                        
          IF( IFG .EQ. 0 ) THEN                                         
                                                                        
              NX     = IBUF(I+IP2)                                      
              VX     = VAL(I)                                           
              VX1    = VX                                               
              IF( VX .LT. 0.0 ) VX1 = -VX                               
                                                                        
                                                                        
              IF( I .EQ. 0 .OR. I. EQ. NXBIN ) THEN                     
                  CALL XHORDR( VX1, F2, ORDER, IORDR )                  
                  F2     = VX/ORDER                                     
                  WRITE(LU,9300) IORD,F2,IORDR,CHARR                    
 9300             FORMAT(1X,'I  E',I3,' I',F6.3,8X,'E',I3,              
     .                                     'I',A50)                     
              ELSE                                                      
                  XM    = (XMIN + DEV*(I-1))/ORD                        
                  VE     = VERR(I)                                      
                  IF( VE .GT. VX1 ) THEN                                
                      CALL XHORDR(  VE, F2, ORDER, IORDR )              
                  ELSE                                                  
                      CALL XHORDR( VX1, F2, ORDER, IORDR )              
                  ENDIF                                                 
                  F2   = VX/ORDER                                       
                  VE   = VE/ORDER                                       
                  WRITE(LU,9340) XM,F2,VE,IORDR,CHARR                   
 9340             FORMAT(1X,'I', F6.3,' I',F6.3,'+-',F5.3,' E',I3,      
     .                                    'I',A50)                      
             ENDIF                                                      
          ELSE                                                          
             NX  = VAL(I)                                               
             VX     = VAL(I)                                            
             VX1    = VX                                                
             IF( VX .LT. 0.0 ) VX1 = -VX                                
             CALL XHORDR( VX1, F2, ORDER, IORDR )                       
             F2     = VX/ORDER                                          
             IF( I .EQ. 0 .OR. I .EQ. NXBIN ) THEN                      
                 WRITE(LU,9400) IORD,F2,IORDR,NX,CHARR                  
 9400            FORMAT(1X,'I  E',I3,' I',F6.3,'E',I3,'I',              
     .                                            I7,'I',A50)           
             ELSE                                                       
                   XM  = (XMIN + DEV*(I - 1))/ORD                       
                   WRITE(LU,9440) XM,F2,IORDR,NX,CHARR                  
 9440              FORMAT(1X,'I',F6.3,' I',F6.3,'E',I3,'I',             
     .                                        I7,'I',A50)               
             ENDIF                                                      
          ENDIF                                                         
  300  CONTINUE                                                         
                                                                        
       IF( VMIN .GE. 0.0 ) THEN                                         
           CALL XHSCLE( 1, VLMIN, VLMAX, VLSTP, UNITL, SCALE, CHAR1)    
           VXMIN  = 10**VLMIN                                           
       ENDIF                                                            
                                                                        
       IF( IFG .EQ. 0 ) THEN                                            
           WRITE(LU,9250) CHAR1                                         
           IF( VMIN .GE. 0.0 ) THEN                                     
               WRITE(LU,9200) SCALE                                     
               WRITE(LU,9260)                                           
 9260          FORMAT(30X,'Logarithmic Scale indicated by "O"')         
           ELSE                                                         
               WRITE(LU,9200) SCALE                                     
           ENDIF                                                        
       ELSE                                                             
           WRITE(LU,9251) CHAR1                                         
           WRITE(LU,9205) SCALE                                         
           WRITE(LU,9360)                                               
 9360      FORMAT(30X,'"O" : No. of Events in Log. scale.')             
       ENDIF                                                            
                                                                        
C                                                                       
  500  CONTINUE                                                         
                                                                        
      RETURN                                                            
      END                                                               
C***********************************************************************
C*                                                                     *
C*============================================================         *
C*  SUBROUTINE XHRNGE( IFLG, VMIN, VMAX, VTMIN, VTMAX, STEP)           *
C*============================================================         *
C*((Function))                                                         *
C*    Determine the vertical range of the histogram.                   *
C*((Input))                                                            *
C*    IFLG   : Flag which indicates whether logarithmic or linear      *
C*             scale.  IFLG = ( 1 / any other ) = ( log / linear )     *
C*    VMIN,VMAX : Minimum and maximum values of vertical window.       *
C*((Output))                                                           *
C*    VTMIN,VTMAX : Minimum and maxmum values of optimized vertical    *
C*                  window.                                            *
C*    STEP   : step of scale for the optimized vertical window         *
C*((Author))                                                           *
C*    S.Kawabata    Oct '85  at KEK                                    *
C*                                                                     *
C***********************************************************************
C                                                                       
      SUBROUTINE XHRNGE( IFLG, VMIN, VMAX, VTMIN, VTMAX, STEP)          
C                                                                       
C     IFLG =    1 : Log scale                                           
C            other: Linear scale                                        
C                                                                       
      PARAMETER ( NBIN  = 25 )                                          
      REAL    WIND(NBIN),STP1(NBIN),STP2(NBIN)                          
C                                                                       
      DATA WIND/                                                        
     .   1.00, 1.10, 1.20, 1.30, 1.40, 1.50, 1.60, 1.80, 2.00,  2.20,   
     .   2.50, 2.70, 3.00, 3.30, 3.60, 4.00, 4.50, 5.00, 5.50,  6.00,   
     .   6.50, 7.00, 8.00, 9.00, 10.0/                                  
*     DATA STP1/                                                        
*    .   0.20, 0.22, 0.30, 0.26, 0.28, 0.30, 0.32, 0.36, 0.40,  0.44,   
*    .   0.50, 0.54, 0.60, 0.66, 0.60, 0.80, 0.90, 1.00, 1.10,  1.00,   
*    .   1.30, 1.00, 1.60, 1.80, 2.00/                                  
      DATA STP1/                                                        
     .   0.250,0.275,0.300,0.325,0.350,0.375,0.400,0.450,0.500,0.550,   
     .   0.625,0.675,0.750,0.825,0.900,1.000,1.125,1.250,1.375,1.500,   
     .   1.625,1.750,2.000,2.250,2.500/                                 
      DATA STP2/                                                        
     .   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,  1.00,   
     .   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,  2.00,   
     .   2.00, 2.00, 2.00, 2.00, 2.00/                                  
C                                                                       
          XMAX   = VMAX                                                 
          XMIN   = VMIN                                                 
          IFLAG  = IFLG                                                 
          IF( IFLG .NE. 1 .AND. VMIN .LT. 0.0 ) THEN                    
              IF( VMAX .LE. 0.0 )THEN                                   
                  IFLAG  = 2                                            
                  XMAX  = - VMIN                                        
                  XMIN  =  0.0                                          
              ELSE                                                      
                  AVMIN  = - VMIN                                       
                  XMIN  =0.0                                            
                  IF( VMAX .GE. AVMIN ) THEN                            
                      IFLAG  = 3                                        
                      XMAX  = VMAX                                      
                      XMIN1 = AVMIN                                     
                  ELSE                                                  
                      IFLAG  = 4                                        
                      XMAX  = AVMIN                                     
                      XMIN1 = VMAX                                      
                  ENDIF                                                 
              ENDIF                                                     
          ENDIF                                                         
          DSCALE = XMAX - XMIN                                          
          CALL XHORDR( DSCALE, DSF2, DSORDR, IORD)                      
                                                                        
          DO 100 I = 2, 25                                              
             IF( DSF2 .GE. WIND(I-1) .AND.                              
     .           DSF2 .LE. WIND( I )       ) GO TO 200                  
 100      CONTINUE                                                      
          I = 25                                                        
C                                                                       
 200      CONTINUE                                                      
                                                                        
          XMAX = WIND(I)*DSORDR + XMIN                                  
          IF(     DSORDR .GE. 10.0 .OR. IFLG .NE. 1 ) THEN              
                  STEP1  = STP1(I)                                      
                  STEP   = STEP1*DSORDR                                 
          ELSE                                                          
                  STEP1  = STP2(I)                                      
                  STEP   = STEP1                                        
          ENDIF                                                         
                                                                        
          IF(     IFLAG .LE. 1 ) THEN                                   
                  VTMAX  = XMAX                                         
                  VTMIN  = XMIN                                         
          ELSEIF( IFLAG .EQ. 2 ) THEN                                   
                  VTMAX  = XMIN                                         
                  VTMIN  = -XMAX                                        
          ELSE                                                          
                                                                        
                  XPLUS   = 0.0                                         
                  DO 300 J = 1, 10                                      
                     XPLUS = XPLUS + STEP                               
                     IF( XPLUS .GT. XMIN1 ) GO TO 400                   
 300              CONTINUE                                              
 400              XMIN = XPLUS                                          
                  XMAX = XMAX                                           
                  IF( IFIX((WIND(I)+0.1)/STEP1)+J .GT. 7 ) THEN         
                      STEP = 2.0*STEP                                   
                  ENDIF                                                 
                  IF( IFLAG .EQ. 3 ) THEN                               
                      VTMAX  = XMAX                                     
                      VTMIN  = -XMIN                                    
                  ELSE                                                  
                      VTMAX  = XMIN                                     
                      VTMIN  = -XMAX                                    
                  ENDIF                                                 
          ENDIF                                                         
C                                                                       
      RETURN                                                            
      END                                                               
************************************************************************
*    =========================================================         *
      SUBROUTINE XHSCLE( IFLG,VMIN,VMAX,VSTP,UNIT,SCALE,CHAR)           
*    =========================================================         *
* ((Function))                                                         *
*     Determine the vertical scale and make it's format                *
* ((Input))                                                            *
*     IFLG   : Flag which indicates whether logarithmic or linear      *
*              scale.  IFLG = ( 1 / any other ) = ( log / linear )     *
*     VMIN,VMAX : Minimum and maximum values of vertical window.       *
*     VSTEP  : Step of unit scale                                      *
*     UNIT   : Unit of one mark *(or o)                                *
* ((Output))                                                           *
*     NSCL   : Number of scale mark                                    *
*     NBLK   : Number of blanks between scale marks                    *
*     CHAR   : Format of scale                                         *
* ((Author))                                                           *
*     S.Kawabata    Oct '85  at KEK                                    *
*                                                                      *
************************************************************************
                                                                        
      CHARACTER*50 CHAR                                                 
      CHARACTER*52 SCALE                                                
      CHARACTER*1 PLUS,MINUS                                            
      DATA PLUS /'+'/, MINUS /'-'/                                      
                                                                        
C     IFLG =    1 : Log scale                                           
C            other: Linear scale                                        
      WRITE(SCALE,9000)                                                 
 9000 FORMAT(5('          '))                                           
      IF( IFLG .EQ. 1 ) THEN                                            
          SC  = 10.**VMIN                                               
      ELSE                                                              
          SC  = VMIN                                                    
      ENDIF                                                             
                                                                        
      WRITE(SCALE(1:8),9100) SC                                         
 9100 FORMAT(1P,E8.1)                                                   
      I2    = 8                                                         
      STV   = VSTP + VMIN                                               
      STV1  = STV                                                       
      VAL1  = VMIN                                                      
      CHAR(50:50) = PLUS                                                
      DO  100   I = 1, 49                                               
          VAL2    = VAL1 + UNIT                                         
          IF( STV .GE. VAL1 .AND. STV .LT. VAL2 ) THEN                  
              CHAR(I:I)  = PLUS                                         
              NSCL       = NSCL + 1                                     
              IF( IFLG .EQ. 1 ) THEN                                    
                 SC          = 10.0**STV                                
              ELSE                                                      
                 IF(     STV1 .EQ. 0.0 ) THEN                           
                         SC           = STV                             
                 ELSEIF( ABS(STV/STV1) .LT. 1.E-2 ) THEN                
                         SC           = 0.0                             
                 ELSE                                                   
                         SC          = STV                              
                 ENDIF                                                  
                 STV1       = STV                                       
              ENDIF                                                     
              STV  = STV + VSTP                                         
              IF( I2 .LT. I-1 ) THEN                                    
                  I2   = I + 8                                          
                  IF( I2 .LE. 52 ) THEN                                 
                      WRITE(SCALE(I+1:I2),9100) SC                      
                  ENDIF                                                 
              ENDIF                                                     
          ELSE                                                          
              CHAR(I:I) = MINUS                                         
          ENDIF                                                         
          VAL1      = VAL2                                              
  100 CONTINUE                                                          
C                                                                       
      IF( NSCL .EQ. 0 ) THEN                                            
          IF( IFLG .EQ. 1 ) THEN                                        
             SC       = 10.0**VMAX                                      
          ELSE                                                          
             SC       = VMAX                                            
          ENDIF                                                         
          WRITE(SCALE(44:52),9100) SC                                   
      ENDIF                                                             
C                                                                       
      RETURN                                                            
      END                                                               
#endif
