#include "PILOT.inc"
      SUBROUTINE SSMHC(MHCNEG)
C-----------------------------------------------------------------------
C
C         Calculates charged Higgs mass 
C         (scalar Higgs mixing angle) using radiative
C         corrections calculated by M. Bisset
C         and save results in /SSPAR/.
C
C         Both top and bottom couplings are now 
C         included.  Non-degenerate mixed squark
C         masses and A-terms are also included.
C         The D-terms from the squark mass matrix
C         (terms prop. to g**2 * Yukawa coupling)
C         are included by default.
C
C         There is an arbitrary mass scale that must
C         chosen to avoid dimensionful logarithms.
C         The choice does not matter if D-terms are
C         not included, but it does matter if D-terms
C         are included. 
C     
C         Arbitrary mass scale set to
C              QQQ = HIGFRZ = SQRT(AMTLSS*AMTRSS)
C         Updated to include running masses as 2-loop effect
C
C         It is assumed that the A-terms are real.  
C         (Complex A-terms are taken into account 
C         much of the subroutine; but, not in all
C         cases.)
C
C         Fix for NUHM models
C-----------------------------------------------------------------------
#ifdef IMPNONE_X
      IMPLICIT NONE
#endif
#include "sslun.inc"
#include "sssm.inc"
#include "sspar.inc"
#include "sugnu.inc"
C
      INTEGER MHCNEG
      REAL PI,PI2,SR2,G2,GP2,GGP,GG1,GG2
      REAL GGPM,GG3,GG4
      REAL TANB,COTB,COSB,SINB,BE
      REAL SINB2,COSB2,COS2B,SIN2B
      REAL V2,VP2,V,VP,VVP,VPVM,VVPP,MT,MB
      REAL MT2,MB2,MT4,MB4,FT2,FB2,FT,FB
      REAL MW2,MZ2,ZAP,QQQ2,EP,EP2,RR,MHP2
      REAL ATI,ABI,ATR,ABR,AT2,AB2
      REAL MSTL2,MSTR2,MSBL2,MSBR2
      REAL TLRM,BLRM,TLRP,BLRP
      REAL MST1SQ,MST2SQ,MSB1SQ,MSB2SQ
      REAL TTT1,TTT2,TTT3,BBB1,BBB2,BBB3
      REAL TEMPT,TEMPB,ROOTT,ROOTB,TM1B
C
C      For non-degenerate squarks
C
      REAL TERMT,TERMB,DHRT1,DHRT2,DHRB1,DHRB2
      REAL DHRPT1,DHRPT2,DHRPB1,DHRPB2
      REAL DHPST1,DHPST2,DHPSB1,DHPSB2
      REAL DHMST1,DHMST2,DHMSB1,DHMSB2
      REAL ATA1,ATA2,BTA1,BTA2,ABA1,ABA2,BBA1,BBA2
      REAL ATA1SQ,ATA2SQ,BTA1SQ,BTA2SQ
      REAL ABA1SQ,ABA2SQ,BBA1SQ,BBA2SQ
      REAL NABT1,NABT2,NABB1,NABB2
      REAL FTG,FBG,BABA,PT1B1,PT1B2,PT2B1,PT2B2
      REAL PDPST1,PDPST2,PDPSB1,PDPSB2
      REAL PDMST1,PDMST2,PDMSB1,PDMSB2
      REAL PDPMT1,PDPMT2,PDPMB1,PDPMB2
      REAL LMST1,LMST2,LMSB1,LMSB2
      REAL EMI1,EMI2,EM3,TEMPF
      REAL DVRR,DVRPRP,DVRRP,TRACE,DETV 
      REAL TERMSQ,GOLD2,MHC2
      REAL HIGFRZ,ASMB,MBMB,MBQP,ASMT,MTMT,MTQP,SUALFS
      DOUBLE PRECISION SSMQCD
C
      MHCNEG=0
      PI=4.*ATAN(1.)
      PI2=PI**2
      SR2=SQRT(2.)
      G2=4.*PI*ALFAEM/SN2THW
      GP2=G2*SN2THW/(1.-SN2THW)
      HIGFRZ=SQRT(MAX(AMZ**2,AMTLSS*AMTRSS*SIGN(1.,AMTLSS*AMTRSS)))
c      ASMB=SUALFS(AMBT**2,.36,AMTP,3)
c      MBMB=AMBT*(1.-4*ASMB/3./PI)
c      MBQP=SSMQCD(DBLE(MBMB),DBLE(HIGFRZ))
c      ASMT=SUALFS(AMTP**2,.36,AMTP,3)
c      MTMT=AMTP/(1.+4*ASMT/3./PI+(16.11-1.04*(5.-6.63/AMTP))*
c     $(ASMT/PI)**2)
c      MTQP=SSMQCD(DBLE(MTMT),DBLE(HIGFRZ))
      MT=MTQ
      MB=MBQ
      MT2=MT**2
      MB2=MB**2
      MT4=MT2**2
      MB4=MB2**2
      MW2=AMW**2
      MZ2=AMZ**2
      EP=TWOM1
      EP2=EP**2
      RR=RV2V1
      MHP2=AMHA**2
      TANB=1.0/RR
      COTB=RR
      BE=ATAN(1./RV2V1)
      SINB=SIN(BE)
      COSB=COS(BE)
      SINB2=SINB**2
      COSB2=COSB**2
      SIN2B=SIN(2.*BE)
      COS2B=COS(2.*BE)
      V2=2.0*MW2*SINB2/G2
      VP2=2.0*MW2*COSB2/G2
      V=SQRT(V2)
      VP=SQRT(VP2)
      VVP=SQRT(V2*VP2)
      VPVM=VP2-V2
      GGP=G2+GP2
      GGPM=G2-GP2
      GG1=G2-5.0*GP2/3.0
      GG2=G2-GP2/3.0
      GG3=G2+5.0*GP2/3.0
      GG4=G2+GP2/3.0
C
      VVPP=2.0*AMZ**2/GGP
      FT2=MT2/V2
      FB2=MB2/VP2
      FT=SQRT(FT2)
      FB=SQRT(FB2)
C
      MSTL2=AMTLSS**2*SIGN(1.,AMTLSS)
      MSTR2=AMTRSS**2*SIGN(1.,AMTRSS)
      MSBL2=AMBLSS**2
      MSBR2=AMBRSS**2
      TLRM=MSTL2-MSTR2
      BLRM=MSBL2-MSBR2 
      TLRP=MSTL2+MSTR2
      BLRP=MSBL2+MSBR2 
C
C          Charged Higgs mass calculation
C          (AAT and AAB are also assumed to be real)
C          
      ATR=AAT
      ABR=AAB
      ATI=0.0
      ABI=0.0
      AT2=ATR**2+ATI**2
      AB2=ABR**2+ABI**2
C
C      UNFORTUNATELY, I HAVE USED MY OLD CONVENTION
C      FOR THE STOP AND SBOTTOM EIGENVALUES HERE 
C      (T1 <==> T2 OF NOTATION IN X. TATA'S AND OTHER
C      PEOPLE'S NOTATION).  SO THE NEXT EIGHT LINES ARE
C      A FIX-UP UNTIL I GO THROUGH AND CHANGE THE
C      NOTATION THROUGHOUT THIS SUBROUTINE.
C
C
C     Calculate mt1,mt2,mb1,mb2 internally so masses consistent
C     with value of mbq and mtq used here
C
      MST2SQ=TLRP/2.+MZ2*COS2B/4.+MTQ**2-SQRT((TLRM/2.+COS2B/12.
     ,*(8*MW2-5*MZ2))**2+MTQ**2*(-AAT-EP*COTB)**2)
      MST1SQ=TLRP/2.+MZ2*COS2B/4.+MTQ**2+SQRT((TLRM/2.+COS2B/12.
     ,*(8*MW2-5*MZ2))**2+MTQ**2*(-AAT-EP*COTB)**2)
      MSB2SQ=BLRP/2.-MZ2*COS2B/4.+MBQ**2-SQRT((BLRM/2.-COS2B/12.
     ,*(4*MW2-MZ2))**2+MBQ**2*(-AAB-EP*TANB)**2)
      MSB1SQ=BLRP/2.-MZ2*COS2B/4.+MBQ**2+SQRT((BLRM/2.-COS2B/12.
     ,*(4*MW2-MZ2))**2+MBQ**2*(-AAB-EP*TANB)**2)
      IF (MST2SQ.LE.0..OR.MSB2SQ.LE.0.) THEN
        MHC2=-1.
        GO TO 50
      END IF
C
      QQQ2=HIGFRZ**2
      ZAP=1
C
C          Non-degenerate squarks and/or D-terms. Since D-terms are
C          always included, old dead code has been deleted. FEP
C
C          ROOTT recast as a sum of squares. Note that ROOTT=0 
C          could happen accidently and causes an error.
      TTT1=0.5*(MSTL2+MSTR2)+MT2+ZAP*VPVM*GGP/8.0
      TTT2=TLRM+ZAP*GG1*VPVM/4.0
      TTT3=4.0*FT2*(EP2*VP2+2.0*EP*VVP*ATR+AT2*V2)
      ROOTT=4*MT2*(COSB*EP + AAT*SINB)**2/SINB**2 +
     $(MSTL2-MSTR2 +
     $AMW**2*(-5*GP2/3 + G2)*(COSB**2 - SINB**2)/(2*G2))**2
      ROOTT=0.5*SQRT(ROOTT)
      IF(ROOTT.EQ.0.0) THEN
        WRITE(LOUT,*) 'SSMHC: ERROR: ROOTT = 0,',
     $  '  CANNOT CALCULATE H+ MASS FOR THIS CASE.'
        STOP99
      ENDIF
C
      BBB1=0.5*(MSBL2+MSBR2)+MB2-ZAP*VPVM*GGP/8.0
      BBB2=BLRM-ZAP*GG2*VPVM/4.0
      BBB3=4.0*FB2*(EP2*V2+2.0*EP*VVP*ABR+AB2*VP2)
C          ROOTB recast as a sum of squares.
      ROOTB=4*MB2*(AAB*COSB + EP*SINB)**2/COSB**2 + 
     $(AMBLSS**2 - AMBRSS**2 - 
     $AMW**2*(-GP2/3 + G2)*(COSB**2 - SINB**2)/(2*G2))**2
      ROOTB=0.5*SQRT(ROOTB)
      IF(ROOTB.EQ.0.0) THEN
        WRITE(LOUT,*) 'SSMHC: ERROR: ROOTB = 0,',
     $  '  CANNOT CALCULATE H+ MASS FOR THIS CASE.'
        STOP99
      ENDIF 
C
C      Calculate 2M1*B term
C
      TEMPT=EP*FT2*VVP*ATI**2/(ROOTT**2)
      TEMPB=EP*FB2*VVP*ABI**2/(ROOTB**2)
      TM1B=-FT2*(TEMPT+ATR)*TTT1*LOG(MST1SQ/MST2SQ)/ROOTT
      TM1B=TM1B-FT2*ATR*LOG(MST1SQ*MST2SQ/QQQ2/QQQ2)
      TM1B=TM1B+FT2*(2.0*TEMPT-ATR)
      TM1B=TM1B-FB2*(TEMPB+ABR)*BBB1*LOG(MSB1SQ/MSB2SQ)/ROOTB
      TM1B=TM1B-FB2*ABR*LOG(MSB1SQ*MSB2SQ/QQQ2/QQQ2)
      TM1B=TM1B+FB2*(2.0*TEMPB-ABR)
      TM1B=3.0*EP*TM1B/32.0/PI2
      TM1B=TM1B-VVP*MHP2/VVPP
      IF (INUHM.EQ.1) THEN
        TM1B=-MHP2/(TANB+COTB)
        amhc=sqrt(mhp2+amw**2)
        go to 1000
      END IF
C
C       Calculate derivatives w.r.t. H_R divided by v*sqrt(2)
C
      TEMPT=ZAP*GG1*(TLRM+VPVM*GG1/4.0)/8.0
      TERMT=-TEMPT+FT2*(EP*COTB*ATR+AT2)
      TERMT=TERMT/(2.0*ROOTT)
      DHRT1=FT2-ZAP*GGP/8.0 + TERMT
      DHRT2=FT2-ZAP*GGP/8.0 - TERMT
C     
      TEMPB=ZAP*GG2*(BLRM-VPVM*GG2/4.0)/8.0
      TERMB=TEMPB+FB2*EP*(EP+COTB*ABR)
      TERMB=TERMB/(2.0*ROOTB)
      DHRB1=ZAP*GGP/8.0 + TERMB
      DHRB2=ZAP*GGP/8.0 - TERMB
C
C       Calculate derivatives w.r.t. H_R' divided by v'*sqrt(2)
C
      TERMT=TEMPT+FT2*EP*(EP+TANB*ATR)
      TERMT=TERMT/(2.0*ROOTT)
      DHRPT1=ZAP*GGP/8.0 + TERMT
      DHRPT2=ZAP*GGP/8.0 - TERMT
C          
      TERMB=-TEMPB+FB2*(EP*TANB*ABR+AB2)
      TERMB=TERMB/(2.0*ROOTB)
      DHRPB1=FB2-ZAP*GGP/8.0 + TERMB
      DHRPB2=FB2-ZAP*GGP/8.0 - TERMB
C
C       Calculate second derivatives w.r.t. H_R^+
C
      TEMPT=(TLRM+ZAP*VPVM*GG1/4.0)/(4.0*ROOTT)
      TERMT=TEMPT*(-FT2+ZAP*GG3/4.0)
      DHPST1=FT2/2.0 + ZAP*GGPM/8.0 + TERMT
      DHPST2=FT2/2.0 + ZAP*GGPM/8.0 - TERMT
C
      TEMPB=(BLRM-ZAP*VPVM*GG2/4.0)/(4.0*ROOTB)
      TERMB=TEMPB*(FT2-ZAP*GG4/4.0)
      DHPSB1=FT2/2.0 - ZAP*GGPM/8.0 + TERMB
      DHPSB2=FT2/2.0 - ZAP*GGPM/8.0 - TERMB
C
C       Calculate second derivatives w.r.t. H_R'^-
C
      TERMT=TEMPT*(FB2-ZAP*GG3/4.0)
      DHMST1=FB2/2.0 - ZAP*GGPM/8.0 + TERMT
      DHMST2=FB2/2.0 - ZAP*GGPM/8.0 - TERMT
C
      TERMB=TEMPB*(-FB2+ZAP*GG4/4.0)
      DHMSB1=FB2/2.0 + ZAP*GGPM/8.0 + TERMB
      DHMSB2=FB2/2.0 + ZAP*GGPM/8.0 - TERMB
C
C       From perturbative terms
C       Here I assume A_t and A_b are real
C       and therefore the eigenvectors are real
C
C       Find stop eigenvector factors
C
      TEMPT=-TLRM/2.0 - ZAP*VPVM*GG1/8.0
      ATA1=TEMPT+ROOTT
      ATA2=TEMPT-ROOTT
      BTA1=-MT*(EP*COTB + ATR)
      BTA2=BTA1
      IF(ATA1.EQ.0.0 .AND. BTA1.EQ.0.0) THEN
        ATA1=-BTA1
        BTA1 = TEMPT - ROOTT
        IF(ATA1.EQ.0.0 .AND. BTA1.EQ.0.0) THEN
          WRITE(LOUT,*) 'SSMHC: ERROR: ZERO EIGENVECTOR FOR MST1SQ'
          STOP99
        ENDIF
      ENDIF
      IF(ATA2.EQ.0.0 .AND. BTA2.EQ.0.0) THEN
        ATA2=-BTA2
        BTA2=TEMPT+ROOTT
        IF(ATA2.EQ.0.0 .AND. BTA2.EQ.0.0) THEN
          WRITE(LOUT,*) 'SSMHC: ERROR:  ZERO EIGENVECTOR FOR MST2SQ'
          STOP99
        ENDIF
      ENDIF
      ATA1SQ=ATA1**2
      BTA1SQ=BTA1**2
      ATA2SQ=ATA2**2
      BTA2SQ=BTA2**2
      NABT1=1.0/(ATA1SQ+BTA1SQ)
      NABT2=1.0/(ATA2SQ+BTA2SQ)
C     
C       Find sbottom eigenvector factors
C
      TEMPB=-BLRM/2.0 + ZAP*VPVM*GG2/8.0
      ABA1=TEMPB+ROOTB
      ABA2=TEMPB-ROOTB
      BBA1=-MB*(EP*TANB + ABR)
      BBA2=BBA1
      IF(ABA1.EQ.0.0 .AND. BBA1.EQ.0.0) THEN
        ABA1=-BBA1
        BBA1=TEMPB-ROOTB
        IF(ABA1.EQ.0.0 .AND. BBA1.EQ.0.0) THEN
          WRITE(LOUT,*) 'SSMHC: ERROR: ZERO EIGENVECTOR FOR MSB1SQ'
          STOP99
        ENDIF
      ENDIF
      IF(ABA2.EQ.0.0 .AND. BBA2.EQ.0.0) THEN
        ABA2=-BBA2
        BBA2=TEMPB+ROOTB            
        IF(ABA2.EQ.0.0 .AND. BBA2.EQ.0.0) THEN
          WRITE(LOUT,*) 'SSMHC: ERROR ZERO EIGENVECTOR FOR MSB2SQ'
          STOP99
        ENDIF
      ENDIF
      ABA1SQ=ABA1**2
      BBA1SQ=BBA1**2
      ABA2SQ=ABA2**2
      BBA2SQ=BBA2**2
      NABB1=1.0/(ABA1SQ+BBA1SQ)
      NABB2=1.0/(ABA2SQ+BBA2SQ)
C
C       Calculate perturbative terms 
C        from H_R^+2 derivative terms
C         
      FTG=FT2-ZAP*G2/2.0
      BABA=FT*FB*(VVP*FTG-EP*ATR)
      PT1B1=V2*(FTG**2)*BTA1SQ*BBA1SQ 
      PT1B1=PT1B1+2.0*EP*FB*V*FTG*BTA1SQ*BBA1*ABA1 
      PT1B1=PT1B1-2.0*ATR*MT*FTG*BTA1*ATA1*BBA1SQ
      PT1B1=PT1B1+2.0*BABA*BTA1*ATA1*BBA1*ABA1
      PT1B1=PT1B1-2.0*ATR*FT2*MB*ATA1SQ*BBA1*ABA1
      PT1B1=PT1B1+2.0*EP*FT*FB*MB*BTA1*ATA1*ABA1SQ
      PT1B1=PT1B1+FT2*AT2*ATA1SQ*BBA1SQ
      PT1B1=PT1B1+FB2*EP2*BTA1SQ*ABA1SQ
      PT1B1=PT1B1+FT2*MB2*ATA1SQ*ABA1SQ
      PT1B1=PT1B1*NABT1*NABB1
C
      PT1B2=V2*(FTG**2)*BTA1SQ*BBA2SQ 
      PT1B2=PT1B2+2.0*EP*FB*V*FTG*BTA1SQ*BBA2*ABA2 
      PT1B2=PT1B2-2.0*ATR*MT*FTG*BTA1*ATA1*BBA2SQ
      PT1B2=PT1B2+2.0*BABA*BTA1*ATA1*BBA2*ABA2
      PT1B2=PT1B2-2.0*ATR*FT2*MB*ATA1SQ*BBA2*ABA2
      PT1B2=PT1B2+2.0*EP*FT*FB*MB*BTA1*ATA1*ABA2SQ
      PT1B2=PT1B2+FT2*AT2*ATA1SQ*BBA2SQ
      PT1B2=PT1B2+FB2*EP2*BTA1SQ*ABA2SQ
      PT1B2=PT1B2+FT2*MB2*ATA1SQ*ABA2SQ
      PT1B2=PT1B2*NABT1*NABB2
C
      PT2B1=V2*(FTG**2)*BTA2SQ*BBA1SQ 
      PT2B1=PT2B1+2.0*EP*FB*V*FTG*BTA2SQ*BBA1*ABA1 
      PT2B1=PT2B1-2.0*ATR*MT*FTG*BTA2*ATA2*BBA1SQ
      PT2B1=PT2B1+2.0*BABA*BTA2*ATA2*BBA1*ABA1
      PT2B1=PT2B1-2.0*ATR*FT2*MB*ATA2SQ*BBA1*ABA1
      PT2B1=PT2B1+2.0*EP*FT*FB*MB*BTA2*ATA2*ABA1SQ
      PT2B1=PT2B1+FT2*AT2*ATA2SQ*BBA1SQ
      PT2B1=PT2B1+FB2*EP2*BTA2SQ*ABA1SQ
      PT2B1=PT2B1+FT2*MB2*ATA2SQ*ABA1SQ
      PT2B1=PT2B1*NABT2*NABB1
C
      PT2B2=V2*(FTG**2)*BTA2SQ*BBA2SQ 
      PT2B2=PT2B2+2.0*EP*FB*V*FTG*BTA2SQ*BBA2*ABA2 
      PT2B2=PT2B2-2.0*ATR*MT*FTG*BTA2*ATA2*BBA2SQ
      PT2B2=PT2B2+2.0*BABA*BTA2*ATA2*BBA2*ABA2
      PT2B2=PT2B2-2.0*ATR*FT2*MB*ATA2SQ*BBA2*ABA2
      PT2B2=PT2B2+2.0*EP*FT*FB*MB*BTA2*ATA2*ABA2SQ
      PT2B2=PT2B2+FT2*AT2*ATA2SQ*BBA2SQ
      PT2B2=PT2B2+FB2*EP2*BTA2SQ*ABA2SQ
      PT2B2=PT2B2+FT2*MB2*ATA2SQ*ABA2SQ
      PT2B2=PT2B2*NABT2*NABB2
C          The following used to add 1e-8, but this may be less than
C          machine precision. Multiply by 1.001 instead.
      IF(MST1SQ.EQ.MSB1SQ) THEN
        WRITE(LOUT,*) 'SSMHC: WARNING: MST1 = MSB1',
     $  '  MST1 RAISED BY 0.0001' 
        MST1SQ = MST1SQ*1.0001
      ENDIF
      IF(MST1SQ.EQ.MSB2SQ) THEN
        WRITE(LOUT,*) 'SSMHC: WARNING: MST1 = MSB2',
     $  '  MST1 RAISED BY 0.0001' 
        MST1SQ = MST1SQ*1.0001
      ENDIF
      IF(MST2SQ.EQ.MSB1SQ) THEN
        WRITE(LOUT,*) 'SSMHC: WARNING: MST2 = MSB1',
     $  '  MST2 RAISED BY 0.0001' 
        MST2SQ = MST2SQ*1.0001
      ENDIF
      IF(MST2SQ.EQ.MSB2SQ) THEN
        WRITE(LOUT,*) 'SSMHC: WARNING: MST2 = MSB2',
     $  '  MST2 RAISED BY 0.0001'
        MST2SQ = MST2SQ*1.0001
      ENDIF
C
      PDPST1=PT1B1/(MST1SQ-MSB1SQ) +PT1B2/(MST1SQ-MSB2SQ)
      PDPST2=PT2B1/(MST2SQ-MSB1SQ) +PT2B2/(MST2SQ-MSB2SQ)
      PDPSB1=PT1B1/(MSB1SQ-MST1SQ) +PT2B1/(MSB1SQ-MST2SQ)
      PDPSB2=PT1B2/(MSB2SQ-MST1SQ) +PT2B2/(MSB2SQ-MST2SQ)
C     
C       Calculate perturbative terms 
C        from H_R'^-2 derivative terms
C
      FBG=FB2-ZAP*G2/2.0
      BABA=FT*FB*(VVP*FBG-EP*ABR)
      PT1B1=VP2*(FBG**2)*BTA1SQ*BBA1SQ 
      PT1B1=PT1B1-2.0*ABR*MB*FBG*BTA1SQ*BBA1*ABA1 
      PT1B1=PT1B1+2.0*EP*FT*VP*FBG*BTA1*ATA1*BBA1SQ
      PT1B1=PT1B1+2.0*BABA*BTA1*ATA1*BBA1*ABA1
      PT1B1=PT1B1+2.0*EP*FT*FB*MT*ATA1SQ*BBA1*ABA1
      PT1B1=PT1B1-2.0*ABR*FB2*MT*BTA1*ATA1*ABA1SQ
      PT1B1=PT1B1+FT2*EP2*ATA1SQ*BBA1SQ
      PT1B1=PT1B1+FB2*AB2*BTA1SQ*ABA1SQ
      PT1B1=PT1B1+FB2*MT2*ATA1SQ*ABA1SQ
      PT1B1=PT1B1*NABT1*NABB1
C
      PT1B2=VP2*(FBG**2)*BTA1SQ*BBA2SQ 
      PT1B2=PT1B2-2.0*ABR*MB*FBG*BTA1SQ*BBA2*ABA2 
      PT1B2=PT1B2+2.0*EP*FT*VP*FBG*BTA1*ATA1*BBA2SQ
      PT1B2=PT1B2+2.0*BABA*BTA1*ATA1*BBA2*ABA2
      PT1B2=PT1B2+2.0*EP*FT*FB*MT*ATA1SQ*BBA2*ABA2
      PT1B2=PT1B2-2.0*ABR*FB2*MT*BTA1*ATA1*ABA2SQ
      PT1B2=PT1B2+FT2*EP2*ATA1SQ*BBA2SQ
      PT1B2=PT1B2+FB2*AB2*BTA1SQ*ABA2SQ
      PT1B2=PT1B2+FB2*MT2*ATA1SQ*ABA2SQ
      PT1B2=PT1B2*NABT1*NABB2
C     
      PT2B1=VP2*(FBG**2)*BTA2SQ*BBA1SQ 
      PT2B1=PT2B1-2.0*ABR*MB*FBG*BTA2SQ*BBA1*ABA1 
      PT2B1=PT2B1+2.0*EP*FT*VP*FBG*BTA2*ATA2*BBA1SQ
      PT2B1=PT2B1+2.0*BABA*BTA2*ATA2*BBA1*ABA1
      PT2B1=PT2B1+2.0*EP*FT*FB*MT*ATA2SQ*BBA1*ABA1
      PT2B1=PT2B1-2.0*ABR*FB2*MT*BTA2*ATA2*ABA1SQ
      PT2B1=PT2B1+FT2*EP2*ATA2SQ*BBA1SQ
      PT2B1=PT2B1+FB2*AB2*BTA2SQ*ABA1SQ
      PT2B1=PT2B1+FB2*MT2*ATA2SQ*ABA1SQ
      PT2B1=PT2B1*NABT2*NABB1
C
      PT2B2=VP2*(FBG**2)*BTA2SQ*BBA2SQ 
      PT2B2=PT2B2-2.0*ABR*MB*FBG*BTA2SQ*BBA2*ABA2 
      PT2B2=PT2B2+2.0*EP*FT*VP*FBG*BTA2*ATA2*BBA2SQ
      PT2B2=PT2B2+2.0*BABA*BTA2*ATA2*BBA2*ABA2
      PT2B2=PT2B2+2.0*EP*FT*FB*MT*ATA2SQ*BBA2*ABA2
      PT2B2=PT2B2-2.0*ABR*FB2*MT*BTA2*ATA2*ABA2SQ
      PT2B2=PT2B2+FT2*EP2*ATA2SQ*BBA2SQ
      PT2B2=PT2B2+FB2*AB2*BTA2SQ*ABA2SQ
      PT2B2=PT2B2+FB2*MT2*ATA2SQ*ABA2SQ
      PT2B2=PT2B2*NABT2*NABB2
C
      PDMST1=PT1B1/(MST1SQ-MSB1SQ) +PT1B2/(MST1SQ-MSB2SQ)
      PDMST2=PT2B1/(MST2SQ-MSB1SQ) +PT2B2/(MST2SQ-MSB2SQ)
      PDMSB1=PT1B1/(MSB1SQ-MST1SQ) +PT2B1/(MSB1SQ-MST2SQ)
      PDMSB2=PT1B2/(MSB2SQ-MST1SQ) +PT2B2/(MSB2SQ-MST2SQ)
C     
C        Calculate perturbative terms 
C        from  H_R^+ H_R'^- derivative terms
C
      BABA=FT*FB*(V2*FTG+VP2*FBG+EP2+ATR*ABR)
      PT1B1=-VVP*FTG*FBG*BTA1SQ*BBA1SQ
      PT1B1=PT1B1+FB*(ABR*V*FTG-EP*VP*FBG)*BTA1SQ*BBA1*ABA1
      PT1B1=PT1B1-FT*(EP*V*FTG-ATR*VP*FBG)*BTA1*ATA1*BBA1SQ
      PT1B1=PT1B1-BABA*BTA1*ATA1*BBA1*ABA1
      PT1B1=PT1B1+FT2*FB*(ATR*V-EP*VP)*ATA1SQ*BBA1*ABA1
      PT1B1=PT1B1+FT*FB2*(ABR*VP-EP*V)*BTA1*ATA1*ABA1SQ
      PT1B1=PT1B1+FT2*EP*ATR*ATA1SQ*BBA1SQ
      PT1B1=PT1B1+FB2*EP*ABR*BTA1SQ*ABA1SQ
      PT1B1=PT1B1-FT*FB*MT*MB*ATA1SQ*ABA1SQ
      PT1B1=PT1B1*NABT1*NABB1
C
      PT1B2=-VVP*FTG*FBG*BTA1SQ*BBA2SQ
      PT1B2=PT1B2+FB*(ABR*V*FTG-EP*VP*FBG)*BTA1SQ*BBA2*ABA2
      PT1B2=PT1B2-FT*(EP*V*FTG-ATR*VP*FBG)*BTA1*ATA1*BBA2SQ
      PT1B2=PT1B2-BABA*BTA1*ATA1*BBA2*ABA2
      PT1B2=PT1B2+FT2*FB*(ATR*V-EP*VP)*ATA1SQ*BBA2*ABA2
      PT1B2=PT1B2+FT*FB2*(ABR*VP-EP*V)*BTA1*ATA1*ABA2SQ
      PT1B2=PT1B2+FT2*EP*ATR*ATA1SQ*BBA2SQ
      PT1B2=PT1B2+FB2*EP*ABR*BTA1SQ*ABA2SQ
      PT1B2=PT1B2-FT*FB*MT*MB*ATA1SQ*ABA2SQ
      PT1B2=PT1B2*NABT1*NABB2
C
      PT2B1= -VVP*FTG*FBG*BTA2SQ*BBA1SQ
      PT2B1= PT2B1 + FB*(ABR*V*FTG - EP*VP*FBG)*BTA2SQ*BBA1*ABA1
      PT2B1= PT2B1 - FT*(EP*V*FTG - ATR*VP*FBG)*BTA2*ATA2*BBA1SQ
      PT2B1= PT2B1 - BABA*BTA2*ATA2*BBA1*ABA1
      PT2B1= PT2B1 + FT2*FB*(ATR*V - EP*VP)*ATA2SQ*BBA1*ABA1
      PT2B1= PT2B1 + FT*FB2*(ABR*VP - EP*V)*BTA2*ATA2*ABA1SQ
      PT2B1= PT2B1 + FT2*EP*ATR*ATA2SQ*BBA1SQ
      PT2B1= PT2B1 + FB2*EP*ABR*BTA2SQ*ABA1SQ
      PT2B1= PT2B1 - FT*FB*MT*MB*ATA2SQ*ABA1SQ
      PT2B1= PT2B1*NABT2*NABB1
C
      PT2B2=-VVP*FTG*FBG*BTA2SQ*BBA2SQ
      PT2B2=PT2B2+FB*(ABR*V*FTG-EP*VP*FBG)*BTA2SQ*BBA2*ABA2
      PT2B2=PT2B2-FT*(EP*V*FTG-ATR*VP*FBG)*BTA2*ATA2*BBA2SQ
      PT2B2=PT2B2-BABA*BTA2*ATA2*BBA2*ABA2
      PT2B2=PT2B2+FT2*FB*(ATR*V-EP*VP)*ATA2SQ*BBA2*ABA2
      PT2B2=PT2B2+FT*FB2*(ABR*VP-EP*V)*BTA2*ATA2*ABA2SQ
      PT2B2=PT2B2+FT2*EP*ATR*ATA2SQ*BBA2SQ
      PT2B2=PT2B2+FB2*EP*ABR*BTA2SQ*ABA2SQ
      PT2B2=PT2B2-FT*FB*MT*MB*ATA2SQ*ABA2SQ
      PT2B2=PT2B2*NABT2*NABB2
C
      PDPMT1=PT1B1/(MST1SQ-MSB1SQ) +PT1B2/(MST1SQ-MSB2SQ)
      PDPMT2=PT2B1/(MST2SQ-MSB1SQ) +PT2B2/(MST2SQ-MSB2SQ)
      PDPMB1=PT1B1/(MSB1SQ-MST1SQ) +PT2B1/(MSB1SQ-MST2SQ)
      PDPMB2=PT1B2/(MSB2SQ-MST1SQ) +PT2B2/(MSB2SQ-MST2SQ)
C
      LMST1=MST1SQ*(2.0*LOG(MST1SQ/QQQ2)+1.0)            
      LMST2=MST2SQ*(2.0*LOG(MST2SQ/QQQ2)+1.0)             
      LMSB1=MSB1SQ*(2.0*LOG(MSB1SQ/QQQ2)+1.0) 
      LMSB2=MSB2SQ*(2.0*LOG(MSB2SQ/QQQ2)+1.0)            
C
      EMI1=LMST1*(PDPST1+DHPST1-DHRT1)
      EMI1=EMI1+LMST2*(PDPST2+DHPST2-DHRT2)
      EMI1=EMI1+LMSB1*(PDPSB1+DHPSB1-DHRB1)
      EMI1=EMI1+LMSB2*(PDPSB2+DHPSB2-DHRB2)
C     
      EMI2=LMST1*(PDMST1+DHMST1-DHRPT1)
      EMI2=EMI2+LMST2*(PDMST2+DHMST2-DHRPT2)
      EMI2=EMI2+LMSB1*(PDMSB1+DHMSB1-DHRPB1)
      EMI2=EMI2+LMSB2*(PDMSB2+DHMSB2-DHRPB2)
C
      EM3=LMST1*PDPMT1+LMST2*PDPMT2
      EM3=EM3+LMSB1*PDPMB1+LMSB2*PDPMB2
C
      TEMPF=MT2*LOG(MT2/QQQ2)-MB2*LOG(MB2/QQQ2)
      DVRR=4.0*FT2*MB2*TEMPF/(MT2-MB2)
      DVRR=3.0*(EMI1-DVRR-2.0*FT2*MB2)/32.0/PI2
      DVRR=-TM1B*COTB +VP2*G2/2.0 +DVRR
C
      DVRPRP=4.0*FB2*MT2*TEMPF/(MT2-MB2)
      DVRPRP=3.0*(EMI2-DVRPRP-2.0*FB2*MT2)/32.0/PI2
      DVRPRP=-TM1B*TANB +V2*G2/2.0 +DVRPRP
C
      DVRRP=1.0 +(MT2+MB2)*LOG(MT2/MB2)/(MT2-MB2)
      DVRRP=2.0*FT*FB*MT*MB*(DVRRP+LOG(MT2*MB2/(QQQ2**2)))
      DVRRP=3.0*(EM3+DVRRP)/32.0/PI2
      DVRRP=TM1B -G2*VVP/2.0 +DVRRP
C
      TRACE=DVRR+DVRPRP
      DETV=4.0*(DVRR*DVRPRP-DVRRP**2)
C          Rewrite TERMSQ=TRACE**2-DETV
      TERMSQ=(DVRR-DVRPRP)**2+4*DVRRP**2
      TERMSQ=SQRT(TERMSQ)/2.0
      GOLD2=TRACE/2.0 -TERMSQ
      MHC2=TRACE/2.0 +TERMSQ
C
50    IF(MHC2.LT.0.0) THEN
        MHCNEG=1
        AMHC=0.
        GO TO 1000
      ENDIF
      AMHC=SQRT(MHC2)
1000  RETURN
      END
