#
#	Created by Nasuf Sönmez, Jan, 2018
#
# this script runs a loop over isajet for varius inputs and calculates SLHA values.
# increment the tan beta values and calculates the SLHE files for later analysis.

# ENTER 1 for mSUGRA:
# ENTER 2 for mGMSB:
# ENTER 3 for non-universal SUGRA:
# ENTER 4 for SUGRA with truly unified gauge couplings:
# ENTER 5 for non-minimal GMSB:
# ENTER 6 for SUGRA+right-handed neutrino:
# ENTER 7 for minimal anomaly-mediated SUSY breaking:
# ENTER 8 for non-minimal AMSB:
# ENTER 9 for mixed moduli-AMSB:
# ENTER 10 for Hypercharged-AMSB:
# ENTER 11 for NUHM from D-term:
# ENTER 12 for general mirage mediation (GMM):
# ENTER 13 for natural AMSB (nAMSB):

# which model :
which_model=3

#the name of output and SLHA files.
name_output="nuhm2"



# ma is the CP-odd Higgs mass
# i increments its value
for i in {0..1}
do
    ma=`echo "150+15*$i"|bc`

# tb is the tanbeta, the ratio of the higgs vacuum expectation values
# j increments its value
for j in {0..2}
do
    tb=`echo "1+$j*2"|bc`


#for j in {1..10}
#do
#mu=`echo "100+1000*$j"|bc`


    echo "------------->  for model $which_model, mA=$ma and tb=$tb " are calculated
    echo ""

	echo "${name_output}_mA${ma}_tb${tb}"       		    > input_mA${ma}_tb${tb}
	echo "${name_output}_mA${ma}_tm${tb}.slha"			>> input_mA${ma}_tb${tb}
	echo "/"   								            >> input_mA${ma}_tb${tb}
	echo ${which_model}						            >> input_mA${ma}_tb${tb}
	echo "10000, 500, -16000, ${tb}, 1, 173.34"            >> input_mA${ma}_tb${tb}
	echo "8"   								            >> input_mA${ma}_tb${tb}
	echo "6000, ${ma}"   					            >> input_mA${ma}_tb${tb}
	echo "0"   								            >> input_mA${ma}_tb${tb}
	echo "/"   								            >> input_mA${ma}_tb${tb}
	echo ""   								            >> input_mA${ma}_tb${tb}
	
	cat input_mA${ma}_tb${tb} | ./isajet/isasugra.x

    echo ""
done
done



# ENTER M_0, M_(1/2), A_0, tan(beta), sgn(mu), M_t:
# ENTER 8 for mu, mA input at weak scale (NUHM model)
# ENTER mu(Q), mA(Q)
# Run Isatools? Choose 2=all, 1=some, 0=none:
# To run RGEFLAV, enter filename Prefix [/ for none]:


