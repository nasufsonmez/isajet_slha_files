#
#	Created by Nasuf Sönmez, Nov, 2018
#
# this script runs a loop over isajet for varius inputs and calculates SLHA values.
# increment the tan beta values and calculates the LHE files for later analysis.



for i in {0..17}
do

# increment the mA values
for j in {0..25}
do

#for j in {1..10}
#do

#mu=`echo "100+1000*$j"|bc`
ma=`echo "150+15*$i"|bc`
tb=`echo "1+$j*2"|bc`

echo "------------->  $ma and $tb " are calculated 
echo ""

	echo "nuhm2_${ma}_${tb}"       			> input_${ma}_${tb}
	echo "nuhm2_${ma}_${tb}.slha"			>> input_${ma}_${tb}
	echo "/"   								>> input_${ma}_${tb}
	echo "3"   								>> input_${ma}_${tb}
	echo "10000, 500, -16000, ${tb}, 1, 173.34">> input_${ma}_${tb}
	echo "8"   								>> input_${ma}_${tb}
	echo "6000, ${ma}"   					>> input_${ma}_${tb}
	echo "0"   								>> input_${ma}_${tb}
	echo "/"   								>> input_${ma}_${tb}
	echo ""   								>> input_${ma}_${tb}
	
	cat input_${ma}_${tb} | ./isasugra.x
done
done



# ENTER M_0, M_(1/2), A_0, tan(beta), sgn(mu), M_t:
# ENTER 8 for mu, mA input at weak scale (NUHM model)
# ENTER mu(Q), mA(Q)
# Run Isatools? Choose 2=all, 1=some, 0=none:
# To run RGEFLAV, enter filename Prefix [/ for none]:


