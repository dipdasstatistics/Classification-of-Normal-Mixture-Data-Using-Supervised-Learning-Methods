#!/bin/bash
#SBATCH --chdir=./
#SBATCH --job-name=thesis_simulation
#SBATCH --output=./report/report_%j.txt
#SBATCB –-error=./report/err_%j.txt
#SBATCH --partition nocona
#SBATCH --nodes=20
#SBATCH --ntasks=100
#SBATCH --mem-per-cpu=3G
#SBATCH --time=48:00:00

cd /home/das70453 # Changing to this directory to load all packages

#Loading randomforest package
. spack/share/spack/setup-env.sh
spack load r-mvtnorm r-nnet r-e1071 r-class r-rpart r-ipred r-randomforest r-caret



#Allow R to perform some automatic parallelization.
#	MKL_NUM_THREADS - The maximum number of threads you want R to spawn on your behalf.
#	$NSLOTS - This will be replaced by the number of slots you request in yout parallel environment.
#		Example:  -pe sm 36 -> $NSLOTS=36.
#export MKL_NUM_THREADS=$NSLOTS
#export MKL_NUM_THREADS=$NSLOTS
#Run the example R script using the Rscript application.

# Changing to this directory to run the files and to store the results.
cd /home/das70453/thesis/simulation

RandomSeed=$1

for number_of_observations in 10000
 do
   for r in 0.5 1 5
      do
        for sigma_for_data in "identity" "toeplitz" "equicorrelation"
           do
               Rscript ./Running_simulation.R $number_of_observations $r $sigma_for_data $RandomSeed;
           done  
      done
 done
