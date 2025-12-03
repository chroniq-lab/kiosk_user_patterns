#!/bin/bash
#SBATCH --job-name=kthyc301
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --partition=c64-m512
#SBATCH --output=/users/jvargh7/output/%x_%j.out
#SBATCH --error=/users/jvargh7/error/%x_%j.err
#SBATCH --time=24:00:00
#SBATCH --mem=8G
#SBATCH --mail-type=begin,end,fail
#SBATCH --mail-user=jvargh7@emory.edu

# Print diagnostic information
echo "Job started at $(date)"

echo "Running as user: $(whoami)"
echo "Working directory: $(pwd)"
echo "Files in directory: $(ls -la)"

# Check if R is available and working
R --version

# Run a minimal R command first to test R functionality
R -e 'cat("R is working properly\n")'

R_SCRIPT=/users/jvargh7/kinetic-t2d-hyperc3/code/kthyc301_fitting_mixed_model.R

# Record start time
START_TIME=$(date +%s)
echo "Job started at $(date)"

# Run the R script directly
srun Rscript $R_SCRIPT

# Record end time and calculate duration
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))
HOURS=$((DURATION / 3600))
MINUTES=$(( (DURATION % 3600) / 60 ))
SECONDS=$((DURATION % 60))

echo "Job completed at $(date)"
echo "Total runtime: ${HOURS}h ${MINUTES}m ${SECONDS}s"