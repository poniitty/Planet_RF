#!/bin/bash -l
#SBATCH --job-name=Gather_teach
#SBATCH --account=Project_2003061
#SBATCH --output=output_%j.txt
#SBATCH --error=errors_%j.txt
#SBATCH --time=06:00:00
#SBATCH --ntasks=20
#SBATCH --nodes=2-10
#SBATCH --partition=large
#SBATCH --mem-per-cpu=32G

# Load r-env-singularity
module load r-env-singularity

# Clean up .Renviron file in home directory
if test -f ~/.Renviron; then
    sed -i '/TMPDIR/d' ~/.Renviron
    sed -i '/OMP_NUM_THREADS/d' ~/.Renviron
fi

# Specify a temp folder path
echo "TMPDIR=/scratch/project_2003061/temp" >> ~/.Renviron

# Run the R script
srun singularity_wrapper exec Rscript --no-save gather_teaching_data.R