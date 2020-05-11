for dens in 1 5 20 NA; do for app in 0 0.25 0.5 0.75; do for c in 7.5 10 14.8; do echo Rscript generate_images.R $dens $app $c NA; done; done; done > job.sh

for dens in 1 5 20 NA; do for app in 0 0.25 0.5 0.75; do for v in 1.5 2.5 5.2; do echo Rscript generate_images.R $dens $app NA $v; done; done; done >> job.sh