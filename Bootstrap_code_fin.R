### We select the PCA axis for the analysis

PC1_env_diplo = put_it_here # Put your PC1 for diploids - X list from grid
PC2_env_diplo = put_it_here # Put your PC2 for diploids - Y list from grid

PC1_env_tetra = put_it_here # Put your PC1 for tetraploids - X list from grid
PC2_env_tetra = put_it_here # Put your PC2 for tetraploids - Y list from grid

density_2x = put_it_here # z values for diploids 
density_4x = put_it_here # z values for tetraploids
  
nb_bootstrap = 100 # Choose the number of times you want bootstrap your cells
nb_cells_sample = 100 # The number of cells to sample for the analyses

### We create the vectors for the desired metrics 

optimum_PC1_diplo = c(NULL) # Vector of niche optimum for PC1 in diploids
optimum_PC2_diplo = c(NULL) # Vector of niche optimum for PC2 in diploids

Breadth_PC1_diplo = c(NULL) # Vector of niche breadth for PC1 in diploids
Breadth_PC2_diplo = c(NULL) # Vector of niche breadth for PC2 in diploids

optimum_PC1_tetra = c(NULL) # Vector of niche optimum for PC1 in tetraploids
optimum_PC2_tetra = c(NULL) # Vector of niche optimum for PC2 in tetraploids

Breadth_PC1_tetra = c(NULL) # Vector of niche breadth for PC1 in tetraploids
Breadth_PC2_tetra = c(NULL) # Vector of niche breadth for PC2 in tetraploids

### I create a code for knowing what number correspond to which X and Y coordinate

X_meaning=rep(1:100, times = 100)
Y_meaning=c(NULL)

for(i in 1:100){
  
  temporary = rep(i, times = 100)
  
  Y_meaning = c(Y_meaning, temporary)

}

### We write the code for bootstrapping 

for(i in 1:nb_bootstrap){
  
  # We sample the cells (X and Y dimensions, with or without replacement (replace = T or F in the following code)
  # pending their weight (which is described by the prob = density)
  
  cells_diplo = sample(1:10000, nb_cells_sample, replace = T, prob = density_2x)
  cells_tetra = sample(1:10000, nb_cells_sample, replace = T, prob = density_4x)
  
  # we create the new PCA vectors
  
  new_PC1_diplo = c(NULL) # New PC1 vector for diploids
  new_PC2_diplo = c(NULL) # New PC2 vector for diploids
  
  new_PC1_tetra = c(NULL) # New PC1 vector for tetraploids
  new_PC2_tetra = c(NULL) # New PC2 vector for tetraploids
  
  for(j in 1:nb_cells_sample){
  
    # We select the value in the original PC1 and PC2 axis for our subsamples
    
    new_PC1_diplo[j] = PC1_env_diplo[X_meaning[cells_diplo[j]]]  
    new_PC2_diplo[j] = PC2_env_diplo[Y_meaning[cells_diplo[j]]]
    
    new_PC1_tetra[j] = PC1_env_tetra[X_meaning[cells_tetra[j]]]
    new_PC2_tetra[j] = PC2_env_tetra[Y_meaning[cells_tetra[j]]]
    
  }

  # We infer the mean and variance of these new subsets
  
  optimum_PC1_diplo[i] = median(new_PC1_diplo) # Vector of niche optimum for PC1 in diploids
  optimum_PC2_diplo[i] = median(new_PC2_diplo) # Vector of niche optimum for PC2 in diploids
  
  Breadth_PC1_diplo[i] = sd(new_PC1_diplo) # Vector of niche breadth for PC1 in diploids
  Breadth_PC2_diplo[i] = sd(new_PC2_diplo) # Vector of niche breadth for PC2 in diploids
  
  optimum_PC1_tetra[i] = median(new_PC1_tetra) # Vector of niche optimum for PC1 in tetraploids
  optimum_PC2_tetra[i] = median(new_PC2_tetra) # Vector of niche optimum for PC2 in tetraploids
  
  Breadth_PC1_tetra[i] = sd(new_PC1_tetra) # Vector of niche breadth for PC1 in tetraploids
  Breadth_PC2_tetra[i] = sd(new_PC2_tetra) # Vector of niche breadth for PC2 in tetraploids
  
}

## Now you can make tests with the vectors of values

# for example

t.test(optimum_PC1_diplo,optimum_PC1_tetra) 

# if you want a parametric test, but following the central limit theorem
# normality is respected for this kind of bootstrapping