base_path <- '/Users/jessezhou/Desktop/cGNF_test/rcGNF/' # Define the base path for file operations.
folder <- '_NDIE'  # Define the folder where files will be stored.
path <- file.path(base_path, folder, '')  # Combines the base path and folder into a complete path.
dataset_name <- 'NDIE_20k'  # Define the name of the dataset.

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}
# Checks if a directory with the name 'path' exists.
# If not, creates a new directory with this name. This is where the logs and model weights will be saved.

## DATA SIMULATION
obs <- 20000  # Sets the number of observations.
set.seed(2813)  # Sets the seed for simulation.

C <- rbinom(n = obs, size = 1, prob = 0.4)

# Introduce unobserved confounders U1 and U2
U1 = rnorm(obs, 0, 3) # U1 affects A and Y
U2 = rnorm(obs, 0, 3)  # U2 affects M and Y

epsilon_A <- rnorm(obs, 0, 1) + 0.2 * U1
epsilon_M <- rlogis(obs, 0, 1) + 0.4 * U2
epsilon_Y <- rlogis(obs, 0, 1) + 0.2 * U1 + 0.25 * U2

A <- 0.2 * C + epsilon_A
M <- 0.25 * A + epsilon_M
Y <- 0.2 * A + 0.4 * M + 0.2 * C + epsilon_Y

df <- data.frame(C = C, A = A, M = M, Y = Y)
df_filename <- file.path(path, paste0(dataset_name, '.csv'))
write.csv(df, df_filename, row.names = FALSE)

## DAG SPECIFICATION
library(igraph)

simDAG <- graph_from_edgelist(matrix(c("C", "A", "C", "Y", "A", "M", "A", "Y", "M", "Y"), ncol = 2, byrow = TRUE), directed = TRUE)
plot(simDAG)

# Convert the adjacency matrix to a base R matrix
adj_matrix <- as.matrix(as_adjacency_matrix(simDAG, type = "both", attr = NULL, sparse = FALSE))
df_cDAG <- as.data.frame(adj_matrix)

# Set the row names
rownames(df_cDAG) <- V(simDAG)$name

print("------- Adjacency Matrix -------")
print(df_cDAG)

write.csv(df_cDAG, file.path(path, paste0(dataset_name, '_DAG.csv')), row.names = TRUE)


library(RcGNF)

## DATA PREPROCESSING
# Specify sensitivity correlation strength
corr_strength_1 <- list(`("A", "Y")` = 0.15, `("M", "Y")` = 0.2)
# For real-world data, always test on a range of sensitivity correlations
# corr_strength_2 <- list(`("A", "Y")` = 0.2, `("M", "Y")` = 0.15)
# corr_strength_3 <-...

process(path = path,
        dataset_name = dataset_name,
        dag_name = paste0(dataset_name, '_DAG'),
        sens_corr=corr_strength_1,
        test_size = 0.2,
        cat_var = c("C"),
        seed = 111)

## MODEL TRAINING
train(path = path,
      dataset_name = dataset_name,
      model_name = "20k",
      trn_batch_size = 128,
      val_batch_size = 2048,
      learning_rate = 1e-4,
      seed = 8675309,
      nb_epoch = 50000,
      emb_net = c(90, 80, 70, 60, 50),
      int_net = c(50, 40, 30, 20, 10),
      nb_estop = 50,
      val_freq = 1)

## POTENTIAL OUTCOME ESTIMATION
sim(path = path,
    dataset_name = dataset_name,
    model_name = "20k",
    n_mce_samples = 10000,
    treatment = 'A',
    cat_list = c(0, 1),
    moderator = NULL,
    mediator = c("M"),
    outcome = 'Y',
    inv_datafile_name = 'rsim_20k')

