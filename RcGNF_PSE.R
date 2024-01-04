base_path <- '/Users/jessezhou/Desktop/cGNF_test/rcGNF' # Define the base path for file operations.
folder <- '_PSE/'  # Define the folder where files will be stored.
path <- file.path(base_path, folder, '')  # Combines the base path and folder into a complete path.
dataset_name <- 'PSE_20k'  # Define the name of the dataset.

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}
# Checks if a directory with the name 'path' exists.
# If not, creates a new directory with this name. This is where the logs and model weights will be saved.

## DATA SIMULATION
obs <- 20000  # Sets the number of observations.
set.seed(2813)  # Sets the seed for simulation.

C <- rbinom(n = obs, size = 1, prob = 0.4)

epsilon_A <- rnorm(obs, 0, 1)
epsilon_L <- rlogis(obs, 0, 1)
epsilon_M <- rlogis(obs, 0, 1)
epsilon_Y <- rnorm(obs, 0, 1)

A <- 0.2 * C + epsilon_A
L <- 0.1 * A + 0.2 * C + epsilon_M
M <- 0.1 * A + 0.1 * C + 0.2 * L + epsilon_M
Y <- 0.1 * A + 0.1 * C + 0.2 * M + 0.2 * L + epsilon_Y

df <- data.frame(C = C, A = A, L = L, M = M, Y = Y)
df_filename <- file.path(path, paste0(dataset_name, '.csv'))
write.csv(df, df_filename, row.names = FALSE)

## DAG SPECIFICATION
library(igraph)

simDAG <- graph_from_edgelist(matrix(c("C", "A", "C", "L", "C", "M", "C", "Y", "A", "L", "A", "M", "A", "Y", "L", "M", "L", "Y", "M", "Y"), ncol = 2, byrow = TRUE), directed = TRUE)
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
process(path = path,
        dataset_name = dataset_name,
        dag_name = paste0(dataset_name, '_DAG'),
        sens_corr=NULL,
        test_size = 0.2,
        cat_var = c("C"),
        seed = NULL)

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
    mediator = c("L", "M"),
    outcome = 'Y',
    inv_datafile_name = 'rsim_20k')

