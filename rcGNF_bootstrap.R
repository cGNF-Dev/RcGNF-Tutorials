
# Setting up environment and paths
base_path <- '/Users/jessezhou/Desktop/cGNF_test/rcGNF/'
folder <- 'ATE_8k'
path <- file.path(base_path, folder, '')
dataset_name <- 'ATE_8k'

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}

# Set the number of observations
obs <- 8000

# Set seed for reproducibility
set.seed(1001)

# Simulate data
C <- rbinom(n = obs, size = 1, prob = 0.5)

epsilon_A <- rnorm(obs, 0, 1)
epsilon_L <- rlogis(obs, 0, 1)
epsilon_M <- rlogis(obs, 0, 1)
epsilon_Y <- rnorm(obs, 0, 1)

A <- 0.2 * C + epsilon_A
L <- 0.1 * A + 0.2 * C + epsilon_L
M <- 0.1 * A + 0.1 * C + 0.2 * L + epsilon_M
Y <- 0.1 * A + 0.1 * C + 0.2 * M + 0.2 * L + epsilon_Y * (1 + 0.2 * C)

# Create a dataframe
df <- data.frame(C = C, A = A, L = L, M = M, Y = Y)

# Write the dataframe to a CSV file
write.csv(df, file = paste0(path, dataset_name, '.csv'), row.names = FALSE)

# DAG Specification
library(igraph)

# Create the DAG
simDAG <- graph_from_edgelist(matrix(c("C", "A", "C", "L", "C", "M", "C", "Y", "A", "L", "A", "M", "A", "Y", "L", "M", "L", "Y", "M", "Y"), byrow = TRUE, ncol = 2), directed = TRUE)

# Plot the DAG
plot(simDAG)


# Convert the adjacency matrix to a base R matrix
adj_matrix <- as.matrix(as_adjacency_matrix(simDAG, type = "both", attr = NULL, sparse = FALSE))
df_cDAG <- as.data.frame(adj_matrix)

# Print the adjacency matrix
print("------- Adjacency Matrix -------")
print(df_cDAG)

# Write the adjacency matrix to a CSV file
write.csv(df_cDAG, file = paste0(path, dataset_name, '_DAG.csv'), row.names = TRUE)

# devtools::install_github("cGNF-Dev/RcGNF")  # install the package first

library(RcGNF)

# Specify process_args
process_args <- list(
  test_size = 0.2,
  cat_var = c('C')
)

# Specify train_args
train_args <- list(
  model_name = '20k',
  trn_batch_size = 128,
  val_batch_size = 2048,
  learning_rate = 1e-4,
  seed = 8675309,
  nb_epoch = 50000,
  nb_estop = 50,
  val_freq = 1,
  emb_net = c(90, 80, 70, 60, 50),
  int_net = c(50, 40, 30, 20, 10)
)

# Specify sim_args1
sim_args1 <- list(
  model_name = '20k',
  treatment = 'A',
  cat_list = c(0, 1),
  mediator = c('L'),
  outcome = 'M',
  inv_datafile_name = 'med'
)

# Specify sim_args2
sim_args2 <- list(
  model_name = '20k',
  treatment = 'A',
  cat_list = c(0, 1),
  mediator = c('L', 'M'),
  outcome = 'Y',
  inv_datafile_name = 'pse'
)

# Combine sim_args1 and sim_args2 into a list for sim_args_list
sim_args_list <- list(sim_args1, sim_args2)

# Bootstrapping


bootstrap(n_iterations = 10,
          num_cores_reserve = 6,
          base_path = path,
          folder_name = 'bootstrap',
          dataset_name = dataset_name,
          dag_name = paste0(dataset_name, '_DAG'),
          process_args = process_args,
          train_args = train_args,
          sim_args_list = sim_args_list)
