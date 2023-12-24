base_path <- '/Users/jessezhou/Desktop/cGNF_test/rcGNF/' # Define the base path for file operations.
folder <- '_PSE'  # Define the folder where files will be stored.
path <- file.path(base_path, folder, '')  # Combines the base path and folder into a complete path.
dataset_name <- 'PSE_20k'  # Define the name of the dataset.

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}
# Checks if a directory with the name 'path' exists.
# If not, creates a new directory with this name. This is where the logs and model weights will be saved.

## DATA SIMULATION
obs <- 20000  # Sets the number of observations.
set.seed(2813308004)  # Sets the seed for simulation.

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

## FUNCTION HYPER-PARAMETER SPECIFICATION
process_args <- list(
  test_size = 0.2,
  cat_var = c('C')
)

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

sim_args1 <- list(
  model_name = '20k',
  treatment = 'A',
  cat_list = c(0, 1),
  mediator = c('L'),
  outcome = 'M',
  inv_datafile_name = 'med'
)

sim_args2 <- list(
  model_name = '20k',
  treatment = 'A',
  cat_list = c(0, 1),
  mediator = c('L', 'M'),
  outcome = 'Y',
  inv_datafile_name = 'pse'
)

## BOOTSTRAP INFERENCE
final_result <- bootstrap(n_iterations = 10,
                          num_cores_reserve = 6,
                          base_path = path,
                          folder_name = 'bootstrap',
                          dataset_name = dataset_name,
                          dag_name = paste0(dataset_name, '_DAG'),
                          process_args = process_args,
                          train_args = train_args,
                          sim_args_list = list(sim_args1, sim_args2))


library(dplyr)

## EFFECT ESTIMATION
final_result$ATE_A_Y <- final_result$E.Y.A.1.. - final_result$E.Y.A.0..
final_result$PSE_A_Y <- final_result$E.Y.A.1..L.A.0...M.A.0.. - final_result$E.Y.A.0..
final_result$PSE_A_L_Y <- final_result$E.Y.A.1.. - final_result$E.Y.A.1..L.A.0...
final_result$PSE_A_M_Y <- final_result$E.Y.A.1..L.A.0... - final_result$E.Y.A.1..L.A.0...M.A.0..
final_result$ATE_A_M <- final_result$E.M.A.1.. - final_result$E.M.A.0..
final_result$NDE <- final_result$E.M.A.1..L.A.0... - final_result$E.M.A.0..
final_result$NIE <- final_result$E.M.A.1.. - final_result$E.M.A.1..L.A.0...

# Create a new dataframe with the calculated columns
final_results_df <- final_result[, c("ATE_A_Y", "PSE_A_Y", "PSE_A_L_Y", "PSE_A_M_Y", "ATE_A_M", "NDE", "NIE")]

# Rename columns
colnames(final_results_df) <- c("ATE (A->Y)", "PSE (A->Y)", "PSE (A->L->Y)", "PSE (A->M->Y)", "ATE (A->M)", "NDE", "NIE")

## CONFIDENCE INTERVAL ESTIMATION
percentiles <- list()
percentile_values <- c(10, 90)
outcome_columns <- names(final_results_df)

for (column in outcome_columns) {
  percentiles[[column]] <- sapply(percentile_values, function(p) quantile(final_results_df[[column]], probs = p / 100, na.rm = TRUE))
}

# Convert the list of percentiles to a dataframe
percentiles_df <- do.call(cbind, percentiles)

# Name the rows with percentile values
row.names(percentiles_df) <- c("10th percentile", "90th percentile")

# Save the percentiles to a new CSV file
percentiles_csv_path <- paste0(path, dataset_name, "_80%CI.csv")
write.csv(percentiles_df, percentiles_csv_path, row.names = TRUE)
