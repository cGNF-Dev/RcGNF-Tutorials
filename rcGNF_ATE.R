# Setting up environment and paths
base_path <- '/Users/jessezhou/Desktop/cGNF_test/rcGNF/'
folder <- '_ATE'
path <- file.path(base_path, folder, '')
dataset_name <- 'ATE_20k'

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}

# Data Simulation
obs <- 20000
set.seed(2813308004)

A <- rbinom(n = obs, size = 1, prob = 0.5)
Y <- ifelse(A == 1, rbinom(n = obs, size = 1, prob = 0.6), rbinom(n = obs, size = 1, prob = 0.4))

df <- data.frame(A = A, Y = Y)
df_filename <- file.path(path, paste0(dataset_name, '.csv'))
write.csv(df, df_filename, row.names = FALSE)

# DAG Specification
library(igraph)

simDAG <- graph_from_edgelist(matrix(c("A", "Y"), ncol = 2, byrow = TRUE), directed = TRUE)
plot(simDAG)

# Convert the adjacency matrix to a base R matrix
adj_matrix <- as.matrix(as_adjacency_matrix(simDAG, type = "both", attr = NULL, sparse = FALSE))
df_cDAG <- as.data.frame(adj_matrix)

# Set the row names
rownames(df_cDAG) <- V(simDAG)$name

print("------- Adjacency Matrix -------")
print(df_cDAG)

write.csv(df_cDAG, file.path(path, paste0(dataset_name, '_DAG.csv')), row.names = TRUE)

# devtools::install_github("cGNF-Dev/RcGNF")  # install the package first

library(RcGNF)

process(path = path,
        dataset_name = dataset_name,
        dag_name = paste0(dataset_name, '_DAG'),
        sens_corr=NULL,
        test_size = 0.2,
        cat_var = c("A", "Y"),
        seed = NULL)

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

sim(path = path,
    dataset_name = dataset_name,
    model_name = "20k",
    n_mce_samples = 10000,
    treatment = 'A',
    cat_list = c(0, 1),
    moderator = NULL,
    mediator = NULL,
    outcome = 'Y',
    inv_datafile_name = 'rsim_20k')

