# example

# Setting up environment and paths
base_path <- '/Users/jessezhou/Desktop/cGNF_test/rcGNF/'
folder <- 'ATE_2k'
path <- file.path(base_path, folder, '')
dataset_name <- 'ATE_2k'

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}

# Data Simulation
obs <- 2000
set.seed(1001)

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

setwd('/Users/jessezhou/Desktop/RcGNF') # install the package first
devtools::load_all()

library(RcGNF)

process(path = path,
        dataset_name = dataset_name,
        dag_name = paste0(dataset_name, '_DAG'),
        sens_corr=NULL,
        test_size = 0.2,
        cat_var = c("A", "Y"),
        seed = 1001)

train(path = path,
      dataset_name = dataset_name,
      model_name = "models",
      trn_batch_size = 128,
      val_batch_size = 4096,
      learning_rate = 1e-4,
      seed = 1001,
      nb_epoch = 50000,
      emb_net = c(100, 90, 80, 70, 60),
      int_net = c(60, 50, 40, 30, 20),
      nb_estop = 50,
      val_freq = 1)

sim(path = path,
    dataset_name = dataset_name,
    model_name = "models",
    n_mce_samples = 10000,
    treatment = 'A',
    cat_list = c(0, 1),
    moderator = NULL,
    quant_mod = 4,
    mediator = NULL,
    outcome = 'Y',
    inv_datafile_name = 'rsim')

