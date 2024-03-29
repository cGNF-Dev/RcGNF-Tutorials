# RcGNF: R Wrapper for causal-Graphical Normalizing Flows

## About RcGNF

`RcGNF` is the R package wrapper for causal-Graphical Normalizing Flows (cGNF), a deep learning-based tool designed to answer causal questions using normalizing flows. It builds upon Graphical Normalizing Flows ([GNFs](https://github.com/AWehenkel/Graphical-Normalizing-Flows)) and Unconstrained Monotonic Neural Networks ([UMNNs](https://github.com/AWehenkel/UMNN)), with a focus on causality within a Directed Acyclic Graph (DAG) framework. `RcGNF` allows R users to integrate the advanced capabilities of cGNF into their data analysis workflows seamlessly.

---

## User Guide

This guide will help you install and utilize `RcGNF` within an R environment. 


### Tutorial Contents

1. [Setting up RcGNF](#setting-up-rcgnf)
2. [Setting up a Dataset](#setting-up-a-dataset)
   - [Preparing a Data Frame](#preparing-a-data-frame)
   - [Specifying a Directed Acyclic Graph (DAG)](#specifying-a-directed-acyclic-graph-dag)
3. [Training a Model](#training-a-model)
   - [Data Preprocessing](#data-preprocessing)
   - [Training](#training)
   - [Estimation](#estimation)
   - [Bootstrapping](#bootstrapping)
4. [Coding Tutorials with Simulated Data](#coding-tutorials-with-simulated-data)
   - [Estimating an Average Treatment Effect (ATE)](#estimating-average-treatment-effects-ate)
   - [Estimating Conditional Average Treatment Effects (CATE)](#estimating-conditional-average-treatment-effects-cate)
   - [Estimating Natural Direct and Indirect Effects](#estimating-direct-and-indirect-effects)
   - [Estimating Path-Specific Effects](#estimating-path-specific-effects)
   - [Conducting a Sensitivity Analysis](#conducting-a-sensitivity-analysis)
   - [Constructing Confidence Intervals with the Bootstrap](#constructing-confidence-intervals-with-the-bootstrap)

---

## Setting up RcGNF

1. **Install R**:
   
   Ensure you have the latest version of R installed on your system.

2. **Install Python**:
   
   RcGNF requires Python 3.9. Ensure you have Python 3.9 installed, other versions may not be compatible with the dependencies for `cGNF`:
   
   - [Download Python 3.9.13](https://www.python.org/downloads/release/python-3913/)
   
   During the installation, make sure to tick the option `Add Python to PATH` to ensure Python is accessible from the command line.

3. **Install RcGNF**:

   In R or RStudio, install the RcGNF package using the following command:

   ```R
   devtools::install_github("cGNF-Dev/RcGNF")
   ```
   
   If you encounter the error message `Python installation not found in common locations. Please set the RCGNF_PYTHON_PATH environment variable to your Python installation and reload the package`, proceed as follows:

   1. Find the Python path by typing `which python` (macOS/Linux) or `where python` (Windows) in the terminal or Command Prompt.

   2. Set the `RCGNF_PYTHON_PATH` environment variable in R or RStudio:

   ```R
   Sys.setenv(RCGNF_PYTHON_PATH = "/your/python/path")
   ```
   
   3. Reinstall the RcGNF package.
   
5. **Load RcGNF**:

   Once installed, load the package using:

   ```R
   library(RcGNF)
   ```

   *Note for Windows Users*:
   
   - Currently, the RcGNF package is only functional within RStudio on Windows environments. Running it directly in R may lead to errors. We recommend using RStudio for a seamless experience with RcGNF on Windows.

---


## Setting up a Dataset

### Preparing a Data Frame

   Ensure your data frame is stored in CSV format with the first row set as variable names and subsequent rows as values. An example structure:
        
   | X         | Y          | Z         |
   |-----------|------------|-----------|
   | -0.673503 | 0.86791503 | -0.673503 |
   | 0.7082311 | -0.8327477 | 0.7082311 |
   | ...       | ...        | ...       |

   *Note*: any row with at least one missing value will be automatically removed during the data preprocessing stage (see [Training a Model](#training-a-model)).

---

### Specifying a Directed Acyclic Graph (DAG)

   `RcGNF` utilizes an adjacency matrix in CSV format to recognize a DAG. Use the following steps in Python to generate an adjacency matrix:
    
   #### a. **Import Required Libraries**:
    
   ```R
   library(igraph)
   ```
    
   #### b. **Draw the DAG**:
    
   Define your DAG structure using the `igraph`:
    
   ```python
   Your_DAG_name <- graph_from_edgelist(matrix(c("parent", "child"), ncol  =  2, byrow  =  TRUE), directed  =  TRUE)
   ```
    
   For example, with a simple DAG X &rarr; Y &rarr; Z, the argument will be as follows:
    
   ```python
   Simple_DAG <- graph_from_edgelist(matrix(c("X", "Y", "Y", "Z"), ncol  =  2, byrow  =  TRUE), directed  =  TRUE)
   ```
    
   #### c. **Convert the DAG to an Adjacency Matrix**:
    
   ```python
   your_adj_mat_name <- as.matrix(as_adjacency_matrix(Your_DAG_name, type  =  "both", attr  =  NULL, sparse  =  FALSE))
   your_adj_mat_name <- as.data.frame(your_adj_mat_name)
   ```
    
   Save the matrix as a CSV file:
    
   ```python
   write.csv(your_adj_mat_name, file  =  paste0('/path_to_data_directory/', 'your_adj_mat_name', '.csv'), row.names  =  TRUE)
   ```
    
   #### d. **Manually Create an Adjacency Matrix**:
    
   Alternatively, you can manually create an adjacency matrix in a CSV file by listing variables in both the first row and the first column. Here's how you interpret the matrix:
    
   - The row represents the parent node, and the column represents the child node.
      
   - If the cell at row X and column Y (i.e., position (X, Y)) contains a 1, it means X leads to Y.
      
   - If it contains a 0, it means X does not lead to Y.
  
   - Remember, since this is a directed graph, a 0 at position (Y, X) doesn't imply a 0 at position (X, Y).
    
   For example, the below adjacency matrix describes a DAG where X &rarr; Y &rarr; Z. 
    
   |   | X | Y | Z |
   |---|---|---|---|
   | X | 0 | 1 | 0 |
   | Y | 0 | 0 | 1 |
   | Z | 0 | 0 | 0 |

   _Note_: 
     
   - Make sure you save the adjacency matrix in the same directory as your dataframe.
   
---

## Training a Model

### Essential Functions

`RcGNF` is implemented in three stages, corresponding to three separate Python functions:

1. **`process`**: Prepares the dataset and adjacency matrix.
   
2. **`train`**: Trains the model.
   
3. **`sim`**: Estimates potential outcomes.

Additionally, a **`bootstrap`** function is provided to facilitate parallel execution of these functions across multiple CPU cores.

---

### Data Preprocessing

   ```R
    process(
        path = '/path_to_data_directory/',  # File path where the dataset and DAG are located
        dataset_name = 'your_dataset_name',  # Name of the dataset
        dag_name =  'you_adj_mat_name',  # Name of the adjacency matrix (DAG) to be used
        test_size = 0.2,  # Proportion of data used for the validation set
        cat_var = c('X', 'Y'),  # List of categorical variables
        sens_corr = list(`("X", "Y")` = 0.2, `("C", "Y")` = 0.1), # Vector of sensitivity parameters (i.e., normalized disturbance correlations)
        seed = NULL  # Seed for reproducibility
    )
   ```

   *Notes*:
   - `cat_var`: If the dataset has no categorical variables, set `cat_var = NULL`.

   - `sens_corr`: If specified, the train and sim functions will produce bias-adjusted estimates using the supplied disturbance correlations.
   
   - The function will automatically remove any row that contains at least one missing value.

   - The function converts the dataset and the adjacency matrix into tensors. These tensors are then packaged into a PKL file named after `dataset_name` and saved within the `path` directory. This PKL file is later used for model training.

---

### Training
   
   ```R
    train(
        path = '/path_to_data_directory/',  # File path where the PKL file is located
        dataset_name = 'your_dataset_name',  # Name of the dataset
        model_name = 'models',  # Name of the folder where the trained model will be saved
        trn_batch_size = 128,  # Training batch size
        val_batch_size = 2048,  # Validation batch size
        learning_rate = 1e-4,  # Learning rate
        seed = NULL,  # Seed for reproducibility
        nb_epoch = 50000,  # Number of total epochs
        emb_net = c(90, 80, 60, 50),  # Architecture of the embedding network (nodes per hidden layer)
        int_net = c(50, 40, 30, 20),  # Architecture of the integrand network (nodes per hidden layer)
        nb_estop = 50,  # Number of epochs for early stopping
        val_freq = 1  # Frequency per epoch with which the validation loss is computed
    )
   ```

   *Notes*:
   - `model_name`: The folder will be saved under the `path`.
     
   - Hyperparameters that influence the neural network's performance include the number of layers and nodes in `emb_net` & `int_net`, the early stopping criterion in `nb_estops`, the learning rate in `learning_rate`, the training batch size in `trn_batch_size`, and the frequency with which the validation loss is evaluated to determine whether the early stopping criterion has been met in `val_freq`. When setting these parameters, always be mindful of the potential for bias (in simple models, trained rapidly, with a stringent early stopping criterion) versus overfitting (in complex models, trained slowly, with little regularization).

---

### Estimation

   ```R
    sim(
        path = '/path_to_data_directory/',  # File path where the PKL file is located
        dataset_name = 'your_dataset_name',  # Name of the dataset
        model_name = 'models',  # Name of the folder where the trained model is located
        n_mce_samples = 50000,  #  Number of Monte Carlo draws from the trained distribution model
        treatment = 'X',  # Treatment variable
        cat_list = c(0, 1),  # Treatment values for counterfactual outcomes
        moderator = 'C',  # Specify to conduct moderation analysis (i.e., compute effects conditional on the supplied moderator)
        quant_mod = 4,  # If the moderator is continuous, specify the number of quantiles used to evaluate the conditional effects
        mediator = c('M1', 'M2'),  # List mediators for mediation analysis (i.e., to compute direct, indirect, or path-specific effects)
        outcome = 'Y',   # Outcome variable
        inv_datafile_name = 'your_counterfactual_dataset'  # Name of the file where Monte Carlo samples are saved
    )
   ```

   *Notes*:
   - Increasing `n_mce_samples` helps reduce simulation error during the inference stage but may increase computation time.

   - `cat_list`: Multiple treatment values are permitted. If a mediator is specified, only two values are allowed, where the first value represents the control condition and the second represents the treated condition.

   - `moderator`: If the moderator is categorical and has fewer than 10 categories, the function will display potential outcomes based on different moderator values.

     For continuous moderators or those with over ten categories, the outcomes are displayed based on quantiles, determined by `quant_mod`. By default, with `quant_mod = 4`, the moderator values are divided on **quartiles**.

     When conditional treatment effects are not of interest, or the dataset has no moderators, set `moderator = NULL`.

   - `mediator`: Multiple mediators are permitted. When specifying several mediators, ensure they are supplied in their causal order, in which case the function returns a set of path-specific effects.

     When direct, indirect, or path-specific effects are not of interest, or the dataset has no mediators, set `mediator = NULL`.

     Moderated mediation analysis is available by specifying the `moderator` and `mediator` parameters simultaneously.

   - `inv_datafile_name`: The function, by default, creates `potential_outcome.csv`, which holds counterfactual samples derived from the `cat_list` input values, and `potential_outcome_results.csv`, cataloging the computed potential outcomes. These outputs are saved in the designated `path` directory.

     With `mediator` specified, additional counterfactual data files will be produced for each path-specific effect. These files are named with the suffix m*n*_0 or m*n*_1, corresponding to different treatment conditions.

     The suffix '_0' indicates the scenario where the treatment and all subsequent mediators past the *n*th mediator are set to the control condition, whereas the *n*th mediator and those before it assume the treated condition.

     Conversely, the suffix '_1' indicates the scenario where the treatment and all mediators following the *n*th mediator are in the treated condition, and those mediators preceding and including the *n*th mediator are in the control condition.

---

### Bootstrapping

   ```R
   process_args <- list(
      seed = 2121380
   )

   train_args <- list(
      seed = 2121380
   )

   sim_args1 <- list(
      treatment = 'A',
      outcome = 'Y',
      inv_datafile_name = 'A_Y'
   )

   sim_args2 <- list(
      treatment = 'C',
      outcome = 'Y',
      inv_datafile_name = 'C_Y'
   )

    bootstrap(
       n_iterations = 10,  # Number of bootstrap iterations
       num_cores_reserve = 2,  # Number of cores to reserve
       base_path = '/path_to_data_directory/',  # Base directory where the dataset and DAG are located
       folder_name = 'bootstrap_2k',  # Folder name for this bootstrap session
       dataset_name = 'your_dataset_name',  # Name of the dataset being used
       dag_name = 'you_adj_mat_name',  # Name of the DAG file associated with the dataset
       process_args = process_args,  # Arguments for the data preprocessing function
       train_args = train_args,  # Arguments for the model training function
       sim_args_list =  list(sim_args1, sim_args2)  # List of arguments for multiple estimation configurations
    )
   ```

   *Notes*:
   - The function generates a file named `<dataset_name>_result.csv` under `base_path`, which contains all the potential outcome results from each bootstrap iteration.

   - To skip certain stages, you can add `skip_process = TRUE`, `skip_train = TRUE`, or set `sim_args_list = NULL`.
     
   - The function generates `n_iterations` number of folders under `base_path`, each named with the `folder_name` followed by an iteration suffix.
     
   - `base_path`, `dataset_name`, and `dag_name` are automatically included in `process_args`, `train_args`, and `sim_args_list`, so you don't need to specify them separately for each set of arguments.

#### Remember to adjust paths, environment names, and other placeholders.

---

## Coding Tutorials with Simulated Data

### Estimating Average Treatment Effects (ATE)
(See **`RcGNF_ATE.py`** for detailed implementation)

Define the data generating process (DGP) as:

$$
\begin{align*}
A &\sim \text{Binomial}(0.5) \\
\\
Y &\sim 
\begin{cases} 
\text{Binomial}(0.6) & \text{if } A = 1 \\
\text{Binomial}(0.4) & \text{if } A = 0 
\end{cases}
\end{align*}
$$

The corresponding directed acyclic graph (DAG):

```mermaid
stateDiagram
   A --> Y
```

#### Steps:

1. Generate _N_ samples from the DGP and construct the corresponding adjacency matrix.
2. Store both the sample dataframe and adjacency matrix at the designated `path`.
3. Process the dataset and initiate model training.
4. Conduct potential outcome estimation to estimate:

$$
\begin{align*}
   &E[Y(A=0)] \\
   &E[Y(A=1)]
\end{align*}
$$

$$
\begin{align*}
\text{Average Treatment Effect (ATE)} &= E[Y(A=1)] - E[Y(A=0)]
\end{align*}
$$

---

### Estimating Conditional Average Treatment Effects (CATE)
(See **`RcGNF_CATE.py`** for detailed implementation)

Define the DGP as:

$$
\begin{align*}
C &\sim \text{Binomial}(0.5) \\
\\
\epsilon_A &\sim \text{Logistic}(0, 1) \\
\epsilon_Y &\sim \text{Normal}(0, 1) \\
\\
A &= 0.3 \cdot C + \epsilon_A \\
Y &= 0.4 \cdot C + 0.2 \cdot A + \epsilon_Y
\end{align*}
$$

The corresponding DAG:

```mermaid
stateDiagram
   A --> Y
   C --> A
   C --> Y
```

#### Steps:

1. Generate _N_ samples from the DGP and construct the corresponding adjacency matrix.
2. Store both the sample dataframe and adjacency matrix at the designated `path`.
3. Process the dataset and initiate model training.
4. Conduct potential outcome estimation to estimate:

$$
\begin{align*}
   &E[Y(A=0 | C=0)] \\
   &E[Y(A=0 | C=1)] \\
   &E[Y(A=1 | C=0)] \\
   &E[Y(A=1 | C=1)]
\end{align*}
$$


$$
\begin{align*}
&\text{Conditional Average Treatment Effects (CATE)}= \\
&E[Y(A=1 | C=0)] - E[Y(A=0 | C=0)] \\
&and \\
&E[Y(A=1 | C=1)] - E[Y(A=0 | C=1)]
\end{align*}
$$

---

### Estimating Direct and Indirect Effects 
(See **`RcGNF_ND(I)E.py`** for detailed implementation)

Define the DGP as:

$$
\begin{align*}
C &\sim \text{Binomial}(0.4) \\
\\
\epsilon_A &\sim \text{Normal}(0, 1) \\
\epsilon_M &\sim \text{Logistic}(0, 1) \\
\epsilon_Y &\sim \text{Logistic}(0, 1) \\
\\
A &= 0.2 \cdot C + \epsilon_A \\
M &= 0.25 \cdot A + \epsilon_M \\
Y &= 0.2 \cdot C + 0.1 \cdot A + 0.4 \cdot M + \epsilon_Y
\end{align*}
$$

The corresponding DAG:

```mermaid
stateDiagram
   A --> M
   M --> Y
   A --> Y
   C --> A
   C --> Y
```

#### Steps:

1. Generate _N_ samples from the DGP and construct the corresponding adjacency matrix.
2. Store both the sample dataframe and adjacency matrix at the designated `path`.
3. Process the dataset and initiate model training.
4. Conduct potential outcome estimation to estimate:

$$
\begin{align*}
   &E[Y(A=0)] \\
   &E[Y(A=0, M(A=1))] \\
   &E[Y(A=1)] \\
   &E[Y(A=1, M(A=0))]
\end{align*}
$$

* Direct and Indirect Effects Decomposition:

$$
\begin{align*}
   \text{Natural Direct Effect (NDE)} & = E[Y(A=1, M(A=0))] - E[Y(A=0)] \\
   \\
   \text{Natural Indirect Effects (NID)} & = E[Y(A=1)] - E[Y(A=1, M(A=0))] \\
   \\
   \text{Total Direct Effect (TDE)} & = E[Y(A=1)] - E[Y(A=0, M(A=1))] \\
   \\
   \text{Pure Indirect Effects (NID)} & = E[Y(A=0, M(A=1))] - E[Y(A=0)] \\
\end{align*}
$$

_Note_: for conditional direct and indirect effect estimates, adjust the `moderator=None` parameter to `moderator='C'`.

---

### Estimating Path-Specific Effects 
(See **`RcGNF_PSE.py`** for detailed implementation)

Define the DGP as:

$$
\begin{align*}
C &\sim \text{Binomial}(0.5) \\
\\
\epsilon_A &\sim \text{Normal}(0, 1) \\
\epsilon_L &\sim \text{Logistic}(0, 1) \\
\epsilon_M &\sim \text{Logistic}(0, 1) \\
\epsilon_Y &\sim \text{Normal}(0, 1) \\
\\
A &= 0.2 \cdot C + \epsilon_A \\
L &= 0.2 \cdot C + 0.1 \cdot A + \epsilon_L \\
M &= 0.1 \cdot C + 0.1 \cdot A + 0.2 \cdot L + \epsilon_M \\
Y &= 0.1 \cdot C + 0.1 \cdot A + 0.2 \cdot L + 0.2 \cdot M + \epsilon_Y \cdot (1 + 0.2 \cdot C)
\end{align*}
$$

The corresponding DAG:

```mermaid
stateDiagram
   A --> L
   L --> M
   A --> M
   M --> Y
   A --> Y
   L --> Y
   C --> A
   C --> L
   C --> M
   C --> Y
```

#### Steps:

1. Generate _N_ samples from the DGP and construct the corresponding adjacency matrix.
2. Store both the sample dataframe and adjacency matrix at the designated `path`.
3. Process the dataset and initiate model training.
4. Conduct potential outcome estimation to estimate:

$$
\begin{align*}
   &E[Y(A=0)] \\
   &E[Y(A=0, L(A=1))] \\
   &E[Y(A=0, L(A=1), M(A=1))] \\
   &E[Y(A=1)] \\
   &E[Y(A=1, L(A=0))] \\
   &E[Y(A=1, L(A=0), M(A=0))]
\end{align*}
$$

* Path-Specific Effects Decomposition:

$$
\begin{align*}
   &\text{Path-Specific Effect of A &rarr; Y}= \\
   &E[Y(A=1, L(A=0), M(A=0))] - E[Y(A=0)] \\
   &or \\
   &E[Y(A=0, L(A=1), M(A=1))] - E[Y(A=1)] \\
   \\
   &\text{Path-Specific Effect of A &rarr; L &rarr; Y; A &rarr; L &rarr; M &rarr; Y}= \\ 
   &E[Y(A=1)] - E[Y(A=1, L(A=0))] \\
   &or \\
   &E[Y(A=0)] - E[Y(A=0, L(A=1))] \\
   \\
   &\text{Path-Specific Effect of A &rarr; M &rarr; Y}= \\ 
   &E[Y(A=1, L(A=0))] - E[Y(A=1, L(A=0), M(A=0))] \\
   &or \\
   &E[Y(A=0, L(A=1))] - E[Y(A=0, L(A=1), M(A=1))] 
\end{align*}
$$

_Note_: for conditional path-specific effect estimates, adjust the `moderator=None` parameter to `moderator='C'`.

---

### Conducting a Sensitivity Analysis
(See **`RcGNF_sensitivity.py`** for detailed implementation)

Based on **`RcGNF_ND(I)E.py`**, refine the DGP to simulate correlated error terms $\epsilon_A$ and $\epsilon_Y$ with $\rho_{\epsilon_A,\epsilon_Y} \approx 0.15$ and $\epsilon_M$ and $\epsilon_Y$ with $\rho_{\epsilon_M,\epsilon_Y} \approx 0.2$, induced by unobserved confounders $U_1$ and $U_2$:

$$
\begin{align*}
C &\sim \text{Binomial}(0.4) \\
\\
U_1 &\sim \text{Normal}(0, 3) \\
U_2 &\sim \text{Normal}(0, 3) \\
\\
\epsilon_A &\sim \text{Normal}(0, 1) + 0.2 \cdot U_1\\
\epsilon_M &\sim \text{Logistic}(0, 1) + 0.4 \cdot U_2\\
\epsilon_Y &\sim \text{Logistic}(0, 1) + 0.2 \cdot U_1 + 0.25 \cdot U_2\\
\\
A &= 0.2 \cdot C + \epsilon_A \\
M &= 0.25 \cdot A + \epsilon_M \\
Y &= 0.2 \cdot C + 0.1 \cdot A + 0.4 \cdot M + \epsilon_Y
\end{align*}
$$

The corresponding DAG:

```mermaid
stateDiagram
   U1 --> A
   U1 --> Y
   U2 --> M
   U2 --> Y
   A --> M
   M --> Y
   A --> Y
   C --> A
   C --> Y
```

#### Steps:

1. Generate _N_ samples from the DGP and construct the corresponding adjacency matrix. Exclude U1 and U2 from the dataframe and adjacency matrix to simulate the presence of unobserved confounding.
2. Store both the sample dataframe and adjacency matrix at the designated `path`.
3. Process the dataset by adding `sens_corr` to test correlations of error terms, and then initiate model training.
4. Conduct potential outcome estimation to estimate desired effects.

---

### Constructing Confidence Intervals with the Bootstrap
(See **`RcGNF_bootstrap.py`** for detailed implementation)

Define the DGP and DAG as same as in **`RcGNF_PSE.py`**.

#### Steps:

1. Generate _N_ samples from the DGP and construct the corresponding adjacency matrix.
2. Store both the sample dataframe and adjacency matrix at the designated `base_path`.
3. Initiate _n_ bootstrapping iterations.
4. In each iteration, perform two stages of potential outcome estimations to estimate:

$$
\begin{align*}
   &E[M(A=0)] \\
   &E[M(A=0, L(A=1))] \\
   &E[M(A=1)] \\
   &E[M(A=1, L(A=0))]
\end{align*}
$$

AND

$$
\begin{align*}
   &E[Y(A=0)] \\
   &E[Y(A=0, L(A=1))] \\
   &E[Y(A=0, L(A=1), M(A=1))] \\
   &E[Y(A=1)] \\
   &E[Y(A=1, L(A=0))] \\
   &E[Y(A=1, L(A=0), M(A=0))]
\end{align*}
$$

5. Calculate the 80% Confidence Interval (CI) for the desired effects by identifying the _10th_ and _90th_ percentiles.

---


