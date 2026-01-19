# Lab 10.9.5
library(ISLR2)
library(torchdatasets)
library(torch)
library(luz)

# Set seed for reproduce-ability
set.seed(1)

# Create our list of features to test

max_features_list = c(1000, 3000, 5000, 10000)

# Loop through number of features list
for (i in max_features_list) {
    
    # Set max features
    max_features <- i
    
    # Load the IMDB datasets with the specified number of features
    imdb_train <- imdb_dataset(
        root = ".", 
        download = TRUE,
        split="train",
        num_words = max_features
    )
    imdb_test <- imdb_dataset(
        root = ".", 
        download = TRUE,
        split="test",
        num_words = max_features
    )
    
    # Examine the first review
    imdb_train[1]$x[1:12]
    
    # Function to decode the review back to words
    word_index <- imdb_train$vocabulary
    decode_review <- function(text, word_index) {
        word <- names(word_index)
        idx <- unlist(word_index, use.names = FALSE)
        word <- c("<PAD>", "<START>", "<UNK>", word)
        words <- word[text]
        paste(words, collapse = " ")
    }
    decode_review(imdb_train[1]$x[1:12], word_index)
    
    # One-hot encoding function
    library(Matrix)
    one_hot <- function(sequences, dimension) {
        seqlen <- sapply(sequences, length)
        n <- length(seqlen)
        rowind <- rep(1:n, seqlen)
        colind <- unlist(sequences)
        sparseMatrix(i = rowind, j = colind,
                     dims = c(n, dimension))
    }
    
    # collect all values into a list
    train <- seq_along(imdb_train) %>% 
        lapply(function(i) imdb_train[i]) %>% 
        purrr::transpose()
    test <- seq_along(imdb_test) %>% 
        lapply(function(i) imdb_test[i]) %>% 
        purrr::transpose()
    
    # num_words + padding + start + oov token = 10000 + 3
    x_train_1h <- one_hot(train$x, 10000 + 3)
    x_test_1h <- one_hot(test$x, 10000 + 3)
    dim(x_train_1h)
    nnzero(x_train_1h) / (25000 * (10000 + 3))
    
    # Create validation set
    set.seed(3)
    ival <- sample(seq(along = train$y), 2000)
    itrain <- seq_along(train$y)[-ival]
    
    # Fit logistic regression model with glmnet
    library(glmnet)
    accuracy <- function(pred, truth) {
        mean(pred == truth) }
    
    y_train <- unlist(train$y)
    
    fitlm <- glmnet(x_train_1h[itrain, ], unlist(y_train[itrain]),
                    family = "binomial", standardize = FALSE)
    classlmv <- predict(fitlm, x_train_1h[ival, ]) > 0
    acclmv <- apply(classlmv, 2, accuracy,  unlist(y_train[ival]) > 0)
    
    # Plot accuracy vs. lambda
    par(mar = c(4, 4, 4, 4), mfrow = c(1, 1))
    plot(-log(fitlm$lambda), acclmv)
    
    # Find best lambda
    model <- nn_module(
        initialize = function(input_size = 10000 + 3) {
            self$dense1 <- nn_linear(input_size, 16)
            self$relu <- nn_relu()
            self$dense2 <- nn_linear(16, 16)
            self$output <- nn_linear(16, 1)
        },
        forward = function(x) {
            x %>% 
                self$dense1() %>% 
                self$relu() %>% 
                self$dense2() %>% 
                self$relu() %>% 
                self$output() %>% 
                torch_flatten(start_dim = 1)
        }
    )
    
    # Set up luz model
    model <- model %>% 
        setup(
            loss = nn_bce_with_logits_loss(),
            optimizer = optim_rmsprop,
            metrics = list(luz_metric_binary_accuracy_with_logits())
        ) %>% 
        set_opt_hparams(lr = 0.001)
    
    fitted <- model %>% 
        fit(
            list(
                torch_tensor(as.matrix(x_train_1h[itrain,]), dtype = torch_float()), 
                torch_tensor(unlist(train$y[itrain]))
            ),
            valid_data = list(
                torch_tensor(as.matrix(x_test_1h), dtype = torch_float()), 
                torch_tensor(unlist(test$y))
            ),
            dataloader_options = list(batch_size = 512),
            epochs = 10
        )
    
    plot(fitted) 
    input("Press [enter] to continue")
}
