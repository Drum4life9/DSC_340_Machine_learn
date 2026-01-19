library(ISLR2)
library(keras)
library(glmnet)
library(ggplot2)
library(glue)
# uncomment this for first run
# reticulate::use_condaenv(condaenv = 'r-tensorflow')

data = Default # Assign default data to data variable

# First let's fit a linear model to predict default using 
# income, balance, and student status
n = nrow(data)
set.seed(42)

# Give us a test set based on 1/3 the data
ntest = trunc(n / 3)
testid = sample(1:n, ntest)

# Create our x and y lists to do the logistic regression
x = scale(model.matrix(default ~ . -1, data))
y = data$default

# Fit our cross-validated glmnet model
# use entropy loss
timing_result_glmnet <- system.time(
    cvfit <- cv.glmnet(x[-testid, ], y[-testid], type.measure = 'class', family = 'binomial')
)
cpred = predict(cvfit, x[testid, ],
                  s = 'lambda.min')
# Confusion matrix of output
print(with(data[testid, ], table(default, cpred > 0.5)))

# Create a model with dense layer, dropout, and output layer
neuralModel = keras_model_sequential() %>%
    layer_dense(units = 10, activation = 'relu', input_shape = ncol(data)) %>%
    layer_dropout(rate = 0.4) %>% # This dropout rate is variable!
    layer_dense(units = 1, activation = 'sigmoid')

# Compile the model, setting the appropriate loss and optimizer for training
neuralModel %>% compile(loss = 'binary_crossentropy', 
                        optimizer = optimizer_rmsprop(), metrics = c("accuracy"))

# Train the model!
timing_result_nn = system.time(
    history <- neuralModel %>% fit(
        x[-testid, ], 
        as.numeric(y[-testid]) - 1,
        epochs = 40,
        batch_size = 32,
        validation_split = 0.2
    )
)
# plot the history model
plot(history)

# use the model to make predictions after training
nnpred = neuralModel %>% predict(x[testid, ])
# Confusion matrix of output, compare to the linear model
print('Neural Network Confusion Matrix:')
print(with(data[testid, ], table(default, nnpred > 0.5)))
print(glue('Took {timing_result_nn[3]} seconds to train neural network'))
# Confusion matrix of glmnet
print('GLMNet Confusion Matrix:')
print(with(data[testid, ], table(default, cpred > 0.5)))
print(glue('Took {timing_result_glmnet[3]} seconds to train GLMNet'))

