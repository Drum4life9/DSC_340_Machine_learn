library(ISLR2)
library(torch)
library(luz)
library(lubridate)
library(magrittr)
library(dplyr)

# PROBLEM 9
# Fit a lag-5 AR model to the NYSE data
xdata = data.matrix(
    NYSE[, c("DJ_return", "log_volume", "log_volatility")]
)
istrain = NYSE[, "train"]
xdata = scale(xdata)

lagm = function(x, k=1) {
    n <- nrow(x)
    pad <- matrix(NA, k, ncol(x))
    rbind(pad, x[1:(n-k), ])
}

arframe <- data.frame(log_volume = xdata[, "log_volume"],
 L1 = lagm(xdata, 1), 
 L2 = lagm(xdata, 2),
 L3 = lagm(xdata, 3), 
 L4 = lagm(xdata, 4),
 L5 = lagm(xdata, 5)
 )

arframe <- arframe[-(1:5),]
istrain <- istrain[-(1:5)]

arfit <- lm(log_volume ~ ., data = arframe[istrain, ])
arpred <- predict(arfit, arframe[!istrain, ])
V0 <- var(arframe[!istrain, "log_volume"])
print("R^2 of Lag 5 AR model")
print(1 - mean((arpred - arframe[!istrain, "log_volume"])^2) / V0)

# Re-frame data to include month as a factor

arframed <- data.frame(month = month(NYSE[-(1:5), "date"], label = TRUE), arframe)
arfitd <- lm(log_volume ~ ., data = arframed[istrain, ])
arpredd <- predict(arfitd, arframed[!istrain, ])
print("R^2 of Lag 5 AR model with month factor")
V0 <- var(arframed[!istrain, "log_volume"])
print(1 - mean((arpredd - arframed[!istrain, "log_volume"])^2) / V0)

# Result: R^2 marginally increases


# PROBLEM 10: Linear AR RNN

n <- nrow(arframe)
xrnn <- data.matrix(arframe[, -1])
xrnn <- array(xrnn, c(n, 3, 5))
xrnn <- xrnn[,, 5:1]
xrnn <- aperm(xrnn, c(1,3,2))


model <- nn_module(
  initialize = function() {
    self$dense <- nn_linear(15, 1)
  },
  forward = function(x) {
    x %>% 
      torch_flatten(start_dim = 2) %>% 
      self$dense()
  }
)

model <- model %>% 
  setup(
    optimizer = optim_rmsprop,
    loss = nn_mse_loss()
  ) %>% 
  set_opt_hparams(lr = 0.001)

fitted <- model %>% fit(
  list(xrnn[istrain,, ], arframe[istrain, "log_volume"]),
  epochs = 30, # = 200,
  dataloader_options = list(batch_size = 64),
  valid_data =
    list(xrnn[!istrain,, ], arframe[!istrain, "log_volume"])
)
kpred <- as.numeric(predict(fitted, xrnn[!istrain,, ]))
print("R^2 of Linear AR model")
V0 <- var(arframe[!istrain, "log_volume"])
print(1 - mean((kpred - arframe[!istrain, "log_volume"])^2) / V0)


# PROBLEM 11: Nonlinear flattened AR model


x <- model.matrix(log_volume ~ . - 1, data = arframe)


arnnd <- nn_module(
    initialize = function() {
        self$dense <- nn_linear(15, 32)
        self$dropout <- nn_dropout(0.5)
        self$activation <- nn_relu()
        self$output <- nn_linear(32, 1)
        
    },
    forward = function(x) {
        x %>% 
            torch_flatten(start_dim = 2) %>% 
            self$dense() %>% 
            self$activation() %>% 
            self$dropout() %>% 
            self$output() %>% 
            torch_flatten(start_dim = 1)
    }
)
arnnd <- arnnd %>% 
    setup(
        optimizer = optim_rmsprop,
        loss = nn_mse_loss()
    ) %>% 
    set_opt_hparams(lr = 0.001)

fitted_flat_ar <- arnnd %>% fit(
    list(x[istrain,], arframe[istrain, "log_volume"]),
    epochs = 30, 
    dataloader_options = list(batch_size = 64),
    valid_data =
        list(x[!istrain,], arframe[!istrain, "log_volume"])
)
plot(fitted_flat_ar)

npred <- as.numeric(predict(fitted_flat_ar, x[!istrain, ]))
VO <- var(arframe[!istrain, "log_volume"])
print("R^2 of Nonlinear Flattened AR model")
print(1 - mean((arframe[!istrain, "log_volume"] - npred)^2) / V0)

# PROBLEM 12: Include day of week as factor in nonlinear AR model

arframe_dow <- data.frame(
    dow = NYSE[-(1:5), "day_of_week"], arframe)

n <- nrow(arframe_dow)
xrnn <- data.matrix(arframe_dow[, -1])
xrnn <- array(xrnn, c(n, 3, 5))
xrnn <- xrnn[,, 5:1]
xrnn <- aperm(xrnn, c(1,3,2))

x <- model.matrix(log_volume ~ . - 1, data = arframe_dow)


arnnd <- nn_module(
    initialize = function() {
        self$dense <- nn_linear(20, 32)
        self$dropout <- nn_dropout(0.5)
        self$activation <- nn_relu()
        self$output <- nn_linear(32, 1)
        
    },
    forward = function(x) {
        x %>% 
            torch_flatten(start_dim = 2) %>% 
            self$dense() %>% 
            self$activation() %>% 
            self$dropout() %>% 
            self$output() %>% 
            torch_flatten(start_dim = 1)
    }
)
arnnd <- arnnd %>% 
    setup(
        optimizer = optim_rmsprop,
        loss = nn_mse_loss()
    ) %>% 
    set_opt_hparams(lr = 0.001)

fitted_flat_ar_dow <- arnnd %>% fit(
    list(x[istrain,], arframe_dow[istrain, "log_volume"]),
    epochs = 30, 
    dataloader_options = list(batch_size = 64),
    valid_data =
        list(x[!istrain,], arframe_dow[!istrain, "log_volume"])
)
plot(fitted_flat_ar_dow)

npred <- as.numeric(predict(fitted_flat_ar_dow, x[!istrain, ]))
VO <- var(arframe_dow[!istrain, "log_volume"])
print("R^2 of Nonlinear Flattened AR model")
print(1 - mean((arframe_dow[!istrain, "log_volume"] - npred)^2) / V0)
