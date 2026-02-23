library(ISLR2)
library(glmnet)
library(keras3)

# Problem 7

df <- na.omit(tibble(Default)) %>% 
  mutate(default = ifelse(default == "Yes", 1, 0))
n <- nrow(df)
set.seed(13)
ntest <- trunc(n / 3)
testid <- sample(1:n, ntest)

###
lfit <- glm(default ~ ., data = df[-testid, ])
lpred <- predict(lfit, df[testid, ])
log_error <- with(df[testid, ], mean(abs(lpred - default)))

###
x <- scale(model.matrix(default ~ . - 1, data = df))
y <- df$default

###

cvfit <- cv.glmnet(x[-testid, ], y[-testid], type.measure = "mae")
cpred <- predict(cvfit, x[testid, ], s = "lambda.min")
mean(abs(y[testid] - cpred))

###

modnn <- keras_model_sequential(input_shape = ncol(x)) |>
  layer_dense(units = 10, activation = "relu") |>
  layer_dropout(rate = 0.4) |> 
  layer_dense(units = 1)

compile(modnn, loss = "mse", optimizer = optimizer_rmsprop(),
        metrics = list("mean_absolute_error"))

### this one takes a while (1500 epochs!)
history <- fit(modnn, x[-testid, ], y[-testid], epochs = 500, 
               batch_size=32, validation_data=list(x[testid, ], y[testid]))

evaluate(modnn, x[testid, ], y[testid])
npred <- predict(modnn, x[testid, ])
nn_error <- mean(abs(y[testid] - npred))

# comparing logistic regression with NN
log_error
nn_error

# neural network outperforms logistic regression by ~60% or 0.03 percentage points

## Question 8


image_files <- list.files("animal_photos", full.names = TRUE)
num_images <- length(image_files)

x <- array(dim = c(num_images, 224, 224, 3))
for (i in 1:num_images) {
  img <- image_load(image_files[i], target_size = c(224, 224))
  x[i,,, ] <- image_to_array(img)
}

x <- imagenet_preprocess_input(x)
model <- application_resnet50(weights = "imagenet")
summary(model)

pred6 <- predict(model, x) |>
  imagenet_decode_predictions(top = 3)
names(pred6) <- basename(image_files)
print(pred6)

# image 1 correctly identified as cat (although egyptian cat is a bit too specific)
# image 2 correctly identified as llama
# image 3 close, says miniature poodle but i think this is a poodle mix
# image 4 is C3-PO, didn't expect it to get it, predicted altar/organ
# image 5 is correct, hog
# image 6 is incorrect, photo of turkey but predicted as ostrich/hen
# image 7 correct as a dog, but predicted standard poodle, this is golden doodle
# image 8 predicts "sorrel", which is a specific brown horse apparently, which is the image



