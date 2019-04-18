SelectComps <- function(doc_ids, pairs, n_select, x = NULL, y = NULL,
                        mean_function = list("meanPref", list("meanZero")),
                        likelihood  = "likErf",
                        inference_method = "infLaplace",
                        covariance_function = list("covSum", 
                                                   list(list("covPref", 
                                                             list("covNoise")),
                                                        list("covNoise"))),
                        theta = list(mean = numeric(), 
                                     cov = c(log(1), log(10^-3)))){
  ### functions ###
  random_oracle = function(n = 1) sample(c(-1, 1), n)
  ### operations ###
  # for missing x input
  if(is.null(x)){
    # sample first 20 observations randomly
    train_ind <- sample(x = 1:nrow(pairs), size = n_select);
    x <- pairs[train_ind, , drop = FALSE]
    return(x)
  }
  # for valid x
  Xrows0 <- nrow(x)
  while(nrow(x) < (Xrows0 + n_select)){
    res <- gp(theta, inference_method, mean_function, covariance_function, likelihood, 
              x, y, pairs)
    bald_scores <- bald_score(res$FMU, res$FS2)
    x <- rbind(x, pairs[which.max(bald_scores), ])
    # overwrites y in the function, but does not return outside
    y <- c(y, random_oracle())
  }
  rownames(x) <- NULL
  return(x)
}


# old code
# SelectComps <- function(doc_ids, pairs, n_select, x = NULL, y = NULL){
#   ### functions ###
#   random_oracle = function(n = 1) sample(c(-1, 1), n)
#   ### priors ###
#   # octave params
#   mean_function       = list("meanPref", list("meanZero"))
#   likelihood          = "likErf";
#   inference_method    = "infLaplace";
#   covariance_function = list("covSum",
#                              list(list("covPref", list("covNoise")),
#                                   list("covNoise")))
#   theta <- list(mean = numeric(), cov = c(log(1), log(1)))
#   ### operations ###
#   if(is.null(x)){
#     # sample first 20 observations randomly
#     train_ind = sample(x = 1:nrow(pairs), size = n_select);
#     x = pairs[train_ind, , drop = FALSE]
#     return(x)
#   }
#   # generate bald scores
#   res <- gp(theta, inference_method, mean_function, covariance_function,
#             likelihood, x, y, pairs)
#   mu = res$FMU
#   sigma2 = res$FS2
#   bald_scores = bald_score(mu, sigma2)
#   # objects to store output
#   next_comp <- pairs[which.max(bald_scores), ]
#   y_guess = y
#   for(i in 1:n_select){
#     x = rbind(x, next_comp)
#     y_guess = c(y_guess, random_oracle())
#     #run gp and get bald_scores again
#     res <- gp(theta, inference_method, mean_function, covariance_function,
#               likelihood, x, y_guess, pairs)
#     mu = res$FMU
#     sigma2 = res$FS2
#     bald_scores = bald_score(mu, sigma2)
#     #which pair to compare next?
#     next_comp = pairs[which.max(bald_scores), ]
#   }
#   rownames(x) <- NULL
#   return(x)
# }