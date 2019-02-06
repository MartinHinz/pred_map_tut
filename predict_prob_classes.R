predict_prob_classes <- function(my_model, my_env_data, my_evidence, probs = c(0,0.05))
{
  if("naiveBayes" %in% class(my_model)) {
    predict_type = "raw"
  } else if ("glm" %in% class(my_model)) {
    predict_type = "response"
  } else {
    stop("Unknown model!")
  }
  
  my_pcdata <- predict(my_model, newdata = my_evidence, type = predict_type)
  if("naiveBayes" %in% class(my_model)) {my_pcdata <- my_pcdata[,2]}
  
  thresholds <- quantile(my_pcdata[my_evidence$site],
           prob = probs)
  
  my_pdata <- predict(my_model, newdata = my_env_data, type = predict_type)
  
  if("naiveBayes" %in% class(my_model)) {my_pdata <- my_pdata[,2]}
  
  my_pdata_class <- cut(my_pdata, breaks = c(-Inf, thresholds, Inf),
                         labels=c("below_treshold", as.character(probs)),
                         include.lowest = TRUE, right = FALSE, ordered_result = TRUE )
  return(my_pdata_class)
}
