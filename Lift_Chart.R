# Function for creating Lift Charts - binary classification
# Could also be used for regression to check 

function (predicted, labels, cumulative = TRUE, n.buckets = 10) 
{

  # classes to numeric
  if (is.factor(labels)) 
    labels <- as.integer(as.character(labels))
  
  # create matrix and sort by desc of predicted values
  lift <- cbind(predicted, labels)
  lift <- lift[order(-lift[, 1]), ]
  
  # create deciles / buckets
  buckets <- ceiling(seq_along(lift[, 2])/floor(length(lift[,2])/n.buckets))
  cap <- floor(length(lift[, 2])/n.buckets) * n.buckets
  
  # for each buckets find mean of labels( additionally you can also include for predicted ones
  # and plot for comparison )
  lift <- aggregate(lift[1:cap, 2], by = list(buckets[1:cap]), 
                    mean)
  ylab <- "Lift"
  
  if (cumulative) {
    lift[, 2] <- cumsum(lift[, 2])/seq_along(lift[, 2])
    ylab <- "Cumulative lift"
  }
  
  plot(lift[, 1], lift[, 2]/mean(labels), type = "l", ylab = ylab, 
       xlab = "bucket")
  
  # additionally can also include lines for actual mean, predicted mean
  # accuracy, rmse etc.
}