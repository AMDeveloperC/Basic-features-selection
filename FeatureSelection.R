library('FSelector');

# return the top k word most frequent
most_frequent <- function(dataset, features, etichette) {
    mdataset <- as.matrix(dataset[,1:length(dataset)-1]);
    selected <- mdataset[, order(colSums(mdataset), decreasing=T)][,1:features];
    selected <- cbind(selected, etichette);
    colnames(selected)[ncol(selected)] <- "Label";
    return(selected);
}

# perform the feature selection using information gain method
informationGainSelection <- function(dataset, features, etichette) {
    weights <- information.gain(Label~., dataset);
    subset <- cutoff.k(weights, features);
    selected <- dataset[subset];
    selected <- cbind(selected, etichette);
    colnames(selected)[ncol(selected)] <- "Label";
    return(selected);
}

# perform the feature selection using chi squared test
chisquared <- function(dataset, features, etichette) {
    weights <- chi.squared(Label~., dataset);
    subset <- cutoff.k(weights, features);
    selected <- dataset[subset];
    selected <- cbind(selected, etichette);
    colnames(selected)[ncol(selected)] <- "Label";
    return(selected);
}

# Demo
if (interactive()) {
    # Assumptions:
    # - you have the NxM documents-terms matrix with N documents and M words
    # - you have the label in a separated file

    # Reading dataset and preparation to features selection
    dataset <- read.table("./Matrix.txt", header=T, sep=" ", dec=".");
    labels <- read.table("./Labels.txt");
    submatrix <- most_frequent(dataset, 50, labels);
    write.table(submatrix, "./SubMatrix.txt", sep=" ");
}
