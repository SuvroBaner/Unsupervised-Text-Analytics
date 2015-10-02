########################## Text Analysis of Movie Tag Lines (R) ###################################

install.packages("tm")  # text mining and document management
install.packages("proxy")  # dissimilarity calculations by dist()
install.packages("stringr") # character manipulation with regular expressions
install.packages("grid")  # grid graphics utilities
install.packages("ggplot2")  # graphics
install.packages("latticeExtra")  # package used for text horizon plot
install.packages("wordcloud")  # provides utility for plotting non-overlapping text
install.packages("cluster") # cluster analysis

library(tm)
library(proxy)
library(stringr)
library(grid)
library(ggplot2)
library(latticeExtra)
library(wordcloud)
library(cluster)

source('R_utility_program_3.R')  # User-defined utilities for plotting.

# Creating the movies data frame for analysis.

movies = read.csv('movie_tagline_data_parsed.csv', stringsAsFactors = FALSE)
summary(movies)
dim(movies)

# Plot frequency of movies by year

ggplot.object = ggplot(data = movies, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(x = "Year of Release",
         y = "Number of Movies in the Database") +
            coord_fixed(ratio = 1/50)

ggplot.print.with.margins(ggplot.object.name = ggplot.object,
                          left.margin.pct = 10, right.margin.pct = 10,
                          top.margin.pct = 10, bottom.margin.pct = 10)
  
# As most of the movies are from 1974 to 2013, so let's work with only these movies.

# Creating an aggregate tagline_text collection for each year of interest

years.list = 1974:2013

document.collection = NULL # initialize

for (index.for.year in seq(along = years.list))  # 1, 2, 3, 4, ... 39, 40
{
  working.year.data.frame = subset(movies, subset = (year == years.list[index.for.year]))
  # for the 1st iteration it stores all the movie details for the year 1974 in a data frame (i.e. row and column)
  tagline_text = NULL
  
  for (index.for.movie in seq(along = working.year.data.frame$movie))  # 1, 2, 3, ... 163, 164 for the year 1974 (numnber of movies)
  {
    tagline_text = paste(tagline_text, working.year.data.frame$tagline[index.for.movie])
    # It contains all the taglines of all the movies for a given year.
  }
  
  # create plain text doxuments
  
  document = PlainTextDocument(x = tagline_text,
                               author = "Suvro",
                               description = paste("movie taglines for ",
                                                   as.character(years.list[index.for.year]), sep = ""),
                               id = paste("movies_", as.character(years.list[index.for.year]), sep = ""),
                               heading = "taglines",
                               origin = "IMDb",
                               language = "en_US",
                               localmetadata = list(year = years.list[index.for.year]))
  
  # Give each created document a unique name
  
  if (years.list[index.for.year] == 1974) Y1974 = document
  if (years.list[index.for.year] == 1975) Y1975 = document
  if (years.list[index.for.year] == 1976) Y1976 = document
  if (years.list[index.for.year] == 1977) Y1977 = document
  if (years.list[index.for.year] == 1978) Y1978 = document
  if (years.list[index.for.year] == 1979) Y1979 = document
  if (years.list[index.for.year] == 1980) Y1980 = document
  if (years.list[index.for.year] == 1981) Y1981 = document
  if (years.list[index.for.year] == 1982) Y1982 = document
  if (years.list[index.for.year] == 1983) Y1983 = document
  if (years.list[index.for.year] == 1984) Y1984 = document
  if (years.list[index.for.year] == 1985) Y1985 = document
  if (years.list[index.for.year] == 1986) Y1986 = document
  if (years.list[index.for.year] == 1987) Y1987 = document
  if (years.list[index.for.year] == 1988) Y1988 = document
  if (years.list[index.for.year] == 1989) Y1989 = document
  if (years.list[index.for.year] == 1990) Y1990 = document
  if (years.list[index.for.year] == 1991) Y1991 = document
  if (years.list[index.for.year] == 1992) Y1992 = document
  if (years.list[index.for.year] == 1993) Y1993 = document
  if (years.list[index.for.year] == 1994) Y1994 = document
  if (years.list[index.for.year] == 1995) Y1995 = document
  if (years.list[index.for.year] == 1996) Y1996 = document
  if (years.list[index.for.year] == 1997) Y1997 = document
  if (years.list[index.for.year] == 1998) Y1998 = document
  if (years.list[index.for.year] == 1999) Y1999 = document
  if (years.list[index.for.year] == 2000) Y2000 = document
  if (years.list[index.for.year] == 2001) Y2001 = document
  if (years.list[index.for.year] == 2002) Y2002 = document
  if (years.list[index.for.year] == 2003) Y2003 = document
  if (years.list[index.for.year] == 2004) Y2004 = document
  if (years.list[index.for.year] == 2005) Y2005 = document
  if (years.list[index.for.year] == 2006) Y2006 = document
  if (years.list[index.for.year] == 2007) Y2007 = document
  if (years.list[index.for.year] == 2008) Y2008 = document
  if (years.list[index.for.year] == 2009) Y2009 = document
  if (years.list[index.for.year] == 2010) Y2010 = document
  if (years.list[index.for.year] == 2011) Y2011 = document
  if (years.list[index.for.year] == 2012) Y2012 = document
  if (years.list[index.for.year] == 2013) Y2013 = document
  
} # end of for-loop for the selected years

document.collection = c(Y1974,Y1975,Y1976,Y1977,Y1978,Y1979,
                        Y1980,Y1981,Y1982,Y1983,Y1984,Y1985,Y1986,Y1987,Y1988,Y1989,
                        Y1990,Y1991,Y1992,Y1993,Y1994,Y1995,Y1996,Y1997,Y1998,Y1999,
                        Y2000,Y2001,Y2002,Y2003,Y2004,Y2005,Y2006,Y2007,Y2008,Y2009,
                        Y2010,Y2011,Y2012,Y2013)

# document.collection is a large corpus of plain text document by year . It is stored as a vector.

# Now let's do the data manipulation-

# We'll use the function tm_map() which applies transformation functions to corpora.

# Strip whitespace from documents in the collection
document.collection = tm_map(document.collection, stripWhitespace)

# Convert uppercase to lowercase in the document collection
document.collection = tm_map(document.collection, content_transformer(tolower))

# Remove numbers from the document collection
document.collection = tm_map(document.collection, removeNumbers)

# Remove punctuation from the document collection
document.collection = tm_map(document.collection, removePunctuation)

# Using a standard list, remove English stopwords from the document collection
document.collection = tm_map(document.collection, removeWords, stopwords("english"))

# There is more we could do in terms of data preparation like
# stemming, looking for contractions, pronoun possessives (they are part of NLP - Natural Language Processing)

# We take what is clearly a "bag of words" approach.
# Here, the workhorse technique will be TermDocumentMatrix()

# Let's create a terms-by-documents matrix across the document collection.

initial.movies.tdm = TermDocumentMatrix(document.collection)

# Note initial.movies.tdm is a TermDocument Matrix which also means that it is a sparse matrix
# Lets see what it is.
initial.movies.tdm

#<<TermDocumentMatrix (terms: 25646, documents: 40)>>
#  Non-/sparse entries: 88431/937409
#Sparsity           : 91%
#Maximal term length: 59
#Weighting          : term frequency (tf)

# So, it is 91 % sparse, which means they have values zero, which also means that those words (terms)
# are not present in the movie year (all the movies in a given year)
# We can use the inspect() to view it for some random terms and movie years.

inspect(initial.movies.tdm[c(2000, 3120, 5550, 9000, 13000), c(1, 10, 20, 30, 40)])

#Docs
#Terms            movies_1974 movies_1983 movies_1993 movies_2003 movies_2013
#besinningslˆst           0           0           0           0           0
#calvin                   0           0           0           0           1
#deity                    0           0           0           0           0
#gehrigs                  0           0           0           0           0
#lifeguard                0           0           0           0           1


# So, now let's remove the sparse terms from the matrix and report the most common terms.

examine.movies.tdm = removeSparseTerms(initial.movies.tdm, sparse = 0.25)
# Here, sparse = 0.25 is the maximal allowed sparsity in the range from 0 and 1.

examine.movies.tdm

#<<TermDocumentMatrix (terms: 300, documents: 40)>>
#  Non-/sparse entries: 10690/1310
#Sparsity           : 11%
#Maximal term length: 10
#Weighting          : term frequency (tf)

# Now, we can see that the sparsity has reduced to 11%.

inspect(examine.movies.tdm[1:5, c(1, 10, 20, 30, 40)])

#Docs
#Terms       movies_1974 movies_1983 movies_1993 movies_2003 movies_2013
#action              5           6           1           6           5
#adventure           2           9           5           9           7
#alive               2           0           0           3           7
#alone               1           1           1           4           6
#always              2           3           5          10          23

# The frequency of these terms are also called tokens.

# Few important functions which we use in Text Analytics

nDocs(initial.movies.tdm)  # Numnber of documents are 40
nTerms(initial.movies.tdm) # Number of terms are 25646

nDocs(examine.movies.tdm)  # Number of documents are 40
nTerms(examine.movies.tdm) # Number of terms are 300  (after the sparsity is removed)

top.words = Terms(examine.movies.tdm)
# it gives you the terms, it is a character vector. Another method is Docs(x) which gives document ids.

length(top.words)  # there are 300 words after removing the sparsity which we'll call top words.

top.words[1:5]
# [1] "action"    "adventure" "alive"     "alone"     "always"

print(top.words)

# Scaning these top words we see there are few words like 'cant' etc. which we also call as 
# word contractions which we might like to drop from further analysis.
# Let's now recognize them as stop words to be dropped from the document collection.

# Note 'document.collection' has already been cleaned, we are fine tuning it based on our recent findings.

more.stop.words = c("cant", "didnt", "doesnt", "dont", "goes", "inst", "hes", "shes", 
                    "thats", "theres", "theyre", "wont", "youll", "youre", "youve")

document.collection = tm_map(document.collection, removeWords, more.stop.words)

# Note, document.collection is a VCorpus with 40 documents in it. It is a plain text document.

# Now let's again create terms-by-documents matrix across the final document collection.

movies.tdm = TermDocumentMatrix(document.collection)

nDocs(movies.tdm)  # 40 documents
nTerms(movies.tdm) # 25,631 terms

# Now let's save the movie documents and document collection (corpus)

save("movies", "document.collection", "movies.tdm", file = "000_movies_data.Rdata")


# Now as we had done before let's remove the sparse terms from the matrix and report the most common terms.

examine.movies.tdm = removeSparseTerms(movies.tdm, sparse = 0.25)

nDocs(examine.movies.tdm)  # 40 documents
nTerms(examine.movies.tdm) # 286 terms

inspect(examine.movies.tdm[c(50, 100, 150, 200, 250), c(5, 15, 25, 35, 40)])

#Docs
#Terms      movies_1978 movies_1988 movies_1998 movies_2008 movies_2013
#days               3           3           3           7           3
#get                5          11          20          63          45
#live               2           1           4          30          15
#power              4           2           6          13           5
#thriller           1           1           1           5           2

top.words = Terms(examine.movies.tdm)
length(top.words)
# So, the result of this is a bag of 286 words.
print(top.words)


# Now let's try to find a trend (if any) in the movie taglines as seen through time.

# For this we'll use Multidimensional scaling.

# Let's create a dictionary of the top words from the corpus.

top.words.dictionary = c(as.character(top.words))
# Earlier the same would have been done using the Dictionary() but now it is removed from the tm library.
# So, we use as.character()

# Create terms-by-documents matrix 

top.words.movies.tdm = TermDocumentMatrix(document.collection, list(dictionary = top.words.dictionary))

#<<TermDocumentMatrix (terms: 286, documents: 40)>>
#  Non-/sparse entries: 10163/1277
#Sparsity           : 11%
#Maximal term length: 10
#Weighting          : term frequency (tf)

# Note: You can see that there are 286 terms and 40 documents and the sparsity is very low at 11 %.

inspect(top.words.movies.tdm[c(50, 100, 150, 200, 250), c(5, 15, 25, 35, 40)])

#Docs
#Terms      movies_1978 movies_1988 movies_1998 movies_2008 movies_2013
#days               3           3           3           7           3
#get                5          11          20          63          45
#live               2           1           4          30          15
#power              4           2           6          13           5
#thriller           1           1           1           5           2

# Note it's useful to transform pure words into tokens, and so we just converted them into a Terms-document-matrix.

# Let's proceede with the analysis if there is any trend.

# Dissimilarity Measures and Multidimensional scaling.

years.dissimilarity.matrix = dist(t(inspect(top.words.movies.tdm)), method = "cosine")

# years.dissimilarity.matrix is matrix which has all pairwise "cosine" distances b/w the movie years.
# The distance is calculated based on the frequency of the terms (from the taglines) each year has.
# Note: we did a matrix transpose to get a Documents-by-terms matrix to compute the matrix distances.

# It can also be seen as the distace or similarity matrix, so that we can cluster it accordingly.
# As a dissimilarity measure we are taking the cosine distances and
# we'll group together which is similar or least dissmilar or in other words
# they'll be in the plot together due to their similarity of taglines. They look like below-

#              movies_1974 movies_1975 movies_1976
# movies_1975  0.27227425                                                                                    
# movies_1976  0.24432302  0.22142683                                                                        
# movies_1977  0.22209392  0.24840276  0.27115855

# Based on the above dissimilarity measure let's do the multidimensional scaling
# cmdscale() is the Classical Multidimensional Scaling method.
# It is also known as principal coordinate analysis.

years.mds.solution = cmdscale(years.dissimilarity.matrix, k = 2, eig = TRUE)

# Here, k = 2, represents the maximum dimension of the space which the data are to be represented in.
# eig = TRUE indicates the eigenvlues to be returned i.e. a special set of scalar solution.

# Multidimensional scaling takes a set of dissimilarities and returns a set of points such that the distances
# between the points are approximately equal to the dissimilarities.
# As output you get the following.

names(years.mds.solution)
# [1] "points" "eig"    "x"      "ac"     "GOF"

# points : a matrix with up to k columns whose rows give the coordinates of the points chosen to represent the dissimilarities.
# Here, k = 2, and it is very similar to the loading vectors used in PCA, which shows the dimension where the data variability is maximum.
# Here, the rows contain the movie years and their coordinates are the columns in 2 dimensions.

# eig : the n eigenvalues computed during the scaling process.Here there are 40 eigen values for 40 movies years.

x = years.mds.solution$points[,1]  # setting the x-coordinates
y = -years.mds.solution$points[,2]  # setting the y-coordinates, rotated to be consistent with the biplot.

w = c("1974", "1975", "1976", "1977", "1978", "1979",
      "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
      "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
      "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
      "2010", "2011", "2012", "2013")

plot(x, y, type = "n", xlim = c(-0.075, 0.075), ylim = c(-0.075, 0.075),
     xlab = "First Dimension", ylab = "Second Dimension")  # "n" is for no plotting.

lay = wordlayout(x, y, w, xlim = c(-0.075, 0.075), ylim = c(-0.075, 0.075))
# wordlayout() is a method in the "wordcloud" package, which finds text plot layout coordinates
# such that no text overlaps. Here "w" is the text to plot.

text(lay[, 1] + .5 * lay[, 3], lay[, 2] + .5 * lay[, 4], w)  # this sysntax you get it from the R-help "wordlayout"

# Clearly there is a time trend in the movie taglines. 
# Here years closest in time are closest to one another in the text measurement space.


# Now let's do the classification of words into groups for further analysis.

# We'll use hierarchical clustering as we don't have to prespecify the number of clusters.
# The clustering method I'll use here is called agnes() which stands for Agglomerative Nesting.
# It is part of the package called "cluster",
# The other implementation of hierarchical clustering is hclust() which is part of the default installation of R.

# Both needs a dissimilarity matrix as an input (at least in this context)

words.distance.object = dist(x = as.matrix(top.words.movies.tdm), method = "euclidean")

top.words.hierarchical.clustering = agnes(words.distance.object,
                                          diss = TRUE,
                                          metric = "euclidean",
                                          stand = FALSE,
                                          method = "ward")

# Here "ward" is the linkage method which is used to compute the inter-cluster dissimilarity
# which is slightly different from the dissimilarity measure b/w the observations for which we had used the Euclidean distance.
plot(top.words.hierarchical.clustering)

hc.complete = hclust(words.distance.object, method = "complete")
plot(hc.complete)

# Both the plots (dendrograms) from the agnes and hclust() using different linkage methods like
# "complete" and "ward" suggets that 5 clusters may work. Look at the branches of the tree to determine that.
# Technically horizontally cut the dendrogram at the height and see that you get approximately 5 clusters.

# ward() : the criterion for choosing the pair of clusters to merge at each step is based on the optimal value of an objective function.
# The objective function is solved to minimize the intercluster variance.

# In the following section we will use a clustering method called Partitioning Around Medoids (pam)
# which is a robust version of k-means clustering. We'll discuss it separately.



number.of.clusters.test = NULL

for(number.of.clusters in 2:20)
{
  try.words.clustering = pam(words.distance.object,
                             diss = TRUE,
                             metric = "euclidean",
                             stand = FALSE,
                             k = number.of.clusters)
  
  number.of.clusters.test = rbind(number.of.clusters.test,
                                  data.frame(number.of.clusters,
                                             ave.sil.width = try.words.clustering$silinfo$avg.width))
  
  cat("\n\n", "Number of Clusters: ", number.of.clusters,
      "Average silhouette width: ", try.words.clustering$silinfo$avg.width,
      "\nKey identified concepts: ", try.words.clustering$medoids,
      "\nCluster average silhouette widths: ")
  print(try.words.clustering$silinfo$clus.avg.widths)
} # end of for loop for numnber-of-clusters test

print(number.of.clusters.test)

#number.of.clusters ave.sil.width
#1                   2    0.48264222
#2                   3    0.43632421
#3                   4    0.28948091
#4                   5    0.29979462
#5                   6    0.23987783

# Here the maximum ave.sil.width is with cluster 2 but it might not be enough.
# Look at from cluster 4 to 5 to 6. Cluster 5 has the maximum width among these three.
# Also looking at each cluster silhouette width from the earlier output,
# choosing k = 5 will not be a bad idea, although it is not very evident.

# Now lets run the PAM clustering model again with value of k = 5.

top.words.clustering = pam(words.distance.object,
                           diss = TRUE,
                           metric = "euclidean",
                           stand = FALSE,
                           k = 5)

names(top.words.clustering)
#[1] "medoids"    "id.med"     "clustering" "objective"  "isolation"  "clusinfo"   "silinfo"    "diss"      
#[9] "call" 

plot(top.words.clustering)

# Let's now review the clustering results-

print(summary(top.words.clustering)) # you get a host of information.

# The medoid identified through the clustering process is an object at the center of the cluster.
# It is used to define the cluster, so here we identify their names.

cat("\n Key words identified by cluster analysis: \n")

key.word.set = top.words.clustering$medoids
print(key.word.set)
# [1] "head"  "dark"  "truth" "life"  "never"

# Convert the distance object to an actual distance matrix for doing word searches directly on the matrix calculations.

words.distance.matrix = as.matrix(words.distance.object)
dim(words.distance.matrix)  # 286 x 286

# So, we see it is square matrix.

words.distance.matrix[1:3,1:3]

#            action   adventure    alive
#action     0.00000  37.04052 24.57641
#adventure 37.04052   0.00000 43.86342
#alive     24.57641  43.86342  0.00000

# So, this distance matrix captures distances b/w the terms based on the number of times they occured in a given year.

# Now using the Partition Around Medoids (PAM) clustering technique we have found out 5 best clusters.
# In each each cluster we have also figured out the medoids for which we also know the name of the terms.

# Now for each medoid, try to locate the closest words from the distance matrix.

for(index.for.key.word in seq(along = key.word.set))
{
  # identify the column for the key word in the word.distance.matrix, say the 1st one is 'head'
  # it returns a list vector of the pairwise euclidean distances b/w 'head' and all other terms.
  # also note that the distance of 'head' with itself will be 0
  key.word.column = words.distance.matrix[, c(key.word.set[index.for.key.word])]
  
  # sort the key word column by distance.
  sorted.key.word.column = sort(key.word.column)
  # as the first word will be this word itself, e.g 'head', so let's choose the 2nd through the 6th word
  print(sorted.key.word.column[2:6])
  cat("\n\n")
  
  if(index.for.key.word == 1)
    head.word.set = names(sorted.key.word.column[1:3])
    
  if(index.for.key.word == 2)
    dark.word.set = names(sorted.key.word.column[1:3])
  
  if(index.for.key.word == 3)
    truth.word.set = names(sorted.key.word.column[1:3])
  
  if(index.for.key.word == 4)
    life.word.set = names(sorted.key.word.column[1:3])
  
  if(index.for.key.word == 5)
    never.word.set = names(sorted.key.word.column[1:3])
    
}

# Now turn the word sets into dictionaries for analysis

head.words.dictionary = c(as.character(head.word.set))
dark.words.dictionary = c(as.character(dark.word.set))
truth.words.dictionary = c(as.character(truth.word.set))
life.words.dictionary = c(as.character(life.word.set))
never.words.dictionary = c(as.character(never.word.set))


year = 1974:2013

total.words = integer(length(year))

head.words = integer(length(year)) # 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
dark.words = integer(length(year))
truth.words = integer(length(year))
life.words = integer(length(year))
never.words = integer(length(year))

for (index.for.document in seq(along = year))
{
  head.words[index.for.document] = sum(termFreq(document.collection[[index.for.document]],
                                                control = list(dictionary = head.words.dictionary)))
  # At first iteration head.words[1] will have the total number of terms among "head"  "loved" "boys"
  # for the year 1974. Remember document.collection is a Vcorpus of plain text documents and 
  # each year's documents are there as a list. It's syntatctical to give document.collection[[1]]
  
  dark.words[index.for.document] = sum(termFreq(document.collection[[index.for.document]],
                                                control = list(dictionary = dark.words.dictionary)))
  
  truth.words[index.for.document] = sum(termFreq(document.collection[[index.for.document]],
                                                 control = list(dictionary = truth.words.dictionary)))
  
  life.words[index.for.document] = sum(termFreq(document.collection[[index.for.document]],
                                                control = list(dictionary = life.words.dictionary)))
  
  never.words[index.for.document] = sum(termFreq(document.collection[[index.for.document]],
                                                 control = list(dictionary = never.words.dictionary)))
  
  total.words[index.for.document] = length(movies.tdm[, index.for.document][["i"]])  # total words in a year
}


# Now let's gather the results in a data frame.

movie.analytics.data.frame = data.frame(year,
                                        total.words,
                                        head.words,
                                        dark.words,
                                        truth.words,
                                        life.words,
                                        never.words)

# Compute text measures as percentages of words in each set.

movie.analytics.data.frame$HEAD = 100 * (movie.analytics.data.frame$head.words / 
                                           movie.analytics.data.frame$total.words)

HEAD = scale(movie.analytics.data.frame$HEAD)
HEAD.ts = ts(HEAD, start = c(1974, 1), end = c(2013, 1), frequency = 1)


print(movie.analytics.data.frame)
movie.analytics.data.frame$DARK = 100 * (movie.analytics.data.frame$dark.words / 
                                           movie.analytics.data.frame$total.words)

DARK = scale(movie.analytics.data.frame$DARK)
DARK.ts = ts(DARK, start = c(1974, 1), end = c(2013, 1), frequency = 1)


movie.analytics.data.frame$TRUTH = 100 * (movie.analytics.data.frame$truth.words / 
                                           movie.analytics.data.frame$total.words)

TRUTH = scale(movie.analytics.data.frame$TRUTH)
TRUTH.ts = ts(TRUTH, start = c(1974, 1), end = c(2013, 1), frequency = 1)

movie.analytics.data.frame$LIFE = 100 * (movie.analytics.data.frame$life.words / 
                                           movie.analytics.data.frame$total.words)

LIFE = scale(movie.analytics.data.frame$LIFE)
LIFE.ts = ts(LIFE, start = c(1974, 1), end = c(2013, 1), frequency = 1)

movie.analytics.data.frame$NEVER = 100 * (movie.analytics.data.frame$never.words / 
                                           movie.analytics.data.frame$total.words)

NEVER = scale(movie.analytics.data.frame$NEVER)
NEVER.ts = ts(NEVER, start = c(1974, 1), end = c(2013, 1), frequency = 1)

### Data frame of standardized text measures

text.measures.data.frame = data.frame(HEAD, DARK, TRUTH, LIFE, NEVER)  # scaled values
rownames(text.measures.data.frame) = 1974:2013

# Now let's plot it in two dimensions using the PCA

principal.components.solution = princomp(text.measures.data.frame, cor = TRUE)

print(summary(principal.components.solution))

names(principal.components.solution)

#"sdev"     "loadings" "center"   "scale"    "n.obs"    "scores"   "call" 

principal.components.solution$loadings

principal.components.solution$scores


# Note: The first two principal components explain about 90 % of the variance in the data, which is great.

# Now let's plot it using biplot, rendering of text measures and documents by year.

biplot(principal.components.solution, xlab = "First Principal Component",
       ylab = "Second Principal Component", col = c("black", "darkblue"), las = 1)

# Multiple time series object for text measures

text.measures.mts = cbind(HEAD.ts, DARK.ts, TRUTH.ts, LIFE.ts, NEVER.ts)
colnames(text.measures.mts) = c("HEAD", "DARK", "TRUTH", "LIFE", "NEVER")

# Text horizons for 40 years of movies

print(horizonplot(text.measures.mts, colorkey = TRUE,
                  layout = c(1, 5), strip.left = FALSE, horizonscale = 1,
                  origin = 0,
                  ylab = list(rev(colnames(text.measures.mts)), rot = 0, cex = 0.7)) +
                  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white")))

# The result in a nutshell-

# Our analysis of movie taglines from The Internet Movies Database (IMDb) suggests that movies from the 
# turn of the century onward are decidedly different from movies prior to the turn on the century.
# Text measures suggest a shift in the words used to describe movies.
# In particular, there seems to be a shift toward true-to-life stories. This could be a reality TV effect worthy of further study.
# These patterns can also be seen through the biplot and horizon plot.