# Association Rule

library(arules)

bookbaskets <- read.transactions("bookdata.tsv.gz", format="single",
                  sep="\t",
                  cols=c("userid", "title"),
                  header=T,
                  rm.duplicates=T)

class(bookbaskets)

bookbaskets

colnames(bookbaskets)[1:5]

# Examine the distribution of transaction sizes (or basket sizes)
basketSizes <- size(bookbaskets)
summary(basketSizes)

# Most customers (at least half of them, in fact) only expressed interest in one book. 
# But someone has expressed interest in more than 10,000!

# Look at the basket size distribution, in 10% increments
quantile(basketSizes, probs=seq(0.1, 1, by=0.1))

quantile(basketSizes, probs=seq(0.99, 1))

library(ggplot2)

ggplot(data=data.frame(count=basketSizes)) + 
  geom_density(aes(x=count), binwidth=1) +
  scale_x_log10()

# Get relative frequency of each book in the transaction data
bookFreq <- itemFrequency(bookbaskets)
summary(bookFreq)

# Get the absolute count of book occurrences.
bookCount <- bookFreq/sum(bookFreq) * sum(basketSizes)
summary(bookCount)

orderedBooks <- sort(bookCount, decreasing = T)
orderedBooks[1:10]

orderedBooks[1]/dim(bookbaskets)[1]

#As we observed earlier, half of the customers in the data only expressed interest 
#in a single book. Since we want to find books that occur together in people’s 
#interest lists, we can’t make any direct use of people who haven’t shown 
#interest in multiple books. We want to restrict the dataset to customers who have 
#expressed interest in at least two books

bookbaskets_use <- bookbaskets[basketSizes > 1]
dim(bookbaskets_use)

# In order to mine rules, need to decide on a minimum support level and a 
# minimum threshold level. For this example, restrict the itemsets 
# that we’ll consider to those that are supported by at least 100 people. This 
# leads to a minimum support of 100/dim(bookbaskets_use)[1] = 100/40822. This 
# is about 0.002, or 0.2%. We’ll use a confidence threshold of 75%.

rules <- apriori(bookbaskets_use,
                 parameter =list(support = 0.002, confidence=0.75))

summary(rules)

#If the lift is near 1, then there’s a good chance that the pattern we observed 
#is occurring just by chance. The larger the lift, the more likely that the 
#pattern is “real.” In this case, all the discovered rules have a lift of at 
#least 40, so they’re likely to be real patterns in customer behavior.

# Rules can be evaluated using the function interestMeasure(). Coverage is the 
# support of the left side of the rule (X); it tells how often the rule would 
# be applied in the dataset. Fisher’s exact test is a significance test for whether 
# an observed pattern is real, or chance (the same thing lift measures;
# Fisher’s test is more formal). Fisher’s exact test returns the p-value, 
# or the probability that you would see the observed pattern by chance;

measures <- interestMeasure(rules, 
                            measure=c("coverage", "fishersExactTest"),
                            transactions=bookbaskets_use)
summary(measures)

# All the p-values from Fisher’s test are small, so it’s 
# likely that the rules reflect actual customer behavior patterns.

# The function inspect() pretty-prints the rules. 
inspect(head((sort(rules, by="confidence")), n=5))


#To find books that tend to co-occur with the novel The Lovely Bones. 
brules <- apriori(bookbaskets_use,
                  parameter =list(support = 0.001,
                                  confidence=0.6),
                  appearance=list(rhs=c("The Lovely Bones: A Novel"),
                                  default="lhs"))

summary(brules)

# Inspect the rules, sorted by confidence.
brulesConf <- sort(brules, by="confidence")
inspect(head(lhs(brulesConf), n=5))

# Use subset() to filter down only rules that don’t include Lucky.
brulesSub <- subset(brules, subset=!(lhs %in% 'Lucky : A Memoir'))
brulesConf <-sort(brulesSub, by='confidence')

inspect(head(lhs(brulesConf), n=5))
