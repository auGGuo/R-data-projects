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

# You can examine the distribution of transaction sizes (or basket sizes) with 
# the function size()
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

# get relative frequency of each book in the transaction data
bookFreq <- itemFrequency(bookbaskets)
summary(bookFreq)

# Get the absolute count of book occurrences.
bookCount <- bookFreq/sum(bookFreq) * sum(basketSizes)
summary(bookCount)

orderedBooks <- sort(bookCount, decreasing = T)
orderedBooks[1:10]

orderedBooks[1]/dim(bookbaskets)[1]

#As we observed earlier, half of the customers in the data only expressed interest 
#in a single book. Since you want to find books that occur together in people’s 
#interest lists, you can’t make any direct use of people who haven’t yet shown 
#interest in multiple books. You can restrict the dataset to customers who have 
#expressed interest in at least two books

bookbaskets_use <- bookbaskets[basketSizes > 1]
dim(bookbaskets_use)

# In order to mine rules, you need to decide on a minimum support level and a 
# minimum threshold level. For this example, let’s try restricting the itemsets 
# that we’ll consider to those that are supported by at least 100 people. This 
# leads to a minimum support of 100/dim(bookbaskets_use)[1] = 100/40822. This 
# is about 0.002, or 0.2%. We’ll use a confidence threshold of 75%.

rules <- apriori(bookbaskets_use,
                 parameter =list(support = 0.002, confidence=0.75))

summary(rules)

#If the lift is near 1, then there’s a good chance that the pattern you observed 
#is occurring just by chance. The larger the lift, the more likely that the 
#pattern is “real.” In this case, all the discovered rules have a lift of at 
#least 40, so they’re likely to be real patterns in customer behavior.

# rules can be evaluated using the function interestMeasure(). Coverage is the 
# support of the left side of the rule (X); it tells you how often the rule would 
# be applied in the dataset. Fisher’s exact test is a significance test for whether 
# an observed pattern is real, or chance (the same thing lift measures;
# Fisher’s test is more formal). Fisher’s exact test returns the p-value, 
# or the probability that you would see the observed pattern by chance;

measures <- interestMeasure(rules, 
                            measure=c("coverage", "fishersExactTest"),
                            transactions=bookbaskets_use)
summary(measures)

# The coverage of the discovered rules ranges from 0.002–0.007, equivalent to a range
# of about 100–250 people. All the p-values from Fisher’s test are small, so it’s 
# likely that the rules reflect actual customer behavior patterns.

# The function inspect() pretty-prints the rules. The function sort() allows you to
# sort the rules by a quality or interest measure, like confidence. To print the 
# five most confident rules in the dataset
inspect(head((sort(rules, by="confidence")), n=5))


#To find books that tend to co-occur with the novel The Lovely Bones. You can do this 
#by restricting which books appear on the right side of the rule, using the 
#appearance parameter.
brules <- apriori(bookbaskets_use,
                  parameter =list(support = 0.001,
                                  confidence=0.6),
                  appearance=list(rhs=c("The Lovely Bones: A Novel"),
                                  default="lhs"))

summary(brules)

# inspect the rules, sorted by confidence. Since they’ll all have the same right
# side, you can use the lhs() function to only look at the left sides.
brulesConf <- sort(brules, by="confidence")
inspect(head(lhs(brulesConf), n=5))

#Note that four of the five most confident rules include Lucky: A Memoir in the left side,
#which perhaps isn’t surprising, since Lucky was written by the author of The Lovely
#Bones. Suppose you want to find out about works by other authors that are interesting
#to people who showed interest in The Lovely Bones; you can use subset() to filter down
#to only rules that don’t include Lucky.
brulesSub <- subset(brules, subset=!(lhs %in% 'Lucky : A Memoir'))
brulesConf <-sort(brulesSub, by='confidence')

inspect(head(lhs(brulesConf), n=5))