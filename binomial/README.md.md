# workout03-binomial

### The Binomial Package

#### The binomial package contains a set of functions that allow the user to easily compute, visualize and summarize computations, especially binomial probabilities.

**Its main functions are:**
  * bin_choose(), which allows the user to compute the binomial coefficient using n and k as inputs.
* bin_probability(), which allows the user to compute the probability of a number of successes occurring over a certain number of trials.
* bin_distribution(), which allows the user to compute the distribution of probabilities, and returns in data frame form. This can then be plotted with the base plot() function.
* bin_cumulative(), which allows the user to compute both the distribution of probabilities, as well as the cumulative sum of those probabilites. This also returns in data frame form. This can then be plotted with the base plot() function.

**The summarization function is:**
  * bin_variable(), which takes trials and probability, and displays them in a neatly formatted output. It can also be used in conjunction with the summary() function to get a comprehensive list of various summarizing information, such as mean, mode, skewness, variance, and kurtosis.

*Note: the summary information, such as mean, mode, and so on, can also be called individually with bin_mean(), bin_mode(), and so on.*
  
  The purpose of this package is to have a user-friendly set of functions that allows for basic probabilities and summaries to be created.