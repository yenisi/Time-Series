#STEPS:

#1.Plot the entire data (Should contaion  proper comments
#2.
#3.
With monthly data we first decide whether to use ratio to trend or ratio to moving average.
If we select ratio to moving average we make use of the decompose function.
Remember the dataset is to be in the time series format.
From the output we get the adjusted sdeasonal indices
Next we deseasonalise the original series depending upon the model either by subtracting or dividing by the corresponding seasonal index values
Having obtained the deseasonalised data now we plot the series and fit an apt trend equation to the entire data
We obtain the deseasonalised detrended series ie the residual series by eliminating trend component from the deseasonalised series

In case of ratio to trend method, we first obtain the yearkly averages and fit an appropriate trend equation 
From the yearly trend equationm we come back to the monthly trend equation 
Special attention to be give to the origin and unit
Next we obtain the fitted monthly trend values and we detrend the original series
We obtain the average corresponding to each month over the years from the detrended series
These averages should give us the unadjusted seasonal indices from that we get the adjusted seasonal indices
Next part would be same as ratio to moving average