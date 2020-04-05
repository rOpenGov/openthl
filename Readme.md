
## openthl R package

R package for reading data from the THL open data API

[API documentation](https://yhteistyotilat.fi/wiki08/x/RoCkAQ)

## Examples

```{r}
library(openthl)

x <- getSubject("epirapo") # S3 classes "api_response" "thl_subject" 
datasets <- getDatasets(x)
cube <- getCube(datasets, index = 1L) # a very messy object for now
```

### S3 classes

These are preliminary and very much subject to change

**api_data_url**

list with elements:

- api: url to the api
- path: defines what to get from the api
- format: "json"

has a print method

**api_response**

list with elements:

- response (parsed json response: a list or a data frame)
- api_data_url: api_data_url Object
- status: status code of the response


