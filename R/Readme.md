## Development plans

the thlCube function now returns an object which includes

- url: the url of the cube (fact url), which is also the argument of thlCube
- dimension: A list of data frames which include the complete hierarchy of each
of the dimensions in the dataset (including also measures).

```
library(openthl)
url <- "https://sampo.thl.fi/pivot/prod/fi/toitu/ennakko3/fact_toitu_ennakko.json"
cube <- openthl::thlCube(url)
names(cube$dimensions)
attr(cube$dimensions[[1]], "root") # root of the first dimension
View(cube$dimensions[[1]]) # look at the first dimension
```

Methods can now be written which utilise the cube object to build queries which

- retrieve data
- add labels to the data

For example the following methods could be implemented:

- select() (builds a query which chooses dimensions/measures)
- filter() (adds a filter to the query)
- collect() (retrieves the data)

Some choices need to be made regarding how the user refers to the dimensions 
and measures, i.e. whether to use ID's or labels. 
It may make sense to use ID's when select():ing. 
A stage could be referred to by for example <dimension_id>.<stage_id>. 
This needs some consideration and exploration on what is most straight forward.


