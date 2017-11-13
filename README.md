# "Game of Life" UI WS client


## JSON commands

### start creates a new grid
{ "start" : { "width" : 60, "height" : 40 }}

### next state
{ "next" : 1 }

### server should return data in this format
{ "alive" : [[2,3], [1,3]] }