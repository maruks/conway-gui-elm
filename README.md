# "Game of Life" UI WS client


## JSON commands

### start creates a new grid
```javascript
{ "start" : { "width" : 60, "height" : 40 }}
```

### next state
```javascript
{ "next" : 1 }
```

### server should return data in this format
```javascript
{ "alive" : [[2,3], [1,3]] }
```