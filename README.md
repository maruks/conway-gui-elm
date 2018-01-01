# cellular automaton UI WS client


## JSON commands

### start creates a new grid
```javascript
{ "start" : { "width" : 60, "height" : 40 }}
```

### next state
```javascript
{ "next" : 1 }
```

### server should return data in the following format
```javascript
{"type":"cells", "cells":[ {"color":1, "point":[1, 0]}, {"color":2, "point":[0, 0]} ]}
```

```javascript
{"type":"colors", "colors":[ {"color":"#aabbcc", "code":2}, {"color":"#1100ff", "code":1} ]}
```

```javascript
{"type":"error", "code":1}
```

## Build

```
elm-make Main.elm --output=elm.js
```

```
elm-live Main.elm --output=elm.js
```
