## GET /book

#### Authentication



Clients must supply the following data


#### GET Parameters:

- searchField
     - **Values**: *title, author*
     - **Description**: Hejsearchfield

- searchStr
     - **Values**: *Kurt, Anders*
     - **Description**: Hejsearchstr

- offset
     - **Values**: *0, 10*
     - **Description**: Hejoffset

- limit
     - **Values**: *10, 20*
     - **Description**: Hejlimit


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null}]
```

- 

```javascript
[{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null}]
```

- 

```javascript
[{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null}]
```

- 

```javascript
[{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null}]
```

## GET /book/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *1*: Record number

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
null
```

- 

```javascript
{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null}
```

## GET /books

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null}]
```

- 

```javascript
[{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null}]
```

- 

```javascript
[{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null}]
```

- 

```javascript
[{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null},{"content":"innehåll","year":2004,"author":"jag","id":1,"title":"hej","user_id":null}]
```

