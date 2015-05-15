# xapi-client
An xAPI javascript client.


## Description


## Features


## Installation


## Script Usage


### Statement

Statements are the objects that hold all the collected xapi data for a single activity/verb/actor combination.

#### Creating Statements

`xapi.Statement.create()`

Builds out a complete xapi statement based on the provided data.

```javascript
    // build out a new statement with individual statement pieces
    var myStatement = xapi.State.create(activityId, agent, registration, stateId, since, document);

    // alternatively, build out an object containing all necessary data then call create with the aggregate object
    var statementData = {
        "activityId": activityId,
        "agent": agent,
        "registration": registration,
        "stateId": stateId,
        "since": since,
        "document": document
    };
    var myStatement = xapi.State.create(statementData);
```

#### Storing Statements

`xapi.Statement.store()`

Saves the xapi statment in the local `backlog` and syncronizes the local `backlog` in the browser's localStorage and sessionStorage.

```javascript
    // assume myStatement is previously created xapi statement
    // store the statement
    myStatement.store();
```

### Statement Query

An object for selecting statements from the configured `LRS`.

#### Create Statement Query

`xapi.StatementQuery()`

Builds out a statementQuery object for fetching xapi statements.

```javascript

    //setup query parameters
    var queryParams = {
        "limit": "1000"
    };

    // build out a new query with individual statement pieces
    var myQuery = xapi.StatementQuery(queryParams);
```

#### Query for Statements

`xapi.StatementQuery.get()`

Call get to retrieve a the results of the query parameters set in the the Statement Query Object (myQuery).

```javascript
    // assume mQuery is a previously created query object
    // get a statement(s) data sent to the callbackFn
    myQuery.get(callbackFn);
```

#### Setup Live Query

`xapi.Statement.live()`

Sets up a live query.

```javascript

    // assume mQuery is a previously created query object
    // setup live query options
    var options = {
        "update": updateFunction,
        "connect": connectFunction,
        "disconnect": disconnectFunction,
        "error": errorFunction,
        "data": dataFunction
    };
    myQuery.live(options);
```

### Document


#### Creating Documents


#### Creating Documents


### Agent


### Agent Profile


### Activity


### Backlog

Storage mechanism for holding and managing statements until able to send to the configured `LRS`.

#### Create a Backlog

`xapi.Backlog()`

Builds a new statement backlog.

```javascript
    // create a new, empty backlog with the given string as a name
    var myBacklog = xapi.Backlog('name');
```

#### Add to Backlog

`xapi.Backlog.add()`

Adds a statement or array of statements to the backlog and syncs with both the browser's LocalStorage and SessionStorage.

```javascript
    // assume myBacklog is a previously created backlog
    // assume myStatement is a previously created xapi statement
    myBacklog.add(myStatement);


    // alternativley, sent a batch array of statements to the backlog at once.
    // assume manyStatemens is a collection of previously created xapi statements
    myBacklog.add(manyStatemens);
```

#### Get Backlog

`xapi.Backlog.get()`

Retrieves the array of xapi statements in the backlog from localStorage and sessionStroage. Returns the array of backlog items.

```javascript
    // assume myBacklog is a previously created backlog
    var myBackloggedStatements = myBacklog.get();
```


#### SyncStorage

`xapi.Backlog.syncStorage()`

Sets the localStorage and sessionStorage caches contents to the provided collection of xapi statements.

```javascript
    // assume myBacklog is a previously created backlog
    // assume manyStatemens is a collection of previously created xapi statements
    myBacklog.syncStorage(manyStatemens);
```

#### Empty Backlog Stroage

`xapi.Backlog.empty()`

Clears out the backlog completely.

```javascript
    // assume myBacklog is a previously created backlog
    myBacklog.empty();
```

#### Send Statement from Backlog

`xapi.Backlog.send()`

Sends current statements in the backlog to the configured `LRS` endpoint.

```javascript
    // assume myBacklog is a previously created backlog
    myBacklog.send();
```
