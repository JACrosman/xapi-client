# xapi-client
A javascript xAPI client.

## Description
The project contains a comprehensive xAPI javascript client sdk. It was developed to allow applications using javascript the capability to communicate with an LRS. It is currently compatible with xAPI Spec `1.0.2` - `1.0.3`.

## Features
* Compatible with xAPI Spec `1.0.2` - `1.0.3`
* Uses version 4 uuids
* Client side validation of xAPI statement
* Queuing of failed requests from network connectivity
* Standalone script (No third party dependencies)
* Support for Document APIs
* Support for statement attachments using multipart/mixed content type

## Installation
1) Include javascript
```javascript
<script type="text/javascript" src="/lib/xapi.js"></script>
```
2) Set LRS Endpoint
```javascript
xapi.setLRS('http://www.rockinlrs.com');
```

All classes are stored in the xapi namespace.

## Table of Contents
* [**Description**](#description)
* [**Installation**](#installation)
* [**Script Usage**](#script-usage)
    * [Activity](#activity)
    * [Activity Profile](#activity-profile)
    * [Actor](#actor)
    * [Agent Profile](#agent-profile)
    * [Backlog](#backlog)
    * [Statement](#statement)
    * [Statement Query](#statement-query)
    * [Verb](#verb)

## Script Usage

Below are many of the classes and methods defined in xAPI client script, including details about usage and example code blocks.

### Activity

#### Create Activity

`xapi.Activity.create()`

Build a new activity for an xAPI statement from the provided data.

```javascript
// setup activity information
var activityData = {
    "id": id,
    "objectType": objectType,
    "definition": definition,
};

// then create a new activity with the data
var myActivity = new xapi.Activity.create(activityData);

// alternatively, create an activity piece by piece
var myActivity = new xapi.Activity.create(id, objectType, definition);
```

#### Get Activity Profile from Activity

`xapi.Activity.get()`

Retrieves the activity's profile and sends the data to a callback.

```javascript
// assume myActivity is a previously created activity object
myActivity.get(callbackFn);
```

### Activity Profile

#### Create Activity Profile

`xapi.ActivityProfile.create()`

Build a new activityProfile for an xAPI statement from the provided data.

```javascript
// setup profile information
var activityProfileData = {
    "activityId": activityId,
    "profileId": profileId,
    "since": email,
    "document": id
};
// then create a new activityProfile with the data
var myActivityProfile = new xapi.ActivityProfile.create(activityProfileData);

// alternatively, create an actor piece by piece.
var myActivityProfile = new xapi.ActivityProfile.create(activityId, profileId, since, document);
```

#### Get ActivityProfile

`xapi.ActivityProfile.get()`

Retrieve the specified profile document and feed it to the provided callback.

```javascript
// assume myActivityProfile is a previously created activity profile object
myActivityProfile.get(callbackFn);
```

#### Get All ActivityProfiles

`xapi.ActivityProfile.getAll()`

Retrieve all profile id entries for the the activity profile and feed it to the provided callback.

```javascript
// assume myActivityProfile is a previously created activity profile object
myActivityProfile.getAll(callbackFn);
```

#### Update ActivityProfile

`xapi.ActivityProfile.update()`

Modify a specified profile document and send resulting data to the provided callback.

```javascript
// assume myActivityProfile is a previously created activity profile object
myActivityProfile.update(callbackFn);
```

#### Create ActivityProfile Document

`xapi.ActivityProfile.post()`

Creates an activity profile document for the specified profile document. Send resulting data to the provided callback.

```javascript
// assume myActivityProfile is a previously created activity profile object
myActivityProfile.post(callbackFn);
```

#### Remove ActivityProfile Documents

`xapi.ActivityProfile.remove()`

Deletes the profile document for the specified activity profile. Send resulting data to the provided callback.

```javascript
// assume myActivityProfile is a previously created activity profile object
myActivityProfile.remove(callbackFn);
```


### Actor

Actors can be `Person`s or `Agent`s.

#### Create Actor

`xapi.Actor.create()`

Build a new actor for an xAPI statement.

```javascript
// setup actor information
var actorData = {
    "objectType": objectType,
    "name": name,
    "mbox": email,
    "openid": id,
    "account": account,
    "members": members
};
// then create a new actor with the data
var myActor = new xapi.Actor.create(actorData);

// alternatively, create an actor piece by piece.
var myActor = new xapi.Actor.create(objectType, name, mbox, openid, account, members);
```

#### Get Agent from Actor

`xapi.Actor.get()`

Retrieve the agent data from the given actor and feed it to the provided callback.

```javascript
// assume myActor is a previously created actor object (which is an Agent)
// get the Actor's agent's data sent to the callbackFn
myActor.get(callbackFn);
```

### Agent Profile

#### Create an AgentProfile

`xapi.AgentProfile.create()`

Build a new Agent Profile.

```javascript
// create an agent profile piece by piece
var myAgentProfile = new xapi.AgentProfile.create(agent, profileId, since, document);

// alternatively, build an agentProfileData object
var agentProfileData = {
    "agent": agent,
    "profileId": profileId,
    "since": since,
    "document": document
};
// then, create the profile by passing in the new object.
var myAgentProfile = new xapi.AgentProfile.create(agentProfileData);
```

#### Get Agent Profile

`xapi.AgentProfile.get()`

Retrieves a profile that matches the agentProfileData and sends the results to a callback function.

```javascript
// assume myAgentProfile is a previously created profile
myAgentProfile.get(callbackFn);
```

#### Get All Agent Profiles

`xapi.AgentProfile.getAll()`

Retrieves all agent profiles that match the agentProfileData and sends the results to a callback function.

```javascript
// assume myAgentProfile is a previously created profile
myAgentProfile.getAll(callbackFn);
```

#### Update Agent Profile

`xapi.AgentProfile.update()`

Modifies a profile that matches the agentProfileData and sends the results to a callback function.

```javascript
// assume myAgentProfile is a previously created profile
myAgentProfile.update(callbackFn);
```

#### Add Agents Profile

`xapi.AgentProfile.post()`

Adds the specified profile to the agent and sends the results to a callback function.

```javascript
// assume myAgentProfile is a previously created profile
myAgentProfile.post(callbackFn);
```

#### Remove Agents Profile

`xapi.AgentProfile.remove()`

Removes an agent's stored profile.

```javascript
// assume myAgentProfile is a previously created profile
myAgentProfile.remove(callbackFn);
```


### Backlog

Storage mechanism for holding and managing statements until able to send to the configured `LRS`.

#### Create Backlog

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
// assume myStatement is a previously created xAPI statement
myBacklog.add(myStatement);


// alternatively, send a batch array of statements to the backlog at once.
// assume manyStatements is a collection of previously created xAPI statements
myBacklog.add(manyStatements);
```

#### Get Backlog

`xapi.Backlog.get()`

Retrieves the array of xAPI statements in the backlog from localStorage and sessionStorage. Returns the array of backlog items.

```javascript
// assume myBacklog is a previously created backlog
var myBackloggedStatements = myBacklog.get();
```


#### Synchronize Backlog Storage

`xapi.Backlog.syncStorage()`

Sets the localStorage and sessionStorage caches contents to the provided collection of xAPI statements.

```javascript
// assume myBacklog is a previously created backlog
// assume manyStatements is a collection of previously created xAPI statements
myBacklog.syncStorage(manyStatements);
```

#### Empty Backlog Storage

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

### Document

#### Creating Documents

`xapi.Document.create()`

### Statement

Statements are the objects that hold all the collected xAPI data for a single activity/verb/actor combination.

#### Creating Statements

`xapi.Statement.create()`

Builds out a complete xAPI statement based on the provided data.

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

Saves the xAPI statment in the local `backlog` and synchronizes the local `backlog` in the browser's localStorage and sessionStorage.

```javascript
// assume myStatement is previously created xAPI statement
// store the statement
myStatement.store();
```

### Statement Query

An object for selecting statements from the configured `LRS`.

#### Create Statement Query

`xapi.StatementQuery()`

Builds out a statementQuery object for fetching xAPI statements.

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

Call get to retrieve the results of the query parameters set in the Statement Query Object (myQuery).

```javascript
// assume myQuery is a previously created query object
// get statement data sent to the callbackFn
myQuery.get(callbackFn);
```

#### Setup Live Query

`xapi.Statement.live()`

Sets up a live query.

```javascript
// assume myQuery is a previously created query object
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


### Verb

An Object for creating and managing predefined xAPI verbs.

#### Create Verb

`xapi.Verb.create()`

Builds a verb for use in an xAPI statement.

```javascript
//setup verb data
var verbData = {
    "id": id,
    "display": display
};

// then create the verb from the verbData object
var myVerb = xapi.Verb.create(verbData);

// alternatively, build a verb from individual peices
var myVerb = xapi.Verb.create(id, display);
```
