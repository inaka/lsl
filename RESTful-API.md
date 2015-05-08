## RESTful API
### Expected Workflows
#### First time player
1. Create a player ([``POST /players``](#post-players))
2. Start a session ([``POST /sessions``](#post-sessions)) to get a token. Tokens are short lived.
   You have to repeat this step each time the token is invalidated.

#### Human vs Human match
1. Check the list of players ([``GET /players``](#get-players))
2. Start a match against one ([``POST /matches``](#post-matches))
3. Check the match status to see if it's your turn ([``GET /matches/:match_id``](#get-matchesmatch_id))
4. When it's your turn, play ([``PATCH /matches/:match_id``](#patch-matchesmatch_id))
5. Repeat from _3_ until the response from _3_ or _4_ indicates the game is over

#### Human vs AI match
1. Check the list of AI players ([``GET /ai-players``](#get-ai-players))
2. Start a match against one ([``POST /matches``](#post-matches)).
3. Check the response from _2_, it's your turn now
4. Play ([``PATCH /matches/:match_id``](#patch-matchesmatch_id))
5. If you haven't won yet, check the match status ([``GET /matches/:match_id``](#get-matchesmatch_id))
6. If you haven't lost yet, repeat from _4_

#### Voluntarily invalidating a session
1. Delete the session ([``DELETE /sessions/:session_token``](#delete-sessionssession_token))

#### Delete an Account
1. Delete the player ([``DELETE /players/:player_id``](#delete-playersplayer_id))

#### Cancelling a Match
1. Delete the match ([``DELETE /matches/:match_id``](#delete-matchesmatch_id))

---

### Endpoints
#### ``POST /players``
To register players
##### Authentication
None
##### Parameters
```json
{ "name": [PLAYER NAME]
, "password": [PLAYER PASSWORD]
}
```
##### Responses
* **409 Conflict** if username already taken
* **400 Bad Request** with a descriptive message
* **200 OK** with
```json
{ "id": [PLAYER ID]
, "name": [PLAYER NAME]
}
```

#### ``POST /sessions``
To generate temporary tokens
##### Authentication
Basic Auth with username and password
##### Parameters
None
##### Responses
* **401 Unauthorized**
* **400 Bad Request** with a descriptive message
* **200 OK** with
```json
{ "token": [SESSION TOKEN]
, "secret": [SESSION SECRET]
}
```

#### ``DELETE /sessions/:session_token``
To invalidate temporary tokens
##### Authentication
Basic Auth with username and password or session token and secret
##### Parameters
None
##### Responses
* **401 Unauthorized**
* **400 Bad Request** with a descriptive message
* **204 No Content**

#### ``GET /players``
To retrieve the list of players
##### Authentication
Basic Auth with session token and secret
##### Parameters
None
##### Responses
* **401 Unauthorized**
* **400 Bad Request** with a descriptive message
* **200 OK** with
```json
[ {"id" : [PLAYER ID], "name": [PLAYER NAME]}
, {"id" : [PLAYER ID], "name": [PLAYER NAME]}
, …
]
```

#### ``DELETE /players/:player_id``
To unregister a player
##### Authentication
Basic Auth with username and password or session token and secret
##### Parameters
None
##### Responses
* **401 Unauthorized**
* **400 Bad Request** with a descriptive message
* **204 No Content**

#### ``POST /matches``
To create matches
##### Authentication
Basic Auth with session token and secret
##### Parameters
```json
{ "rival": [PLAYER ID]
, "rows": [BOARD SIZE]
}
```
##### Responses
* **401 Unauthorized**
* **400 Bad Request** with a descriptive message
* **200 OK** with
```json
{ "id": [MATCH ID]
, "rival": [PLAYER ID]
, "board": [[true], [false,true], [false,false,false], …] // true means crossed
, "current-player": [PLAYER ID]
, "status": "playing"
}
```

#### ``GET /matches``
To retrieve the list of matches for the player
##### Authentication
Basic Auth with session token and secret
##### Parameters
- **status** (Query String): either `all`, `won`, `lost` or `playing` (**default:** `playing`)

##### Responses
* **401 Unauthorized**
* **400 Bad Request** with a descriptive message
* **200 OK** with
```json
[ { "id": [MATCH ID]
  , "rival": [PLAYER ID]
  , "board": [[true], [false,true], [false,false,false], …]
  , "current-player": [PLAYER ID]
  , "status": "playing" | "won" | "lost"
  }
, { "id": [MATCH ID]
  , "rival": [PLAYER ID]
  , "board": [[true], [false,true], [false,false,false], …]
  , "current-player": [PLAYER ID]
  , "status": "playing" | "won" | "lost"
  }
, …
]
```

#### ``GET /matches/:match_id``
To retrieve a match
##### Authentication
Basic Auth with session token and secret
##### Parameters
None
##### Responses
* **404 Not Found**
* **401 Unauthorized**
* **400 Bad Request** with a descriptive message
* **200 OK** with
```json
{ "id": [MATCH ID]
, "rival": [PLAYER ID]
, "board": [[true], [false,true], [false,false,false], …]
, "current-player": [PLAYER ID]
, "status": "playing" | "won" | "lost"
}
```

#### ``PATCH /matches/:match_id``
To play
##### Authentication
Basic Auth with session token and secret
##### Parameters
```json
{ "row": [ROW NUMBER]
, "col": [COL NUMBER]
, "length": [NUMBER OF STICKS TO CROSS]
}
```
##### Responses
* **404 Not Found** if match is not accesible to the player
* **403 Forbidden** if it's not the player's turn
* **401 Unauthorized**
* **400 Bad Request** with a descriptive message
* **200 OK** with
```json
{ "id": [MATCH ID]
, "rival": [PLAYER ID]
, "board": [[true], [false,true], [false,false,false], …]
, "current-player": [PLAYER ID]
, "status": "playing" | "won" | "lost"
}
```

#### ``DELETE /matches/:match_id``
To invalidate temporary tokens
##### Authentication
Basic Auth with session token and secret
##### Parameters
None
##### Responses
* **404 Not Found** if match is not accesible to the player
* **401 Unauthorized**
* **400 Bad Request** with a descriptive message
* **204 No Content**

#### ``GET /ai-players``
To retrieve the list of AI players
##### Authentication
Basic Auth with session token and secret
##### Parameters
None
##### Responses
* **401 Unauthorized**
* **400 Bad Request** with a descriptive message
* **200 OK** with
```json
[ {"id": [AI ID], "name": [AI NAME]}
, {"id": [AI ID], "name": [AI NAME]}
]
```
