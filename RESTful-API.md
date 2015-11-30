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

#### Cancelling a Match
1. Delete the match ([``DELETE /matches/:match_id``](#delete-matchesmatch_id))

---

Boot up the server and check the [API's Swagger Page](http://localhost:8585/api-docs)
