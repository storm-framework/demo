# Building an app with STORM

![STORM](/figs/logo.png)

<hr>

# Installation and Use

<hr>

<br>

This tutorial/demo that shows how to build web-apps with STORM.
Like STORM, the demo focuses only on the web-server *back-end*. 
You can use your favorite front-end framework to write the *client*. 
For examples, look at 
[voltron](https://github.com/storm-framework/voltron) and 
[disco](https://github.com/storm-framework/disco), 
two complete applications we built with STORM and [vue.js](https://vuejs.org/).

<br>
<hr>
<br>

## Prerequisites

<br>

STORM has two dependencies: the Haskell build tool `stack` and the SMT solver `z3`.

1. [stack v2.5.1](https://docs.haskellstack.org/en/stable/README/) 
  Install using [these instructions](https://docs.haskellstack.org/en/stable/README/#how-to-install) 

2. [z3 v4.8.8](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.8)
  Install by placing a copy of the precompiled `z3` binary in your `$PATH`.
  You can ignore the shared libraries and bindings for Java and Python.

Additionally this demo will use [`curl`](https://curl.se/) to test our app as we go.

<br>
<hr>
<br>

## Install

<br>

The easiest way to get started is to download the starter *template* app

```sh
$ git clone git@github.com:storm-framework/demo.git storm-app
$ cd storm-app
$ git checkout v0
```

This tutorial is split into multiple pieces or versions which can be found 
in the the cloned repo as branches `v0`, `v1`, `v2`, `v3`, `v4`, and `v5`.
You can skip ahead and look at the progression via 

```sh
$ git diff v0..v1
```


<br>
<hr>
<br>

## Architecture

<br>

A STORM app has the following essential components.

| **File**                           | **Description**                            |
|:-----------------------------------|:-------------------------------------------|
| [`Models.storm`](src/Model.storm)  | Model-policy file                          |
| [`Routes.hs`](src/Routes.hs)       | Route-controller mapping                   |
| [`Types.hs`](src/Types.hs)         | Global type definitions                    |
| [`Config.hs`](src/Config.hs)       | Configuration options (e.g. env variables) |
| [`Controllers/`](src/Controllers/) | Controller implementations                 |

<br>
<hr>
<br>

## Build 

<br>

You can build the web-app by typing 

```sh
$ make build
``` 

This will take a *long while* to download all the dependencies, 
including building the LiquidHaskell verifier. Grab some coffee,
like some tweets, brush your teeth, or go for a jog. Fortunately, 
you only need to do this once; subsequent `make` should be quick.

<br>
<hr>
<br>

## Version 0: Kick the Tires

<br>

Lets kick the tires to make sure stuff is working.

<br>

### Step 1. Fire up the server

<br>


```sh
$ make
``` 

You should see the following at your terminal

```sh
Starting server at: host = "127.0.0.1", port = 3000
```

<br>

### Step 2. Test
<br>

In a *separate* terminal test your server with `curl`

```sh
$ curl http://localhost:3000/api/ping
"pong"
```

<br>
<hr>
<br>

## Version 1: Routes and Controllers

<br>

(You can "cheat" by switching to the branch `v1` in the repo.)

Next, lets add our first route and its controller.

<br>

### Step 1: Add a Route
<br>

Edit `src/Routes.hs` so that the definition of `routes` looks like

```haskell
routes :: [Route]
routes =
  [ post "/api/signin"    signIn
  , post "/api/signout"   signOut
  , get  "/api/ping"      pong
  , get  "/api/list/:uid" list     -- add this line
  ]
```

<br>

### Step 2: Implement a Controller
<br>

Edit `src/Controllers/List.hs` to add the following definition that simply 
responds with a string repeating the `UserId` and the current time.

```haskell
{-@ list :: UserId -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
list :: UserId -> Controller ()
list uid = do
  time   <- getTime
  let msg = "OK: hello " <> tShow uid <> " at " <> time :: T.Text
  respondJSON status200 msg 

getTime :: Controller T.Text
getTime = tShow <$> liftTIO currentTime
```

The text inside `{-@ ... @-}` is a [refinement type signature][blog] 
that tells the verifier that `list` is a *top-level* controller. 
For more details see [the paper].

<br>

### Step 3: Test 

<br>

Run `scripts/test_1.sh` to test your new route and controller!

```sh
$ ./scripts/test_1.sh

list ... returns current time UTC
"OK: hello ... {unSqlBackendKey = 1} at 2021-07-13 22:53:25.51654 UTC"
list ... returns current time UTC
"OK: hello ... {unSqlBackendKey = 2} at 2021-07-13 22:53:25.5285 UTC"
list ... returns current time UTC
"OK: hello ... {unSqlBackendKey = 3} at 2021-07-13 22:53:25.537898 UTC"
```

<br>
<hr>
<br>

## Version 2: Adding Authentication

<br>

Lets look at the model-policy file `src/Models.storm` 

The `User` table defines the set of users of the app.

```
User
  emailAddress  Text
  password      ByteString
  firstName     Text
  lastName      Text
  UniqueEmailAddress emailAddress
```

<br>

### Step 1: Require Authentication

<br>

Lets modify the `list` controller (in `src/Controllers/List.hs`) 
to use `requireAuthUser` which succeeds only when the request is 
from a logged in user. 

```haskell
{-@ list :: UserId -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
list :: UserId -> Controller ()
list uid = do
  requireAuthUser
  time   <- getTime
  user   <- selectFirstOr notFoundJSON (userId' ==. uid)
  userNG <- extractUserNG user
  let msg = "OK: " <> tShow userNG <> " at " <> time
  respondJSON status200 msg 
```

Additionally, notice that 

1. We use the query `selectFirstOr` with the query `(userId' ==. uid)` 
   to find the first record in the `User` table whose primary key equals 
   `uid` -- i.e. to find the record for `uid` if it exists or else, respond 
   with `Not Found`.

2. We use `extractUserNG`, which internally, uses `project` to pull out 
   the values of individual field of the `user` record to build the pure 
   value `userNG` sent back to the client.


<br>

### Step 2: Test 

<br>

Run `make` to build and run the server in one terminal. 

In another test your updated controller with `scripts/test_2.sh`.

```sh
add users
... INFO: addUser: CreateUser {crUserEmail = "alice@st.orm" ...} 
... INFO: addUser: CreateUser {crUserEmail = "bob@st.orm" ...}
... INFO: addUser: CreateUser {crUserEmail = "carlos@st.orm" ...} 

list ... returns nothing

sign-in first!
{"firstName":"Alice","lastName":"Waters"}
list ... now succeeds
"OK: UserNG {userFirstName = \"Alice\", userLastName = \"Waters\"} ..."
list ... now succeeds
"OK: UserNG {userFirstName = \"Robert\", userLastName = \"Barron\"} ..."
list ... returns {} as no such user
{}
```

Note that we don't get any response until we're signed in.

<br>
<hr>
<br>

## Version 3: Adding Items

<br>

Next, lets start populating our users' lists with some items!

<br>

### Step 1: Add a Database Table
<br>

First, lets make an `Item` table by editing `src/Models.storm` to include

```
Item
  owner         UserId
  description   Text
  level         String
```

Similarly, we want a JSON-compatible pure representation of `Item` so edit `src/Types.hs` to

```haskell
data ItemData = ItemData
  { itemDescription :: Text
  , itemLevel       :: String
  }
  deriving (Show, Generic)

mkItemData :: Text -> String -> ItemData
mkItemData descr levl = ItemData (strip descr) (sStrip levl)

instance FromJSON ItemData where
  parseJSON = genericParseJSON (stripPrefix "item")

instance ToJSON ItemData where
  toEncoding = genericToEncoding (stripPrefix "item")
```

<br>

### Step 2: Implement the API to Add Items

<br>

Next, lets make a new API endpoint to insert items for a user

Edit `src/Routes.hs` to include

```haskell
routes :: [Route]
routes =
  [ post "/api/signin"    signIn
  , post "/api/signout"   signOut
  , get  "/api/ping"      pong
  , get  "/api/list/:uid" list
  , get  "/api/add"       add
  ]
```

Edit `src/Controllers/List.hs` to include the controller

```haskell
{-@ add :: TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
add :: Controller ()
add = do
  owner   <- requireAuthUser
  ownerId <- project userId' owner
  ownerEm <- project userEmailAddress' owner
  items   <- decodeBody
  items'  <- mapT (\ItemData {..} -> 
              insert (mkItem ownerId itemDescription itemLevel)
             ) items
  let msg = "OK: add " <> tShow (length items') <> " items " <> ownerEm
  respondJSON status200 msg
```

Note how the controller 

1. Finds the `ownerId` and `ownerEmail` using `requireAuthUser` and `project`
2. Extracts the JSON payload via `decodeBody`
3. Uses `mkItem` to create the `Item` records and
4. Uses `insert` to add the records to the DB.

<br>

### Step 3: Modify `list` Return Items

<br>

Next, lets update `list` to return the items of the given `UserId` (instead of just 
responding with their names...). Edit the `src/Controllers/List.hs` so that

```haskell
list userId = do
  items  <- selectList (itemOwner' ==. userId)
  itemDs <- mapT (\i -> 
              ItemData `fmap` project itemDescription' i
                       <*>    project itemLevel' i
              ) items
  respondJSON status200 itemDs
```

Now, notice that first `selectList` is invoked with 
a query asking for all the `Item`s whose `owner` is 
`userId`. Next, we build a response object by `project`ing 
the `description` and `level` values of each record in `items` 
to build an array of JSON objects that is sent back to the client.

### Step 4: Test

The file `scripts/alice_json` has a few items for `alice`:

```sh
$ more scripts/items_alice.json
[ {"description":"katla","level":"public"},
  {"description":"counterpart","level":"public"},
  {"description":"marley and me","level":"private"},
  {"description":"daniel tiger","level":"follower"},
  {"description":"lupin","level":"follower"}
]
```

Lets test the app by adding and querying the server for those items

```sh
$ ./scripts/test_3.sh

********* add users
...

******** sign-in alice!
{"firstName":"Alice","lastName":"Waters"}
******** add items for alice
"OK: add 5 items for alice@st.orm"
******* logout

******** sign-in alice!
{"firstName":"Alice","lastName":"Waters"}
******** list items for alice ...
[{"description":"katla","level":"public"},
 {"description":"counterpart","level":"public"},
 {"description":"marley and me","level":"private"},
 {"description":"daniel tiger","level":"follower"},
 {"description":"lupin","level":"follower"}]
******** logout

******** sign-in bob!
{"firstName":"Robert","lastName":"Barron"}
******** list items for alice ...
[{"description":"katla","level":"public"},
 {"description":"counterpart","level":"public"},
 {"description":"marley and me","level":"private"},
 {"description":"daniel tiger","level":"follower"},
 {"description":"lupin","level":"follower"}]
******** logout
```

<br>
<hr>
<br>

## Version 4: Restrict to Public Items

<br>

Oops, we hit a bit of a snag -- `bob` (or anyone for that matter!) 
can see *all* of `alice`'s items -- even the ones marked `"private"`! 

<br>

### Step 1: Specify a Security Policy

<br>

Lets specify that only items marked `"public"` should be visible 
to users _other_ than the owner, who should, of course, be able 
to see all their own items. 

Edit `src/Models.storm` to *include* two predicates

1. `IsOwner item viewer` which is true when `viewer` owns `item`, 
2. `IsPublic item` which is true when the `item` is marked `"public"`.

```
policy IsOwner = \item viewer ->
  itemOwner item == entityKey viewer

policy IsPublic = \item ->
  itemLevel item == "public"

policy OwnPublic = \item viewer -> 
  IsOwner item viewer || IsPublic item
```

and further add a `read` clause to restrict `description` 
the owner-unless-marked-public

```
Item
  Item
  owner         UserId
  description   Text
  level         String
  read   [ description ] @OwnPublic 
```

Now when you run `make`, compilation fails with a type error!

```sh
**** LIQUID: UNSAFE **************************************************
../storm-demo/src/Controllers/List.hs:43:40: error:
    Liquid Type Mismatch
    ...
   |
43 |                ItemData `fmap` project itemDescription' i
   |                                        ^^^^^^^^^^^^^^^^
```

STORM is unimpressed as `selectList` yields all of `userId`'s items
but the *viewer* can be someone else who is not allowed (per `OwnPublic`) 
to view those `description`s!

<br>

### Step 2. Fix the Controller 

<br>

Lets fix the controller by checking if the viewer is `userId` 
and otherwise, by conjoining the query with a clause that the 
`Item`'s level be `"public"`. Edit `src/Controllers/List.hs` 
so that `list` looks like

```haskell
{-@ list :: UserId -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
list :: UserId -> Controller ()
list userId = do
  viewId  <- project userId' =<< requireAuthUser
  let pub  = if userId == viewId 
               then trueF 
               else itemLevel' ==. "public"
  items   <- selectList (itemOwner' ==. userId &&: pub)
  itemDs  <- mapT (\i -> ItemData `fmap` project itemDescription' i
                                  <*>    project itemLevel'       i
                  ) items
  respondJSON status200 itemDs
```

<br>

### Step 3. Test

<br>

STORM gladly verifies that the fix controller is policy compliant!
Lets test our server to make sure that only public items are shown 
to users other than `alice`

```sh 
$ scripts/test_3.sh
********* add users
...

******** sign-in alice!
{"firstName":"Alice","lastName":"Waters"}
******** add items for alice
"OK: add 5 items for alice@st.orm"
******* logout

******** sign-in alice!
{"firstName":"Alice","lastName":"Waters"}
******** list items for alice ...
[{"description":"katla","level":"public"},
 {"description":"counterpart","level":"public"},
 {"description":"marley and me","level":"private"},
 {"description":"daniel tiger","level":"follower"},
 {"description":"lupin","level":"follower"}]
******** logout

******** sign-in bob!
{"firstName":"Robert","lastName":"Barron"}
******** list items for alice ...
[{"description":"katla","level":"public"},
 {"description":"counterpart","level":"public"}]
******** logout
```

Note that `bob` only gets to see the subset of `public` items.

<br>
<hr>
<br>

## Version 5 : Restrict to Followers

<br>

As a last step, lets add a feature where a user can 
*share* some of their items with *followers*. 

<br>

### Step 1. Add Followers to the Model

<br>

Add the below definitions to represent the followers relationship 
in `src/Models.storm`.

```
predicate follows :: UserId -> UserId -> Bool

Follower
  subscriber UserId
  publisher  UserId
  status     String

  assert     {status == "accepted" => follows subscriber publisher}
```

The `Follower` table records the actual relationship. 
The `predicate follows` reflects the relationship at 
the logical "policy" level, using the `assert` clause
which says that *each* tuple in the `Follower` table 
*bears witness* to the fact that its subscriber
field follows its publisher field, *when* the `status` 
field has the value `"accepted"`. (This lets one user
*initiate* the relationship, which is only cemented when
the other user *accepts*.)

<br>

### Step 2. Modify Policy to Allow Followers Access

<br>

Next, lets update the policies for `Item` to allow the 
owner's followers to read the `description`. 
Edit `src/Models.storm` to define

```
policy IsFollower = \item viewer ->
  itemLevel item == "follower" && 
  follows (entityKey viewer) (itemOwner item)

policy OwnFollowPub = \item viewer -> 
  IsOwner    item viewer ||
  IsFollower item viewer || 
  IsPublic   item
```

and then edit the `read` clause with the new `OwnFollowPub` policy

```
Item
  owner       UserId
  description Text
  level       String

  read [ description ] @OwnFollowPub
```

<br>

### Step 3. Update the Controller

<br>

The old controller *should* still compile -- but its not what we want as it 
still only shows the `public` items to other users.

```sh
$ script/test_5.sh

********* add users
...

******** bob follows alice

******** sign-in bob!
{"firstName":"Robert","lastName":"Barron"}
******** list items for alice ...
[{"description":"katla","level":"public"},
 {"description":"counterpart","level":"public"}]
******** logout
```

Of course, we have to change how the controller behaves as well, 
by making it issue queries that also return `"follower"` level 
items. Edit `src/Controllers/List.hs` to include a function
`checkFollower vId uId` that queries the database to determine 
whether `vId` is a follower of `uId`

```haskell
{-@ checkFollower ::
      vId:_ -> uId:_ ->
      TaggedT<{\_ -> True}, {\_ -> False}> _ _ {b:_|b => follows vId uId}
  @-}
checkFollower :: UserId -> UserId -> Controller Bool
checkFollower vId uId = do
  flws <- selectList (followerSubscriber' ==. vId &&:
                      followerPublisher' ==. uId)
  case flws of
    [] -> return False
    _  -> return True
```

The text inside `{-@ ... @-}` says that `checkFollower` is a 
*read-only* controller, that accesses data visible to *all* 
users (`True`) and sends data to *no* users (`False`). 
Furthermore, the output is a `Bool` which if `True` implies
the `follows` relationship holds between the input `UserId`s.
For more details on the signatures, see [the paper][paper].

Next, update `list` in `src/Controllers/List.hs` to use a separate 
conjunct if the `viewer` is indeed a follower of `userId`

```haskell
{-@ list :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
list :: UserId -> Controller ()
list userId = do
  viewerId  <- project userId' =<< requireAuthUser
  follower  <- checkFollower viewerId userId
  let self   = viewerId == userId
  let chk
       | self      = trueF
       | follower  = itemLevel' <-. ["public", "follower"]
       | otherwise = itemLevel' ==. "public"
  items     <- selectList (itemOwner' ==. userId &&: chk)
  itemDatas <- mapT (\i -> 
                ItemData `fmap` project itemDescription' i
                         <*>    project itemLevel'       i
               )items
  respondJSON status200 itemDatas
```

Oops, there is a type error, can you figure it out? 

**Hint:** Is the query that checks the *follower* status correct?

<br>

### Step 4. Test

<br>

Now, after fixing the type error, when you run the tests, 
we'll see all the right items for users that are indeed 
followers!

```sh

********* add users
...

******** sign-in alice!
...
******** add items for alice
...
******* logout

******** sign-in alice!
{"firstName":"Alice","lastName":"Waters"}
******** list items for alice ...
[{"description":"katla","level":"public"},
 {"description":"counterpart","level":"public"},
 {"description":"marley and me","level":"private"},
 {"description":"daniel tiger","level":"follower"},
 {"description":"lupin","level":"follower"}]
******** logout

******** sign-in bob!
{"firstName":"Robert","lastName":"Barron"}
******** list items for alice ...
[{"description":"katla","level":"public"},
 {"description":"counterpart","level":"public"}]
******** logout

******** bob follows alice

******** sign-in bob!
{"firstName":"Robert","lastName":"Barron"}
******** list items for alice ...
[{"description":"katla","level":"public"},
 {"description":"counterpart","level":"public"},
 {"description":"daniel tiger","level":"follower"},
 {"description":"lupin","level":"follower"}]
******** logout

******** sign-in carlos!
{"firstName":"Carlos","lastName":"Cortado"}
******** list items for alice ...
[{"description":"katla","level":"public"},
 {"description":"counterpart","level":"public"}]
******** logout
```

Now `alice` sees *all* her items, after adding `bob` as a follower, 
he sees the public and shared items, but `carlos` only sees the public 
ones.

<br>
<hr>
<br>

While the above should get you started, there are lots 
of important technical details that we've skipped over. 
Take a look at the [OSDI 21 paper][paper] for more details 
or email [the team](about.html) if you want to contribute 
to or use STORM!

[paper]: https://cseweb.ucsd.edu/~npolikarpova/publications/osdi21.pdf
[blog]: https://ucsd-progsys.github.io/liquidhaskell-blog/
