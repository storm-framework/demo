# Storm DEMO

![STORM](/figs/logo.png)

## Architecture

A STORM app has the following essential components.

| **File**                           | **Description**                            |
|:-----------------------------------|:-------------------------------------------|
| [`Models.storm`](src/Model.storm)  | (refined) model-policy file                |
| [`Routes.hs`](src/Routes.hs)       | route-controller mapping                   |
| [`Types.hs`](src/Types.hs)         | global type definitions                    |
| [`Config.hs`](src/Config.hs)       | configuration options (e.g. env variables) |
| [`Controllers/`](src/Controllers/) | controller implementations                 |

## Outline

By way of illustration, lets build a small `demo` "wish" list app with routes

- `/signin`   for authentication
- `/add`      create a new entry
- `/list/:id` all the entries

## Build 

You can build and run the web-app by typing 

```sh
$ make
``` 
## Version 1: Blank Demo

1. Fork the (blank) demo template
2. Create the `list` controller which just responds with the current time
3. Test with `$ scripts/test_1.sh`

## Version 2: Adding Authentication

See     `src/Models.storm` 
See the `User` table

1. Modify `list` controller to check if user is logged in (`requireAuthUser`)
2. Add a user `$ scripts/adduser.sh`
3. Test `$ scripts/test_2.sh`

## Version 3: Adding Items

1. Add the `Item` table to `src/Models.storm`
2. Write an `add` controller to add new items for a user
3. Modify `list` controller to return all items of user
4. Test `$ scripts/test_3.sh`

## Version 4: Restrict to Public Items

PROBLEM: `alice` (or anyone!) can see `bob`'s items!

1. Modify: `models.storm` to specify `public` policy
2. Make yields a build error! 
3. Fix the query
4. Test `$ scripts/test_3.sh`

### Error

```
**** LIQUID: UNSAFE ************************************************************

/Users/rjhala/research/storm-demo/src/Controllers/List.hs:31:49: error:
    Liquid Type Mismatch
    .
    The inferred type
      VV : {v : (Database.Persist.Class.PersistEntity.Entity Model.User) | v == getJust (entityKey v)}
    .
    is not a subtype of the required type
      VV : {VV : (Database.Persist.Class.PersistEntity.Entity Model.User) | itemOwner (entityVal (getJust (entityKey VV))) == entityKey VV
                                                                            || itemLevel (entityVal (getJust (entityKey VV))) == "public"}
    .
   |
31 |   itemDatas <- mapT (\i -> ItemData <$> project itemDescription' i <*> project itemLevel' i) items
   |                                                 ^^^^^^^^^^^^^^^^
```

### Fix the query

```
  viewerId  <- project userId' =<< requireAuthUser
  let pub    = if userId == viewerId then trueF else itemLevel' ==. "public" 
  items     <- selectList (itemOwner' ==. userId &&: pub)
```

## Version 5 : Restrict to Followers

PROBLEM: want to share some items with *followers*

1. Add the `Followers` table to `src/Models.storm`
2. Add the policy to `src/Models.storm`
3. Yikes, build error, fix it!
4. Insert items to DB
5. Test `$ scripts/test_5.sh`
