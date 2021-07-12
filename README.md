# Storm DEMO

## TODO 

-???

## Architecture

| **File**                           | **Description**                            |
|:-----------------------------------|:-------------------------------------------|
| [`Models.storm`](src/Model.storm)  | (refined) model-policy file                |
| [`Routes.hs`](src/Routes.hs)       | route-controller mapping                   |
| [`Types.hs`](src/Types.hs)         | global type definitions                    |
| [`Config.hs`](src/Config.hs)       | configuration options (e.g. env variables) |
| [`Controllers/`](src/Controllers/) | controller implementations                 |

## Outline

We will build a small "shopping" or "wish" list app with routes

- `/signin`   for authentication
- `/signout`  ... for authentication
- `/list`     all the entries
- `/create`   create a new entry
- `/remove`   remove an entry    
- `/restore`  restore a (previously) bought entry

After editing the code, you can rebuild and run the server with `make`

```sh
$ make
``` 
## Part I : Blank Demo

1. Fork the (blank) demo template
2. Create the `/list/:id` controller which just responds with the current time
3. Test with `$ scripts/script_1_test.sh`

## Part II : Adding Authentication

See     `src/Models.storm` 
See the `User` table

1. Modify `/list` route to check if auth (`requireAuthUser`)
2. Add a user `$ scripts/adduser.sh`
3. Test `$ scripts/test_2.sh`

## Part III : Adding Items

1. Add the `Item` table to `src/Models.storm`
2. Add a `add/` controller to add new items for a user
3. Modify `list/` controller to return all items of user
4. Test `$ scripts/test_3.sh`

## Part IV : Restrict to Public Items

PROBLEM: `alice` (or anyone!) can see `bob`'s items!

1. Modify: `models.storm` to specify `public` policy <<<< HEREHEREHEREHERE

2. Yikes, build error! 

3. Fix the query

3. Test with `curl`

## Part V : Restrict to Followers

PROBLEM: want to restrict items to 'followers'

1. Add the `Followers` table to `src/Models.storm`
2. Add the policy to `src/Models.storm`
3. Yikes, build error, fix it!
4. Insert items to DB
5. Test it with `curl`
