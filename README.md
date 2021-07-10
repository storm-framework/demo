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
2. Create the `/list` controller which just responds "hello" or the current time
3. Test with `$ scripts/script_1_test.sh`

## Part II : Adding Authentication

See     `src/Models.storm` 
See the `User` table

1. Modify `/list` route to check if auth (`requireAuthUser`)
2. Add a user `$ scripts/script_2_add_user.sh`
3. Test `$ scripts/script_2_test.sh`

## Part III : Adding Items

HEREHEREHEREHERE 

1. Add the `Item` table to `src/Models.storm`
2. Modify `list/` controller to return all items of `user`
3. Insert items to DB
4. Test it with `curl`

## Part IV : Restrict to Public Items
1. PROBLEM: can login as A and then list B's items!
2. Modify: `models.storm` to specify `public` policy
3. Yikes, build error, fix it!
5. Test with `curl`

## Part V : Restrict to Followers
1. PROBLEM: want to restrict items to 'followers'
2. Add the `Followers` table to `src/Models.storm`
3. Add the policy to `src/Models.storm`
4. Yikes, build error, fix it!
5. Insert items to DB
6. Test it with `curl`
