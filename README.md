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

## Part I : Blank Demo

1. Fork the (blank) demo template
2. Create the `/list` controller which just responds "hello" or the current time
3. Test it with `curl` 

```sh
$ stack run
``` 

and separately

```sh
$ curl http://localhost:3000/api/list
"OK: hello at 2021-07-09 21:46:16.4505 UTC"⏎

$ curl http://localhost:3000/api/list
"OK: hello at 2021-07-09 21:46:17.9553 UTC"⏎
```

## Part II : Adding Authentication

See     `src/Models.storm` 
See the `User` table
See the `signin/` controller 

1. Modify `list/` route to check if auth (`requireAuthUser`)

2. Add a user

```sh
$ stack run -- add-user --email=rjhala@eng.ucsd.edu --password=rjhala --firstname=Ranjit --lastname=Jhala
```

curl --cookie-jar cookies.txt -H "Content-Type: application/json" --data @login.json http://localhost:3000/api/signin


http://localhost:3000/api/signin/conf

3. Test with `curl` 
   - test: /api/list/ FAILS
   - test: login
   - test: list       OK

## Part III : Adding Items

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


---
