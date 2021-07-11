#!/usr/bin/env bash

printf "\nsign-in alice!\n"
curl -b cookies.txt -c cookies.txt -H "Content-Type: application/json" --data @scripts/login.json http://localhost:3000/api/signin

printf "\nadd items for alice\n"
curl -b cookies.txt -c cookies.txt -H "Content-Type: application/json" --data @scripts/items_alice.json http://localhost:3000/api/add

printf "\nlogout alice\n"
rm cookies.txt

printf "\nsign-in bob!\n"
curl -b cookies.txt -c cookies.txt -H "Content-Type: application/json" --data @scripts/login_bob.json http://localhost:3000/api/signin

printf "\nadd items for bob\n"
curl -b cookies.txt -c cookies.txt -H "Content-Type: application/json" --data @scripts/items_bob.json http://localhost:3000/api/add

printf "\nlogout bob\n"
rm cookies.txt

printf "\nlist items for alice ...\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list/1

printf "\nlist items for bob...\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list/2


