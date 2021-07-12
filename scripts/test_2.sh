#!/usr/bin/env bash

printf "\nadd users\n"
scripts/adduser.sh

printf "\nlist ... returns nothing\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list/1

printf "\nsign-in first!\n"
curl -b cookies.txt -c cookies.txt -H "Content-Type: application/json" --data @scripts/login.json http://localhost:3000/api/signin

printf "\nlist ... now succeeds\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list/1

printf "\nlist ... now succeeds\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list/2

printf "\nlist ... now succeeds\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list/3

rm cookies.txt
