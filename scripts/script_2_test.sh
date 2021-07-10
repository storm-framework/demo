#!/usr/bin/env bash

printf "\nlist ... returns nothing\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list

printf "\nsign-in first\n"
curl -b cookies.txt -c cookies.txt -H "Content-Type: application/json" --data @login.json http://localhost:3000/api/signin

printf "\nlist ... now succeeds\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list

printf "\nlist ... now succeeds\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list

printf "\nlist ... now succeeds\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list

printf "\nlist ... now succeeds\n"
curl -b cookies.txt -c cookies.txt http://localhost:3000/api/list

rm cookies.txt
