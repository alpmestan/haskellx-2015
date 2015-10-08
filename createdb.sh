#!/bin/bash

createdb haskellx
psql haskellx -f code/db.sql
mkdir songs
