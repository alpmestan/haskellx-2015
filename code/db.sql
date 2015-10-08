create extension pgcrypto;

drop table users;
create table users
	( username text primary key
  , pwhash bytea not null
  , joined_on timestamptz not null default now()
  );

drop table songs;
create table songs
  ( songid serial primary key
  , title text not null
  , filepath text not null
  , username text not null references users(username)
  , added_on timestamptz not null default now()
  );
