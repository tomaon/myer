CREATE SCHEMA IF NOT EXISTS auth
;

USE auth
;

DROP TABLE IF EXISTS users
;

CREATE TABLE users (
   user     VARCHAR(255) NOT NULL
,  password VARCHAR(40)
,  UNIQUE INDEX ui_user (user)
);

INSERT INTO users (user, password) VALUES
  ('root', MYSQL_PASSWORD('fugo'))
, ('test', MYSQL_PASSWORD('test'))
;
