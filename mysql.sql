-- drop database biblio;
CREATE database biblio;
grant all on biblio.* to `biblio`@`localhost` identified by 'biblio';

Flush privileges;
use biblio;

-- SERIAL is an alias for BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE.

CREATE TABLE user (
       id INT UNSIGNED auto_increment PRIMARY KEY,
       login VARCHAR(128) UNIQUE NOT NULL,
       password VARCHAR(128) NOT NULL,
       role CHAR(15)
       );


CREATE TABLE shelf (
       id INT UNSIGNED auto_increment PRIMARY KEY,
       name VARCHAR(128) UNIQUE NOT NULL,
       size INT
       -- room_id INT
       );

CREATE TABLE item (
       id INT UNSIGNED auto_increment PRIMARY KEY,
       name VARCHAR(128) UNIQUE NOT NULL,
       info VARCHAR(128) NOT NULL
       );

CREATE TABLE shelfitem (
       id INT UNSIGNED auto_increment PRIMARY KEY,
       quantity INT,
       shelf_id INT UNSIGNED REFERENCES shelf(id),
       item_id  INT UNSIGNED REFERENCES item(id),
       );

-- CREATE TABLE book(id INT auto_increment PRIMARY KEY,
--                   title VARCHAR(128) UNIQUE NOT NULL,
--                   author VARCHAR(128) NOT NULL,
--                   content VARCHAR(128) NOT NULL,
--                   year INT,
--                   user_id INT);

INSERT INTo user(login, password) values('user', md5('my-salt-123password'));
INSERT INTo shelf(name, size) values('A01', 10);
INSERT INTo item(name, info) values('Träskruv', 'Plattskalle');
INSERT INTo shelfitem(shelf_id, item_id, quantity) values(1, 1, 20);
-- INSERT INTo book(`title`, `author`, `content`, `year`, `user_id`) values('Min Bok', 'Jag', 'Bokens innehåll.', 2016, 1);
