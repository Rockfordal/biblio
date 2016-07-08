#drop database biblio;
create database biblio;
grant all on biblio.* to `biblio`@`localhost` identified by 'biblio';

Flush privileges;
use biblio;


-- Användare
create table `user` (id int auto_increment primary key,
                     login varchar(128) unique not null,
                     password varchar(128) not null,
                     role char(15));

insert into `user`(`login`, `password`) values('user', md5('my-salt-123password'));


-- Hyllor
create table shelf(id int auto_increment primary key,
                  name varchar(128) unique not null,
                  size int);
                  -- room_id int

insert into shelf(`name`, `size`) values('A01', 10);


-- Föremål
create table item(id int auto_increment primary key,
                name varchar(128) unique not null,
                info varchar(128) not null);

insert into item(`name`, `info`) values('Träskruv', 'Plattskalle');


-- Inventarie Hyllor<->Items
create table shelfitem(id int auto_increment primary key,
                quantity int,
                shelf_id int,
                item_id  int);

insert into shelfitem(`shelf_id`, `item_id`, `quantity`) values(1, 1, 20);


-- Böcker
-- create table book(id int auto_increment primary key,
--                   title varchar(128) unique not null,
--                   author varchar(128) not null,
--                   content varchar(128) not null,
--                   year int,
--                   user_id int);

-- insert into book(`title`, `author`, `content`, `year`, `user_id`) values('Min Bok', 'Jag', 'Bokens innehåll.', 2016, 1);
