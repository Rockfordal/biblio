#drop database biblio;
create database biblio;
grant all on biblio.* to `biblio`@`localhost` identified by 'biblio';

Flush privileges;

use biblio;
create table `user` (id int auto_increment primary key,
                     login varchar(128) unique not null,
                     password varchar(128) not null,
                     role char(15));

insert into `user`(`login`, `password`) values('user', md5('my-salt-123password'));


create table book(id int auto_increment primary key,
                  title varchar(128) unique not null,
                  author varchar(128) not null,
                  content varchar(128) not null,
                  year int,
                  user_id int);

create table shelf(id int auto_increment primary key,
                  name varchar(128) unique not null,
                  size int
                  );
                  -- user_id int
                  -- room_id int

insert into book(`title`, `author`, `content`, `year`, `user_id`) values('Min Bok', 'Jag', 'Bokens innehåll.', 2016, 1);
insert into shelf(`name`, `size`) values('A01', 10);
