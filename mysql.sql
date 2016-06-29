create database Books;
grant all on Books.* on Books@localhost identified by 'Books';

flush privileges;

use Books;
create table `user` (id int auto_increment primary key, login varchar(128) unique not null, password varchar(128) not null);

-- The `user` table contains one user with login 'user' and password 'password'
insert into `user`(`login`, `password`) values('user', md5('my-salt-123password'));


create table `book` (id int auto_increment primary key, title varchar(128) unique not null, author varchar(128) not null, content varchar(128) not null);

insert into `book`(`title`, `author`, `content`) values('Min Bok', 'Jag', 'Bokens innehåll.');
