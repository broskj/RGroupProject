/*
don't use density-based clustering - too many items
plot (<>, genre) in 2d
google scholar: imdb movie recommendations, similar (for literature section of report)

relevant tables:
	cast_info:
		id, person_id (->name.id), movie_id (->title.id)
	movie_info:
		id, movie_id (->title.id), info (genre)
	name:
		id, name
	title:
		id, title, kind_id (1=movie)
	movie_info(_idx):
		movie_id, info_type_id (1=runtime, 97=mpaa, 99=votes distribution, 101=rating, 105=budget, 106=weekend gross, 107=gross), info

mysqldump -u root -p imdb_full m > ~/Desktop/CIS4930/Projects/Group\ Project/RGroupProject/imdb_full.sql
mysqldump -u root -p imdb_full movie_temp --tab=/tmp --fields-terminated-by=',' --fields-optionally-enclosed-by='"' --lines-terminated-by='\n' && sudo mv /tmp/movie_temp.txt ~/Desktop/CIS4930/Projects/Group\ Project/RGroupProject/imdb_arm.csv && sudo mv /tmp/movie_temp.sql ~/Desktop/CIS4930/Projects/Group\ Project/RGroupProject/imdb_arm.sql

mysqldump -u root -p imdb_full mm --tab=/tmp --fields-terminated-by=',' --fields-optionally-enclosed-by='"' --lines-terminated-by='\n' && sudo mv /tmp/mm.txt ~/Desktop/CIS4930/Projects/Group\ Project/RGroupProject/imdb_clustering.csv && sudo mv /tmp/mm.sql ~/Desktop/CIS4930/Projects/Group\ Project/RGroupProject/imdb_clustering.sql

mysqldump -u root -p imdb_full m --tab=/tmp --fields-terminated-by=',' --fields-optionally-enclosed-by='"' --lines-terminated-by='\n' && sudo mv /tmp/m.txt ~/Desktop/CIS4930/Projects/Group\ Project/RGroupProject/use_this_dataset.csv && sudo mv /tmp/m.sql ~/Desktop/CIS4930/Projects/Group\ Project/RGroupProject/use_this_dataset.sql
^?
*/

/*
 * start here
 *
 * describe q;
 * +----------+---------+------+-----+---------+-------+
 * | Field    | Type    | Null | Key | Default | Extra |
 * +----------+---------+------+-----+---------+-------+
 * | movie_id | int(11) | NO   |     | 0       |       |
 * | title    | text    | NO   |     | NULL    |       |
 * | genre    | text    | NO   |     | NULL    |       |
 * | name_id  | int(11) | NO   |     | 0       |       |
 * | name     | text    | NO   |     | NULL    |       |
 * +----------+---------+------+-----+---------+-------+
 *
 * sample queries:
 * select * from q order by rand() limit 10;
 * +----------+------------------------------------+---------+---------+-------------------+
 * | movie_id | title                              | genre   | name_id | name              |
 * +----------+------------------------------------+---------+---------+-------------------+
 * |  2872927 | Kaidan chibusa enoki               | Horror  | 4052359 | Tanabe, Torao     |
 * |  2411704 | A Ranchman's Wooing                | Short   | 3476761 | Williams, Clara   |
 * |  2977455 | Mad Dog Morgan                     | Western |  126260 | Barkham, Don      |
 * |  3030159 | Mucha sangre                       | Action  | 1451181 | Naschy, Paul      |
 * |  3379436 | The Oscar                          | Drama   |  102906 | Bacon, James      |
 * |  3330942 | The Devil, the Servant and the Man | Short   | 3477678 | Williams, Kathlyn |
 * |  2556292 | Chamatkar                          | Fantasy | 3298768 | Shobha            |
 * |  2731850 | Free Willy 2: The Adventure Home   | Drama   |  454107 | Dalton, Wally     |
 * |  2769905 | Gus                                | Drama   | 1600687 | Pierini, Anthony  |
 * |  3356851 | The Juror                          | Drama   | 1709110 | Rini, Peter       |
 * +----------+------------------------------------+---------+---------+-------------------+
 *
 * select * from q where title='Evil Dead';
 * +----------+-----------+--------+---------+----------------------+
 * | movie_id | title     | genre  | name_id | name                 |
 * +----------+-----------+--------+---------+----------------------+
 * |  2697990 | Evil Dead | Horror |  284361 | Butterworth, Stephen |
 * |  2697990 | Evil Dead | Horror |  300231 | Campbell, Bruce      |
 * |  2697990 | Evil Dead | Horror |  490421 | Degas, Rupert        |
 * |  2697990 | Evil Dead | Horror |  536001 | Dorian, Bob          |
 * |  2697990 | Evil Dead | Horror |  626934 | Fernandez, Shiloh    |
 * |  2697990 | Evil Dead | Horror |  939040 | Inca                 |
 * |  2697990 | Evil Dead | Horror | 1331181 | McLarty, Jim         |
 * |  2697990 | Evil Dead | Horror | 1640652 | Pucci, Lou Taylor    |
 * |  2697990 | Evil Dead | Horror | 2194874 | Willetts, Karl       |
 * |  2697990 | Evil Dead | Horror | 2380065 | Blackmore, Elizabeth |
 * |  2697990 | Evil Dead | Horror | 2491685 | Connolly, Phoenix    |
 * |  2697990 | Evil Dead | Horror | 2917229 | Levy, Jane           |
 * |  2697990 | Evil Dead | Horror | 2942018 | Lucas, Jessica       |
 * |  2697990 | Evil Dead | Horror | 3258083 | Sandweiss, Ellen     |
 * |  2697990 | Evil Dead | Horror | 3481865 | Wilson, Randal       |
 * |  2697990 | Evil Dead | Horror |   45879 | Alvarez, Fede        |
 * |  2697990 | Evil Dead | Horror | 3875914 | Shaw, Bryan          |
 * |  2697990 | Evil Dead | Horror |   45879 | Alvarez, Fede        |
 * |  2697990 | Evil Dead | Horror | 1659217 | Raimi, Sam           |
 * |  2697990 | Evil Dead | Horror | 1808537 | Sayagues, Rodo       |
 * +----------+-----------+--------+---------+----------------------+
 */

# temporary table of genre list (py script generates hundreds of weird ones - first 32 are normal)
create table t as select distinct info from movie_info where info_type_id=3 limit 32;

# table combining attributes by key
create table q as select t.id as movie_id, t.title, mi.info as genre, n.id as name_id, n.name from title t, movie_info mi, name n, cast_info where t.kind_id=1 and n.id=cast_info.person_id and t.id = cast_info.movie_id and t.id = mi.movie_id and mi.info in (select info from t);

# table no longer needed
drop table t;

#
select count(distinct(movie_id)) from q;

# get 10,000 random movie IDs
create table u as select distinct(movie_id) from q order by rand() limit 10000;

# match q.movie_id with u.movie_id to generate table with 10,000 movies
create table a as select * from q where q.movie_id in (select movie_id from u);

# u no longer needed - get movie ids again with 'select distinct(movie_id) from a;'
drop table u;

#
select count(distinct(movie_id)) from a;
select count(*) from a;

# add column to main table for person role
create table b as select distinct a.movie_id, a.title, a.genre, a.name_id, a.name, rt.role from a a inner join cast_info ci on a.name_id=ci.person_id inner join role_type rt on ci.role_id=rt.id;

# add column to main table for movie rating - info_type_id=101 -> rating
create table c as select distinct b.movie_id, b.title, mi.info as rating, b.genre, b.name_id, b.name, b.role from b b inner join movie_info_idx mi on b.movie_id = mi.movie_id and mi.info_type_id = 101;

# info_type_id=105 -> budget, but I dont have the .list.gz file downloaded, so selecting budget returns an empty set

create table d as select distinct c.movie_id, c.title, substring(mi.info from length(mi.info)-4) as year, c.rating, c.genre, c.name_id, c.name, c.role from c c inner join movie_info mi on c.movie_id = mi.movie_id and mi.info_type_id=16;

===

create table j as select distinct q.movie_id, q.title, substring(mi.info from length(mi.info)-4) as year, q.genre, q.name_id, q.name from q q, movie_info mi where q.movie_id = mi.movie_id and mi.info_type_id=16;

create table k as select distinct j.movie_id, j.title, j.year, j.genre, mi.info as rating, j.name_id, j.name from j j, movie_info_idx mi where j.movie_id = mi.movie_id and mi.info_type_id=101;

create table l as select distinct k.movie_id, k.title, k.year, k.genre, substring(mi.info from 7 for instr(mi.info, 'f')-7) as mpaa, k.rating, k.name_id, k.name from k k, movie_info mi where k.movie_id=mi.movie_id and mi.info_type_id=97;

create table m as select distinct l.movie_id, l.title, l.year, l.genre, l.mpaa, l.rating, l.name_id, l.name, rt.role from lll l inner join cast_info ci on l.name_id=ci.person_id inner join role_type rt on ci.role_id=rt.id;

create table u as select distinct(movie_id) from m order by rand() limit 10000;

create table ds as select * from m where m.movie_id in (select movie_id from u);
