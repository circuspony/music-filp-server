/*
Defining Genres
*/
genres_exist(["None", "Chiptune","Hip-Hop"]).
/*
Model for Assembl - Demonic Possession
*/
artist_exists("1","MODEL FOR ASSEMBL","Russian experimental artist working since 2014.").
song_exists("1","DISS MY DEMONS",109,"2019-12-23", ["Chiptune"], ["1"]).
song_exists("2","BARON SUNDAY",126,"2019-12-23", ["Chiptune"], ["1"]).
song_exists("3","SING 2",69,"2019-12-23", ["Chiptune"], ["1"]).
song_exists("4","QARAQALPAQ",150,"2019-12-23", ["Chiptune"], ["1"]).
song_exists("5","WE'LL FIGURE IT OUT",198,"2019-12-23", ["Chiptune"], ["1"]).
song_exists("6","ARMENIA",145,"2019-12-23", ["Chiptune"], ["1"]).
album_exists("1","DEMONIC POSSESSION","An original soundtrack for Demonic Possession JRPG.","2019-12-23","https://f4.bcbits.com/img/a0283480666_10.jpg",["Chiptune"]).
album_has_song("1","1","1").
album_has_song("1","2","2").
album_has_song("1","3","3").
album_has_song("1","4","4").
album_has_song("1","5","5").
album_has_song("1","6","6").
artist_has_album("1","1").
/*
Gunna - Wunna
*/
artist_exists("2","GUNNA","Very prolific and versatile rap artist. Has a LOT of flows.").
song_exists("7", "ARGENTINA",149,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("8", "GIMMICK",129,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("9", "MOTW",153,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("10", "FEIGNING",166,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("11", "DOLLAZ ON MY HEAD",198,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("12", "ADDYS",179,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("13", "SKYBOX",178,"2020-03-06",["Hip-Hop"], ["2"]).
song_exists("14", "WUNNA",158,"2020-05-18",["Hip-Hop"], ["2"]).
song_exists("15", "BLINDFOLD",155,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("16", "ROCKSTAR BIKERS & CHAINS",129,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("17", "MET GALA",149,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("18", "NASTY GIRL / ON CAMERA",231,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("19", "COOLER THAN A B",197,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("20", "I'M ON SOME",143,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("21", "TOP FLOOR",170,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("22", "DON'T PLAY AROUND",191,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("23", "DO BETTER",152,"2020-05-22",["Hip-Hop"], ["2"]).
song_exists("24", "FAR",179,"2020-05-22",["Hip-Hop"], ["2"]).
album_exists("2","WUNNA","Wunna is the second studio album by American rapper Gunna. It's the most album of all time","2020-05-22","https://upload.wikimedia.org/wikipedia/ru/2/23/Wunnaalbum.png",["Hip-Hop"]).
album_has_song("2","1","7").
album_has_song("2","2","8").
album_has_song("2","3","9").
album_has_song("2","4","10").
album_has_song("2","5","11").
album_has_song("2","6","12").
album_has_song("2","7","13").
album_has_song("2","8","14").
album_has_song("2","9","15").
album_has_song("2","10","16").
album_has_song("2","11","17").
album_has_song("2","12","18").
album_has_song("2","13","19").
album_has_song("2","14","20").
album_has_song("2","15","21").
album_has_song("2","16","22").
album_has_song("2","17","23").
album_has_song("2","18","24").
artist_has_album("2","2").

