
:- encoding(utf8).
/*
Defining Genres
*/
genre_exists("Нет").
genre_exists("Electronic").
genre_exists("Ambient").
genre_exists("Death Metal").
genre_exists("Hip-Hop").
genre_exists("Post-Rock").
genre_exists("Classical").
/*
Model for Assembl - Demonic Possession
*/
artist_exists("1","MODEL FOR ASSEMBL","Экспериментальный музыкальный проект из России. Активен с 2014.").
song_exists("1","DISS MY DEMONS",109,"2019-12-23", ["Ambient", "Electronic"], ["1"]).
song_exists("2","BARON SUNDAY",126,"2019-12-23", ["Electronic"], ["1"]).
song_exists("3","SING 2",69,"2019-12-23", ["Electronic"], ["1"]).
song_exists("4","QARAQALPAQ",150,"2019-12-23", ["Electronic"], ["1"]).
song_exists("5","WE'LL FIGURE IT OUT",198,"2019-12-23", ["Electronic"], ["1"]).
song_exists("6","ARMENIA",145,"2019-12-23", ["Electronic"], ["1"]).
album_exists("1","DEMONIC POSSESSION","Оригинальный саундтрек для JRPG игры Demonic Possession.","2019-12-23","https://f4.bcbits.com/img/a0283480666_10.jpg").
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
artist_exists("2","GUNNA","Популярный и крайне продуктивный хип-хоп исполнитель. Имеет рекорд по количеству коллабораций с другими исполнителями.").
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
album_exists("2","WUNNA","Wunna является вторым студийным альбомом Gunna. Один из самых альбомов всех времен.","2020-05-22","https://upload.wikimedia.org/wikipedia/ru/2/23/Wunnaalbum.png").
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
/*
Gorguts- Obscura
*/
artist_exists("3","GORGUTS","Gorguts — дэт-метал-группа, образованная в 1989 году в Шербруке (провинция Квебек, Канада). Группа известна своей сложной, музыкально плотной формой техничного дэт-метала и, по ряду мнений, стала «одной из самых прогрессивных, экспериментальных и смелых групп во всем жанре».").
song_exists("25","OBSCURA",244,"1998-06-23", ["Death Metal"], ["3"]).
song_exists("26","EARTHLY LOVE",244,"1998-06-23", ["Death Metal"], ["3"]).
song_exists("27","THE CARNAL STATE",188,"1998-06-23", ["Death Metal"], ["3"]).
song_exists("28","NOSTALGIA",370,"1998-06-23", ["Death Metal"], ["3"]).
song_exists("29","THE ART OF SOMBRE ECSTASY",260,"1998-06-23", ["Death Metal"], ["3"]).
song_exists("30","CLOUDED",572,"1998-06-23", ["Death Metal"], ["3"]).
song_exists("31","SUBTLE BODY",203,"1998-06-23", ["Death Metal"], ["3"]).
song_exists("32","RAPTUROUS GRIEF",327,"1998-06-23", ["Death Metal"], ["3"]).
song_exists("33","LA VIE EST PRELUDE",208,"1998-06-23", ["Death Metal","Classical"], ["3"]).
song_exists("34","ILLUMINATUS",375,"1998-06-23", ["Death Metal"], ["3"]).
song_exists("35","FACELESS ONES",230,"1998-06-23", ["Death Metal"], ["3"]).
song_exists("36","SWEET SILENCE",405,"1998-06-23", ["Death Metal"], ["3"]).
album_exists("3","OBSURA","Третий альбом GORGUTS и альбом, о котором постоянно спрашивают студентов, обучающихся академической музыке.","1998-06-23","https://upload.wikimedia.org/wikipedia/ru/4/42/Gorguts_Obscura_Cover.jpg").
album_has_song("3","1","25").
album_has_song("3","2","26").
album_has_song("3","3","27").
album_has_song("3","4","28").
album_has_song("3","5","29").
album_has_song("3","6","30").
album_has_song("3","7","31").
album_has_song("3","8","32").
album_has_song("3","9","33").
album_has_song("3","10","34").
album_has_song("3","11","35").
album_has_song("3","12","36").
artist_has_album("3","3").
/*
Mono - Are You There
*/
artist_exists("4","MONO","Японский инструментальный коллектив, работающий в жанрах пост-рок и современная классика.").
song_exists("37","THE FLAMES BEYONG THE COLD MOUNTAIN",809,"2006-03-15", ["Post-Rock"], ["4"]).
song_exists("38","A HEART HAS ASKED FOR THE PLEASURE",223,"2006-03-15", ["Post-Rock"], ["4"]).
song_exists("39","YEARNING",938,"2006-03-15", ["Post-Rock"], ["4"]).
song_exists("40","ARE YOU THERE?",625,"2006-03-15", ["Post-Rock"], ["4"]).
song_exists("41","THE REMAINS OF THE DAY",221,"2006-03-15", ["Post-Rock"], ["4"]).
song_exists("42","MOONLIGHT",784,"2006-03-15", ["Post-Rock","Ambient","Classical"], ["4"]).
album_exists("4","YOU ARE THERE","Четвертый альбом группы, изначально выпущенный на двух дисках.","2006-03-15","https://upload.wikimedia.org/wikipedia/en/b/b1/Mono-YouAreThere.jpg").
album_has_song("4","1","37").
album_has_song("4","2","38").
album_has_song("4","3","39").
album_has_song("4","4","40").
album_has_song("4","5","41").
album_has_song("4","6","42").
artist_has_album("4","4").