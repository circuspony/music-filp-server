:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_server)).
:- encoding(utf8).

/*

:- initialization
    http_server([port(8080)]).
:-  tell('playlists_data.pl'),
    listing(playlist_exists),
    listing(playlist_has_song),
    told.
*/
:-dynamic playlist_exists/5.
:-dynamic playlist_has_song/2.

:- set_setting_default(http:cors, [*]).
:-consult(facts).
:-consult(playlists).
:- http_handler(root(.),
                http_redirect(moved, location_by_id(all_albums)),
                []).
:- http_handler(root(albums), all_albums, []).
:- http_handler(root(genres), all_genres, []).
:- http_handler(root(songs), all_songs, []).
:- http_handler(root(find), find_artist, []).
:- http_handler(root(artist), get_artist_profile, []).
:- http_handler(root(generate), generate_playlist, []).
:- http_handler(root(create), create_playlist, []).
:- http_handler(root(playlists), all_playlists, []).
:- http_handler(root(tracklist), album_tracklist, []).

/*
Starting route functions
*/
all_albums(Request) :-
    findall([Id, Name,Description,Date,Artwork], album_exists(Id,Name,Description,Date,Artwork),List),
    get_album_genres(List,ListG),
    get_album_artists(ListG, ListWithArtists),
    give_albums_durations(ListWithArtists,ListWithArtistsAndDurations),
    turn_to_dicts_albums(ListWithArtistsAndDurations,ListDicts),
    check_for_genre(Request, ListDicts, ListGenredDicts),
    check_for_artist(Request, ListGenredDicts, ListGenredArtistedDicts),
    check_for_search(Request, ListGenredArtistedDicts, ListGenredArtistedDictsN),
    request_sorter(Request,ListGenredArtistedDictsN,ListSorted),
    cors_enable,
    reply_json(json([ album_list=ListSorted])).


/*
This is where all things genre related are stored
*/
get_genre_list([],[]).
get_genre_list([Id|Rest],FinalGenres):-song_exists(Id,_,_,_, Genres, _), get_genre_list(Rest,Temp), append_without_dupicates(Temp,Genres,FinalGenres).

append_without_dupicates(Temp,[],Temp).
append_without_dupicates(Temp, [G|R],[G|NewTemp]):-not(member(G,Temp)),append_without_dupicates(Temp,R,NewTemp).
append_without_dupicates(Temp, [G|R],NewTemp):-member(G,Temp),append_without_dupicates(Temp,R,NewTemp).

get_album_genres([],[]).
get_album_genres([[AlbumId,Name,Description,Date,Artwork]|Rest], [[AlbumId, Name,Description,Date,Artwork, Genres]|NewRest]):-findall(SongId,album_has_song(AlbumId, _, SongId),Songs), get_genre_list(Songs,Genres), get_album_genres(Rest,NewRest).

all_genres(Request) :-
    findall(Genre, genre_exists(Genre),GenreList),
    cors_enable,
    reply_json(json([ genre_list=GenreList])).
/*
This turns arrays into dicts
*/
turn_to_dicts_albums([[Duration, AlbumId, ArtistIds, Artists, Name, Description, Date, Artwork,Genres]],[point{duration:Duration, albumid:AlbumId, artistids:ArtistIds, artists:Artists, name:Name, description:Description, date:Date, artwork:Artwork,genres:Genres}]).
turn_to_dicts_albums([[Duration, AlbumId, ArtistIds, Artists, Name, Description, Date, Artwork,Genres]|Rest],[point{duration:Duration, albumid:AlbumId, artistids:ArtistIds, artists:Artists, name:Name, description:Description, date:Date, artwork:Artwork,genres:Genres}|NewRest]):-turn_to_dicts_albums(Rest,NewRest).

turn_to_dicts_playlists([[Duration, AlbumId,  Author, Name, Description, Artwork]],[point{duration:Duration, albumid:AlbumId,  author:Author, name:Name, description:Description, artwork:Artwork}]).
turn_to_dicts_playlists([[Duration, AlbumId,  Author, Name, Description, Artwork]|Rest],[point{duration:Duration, albumid:AlbumId,  author:Author, name:Name, description:Description, artwork:Artwork}|NewRest]):-turn_to_dicts_playlists(Rest,NewRest).

turn_to_dicts_songs([[Artwork, AlbumName, AlbumId, SongId, Name, Duration, Date, Genres, ArtistIds, Artists, TrackListNumber]],[point{artwork:Artwork, albumname:AlbumName, albumid:AlbumId, songid:SongId, name: Name, duration:Duration, date:Date, genres:Genres, artistids:ArtistIds, artists:Artists, tracklistnumber:TrackListNumber}]).
turn_to_dicts_songs([[Artwork, AlbumName, AlbumId, SongId, Name, Duration, Date, Genres, ArtistIds, Artists, TrackListNumber]|Rest],[point{artwork:Artwork, albumname:AlbumName, albumid:AlbumId, songid:SongId, name: Name, duration:Duration, date:Date, genres:Genres, artistids:ArtistIds, artists:Artists, tracklistnumber:TrackListNumber}|NewRest]):-turn_to_dicts_songs(Rest,NewRest).
/*
This checks for genres in a query
*/
check_for_genre(Request, Dict, NewDict):- http_parameters(Request,
                    [ genre(Genre, [string,optional(true)])
                    ]),
    replace_string(Genre, "%20", GenreSpace),
    GenreSpace \== "None",
    filter_dicts(Dict,GenreSpace,genres,NewDict).
check_for_genre(Request, Dict, Dict):- http_parameters(Request,
                    [ genre(Genre, [string,optional(true)])
                    ]),
    Genre = "None".

filter_dicts([],_,_,[]).
filter_dicts([Dict|Rest],Element,Key,[Dict|NewRest]):-Elements =Dict.get(Key), member(Element, Elements), filter_dicts(Rest,Element,Key,NewRest).
filter_dicts([Dict|Rest],Element,Key,NewRest):-Elements = Dict.get(Key), not(member(Element, Elements)), filter_dicts(Rest,Element,Key,NewRest).

filter_dicts_field([],_,_,[]).
filter_dicts_field([Dict|Rest],Element,Key,[Dict|NewRest]):-ElementReal =Dict.get(Key),sub_string(ElementReal, _, _, _, Element), filter_dicts_field(Rest,Element,Key,NewRest).
filter_dicts_field([Dict|Rest],Element,Key,NewRest):-ElementReal = Dict.get(Key), not(sub_string(ElementReal, _, _, _, Element)), filter_dicts_field(Rest,Element,Key,NewRest).

/*
This checks for artist in a query
*/
check_for_artist(Request, Dict, NewDict):- http_parameters(Request,
                    [ artist(ArtistId, [string,optional(true)])
                    ]),
    ArtistId \== "0",
    filter_dicts(Dict,ArtistId,artistids,NewDict).
check_for_artist(Request, Dict, Dict):- http_parameters(Request,
                    [ artist(ArtistId, [string,optional(true)])
                    ]),
    ArtistId = "0".
/*
This checks for search in a query
*/
check_for_search(Request, Dict, NewDict):- http_parameters(Request,
                    [ search(Search, [boolean,optional(true)]),
                    searchstring(SearchString, [string,optional(true)])
                    ]),
    Search,
    replace_string(SearchString, "%20", SearchStringWS),
    string_upper(SearchStringWS,SearchStringUC),
    filter_dicts_field(Dict,SearchStringUC,name,NewDict).
check_for_search(Request, Dict, Dict):- http_parameters(Request,
                    [ search(Search, [boolean,optional(true)]),
                    searchstring(SearchString, [string,optional(true)])
                    ]),
    not(Search).

/*
This checks for the sorting parameters
*/
request_sorter(Request, DataList, NewDataList):- http_parameters(Request,
                    [ sortdu(SortDurationUp, [boolean,optional(true)])
                    ]),
    SortDurationUp,
    sort(duration,@=<,DataList,NewDataList).
request_sorter(Request, DataList, NewDataList):- http_parameters(Request,
                    [ sortdd(SortDurationDown, [boolean,optional(true)])
                    ]),
    SortDurationDown,
    sort(duration,@>=,DataList,NewDataList).
request_sorter(Request, DataList, NewDataList):- http_parameters(Request,
                    [ sortyu(SortYearUp, [boolean,optional(true)])
                    ]),
    SortYearUp,
    sort(date,@=<,DataList,NewDataList).
request_sorter(Request, DataList, NewDataList):- http_parameters(Request,
                    [ 
                    sortyd(SortYearDown, [boolean,optional(true)])
                    ]),
    SortYearDown,
    sort(date,@>=,DataList,NewDataList).
request_sorter(Request, DataList, DataList):- http_parameters(Request,
                    [ sortdu(SortDurationUp, [boolean,optional(true)]),
                    sortdu(SortDurationDown, [boolean,optional(true)]),
                    sortyu(SortYearUp, [boolean,optional(true)]),
                    sortyd(SortYearDown, [boolean,optional(true)])
                    ]),
   not(SortDurationUp),
    not(SortDurationDown),
    not(SortYearUp),
    not(SortYearDown).

/*
This works with album sorters
get_artists should be renamed because it returns in reality the most full set of dat 
*/
populate_songs([],[]).
populate_songs([SongID|Rest],[[SongID,Name,Duration,Date,Genres,Artists]|PopulatedRest]):-song_exists(SongID,Name,Duration,Date,Genres,Artists),populate_songs(Rest,PopulatedRest).

get_tracklist(AlbumId,FinalTrackList):-findall(SongID,album_has_song(AlbumId,_,SongID),SimpleTrackList),populate_songs(SimpleTrackList,DetailedTrackList),  get_artists(DetailedTrackList,FinalTrackList).
get_tracklist(PlaylistID,FinalTrackList):-findall(SongID,playlist_has_song(PlaylistID,SongID),SimpleTrackList), populate_songs(SimpleTrackList,DetailedTrackList), get_artists(DetailedTrackList,FinalTrackList).

sum_tracklist_duration([],Temp, Temp).
sum_tracklist_duration([[_,_,_,_,_,Duration,_,_,_,_,_]|Rest],Temp, Sum):-NewTemp is Temp+Duration,sum_tracklist_duration(Rest,NewTemp,Sum).
sum_tracklist_duration([[_,_,Duration,_,_,_]|Rest],Temp, Sum):-NewTemp is Temp+Duration,sum_tracklist_duration(Rest,NewTemp,Sum).

give_albums_durations([[ID|RestInfo]],[[Duration,ID|RestInfo]]):-get_tracklist(ID,TrackList), sum_tracklist_duration(TrackList,0,Duration).
give_albums_durations([[ID|RestInfo]|RestOfAlbums],[[Duration,ID|RestInfo]|UpdatedRest]):-get_tracklist(ID,TrackList), sum_tracklist_duration(TrackList,0,Duration),give_albums_durations(RestOfAlbums,UpdatedRest).

/*
This shoud find all main artists of the album
*/
get_album_artists([[Album|Rest]],[[Album, ListOfArtistIds, PopulatedArtists|Rest]] ):-findall(Artist, artist_has_album(Artist, Album), ListOfArtistIds),populate_artists(ListOfArtistIds,PopulatedArtists).
get_album_artists([[Album|Rest]|RestOfAlbums],[[Album, ListOfArtistIds, PopulatedArtists|Rest]|UpdatedRest]):-findall(Artist, artist_has_album(Artist, Album), ListOfArtistIds),populate_artists(ListOfArtistIds,PopulatedArtists), get_album_artists(RestOfAlbums, UpdatedRest).

/*
This request should get all songs
*/
all_songs(Request) :-
    findall([Id,Name,Duration,Date,Genres,Artists], song_exists(Id,Name,Duration,Date,Genres,Artists),SongList),
    get_artists(SongList,UpdatedSongList),
    turn_to_dicts_songs(UpdatedSongList,UpdatedSongObj),
    check_for_genre(Request, UpdatedSongObj, GenreUpdatedSongObj),
    check_for_artist(Request, GenreUpdatedSongObj, GenreUpdatedArtistedSongObj),
    check_for_search(Request, GenreUpdatedArtistedSongObj, GenreUpdatedArtistedSongObjN),
    request_sorter(Request,GenreUpdatedArtistedSongObjN,SotredObj),
    cors_enable,
    reply_json(json([ song_list=SotredObj])).
/*
This goes through songs to give them real artist names
*/
get_artists([[Id,Name,Duration,Date,Genres,Artists]], [[Art, AlbumName, AlbumId, Id, Name,Duration,Date,Genres,Artists, PopulatedArtists,TrackListNumber]]):-populate_artists(Artists,PopulatedArtists),album_has_song(AlbumId,TrackListNumber,Id),album_exists(AlbumId,AlbumName,_,_,Art).
get_artists([[Id,Name,Duration,Date,Genres,Artists]|RestOfSongs], [[Art, AlbumName, AlbumId, Id, Name,Duration,Date,Genres,Artists, PopulatedArtists,TrackListNumber] | UpdatedList]):-populate_artists(Artists,PopulatedArtists), album_has_song(AlbumId,TrackListNumber,Id), album_exists(AlbumId,AlbumName,_,_,Art), get_artists(RestOfSongs, UpdatedList).
/*
This associates the names with ids
*/
populate_artists([Artist],[ArtistName]):-artist_exists(Artist,ArtistName,_).
populate_artists([Artist|Rest],[ArtistName|PopulatedRest]):-artist_exists(Artist,ArtistName,_), populate_artists(Rest,PopulatedRest).
/*
This looks for an artist
*/
find_artist(Request) :-http_parameters(Request,
                    [ 
                    name(ArtistName, [string,optional(true)])
                    ]),
    string_upper(ArtistName,ArtistNameUC),
    findall(point{id:ID,name:ArtistNameFound, description:Description}, artist_exists(ID,ArtistNameFound,Description), NameList),
    filter_dicts_field(NameList,ArtistNameUC,name,NewDict),
    cors_enable,
    reply_json(json([ artist_id=NewDict])).

get_artist_profile(Request) :-http_parameters(Request,
                    [ 
                    id(ArtistId, [string,optional(true)])
                    ]),
    artist_exists(ArtistId,ArtistName,Description),
    Profile = point{artistname:ArtistName,description:Description},
    cors_enable,
    reply_json(json([ artist=Profile])).
/*
This generates playlists
*/
generate_playlist(Request):-
    http_parameters(Request,
                    [ 
                    number(Number, [number,optional(true)])
                    ]),
   get_a_random_list(Number,List),
    get_artists(List,UpdatedSongList),
    turn_to_dicts_songs(UpdatedSongList,UpdatedSongObj),
    cors_enable,
    reply_json(json([ playlist=UpdatedSongObj])).



get_a_random_list(0,[]).
get_a_random_list(Number,[RName|Rest]):- 
    predicate_property(song_exists(_,_,_,_, _, _),number_of_clauses(ClauseCount)),
    random_between(1,ClauseCount,RandomNumber),
    RandomNumber \== 0,
    findnsols(RandomNumber, [Id,Name,Duration,Date,Genres,Artists],song_exists(Id,Name,Duration,Date,Genres,Artists) , List),
    last(List,RName),
    NewNumber is Number-1,
    get_a_random_list(NewNumber, Rest).
/*
This deals with playlists
*/
all_playlists(Request):-
    reconsult('playlists_data.pl'),
    findall([Id, Name,  AuthorName, Description,Artwork], playlist_exists(Id, Name, AuthorName, Description,Artwork),List),
    give_albums_durations(List,ListWithArtistsAndDurations),
    turn_to_dicts_playlists(ListWithArtistsAndDurations,ListDicts),
    cors_enable,
    reply_json(json([ playlists=ListDicts])).

create_playlist(Request):-   
    reconsult('playlists_data.pl'),     
    http_parameters(Request,
                    [ 
                    name(Name, [string,optional(true)]),
                    author(Author, [string,optional(true)]),
                    description(Description, [string,optional(true)]),
                    arraystring(Array, [string,optional(true)])
                    ]),
   findall(ID,playlist_exists(ID,_,_,_,_),List),
   last(List,LastID),   
   string_concat("p",LastIDNumber,LastID),
   number_string(Number,LastIDNumber),
   NewNumber is Number+1,
   number_string(NewNumber,NewString),
   string_concat("p",NewString,NewID),
   random_between(1,3,RandomNumber),
   get_a_random_image(RandomNumber,Image),
   assert(playlist_exists(NewID,Author, Name,Description,Image)),
   split_string(Array, ',', ',', NewArray),
   assert_arrays(NewArray,NewID),
    tell('playlists_data.pl'),   
    listing(playlist_exists),
    listing(playlist_has_song),
    told,
    cors_enable,
    reply_json(json([ status="GOOD"])).

assert_arrays([],_).
assert_arrays([ID|R],PID):-assert(playlist_has_song(PID,ID)),assert_arrays(R,PID).

get_a_random_image(1,"https://pbs.twimg.com/profile_images/1161679754065563648/4_1yjOO7_400x400.jpg").
get_a_random_image(2,"https://lh3.googleusercontent.com/adbDj4JGglVp2kn4p1lbzl1svZ59-9M2JODP3MqUtNWCuyXU3KSFepXWSL2rcq0LUBA37q6SHJ1rIQag4Emm5xjVKmX6TNO-1A=s750").
get_a_random_image(3,"https://playlistpush.com/wp-content/uploads/2017/11/logo-final.png").

/*
This replies with tracklists for the realeases
*/
album_tracklist(Request) :-http_parameters(Request,
                    [ 
                    id(AlbumId, [string,optional(true)])
                    ]),
    get_tracklist(AlbumId, Tracklist),
    turn_to_dicts_songs(Tracklist,TrackListObj),
    album_exists(AlbumId,Name,Description,_,Artwork),
    cors_enable,
    reply_json(json([ tracklist=TrackListObj,
    album = point{albumid:AlbumId, name:Name, description:Description, artwork:Artwork}
    ])).

album_tracklist(Request) :-http_parameters(Request,
                    [ 
                    id(AlbumId, [string,optional(true)])
                    ]),
    get_tracklist(AlbumId, Tracklist),
    turn_to_dicts_songs(Tracklist,TrackListObj),
    playlist_exists(AlbumId,Author,Name, Description,Artwork),
    cors_enable,
    reply_json(json([ tracklist=TrackListObj, 
    album=point{albumid:AlbumId, author:Author, name:Name, description:Description, artwork:Artwork}]
    )).

/*
A quick replacer
*/
concat_with_space([S],S).
concat_with_space([S|R],FS):-string_concat(S," ",NS), concat_with_space(R,RS), string_concat(NS,RS,FS).

replace_string(String, SubString, NewString):-split_string(String, SubString, SubString, List), concat_with_space(List,NewString).