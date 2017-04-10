-define(APPNAME, erlfirebase).

-record(request, {uid::binary(),            %% request id
                  operation::atom(),        %% operation
                  deviceId::binary(),       %% device Firebase registration id
                  payload::binary(),        %% request payload
                  callbackPID::atom(),      %% notification process ID
                  status= <<>> ::binary(),  %% request status
                  timestamp::integer()}).   %% status update timestamp 

-define(URL, "https://fcm.googleapis.com/fcm/send").

%%
%%{
%%  "notification": {
%%    "title": "New chat message!",
%%    "body": "There is a new message in Jongla",
%%    "icon": "/images/profile_placeholder.png",
%%    "click_action": "https://jongla-dev.firebaseapp.com/"
%%  },
%%  "to":"c5oFnEY15w4:APA91bFih0eL9FsYGxMO1QopQXvxho-3S8HQD39_iMCLy4WYSzjTij-Q7gAwIhGKkozhMK--kK6SDFyQLT1mIIMlgZK-z_TzflnP6iS37aeOD84SFS0jPjkT4-KfXETC8sms5SPpb7ZC"
%%}
%%
%%{
%%  "notification": {
%%    "title": "New chat message!",
%%    "body": "There is a new message in Jongla"
%%  },
%%  "to":"e0y9qP2HdsM:APA91bFoWma2liEwBe-4gjXc0G1HFtSTJuACBBDBzXOLRp1lmjGmxOGGrN9aUfWIk91GmqkUFpzN7-LwI6zF8JDAUBr2Q6udnwtcdUVsmjmIipazxuLV25dcyNR3_j7cOoW43LXhkyYV"
%%}