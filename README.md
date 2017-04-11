# erlfirebase
erlang REST client for Google FireBase
## Feature list
1. implemented sending push push notifications to devices through Cloud Messaging service 

## howto build it
```shell
  make
```
## howto use it
This steps below are useful for manual testing:
1. start app: 
```shell
  make run
```
2. load application dependecies
```shell
  application:ensure_all_started(erlfirebase).
```
3. specify your Server key. This can be found on Firebase dashboard: Overview -> Project settings -> Cloud Messagig:
```shell
  application:set_env(erlfirebase, apikey, <<"SERVER_KEY">>).
```
4. Have Android device ready to receive push notification. 
5. Send push notification to __Android__ or __iOS__ device:
```shell
  DeviceId= <<"DEVICE_REGISTRATION_ID">>,
  Title= <<"Hi there!">>,
  Body= <<"message notification content">>.
  erlfirebase:push(DeviceId, Title, Body).
```
6. Send push notification to __web application__ :
```shell
  DeviceId2= <<"WEBAPP_REGISTRATION_ID">>,
  Title2= <<"Hi from webapp!">>,
  Body= <<"message notification content">>.
  Opts= [{<<"icon">>, <<"/images/profile_placeholder.png">>},
         {<<"click_action">>, <<"https://YOUR_PROJECT_NAME.firebaseapp.com/">>}].
  erlfirebase:push(DeviceId2, Title2, Body, Opts).
```
if everything is correct with data you provided you should receive push notification on corresponded device, otherwise please check error messages in the Erlang console and the log file located in `log` directory. The directory will be created when the application is running.
