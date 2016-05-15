# Calculator
RESTful API using Yesod

This report describes the design and implementation of a RESTful API using Yesod on Haskell. 

The API provides five basic operations:
Add, sub, mul, div and pigit. All the results are returned using JSON format. For example:
URL

localhost:3000/add/6/5
{"result":"11"}

localhost:3000/sub/6/5
{"result":"1"}

localhost:3000/mul/6/5
{"result":"30"}

localhost:3000/div/60/5
{"result":"12"}

localhost:3000/pidigit/60
{"result":"5"}

In the case of the four first operations the results is obvious. The result for pidigit is the nth hexadecimal digit of Pi.
User accounts are handled using sessions. The following GET requests allows the user to login and logout:
login/user/pass
logout

For example, a user with username “peter” and password “peter123” would go to the URL:
localhost:3000/login/peter/peter123
In order to logout he would go to
localhost:3000/logout

All the information about user accounts is stored in a file users.txt which has one line per user. On each line there will be a username and a password separated by a white space.
The history information is stored on a file history.txt which has a line per user. Each line contains the user name followed by all the history for that user. Each element of this line is separated with a colon (:). For example:
user1:add 6 5:sub 58:
user2:sub 6 4:pi 10:
Whenever a user request is routed by Yesod (arithmetic routes such as add, sub, and others), before computing the result, the session is obtained to check if currently there is a user logged in. If there is a user logged in we open the history file to get the history of that user, and add the new operation to the history of that user. Finally we actually compute the final result of the operation and create the corresponding JSON to be responded to the client.
In order to get the history of an user, first the user must be logged in. Once the user is logged in, he must do a history petition: localhost:3000/history. This GET request will be responded with a JSON containing the history of the current user:
{"result":"pi 10, add 2 20, add 1 1, add 6 5, "}
In order to calculate the n-th digit of Pi directly the BBP formula was implemented. 
The project is divided in two files:
CalcFunctions.hs which contains the implementation of all the arithmetic functions, and some other utility functions.
main.hs: contains the implementation of the RESTful API, having the declaration of all the routes for Yesod, and the functions for handling the requests.


