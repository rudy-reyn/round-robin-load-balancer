# Round robin load balancer

A small implementation of load balancer written in Haskell using network sockets.
This works by a client establishing a connection with the LB which then
forwards the request to a backend server.
Backend servers are selected using round robin scheduling, continuously
cycling through each one.

Load balancing allows for use of distributed networks and backend servers,
allowing for salable applications as well as providing redundancy.
Front end and client side servers can also be generally agnostic to the use of a load balancer,
which can significantly reduce downtime and can improve bandwidth significantly.

While round robin schedulers are relatively easy to implement,
server selection can be improved by monitoring the number of connections per server,
payload sizes, connection time, and other contributing factors.
Servers can also be weighted individually by considering the individual server's
ability to handle traffic.
