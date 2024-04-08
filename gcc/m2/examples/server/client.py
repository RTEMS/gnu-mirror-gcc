#!/usr/bin/python3

import sys
from socket import *
serverHost = ''
serverPort = 7000

# create a TCP socket
s = socket(AF_INET, SOCK_STREAM)

s.connect((serverHost, serverPort))
while 1:
   s.send('h')
   data = s.recv(1024)
   print(data)
