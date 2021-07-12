
import * as net from 'net'; 

const server = new net.Server(() => { });
server.address();
server.close(() => { });

server.getConnections(() => { });
server.listen();

server.listen('', () => { });
server.ref();
server.unref();

const socket = new net.Socket();

socket.address();
socket.connect();
socket.connect('', () => { });
socket.destroy();
socket.end(() => { });
socket.pause();
socket.ref();
socket.resume();
socket.setEncoding();
socket.setKeepAlive();
socket.setNoDelay();
socket.setTimeout(() => { });
socket.unref();
socket.write('', () => { });

net.connect('', () => { });

net.createConnection('', () => { });
net.createServer(() => { });

net.isIP(input)
net.isIPv4(input)
net.isIPv6(input)
