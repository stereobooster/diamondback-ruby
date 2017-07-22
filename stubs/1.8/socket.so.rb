class SocketError < StandardError
end

class BasicSocket < IO
  include File::Constants
  include Enumerable
  ##% close_read : (*!FIXME) -> !FIXME
  def close_read(*) end
  ##% close_write : (*!FIXME) -> !FIXME
  def close_write(*) end
  ##% getpeername : (*!FIXME) -> !FIXME
  def getpeername(*) end
  ##% getsockname : (*!FIXME) -> !FIXME
  def getsockname(*) end
  ##% getsockopt : (*!FIXME) -> !FIXME
  def getsockopt(*) end
  ##% recv : (Numeric, ?Numeric) -> String
  def recv(*rest) end
  ##% recv_nonblock : (*!FIXME) -> !FIXME
  def recv_nonblock(*rest) end
  ##% send : (*!FIXME) -> !FIXME
  def send(*rest) end
  ##% setsockopt : (*!FIXME) -> !FIXME
  def setsockopt(*) end
  ##% shutdown : (*!FIXME) -> !FIXME
  def shutdown(*rest) end
  ##% BasicSocket.do_not_reverse_lookup : () -> Boolean
  def BasicSocket.do_not_reverse_lookup() end
  ##% BasicSocket.do_not_reverse_lookup= : Boolean -> Boolean
  def BasicSocket.do_not_reverse_lookup=(p0) end
  ##% BasicSocket.for_fd : (*!FIXME) -> !FIXME
  def BasicSocket.for_fd(p0) end
end

class Socket < BasicSocket
  AF_APPLETALK = 5
  AF_AX25 = 3
  AF_INET = 2
  AF_INET6 = 10
  AF_IPX = 4
  AF_LOCAL = 1
  AF_MAX = 34
  AF_ROUTE = 16
  AF_SNA = 22
  AF_UNIX = 1
  AF_UNSPEC = 0
  AI_ADDRCONFIG = 32
  AI_ALL = 16
  AI_CANONNAME = 2
  AI_NUMERICHOST = 4
  AI_PASSIVE = 1
  AI_V4MAPPED = 8
  module Socket::Constants
    AF_APPLETALK = 5
    AF_AX25 = 3
    AF_INET = 2
    AF_INET6 = 10
    AF_IPX = 4
    AF_LOCAL = 1
    AF_MAX = 34
    AF_ROUTE = 16
    AF_SNA = 22
    AF_UNIX = 1
    AF_UNSPEC = 0
    AI_ADDRCONFIG = 32
    AI_ALL = 16
    AI_CANONNAME = 2
    AI_NUMERICHOST = 4
    AI_PASSIVE = 1
    AI_V4MAPPED = 8
    EAI_ADDRFAMILY = -9
    EAI_AGAIN = -3
    EAI_BADFLAGS = -1
    EAI_FAIL = -4
    EAI_FAMILY = -6
    EAI_MEMORY = -10
    EAI_NODATA = -5
    EAI_NONAME = -2
    EAI_SERVICE = -8
    EAI_SOCKTYPE = -7
    EAI_SYSTEM = -11
    INADDR_ALLHOSTS_GROUP = -536870911
    INADDR_ANY = 0
    INADDR_BROADCAST = -1
    INADDR_LOOPBACK = -16777215
    INADDR_MAX_LOCAL_GROUP = -536870657
    INADDR_NONE = -1
    INADDR_UNSPEC_GROUP = -536870912
    IPPORT_RESERVED = 1024
    IPPORT_USERRESERVED = 5000
    IPPROTO_EGP = 8
    IPPROTO_ICMP = 1
    IPPROTO_IDP = 22
    IPPROTO_IGMP = 2
    IPPROTO_IP = 0
    IPPROTO_PUP = 12
    IPPROTO_RAW = 255
    IPPROTO_TCP = 6
    IPPROTO_TP = 29
    IPPROTO_UDP = 17
    IP_ADD_MEMBERSHIP = 35
    IP_DEFAULT_MULTICAST_LOOP = 1
    IP_DEFAULT_MULTICAST_TTL = 1
    IP_DROP_MEMBERSHIP = 36
    IP_HDRINCL = 3
    IP_MAX_MEMBERSHIPS = 20
    IP_MULTICAST_IF = 32
    IP_MULTICAST_LOOP = 34
    IP_MULTICAST_TTL = 33
    IP_OPTIONS = 4
    IP_RECVOPTS = 6
    IP_RECVRETOPTS = 7
    IP_RETOPTS = 7
    IP_TOS = 1
    IP_TTL = 2
    MSG_CTRUNC = 8
    MSG_DONTROUTE = 4
    MSG_DONTWAIT = 64
    MSG_EOR = 128
    MSG_OOB = 1
    MSG_PEEK = 2
    MSG_TRUNC = 32
    MSG_WAITALL = 256
    NI_DGRAM = 16
    NI_MAXHOST = 1025
    NI_MAXSERV = 32
    NI_NAMEREQD = 8
    NI_NOFQDN = 4
    NI_NUMERICHOST = 1
    NI_NUMERICSERV = 2
    PF_APPLETALK = 5
    PF_AX25 = 3
    PF_INET = 2
    PF_INET6 = 10
    PF_IPX = 4
    PF_KEY = 15
    PF_LOCAL = 1
    PF_MAX = 34
    PF_ROUTE = 16
    PF_SNA = 22
    PF_UNIX = 1
    PF_UNSPEC = 0
    SHUT_RD = 0
    SHUT_RDWR = 2
    SHUT_WR = 1
    SOCK_DGRAM = 2
    SOCK_PACKET = 10
    SOCK_RAW = 3
    SOCK_RDM = 4
    SOCK_SEQPACKET = 5
    SOCK_STREAM = 1
    SOL_IP = 0
    SOL_SOCKET = 1
    SOL_TCP = 6
    SOL_UDP = 17
    SO_ACCEPTCONN = 30
    SO_ATTACH_FILTER = 26
    SO_BINDTODEVICE = 25
    SO_BROADCAST = 6
    SO_DEBUG = 1
    SO_DETACH_FILTER = 27
    SO_DONTROUTE = 5
    SO_ERROR = 4
    SO_KEEPALIVE = 9
    SO_LINGER = 13
    SO_NO_CHECK = 11
    SO_OOBINLINE = 10
    SO_PASSCRED = 16
    SO_PEERCRED = 17
    SO_PEERNAME = 28
    SO_PRIORITY = 12
    SO_RCVBUF = 8
    SO_RCVLOWAT = 18
    SO_RCVTIMEO = 20
    SO_REUSEADDR = 2
    SO_SECURITY_AUTHENTICATION = 22
    SO_SECURITY_ENCRYPTION_NETWORK = 24
    SO_SECURITY_ENCRYPTION_TRANSPORT = 23
    SO_SNDBUF = 7
    SO_SNDLOWAT = 19
    SO_SNDTIMEO = 21
    SO_TIMESTAMP = 29
    SO_TYPE = 3
    TCP_MAXSEG = 2
    TCP_NODELAY = 1
  end

  EAI_ADDRFAMILY = -9
  EAI_AGAIN = -3
  EAI_BADFLAGS = -1
  EAI_FAIL = -4
  EAI_FAMILY = -6
  EAI_MEMORY = -10
  EAI_NODATA = -5
  EAI_NONAME = -2
  EAI_SERVICE = -8
  EAI_SOCKTYPE = -7
  EAI_SYSTEM = -11
  INADDR_ALLHOSTS_GROUP = -536870911
  INADDR_ANY = 0
  INADDR_BROADCAST = -1
  INADDR_LOOPBACK = -16777215
  INADDR_MAX_LOCAL_GROUP = -536870657
  INADDR_NONE = -1
  INADDR_UNSPEC_GROUP = -536870912
  IPPORT_RESERVED = 1024
  IPPORT_USERRESERVED = 5000
  IPPROTO_EGP = 8
  IPPROTO_ICMP = 1
  IPPROTO_IDP = 22
  IPPROTO_IGMP = 2
  IPPROTO_IP = 0
  IPPROTO_PUP = 12
  IPPROTO_RAW = 255
  IPPROTO_TCP = 6
  IPPROTO_TP = 29
  IPPROTO_UDP = 17
  IP_ADD_MEMBERSHIP = 35
  IP_DEFAULT_MULTICAST_LOOP = 1
  IP_DEFAULT_MULTICAST_TTL = 1
  IP_DROP_MEMBERSHIP = 36
  IP_HDRINCL = 3
  IP_MAX_MEMBERSHIPS = 20
  IP_MULTICAST_IF = 32
  IP_MULTICAST_LOOP = 34
  IP_MULTICAST_TTL = 33
  IP_OPTIONS = 4
  IP_RECVOPTS = 6
  IP_RECVRETOPTS = 7
  IP_RETOPTS = 7
  IP_TOS = 1
  IP_TTL = 2
  MSG_CTRUNC = 8
  MSG_DONTROUTE = 4
  MSG_DONTWAIT = 64
  MSG_EOR = 128
  MSG_OOB = 1
  MSG_PEEK = 2
  MSG_TRUNC = 32
  MSG_WAITALL = 256
  NI_DGRAM = 16
  NI_MAXHOST = 1025
  NI_MAXSERV = 32
  NI_NAMEREQD = 8
  NI_NOFQDN = 4
  NI_NUMERICHOST = 1
  NI_NUMERICSERV = 2
  PF_APPLETALK = 5
  PF_AX25 = 3
  PF_INET = 2
  PF_INET6 = 10
  PF_IPX = 4
  PF_KEY = 15
  PF_LOCAL = 1
  PF_MAX = 34
  PF_ROUTE = 16
  PF_SNA = 22
  PF_UNIX = 1
  PF_UNSPEC = 0
  SHUT_RD = 0
  SHUT_RDWR = 2
  SHUT_WR = 1
  SOCK_DGRAM = 2
  SOCK_PACKET = 10
  SOCK_RAW = 3
  SOCK_RDM = 4
  SOCK_SEQPACKET = 5
  SOCK_STREAM = 1
  SOL_IP = 0
  SOL_SOCKET = 1
  SOL_TCP = 6
  SOL_UDP = 17
  SO_ACCEPTCONN = 30
  SO_ATTACH_FILTER = 26
  SO_BINDTODEVICE = 25
  SO_BROADCAST = 6
  SO_DEBUG = 1
  SO_DETACH_FILTER = 27
  SO_DONTROUTE = 5
  SO_ERROR = 4
  SO_KEEPALIVE = 9
  SO_LINGER = 13
  SO_NO_CHECK = 11
  SO_OOBINLINE = 10
  SO_PASSCRED = 16
  SO_PEERCRED = 17
  SO_PEERNAME = 28
  SO_PRIORITY = 12
  SO_RCVBUF = 8
  SO_RCVLOWAT = 18
  SO_RCVTIMEO = 20
  SO_REUSEADDR = 2
  SO_SECURITY_AUTHENTICATION = 22
  SO_SECURITY_ENCRYPTION_NETWORK = 24
  SO_SECURITY_ENCRYPTION_TRANSPORT = 23
  SO_SNDBUF = 7
  SO_SNDLOWAT = 19
  SO_SNDTIMEO = 21
  SO_TIMESTAMP = 29
  SO_TYPE = 3
  TCP_MAXSEG = 2
  TCP_NODELAY = 1
  include File::Constants
  include Enumerable
  ##% accept : (*!FIXME) -> !FIXME
  def accept(*) end
  ##% accept_nonblock : (*!FIXME) -> !FIXME
  def accept_nonblock(*) end
  ##% bind : (*!FIXME) -> !FIXME
  def bind(p0) end
  ##% connect : (*!FIXME) -> !FIXME
  def connect(p0) end
  ##% connect_nonblock : (*!FIXME) -> !FIXME
  def connect_nonblock(p0) end
  ##% listen : (*!FIXME) -> !FIXME
  def listen(p0) end
  ##% recvfrom : (*!FIXME) -> !FIXME
  def recvfrom(*rest) end
  ##% recvfrom_nonblock : (*!FIXME) -> !FIXME
  def recvfrom_nonblock(*rest) end
  ##% sysaccept : (*!FIXME) -> !FIXME
  def sysaccept(*) end
  ##% Socket.getaddrinfo : (*!FIXME) -> !FIXME
  def Socket.getaddrinfo(*rest) end
  ##% Socket.gethostbyaddr : (*!FIXME) -> !FIXME
  def Socket.gethostbyaddr(*rest) end
  ## returns an array of the canonical host name, a subarray of host
  ## aliases, the address family, and the address portion of the
  ## sockaddr structure.  
  ##% Socket.gethostbyname : String -> (String,Array<String>,Fixnum,String,String,String,String)
  def Socket.gethostbyname(p0) end
  ##% Socket.gethostname : () -> String
  def Socket.gethostname() end
  ##% Socket.getnameinfo : (*!FIXME) -> !FIXME
  def Socket.getnameinfo(*rest) end
  ##% Socket.getservbyname : (*!FIXME) -> !FIXME
  def Socket.getservbyname(*rest) end
  ##% Socket.pack_sockaddr_in : (*!FIXME) -> !FIXME
  def Socket.pack_sockaddr_in(*) end
  ##% Socket.pack_sockaddr_un : (*!FIXME) -> !FIXME
  def Socket.pack_sockaddr_un(p0) end
  ##% Socket.pair : (*!FIXME) -> !FIXME
  def Socket.pair(*) end
  ##% Socket.sockaddr_in : (*!FIXME) -> !FIXME
  def Socket.sockaddr_in(*) end
  ##% Socket.sockaddr_un : (*!FIXME) -> !FIXME
  def Socket.sockaddr_un(p0) end
  ##% Socket.socketpair : (*!FIXME) -> !FIXME
  def Socket.socketpair(*) end
  ##% Socket.unpack_sockaddr_in : (*!FIXME) -> !FIXME
  def Socket.unpack_sockaddr_in(p0) end
  ##% Socket.unpack_sockaddr_un : (*!FIXME) -> !FIXME
  def Socket.unpack_sockaddr_un(p0) end
end

class UNIXSocket < BasicSocket
  include File::Constants
  include Enumerable
  ##% addr : (*!FIXME) -> !FIXME
  def addr(*) end
  ##% path : (*!FIXME) -> !FIXME
  def path(*) end
  ##% peeraddr : (*!FIXME) -> !FIXME
  def peeraddr(*) end
  ##% recv_io : (*!FIXME) -> !FIXME
  def recv_io(*rest) end
  ##% recvfrom : (*!FIXME) -> !FIXME
  def recvfrom(*rest) end
  ##% send_io : (*!FIXME) -> !FIXME
  def send_io(p0) end
  ##% UNIXSocket.pair : (*!FIXME) -> !FIXME
  def UNIXSocket.pair(*rest) end
  ##% UNIXSocket.socketpair : (*!FIXME) -> !FIXME
  def UNIXSocket.socketpair(*rest) end
end

class IPSocket < BasicSocket
  include File::Constants
  include Enumerable
  ##% addr : (*!FIXME) -> !FIXME
  def addr(*) end
  ##% peeraddr : (*!FIXME) -> !FIXME
  def peeraddr(*) end
  ##% recvfrom : (Fixnum,?Fixnum) -> Array<!FIXME>
  def recvfrom(*rest) end
  ##% IPSocket.getaddress : (Fixnum or String) -> String
  def IPSocket.getaddress(p0) end
end

class UNIXServer < UNIXSocket
  include File::Constants
  include Enumerable
  ##% accept : (*!FIXME) -> !FIXME
  def accept(*) end
  ##% accept_nonblock : (*!FIXME) -> !FIXME
  def accept_nonblock(*) end
  ##% listen : (*!FIXME) -> !FIXME
  def listen(p0) end
  ##% sysaccept : (*!FIXME) -> !FIXME
  def sysaccept(*) end
end

class UDPSocket < IPSocket
  include File::Constants
  include Enumerable
  ##% bind : (*!FIXME) -> !FIXME
  def bind(*) end
  ##% connect : (String,Fixnum) -> Fixnum
  def connect(*) end
  ##% initialize : (?Fixnum) -> UDPSocket
  def initialize(*) end
  ##% recvfrom_nonblock : (*!FIXME) -> !FIXME
  def recvfrom_nonblock(*rest) end
  ##% send : (*!FIXME) -> !FIXME
  def send(*rest) end
end

class TCPSocket < IPSocket
  include File::Constants
  include Enumerable
  ##% initialize : (String, Fixnum, ?String, ?Fixnum) -> TCPSocket
  def initialize(rhost,rport) end
  ##% TCPSocket.gethostbyname : (*!FIXME) -> !FIXME
  def TCPSocket.gethostbyname(p0) end
end

class TCPServer < TCPSocket
  include File::Constants
  include Enumerable
  ##% accept : (*!FIXME) -> !FIXME
  def accept(*) end
  ##% accept_nonblock : (*!FIXME) -> !FIXME
  def accept_nonblock(*) end
  ##% listen : (*!FIXME) -> !FIXME
  def listen(p0) end
  ##% sysaccept : (*!FIXME) -> !FIXME
  def sysaccept(*) end
end

IPsocket = IPSocket
TCPserver = TCPServer
TCPsocket = TCPSocket
UDPsocket = UDPSocket
UNIXserver = UNIXServer
UNIXsocket = UNIXSocket
