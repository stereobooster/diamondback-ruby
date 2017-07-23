module OpenSSL
  class Cipher
    ##% "<<" : (*!FIXME) -> !FIXME
    def <<(p0) end
    ##% block_size : (*!FIXME) -> !FIXME
    def block_size(*) end
    ##% decrypt : (*!FIXME) -> !FIXME
    def decrypt(*rest) end
    ##% encrypt : (*!FIXME) -> !FIXME
    def encrypt(*rest) end
    ##% final : (*!FIXME) -> !FIXME
    def final(*) end
    ##% iv= : (*!FIXME) -> !FIXME
    def iv=(p0) end
    ##% iv_len : (*!FIXME) -> !FIXME
    def iv_len(*) end
    ##% key= : (*!FIXME) -> !FIXME
    def key=(p0) end
    ##% key_len : (*!FIXME) -> !FIXME
    def key_len(*) end
    ##% key_len= : (*!FIXME) -> !FIXME
    def key_len=(p0) end
    ##% name : (*!FIXME) -> !FIXME
    def name(*) end
    ##% padding= : (*!FIXME) -> !FIXME
    def padding=(p0) end
    ##% pkcs5_keyivgen : (*!FIXME) -> !FIXME
    def pkcs5_keyivgen(*rest) end
    ##% reset : (*!FIXME) -> !FIXME
    def reset(*) end
    ##% update : (*!FIXME) -> !FIXME
    def update(p0) end

    ##% Cipher.ciphers : (*!FIXME) -> !FIXME
    def Cipher.ciphers(*) end
  end

  class BN
    ##% "%" : (*!FIXME) -> !FIXME
    def %(p0) end
    ##% "*" : (*!FIXME) -> !FIXME
    def *(p0) end
    ##% "**" : (*!FIXME) -> !FIXME
    def **(p0) end
    ##% "+" : (*!FIXME) -> !FIXME
    def +(p0) end
    ##% "-" : (*!FIXME) -> !FIXME
    def -(p0) end
    ##% "/" : (*!FIXME) -> !FIXME
    def /(p0) end
    ##% "<<" : (*!FIXME) -> !FIXME
    def <<(p0) end
    ##% "<=>" : (*!FIXME) -> !FIXME
    def <=>(p0) end
    ##% "==" : (*!FIXME) -> !FIXME
    def ==(p0) end
    ##% "===" : (*!FIXME) -> !FIXME
    def ===(p0) end
    ##% ">>" : (*!FIXME) -> !FIXME
    def >>(p0) end
    ##% bit_set? : (*!FIXME) -> !FIXME
    def bit_set?(p0) end
    ##% clear_bit! : (*!FIXME) -> !FIXME
    def clear_bit!(p0) end
    ##% cmp : (*!FIXME) -> !FIXME
    def cmp(p0) end
    ##% coerce : (*!FIXME) -> !FIXME
    def coerce(p0) end
    ##% copy : (*!FIXME) -> !FIXME
    def copy(p0) end
    ##% eql? : (*!FIXME) -> !FIXME
    def eql?(p0) end
    ##% gcd : (*!FIXME) -> !FIXME
    def gcd(p0) end
    ##% initialize<t> : String -> t
    def initialize(v) end
    ##% mask_bits! : (*!FIXME) -> !FIXME
    def mask_bits!(p0) end
    ##% mod_add : (*!FIXME) -> !FIXME
    def mod_add(p0, p1) end
    ##% mod_exp : (*!FIXME) -> !FIXME
    def mod_exp(p0, p1) end
    ##% mod_inverse : (*!FIXME) -> !FIXME
    def mod_inverse(p0) end
    ##% mod_mul : (*!FIXME) -> !FIXME
    def mod_mul(p0, p1) end
    ##% mod_sqr : (*!FIXME) -> !FIXME
    def mod_sqr(p0) end
    ##% mod_sub : (*!FIXME) -> !FIXME
    def mod_sub(p0, p1) end
    ##% num_bits : (*!FIXME) -> !FIXME
    def num_bits(*) end
    ##% num_bytes : (*!FIXME) -> !FIXME
    def num_bytes(*) end
    ##% odd? : (*!FIXME) -> !FIXME
    def odd?(*) end
    ##% one? : (*!FIXME) -> !FIXME
    def one?(*) end
    ##% prime? : (*!FIXME) -> !FIXME
    def prime?(*rest) end
    ##% prime_fasttest? : (*!FIXME) -> !FIXME
    def prime_fasttest?(*rest) end
    ##% set_bit! : (*!FIXME) -> !FIXME
    def set_bit!(p0) end
    ##% sqr : (*!FIXME) -> !FIXME
    def sqr(*) end
    ##% to_bn : (*!FIXME) -> !FIXME
    def to_bn(*) end
    ##% to_i : (*!FIXME) -> !FIXME
    def to_i(*) end
    ##% to_int : (*!FIXME) -> !FIXME
    def to_int(*) end
    ##% to_s : (*!FIXME) -> !FIXME
    def to_s(*rest) end
    ##% ucmp : (*!FIXME) -> !FIXME
    def ucmp(p0) end
    ##% zero? : (*!FIXME) -> !FIXME
    def zero?(*) end
    ##% BN.generate_prime : (*!FIXME) -> !FIXME
    def BN.generate_prime(*rest) end
    ##% BN.pseudo_rand : (*!FIXME) -> !FIXME
    def BN.pseudo_rand(*rest) end
    ##% BN.pseudo_rand_range : (*!FIXME) -> !FIXME
    def BN.pseudo_rand_range(p0) end
    ##% BN.rand : (*!FIXME) -> !FIXME
    def BN.rand(*rest) end
    ##% BN.rand_range : (*!FIXME) -> !FIXME
    def BN.rand_range(p0) end
  end

  class Config
    DEFAULT_CONFIG_FILE = "/usr/lib/ssl/openssl.cnf"
    include Enumerable
    ##% "[]" : (*!FIXME) -> !FIXME
    def [](p0) end
    ##% "[]=" : (*!FIXME) -> !FIXME
    def []=(p0, p1) end
    ##% add_value : (*!FIXME) -> !FIXME
    def add_value(p0, p1, p2) end
    ##% each : (*!FIXME) -> !FIXME
    def each(*) end
    ##% get_value : (*!FIXME) -> !FIXME
    def get_value(p0, p1) end
    ##% inspect : (*!FIXME) -> !FIXME
    def inspect(*) end
    ##% section : (*!FIXME) -> !FIXME
    def section(p0) end
    ##% sections : (*!FIXME) -> !FIXME
    def sections(*) end
    ##% to_s : (*!FIXME) -> !FIXME
    def to_s(*) end
    ##% value : (*!FIXME) -> !FIXME
    def value(*rest) end
    ##% Config.parse : (*!FIXME) -> !FIXME
    def Config.parse(p0) end
  end

  OPENSSL_VERSION_NUMBER = 9470079
  VERSION = "2.3.3"
  class HMAC
    ##% "<<" : (*!FIXME) -> !FIXME
    def <<(p0) end
    ##% digest : (*!FIXME) -> !FIXME
    def digest(*) end
    ##% hexdigest : (*!FIXME) -> !FIXME
    def hexdigest(*) end
    ##% inspect : (*!FIXME) -> !FIXME
    def inspect(*) end
    ##% to_s : (*!FIXME) -> !FIXME
    def to_s(*) end
    ##% update : (*!FIXME) -> !FIXME
    def update(p0) end
    ##% HMAC.digest : (*!FIXME) -> !FIXME
    def HMAC.digest(p0, p1, p2) end
    ##% HMAC.hexdigest : (*!FIXME) -> !FIXME
    def HMAC.hexdigest(p0, p1, p2) end
  end

  OPENSSL_VERSION = "OpenSSL 0.9.8g 19 Oct 2007"
  class OpenSSLError < StandardError
  end

  class CipherError < OpenSSL::OpenSSLError
  end

  class PKCS12
    ##% ca_certs : (*!FIXME) -> !FIXME
    def ca_certs(*) end
    ##% certificate : (*!FIXME) -> !FIXME
    def certificate(*) end
    ##% key : (*!FIXME) -> !FIXME
    def key(*) end
    ##% to_der : (*!FIXME) -> !FIXME
    def to_der(*) end

    class PKCS12Error < OpenSSL::OpenSSLError
    end

    ##% PKCS12.create : (*!FIXME) -> !FIXME
    def PKCS12.create(*rest) end
  end

  module SSL
    OP_NO_SSLv2 = 16777216
    VERIFY_PEER = 1
    OP_NETSCAPE_CA_DN_BUG = 536870912
    class SSLSocket
      ##% accept : (*!FIXME) -> !FIXME
      def accept(*) end
      ##% cert : (*!FIXME) -> !FIXME
      def cert(*) end
      ##% cipher : (*!FIXME) -> !FIXME
      def cipher(*) end
      ##% connect : (*!FIXME) -> !FIXME
      def connect(*) end
      ##% context : (*!FIXME) -> !FIXME
      def context(*) end
      ##% io : (*!FIXME) -> !FIXME
      def io(*) end
      ##% initialize : (BaseIO, ?OpenSSL::SSL::SSLContext) -> SSLSocket
      def initialize(*) end
      ##% peer_cert : () -> X509::Certificate
      def peer_cert(*) end
      ##% peer_cert_chain : (*!FIXME) -> !FIXME
      def peer_cert_chain(*) end
      ##% pending : (*!FIXME) -> !FIXME
      def pending(*) end
      ##% state : (*!FIXME) -> !FIXME
      def state(*) end
      ##% sync_close : () -> Boolean
      def sync_close(*) end
      ##% sync_close= : Boolean -> Boolean
      def sync_close=(p0) end
      ##% sysclose : () -> NilClass
      def sysclose(*) end
      ##% sysread : (Fixnum,?String) -> String
      def sysread(*rest) end
      ##% syswrite : String -> Fixnum
      def syswrite(p0) end
      ##% to_io : () -> BaseIO
      def to_io(*) end
    end

    OP_NO_SSLv3 = 33554432
    OP_SINGLE_ECDH_USE = 524288
    OP_PKCS1_CHECK_1 = 134217728
    OP_ALL = 4095
    class SSLContext
      ##% ca_file : (*!FIXME) -> !FIXME
      def ca_file(*) end
      ##% ca_file= : (*!FIXME) -> !FIXME
      def ca_file=(p0) end
      ##% ca_path : (*!FIXME) -> !FIXME
      def ca_path(*) end
      ##% ca_path= : (*!FIXME) -> !FIXME
      def ca_path=(p0) end
      ##% cert : (*!FIXME) -> !FIXME
      def cert(*) end
      ##% cert= : (*!FIXME) -> !FIXME
      def cert=(p0) end
      ##% cert_store : () -> OpenSSL::X509::Store
      def cert_store(*) end
      ##% cert_store= : OpenSSL::X509::Store -> OpenSSL::X509::Store
      def cert_store=(p0) end
      ##% ciphers : (*!FIXME) -> !FIXME
      def ciphers(*) end
      ##% ciphers= : (*!FIXME) -> !FIXME
      def ciphers=(p0) end
      ##% client_ca : (*!FIXME) -> !FIXME
      def client_ca(*) end
      ##% client_ca= : (*!FIXME) -> !FIXME
      def client_ca=(p0) end
      ##% client_cert_cb : (*!FIXME) -> !FIXME
      def client_cert_cb(*) end
      ##% client_cert_cb= : (*!FIXME) -> !FIXME
      def client_cert_cb=(p0) end
      ##% extra_chain_cert : (*!FIXME) -> !FIXME
      def extra_chain_cert(*) end
      ##% extra_chain_cert= : (*!FIXME) -> !FIXME
      def extra_chain_cert=(p0) end
      ##% key : (*!FIXME) -> !FIXME
      def key(*) end
      ##% key= : (*!FIXME) -> !FIXME
      def key=(p0) end
      ##% options : (*!FIXME) -> !FIXME
      def options(*) end
      ##% options= : (*!FIXME) -> !FIXME
      def options=(p0) end
      ##% session_id_context : (*!FIXME) -> !FIXME
      def session_id_context(*) end
      ##% session_id_context= : (*!FIXME) -> !FIXME
      def session_id_context=(p0) end
      ##% timeout : () -> Fixnum
      def timeout(*) end
      ##% timeout= : Fixnum -> Fixnum
      def timeout=(p0) end
      ##% tmp_dh_callback : () -> Proc<^(!FIXME,Fixnum,Fixnum),!FIXME>
      def tmp_dh_callback(*) end
      ##% tmp_dh_callback= : Proc<^(!FIXME,Fixnum,Fixnum),!FIXME> -> Proc<^(!FIXME,Fixnum,Fixnum),!FIXME>
      def tmp_dh_callback=(p0) end
      ##% verify_callback : (*!FIXME) -> !FIXME
      def verify_callback(*) end
      ##% verify_callback= : (*!FIXME) -> !FIXME
      def verify_callback=(p0) end
      ##% verify_depth : (*!FIXME) -> !FIXME
      def verify_depth(*) end
      ##% verify_depth= : (*!FIXME) -> !FIXME
      def verify_depth=(p0) end
      ##% verify_mode : () -> Fixnum
      def verify_mode(*) end
      ##% verify_mode= : Fixnum -> Fixnum
      def verify_mode=(p0) end
    end

    VERIFY_CLIENT_ONCE = 4
    OP_PKCS1_CHECK_2 = 268435456
    OP_EPHEMERAL_RSA = 2097152
    OP_NO_TLSv1 = 67108864
    OP_SINGLE_DH_USE = 1048576
    OP_CIPHER_SERVER_PREFERENCE = 4194304
    class SSLError < OpenSSL::OpenSSLError
    end

    class Session
      class SessionError < OpenSSLError
      end

      ##% "==" : (*!FIXME) -> !FIXME
      def ==(*) end

      ##% initialize<t>: SSLSocket -> t
      ##% initialize<t>: String -> t
      def initialize(x) end

      ##% time= : (*!FIXME) -> !FIXME
      def time=(*) end

      ##% id : (*!FIXME) -> !FIXME
      def id(*) end

      ##% to_text : (*!FIXME) -> !FIXME
      def to_text(*) end

      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end

      ##% timeout : (*!FIXME) -> !FIXME
      def timeout(*) end

      ##% to_pem : (*!FIXME) -> !FIXME
      def to_pem(*) end

      ##% time : (*!FIXME) -> !FIXME
      def time(*) end

      ##% timeout= : (*!FIXME) -> !FIXME
      def timeout=(*) end
    end

    OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION = 65536
    VERIFY_NONE = 0
    OP_TLS_ROLLBACK_BUG = 8388608
    OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG = -1073741824
    VERIFY_FAIL_IF_NO_PEER_CERT = 2
  end

  module Netscape
    class SPKI
      ##% challenge : (*!FIXME) -> !FIXME
      def challenge(*) end
      ##% challenge= : (*!FIXME) -> !FIXME
      def challenge=(p0) end
      ##% public_key : (*!FIXME) -> !FIXME
      def public_key(*) end
      ##% public_key= : (*!FIXME) -> !FIXME
      def public_key=(p0) end
      ##% sign : (*!FIXME) -> !FIXME
      def sign(p0, p1) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% to_pem : (*!FIXME) -> !FIXME
      def to_pem(*) end
      ##% to_s : (*!FIXME) -> !FIXME
      def to_s(*) end
      ##% to_text : (*!FIXME) -> !FIXME
      def to_text(*) end
      ##% verify : (*!FIXME) -> !FIXME
      def verify(p0) end
    end

    class SPKIError < OpenSSL::OpenSSLError
    end

  end

  class Engine
    METHOD_NONE = 0
    METHOD_DH = 4
    METHOD_RSA = 1
    METHOD_ALL = 65535
    METHOD_RAND = 8
    METHOD_CIPHERS = 64
    METHOD_DSA = 2
    METHOD_DIGESTS = 128
    class EngineError < OpenSSL::OpenSSLError
    end

    ##% cipher : (*!FIXME) -> !FIXME
    def cipher(p0) end
    ##% cmds : (*!FIXME) -> !FIXME
    def cmds(*) end
    ##% ctrl_cmd : (*!FIXME) -> !FIXME
    def ctrl_cmd(*rest) end
    ##% digest : (*!FIXME) -> !FIXME
    def digest(p0) end
    ##% finish : (*!FIXME) -> !FIXME
    def finish(*) end
    ##% id : (*!FIXME) -> !FIXME
    def id(*) end
    ##% inspect : (*!FIXME) -> !FIXME
    def inspect(*) end
    ##% load_private_key : (*!FIXME) -> !FIXME
    def load_private_key(*rest) end
    ##% load_public_key : (*!FIXME) -> !FIXME
    def load_public_key(*rest) end
    ##% name : (*!FIXME) -> !FIXME
    def name(*) end
    ##% set_default : (*!FIXME) -> !FIXME
    def set_default(p0) end
    ##% Engine.by_id : (*!FIXME) -> !FIXME
    def Engine.by_id(p0) end
    ##% Engine.cleanup : (*!FIXME) -> !FIXME
    def Engine.cleanup(*) end
    ##% Engine.engines : (*!FIXME) -> !FIXME
    def Engine.engines(*) end
    ##% Engine.load : (*!FIXME) -> !FIXME
    def Engine.load(*rest) end
  end

  module Random
    class RandomError < OpenSSL::OpenSSLError
    end

    ##% egd : (*!FIXME) -> !FIXME
    def egd(p0) end
    ##% egd_bytes : (*!FIXME) -> !FIXME
    def egd_bytes(p0, p1) end
    ##% load_random_file : (*!FIXME) -> !FIXME
    def load_random_file(p0) end
    ##% pseudo_bytes : (*!FIXME) -> !FIXME
    def pseudo_bytes(p0) end
    ##% random_bytes : Fixnum -> String
    def random_bytes(p0) end
    ##% seed : (*!FIXME) -> !FIXME
    def seed(p0) end
    ##% write_random_file : (*!FIXME) -> !FIXME
    def write_random_file(p0) end
    ##% Random.egd : (*!FIXME) -> !FIXME
    def Random.egd(p0) end
    ##% Random.egd_bytes : (*!FIXME) -> !FIXME
    def Random.egd_bytes(p0, p1) end
    ##% Random.load_random_file : (*!FIXME) -> !FIXME
    def Random.load_random_file(p0) end
    ##% Random.pseudo_bytes : (*!FIXME) -> !FIXME
    def Random.pseudo_bytes(p0) end
    ##% Random.random_bytes : Fixnum -> String
    def Random.random_bytes(p0) end
    ##% Random.seed : (*!FIXME) -> !FIXME
    def Random.seed(p0) end
    ##% Random.write_random_file : (*!FIXME) -> !FIXME
    def Random.write_random_file(p0) end
  end

  module ASN1
    T61STRING = 20
    EXTERNAL = 8
    CHARACTER_STRING = 29
    ENUMERATED = 10
    OCTET_STRING = 4
    BOOLEAN = 1
    INTEGER = 2
    SET = 17
    UTF8STRING = 12
    PRINTABLESTRING = 19
    OBJECT = 6
    GENERALSTRING = 27
    BIT_STRING = 3
    EOC = 0
    IA5STRING = 22
    OBJECT_DESCRIPTOR = 7
    BMPSTRING = 30
    UNIVERSAL_TAG_NAME = ["EOC", "BOOLEAN", "INTEGER", "BIT_STRING", "OCTET_STRING", "NULL", "OBJECT", "OBJECT_DESCRIPTOR", "EXTERNAL", "REAL", "ENUMERATED", "EMBEDDED_PDV", "UTF8STRING", "RELATIVE_OID", nil, nil, "SEQUENCE", "SET", "NUMERICSTRING", "PRINTABLESTRING", "T61STRING", "VIDEOTEXSTRING", "IA5STRING", "UTCTIME", "GENERALIZEDTIME", "GRAPHICSTRING", "ISO64STRING", "GENERALSTRING", "UNIVERSALSTRING", "CHARACTER_STRING", "BMPSTRING"]
    SEQUENCE = 16
    RELATIVE_OID = 13
    EMBEDDED_PDV = 11
    VIDEOTEXSTRING = 21
    GRAPHICSTRING = 25
    NULL = 5
    NUMERICSTRING = 18
    REAL = 9
    class ASN1Error < OpenSSL::OpenSSLError
    end

    class ASN1Data
      ##% tag : () -> Fixnum
      def tag(*) end
      ##% tag= : Fixnum -> Fixnum
      def tag=(p0) end
      ##% tag_class : (*!FIXME) -> !FIXME
      def tag_class(*) end
      ##% tag_class= : (*!FIXME) -> !FIXME
      def tag_class=(p0) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% value : () -> Array<ASN1Data>
      def value(*) end
      ##% value= : Array<ASN1Data> -> Array<ASN1Data>
      def value=(p0) end
    end

    GENERALIZEDTIME = 24
    ISO64STRING = 26
    UNIVERSALSTRING = 28
    UTCTIME = 23
    class Constructive < OpenSSL::ASN1::ASN1Data
      include Enumerable
      ##% each : (*!FIXME) -> !FIXME
      def each(*) end
      ##% tagging : (*!FIXME) -> !FIXME
      def tagging(*) end
      ##% tagging= : (*!FIXME) -> !FIXME
      def tagging=(p0) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
    end

    class Sequence < OpenSSL::ASN1::Constructive
      include Enumerable
    end

    class Primitive < OpenSSL::ASN1::ASN1Data
      ##% tagging : (*!FIXME) -> !FIXME
      def tagging(*) end
      ##% tagging= : (*!FIXME) -> !FIXME
      def tagging=(p0) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
    end

    class IA5String < OpenSSL::ASN1::Primitive
    end

    class GeneralString < OpenSSL::ASN1::Primitive
    end

    class VideotexString < OpenSSL::ASN1::Primitive
    end

    class Boolean < OpenSSL::ASN1::Primitive
    end

    class BitString < OpenSSL::ASN1::Primitive
      ##% unused_bits : (*!FIXME) -> !FIXME
      def unused_bits(*) end
      ##% unused_bits= : (*!FIXME) -> !FIXME
      def unused_bits=(p0) end
    end

    class ObjectId < OpenSSL::ASN1::Primitive
      ##% ln : (*!FIXME) -> !FIXME
      def ln(*) end
      ##% long_name : (*!FIXME) -> !FIXME
      def long_name(*) end
      ##% oid : (*!FIXME) -> !FIXME
      def oid(*) end
      ##% short_name : (*!FIXME) -> !FIXME
      def short_name(*) end
      ##% sn : (*!FIXME) -> !FIXME
      def sn(*) end
      ##% ObjectId.register : (*!FIXME) -> !FIXME
      def ObjectId.register(p0, p1, p2) end
    end

    class UTCTime < OpenSSL::ASN1::Primitive
    end

    class Enumerated < OpenSSL::ASN1::Primitive
    end

    class BMPString < OpenSSL::ASN1::Primitive
    end

    class OctetString < OpenSSL::ASN1::Primitive
    end

    class NumericString < OpenSSL::ASN1::Primitive
    end

    class Integer < OpenSSL::ASN1::Primitive
    end

    class Set < OpenSSL::ASN1::Constructive
      include Enumerable
    end

    class PrintableString < OpenSSL::ASN1::Primitive
    end

    class ISO64String < OpenSSL::ASN1::Primitive
    end

    class UTF8String < OpenSSL::ASN1::Primitive
    end

    class GraphicString < OpenSSL::ASN1::Primitive
    end

    class T61String < OpenSSL::ASN1::Primitive
    end

    class GeneralizedTime < OpenSSL::ASN1::Primitive
    end

    class UniversalString < OpenSSL::ASN1::Primitive
    end

    class Null < OpenSSL::ASN1::Primitive
    end

    ##% ASN1.BMPString : (*!FIXME) -> !FIXME
    def ASN1.BMPString(*rest) end
    ##% ASN1.BitString : (*!FIXME) -> !FIXME
    def ASN1.BitString(*rest) end
    ##% ASN1.Boolean : (*!FIXME) -> !FIXME
    def ASN1.Boolean(*rest) end
    ##% ASN1.Enumerated : (*!FIXME) -> !FIXME
    def ASN1.Enumerated(*rest) end
    ##% ASN1.GeneralString : (*!FIXME) -> !FIXME
    def ASN1.GeneralString(*rest) end
    ##% ASN1.GeneralizedTime : (*!FIXME) -> !FIXME
    def ASN1.GeneralizedTime(*rest) end
    ##% ASN1.GraphicString : (*!FIXME) -> !FIXME
    def ASN1.GraphicString(*rest) end
    ##% ASN1.IA5String : (*!FIXME) -> !FIXME
    def ASN1.IA5String(*rest) end
    ##% ASN1.ISO64String : (*!FIXME) -> !FIXME
    def ASN1.ISO64String(*rest) end
    ##% ASN1.Integer : (*!FIXME) -> !FIXME
    def ASN1.Integer(*rest) end
    ##% ASN1.Null : (*!FIXME) -> !FIXME
    def ASN1.Null(*rest) end
    ##% ASN1.NumericString : (*!FIXME) -> !FIXME
    def ASN1.NumericString(*rest) end
    ##% ASN1.ObjectId : (*!FIXME) -> !FIXME
    def ASN1.ObjectId(*rest) end
    ##% ASN1.OctetString : (*!FIXME) -> !FIXME
    def ASN1.OctetString(*rest) end
    ##% ASN1.PrintableString : (*!FIXME) -> !FIXME
    def ASN1.PrintableString(*rest) end
    ##% ASN1.Sequence : (*!FIXME) -> !FIXME
    def ASN1.Sequence(*rest) end
    ##% ASN1.Set : (*!FIXME) -> !FIXME
    def ASN1.Set(*rest) end
    ##% ASN1.T61String : (*!FIXME) -> !FIXME
    def ASN1.T61String(*rest) end
    ##% ASN1.UTCTime : (*!FIXME) -> !FIXME
    def ASN1.UTCTime(*rest) end
    ##% ASN1.UTF8String : (*!FIXME) -> !FIXME
    def ASN1.UTF8String(*rest) end
    ##% ASN1.UniversalString : (*!FIXME) -> !FIXME
    def ASN1.UniversalString(*rest) end
    ##% ASN1.VideotexString : (*!FIXME) -> !FIXME
    def ASN1.VideotexString(*rest) end
    ##% ASN1.decode : String -> OpenSSL::ASN1::ASN1Data
    def ASN1.decode(p0) end
    ##% ASN1.decode_all : (*!FIXME) -> !FIXME
    def ASN1.decode_all(p0) end
    ##% ASN1.traverse : (*!FIXME) -> !FIXME
    def ASN1.traverse(p0) end
  end

  class HMACError < OpenSSL::OpenSSLError
  end

  class PKCS7
    DETACHED = 64
    NOCHAIN = 8
    NOATTR = 256
    class RecipientInfo
      ##% enc_key : (*!FIXME) -> !FIXME
      def enc_key(*) end
      ##% issuer : (*!FIXME) -> !FIXME
      def issuer(*) end
      ##% serial : (*!FIXME) -> !FIXME
      def serial(*) end
    end

    BINARY = 128
    TEXT = 1
    class SignerInfo
      ##% issuer : (*!FIXME) -> !FIXME
      def issuer(*) end
      ##% name : (*!FIXME) -> !FIXME
      def name(*) end
      ##% serial : (*!FIXME) -> !FIXME
      def serial(*) end
      ##% signed_time : (*!FIXME) -> !FIXME
      def signed_time(*) end
    end

    Signer = OpenSSL::PKCS7::SignerInfo
    class PKCS7
      ##% add_certificate : (*!FIXME) -> !FIXME
      def add_certificate(p0) end
      ##% add_crl : (*!FIXME) -> !FIXME
      def add_crl(p0) end
      ##% add_data : (*!FIXME) -> !FIXME
      def add_data(p0) end
      ##% add_recipient : (*!FIXME) -> !FIXME
      def add_recipient(p0) end
      ##% add_signer : (*!FIXME) -> !FIXME
      def add_signer(p0) end
      ##% certificates : (*!FIXME) -> !FIXME
      def certificates(*) end
      ##% certificates= : (*!FIXME) -> !FIXME
      def certificates=(p0) end
      ##% cipher= : (*!FIXME) -> !FIXME
      def cipher=(p0) end
      ##% crls : (*!FIXME) -> !FIXME
      def crls(*) end
      ##% crls= : (*!FIXME) -> !FIXME
      def crls=(p0) end
      ##% data : (*!FIXME) -> !FIXME
      def data(*) end
      ##% data= : (*!FIXME) -> !FIXME
      def data=(p0) end
      ##% decrypt : (*!FIXME) -> !FIXME
      def decrypt(*rest) end
      ##% detached : (*!FIXME) -> !FIXME
      def detached(*) end
      ##% detached= : (*!FIXME) -> !FIXME
      def detached=(p0) end
      ##% detached? : (*!FIXME) -> !FIXME
      def detached?(*) end
      ##% error_string : (*!FIXME) -> !FIXME
      def error_string(*) end
      ##% error_string= : (*!FIXME) -> !FIXME
      def error_string=(p0) end
      ##% recipients : (*!FIXME) -> !FIXME
      def recipients(*) end
      ##% signers : (*!FIXME) -> !FIXME
      def signers(*) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% to_pem : (*!FIXME) -> !FIXME
      def to_pem(*) end
      ##% to_s : (*!FIXME) -> !FIXME
      def to_s(*) end
      ##% type : (*!FIXME) -> !FIXME
      def type(*) end
      ##% type= : (*!FIXME) -> !FIXME
      def type=(p0) end
      ##% verify : (*!FIXME) -> !FIXME
      def verify(*rest) end
    end

    NOSMIMECAP = 512
    class PKCS7Error < OpenSSL::OpenSSLError
    end

    NOCERTS = 1
    NOSIGS = 4
    NOINTERN = 2
    NOVERIFY = 16
    ##% PKCS7.encrypt : (*!FIXME) -> !FIXME
    def PKCS7.encrypt(*rest) end
    ##% PKCS7.read_smime : (*!FIXME) -> !FIXME
    def PKCS7.read_smime(p0) end
    ##% PKCS7.sign : (*!FIXME) -> !FIXME
    def PKCS7.sign(*rest) end
    ##% PKCS7.write_smime : (*!FIXME) -> !FIXME
    def PKCS7.write_smime(*rest) end
  end

  module OCSP
    RESPID_KEY = 1024
    class CertificateId
      ##% cmp : (*!FIXME) -> !FIXME
      def cmp(p0) end
      ##% cmp_issuer : (*!FIXME) -> !FIXME
      def cmp_issuer(p0) end
      ##% serial : (*!FIXME) -> !FIXME
      def serial(*) end
    end

    NOCHAIN = 8
    V_RESPID_NAME = 0
    RESPONSE_STATUS_TRYLATER = 3
    class OCSPError < OpenSSL::OpenSSLError
    end

    NOCHECKS = 256
    RESPONSE_STATUS_SIGREQUIRED = 5
    RESPONSE_STATUS_SUCCESSFUL = 0
    V_RESPID_KEY = 1
    TRUSTOTHER = 512
    REVOKED_STATUS_KEYCOMPROMISE = 1
    RESPONSE_STATUS_UNAUTHORIZED = 6
    V_CERTSTATUS_UNKNOWN = 2
    NOTIME = 2048
    RESPONSE_STATUS_MALFORMEDREQUEST = 1
    REVOKED_STATUS_CACOMPROMISE = 2
    REVOKED_STATUS_UNSPECIFIED = 0
    class Response
      ##% basic : (*!FIXME) -> !FIXME
      def basic(*) end
      ##% status : (*!FIXME) -> !FIXME
      def status(*) end
      ##% status_string : (*!FIXME) -> !FIXME
      def status_string(*) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% Response.create : (*!FIXME) -> !FIXME
      def Response.create(p0, p1) end
    end

    NOCASIGN = 64
    REVOKED_STATUS_SUPERSEDED = 4
    REVOKED_STATUS_REMOVEFROMCRL = 8
    V_CERTSTATUS_GOOD = 0
    NOCERTS = 1
    NOSIGS = 4
    class Request
      ##% add_certid : (*!FIXME) -> !FIXME
      def add_certid(p0) end
      ##% add_nonce : (*!FIXME) -> !FIXME
      def add_nonce(*rest) end
      ##% certid : (*!FIXME) -> !FIXME
      def certid(*) end
      ##% check_nonce : (*!FIXME) -> !FIXME
      def check_nonce(p0) end
      ##% sign : (*!FIXME) -> !FIXME
      def sign(*rest) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% verify : (*!FIXME) -> !FIXME
      def verify(*rest) end
    end

    NODELEGATED = 128
    class BasicResponse
      ##% add_nonce : (*!FIXME) -> !FIXME
      def add_nonce(*rest) end
      ##% add_status : (*!FIXME) -> !FIXME
      def add_status(p0, p1, p2, p3, p4, p5, p6) end
      ##% copy_nonce : (*!FIXME) -> !FIXME
      def copy_nonce(p0) end
      ##% sign : (*!FIXME) -> !FIXME
      def sign(*rest) end
      ##% status : (*!FIXME) -> !FIXME
      def status(*) end
      ##% verify : (*!FIXME) -> !FIXME
      def verify(*rest) end
    end

    NOINTERN = 2
    NOVERIFY = 16
    V_CERTSTATUS_REVOKED = 1
    REVOKED_STATUS_NOSTATUS = -1
    REVOKED_STATUS_AFFILIATIONCHANGED = 3
    REVOKED_STATUS_CESSATIONOFOPERATION = 5
    REVOKED_STATUS_CERTIFICATEHOLD = 6
    RESPONSE_STATUS_INTERNALERROR = 2
    NOEXPLICIT = 32
  end

  class ConfigError < OpenSSL::OpenSSLError
  end

  module X509
    V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE = 21
    V_ERR_CERT_NOT_YET_VALID = 9
    V_ERR_SUBJECT_ISSUER_MISMATCH = 29
    TRUST_OCSP_SIGN = 6
    class Name
      OBJECT_TYPE_TEMPLATE = {"serialNumber"=>19, "domainComponent"=>22, "C"=>19, "dnQualifier"=>19, "countryName"=>19, "emailAddress"=>22, "DC"=>22}
      ONELINE = 8520479
      RFC2253 = 17892119
      MULTILINE = 4
      DEFAULT_OBJECT_TYPE = 12
      COMPAT = 0
      ##% "<=>" : (*!FIXME) -> !FIXME
      def <=>(p0) end
      ##% add_entry : (*!FIXME) -> !FIXME
      def add_entry(*rest) end
      ##% cmp : (*!FIXME) -> !FIXME
      def cmp(p0) end
      ##% eql? : (*!FIXME) -> !FIXME
      def eql?(p0) end
      ##% hash : (*!FIXME) -> !FIXME
      def hash(*) end
      ##% initialize<t> : (Array,?Hash<String,Fixnum>) -> t
      ##% initialize<t> : ?String -> t
      def initialize() end
      ##% to_a : (*!FIXME) -> !FIXME
      def to_a(*) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% to_s : (*!FIXME) -> !FIXME
      def to_s(*rest) end
    end

    PURPOSE_OCSP_HELPER = 8
    class Store
      ##% add_cert : (*!FIXME) -> !FIXME
      def add_cert(p0) end
      ##% add_crl : (*!FIXME) -> !FIXME
      def add_crl(p0) end
      ##% add_file : (*!FIXME) -> !FIXME
      def add_file(p0) end
      ##% add_path : (*!FIXME) -> !FIXME
      def add_path(p0) end
      ##% chain : (*!FIXME) -> !FIXME
      def chain(*) end
      ##% error : (*!FIXME) -> !FIXME
      def error(*) end
      ##% error_string : (*!FIXME) -> !FIXME
      def error_string(*) end
      ##% flags= : Fixnum -> Fixnum
      def flags=(p0) end
      ##% purpose= : (*!FIXME) -> !FIXME
      def purpose=(p0) end
      ##% set_default_paths : (*!FIXME) -> !FIXME
      def set_default_paths(*) end
      ##% time= : (*!FIXME) -> !FIXME
      def time=(p0) end
      ##% trust= : (*!FIXME) -> !FIXME
      def trust=(p0) end
      ##% verify : (*!FIXME) -> !FIXME
      def verify(*rest) end
      ##% verify_callback : (*!FIXME) -> !FIXME
      def verify_callback(*) end
      ##% verify_callback= : (*!FIXME) -> !FIXME
      def verify_callback=(p0) end
    end

    V_ERR_PATH_LENGTH_EXCEEDED = 25
    V_ERR_INVALID_PURPOSE = 26
    class Attribute
      ##% oid : (*!FIXME) -> !FIXME
      def oid(*) end
      ##% oid= : (*!FIXME) -> !FIXME
      def oid=(p0) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% value : (*!FIXME) -> !FIXME
      def value(*) end
      ##% value= : (*!FIXME) -> !FIXME
      def value=(p0) end
    end

    TRUST_SSL_CLIENT = 2
    V_ERR_OUT_OF_MEM = 17
    class RequestError < OpenSSL::OpenSSLError
    end

    class StoreContext
      ##% chain : (*!FIXME) -> !FIXME
      def chain(*) end
      ##% cleanup : (*!FIXME) -> !FIXME
      def cleanup(*) end
      ##% current_cert : (*!FIXME) -> !FIXME
      def current_cert(*) end
      ##% current_crl : (*!FIXME) -> !FIXME
      def current_crl(*) end
      ##% error : (*!FIXME) -> !FIXME
      def error(*) end
      ##% error= : (*!FIXME) -> !FIXME
      def error=(p0) end
      ##% error_depth : (*!FIXME) -> !FIXME
      def error_depth(*) end
      ##% error_string : (*!FIXME) -> !FIXME
      def error_string(*) end
      ##% flags= : (*!FIXME) -> !FIXME
      def flags=(p0) end
      ##% purpose= : (*!FIXME) -> !FIXME
      def purpose=(p0) end
      ##% time= : (*!FIXME) -> !FIXME
      def time=(p0) end
      ##% trust= : (*!FIXME) -> !FIXME
      def trust=(p0) end
      ##% verify : (*!FIXME) -> !FIXME
      def verify(*) end
    end

    V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE = 5
    V_ERR_UNABLE_TO_GET_ISSUER_CERT = 2
    V_ERR_CERT_UNTRUSTED = 27
    V_ERR_UNABLE_TO_GET_CRL = 3
    V_ERR_CERT_CHAIN_TOO_LONG = 22
    PURPOSE_CRL_SIGN = 6
    V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD = 16
    V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD = 14
    V_ERR_INVALID_CA = 24
    TRUST_COMPAT = 1
    class CertificateError < OpenSSL::OpenSSLError
    end

    DEFAULT_CERT_AREA = "/usr/lib/ssl"
    V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY = 6
    V_ERR_SELF_SIGNED_CERT_IN_CHAIN = 19
    V_ERR_APPLICATION_VERIFICATION = 50
    PURPOSE_SMIME_SIGN = 4
    V_ERR_AKID_SKID_MISMATCH = 30
    V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD = 13
    V_ERR_CERT_SIGNATURE_FAILURE = 7
    class CRLError < OpenSSL::OpenSSLError
    end

    class CRL
      ##% add_extension : (*!FIXME) -> !FIXME
      def add_extension(p0) end
      ##% add_revoked : (*!FIXME) -> !FIXME
      def add_revoked(p0) end
      ##% extensions : (*!FIXME) -> !FIXME
      def extensions(*) end
      ##% extensions= : (*!FIXME) -> !FIXME
      def extensions=(p0) end
      ##% issuer : (*!FIXME) -> !FIXME
      def issuer(*) end
      ##% issuer= : (*!FIXME) -> !FIXME
      def issuer=(p0) end
      ##% last_update : (*!FIXME) -> !FIXME
      def last_update(*) end
      ##% last_update= : (*!FIXME) -> !FIXME
      def last_update=(p0) end
      ##% next_update : (*!FIXME) -> !FIXME
      def next_update(*) end
      ##% next_update= : (*!FIXME) -> !FIXME
      def next_update=(p0) end
      ##% revoked : (*!FIXME) -> !FIXME
      def revoked(*) end
      ##% revoked= : (*!FIXME) -> !FIXME
      def revoked=(p0) end
      ##% sign : (*!FIXME) -> !FIXME
      def sign(p0, p1) end
      ##% signature_algorithm : (*!FIXME) -> !FIXME
      def signature_algorithm(*) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% to_pem : (*!FIXME) -> !FIXME
      def to_pem(*) end
      ##% to_s : (*!FIXME) -> !FIXME
      def to_s(*) end
      ##% to_text : (*!FIXME) -> !FIXME
      def to_text(*) end
      ##% verify : (*!FIXME) -> !FIXME
      def verify(p0) end
      ##% version : (*!FIXME) -> !FIXME
      def version(*) end
      ##% version= : (*!FIXME) -> !FIXME
      def version=(p0) end
    end

    V_ERR_CRL_SIGNATURE_FAILURE = 8
    DEFAULT_PRIVATE_DIR = "/usr/lib/ssl/private"
    class ExtensionFactory
      ##% config : (*!FIXME) -> !FIXME
      def config(*) end
      ##% config= : (*!FIXME) -> !FIXME
      def config=(p0) end
      ##% create_ext : (*?) -> ?
      def create_ext(*rest) end
      ##% crl : (*!FIXME) -> !FIXME
      def crl(*) end
      ##% crl= : (*!FIXME) -> !FIXME
      def crl=(p0) end
      ##% initialize<t> : (?!FIXME,?Certificate,?!FIXME,?!FIXME) -> t
      def initialize(a=nil,b=nil,c=nil,d=nil) end
      ##% issuer_certificate : (*!FIXME) -> !FIXME
      def issuer_certificate(*) end
      ##% issuer_certificate= : (*!FIXME) -> !FIXME
      def issuer_certificate=(p0) end
      ##% subject_certificate : (*!FIXME) -> !FIXME
      def subject_certificate(*) end
      ##% subject_certificate= : (*!FIXME) -> !FIXME
      def subject_certificate=(p0) end
      ##% subject_request : (*!FIXME) -> !FIXME
      def subject_request(*) end
      ##% subject_request= : (*!FIXME) -> !FIXME
      def subject_request=(p0) end
    end

    PURPOSE_SSL_SERVER = 2
    V_ERR_AKID_ISSUER_SERIAL_MISMATCH = 31
    V_ERR_CERT_HAS_EXPIRED = 10
    V_FLAG_CRL_CHECK_ALL = 8
    DEFAULT_CERT_DIR = "/usr/lib/ssl/certs"
    class RevokedError < OpenSSL::OpenSSLError
    end

    V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD = 15
    V_ERR_CERT_REJECTED = 28
    PURPOSE_NS_SSL_SERVER = 3
    TRUST_OCSP_REQUEST = 7
    V_OK = 0
    class Revoked
      ##% add_extension : (*!FIXME) -> !FIXME
      def add_extension(p0) end
      ##% extensions : (*!FIXME) -> !FIXME
      def extensions(*) end
      ##% extensions= : (*!FIXME) -> !FIXME
      def extensions=(p0) end
      ##% serial : (*!FIXME) -> !FIXME
      def serial(*) end
      ##% serial= : (*!FIXME) -> !FIXME
      def serial=(p0) end
      ##% time : (*!FIXME) -> !FIXME
      def time(*) end
      ##% time= : (*!FIXME) -> !FIXME
      def time=(p0) end
    end

    DEFAULT_CERT_FILE = "/usr/lib/ssl/cert.pem"
    V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT = 18
    TRUST_OBJECT_SIGN = 5
    TRUST_EMAIL = 4
    V_FLAG_CRL_CHECK = 4
    PURPOSE_SSL_CLIENT = 1
    DEFAULT_CERT_DIR_ENV = "SSL_CERT_DIR"
    DEFAULT_CERT_FILE_ENV = "SSL_CERT_FILE"
    V_ERR_CRL_HAS_EXPIRED = 12
    PURPOSE_ANY = 7
    V_ERR_KEYUSAGE_NO_CERTSIGN = 32
    class NameError < OpenSSL::OpenSSLError
    end

    V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE = 4
    class ExtensionError < OpenSSL::OpenSSLError
    end

    PURPOSE_SMIME_ENCRYPT = 5
    TRUST_SSL_SERVER = 3
    class Certificate
      ##% add_extension : (*!FIXME) -> !FIXME
      def add_extension(p0) end
      ##% check_private_key : (*!FIXME) -> !FIXME
      def check_private_key(p0) end
      ##% extensions : () -> Array<OpenSSL::X509::Extension>
      def extensions(*) end
      ##% extensions= : Array<OpenSSL::X509::Extension> -> Array<OpenSSL::X509::Extension>
      def extensions=(p0) end
      ##% initialize<t> : () -> t
      ##% initialize<t> : String -> t
      def initialize(z="") end
      ##% inspect : (*!FIXME) -> !FIXME
      def inspect(*) end
      ##% issuer : (*!FIXME) -> !FIXME
      def issuer(*) end
      ##% issuer= : (*!FIXME) -> !FIXME
      def issuer=(p0) end
      ##% not_after : () -> Time
      def not_after(*) end
      ##% not_after= : Time -> Time
      def not_after=(p0) end
      ##% not_before : () -> Time
      def not_before(*) end
      ##% not_before= : Time -> Time
      def not_before=(p0) end
      ##% public_key : () -> OpenSSL::PKey::PKey
      def public_key(*) end
      ##% public_key= : OpenSSL::PKey::PKey -> OpenSSL::PKey::PKey
      def public_key=(p0) end
      ##% serial : () -> Fixnum
      def serial(*) end
      ##% serial= : Fixnum -> Fixnum
      def serial=(p0) end
      ##% sign : (*!FIXME) -> !FIXME
      def sign(p0, p1) end
      ##% signature_algorithm : (*!FIXME) -> !FIXME
      def signature_algorithm(*) end
      ##% subject : () -> String
      def subject(*) end
      ##% subject= : String -> String
      def subject=(p0) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% to_pem : (*!FIXME) -> !FIXME
      def to_pem(*) end
      ##% to_s : (*!FIXME) -> !FIXME
      def to_s(*) end
      ##% to_text : (*!FIXME) -> !FIXME
      def to_text(*) end
      ##% verify : (*!FIXME) -> !FIXME
      def verify(p0) end
      ##% version : () -> Fixnum
      def version(*) end
      ##% version= : Fixnum -> Fixnum
      def version=(p0) end
    end

    class AttributeError < OpenSSL::OpenSSLError
    end

    class StoreError < OpenSSL::OpenSSLError
    end

    V_ERR_CERT_REVOKED = 23
    class Extension
      ##% critical= : (*!FIXME) -> !FIXME
      def critical=(p0) end
      ##% critical? : (*!FIXME) -> !FIXME
      def critical?(*) end
      ##% oid : (*!FIXME) -> !FIXME
      def oid(*) end
      ##% oid= : (*!FIXME) -> !FIXME
      def oid=(p0) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% value : (*!FIXME) -> !FIXME
      def value(*) end
      ##% value= : (*!FIXME) -> !FIXME
      def value=(p0) end
    end

    class Request
      ##% add_attribute : (*!FIXME) -> !FIXME
      def add_attribute(p0) end
      ##% attributes : (*!FIXME) -> !FIXME
      def attributes(*) end
      ##% attributes= : (*!FIXME) -> !FIXME
      def attributes=(p0) end
      ##% public_key : () -> !FIXME
      def public_key(*) end
      ##% public_key= : (*!FIXME) -> !FIXME
      def public_key=(p0) end
      ##% sign : (*!FIXME) -> !FIXME
      def sign(p0, p1) end
      ##% signature_algorithm : (*!FIXME) -> !FIXME
      def signature_algorithm(*) end
      ##% subject : (*!FIXME) -> !FIXME
      def subject(*) end
      ##% subject= : (*!FIXME) -> !FIXME
      def subject=(p0) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% to_pem : (*!FIXME) -> !FIXME
      def to_pem(*) end
      ##% to_s : (*!FIXME) -> !FIXME
      def to_s(*) end
      ##% to_text : (*!FIXME) -> !FIXME
      def to_text(*) end
      ##% verify : (*!FIXME) -> !FIXME
      def verify(p0) end
      ##% version : (*!FIXME) -> !FIXME
      def version(*) end
      ##% version= : (*!FIXME) -> !FIXME
      def version=(p0) end
    end

    V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY = 20
    V_ERR_CRL_NOT_YET_VALID = 11
  end

  class BNError < OpenSSL::OpenSSLError
  end

  class Digest
    class DigestError < OpenSSL::OpenSSLError
    end

    ##% "<<" : (*!FIXME) -> !FIXME
    def <<(p0) end
    ##% "==" : (*!FIXME) -> !FIXME
    def ==(p0) end
    ##% digest : (String,String) -> String
    def digest(s1,s2) end
    ##% hexdigest : (*!FIXME) -> !FIXME
    def hexdigest(*) end
    ##% initialize : (String,?String) -> NilClass
    def initialize(s) end
    ##% inspect : (*!FIXME) -> !FIXME
    def inspect(*) end
    ##% name : (*!FIXME) -> !FIXME
    def name(*) end
    ##% reset : (*!FIXME) -> !FIXME
    def reset(*) end
    ##% size : (*!FIXME) -> !FIXME
    def size(*) end
    ##% to_s : (*!FIXME) -> !FIXME
    def to_s(*) end
    ##% update : (*!FIXME) -> !FIXME
    def update(p0) end
    ##% Digest.digest : (String,String) -> String
    def Digest.digest(p0, p1) end
    ##% Digest.hexdigest : (String,String) -> String
    def Digest.hexdigest(p0, p1) end
  end

  module PKey
    class PKeyError < OpenSSL::OpenSSLError
    end

    class PKey
      ##% sign : (OpenSSL::Digest,String) -> String
      def sign(p0, p1) end
      ##% verify : (*!FIXME) -> !FIXME
      def verify(p0, p1, p2) end
    end

    class RSA < OpenSSL::PKey::PKey
      NO_PADDING = 3
      PKCS1_PADDING = 1
      SSLV23_PADDING = 2
      PKCS1_OAEP_PADDING = 4
      ##% d : (*!FIXME) -> !FIXME
      def d(*) end
      ##% d= : (*!FIXME) -> !FIXME
      def d=(p0) end
      ##% dmp1 : (*!FIXME) -> !FIXME
      def dmp1(*) end
      ##% dmp1= : (*!FIXME) -> !FIXME
      def dmp1=(p0) end
      ##% dmq1 : (*!FIXME) -> !FIXME
      def dmq1(*) end
      ##% dmq1= : (*!FIXME) -> !FIXME
      def dmq1=(p0) end
      ##% e : (*!FIXME) -> !FIXME
      def e(*) end
      ##% e= : (*!FIXME) -> !FIXME
      def e=(p0) end
      ##% export : (*!FIXME) -> !FIXME
      def export(*rest) end
      ##% initialize<t>: () -> t
      ##% initialize<t>: (Fixnum,?String) -> t
      ##% initialize<t>: (String,?String) -> t
      def initialize(a=nil,b=nil) end
      ##% iqmp : (*!FIXME) -> !FIXME
      def iqmp(*) end
      ##% iqmp= : (*!FIXME) -> !FIXME
      def iqmp=(p0) end
      ##% n : (*!FIXME) -> !FIXME
      def n(*) end
      ##% n= : (*!FIXME) -> !FIXME
      def n=(p0) end
      ##% p : (*!FIXME) -> !FIXME
      def p(*) end
      ##% p= : (*!FIXME) -> !FIXME
      def p=(p0) end
      ##% params : (*!FIXME) -> !FIXME
      def params(*) end
      ##% private? : (*!FIXME) -> !FIXME
      def private?(*) end
      ##% private_decrypt : (*!FIXME) -> !FIXME
      def private_decrypt(*rest) end
      ##% private_encrypt : (*!FIXME) -> !FIXME
      def private_encrypt(*rest) end
      ##% public? : (*!FIXME) -> !FIXME
      def public?(*) end
      ##% public_decrypt : (*!FIXME) -> !FIXME
      def public_decrypt(*rest) end
      ##% public_encrypt : (*!FIXME) -> !FIXME
      def public_encrypt(*rest) end
      ##% public_key : (*!FIXME) -> !FIXME
      def public_key(*) end
      ##% q : (*!FIXME) -> !FIXME
      def q(*) end
      ##% q= : (*!FIXME) -> !FIXME
      def q=(p0) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% to_pem : (*!FIXME) -> !FIXME
      def to_pem(*rest) end
      ##% to_s : (*!FIXME) -> !FIXME
      def to_s(*rest) end
      ##% to_text : (*!FIXME) -> !FIXME
      def to_text(*) end
      ##% RSA.generate : (*!FIXME) -> !FIXME
      def RSA.generate(*rest) end
    end

    class RSAError < OpenSSL::PKey::PKeyError
    end

    class DH < OpenSSL::PKey::PKey
      ##% compute_key : (*!FIXME) -> !FIXME
      def compute_key(p0) end
      ##% export : (*!FIXME) -> !FIXME
      def export(*) end
      ##% g : (*!FIXME) -> !FIXME
      def g(*) end
      ##% g= : (*!FIXME) -> !FIXME
      def g=(p0) end
      ##% generate_key! : (*!FIXME) -> !FIXME
      def generate_key!(*) end
      ##% p : (*!FIXME) -> !FIXME
      def p(*) end
      ##% p= : (*!FIXME) -> !FIXME
      def p=(p0) end
      ##% params : (*!FIXME) -> !FIXME
      def params(*) end
      ##% params_ok? : (*!FIXME) -> !FIXME
      def params_ok?(*) end
      ##% priv_key : (*!FIXME) -> !FIXME
      def priv_key(*) end
      ##% priv_key= : (*!FIXME) -> !FIXME
      def priv_key=(p0) end
      ##% private? : (*!FIXME) -> !FIXME
      def private?(*) end
      ##% pub_key : (*!FIXME) -> !FIXME
      def pub_key(*) end
      ##% pub_key= : (*!FIXME) -> !FIXME
      def pub_key=(p0) end
      ##% public? : (*!FIXME) -> !FIXME
      def public?(*) end
      ##% public_key : (*!FIXME) -> !FIXME
      def public_key(*) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% to_pem : (*!FIXME) -> !FIXME
      def to_pem(*) end
      ##% to_s : (*!FIXME) -> !FIXME
      def to_s(*) end
      ##% to_text : (*!FIXME) -> !FIXME
      def to_text(*) end
      ##% DH.generate : (*!FIXME) -> !FIXME
      def DH.generate(*rest) end
    end

    class DSAError < OpenSSL::PKey::PKeyError
    end

    class DHError < OpenSSL::PKey::PKeyError
    end

    class DSA < OpenSSL::PKey::PKey
      ##% export : (*!FIXME) -> !FIXME
      def export(*rest) end
      ##% g : (*!FIXME) -> !FIXME
      def g(*) end
      ##% g= : (*!FIXME) -> !FIXME
      def g=(p0) end
      ##% p : (*!FIXME) -> !FIXME
      def p(*) end
      ##% p= : (*!FIXME) -> !FIXME
      def p=(p0) end
      ##% params : (*!FIXME) -> !FIXME
      def params(*) end
      ##% priv_key : (*!FIXME) -> !FIXME
      def priv_key(*) end
      ##% priv_key= : (*!FIXME) -> !FIXME
      def priv_key=(p0) end
      ##% private? : (*!FIXME) -> !FIXME
      def private?(*) end
      ##% pub_key : (*!FIXME) -> !FIXME
      def pub_key(*) end
      ##% pub_key= : (*!FIXME) -> !FIXME
      def pub_key=(p0) end
      ##% public? : (*!FIXME) -> !FIXME
      def public?(*) end
      ##% public_key : (*!FIXME) -> !FIXME
      def public_key(*) end
      ##% q : (*!FIXME) -> !FIXME
      def q(*) end
      ##% q= : (*!FIXME) -> !FIXME
      def q=(p0) end
      ##% syssign : (*!FIXME) -> !FIXME
      def syssign(p0) end
      ##% sysverify : (*!FIXME) -> !FIXME
      def sysverify(p0, p1) end
      ##% to_der : (*!FIXME) -> !FIXME
      def to_der(*) end
      ##% to_pem : (*!FIXME) -> !FIXME
      def to_pem(*rest) end
      ##% to_s : (*!FIXME) -> !FIXME
      def to_s(*rest) end
      ##% to_text : (*!FIXME) -> !FIXME
      def to_text(*) end
      ##% DSA.generate : (*!FIXME) -> !FIXME
      def DSA.generate(p0) end
    end
  end

  ##% OpenSSL.debug : (*!FIXME) -> !FIXME
  def OpenSSL.debug(*) end
  ##% OpenSSL.debug= : (*!FIXME) -> !FIXME
  def OpenSSL.debug=(p0) end
  ##% OpenSSL.errors : (*!FIXME) -> !FIXME
  def OpenSSL.errors(*) end
end
