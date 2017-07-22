module Digest
  class Base
    ##% "<<" : (*!FIXME) -> !FIXME
    def <<(*) end
    ##% "==" : (*!FIXME) -> !FIXME
    def ==(*) end
    ##% digest : ?String -> String
    def digest(*) end
    ##% hexdigest : ?String -> String
    def hexdigest(*) end
    ##% to_s : (*!FIXME) -> !FIXME
    def to_s(*) end
    ##% update : (*!FIXME) -> !FIXME
    def update(*) end
    ## MikeF: actually, this can take more arguments, its really closer to
    ## (String,^rest) -> String ; self <= [intialize : (^rest) -> self]
    ## but we don't support nested ^ params
    ##% Base.digest : (String) -> String
    def Base.digest(p0) end
    ##% Base.hexdigest : (*!FIXME) -> !FIXME
    def Base.hexdigest(*) end
  end

end

