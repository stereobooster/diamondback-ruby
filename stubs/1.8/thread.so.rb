
class Thread
  ##% exclusive: (*!FIXME) -> !FIXME
  def exclusive(*)
  end
end

class Mutex

  ##% exclusive_unlock: () {() -> !FIXME} -> !FIXME
  def exclusive_unlock() end

  ##% lock : () -> !FIXME
  def lock() end

  ##% locked? : () -> Boolean
  def locked?() end

  ##% synchronize: () {() -> NilClass} -> NilClass
  def synchronize() end

  ##% try_lock : () -> Boolean
  def try_lock() end

  ##% unlock: () -> !FIXME
  def unlock() end
end
