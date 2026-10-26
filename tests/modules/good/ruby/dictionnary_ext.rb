# Implementation of the external module Dictionnary_ext (see
# ../dictionnary_ext.catala_en)

require 'catala_runtime'

module Dictionnary_ext
  class Dictionnary < CatalaRuntime::Value
  end

  EMPTY = Dictionnary.new({}.freeze)

  def self.empty
    EMPTY
  end

  def self.store(dict, key, value)
    Dictionnary.new(dict.value.merge(key => value).freeze)
  end

  def self.find(dict, key)
    dict.value.key?(key) ? CatalaRuntime::Option.some(dict.value[key]) : CatalaRuntime::Option::NONE
  end
end
