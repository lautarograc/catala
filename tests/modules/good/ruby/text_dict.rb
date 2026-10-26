# Implementation of the external module Text_dict (see ../text_dict.catala_en)

require 'catala_runtime'
require_relative 'Text'
require_relative 'Mod_def'

module Text_dict
  class Text_dict < CatalaRuntime::Value
  end

  EMPTY = Text_dict.new({}.freeze)

  def self.empty
    EMPTY
  end

  def self.store(dict, key, value)
    Text_dict.new(dict.value.merge(key => value).freeze)
  end

  def self.find(dict, key)
    dict.value.key?(key) ? CatalaRuntime::Option.some(dict.value[key]) : CatalaRuntime::Option::NONE
  end
end
