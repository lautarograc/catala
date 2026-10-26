# Implementation of the external module Text (see ../text.catala_en)

require 'catala_runtime'

module Text
  class Text < CatalaRuntime::Value
    def to_s
      "\"#{value}\""
    end
  end

  FOO = Text.new('foo\\')
  BAR = Text.new('bąr')
  FORTYTWO = Text.new('42')

  def self.foo
    FOO
  end

  def self.bar
    BAR
  end

  def self.fortytwo
    FORTYTWO
  end

  def self.of_int(x)
    Text.new(x.to_s)
  end
end
