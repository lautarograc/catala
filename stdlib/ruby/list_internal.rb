# frozen_string_literal: true

# Implementation of the external module List_internal (see
# stdlib/list_internal.catala_en). The OCaml implementation
# (stdlib/ocaml/list_internal.ml) is the reference.

require 'catala_runtime'

module List_internal
  def self.sequence(start, stop)
    stop <= start ? [] : (start...stop).to_a
  end

  # Indexes start at 1
  def self.nth_element(lst, index)
    i = index - 1
    i >= 0 && i < lst.length ? CatalaRuntime::Option.some(lst[i]) : CatalaRuntime::Option::NONE
  end

  def self.remove_nth_element(lst, index)
    i = index - 1
    return lst if i.negative? || i >= lst.length

    lst[0...i] + lst[(i + 1)..]
  end

  def self.reverse(lst)
    lst.reverse
  end
end
