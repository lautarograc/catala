# frozen_string_literal: true

# Implementation of the external module Money_internal (see
# stdlib/money_internal.catala_en). The OCaml implementation
# (stdlib/ocaml/money_internal.ml) is the reference.

require 'catala_runtime'

module Money_internal
  def self.round_to_decimal(variable, nth_decimal)
    return variable if nth_decimal >= 2

    if nth_decimal == 1
      rounded = CatalaRuntime.money_round(CatalaRuntime::Money.new(variable.cents * 10))
      CatalaRuntime::Money.new(CatalaRuntime.truncated_div(rounded.cents, 10))
    else
      pow10 = 10**-nth_decimal
      rounded = CatalaRuntime.money_round(CatalaRuntime::Money.new(CatalaRuntime.truncated_div(variable.cents, pow10)))
      CatalaRuntime::Money.new(rounded.cents * pow10)
    end
  end
end
