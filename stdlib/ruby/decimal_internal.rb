# frozen_string_literal: true

# Implementation of the external module Decimal_internal (see
# stdlib/decimal_internal.catala_en). The OCaml implementation
# (stdlib/ocaml/decimal_internal.ml) is the reference.

require 'catala_runtime'

module Decimal_internal
  def self.round_to_decimal(variable, nth_decimal)
    return Rational(CatalaRuntime.round_rational(variable)) if nth_decimal.zero?

    pow10 = Rational(10**nth_decimal.abs)
    if nth_decimal.positive?
      Rational(CatalaRuntime.round_rational(variable * pow10)) / pow10
    else
      Rational(CatalaRuntime.round_rational(variable / pow10)) * pow10
    end
  end
end
