# Implementation of the external module Prorata_external (see
# ../prorata_external.catala_en)

require 'catala_runtime'

module Prorata_external
  # Trivial algorithm that cumulates all rounding errors to the last element
  def self.prorata(amount, weights)
    total_weight = weights.reduce(:+)
    result = weights.map { |w| amount * CatalaRuntime.div_mon_mon(w, total_weight) }
    remain = amount - result.reduce(:+)
    result[0...-1] + [result[-1] + remain]
  end

  # Another algorithm that distributes errors along the way
  def self.prorata2(amount, weights)
    rem_weight = weights.reduce(:+)
    rem = amount
    weights.map do |w|
      a = rem * CatalaRuntime.div_mon_mon(w, rem_weight)
      rem_weight -= w
      rem -= a
      a
    end
  end
end
