# frozen_string_literal: true

# Implementation of the external module Period_internal (see
# stdlib/period_internal.catala_en). The OCaml implementation
# (stdlib/ocaml/period_internal.ml) is the reference.

require 'catala_runtime'

module Period_internal
  ONE_DAY = CatalaRuntime::Duration.new(0, 0, 1)
  ONE_MONTH = CatalaRuntime::Duration.new(0, 1, 0)
  ONE_YEAR = CatalaRuntime::Duration.new(1, 0, 0)

  # Stable sort of ((begin, end), value) pairs by beginning date
  def self.sort_(p)
    p.each_with_index.sort_by { |elt, i| [elt[0][0].to_a, i] }.map(&:first)
  end

  # Splits the period [start, stop] at the given cut dates, computed from the
  # beginning of each sub-period
  def self.split(start, stop)
    result = []
    loop do
      nxt = yield(start)
      if nxt < stop
        result << CatalaRuntime::Tuple.new(start, nxt.add(-ONE_DAY))
        start = nxt
      else
        result << CatalaRuntime::Tuple.new(start, stop) if start < stop
        return result
      end
    end
  end

  def self.split_by_month(p)
    split(p[0], p[1]) { |start| start.first_day_of_month.add(ONE_MONTH) }
  end

  def self.first_day_of_rolling_year(date, start_month)
    year = date.month < start_month ? date.year - 1 : date.year
    CatalaRuntime::Date.new(year, start_month, 1)
  end

  def self.split_by_year(start_month, p)
    raise ArgumentError, 'invalid start month' unless start_month.between?(1, 12)

    split(p[0], p[1]) { |start| first_day_of_rolling_year(start, start_month).add(ONE_YEAR) }
  end
end
