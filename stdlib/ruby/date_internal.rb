# frozen_string_literal: true

# Implementation of the external module Date_internal (see
# stdlib/date_internal.catala_en). The OCaml implementation
# (stdlib/ocaml/date_internal.ml) is the reference.

require 'catala_runtime'

module Date_internal
  def self.of_ymd(pos, dyear, dmonth, dday)
    CatalaRuntime::Date.new(dyear, dmonth, dday)
  rescue CatalaRuntime::InvalidDate
    raise CatalaRuntime::DateError.new(pos, format('|%04d-%02d-%02d| is not a valid date', dyear, dmonth, dday))
  end

  def self.to_ymd(d)
    CatalaRuntime::Tuple.new(d.year, d.month, d.day)
  end

  def self.last_day_of_month(d)
    d.last_day_of_month
  end

  def self.add_rounded_down(d, dur)
    d.add(dur, :round_down)
  end

  def self.add_rounded_up(d, dur)
    d.add(dur, :round_up)
  end
end
