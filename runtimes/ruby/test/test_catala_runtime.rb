# frozen_string_literal: true

# Unit tests of the Ruby runtime. Run with `make test-ruby-runtime` from the root
# of the repository.

require 'minitest/autorun'
require 'catala_runtime'
require 'date_internal'
require 'decimal_internal'
require 'list_internal'
require 'money_internal'
require 'period_internal'

module CatalaRuntimeTestHelpers
  R = CatalaRuntime

  def money(units) = R::Money.from_units(Rational(units.to_s))
  def date(y, m, d) = R::Date.new(y, m, d)
  def dur(y, m, d) = R::Duration.new(y, m, d)

  def pos
    @pos ||= R::SourcePosition.new('test.catala_en', 1, 2, 3, 4, [])
  end

  def with_lang(lang)
    old = R.lang
    R.lang = lang
    yield
  ensure
    R.lang = old
  end
end

class TestArithmetic < Minitest::Test
  include CatalaRuntimeTestHelpers

  def test_rounding_is_half_away_from_zero
    assert_equal 3, R.round_rational(Rational(5, 2))
    assert_equal(-3, R.round_rational(Rational(-5, 2)))
    assert_equal 2, R.round_rational(Rational(49, 20))
    assert_equal Rational(-2), R.decimal_round(Rational(-3, 2))
  end

  def test_conversions
    assert_equal 2, R.integer_of_decimal(Rational(29, 10))
    assert_equal(-2, R.integer_of_decimal(Rational(-29, 10)))
    assert_equal 3, R.integer_of_money(R::Money.new(250))
    assert_equal Rational(1234, 100), R.decimal_of_money(R::Money.new(1234))
    assert_equal R::Money.new(1235), R.money_of_decimal(Rational(12_345, 1000))
    assert_equal R::Money.new(500), R.money_of_integer(5)
    assert_equal R::Money.new(300), R.money_round(R::Money.new(250))
    assert_equal R::Money.new(-300), R.money_round(R::Money.new(-250))
  end

  def test_money_operations
    assert_equal R::Money.new(333), R::Money.new(1000) * Rational(1, 3)
    assert_equal R::Money.new(3000), R::Money.new(1000) * 3
    assert_equal R::Money.new(-1000), -R::Money.new(1000)
    assert_equal Rational(1, 2), R.div_mon_mon(R::Money.new(50), R::Money.new(100))
    assert_equal R::Money.new(333), R.div_mon_int(R::Money.new(1000), 3)
    assert_equal R::Money.new(2000), R.div_mon_rat(R::Money.new(1000), Rational(1, 2))
    assert R::Money.new(1) < R::Money.new(2)
  end

  def test_divisions_by_zero
    assert_raises(R::DivisionByZero) { R.div_int_int(1, 0, pos) }
    assert_raises(R::DivisionByZero) { R.div_rat_rat(Rational(1), Rational(0), pos) }
    assert_raises(R::DivisionByZero) { R.div_mon_mon(R::Money.new(1), R::Money.new(0), pos) }
    assert_raises(R::DivisionByZero) { R.div_mon_int(R::Money.new(1), 0, pos) }
    err = assert_raises(R::DivisionByZero) { R.div_int_int(1, 0, pos) }
    assert_equal [pos], err.source_positions
  end

  def test_integer_division_is_exact
    assert_equal Rational(1, 3), R.div_int_int(1, 3, pos)
  end
end

class TestDates < Minitest::Test
  include CatalaRuntimeTestHelpers

  def test_invalid_dates
    assert_raises(R::InvalidDate) { date(2023, 2, 29) }
    assert_raises(R::InvalidDate) { date(2023, 13, 1) }
    date(2024, 2, 29)
  end

  def test_add_days
    assert_equal date(2024, 3, 1), date(2024, 2, 28).add(dur(0, 0, 2))
    assert_equal date(2023, 12, 31), date(2024, 1, 1).add(dur(0, 0, -1))
    assert_equal date(2025, 1, 1), date(2024, 1, 1).add(dur(0, 0, 366))
  end

  def test_add_months_rounding
    d = date(2024, 1, 31)
    assert_raises(R::AmbiguousComputation) { d.add(dur(0, 1, 0)) }
    assert_equal date(2024, 2, 29), d.add(dur(0, 1, 0), :round_down)
    assert_equal date(2024, 3, 1), d.add(dur(0, 1, 0), :round_up)
    assert_equal date(2025, 2, 28), date(2024, 2, 29).add(dur(1, 0, 0), :round_down)
    # Years and months are added before rounding
    assert_equal date(2025, 3, 31), date(2024, 1, 31).add(dur(1, 2, 0))
    assert_equal date(2023, 11, 30), date(2024, 1, 31).add(dur(0, -2, 0), :round_down)
  end

  def test_date_errors_from_operators
    err = assert_raises(R::DateError) { R.add_date_duration(date(2024, 1, 31), dur(0, 1, 0), :abort_on_round, pos) }
    assert_match(/ambiguous date computation/, err.message)
    assert_equal date(2024, 1, 1), R.sub_date_duration(date(2024, 2, 1), dur(0, 1, 0), :abort_on_round, pos)
  end

  def test_date_difference
    assert_equal dur(0, 0, 366), date(2025, 1, 1) - date(2024, 1, 1)
    assert_equal dur(0, 0, -1), date(2024, 1, 1) - date(2024, 1, 2)
  end

  def test_duration_comparisons
    assert R.eq(dur(1, 0, 0), dur(0, 12, 0), pos)
    assert R.lt(dur(0, 0, 1), dur(0, 0, 2), pos)
    assert_raises(R::DateError) { R.lt(dur(0, 1, 0), dur(0, 0, 30), pos) }
    assert_raises(R::DateError) { R.eq(dur(0, 1, 0), dur(0, 0, 30), pos) }
    assert_equal Rational(1, 2), R.div_dur_dur(dur(0, 0, 1), dur(0, 0, 2), pos)
    assert_raises(R::DateError) { R.div_dur_dur(dur(0, 1, 0), dur(0, 0, 2), pos) }
  end

  def test_day_numbers_roundtrip
    [[1970, 1, 1], [2000, 2, 29], [1900, 3, 1], [1, 1, 1], [2400, 12, 31]].each do |ymd|
      assert_equal ymd, R::Dates.of_days(R::Dates.to_days(*ymd))
    end
    assert_equal 0, R::Dates.to_days(1970, 1, 1)
  end
end

class TestStructuralOperations < Minitest::Test
  include CatalaRuntimeTestHelpers

  class Point < CatalaRuntime::Struct
    struct_name 'Point'
    field :x, 'x', :integer
    field :y, 'y', :decimal
  end

  class Shape < CatalaRuntime::Enum
    enum_name 'Shape'
    constructor :Empty, 'Empty', :unit
    constructor :Dot, 'Dot', Point
  end

  def test_struct_construction
    assert_raises(ArgumentError) { Point.new(x: 1) }
    assert_raises(ArgumentError) { Point.new(x: 1, y: 2, z: 3) }
    assert Point.new(x: 1, y: Rational(2)).frozen?
  end

  def test_equality_and_ordering
    p1 = Point.new(x: 1, y: Rational(2))
    p2 = Point.new(x: 1, y: Rational(3))
    assert R.eq(p1, Point.new(x: 1, y: Rational(2)), pos)
    refute R.eq(p1, p2, pos)
    assert R.lt(p1, p2, pos)
    assert R.lt(Shape.new(:Empty), Shape.new(:Dot, p1), pos)
    assert R.lt(R::Option::NONE, R::Option.some(0), pos)
    assert R.lt([1, 2], [1, 2, 0], pos)
    refute R.eq([1, 2], [1, 2, 0], pos)
    assert R.lt(R::Tuple.new(1, 2), R::Tuple.new(1, 3), pos)
    assert R.lt(false, true, pos)
    assert_raises(R::UncomparableValues) { R.eq(->(x) { x }, ->(x) { x }, pos) }
  end

  def test_list_operations
    double = ->(x) { x * 2 }
    assert_equal [2, 4], R.map(double, [1, 2])
    assert_equal [4, 6], R.map2(->(a, b) { a + b }, [1, 2], [3, 4], pos)
    assert_raises(R::NotSameLength) { R.map2(->(a, b) { a + b }, [1], [3, 4], pos) }
    assert_equal [2], R.filter(->(x) { x.even? }, [1, 2, 3])
    assert_equal 6, R.fold(->(acc, x) { acc + x }, 0, [1, 2, 3])
    assert_equal R::Option.some(6), R.reduce(->(a, b) { a + b }, [1, 2, 3])
    assert_equal R::Option::NONE, R.reduce(->(a, b) { a + b }, [])
    assert_equal R::Option.some(false), R.find(->(x) { x == false }, [true, false])
    assert_equal R::Option::NONE, R.find(->(x) { x > 3 }, [1, 2])
  end

  def test_closure_converted_functions
    # With closure conversion, functions are (function, environment) pairs
    closure = R::Tuple.new(->(env, x) { x + env[0] }, R::Tuple.new(10))
    assert_equal [11, 12], R.map(closure, [1, 2])
    assert_equal 13, R.fold(R::Tuple.new(->(_env, acc, x) { acc + x }, R::Tuple.new), 10, [1, 2])
  end

  def test_sort_is_stable
    l = [R::Tuple.new(1, 'a'), R::Tuple.new(0, 'b'), R::Tuple.new(1, 'c'), R::Tuple.new(0, 'd')]
    assert_equal %w[b d a c], R.sort_asc(->(t) { t[0] }, l, pos).map { |t| t[1] }
    assert_equal %w[a c b d], R.sort_desc(->(t) { t[0] }, l, pos).map { |t| t[1] }
  end

  def test_handle_exceptions
    p2 = R::SourcePosition.new('test.catala_en', 5, 6, 7, 8, [])
    assert_equal R::Option::NONE, R.handle_exceptions([R::Option::NONE])
    some = R::Option.some(R::Tuple.new(1, pos))
    assert_equal some, R.handle_exceptions([R::Option::NONE, some])
    err = assert_raises(R::Conflict) do
      R.handle_exceptions([some, R::Option.some(R::Tuple.new(2, p2))])
    end
    assert_equal [pos, p2], err.source_positions
    assert_equal 'At test.catala_en:1.2-3.4, test.catala_en:5.6-7.8: ' \
                 'conflict between multiple valid consequences for assigning the same variable',
                 err.message
  end
end

class TestPrinting < Minitest::Test
  include CatalaRuntimeTestHelpers

  def test_numbers
    assert_equal '-1,234,567', R.format_value(-1_234_567)
    assert_equal '0.333,333…', R.format_value(Rational(1, 3))
    assert_equal '-0.5', R.format_value(Rational(-1, 2))
    assert_equal '5.0', R.format_value(Rational(5))
    assert_equal '-$1,234.05', R.format_value(R::Money.new(-123_405))
    with_lang(:fr) do
      assert_equal '1 234 567', R.format_value(1_234_567)
      assert_equal '1 234,05 €', R.format_value(R::Money.new(123_405))
      assert_equal '0,333 333…', R.format_value(Rational(1, 3))
      assert_equal 'vrai', R.format_value(true)
    end
  end

  def test_dates_and_durations
    assert_equal '|2024-03-01|', R.format_value(date(2024, 3, 1))
    assert_equal '[0 day]', R.format_value(dur(0, 0, 0))
    assert_equal '[1 year, 3 days]', R.format_value(dur(1, 0, 3))
    assert_equal '[2 months]', R.format_value(dur(0, 2, 0))
    with_lang(:fr) { assert_equal '[2 ans, 3 mois]', R.format_value(dur(2, 3, 0)) }
  end

  def test_compound_values
    point = TestStructuralOperations::Point.new(x: 1, y: Rational(1, 2))
    assert_equal "Point {\n  -- x: 1\n  -- y: 0.5\n}", R.format_value(point)
    assert_equal "[\n  1;\n  2;\n]", R.format_value([1, 2])
    assert_equal '(1, true)', R.format_value(R::Tuple.new(1, true))
    assert_equal 'Present content 1', R.format_value(R::Option.some(1))
    assert_equal 'Absent', R.format_value(R::Option::NONE)
    assert_equal 'Dot content Point {}', R.format_value(TestStructuralOperations::Shape.new(:Dot, EmptyStruct.new))
  end

  class EmptyStruct < CatalaRuntime::Struct
    struct_name 'Point'
  end
end

class TestJson < Minitest::Test
  include CatalaRuntimeTestHelpers

  Point = TestStructuralOperations::Point
  Shape = TestStructuralOperations::Shape

  def test_to_json
    assert_equal '{"x":"1","y":"1/2"}', R.to_json(Point.new(x: 1, y: Rational(1, 2)))
    assert_equal '"-12.05"', R.to_json(R::Money.new(-1205))
    assert_equal '{"Present":["1",true]}', R.to_json(R::Option.some(R::Tuple.new(1, true)))
    assert_equal '{"Empty":{}}', R.to_json(Shape.new(:Empty))
    assert_equal '{"years":1,"months":2,"days":3}', R.to_json(dur(1, 2, 3))
  end

  def test_from_json
    assert_equal 12, R.from_json(:integer, '"12"', pos)
    assert_equal 12, R.from_json(:integer, '12', pos)
    assert_equal R::Money.new(1205), R.from_json(:money, '"12.05"', pos)
    assert_equal date(2024, 2, 29), R.from_json(:date, '"2024-02-29"', pos)
    assert_equal [Rational(1, 2)], R.from_json([:array, :decimal], '["1/2"]', pos)
    p = Point.new(x: 1, y: Rational(1, 2))
    assert_equal p, R.from_json(Point, R.to_json(p), pos)
    assert_equal Shape.new(:Dot, p), R.from_json(Shape, R.to_json(Shape.new(:Dot, p)), pos)
    assert_equal R::Option.some(1), R.from_json([:option, :integer], '{"Present":1}', pos)
    assert_raises(R::AssertionFailed) { R.from_json(:integer, '"x"', pos) }
    assert_raises(R::AssertionFailed) { R.from_json(:integer, '{', pos) }
  end
end

class TestStdlib < Minitest::Test
  include CatalaRuntimeTestHelpers

  def test_date_internal
    assert_equal date(2024, 2, 29), Date_internal.of_ymd(pos, 2024, 2, 29)
    err = assert_raises(R::DateError) { Date_internal.of_ymd(pos, 2023, 2, 29) }
    assert_match(/\|2023-02-29\| is not a valid date/, err.message)
    assert_equal R::Tuple.new(2024, 2, 29), Date_internal.to_ymd(date(2024, 2, 29))
    assert_equal date(2024, 2, 29), Date_internal.last_day_of_month(date(2024, 2, 3))
  end

  def test_rounding_to_decimals
    assert_equal Rational(123, 100), Decimal_internal.round_to_decimal(Rational(12_345, 10_000), 2)
    assert_equal Rational(1200), Decimal_internal.round_to_decimal(Rational(1234), -2)
    assert_equal R::Money.new(1230), Money_internal.round_to_decimal(R::Money.new(1234), 1)
    assert_equal R::Money.new(1200), Money_internal.round_to_decimal(R::Money.new(1234), 0)
    assert_equal R::Money.new(1000), Money_internal.round_to_decimal(R::Money.new(1234), -1)
    assert_equal R::Money.new(1234), Money_internal.round_to_decimal(R::Money.new(1234), 2)
  end

  def test_list_internal
    assert_equal [2, 3, 4], List_internal.sequence(2, 5)
    assert_equal [], List_internal.sequence(5, 2)
    assert_equal R::Option.some(:b), List_internal.nth_element(%i[a b], 2)
    assert_equal R::Option::NONE, List_internal.nth_element(%i[a b], 3)
    assert_equal %i[a c], List_internal.remove_nth_element(%i[a b c], 2)
    assert_equal %i[a b], List_internal.remove_nth_element(%i[a b], 0)
    assert_equal %i[b a], List_internal.reverse(%i[a b])
  end

  def test_period_internal
    t = R::Tuple
    assert_equal [t.new(date(2024, 1, 15), date(2024, 1, 31)), t.new(date(2024, 2, 1), date(2024, 2, 10))],
                 Period_internal.split_by_month(t.new(date(2024, 1, 15), date(2024, 2, 10)))
    assert_equal [t.new(date(2024, 1, 15), date(2024, 3, 31)), t.new(date(2024, 4, 1), date(2024, 5, 1))],
                 Period_internal.split_by_year(4, t.new(date(2024, 1, 15), date(2024, 5, 1)))
    l = [t.new(t.new(date(2024, 2, 1), date(2024, 3, 1)), 1), t.new(t.new(date(2024, 1, 1), date(2024, 3, 1)), 2)]
    assert_equal [2, 1], Period_internal.sort_(l).map { |x| x[1] }
  end
end
