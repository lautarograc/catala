# Catala Ruby runtime

This is the runtime library required by the programs compiled to Ruby by
Catala (`catala ruby`). It is a single file, `lib/catala_runtime.rb`, without
dependencies outside of the Ruby standard library, and requires Ruby >= 3.0.

## Usage

The easiest way to compile and run Catala programs is to use `clerk`, which
takes care of compiling the standard library and setting up the runtime:

```sh
clerk build --backend ruby
clerk test --backend ruby
```

Programs compiled by hand with `catala ruby` need the runtime in the Ruby load
path, and the modules they use (including the Catala standard library) in the
same directory:

```sh
catala ruby foo.catala_en -o Foo.rb
ruby -I path/to/runtimes/ruby/lib Foo.rb --help
```

The runtime can also be installed as a gem:

```sh
gem build catala_runtime.gemspec
gem install catala_runtime-*.gem
```

## Generated code

Each Catala module (or file) is compiled to a Ruby module of the same name:

- scopes and top-level functions become singleton methods, taking (for scopes)
  an instance of the scope input structure and returning the output structure;
- top-level values are accessible through reader methods;
- structures and enumerations are classes, subclasses of
  `CatalaRuntime::Struct` and `CatalaRuntime::Enum`.

For instance, with a module `Tax` defining a scope `IncomeTax` with an input
`income`:

```ruby
require 'catala_runtime'
require_relative 'Tax'

result = Tax.income_tax(Tax::IncomeTaxIn.new(income_in: CatalaRuntime::Money.from_units(30_000)))
puts result.tax
```

Catala values map to Ruby values as follows:

| Catala              | Ruby                                         |
|---------------------|----------------------------------------------|
| `integer`           | `Integer`                                    |
| `decimal`           | `Rational`                                   |
| `boolean`           | `true` / `false`                             |
| `money`             | `CatalaRuntime::Money` (number of cents)     |
| `date`              | `CatalaRuntime::Date`                        |
| `duration`          | `CatalaRuntime::Duration`                    |
| `list of t`         | `Array`                                      |
| `(t1, t2)`          | `CatalaRuntime::Tuple`                       |
| `optional of t`     | `CatalaRuntime::Option`                      |
| structures          | subclasses of `CatalaRuntime::Struct`        |
| enumerations        | subclasses of `CatalaRuntime::Enum`          |
| external types      | subclasses of `CatalaRuntime::Value`         |
| functions           | callables (`Proc`, `Method`)                 |

Errors raised during the computation are subclasses of `CatalaRuntime::Error`
(`NoValue`, `Conflict`, `AssertionFailed`, `DivisionByZero`, `DateError`...),
and carry the positions in the Catala source code they relate to.

## External modules

The implementation of a Catala module marked as `external` is a Ruby file
defining a module with the expected methods. A template can be generated with
`catala ruby --gen-external`; see `tests/modules/good/ruby` and `stdlib/ruby`
for examples.

## Tests

The unit tests of the runtime can be run with `make test-ruby-runtime` from the
root of the Catala repository.
