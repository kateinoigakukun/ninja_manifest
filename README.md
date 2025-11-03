# NinjaManifest

A Ruby toolkit for parsing and evaluating [Ninja build manifest](https://ninja-build.org/manual.html) files (`build.ninja`).

Features:

* Parses Ninja build manifest files according to the official [Ninja implementation](https://ninja-build.org/)
* Evaluates variable expansions and rule bindings
* Visitor pattern API for flexible processing
* Compact and plain old pure Ruby implementation without external dependencies

## Installation

Install the gem and add to the application's Gemfile by executing:

    $ bundle add ninja_manifest

If bundler is not being used to manage dependencies, install the gem by executing:

    $ gem install ninja_manifest

## Basic Usage

```ruby
require "ninja_manifest"

# Parse and evaluate a build.ninja content
manifest = NinjaManifest.load(<<~NINJA)
cxx = c++
builddir = build

rule cxx
  command = $cxx -c $in -o $out
  description = CXX $out

build $builddir/main.o: cxx src/main.cc
  cxx = clang
NINJA
# Or load from build.ninja file
manifest = NinjaManifest.load_file("build.ninja")

# Access parsed data

manifest.variables             # Variables hash
manifest.variables["cxx"]      # => "c++"

manifest.rules                 # Rules hash
rule = manifest.rules["cxx"]
rule["command"]                # => "$cxx -c $in -o $out"
rule["description"]            # => "CXX $out"

build = manifest.builds.first
build[:outputs][:explicit]     # => ["build/main.o"]
build[:inputs][:explicit]      # => ["src/main.cc"]
build[:vars]["command"]        # => "c++ -c src/main.cc -o build/main.o"
build[:vars]["description"]    # => "CXX build/main.o"
```

## Custom Visitor Usage

```ruby
require "ninja_manifest"

# Create a custom visitor to process parsed constructs
class MyVisitor < NinjaManifest::Visitor
  def visit_variable(name:, value:)
    puts "Variable: #{name} = #{value}"
  end

  def visit_rule(name:, vars:)
    puts "Rule: #{name} with #{vars.keys.size} attributes"
  end

  def visit_build(rule:, outs:, ins:, vars:, **kwargs)
    puts "Build: #{rule} -> #{outs[:explicit].join(', ')}"
  end
end

visitor = MyVisitor.new
File.open("build.ninja", "r") do |f|
  NinjaManifest::parse(f.read, visitor)
end
```

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/kateinoigakukun/ninja_manifest.

## License

The gem is available as open source under the terms of the [MIT License](https://opensource.org/licenses/MIT).

