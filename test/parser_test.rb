require_relative "test_helper"

class RecordingVisitor < NinjaManifest::Visitor
  attr_reader :variables, :rules, :builds, :defaults

  def initialize
    @variables = {}
    @rules = {}
    @builds = []
    @defaults = []
  end

  def visit_variable(name:, value:)
    @variables[name] = value
  end

  def visit_rule(name:, vars:)
    @rules[name] = vars
  end

  def visit_build(**build)
    @builds << build
  end

  def visit_default(targets:)
    @defaults.concat(targets)
  end
end

class NinjaParserTest < Test::Unit::TestCase
  def setup
    @build_file = File.expand_path("./fixtures/build.ninja", __dir__)
  end

  def test_parses_ninja_build_manifest
    visitor = RecordingVisitor.new
    NinjaManifest.parse(File.read(@build_file), visitor)

    assert_equal "g++", visitor.variables["cxx"]
    assert_includes visitor.variables["cflags"],
                    "-g -Wall -Wextra -Wno-deprecated -Wno-missing-field-initializers -Wno-unused-parameter -fno-rtti -fno-exceptions -fvisibility=hidden -pipe '-DNINJA_PYTHON=\"python\"' -O2 -DNDEBUG -DUSE_PPOLL -DNINJA_HAVE_BROWSE -I."

    assert visitor.rules.key?("cxx"), "Expected to capture cxx rule"
    assert_match(
      "$cxx -MMD -MT $out -MF $out.d $cflags -c $in -o $out",
      visitor.rules["cxx"]["command"],
      "Unexpected rule command for cxx"
    )

    ninja_build =
      visitor.builds.find do |b|
        b[:outs][:explicit] == ["ninja"] && b[:outs][:implicit].empty?
      end
    assert_not_nil ninja_build, "Expected to capture build for ninja target"
    assert_equal "link", ninja_build[:rule]
    assert_includes ninja_build[:ins][:explicit], "$builddir/ninja.o"
    assert_includes ninja_build[:ins][:implicit], "$builddir/libninja.a"

    browse_py_header =
      visitor.builds.find do |b|
        b[:outs][:explicit] == ["$builddir/browse_py.h"]
      end
    assert_not_nil browse_py_header
    assert_equal "kBrowsePy", browse_py_header[:vars]["varname"]

    browse_object =
      visitor.builds.find { |b| b[:outs][:explicit] == ["$builddir/browse.o"] }
    assert_not_nil browse_object
    assert_includes browse_object[:ins][:order_only], "$builddir/browse_py.h"
    assert_empty browse_object[:outs][:implicit]
  end

  def test_parses_grouped_build_dependencies
    manifest = <<~NINJA
      cflags = -O2 $
        -DNDEBUG

      rule cc
        command = cc $in -o $out

      build out/a.o out/b.o | out/generated.o: cc src/a.cc src/b.cc | include/a.h include/b.h || order.txt |@ validate.stamp
        depfile = deps.d
    NINJA

    visitor = RecordingVisitor.new
    NinjaManifest.parse(manifest, visitor)

    assert_equal "-O2 -DNDEBUG", visitor.variables["cflags"]

    build =
      visitor.builds.find { |b| b[:outs][:explicit] == %w[out/a.o out/b.o] }
    assert_not_nil build
    assert_equal %w[out/generated.o], build[:outs][:implicit]
    assert_equal %w[src/a.cc src/b.cc], build[:ins][:explicit]
    assert_equal %w[include/a.h include/b.h], build[:ins][:implicit]
    assert_equal ["order.txt"], build[:ins][:order_only]
    assert_equal ["validate.stamp"], build[:ins][:validation]
    assert_equal "deps.d", build[:vars]["depfile"]
  end

  def test_escapes_in_variables_and_paths
    manifest = <<~NINJA
      literal = $$bar$:$ baz

      rule copy
        command = cp $in $out

      build out/file$ with$ space.txt: copy src/input.txt
    NINJA

    visitor = RecordingVisitor.new
    NinjaManifest.parse(manifest, visitor)

    assert_equal "$bar: baz", visitor.variables["literal"]

    build =
      visitor.builds.find do |b|
        b[:outs][:explicit] == ["out/file with space.txt"]
      end

    assert_not_nil build
    assert_equal ["src/input.txt"], build[:ins][:explicit]
  end

  def test_parses_variables_with_spaces
    visitor = RecordingVisitor.new
    manifest = <<~NINJA
two_words_with_one_space = foo $
    bar
one_word_with_no_space = foo$
    bar
    NINJA

    NinjaManifest.parse(manifest, visitor)

    assert_equal "foo bar", visitor.variables["two_words_with_one_space"]
    assert_equal "foobar", visitor.variables["one_word_with_no_space"]
  end

  def test_ignore_include_and_subninja_statements
    manifest = <<~NINJA
      include other.ninja
      subninja dir/child.ninja

      build default: phony
    NINJA

    visitor = RecordingVisitor.new
    NinjaManifest.parse(manifest, visitor)

    assert_empty visitor.variables
    build = visitor.builds.find { |b| b[:outs][:explicit] == ["default"] }
    assert_not_nil build
    assert_equal "phony", build[:rule]
  end

  def test_parses_default_targets
    manifest = <<~NINJA
      default ninja
    NINJA

    visitor = RecordingVisitor.new
    NinjaManifest.parse(manifest, visitor)

    assert_equal ["ninja"], visitor.defaults
  end

  def test_parses_multiple_default_targets
    manifest = <<~NINJA
      default target1 target2 target3
    NINJA

    visitor = RecordingVisitor.new
    NinjaManifest.parse(manifest, visitor)

    assert_equal %w[target1 target2 target3], visitor.defaults
  end

  def test_parses_pool_directive
    manifest = <<~NINJA
      pool link_pool
        depth = 4
    NINJA

    visitor = RecordingVisitor.new
    NinjaManifest.parse(manifest, visitor)

    assert visitor.instance_variable_get(:@pools).nil? ||
             visitor.instance_variable_get(:@pools).empty?,
           "RecordingVisitor should not have pools by default"

    pool_visitor =
      Class
        .new(NinjaManifest::Visitor) do
          attr_reader :pools

          def initialize
            @pools = {}
          end

          def visit_pool(name:, vars:)
            @pools[name] = vars
          end
        end
        .new

    NinjaManifest.parse(manifest, pool_visitor)

    assert_equal({ "depth" => "4" }, pool_visitor.pools["link_pool"])
  end

  def test_parses_include_directive
    manifest = <<~NINJA
      include other.ninja
      build default: phony
    NINJA

    include_visitor =
      Class
        .new(NinjaManifest::Visitor) do
          attr_reader :includes

          def initialize
            @includes = []
          end

          def visit_include(path:)
            @includes << path
          end
        end
        .new

    NinjaManifest.parse(manifest, include_visitor)

    assert_equal ["other.ninja"], include_visitor.includes
  end

  def test_parses_subninja_directive
    manifest = <<~NINJA
      subninja child.ninja
      build default: phony
    NINJA

    subninja_visitor =
      Class
        .new(NinjaManifest::Visitor) do
          attr_reader :subninjas

          def initialize
            @subninjas = []
          end

          def visit_subninja(path:)
            @subninjas << path
          end
        end
        .new

    NinjaManifest.parse(manifest, subninja_visitor)

    assert_equal ["child.ninja"], subninja_visitor.subninjas
  end
end
