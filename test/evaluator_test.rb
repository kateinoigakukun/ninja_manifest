require_relative "test_helper"

class NinjaEvaluatorTest < Test::Unit::TestCase
  def load_manifest(manifest)
    NinjaManifest.load(manifest)
  end

  def test_expands_variables_and_rule_bindings
    manifest = <<~NINJA
      cxx = c++
      builddir = build

      rule cxx
        command = $cxx -c $in -o $out
        description = CXX $out

      build $builddir/main.o: cxx src/main.cc
        cxx = clang
    NINJA

    manifest_obj = load_manifest(manifest)
    build = manifest_obj.builds.first

    assert_equal ["build/main.o"], build[:outputs][:explicit]
    assert_equal ["src/main.cc"], build[:inputs][:explicit]
    assert_equal "clang -c src/main.cc -o build/main.o", build[:vars]["command"]
    assert_equal "CXX build/main.o", build[:vars]["description"]
  end

  def test_resolves_chained_global_variables
    manifest = load_manifest(<<~NINJA)
      root = out
      builddir = $root/obj

      rule cat
        command = cat $in > $out

      build $builddir/result.txt: cat src/input.txt
    NINJA

    assert_equal(
      { "root" => "out", "builddir" => "out/obj" },
      manifest.variables
    )

    build = manifest.builds.first
    assert_equal ["out/obj/result.txt"], build[:outputs][:explicit]
    assert_equal ["src/input.txt"], build[:inputs][:explicit]
    assert_equal "cat src/input.txt > out/obj/result.txt",
                 build[:vars]["command"]
  end

  def test_populates_special_fields_from_rule
    manifest = load_manifest(<<~NINJA)
      rule link
        command = link $in -o $out
        description = LINK $out
        deps = gcc
        rspfile = $out.rsp
        rspfile_content = $in
        hide_success = 1

      build prog: link main.o util.o
    NINJA

    build = manifest.builds.first

    assert_equal "link main.o util.o -o prog", build[:vars]["command"]
    assert_equal "LINK prog", build[:vars]["description"]
    assert_equal "gcc", build[:vars]["deps"]
    assert_equal "prog.rsp", build[:vars]["rspfile"]
    assert_equal "main.o util.o", build[:vars]["rspfile_content"]
  end

  def test_uses_rule_defaults_when_build_binding_missing
    manifest = load_manifest(<<~NINJA)
      rule copy
        command = cp $in $out

      build dest.txt: copy src.txt
    NINJA

    build = manifest.builds.first
    assert_equal "cp src.txt dest.txt", build[:vars]["command"]
  end

  def test_rejects_unknown_rule
    manifest = <<~NINJA
      build out: missing in
    NINJA

    assert_raise(NinjaManifest::Error) { load_manifest(manifest) }
  end

  def test_expands_default_targets
    manifest = <<~NINJA
      target = app
      default $target
    NINJA

    manifest_obj = load_manifest(manifest)
    assert_equal ["app"], manifest_obj.defaults
  end

  def test_expands_default_targets_with_multiple_variables
    manifest = <<~NINJA
      builddir = build
      exe = app
      default $builddir/$exe
    NINJA

    manifest_obj = load_manifest(manifest)
    assert_equal ["build/app"], manifest_obj.defaults
  end

  def test_expands_multiple_default_targets
    manifest = <<~NINJA
      target1 = app1
      target2 = app2
      default $target1 $target2
    NINJA

    manifest_obj = load_manifest(manifest)
    assert_equal %w[app1 app2], manifest_obj.defaults
  end

  def test_parses_pool_directive
    manifest = <<~NINJA
      pool link_pool
        depth = 4

      pool compile_pool
        depth = 2
    NINJA

    manifest_obj = load_manifest(manifest)

    assert_equal({ "depth" => "4" }, manifest_obj.pools["link_pool"])
    assert_equal({ "depth" => "2" }, manifest_obj.pools["compile_pool"])
  end

  def test_parses_pool_with_variables
    manifest = <<~NINJA
      default_depth = 4
      pool link_pool
        depth = $default_depth
    NINJA

    manifest_obj = load_manifest(manifest)

    assert_equal({ "depth" => "4" }, manifest_obj.pools["link_pool"])
  end

  def test_include_directive_loads_file
    require "tmpdir"
    require "fileutils"

    Dir.mktmpdir do |dir|
      included_file = File.join(dir, "included.ninja")
      File.write(included_file, <<~NINJA)
          included_var = from_included
          rule included_rule
            command = echo $in > $out
        NINJA

      manifest = <<~NINJA
        include #{included_file}
        build out.txt: included_rule input.txt
      NINJA

      manifest_obj = load_manifest(manifest)

      assert_equal "from_included", manifest_obj.variables["included_var"]
      assert manifest_obj.rules.key?("included_rule")
      # Rules store raw (unevaluated) strings
      assert_equal(
        { "command" => "echo $in > $out" },
        manifest_obj.rules["included_rule"]
      )
    end
  end

  def test_include_with_variable_expansion
    require "tmpdir"

    Dir.mktmpdir do |dir|
      included_file = File.join(dir, "vars.ninja")
      File.write(included_file, "from_file = included\n")

      manifest = <<~NINJA
        include_dir = #{dir}
        include $include_dir/vars.ninja
      NINJA

      manifest_obj = load_manifest(manifest)

      assert_equal "included", manifest_obj.variables["from_file"]
    end
  end

  def test_subninja_creates_new_scope
    require "tmpdir"

    Dir.mktmpdir do |dir|
      subninja_file = File.join(dir, "sub.ninja")
      File.write(subninja_file, <<~NINJA)
          sub_var = sub_value
          parent_ref = $parent_var
        NINJA

      manifest = <<~NINJA
        parent_var = parent_value
        subninja #{subninja_file}
        after_sub = $parent_var
      NINJA

      manifest_obj = load_manifest(manifest)

      # Parent scope variables should be accessible
      assert_equal "parent_value", manifest_obj.variables["parent_var"]
      assert_equal "parent_value", manifest_obj.variables["after_sub"]

      # Subninja scope variables should NOT leak into parent
      assert_nil manifest_obj.variables["sub_var"]
      assert_nil manifest_obj.variables["parent_ref"]
    end
  end

  def test_subninja_can_reference_parent_variables
    require "tmpdir"

    Dir.mktmpdir do |dir|
      subninja_file = File.join(dir, "sub.ninja")
      File.write(subninja_file, <<~NINJA)
          rule build_from_sub
            command = echo $parent_var > $out
        NINJA

      manifest = <<~NINJA
        parent_var = hello
        subninja #{subninja_file}
        build output.txt: build_from_sub input.txt
      NINJA

      manifest_obj = load_manifest(manifest)

      # Rule from subninja should be available
      assert manifest_obj.rules.key?("build_from_sub")
      # Rules store raw (unevaluated) strings
      assert_equal "echo $parent_var > $out",
                   manifest_obj.rules["build_from_sub"]["command"]

      # When used in a build, the rule's command should be evaluated with parent vars
      build = manifest_obj.builds.first
      assert_equal "echo hello > output.txt", build[:vars]["command"]
    end
  end

  def test_subninja_variable_shadowing
    require "tmpdir"

    Dir.mktmpdir do |dir|
      subninja_file = File.join(dir, "sub.ninja")
      File.write(subninja_file, <<~NINJA)
          shadow_var = shadowed_in_sub
        NINJA

      manifest = <<~NINJA
        shadow_var = original_value
        subninja #{subninja_file}
        after_sub = $shadow_var
      NINJA

      manifest_obj = load_manifest(manifest)

      # Variable shadowing in subninja should not affect parent
      assert_equal "original_value", manifest_obj.variables["shadow_var"]
      assert_equal "original_value", manifest_obj.variables["after_sub"]
    end
  end

  def test_subninja_with_variable_expansion
    require "tmpdir"

    Dir.mktmpdir do |dir|
      subninja_file = File.join(dir, "child.ninja")
      File.write(subninja_file, <<~NINJA)
        child_var = child_value

        rule child_rule
          command = echo $child_var > $out
        build out.txt: child_rule input.txt
        NINJA

      manifest = <<~NINJA
        subninja_dir = #{dir}
        subninja $subninja_dir/child.ninja
      NINJA

      manifest_obj = load_manifest(manifest)

      assert manifest_obj.rules.key?("child_rule")
      assert_equal "echo $child_var > $out",
                   manifest_obj.rules["child_rule"]["command"]
      build = manifest_obj.builds.first
      assert_equal "echo child_value > out.txt", build[:vars]["command"]
      assert_equal ["out.txt"], build[:outputs][:explicit]
      assert_equal ["input.txt"], build[:inputs][:explicit]
    end
  end

  def test_subninja_parent_variables_accessible_in_build
    require "tmpdir"

    Dir.mktmpdir do |dir|
      subninja_file = File.join(dir, "child.ninja")
      File.write(subninja_file, <<~NINJA)
        rule build_with_parent_var
          command = echo $parent_var > $out
        build output.txt: build_with_parent_var input.txt
      NINJA

      manifest = <<~NINJA
        parent_var = from_parent
        subninja #{subninja_file}
      NINJA

      manifest_obj = load_manifest(manifest)

      # Build defined in subninja file should be able to access parent variables
      build = manifest_obj.builds.first
      assert_equal "echo from_parent > output.txt", build[:vars]["command"]
    end
  end

  def test_subninja_variable_priority_in_build
    require "tmpdir"

    Dir.mktmpdir do |dir|
      subninja_file = File.join(dir, "child.ninja")
      File.write(subninja_file, <<~NINJA)
        same_var = from_child
        rule test_rule
          command = echo $same_var > $out
        build output.txt: test_rule
          same_var = from_build
      NINJA

      manifest = <<~NINJA
        same_var = from_parent
        subninja #{subninja_file}
      NINJA

      manifest_obj = load_manifest(manifest)

      build = manifest_obj.builds.first
      # Build-level variable (Layer 2) should take precedence over
      # file-level variables (Layer 4: child, Layer 5: parent)
      assert_equal "echo from_build > output.txt", build[:vars]["command"]
    end
  end
end
