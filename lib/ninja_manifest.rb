# MIT License
#
# Copyright (c) 2025 Yuta Saito
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

##
# The +NinjaManifest+ module provides a simple yet robust mechanism for parsing and
# evaluating build manifests written for the Ninja build system. It expands variables, resolves rule
# bindings, and constructs a fully-evaluated manifest representation suitable for inspection or custom
# tooling.
#
# == What’s Here
#
# - +NinjaManifest.load+ and +NinjaManifest.load_file+: entry-points for parsing/evaluating.
# - +NinjaManifest::parse+: flexible visitor-based parsing API without evaluation.
# - +NinjaManifest::Manifest+: the evaluated manifest (variables, rules, builds, etc.).
# - +NinjaManifest::Visitor+: base visitor class for parsed constructs.
#
module NinjaManifest
  VERSION = "0.1.0"

  ##
  # call-seq:
  #
  #   load(input) -> Manifest
  #
  # Parses and evaluates a Ninja build manifest, expanding all variables and resolving rule bindings.
  #
  # All variable references like $foo and ${bar} are expanded, and build bindings are resolved
  # according to Ninja's scoping rules.
  #
  #   manifest = NinjaManifest.load(<<~NINJA)
  #     cxx = c++
  #     builddir = build
  #
  #     rule cxx
  #       command = $cxx -c $in -o $out
  #       description = CXX $out
  #
  #     build $builddir/main.o: cxx src/main.cc
  #       cxx = clang
  #   NINJA
  #
  #   manifest.variables["cxx"] # => "c++"
  #   manifest.rules["cxx"]     # => {"command" => "$cxx -c $in -o $out", "description" => "CXX $out"}
  #
  #   manifest.builds.each do |build|
  #     puts build[:outputs][:explicit]  # => ["build/main.o"]
  #     puts build[:command]             # => "c++ -c src/main.cc -o build/main.o"
  #   end
  #
  # The input is a String (manifest content). To load from a file path, use NinjaManifest.load_file instead.
  def self.load(input, **opts)
    evaluator = Evaluator.new(**opts)
    parse(input, evaluator)
    evaluator.finalize
  end

  ##
  # call-seq:
  #
  #   load_file(path) -> Manifest
  #
  # Loads and evaluates a Ninja build manifest from a file path.
  #
  # This is a convenience method that reads the file and calls load.
  #
  #   manifest = NinjaManifest.load_file("build.ninja")
  #   manifest.variables["cc"]  # => "gcc"
  #   manifest.builds.size      # => 42
  def self.load_file(path, **opts)
    File.open(path, "r") { |file| load(file.read, **opts) }
  end

  ##
  # Represents an evaluated Ninja build manifest.
  #
  # A Manifest contains all parsed and evaluated data from a +build.ninja+ file,
  # including expanded variables, rules, builds, and etc.
  #
  #   manifest = NinjaManifest.load_file("build.ninja")
  #   manifest.variables["cc"]  # => "gcc"
  #   manifest.rules["cc"]      # => {"command" => "gcc $in -o $out", ...}
  #   manifest.builds.first     # => {:rule => "cc", :outputs => {...}, ...}
  #   manifest.defaults         # => ["app"]
  #
  # You typically create a Manifest using NinjaManifest.load_file or NinjaManifest.load.
  class Manifest
    ##
    # Returns the global variables hash with all values expanded.
    #
    #   manifest.variables["cc"]      # => "gcc"
    #   manifest.variables["cflags"]  # => "-Wall -O2"
    attr_reader :variables

    ##
    # Returns a hash of rule definitions, keyed by rule name.
    #
    # Each rule is a hash of attribute names to their (raw, unevaluated) values.
    #
    #   manifest.rules["cc"]
    #   # => {
    #   #      "command" => "gcc $in -o $out",
    #   #      "description" => "CC $out",
    #   #      ...
    #   #    }
    attr_reader :rules

    ##
    # Returns an array of build records.
    #
    # Each record is a hash with the following keys:
    # - +:rule+: the rule name used for this build
    # - +:outputs+: hash with +:explicit+ and +:implicit+ arrays (expanded paths)
    # - +:inputs+: hash with +:explicit+, +:implicit+, +:order_only+, +:validation+ arrays (expanded paths)
    # - +:vars+: hash of build-local variables (expanded)
    #
    # Example:
    #
    #   build = manifest.builds.first
    #   build[:outputs][:explicit]  # => ["build/main.o"]
    #   build[:inputs][:explicit]   # => ["src/main.cc"]
    attr_reader :builds

    ##
    # Returns an array of default target paths (fully expanded).
    #
    #   manifest.defaults  # => ["all", "tests"]
    attr_reader :defaults

    ##
    # Returns a hash of pool definitions, keyed by pool name.
    #
    # Each pool is a hash of attribute names to their values.
    #
    #   manifest.pools["link"]
    #   # => { "depth" => "1" }
    attr_reader :pools

    def initialize(variables:, rules:, builds:, defaults:, pools:) # :nodoc:
      @variables = variables
      @rules = rules
      @builds = builds
      @defaults = defaults
      @pools = pools
    end
  end

  ##
  # call-seq:
  #
  #   parse(input, visitor) -> nil
  #
  # Parses a Ninja build manifest and invokes callbacks on the provided Visitor object.
  #
  # The input is a String (manifest content). The visitor receives callbacks for all parsed constructs,
  # see Visitor for more details.
  #
  #   class MyVisitor < NinjaManifest::Visitor
  #     def visit_build(rule:, outs:, ins:, vars:, **kwargs)
  #       puts "Build: #{rule} -> #{outs[:explicit].join(', ')}"
  #     end
  #   end
  #
  #   visitor = MyVisitor.new
  #   NinjaManifest.parse(<<~NINJA, visitor)
  #     cxx = c++
  #     builddir = build
  #
  #     rule cxx
  #       command = $cxx -c $in -o $out
  #       description = CXX $out
  #
  #     build $builddir/main.o: cxx src/main.cc
  #       cxx = clang
  #   NINJA
  def self.parse(input, visitor)
    parser = Parser.new(input)
    parser.parse(visitor)
  end

  ##
  # Base visitor class for processing parsed Ninja manifest constructs.
  #
  # Subclass this class and override the visit methods to customize how parsed elements
  # are processed. All methods have default no-op implementations, so you only need to
  # override the ones you care about.
  #
  #   class MyVisitor < NinjaManifest::Visitor
  #     def visit_build(rule:, outs:, ins:, vars:, **kwargs)
  #       puts "Build: #{rule} -> #{outs[:explicit].join(', ')}"
  #     end
  #   end
  #
  #   visitor = MyVisitor.new
  #   NinjaManifest.parse(<<~NINJA, visitor)
  #     cxx = c++
  #     builddir = build
  #
  #     rule cxx
  #       command = $cxx -c $in -o $out
  #       description = CXX $out
  #
  #     build $builddir/main.o: cxx src/main.cc
  #       cxx = clang
  #   NINJA
  class Visitor
    ##
    # Called when a variable assignment is encountered.
    #
    # The value is the raw (unevaluated) string, which may contain variable references
    # like $foo or ${bar}. Variable expansion is not performed at parse time.
    #
    #   def visit_variable(name:, value:)
    #     puts "#{name} = #{value}"  # value may be "$cc -Wall" or similar
    #   end
    def visit_variable(name:, value:)
    end

    ##
    # Called when a rule definition is encountered.
    #
    # Rule attributes are raw strings and may contain variable references.
    # Common attributes include +"command"+, +"description"+, +"depfile"+, etc.
    #
    #   def visit_rule(name:, vars:)
    #     puts "Rule: #{name}"
    #     puts "Command: #{vars["command"]}"  # may contain "$in $out"
    #   end
    def visit_rule(name:, vars:)
    end

    ##
    # Called when a build statement is encountered.
    #
    # The +outs+ hash contains output paths with keys:
    # - +:explicit+: explicit output paths (before |)
    # - +:implicit+: implicit output paths (after |)
    #
    # The +ins+ hash contains input paths with keys:
    # - +:explicit+: explicit dependencies (before |)
    # - +:implicit+: implicit dependencies (after |)
    # - +:order_only+: order-only dependencies (after ||)
    # - +:validation+: validation dependencies (after @)
    #
    # The +rule+ parameter is the rule name, or an empty string for implicit phony builds.
    # The +vars+ hash contains build-local variable assignments (raw, unevaluated).
    #
    # Example:
    #
    #   def visit_build(rule:, outs:, ins:, vars:, **kwargs)
    #     puts "Building #{outs[:explicit].join(', ')} using rule #{rule}"
    #     puts "Dependencies: #{ins[:explicit].join(', ')}"
    #   end
    def visit_build(rule:, outs:, ins:, vars:, **kwargs)
    end

    ##
    # Called when a default statement is encountered.
    #
    # The targets array contains target paths (raw, unevaluated) that are marked
    # as default build targets.
    #
    #   def visit_default(targets:)
    #     puts "Default targets: #{targets.join(', ')}"
    #   end
    def visit_default(targets:)
    end

    ##
    # Called when an include statement is encountered.
    #
    # The path is the path to the included file (raw, unevaluated).
    #
    #   def visit_include(path:)
    #     puts "Including file: #{path}"
    #   end
    def visit_include(path:)
    end

    ##
    # Called when a subninja statement is encountered.
    #
    # The path is the path to the subninja file (raw, unevaluated).
    #
    #   def visit_subninja(path:)
    #     puts "Subninja file: #{path}"
    #   end
    def visit_subninja(path:)
    end

    ##
    # Called when a pool statement is encountered.
    #
    # The name is the name of the pool (raw, unevaluated).
    #
    #   def visit_pool(name:, vars:)
    #     puts "Pool: #{name} = #{vars}"
    #   end
    def visit_pool(name:, vars:)
    end
  end

  # Error raised when evaluation encounters invalid data.
  Error = Class.new(StandardError)

  # Scanner for character-by-character parsing of input text.
  class Scanner # :nodoc:
    attr_reader :offset, :line

    def initialize(buffer)
      @buffer = buffer.force_encoding("utf-8")
      @offset = 0
      @line = 1
    end

    def peek
      return "\0" if @offset >= @buffer.length

      c = @buffer[@offset]
      if c == "\r" && @offset + 1 < @buffer.length &&
           @buffer[@offset + 1] == "\n"
        "\n"
      else
        c
      end
    end

    def next
      read
    end

    def back
      raise Error, "back at start" if @offset == 0

      @offset -= 1
      if @offset >= 0 && @buffer[@offset] == "\n"
        @line -= 1
      elsif @offset > 0 && @buffer[@offset - 1] == "\r" &&
            @buffer[@offset] == "\n"
        @offset -= 1
        @line -= 1
      end
    end

    def read
      return "\0" if @offset >= @buffer.length

      c = @buffer[@offset]
      if c == "\r" && @offset + 1 < @buffer.length &&
           @buffer[@offset + 1] == "\n"
        @offset += 2
        @line += 1
        return "\n"
      end

      @offset += 1
      @line += 1 if c == "\n"
      c
    end

    def skip(ch)
      if read != ch
        back
        return false
      end
      true
    end

    def skip_spaces
      while skip(" ")
      end
    end

    def expect(ch)
      r = read
      if r != ch
        back
        raise Error, "expected #{ch.inspect}, got #{r.inspect}"
      end
    end

    def slice(start, ending)
      @buffer[start...ending]
    end
  end

  private_constant :Scanner

  ##
  # Parser for Ninja build manifest files.
  #
  # Parses build.ninja files according to the Ninja manifest format specification,
  # handling continuations, variable expansions, and all statement types. The parser
  # uses a visitor pattern to deliver parsed constructs.
  class Parser # :nodoc:
    ##
    # Creates a parser for the given input source.
    def initialize(input)
      @scanner = Scanner.new(input)
    end

    ##
    # Parses the manifest and invokes visitor callbacks for each parsed construct.
    def parse(visitor)
      loop do
        case @scanner.peek
        when "\0"
          break
        when "\n"
          @scanner.next
        when "#"
          skip_comment
        when " ", "\t"
          raise Error, "unexpected whitespace"
        else
          ident = read_ident
          skip_spaces
          case ident
          when "rule"
            visitor.visit_rule(**read_rule)
          when "build"
            visitor.visit_build(**read_build)
          when "default"
            visitor.visit_default(**read_default)
          when "include"
            path = read_eval(false)
            visitor.visit_include(path: path)
          when "subninja"
            path = read_eval(false)
            visitor.visit_subninja(path: path)
          when "pool"
            visitor.visit_pool(**read_pool)
          else
            # Variable assignment
            val = read_vardef
            visitor.visit_variable(name: ident, value: val)
          end
        end
      end
    end

    private

    def skip_comment
      loop do
        case @scanner.read
        when "\0"
          @scanner.back
          break
        when "\n"
          break
        end
      end
    end

    def read_ident
      start = @scanner.offset
      while (c = @scanner.read)
        break unless c.match?(/[a-zA-Z0-9_._-]/)
        break if c == "\0"
      end
      @scanner.back
      ending = @scanner.offset
      raise Error, "failed to scan ident" if ending == start
      @scanner.slice(start, ending)
    end

    def read_vardef
      skip_spaces
      @scanner.expect("=")
      skip_spaces
      if @scanner.peek == "\n"
        @scanner.expect("\n")
        return ""
      end
      result = read_eval(false)
      @scanner.expect("\n")
      result
    end

    def read_scoped_vars(variable_name_validator: nil)
      vars = {}
      while @scanner.peek == " "
        skip_spaces
        name = read_ident
        if variable_name_validator && !variable_name_validator.call(name)
          raise Error, "unexpected variable #{name.inspect}"
        end
        skip_spaces
        val = read_vardef
        vars[name] = val
      end
      vars
    end

    def read_rule
      name = read_ident
      @scanner.expect("\n")
      validator =
        lambda do |var|
          %w[
            command
            depfile
            dyndep
            description
            deps
            generator
            pool
            restat
            rspfile
            rspfile_content
            msvc_deps_prefix
            hide_success
            hide_progress
          ].include?(var)
        end
      vars = read_scoped_vars(variable_name_validator: validator)
      { name: name, vars: vars }
    end

    def read_pool
      name = read_ident
      @scanner.expect("\n")
      validator = lambda { |var| var == "depth" }
      vars = read_scoped_vars(variable_name_validator: validator)
      { name: name, vars: vars }
    end

    def read_unevaluated_paths_to(stop_at_path_sep: true)
      skip_spaces
      v = []
      while !matches?(@scanner.peek, ":", "|", "\n")
        v << read_eval(stop_at_path_sep)
        skip_spaces
      end
      v
    end

    def matches?(ch, *chars)
      chars.include?(ch)
    end

    def read_build
      line = @scanner.line
      outs_explicit = read_unevaluated_paths_to(stop_at_path_sep: true)

      outs_implicit = []
      if @scanner.peek == "|"
        @scanner.next
        outs_implicit = read_unevaluated_paths_to(stop_at_path_sep: true)
      end

      @scanner.expect(":")
      skip_spaces
      rule = read_ident

      ins_explicit = read_unevaluated_paths_to(stop_at_path_sep: true)

      ins_implicit = []
      if @scanner.peek == "|"
        @scanner.next
        peek = @scanner.peek
        if peek == "|" || peek == "@"
          @scanner.back
        else
          ins_implicit = read_unevaluated_paths_to(stop_at_path_sep: true)
        end
      end

      ins_order_only = []
      if @scanner.peek == "|"
        @scanner.next
        if @scanner.peek == "@"
          @scanner.back
        else
          @scanner.expect("|")
          ins_order_only = read_unevaluated_paths_to(stop_at_path_sep: true)
        end
      end

      ins_validation = []
      if @scanner.peek == "|"
        @scanner.next
        @scanner.expect("@")
        ins_validation = read_unevaluated_paths_to(stop_at_path_sep: true)
      end

      @scanner.expect("\n")
      vars = read_scoped_vars(variable_name_validator: lambda { |_| true })

      {
        rule: rule,
        line: line,
        outs: {
          explicit: outs_explicit,
          implicit: outs_implicit
        },
        ins: {
          explicit: ins_explicit,
          implicit: ins_implicit,
          order_only: ins_order_only,
          validation: ins_validation
        },
        vars: vars
      }
    end

    def read_default
      defaults = read_unevaluated_paths_to(stop_at_path_sep: true)
      raise Error, "expected path" if defaults.empty?
      @scanner.expect("\n")
      { targets: defaults }
    end

    def read_eval(stop_at_path_sep)
      result = +""
      start = @scanner.offset
      consumed = false

      if stop_at_path_sep
        loop do
          ch = @scanner.read
          case ch
          when "\0"
            raise Error, "unexpected EOF"
          when " ", ":", "|", "\n"
            @scanner.back
            break
          when "$"
            # Append literal part before $
            if @scanner.offset > start + 1
              result << @scanner.slice(start, @scanner.offset - 1)
            end
            # Handle escape sequence
            append_escape(result)
            start = @scanner.offset
            consumed = true
          else
            consumed = true
          end
        end
      else
        loop do
          ch = @scanner.read
          case ch
          when "\0"
            raise Error, "unexpected EOF"
          when "\n"
            @scanner.back
            break
          when "$"
            # Append literal part before $
            if @scanner.offset > start + 1
              result << @scanner.slice(start, @scanner.offset - 1)
            end
            # Handle escape sequence
            append_escape(result)
            start = @scanner.offset
            consumed = true
          else
            consumed = true
          end
        end
      end

      # Append remaining literal part
      if @scanner.offset > start
        result << @scanner.slice(start, @scanner.offset)
      end

      raise Error, "Expected a string" unless consumed

      result
    end

    def read_simple_varname
      start = @scanner.offset
      while (c = @scanner.read)
        break unless c.match?(/[a-zA-Z0-9_-]/)
        break if c == "\0"
      end
      @scanner.back
      ending = @scanner.offset
      raise Error, "failed to scan variable name" if ending == start
      @scanner.slice(start, ending)
    end

    def append_escape(result)
      case @scanner.read
      when "\n"
        @scanner.skip_spaces
        # Line continuation: $ at end of line, do nothing
      when " ", "$", ":"
        # Literal character
        result << @scanner.slice(@scanner.offset - 1, @scanner.offset)
      when "{"
        # ${var} form
        result << "$"
        result << "{"
        start = @scanner.offset
        loop do
          case @scanner.read
          when "\0"
            raise Error, "unexpected EOF"
          when "}"
            result << @scanner.slice(start, @scanner.offset - 1)
            result << "}"
            break
          end
        end
      else
        # $var form
        @scanner.back
        result << "$"
        var = read_simple_varname
        result << var
      end
    end

    def skip_spaces
      loop do
        case @scanner.read
        when " "
        when "$"
          if @scanner.peek == "\n"
            @scanner.read # consume newline and continue loop to skip leading spaces on next line
          else
            @scanner.back
            break
          end
        else
          @scanner.back
          break
        end
      end
    end
  end

  private_constant :Parser

  # Evaluator visitor that parses and fully evaluates Ninja manifests.
  class Evaluator < Visitor # :nodoc:
    def initialize(**opts)
      # Global vars stack: array of simple hash of evaluated strings
      # The last element of the stack is the current global vars.
      @global_vars_stack = [{}]
      # Rules: simple hash of evaluated strings
      @rules = { "phony" => {} }
      # Returns an array of build records.
      @builds = []
      # Returns an array of default target paths (fully expanded).
      @defaults = []
      # Returns a hash of pool definitions.
      @pools = {}
      @file_opener =
        opts[:file_opener] ||
          ->(path, mode, &block) { File.open(path, mode, &block) }
    end

    def visit_variable(name:, value:)
      # Evaluate immediately with current global vars
      global_env = lambda { |key| @global_vars_stack.last[key] }
      evaluated = expand(value, [global_env])
      @global_vars_stack.last[name] = evaluated
    end

    def visit_rule(name:, vars:)
      @rules[name] = vars.transform_values { |val| val.nil? ? nil : val.dup }
    end

    def visit_default(targets:)
      global_env = lambda { |key| @global_vars_stack.last[key] }
      expand_env = [global_env]

      expanded_targets = targets.map { |target| expand(target, expand_env) }
      @defaults.concat(expanded_targets)
    end

    def visit_pool(name:, vars:)
      # Evaluate pool variables with global env
      global_env = lambda { |key| @global_vars_stack.last[key] }
      evaluated_vars = {}
      vars.each do |key, value|
        evaluated_vars[key] = expand(value, [global_env]) if value
      end
      @pools[name] = evaluated_vars
    end

    def visit_include(path:)
      # Expand path variable references
      global_env = lambda { |key| @global_vars_stack.last[key] }
      expanded_path = expand(path, [global_env])
      @file_opener.call(expanded_path, "r") do |file|
        # Parse the included file with the current evaluator as the visitor
        NinjaManifest.parse(file.read, self)
      end
    end

    def visit_subninja(path:)
      global_env = lambda { |key| @global_vars_stack.last[key] }
      expanded_path = expand(path, [global_env])

      # Push a new global vars hash onto the stack
      @global_vars_stack.push({})
      begin
        @file_opener.call(expanded_path, "r") do |file|
          # Parse the subninja file with the current evaluator as the visitor
          NinjaManifest.parse(file.read, self)
        end
      ensure
        # Pop the global vars hash from the stack
        @global_vars_stack.pop
      end
    end

    def visit_build(rule:, outs:, ins:, vars:, **kwargs)
      rule_attrs = @rules[rule]
      raise Error, "unknown rule #{rule.inspect}" unless rule_attrs

      # https://ninja-build.org/manual.html#ref_scope
      # Variable lookup order:
      # 1. Special built-in variables ($in, $out)
      # 2. Build-level variables from the build block
      # 3. Rule-level variables from the rule block
      # 4. File-level variables from the file that the build line was in
      # 5. Variables from files that included this file using subninja keyword

      # Level 4 & 5: File-level variables (current file and parent files via subninja)
      lookup_file_level_env =
        lambda do |key|
          # Search from current file (Level 4) to parent files (Level 5)
          @global_vars_stack.reverse_each do |vars|
            return vars[key] if vars.key?(key)
          end
          nil
        end

      # Level 3: Rule-level variables
      lookup_rule_vars = lambda { |key| rule_attrs[key] }

      # Level 2: Build-level variables (raw, unevaluated)
      build_vars_raw = vars
      lookup_build_vars_env = lambda { |key| build_vars_raw[key] }

      # Evaluate paths using levels 2, 4, and 5
      path_envs = [lookup_build_vars_env, lookup_file_level_env]
      outputs = {
        explicit: outs[:explicit].map { |val| expand(val, path_envs) },
        implicit: outs[:implicit].map { |val| expand(val, path_envs) }
      }

      inputs = {
        explicit: ins[:explicit].map { |val| expand(val, path_envs) },
        implicit: ins[:implicit].map { |val| expand(val, path_envs) },
        order_only: ins[:order_only].map { |val| expand(val, path_envs) },
        validation: ins[:validation].map { |val| expand(val, path_envs) }
      }

      # Level 1: Special built-in variables (requires evaluated paths)
      lookup_implicit_vars =
        lambda do |key|
          case key
          when "in"
            inputs[:explicit].join(" ")
          when "in_newline"
            inputs[:explicit].join("\n")
          when "out"
            outputs[:explicit].join(" ")
          when "out_newline"
            outputs[:explicit].join("\n")
          else
            nil
          end
        end

      path_envs = [
        lookup_implicit_vars,
        lookup_build_vars_env,
        lookup_rule_vars,
        lookup_file_level_env
      ]
      final_vars = {}
      rule_attrs.each do |key, value|
        final_vars[key] = expand(value, path_envs)
      end
      build_vars_raw.each do |key, value|
        final_vars[key] = expand(value, path_envs)
      end

      record = {
        rule: rule,
        outputs: outputs,
        inputs: inputs,
        vars: final_vars
      }

      @builds << record
      record
    end

    def finalize
      Manifest.new(
        variables: @global_vars_stack.last.dup,
        rules: @rules.dup,
        builds: @builds.dup,
        defaults: @defaults.dup,
        pools: @pools.dup
      )
    end

    private

    def expand(text, env_procs)
      result = +""
      i = 0
      while i < text.length
        char = text[i]
        if char != "$"
          result << char
          i += 1
          next
        end

        i += 1
        break if i >= text.length

        next_char = text[i]

        case next_char
        when "$", " ", ":"
          result << next_char
          i += 1
        when "{"
          i += 1
          start = i
          i += 1 while i < text.length && text[i] != "}"
          name = text[start...i]
          i += 1 if i < text.length
          result << (expand(lookup_variable(name, env_procs) || "", env_procs))
        else
          start = i
          i += 1 while i < text.length && text[i].match?(/[A-Za-z0-9_-]/)
          name = text[start...i]
          if name.empty?
            result << "$"
          else
            result << (
              expand(lookup_variable(name, env_procs) || "", env_procs)
            )
          end
        end
      end
      result
    end

    def lookup_variable(name, env_procs)
      env_procs.each do |env|
        next unless env

        value = env.call(name)
        return value unless value.nil?
      end
      nil
    end
  end

  private_constant :Evaluator
end
