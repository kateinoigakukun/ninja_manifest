# frozen_string_literal: true

require_relative "lib/ninja_manifest"

Gem::Specification.new do |spec|
  spec.name = "ninja_manifest"
  spec.version = NinjaManifest::VERSION
  spec.authors = ["Yuta Saito"]
  spec.email = ["katei@ruby-lang.org"]

  spec.summary = "A Ninja build manifest toolkit"
  spec.description = <<END
NinjaManifest is a Ninja build manifest toolkit, including a parser and evaluator.
END
  spec.homepage = "https://github.com/kateinoigakukun/ninja_manifest"
  spec.license = "MIT"
  spec.required_ruby_version = ">= 3.1.0"

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata[
    "source_code_uri"
  ] = "https://github.com/kateinoigakukun/ninja_manifest"

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files =
    Dir.chdir(__dir__) do
      `git ls-files -z`.split("\x0")
        .reject do |f|
          (f == __FILE__) ||
            f.match(
              %r{\A(?:(?:bin|test|spec|features)/|\.(?:git|travis|circleci)|appveyor)}
            )
        end
    end
  spec.require_paths = ["lib"]
end
