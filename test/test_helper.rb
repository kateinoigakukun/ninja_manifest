require "simplecov"
require "simplecov-lcov"

SimpleCov::Formatter::LcovFormatter.config do |c|
  c.single_report_path = "coverage/lcov.info"
  c.report_with_single_file = true
end
SimpleCov.start do
  enable_coverage :branch
  formatter SimpleCov::Formatter::LcovFormatter
end

require_relative "../lib/ninja_manifest"

require "test/unit"
