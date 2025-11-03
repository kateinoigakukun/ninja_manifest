require "bundler/gem_tasks"
require "rake/testtask"
require "rdoc/task"

Rake::TestTask.new(:test) do |t|
  t.libs << "test"
  t.libs << "lib"
  t.test_files = FileList["test/**/*_test.rb"]
end

Rake::RDocTask.new do |rdoc|
  rdoc.rdoc_files.add(%w[README.md LICENSE lib/ninja_manifest.rb])
  rdoc.main = "README.md"
  rdoc.title = "ninja_parser Docs"
  rdoc.rdoc_dir = "doc"
end
