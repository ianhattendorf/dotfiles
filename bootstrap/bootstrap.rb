#!/usr/bin/env ruby

require 'highline/import'
require 'optparse'

options = {}
include_list = %w(. .atom .gnupg)
blacklist = %w(.DS_Store)
destination_dir = '~'

OptionParser.new do |opts|
  opts.banner = 'Usage: ./bootstrap.rb [options]'
  opts.on('-s', '--silent', 'Run silently') { |s| options[:silent] = s }
end.parse!

def build_path(dir, file)
  "#{dir}/#{file}"
end

source_files = []

include_list.each do |include_dir|
  source_files += Dir.entries(include_dir)
    .reject { |file| blacklist.include?(file) || File.directory?(file) }
    .map { |file| build_path(include_dir, file) }
end

destination_files = source_files.map { |file| build_path(destination_dir, file) }
destination_dirs = include_list.map { |dir| build_path(destination_dir, dir) }

puts 'The following files will be overwritten if they exist:'
destination_files.each do |file|
  puts "#{File.expand_path(file)}"
end

abort unless options[:silent] || agree('Continue? (y/n)', true)

puts 'Creating destination folder structure...'

destination_dirs.map { |dir| File.expand_path(dir) }
  .reject { |dir| File.exist?(dir) }
  .each { |dir| Dir.mkdir(dir) }

puts 'Linking...'

destination_files.each_with_index do |file, i|
  source_file = File.expand_path(source_files[i])
  dest_file = File.expand_path(file)
  `ln -sf #{source_file} #{dest_file}`
  puts "#{source_file} => #{dest_file}"
end

puts 'Done.'
