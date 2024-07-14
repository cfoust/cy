#!/usr/bin/env ruby

require "erb"

action_names = [
  :none,
  :clear,
  :collect,
  :csiDispatch,
  :escDispatch,
  :execute,
  :hook,
  :ignore,
  :oscEnd,
  :oscPut,
  :oscStart,
  :param,
  :print,
  :put,
  :unhook,
  :beginUtf8,
]

state_names = [
  :anywhere,
  :csiEntry,
  :csiIgnore,
  :csiIntermediate,
  :csiParam,
  :dcsEntry,
  :dcsIgnore,
  :dcsIntermediate,
  :dcsParam,
  :dcsPassthrough,
  :escape,
  :escapeIntermediate,
  :ground,
  :oscString,
  :sosPmApcString,
  :utf8,
]

states = {}

states[:anywhere] = {
  0x18 => [:ground, :execute],
  0x1a => [:ground, :execute],
  0x1b => [:escape, :none],
}

states[:ground] = {
  0x00..0x17 => [:anywhere, :execute],
  0x19       => [:anywhere, :execute],
  0x1c..0x1f => [:anywhere, :execute],
  0x20..0x7f => [:anywhere, :print],
  0x80..0x8f => [:anywhere, :execute],
  0x91..0x9a => [:anywhere, :execute],
  0x9c       => [:anywhere, :execute],
  0xc2..0xdf => [:utf8, :beginUtf8], #Beginning of UTF-8 2 byte sequence
  0xe0..0xef => [:utf8, :beginUtf8], #Beginning of UTF-8 3 byte sequence
  0xf0..0xf4 => [:utf8, :beginUtf8], #Beginning of UTF-8 4 byte sequence
}

states[:escape] = {
  0x00..0x17 => [:anywhere, :execute],
  0x19       => [:anywhere, :execute],
  0x1c..0x1f => [:anywhere, :execute],
  0x7f       => [:anywhere, :ignore],
  0x20..0x2f => [:escapeIntermediate, :collect],
  0x30..0x4f => [:ground, :escDispatch],
  0x51..0x57 => [:ground, :escDispatch],
  0x59       => [:ground, :escDispatch],
  0x5a       => [:ground, :escDispatch],
  0x5c       => [:ground, :escDispatch],
  0x60..0x7e => [:ground, :escDispatch],
  0x5b       => [:csiEntry, :none],
  0x5d       => [:oscString, :none],
  0x50       => [:dcsEntry, :none],
  0x58       => [:sosPmApcString, :none],
  0x5e       => [:sosPmApcString, :none],
  0x5f       => [:sosPmApcString, :none],
}

states[:escapeIntermediate] = {
  0x00..0x17 => [:anywhere, :execute],
  0x19       => [:anywhere, :execute],
  0x1c..0x1f => [:anywhere, :execute],
  0x20..0x2f => [:anywhere, :collect],
  0x7f       => [:anywhere, :ignore],
  0x30..0x7e => [:ground, :escDispatch],
}

states[:csiEntry] = {
  0x00..0x17 => [:anywhere, :execute],
  0x19       => [:anywhere, :execute],
  0x1c..0x1f => [:anywhere, :execute],
  0x7f       => [:anywhere, :ignore],
  0x20..0x2f => [:csiIntermediate, :collect],
  0x3a       => [:csiIgnore, :none],
  0x30..0x39 => [:csiParam, :param],
  0x3b       => [:csiParam, :param],
  0x3c..0x3f => [:csiParam, :collect],
  0x40..0x7e => [:ground, :csiDispatch],
}

states[:csiIgnore] = {
  0x00..0x17 => [:anywhere, :execute],
  0x19       => [:anywhere, :execute],
  0x1c..0x1f => [:anywhere, :execute],
  0x20..0x3f => [:anywhere, :ignore],
  0x7f       => [:anywhere, :ignore],
  0x40..0x7e => [:ground, :none],
}

states[:csiParam] = {
  0x00..0x17 => [:anywhere, :execute],
  0x19       => [:anywhere, :execute],
  0x1c..0x1f => [:anywhere, :execute],
  0x30..0x39 => [:anywhere, :param],
  0x3b       => [:anywhere, :param],
  0x7f       => [:anywhere, :ignore],
  0x3a       => [:csiIgnore, :none],
  0x3c..0x3f => [:csiIgnore, :none],
  0x20..0x2f => [:csiIntermediate, :collect],
  0x40..0x7e => [:ground, :csiDispatch],
}

states[:csiIntermediate] = {
  0x00..0x17 => [:anywhere, :execute],
  0x19       => [:anywhere, :execute],
  0x1c..0x1f => [:anywhere, :execute],
  0x20..0x2f => [:anywhere, :collect],
  0x7f       => [:anywhere, :ignore],
  0x30..0x3f => [:csiIgnore, :none],
  0x40..0x7e => [:ground, :csiDispatch],
}

states[:dcsEntry] = {
  0x00..0x17 => [:anywhere, :ignore],
  0x19       => [:anywhere, :ignore],
  0x1c..0x1f => [:anywhere, :ignore],
  0x7f       => [:anywhere, :ignore],
  0x3a       => [:dcsIgnore, :none],
  0x20..0x2f => [:dcsIntermediate, :collect],
  0x30..0x39 => [:dcsParam, :param],
  0x3b       => [:dcsParam, :param],
  0x3c..0x3f => [:dcsParam, :collect],
  0x40..0x7e => [:dcsPassthrough, :none],
}

states[:dcsIntermediate] = {
  0x00..0x17 => [:anywhere, :ignore],
  0x19       => [:anywhere, :ignore],
  0x1c..0x1f => [:anywhere, :ignore],
  0x20..0x2f => [:anywhere, :collect],
  0x7f       => [:anywhere, :ignore],
  0x30..0x3f => [:dcsIgnore, :none],
  0x40..0x7e => [:dcsPassthrough, :none],
}

states[:dcsIgnore] = {
  0x00..0x17 => [:anywhere, :ignore],
  0x19       => [:anywhere, :ignore],
  0x1c..0x1f => [:anywhere, :ignore],
  0x20..0x7f => [:anywhere, :ignore],
  0x9c       => [:ground, :none],
}

states[:dcsParam] = {
  0x00..0x17 => [:anywhere, :ignore],
  0x19       => [:anywhere, :ignore],
  0x1c..0x1f => [:anywhere, :ignore],
  0x30..0x39 => [:anywhere, :param],
  0x3b       => [:anywhere, :param],
  0x7f       => [:anywhere, :ignore],
  0x3a       => [:dcsIgnore, :none],
  0x3c..0x3f => [:dcsIgnore, :none],
  0x20..0x2f => [:dcsIntermediate, :collect],
  0x40..0x7e => [:dcsPassthrough, :none],
}

states[:dcsPassthrough] = {
  0x00..0x17 => [:anywhere, :put],
  0x19       => [:anywhere, :put],
  0x1c..0x1f => [:anywhere, :put],
  0x20..0x7e => [:anywhere, :put],
  0x7f       => [:anywhere, :ignore],
  0x9c       => [:ground, :none],
}

states[:sosPmApcString] = {
  0x00..0x17 => [:anywhere, :ignore],
  0x19       => [:anywhere, :ignore],
  0x1c..0x1f => [:anywhere, :ignore],
  0x20..0x7f => [:anywhere, :ignore],
  0x9c       => [:ground, :none],
}

states[:oscString] = {
  0x00..0x06 => [:anywhere, :ignore],
  0x07       => [:ground, :none],
  0x08..0x17 => [:anywhere, :ignore],
  0x19       => [:anywhere, :ignore],
  0x1c..0x1f => [:anywhere, :ignore],
  0x20..0xff => [:anywhere, :oscPut],
}

entry_actions = [
  nil,
  :clear,
  nil,
  nil,
  nil,
  :clear,
  nil,
  nil,
  nil,
  :hook,
  :clear,
  nil,
  nil,
  :oscStart,
  nil,
  nil,
]

exit_actions = [
  nil,
  nil,
  nil,
  nil,
  nil,
  nil,
  nil,
  nil,
  nil,
  :unhook,
  nil,
  nil,
  nil,
  :oscEnd,
  nil,
  nil,
]

table = state_names.map do |state|
  actions = states[state]
  next if actions.nil?

  state_actions = Array.new(256, 0)

  actions.each do |range, act|
    Array(range).each do |r|
      state_actions[r] = act
    end
  end

  state_actions
end

table.compact!

if __FILE__ == $0
  template_path = File.read(File.expand_path("_table.go.erb", File.dirname(__FILE__)))
  out_path = File.expand_path("table.go", File.dirname(__FILE__))

  renderer = ERB.new(template_path, nil, "-")
  output = renderer.result(binding)

  File.write(out_path, output)
  `go fmt #{out_path}`
end
