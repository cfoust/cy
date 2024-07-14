#!/usr/bin/env ruby

require "erb"

action_names = [
 :invalidSequence,
 :emitByte,
 :setByte1,
 :setByte2,
 :setByte2Top,
 :setByte3,
 :setByte3Top,
 :setByte4,
]

state_names = [
  :ground,
  :tail3,
  :tail2,
  :tail1,
  :u32e0,
  :u32ed,
  :utf843f0,
  :utf843f4,
]

states = {}

states[:ground] = {
  0x00..0x7f => [:ground, :emitByte],
  0xc2..0xdf => [:tail1, :setByte2Top],
  0xe0 => [:u32e0, :setByte3Top],
  0xe1..0xec => [:tail2, :setByte3Top],
  0xed => [:u32ed, :setByte3Top],
  0xee..0xef => [:tail2, :setByte3Top],
  0xf0 => [:utf843f0, :setByte4],
  0xf1..0xf3 => [:tail3, :setByte4],
  0xf4 => [:utf843f4, :setByte4],
}

states[:u32e0] = {
  0xa0..0xbf => [:tail1, :setByte2],
}

states[:u32ed] = {
  0x80..0x9f => [:tail1, :setByte2],
}

states[:utf843f0] = {
  0x90..0xbf => [:tail2, :setByte3],
}

states[:utf843f4] = {
  0x80..0x8f => [:tail2, :setByte3],
}

states[:tail3] = {
  0x80..0xbf => [:tail2, :setByte3],
}

states[:tail2] = {
  0x80..0xbf => [:tail1, :setByte2],
}

states[:tail1] = {
  0x80..0xbf => [:ground, :setByte1],
}

table = state_names.map do |state|
  actions = states[state]
  state_actions = Array.new(256, [:ground, :invalidSequence])

  actions.each do |range, act|
    Array(range).each do |r|
      state_actions[r] = act
    end
  end

  state_actions
end

if __FILE__ == $0
  template_path = File.read(File.expand_path("_table.go.erb", File.dirname(__FILE__)))
  out_path = File.expand_path("table.go", File.dirname(__FILE__))

  renderer = ERB.new(template_path, nil, "-")
  output = renderer.result(binding)

  File.write(out_path, output)
  `go fmt #{out_path}`
end
