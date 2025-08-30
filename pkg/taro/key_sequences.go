/*
*
This file is a heavily modified version of key.go as it appear{s,ed} in
https://github.com/charmbracelet/bubbletea with the following LICENSE:

MIT License

# Copyright (c) 2020-2023 Charmbracelet, Inc

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package taro

import "sort"

// extSequences is used by the map-based algorithm below. It contains
// the sequences plus their alternatives with an escape character
// prefixed, plus the control chars, plus the space.
// It does not contain the NUL character, which is handled specially
// by detectOneMsg.
var extSequences = func() map[string]Key {
	s := map[string]Key{}
	for seq, key := range sequences {
		key := key
		s[seq] = key
		if key.Modifiers&KittyModAlt == 0 {
			key.Modifiers |= KittyModAlt
			s["\x1b"+seq] = key
		}
	}
	for i := keyNUL + 1; i <= keyDEL; i++ {
		if i == keyESC {
			continue
		}
		s[string([]byte{byte(i)})] = Key{KeyCode: int(i), EventType: KittyKeyPress}
		s[string([]byte{'\x1b', byte(i)})] = Key{KeyCode: int(i), Modifiers: KittyModAlt, EventType: KittyKeyPress}
		if i == keyUS {
			i = keyDEL - 1
		}
	}
	s[" "] = Key{KeyCode: ' ', EventType: KittyKeyPress}
	s["\x1b "] = Key{KeyCode: ' ', Modifiers: KittyModAlt, EventType: KittyKeyPress}
	s["\x1b\x1b"] = Key{KeyCode: KittyKeyEscape, Modifiers: KittyModAlt, EventType: KittyKeyPress}
	return s
}()

// seqLengths is the sizes of valid sequences, starting with the
// largest size.
var seqLengths = func() []int {
	sizes := map[int]struct{}{}
	for seq := range extSequences {
		sizes[len(seq)] = struct{}{}
	}
	lsizes := make([]int, 0, len(sizes))
	for sz := range sizes {
		lsizes = append(lsizes, sz)
	}
	sort.Slice(lsizes, func(i, j int) bool { return lsizes[i] > lsizes[j] })
	return lsizes
}()

// detectSequence uses a longest prefix match over the input
// sequence and a hash map.
func detectSequence(input []byte) (hasSeq bool, width int, msg Msg) {
	seqs := extSequences
	for _, sz := range seqLengths {
		if sz > len(input) {
			continue
		}
		prefix := input[:sz]
		key, ok := seqs[string(prefix)]
		if ok {
			return true, sz, KeyMsg(key)
		}
	}
	// Is this an unknown CSI sequence?
	if loc := unknownCSIRe.FindIndex(input); loc != nil {
		return true, loc[1], unknownCSISequenceMsg(input[:loc[1]])
	}

	return false, 0, nil
}
