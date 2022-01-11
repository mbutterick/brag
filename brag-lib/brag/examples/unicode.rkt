#lang brag
start: (graphic | other)*
graphic: "\x21" .. "\x7E"
other: "\u0080" .. "\uFFFF"
