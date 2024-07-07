# search

Package search is a high-performance search algorithm to find matches for a regex pattern on the terminal screen over the course of a recorded terminal session. This is more complicated than it seems: it must track the exact byte at which a match first appeared and calculate how long that match remained intact on the screen.
