# Disclojure
A library I am starting to implement all classic data structures and especially the ones which are not mentioned in the classic Okasaki[1][2] in Clojure.

This is an attempt to build a data structures server written entirely on top of the JVM. The library intends to be predominantly in Clojure and use its inherent STM model to ensure thread safety, but wherever necessary I intend to use any other JVM language like Java, Scala, Groovy etc.

It essentially aims to be a drop in replacement for Redis by being more efficient(in time) and providing a more exhaustive collection of data structures for reference and use by any developer.

Contribute and enjoy!


[1] https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf

[2] http://cstheory.stackexchange.com/questions/1539/whats-new-in-purely-functional-data-structures-since-okasaki
