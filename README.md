# [Advent of Code](http://adventofcode.com/) 2019!

Year four!

My goal for this year is simply to enjoy some non-work-related coding: I'll still be using Haskell but I won't cry if one day I'll use another language to complete a challenge, rather than leaving it there.

## IDE

No IDE this year! Only Sublime Text, `ghci` and lots of lookups on [Hoogle](https://www.haskell.org/hoogle/).

## Running the code

To run the code for a specific day, load it up in `ghci`, then call the `part1` or `part2` function:
```bash
# from 'code' folder, so we can access the 'Common' module
$ ghci day42/day42
GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
*Main> part1
42
*Main> part2
"42 is the answer"
```

The code could also be compiled with ghc for improved run-time performance:
```bash
# from 'code' folder, so we can access the 'Common' module
$ ghc day42/day42
$ cd day42
$ ./day42
Solving..
Part 1: 42
Part 2: "42 is the answer"
```

## Journal

Just like the past years, I will keep a journal of the most challenging/interesting things I'll learn during this journey. 

[What I learned 📖](what-i-learned.md)

## Completed challenges

| Day | Problem | Solution | Stars | Notes |
|-----|---------|----------|-------|-------|
| Day  1 | [The Tyranny of the Rocket Equation](https://adventofcode.com/2019/day/1) | [day1.hs](./code/day1/day1.hs) | ⭐️🌟 | |