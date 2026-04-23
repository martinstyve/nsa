# NSA

NSA is a Haskell web application for running pace analysis and guidance.

The site takes a race time and race distance, and shows:

- equivalent race times for standard race distances
- interval pace ranges for ["Norwegian Singles Approach"](https://www.reddit.com/r/NorwegianSinglesRun/wiki/index/) style workouts

This project was created for INF221 Advanced Functional Programming @ UiB (and personal use)

try it out: [nsa.martinstyve.no](https://nsa.martinstyve.no/)

## Features

todo?

## Tech Stack

- Haskell
- Cabal
- Servant
- Lucid
- Megaparsec
- Tasty, HUnit, QuickCheck (tests)

## Run Locally

Requirements:
- GHC and Cabal

Build:

```
cabal build
```

Run:

```
cabal run
```

Open:
- http://localhost:6767

## Run Tests

```
cabal test
```

## Input Format

- Time:
	- mm:ss (example: 18:30)
	- h:mm:ss (example: 1:24:10)
- Distance:
	- preset values from the dropdown
	- custom distance in meters (1 to 50000 for now)

## Personal note

university relevant document in `docs`, listed in `.gitignore`
