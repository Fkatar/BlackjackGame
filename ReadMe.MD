
# Blackjack Haskell Project

### Universidade de Lisboa - Licenciatura em Engenharia Informática
**Course**: Princípios de Programação  
**Project**: 3  

This project implements a Blackjack game in Haskell, allowing a user to play rounds of Blackjack against the house. The program interacts via command-line input, supporting custom deck files and randomized deck generation.

## Table of Contents
- [Overview](#overview)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
- [Features](#features)
- [Testing](#testing)
- [Project Structure](#project-structure)

---

### Overview

The goal of this project is to consolidate knowledge in Haskell by building a command-line Blackjack game. The project includes:
- Modularized game logic
- Command-line and file interactions
- Automated property-based testing with QuickCheck

**Note**: This game is designed for educational purposes and does not support gambling.

### Requirements

- **Haskell Stack**: Required to build and run the Haskell project.
- **QuickCheck**: For running automated tests (included in Stack).

### Installation

1. **Clone this repository** to your local machine:
   ```bash
   git clone https://github.com/Fkatar/BlackjackGame.git
   cd BlackjackGame

   ```
2. **Build the project** with Stack:
   ```bash
   stack build
   ```

### Usage

To start the game, compile and run the `Main.hs` file:

1. **Run the game with a default deck**:
   ```bash
   stack ghc Main.hs
   ./Main default.bar
   ```
   This will load the default deck from `data/default.bar`.

2. **Run the game with a custom deck**:
   ```bash
   ./Main path/to/deck.bar
   ```

3. **Run the game with a randomized deck**:
   ```bash
   ./Main -n X
   ```
   Replace `X` with the number of shuffled decks.

4. **Run tests**:
   ```bash
   ./Main -t
   ```

### Features

1. **Game Rounds**: Play multiple rounds until the deck is exhausted or credits are depleted.
2. **Randomized Decks**: Create a randomized deck with `-n X`.
3. **Custom Decks**: Use `.bar` or `.txt` files to load different decks.
4. **QuickCheck Tests**: Includes automated tests to verify game properties.

### Testing

This project uses QuickCheck to test game logic. Tests are located in `src/Testes.hs` and are run with the `-t` flag:
```bash
./Main -t
```

The tests check:
- Initial hand values are <= 21.
- Credit values after each round match expectations.
- The house hand reaches at least 17 points.

### Project Structure

```plaintext
BlackjackGame/
│
├── src/                      # Haskell source files
│   ├── Main.hs               # Main file for running the game
│   ├── BlackJack.hs          # Game logic
│   └── Testes.hs             # QuickCheck tests
│
├── data/                     # Data files for custom decks
│   ├── default.bar           # Default deck
│   ├── 01_baralho.bar        # Additional deck files
│   ├── 01_input.txt          # Input sequence files for testing
│   └── 01_check.txt          # Expected output files for validation
│
├── bin/                      # Compiled binaries (local use only)
│                               
└──  README.md                 # Project documentation
```



### Authors

This project was completed by Francisco Catarino, Universidade de Lisboa, 2022/2023.
