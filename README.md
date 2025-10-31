# Faculty Digital Archive CLI

## Requirements

- [Cabal](https://www.haskell.org/cabal/)
- The `sqlite3` C library

## Setup & Running

This project uses a local SQLite database.

1.  **Build and Run:**
    Use Cabal to build and run the application.

    ```bash
    cabal run archive-cli
    ```

2.  **Database Creation:**
    The first time you run the app, it will automatically create a file named `archive.db` in the root directory.

The application will then start, and you can interact with it via the command-line menu.
