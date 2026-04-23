# Contributing to RCTRecruit

Welcome! We appreciate your interest in contributing to `RCTRecruit`. This document outlines the process for interacting with the project, reporting issues, and submitting code improvements.

## 1. Reporting Bugs and Issues
If you encounter a bug, unexpected behavior, or a failure in the predictive functions, please report it using the GitHub Issues tracker.

Before opening a new issue, please check the existing issues to ensure it has not already been reported. When opening a new issue, include:
* A brief, descriptive title.
* A clear description of the issue and the expected behavior.
* A minimal reproducible example (reprex) containing the R code that triggered the error.
* The output of `sessionInfo()` to provide your R environment details.

[Open a new issue here](https://github.com/imalagaris/RCTRecruit/issues)

## 2. Requesting Support or Features
If you need help using the package or would like to suggest a new feature, please use the GitHub Issues tracker and tag your issue with "enhancement" or "question". 

For general usage questions, please ensure you have reviewed the package vignette (`browseVignettes("RCTRecruit")`) prior to submitting a request.

## 3. Contributing Code
We welcome external code contributions, including bug fixes, documentation improvements, and new features. To contribute code, please follow the standard GitHub Pull Request (PR) workflow:

1. **Fork the repository** to your own GitHub account.
2. **Clone the fork** to your local machine.
3. **Create a new branch** for your feature or bug fix (`git checkout -b feature/your-feature-name`).
4. **Make your changes**, ensuring that your code adheres to standard R style guidelines.
5. **Run tests:** Ensure that your changes do not break existing functionality by running the local test suite using `devtools::test()`. If adding new functionality, please add corresponding tests to the `tests/` directory.
6. **Commit your changes** with clear, descriptive commit messages.
7. **Push the branch** to your forked repository.
8. **Open a Pull Request** against the `main` branch of the original `RCTRecruit` repository.

A maintainer will review your pull request, provide feedback, and merge the changes once approved.