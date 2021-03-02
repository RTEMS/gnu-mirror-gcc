# microsoft/gcc

This repository is a fork of the [GCC repository](https://gcc.gnu.org/git/gcc.git) for Microsoft internal scenarios. **Consumption of packages produced in this repo for any other scenario is not recommended or supported.**

Integration from the upstream GCC repository will occur regularly. Changes made in this repository will be contributed back upstream through the [existing GCC contribution process](https://gcc.gnu.org/contribute.html). PR requests from the external community will not be accepted as this repository isn't meant to be another mechanism to check into the GCC repository.

This repository also contains scripts and workflow YAML files to build and test GCC using GitHub Actions. See [Scripts and Workflows](#scripts-and-workflows) for more details.

## Active Branches: 

| Branch | Status | Description |
| --- | --- | --- |
| current | [![current Status](https://github.com/microsoft/gcc/actions/workflows/build/badge.svg?branch=current)](https://github.com/microsoft/gcc/actions?query=workflow%3Abuild+branch%3Acurrent) | Points to the [master branch](https://gcc.gnu.org/git/?p=gcc.git;a=shortlog;h=refs/heads/master) in the upstream GCC repository. This will be updated regularly.
| gcc-9.1.0 | [![gcc-9.1.0 Status](https://github.com/microsoft/gcc/actions/workflows/build/badge.svg?branch=gcc-9.1.0)](https://github.com/microsoft/gcc/actions?query=workflow%3Abuild+branch%3Agcc-9.1.0) | Points to [releases/gcc-9.1.0 tag](https://gcc.gnu.org/git/?p=gcc.git;a=shortlog;h=releases/gcc-9.1.0) in the upstream GCC repository.

## Scripts and Workflows:

Scripts and YAML files under the [.github](.github) directory leverage GitHub Actions to build and test GCC in PR and CI builds. These are available for anyone to use ([released under an MIT license](.github/LICENSE.txt)). The host and target of the workflows is **x86_64-pc-linux-gnu**. Future improvements will come to build on more hosts for more targets.

| Workflow | Description |
| --- | --- |
| [build.yaml](.github/workflows/build.yaml) | Builds GCC |
| [test-gcc.yaml](.github/workflows/test-gcc.yaml) | Runs GCC tests in parallel across multiple machines |

## Releases:
*For Microsoft internal scenarios consumption only. TBD with more details*

