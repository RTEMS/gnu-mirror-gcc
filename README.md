# microsoft/gcc

This repository is a mirror of the Microsoft vendor branches in the [GCC repository](https://gcc.gnu.org/git/gcc.git) and is intended specifically for Microsoft internal scenarios. **Consumption of packages produced in this repository for any other scenario is not recommended or supported.** For a list of mirrored branches, see [Mirrored Branches](#mirrored-branches).

This repository also contains scripts and workflow YAML files to build and test GCC using GitHub Actions that can be used by others in the GCC community. See [Scripts and Workflows](#scripts-and-workflows) for more details.

This repository is not meant to be another mechanism for GCC developers to check into the GCC repository. Pull requests from the external community will not be accepted. Microsoft developers will follow the [existing GCC contribution process](https://gcc.gnu.org/contribute.html) and will submit patches to the GCC repository. They will run testing in this GitHub repository to verify that Microsoft scenarios are still working. 

We intend to work with the GCC community to integrate any changes we make in our vendor branches back upstream to the corresponding GCC release branches and master where appropriate.

[Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct)

## Mirrored Branches: 

| Branch | Status | Description |
| --- | --- | --- |
| current | [ [![build](https://github.com/microsoft/gcc/actions/workflows/build.yaml/badge.svg?branch=current)](https://github.com/microsoft/gcc/actions/workflows/build.yaml) [![test-gcc](https://github.com/microsoft/gcc/actions/workflows/test-gcc.yaml/badge.svg?branch=current)](https://github.com/microsoft/gcc/actions/workflows/test-gcc.yaml) | Mirrors the *vendors/microsoft/main branch* in the upstream GCC repository. That vendor branch is based off the [GCC master branch](https://gcc.gnu.org/git/?p=gcc.git;a=shortlog;h=refs/heads/master).
| gcc-9.1.0 | [![build](https://github.com/microsoft/gcc/actions/workflows/build.yaml/badge.svg?branch=gcc-9.1.0)](https://github.com/microsoft/gcc/actions/workflows/build.yaml) [![test-gcc](https://github.com/microsoft/gcc/actions/workflows/test-gcc.yaml/badge.svg?branch=gcc-9.1.0)](https://github.com/microsoft/gcc/actions/workflows/test-gcc.yaml) | Mirrors the *vendors/microsoft/9.1.0 branch* in the upstream GCC repository. That vendor branch is based off the [releases/gcc-9.1.0 tag](https://gcc.gnu.org/git/?p=gcc.git;a=shortlog;h=releases/gcc-9.1.0).

## Scripts and Workflows:

The scripts and YAML files under the [.github](.github) directory leverage GitHub Actions to build and test GCC in pull requests and continuous integration builds. These are available for anyone to use ([released under an MIT license](.github/LICENSE.txt)). The host and target of the workflows is **x86_64-pc-linux-gnu**. Future improvements will come to build on more hosts for more targets.

| Workflow | Description |
| --- | --- |
| [build.yaml](.github/workflows/build.yaml) | Builds GCC |
| [test-gcc.yaml](.github/workflows/test-gcc.yaml) | Runs GCC tests in parallel across multiple machines |

## Releases:
*For Microsoft internal scenarios consumption only. TBD with more details*
