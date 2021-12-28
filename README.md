[![Linux GNU](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/linux_gnu.yml/badge.svg)](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/linux_gnu.yml)
[![Linux Intel](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/linux_intel.yml/badge.svg)](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/linux_intel.yml)
[![Linux NVHPC](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/linux_nvhpc.yml/badge.svg)](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/linux_nvhpc.yml)
[![MacOS GNU](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/macos_gnu.yml/badge.svg)](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/macos_gnu.yml)
[![MacOS Intel](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/macos_intel.yml/badge.svg)](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/macos_intel.yml)
[![Linux GPU NVHPC](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/gpu_nvhpc.yml/badge.svg)](https://github.com/NOAA-GSL/SENA-gf/actions/workflows/gpu_nvhpc.yml)

```
This repository is a scientific product and is not official communication
of the National Oceanic and Atmospheric Administration, or the United States
Department of Commerce. All NOAA GitHub project code is provided on an ‘as
is’ basis and the user assumes responsibility for its use. Any claims against
the Department of Commerce or Department of Commerce bureaus stemming from
the use of this GitHub project will be governed by all applicable Federal
law. Any reference to specific commercial products, processes, or service
by service mark, trademark, manufacturer, or otherwise, does not constitute
or imply their endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and logo of
a DOC bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.
```

# Overview

This repository contains a stand-alone GPU kernel for the Grell-Freitas physics scheme
in the Common Community Physics Package (CCPP)

# Contents

This repository is organized as follows.

### `test/`

Contains the reference test baselines all implementations must use for testing
and validation.

### `ref/`

Contains the source tree for the reference implementation. See the README in that
directory for details about how to run and test it.

### Contributing

Please see the [Contributing Guide](CONTRIBUTING.md) for information about
contributing to this repository.
