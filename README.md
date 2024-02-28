# DRACO

[![License](https://img.shields.io/github/license/grimme-lab/DRACO)](https://github.com/grimme-lab/DRACO/blob/main/LICENSE)
[![Latest Version](https://img.shields.io/github/v/release/grimme-lab/DRACO)](https://github.com/grimme-lab/DRACO/releases/latest)
[![DOI](https://img.shields.io/badge/DOI-10.1021/acs.jpclett.3c03551-blue)](https://doi.org/10.1021/acs.jpclett.3c03551)

## Introduction

<div align="center">
<img src="./toc.webp" alt="Table of contents graphic for DRACO" width="300">
</div>

This is an Open Source framework for the Dynamic Radii Adjustment for COntinuum solvation (DRACO) approach. 
DRACO employs precomputed atomic partial charges and coordination numbers of the solute atoms to improve the solute cavity.
As such, DRACO is compatible with major solvation models, improving their performance significantly and robustly at virtually no extra cost, especially for charged solutes.

The method is published in [The Journal of Physical Chemistry Letters](https://doi.org/10.1021/acs.jpclett.3c03551).

## Building

You can use the release binary of DRACO, or build your own version from source (see below).
To do that, you need to clone the repository to your local environment.
```
git clone https://github.com/grimme-lab/CPCM-X.git
cd CPCM-X
```
The recommended version for building this project is using Meson.


### Building with Meson
To be able to build this project with Meson, you need version 0.60 or newer.
To use the default backend, you also need ninja in version 1.7 or newer.
```
meson setup build --buildtype release
ninja -C build
```
You afterwards have to manually install the created binary in the path of your choice.

## Using DRACO as a standalone

Once you obtained a functioning binary (either through building or by downloading a release binary), you can take a look at the possible options by invoking
```
draco --help
```
DRACO comes with an in-built interface to two efficient charge models (EEQ and CEH) via the light-weight tight-binding framwork [``tblite``](https://github.com/tblite/tblite).
A simple dynamic radii evulation is evoked, e.g. as
```
draco h2o.xyz --solvent water
```
Let's assume your xyz structure file looks like this:
```
3

o            0.00000000000000       -0.06365718653467       -0.00000000010001
h            0.77223547167307        0.50524694437309        0.00000000070010
h           -0.77223547197311        0.50524694397304        0.00000000030004
```
For this molecule, your output (default radii: cpcm, default qmodel: ceh) should look like this
```
           -------------------------------------------------
          |              Structure Information              |
           -------------------------------------------------

             Identifier  Partial Charge   Radii (unscaled)

                 O1          -0.29         1.16   (1.82)
                 H2           0.14         1.32   (1.32)
                 H3           0.14         1.32   (1.32)

```

###Setting up a QC calculation using DRACO's dynamic radii

After you obtained the dynamic radii from a DRACO calculation, you can use them in any QC program of your choice, that allows the usage of custom radii for the implicit solvent calculation.

Additionally, DRACO offers the possibility to automatically modify input files for the two QC program packages [ORCA](https://orcaforum.kofo.mpg.de/app.php/portal) and [TurboMole](https://www.turbomole.org/). To use this feature, you need to setup your input file as usual and afterwards invoke DRACO with e.g.
```
draco h2o.xyz --solvent water --prog orca inp_h2o
```

Given the following input file for a calculation with the CPCM solvation model using the ORCA program, DRACO will automatically rename the original file to ``inp_h2o.original`` and modify the respective input file by adding the dynamic radii.

```
%MaxCore 4000
! r2SCAN-3c RIJCOSX def2/jk defgrid2 cpcm(water)

* xyzfile 0 1 h2o.xyz
```
The resulting ``inp_h2o`` input file will look like this
```
%MaxCore 4000
! r2SCAN-3c RIJCOSX def2/jk defgrid2 cpcm(water)

* xyzfile 0 1 h2o.xyz

 %cpcm
  AtomRadii(0,  1.159790568642)
  AtomRadii(1,  1.318156186766)
  AtomRadii(2,  1.318156186761)
 end
```

This functionality also works for TurboMole and its corresponding ``control`` files.


## Using DRACO as a library

DRACO is also build as a Fortran library and can therefore be easily included into Fortran projects by including the ``DRACO``type into your Fortran code. An in-depth example of this will be added in the near future. If you want to implement the DRACO approach into your Fortran code in the mean time, we are happy to help.




