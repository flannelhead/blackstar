# blackstar
A black hole ray tracer written in Haskell. This is a work in progress.

## Goals
* Parallel ray tracing
* Be fast
* Implement the real 4D geodesic equations
* ~~Use [automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation) to compute the [Christoffel symbols](https://en.wikipedia.org/wiki/Levi-Civita_connection#Christoffel_symbols) from a user-supplied metric~~ As an idea this is fine, but it was painfully slow. Probably will return to that some day
* Render [Schwarzschild](https://en.wikipedia.org/wiki/Schwarzschild_metric) and [Kerr](https://en.wikipedia.org/wiki/Kerr_metric) black holes
* Render accretion disks
* Produce wallpaper quality material by smoothing the images

## Implementation
[Repa](https://hackage.haskell.org/package/repa) is a good candidate for the parallel processing part.

## Building
Use [stack](http://docs.haskellstack.org/en/stable/README/) to build this. First clone the repo, then run `stack build` and follow the instructions. The application can be then run by calling `stack exec blackstar`.

## Inspiration
This project was heavily inspired by [this excellent article](http://rantonels.github.io/starless/) and the [Python code](http://github.com/rantonels/starless).

The movie Interstellar did an excellent rendition of a black hole. They also published a [paper](http://iopscience.iop.org/article/10.1088/0264-9381/32/6/065001) describing their discoveries.

This project was started when I was taking a general relativity course. On the course, [Sean Carroll's lecture notes](http://arxiv.org/pdf/gr-qc/9712019.pdf) were followed and proved very helpful for me.

## What about the name?
It is a tribute to David Bowie, referring to his last album.
