# blackstar
A black hole ray tracer written in Haskell.

![An example image](https://raw.githubusercontent.com/flannelhead/blackstar/master/example.png)

## Goals
* Fast, parallel ray tracing
* Render [Schwarzschild](https://en.wikipedia.org/wiki/Schwarzschild_metric) black holes
* ~~Gracefully deal with coordinate singularities - maybe switch to Cartesian coordinates?~~ Now using Cartesian coordinates for Schwarzschild
* Render accretion disks
* Use a star catalog for the celestial sphere
* Produce wallpaper quality material by smoothing the images
* ~~Use [automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation) to compute the [Christoffel symbols](https://en.wikipedia.org/wiki/Levi-Civita_connection#Christoffel_symbols) from a user-supplied metric~~ As an idea this is fine, but it was painfully slow. Probably will return to that some day

## What about the name?
It is a tribute to David Bowie, referring to his last album.

## Building
Use [`stack`](http://docs.haskellstack.org/en/stable/README/) to build this. First clone the repo, then run `stack build` and follow the instructions given by `stack`. You should be able to build `blackstar` on any platform where you can install `stack`.

This repository includes a star lookup tree (`stars.kdt`), which has been generated from the [PPM star catalog](http://tdc-www.harvard.edu/software/catalogs/ppm.html). The prebuilt tree in binary form is included for convenience, but you can also build it yourself. First, remove `stars.kdt`. Download [this archive](http://tdc-www.harvard.edu/software/catalogs/ppm.tar.gz) and extract the file `PPM` to the root folder of this project. Then run `blackstar` and the tree should be automatically generated and saved.

## Usage
When `blackstar` has been built with `stack`, you can run it with
```
stack exec blackstar -- [-p|--preview] [scenename]
```
Notice the two dashes (`--`) which are required to terminate `stack`'s argument list.

Scenes are defined using YAML config files. Look in the `scenes` folder for examples. `blackstar` looks for scenes under the `scenes` folder, so you'll have to put your scenes there, too. The scene file name should be passed to `blackstar` without the `.yaml` ending. If no scene name is passed, `blackstar` will render the `default` scene. The example image is exactly this scene.

The rendered files go into the folder `output`, named `scenename.png` and `scenename-bloomed.png`. The `--preview` flag can be used to render small-sized previews of the scene while adjusting the parameters.

Better images can be achieved by rendering larger than the target size and then scaling down (some antialiasing is achieved).

## Profiling
Thanks to `stack`, profiling is incredibly easy. Rebuild `blackstar` by running
```
stack build --profile
```
and then run it with
```
stack exec blackstar -- +RTS -p
```
The profile will be generated to `blackstar.prof`.

## Implementation
[`JuicyPixels`](http://hackage.haskell.org/package/JuicyPixels) and [`repa`](http://hackage.haskell.org/package/repa) were used for fast, parallel computation of the image.

[`kdt`](https://hackage.haskell.org/package/kdt) was used for fast lookups into a star catalog. I had to customise the library just a tiny bit to be able to serialize and store the trees, so currently `stack` will download and use [my fork of `kdt`](https://github.com/flannelhead/kdt).

## Things I've learnt
* Using explicit `Double` datatypes instead of polymorphism via the `Floating` typeclass can make a huge difference in terms of speed
* Automatic differentiation is a *really* elegant idea but comes with an overhead
* How to spell Schwarzschild correctly

## Inspiration
This project was heavily inspired by [this excellent article](http://rantonels.github.io/starless/) and the [Python code](http://github.com/rantonels/starless) by [rantonels](https://github.com/rantonels). Without him, this project would never have born. You'll most certainly notice some similarities but also differences.

The movie Interstellar did an excellent rendition of a black hole. They also published a [paper](http://iopscience.iop.org/article/10.1088/0264-9381/32/6/065001) describing their discoveries.

This project was started when I was taking a general relativity course. On the course, [Sean Carroll's lecture notes](http://arxiv.org/pdf/gr-qc/9712019.pdf) were followed and proved very helpful for me.

## TODO
As always, there's a plenty of room for improvement. For example:

* Implement supersampling
* Learn more about GHC and optimize the heck out of this
* Faster Gaussian blur (a minor thing but become restrictive on larger image sizes)
* Better progress reporting (might be hard to do)

Pull requests are welcome! If you find some cool scenes, I'd appreciate if you contributed them to this repository.
