# Blackstar
A black hole ray tracer written in Haskell. There's [an article](https://flannelhead.github.io/projects/blackstar.html) about this on my homepage. I've also written a [theoretical writeup](https://flannelhead.github.io/posts/2016-03-06-photons-and-black-holes.html) on Schwarzschild geodesics.

![An example image](https://raw.githubusercontent.com/flannelhead/blackstar/master/example.png)

## Features
* Fast, parallel ray tracing
* Rendering [Schwarzschild](https://en.wikipedia.org/wiki/Schwarzschild_metric) black holes
* Rendering accretion disks
* Drawing the celestial sphere using a star catalogue
* Bloom effect
* Antialiasing by 4x supersampling for smoother images
* Easy, YAML based configuration
* A simple CLI
* Batch mode and sequence generator for creating animations

## What about the name?
It is a tribute to David Bowie, referring to his last album.

## Building
First, install [LLVM 8.0.0](https://llvm.org/releases/download.html#8.0.0) on your operating system and ensure it is available in `PATH`. This is required by the [Accelerate](https://www.acceleratehs.org/) library, which is used for the computationally intense parts of the code.

Use [`stack`](https://docs.haskellstack.org/en/stable/README/) to build this. First clone the repo, then run `stack build` and follow the instructions given by `stack`. You should be able to build `blackstar` on any platform where you can install `stack`.

You must download the [PPM star catalog](http://tdc-www.harvard.edu/software/catalogs/ppm.html) to generate the star map. Download [this archive](http://tdc-www.harvard.edu/software/catalogs/ppm.tar.gz) and extract the file `PPM` to the root folder of this project. Then run `stack exec generate-starmap PPM` and the map will be generated and saved.

## Usage
When `blackstar` has been built with `stack`, you can run it with
```
stack exec blackstar -- [-p|--preview] [-f|--force] [-o|--output=PATH] [-s|--starmap=PATH] SCENENAME
```
Notice the two dashes (`--`) which are required to terminate `stack`'s argument list.


`cabal` users can run `blackstar` by executing
```
cabal run -- [OPTIONS] SCENENAME
```
in the root folder of the project.

Scenes are defined using YAML config files. Look in the `scenes` folder for examples. To render the `default` scene to the directory `output`, run
```
stack exec blackstar -- scenes/default.yaml --output output
```
in the root directory of the project. The `--output` flag specifies the output directory. By default, `blackstar` searches for a starmap in the path `./stars.bin.gz`, but a different path can be specified using the `--starmap` flag.

The rendered files are named `scenename.png` and `scenename-bloomed.png`. The `--preview` flag can be used to render small-sized previews of the scene while adjusting the parameters. The `--force` flag will cause `blackstar` to overwrite output images without a prompt.

If a directory is given as the input scene path, `blackstar` searches non-recursively for YAML files in that directory and tries to render them. The scenes are placed in the specified output directory.

There's also a help text which can be seen by running
```
stack exec blackstar -- --help
OR
cabal run -- --help
```

Better images can be achieved by rendering larger than the target size and then scaling down (some antialiasing is achieved). This is called supersampling and is implemented in `blackstar`. It can be enabled by setting `supersampling` to true in the YAML config file &mdash; see `scenes/default-aa.yaml` for an example.

## Animation
There is a separate YAML config format for specifying animations. For example, see [default-ani.yaml](animations/default-ani.yaml).

In the first pass, the animation file must be rendered into separate config files for each frame. The `animate` executable takes care of this. First, create a directory where the frame config files will be put.
```
mkdir frames
```
Then run `animate`:
```
stack exec animate -- animations/default-ani.yaml -o frames
```
Now you should find quite a bunch of `.yaml` files in the folder `frames`.

Make another folder for the output frames:
```
mkdir frames-out
```
Now you will be able to run `blackstar` in batch mode to render the frames:
```
stack exec blackstar -- frames -o frames-out
```
This will take quite a while.

After the frames have been rendered, generate a video from the `*.png` still with your utility of preference. You can also use my script `scripts/ffmpeg-animate`, which uses `ffmpeg`. You only need to give it the prefix of the numbered frames:
```
scripts/ffmpeg-animate frames-out/default-ani
```
The output video will be rendered to `out.mkv`.

## Profiling
Thanks to `stack`, profiling is incredibly easy. Rebuild `blackstar` by running
```
stack build --profile
```
and then run it with
```
stack exec --profile blackstar -- scenes/default.yaml -o output +RTS -p
```
The profile will be generated to `blackstar.prof`.

## TODO
As always, there's a plenty of room for improvement. For example:

* Animation: mathematically rigorous non-stationary observer
* Arbitrary textures for accretion disk (or some cool noise generator)
* Redshifting of the accretion disk
* Preview / scene planner GUI ([fltkhs](https://hackage.haskell.org/package/fltkhs))

Pull requests are welcome! If you find some cool scenes, I'd appreciate if you contributed them to this repository.

