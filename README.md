# `imghcprof`

Graphical viewer for GHC `.prof` files

![demo](./img/demo.png)

## Usage

```
$ imghcprof <input.prof>
```

### Tips

- Right click on table header to customize columns
- Drag table header to reorder columns
- Right click on any row to open context menu
- Drag window title to move tabs around

## Features

- Fast. Can parse and display ~230MB `.prof` file in under two seconds on my laptop

## Installation

### With `nix`

Prerequisites:
- Enabled flakes

```
$ nix run github:t4ccer/imghcprof [INPUTS...]
```

### With `cargo`

Prerequisites:
- `SDL2`
- `pkg-config`

```
$ cargo build
```
