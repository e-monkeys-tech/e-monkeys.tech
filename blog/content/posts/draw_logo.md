+++
title = "Draw logo of e-monkeys.tech with love & cairo-rs"
slug = "logo"
+++

**cairo-rs** is a rust binding of [cairo](https://www.cairographics.org/)

The following dependencies are needed in your **Cargo.toml**

```toml
[dependencies]
png = "0.11.0"
cairo-rs = { version = "0.2.0", features = ["png"] }
```

Procedural generation of our logo with cairo-rs and this following code

```rust
use cairo::{ Context, Format, ImageSurface, FontSlant, FontWeight };
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    let surface = ImageSurface::create(Format::ARgb32, 600, 600).expect("Couldn’t create surface");
    let context = Context::new(&surface);
        context.scale(400f64, 400f64);
        context.select_font_face("Sans", FontSlant::Normal, FontWeight::Normal);
        context.set_font_size(0.25);
        context.move_to(0.04, 0.53);
        context.show_text("e-monkeys");
        context.move_to(0.27, 0.65);
        context.text_path("tech");
        context.set_source_rgb(0.5, 0.5, 1.0);
        context.fill_preserve();
        context.set_source_rgb(0.0, 0.0, 0.0);
        context.set_line_width(0.01);
        context.stroke();
        context.set_source_rgba(1.0, 0.2, 0.2, 0.6);
        //context.arc(0.04, 0.53, 0.02, 0.0, PI * 2.);
        context.arc(0.27, 0.65, 0.02, 0.0, PI * 2.);
        context.fill();

        let mut file = File::create("data/e-monkeys.png")
        .expect("Couldn’t create file"); 
        surface.write_to_png(&mut file)
        .expect("Couldn’t write to png");
}
```

Then, we can run or build this code and see the result

```bash
cargo run --release
```

<img src="/posts/e-monkeys.png" alt="e-monkeys.tech logo"/>