#![feature(proc_macro_hygiene, decl_macro)]
#![feature(plugin)]

#[macro_use] extern crate rocket;
#[macro_use] extern crate serde_derive;

extern crate serde_json;

//use rocket_contrib::serve::StaticFiles;
use rocket_contrib::templates::{Template, handlebars};

use handlebars::{Helper, Handlebars, Context, RenderContext, Output, HelperResult, JsonRender};

use std::collections::HashMap;

#[derive(Serialize)]
struct TemplateContext {
    title: &'static str,
    name: Option<String>,
    items: Vec<&'static str>,
    // This key tells handlebars which template is the parent.
    parent: &'static str,
}

fn main() {
    rocket::ignite()
        //.mount("/", StaticFiles::from(concat!(env!("CARGO_MANIFEST_DIR"), "/static")))
        .mount("/", routes![index])
        .attach(Template::fairing())
        .launch();
}

#[rocket::get("/")]
fn index() -> Template {
    Template::render("index", &TemplateContext {
        app: "e-monkeys.tech",
        author: "pbackz",
        description: "Site principal e-monkeys.tech",
    })
}