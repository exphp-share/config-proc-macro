#[macro_use]
extern crate rsp2_config_proc_macro;

#[config_type]
#[derive(Debug, Clone, PartialEq, Eq)]
struct Foo { a: i32, b: String }

#[config_type]
#[derive(Debug, Clone, PartialEq, Eq)]
struct Empty { }

#[config_type]
#[derive(Debug, Clone, PartialEq, Eq)]
struct Tuple(i32, String);

#[config_type]
#[derive(Debug, Clone, PartialEq, Eq)]
struct Newtype(i32);

#[config_type]
#[derive(Debug, Clone, PartialEq, Eq)]
struct Unit;
