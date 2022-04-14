use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {
    num: usize,
}
#[derive(Debug, StructOpt)]
enum OptCargoPlugin {
    Muttest(Opt),
}
impl From<OptCargoPlugin> for Opt {
    fn from(OptCargoPlugin::Muttest(x): OptCargoPlugin) -> Self {
        x
    }
}

fn main() {
    let is_cargo_plugin = std::env::var_os("CARGO").is_some();
    let opt = if is_cargo_plugin {
        OptCargoPlugin::from_args().into()
    } else {
        Opt::from_args()
    };
    println!("{:?}", opt.num);

    // let mut x = Command::new("cargo").arg("test").spawn().unwrap();
    // x.wait().unwrap();
}
