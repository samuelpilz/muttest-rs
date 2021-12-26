use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {}

fn main() {
    let opt = Opt::from_args();
    println!("{:?}", opt);
}
