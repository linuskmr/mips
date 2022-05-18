use std::{error, fs, io};
use std::io::{BufRead};
use std::str::FromStr;
use mips::{Instance, Instruction, MachineWord, ParseInstructionError};

#[derive(clap::Parser)]
#[clap(author, version, about)]
struct Cli {
	file: Option<String>,

	#[clap(subcommand)]
	command: Commands,
}

#[derive(clap::Subcommand)]
enum Commands {
	/// Assemble a mips assembly file into machine representation.
	Asm,
	Exec,
}

fn main() {
	if let Err(err) = run() {
		println!("ERROR: {}", err);
		let mut err: &dyn error::Error = err.as_ref();
		while let Some(source) = err.source() {
			println!("  caused by: {}", source);
			err = source;
		}
	}
}

fn run() -> Result<(), Box<dyn error::Error>> {
	use clap::Parser;
	let cli: Cli = Cli::parse();

	let input: Box<dyn io::Read> = match cli.file {
		Some(filename) => Box::new(fs::File::open(filename)?),
		None => Box::new(io::stdin()),
	};
	let mut input = io::BufReader::new(input);

	match cli.command {
		Commands::Asm => {
			loop {
				let mut line = String::new();
				let bytes_read = input.read_line(&mut line)?;
				if bytes_read == 0 {
					return Ok(()) // EOF reached
				}
				let instruction = Instruction::from_str(&line)?;
				eprintln!("{:?}", instruction);
				println!("{:032b}", <Instruction as Into<MachineWord>>::into(instruction));
			}
		},
		Commands::Exec => {
			let code: Vec<String> = input.lines()
				.collect::<Result<_, io::Error>>()?;
			let code: Vec<Instruction> = code.into_iter()
				.map(|line| Instruction::from_str(&line))
				.collect::<Result<_, ParseInstructionError>>()?;
			let mut instance = Instance::new(code);
			instance.exec();

			// Dump register values
			println!("{}", instance.registers.iter().enumerate()
				.map(|(nr, val)| format!("{}:{}", nr, val))
				.collect::<Vec<String>>()
				.join(", "));
		}
	}
	Ok(())
}