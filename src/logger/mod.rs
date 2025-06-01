use colored::Colorize;

pub struct Logger {}

impl Logger {
    pub fn debug<T: Into<String>>(msg: T) {
        println!("{} {}", "[DEBUG]".blue(), msg.into());
    }
}
