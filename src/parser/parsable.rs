pub trait Parsable {
    fn get_name(&self) -> &String;
    fn get_match(&self) -> &Option<String>;
    fn get_row(&self) -> usize;
    fn get_col(&self) -> usize;
}
