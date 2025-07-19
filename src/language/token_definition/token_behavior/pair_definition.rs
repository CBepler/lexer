#[derive(PartialEq, Debug)]
pub struct PairDefinition {
    pair_type: PairDirection,
    counterpart_name: String,
}

impl PairDefinition {
    pub fn new(pair_type: PairDirection, counterpart_name: String) -> Self {
        PairDefinition {
            pair_type,
            counterpart_name,
        }
    }

    pub fn get_pair_type(&self) -> &PairDirection {
        &self.pair_type
    }

    pub fn get_counterpart_name(&self) -> &str {
        &self.counterpart_name
    }
}

#[derive(PartialEq, Debug)]
pub enum PairDirection {
    Open,
    Close,
}
