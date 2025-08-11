pub trait IDConstructable {
    fn new(id: usize) -> Self;
}

pub struct StateConstructor {
    next_id: usize,
}

impl StateConstructor {
    pub fn new() -> Self {
        StateConstructor { next_id: 0 }
    }

    pub fn new_state<T>(&mut self) -> T
    where
        T: IDConstructable,
    {
        let id = self.next_id;
        self.next_id += 1;
        T::new(id)
    }
}
