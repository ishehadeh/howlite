use std::collections::{HashMap, VecDeque};

use super::Value;

#[derive(Default, Clone, Debug)]
pub struct ScopeManager {
    stack: VecDeque<HashMap<String, Value>>,
}

impl ScopeManager {
    pub fn new() -> ScopeManager {
        ScopeManager::default()
    }

    pub fn get<K: AsRef<str>>(&self, key: K) -> Option<&Value> {
        let key_ref = key.as_ref();
        self.stack.iter().rev().find_map(|m| m.get(key_ref))
    }

    pub fn must_get<K: AsRef<str>>(&self, key: K) -> &Value {
        let key_ref = key.as_ref();
        match self.get(key_ref) {
            Some(v) => v,
            None => panic!("failed to find key in scope: {}", key_ref),
        }
    }

    pub fn push(&mut self) {
        self.stack.push_back(HashMap::new())
    }

    pub fn pop(&mut self) {
        self.stack.pop_back();
    }

    pub fn set<K: Into<String>>(&mut self, key: K, value: Value) -> Option<Value> {
        let key_s = key.into();
        match self.stack.back_mut() {
            Some(m) => m.insert(key_s, value),
            None => {
                self.push();
                self.stack[0].insert(key_s, value)
            }
        }
    }

    pub fn unset<K: AsRef<str>>(&mut self, key: K) -> Option<Value> {
        let key_ref = key.as_ref();
        self.stack.iter_mut().rev().find_map(|m| m.remove(key_ref))
    }

    pub fn current_scope_values<'a>(&'a self) -> impl Iterator<Item = &'a Value> {
        self.stack.back().into_iter().flat_map(|x| x.values())
    }

    pub fn all_values<'a>(&'a self) -> impl Iterator<Item = &'a Value> {
        self.stack.iter().flat_map(|x| x.values())
    }
}
