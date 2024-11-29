use bitvec::vec::BitVec;

#[derive(Default, Clone, Debug)]

pub struct StackState {
    bytes: BitVec,
}

const REGISTER_WIDTH: usize = 8;

impl StackState {
    pub fn reset(&mut self) {
        self.bytes.clear();
    }

    /// Allocate `size` bytes on the stack and return the offset from the bottom of the stack (in bytes)
    /// Note that if this address should be subtracted from the frame pointer, since the RISC-V stack grows down
    pub fn alloc(&mut self, size: usize) -> i32 {
        let mut first_free_ind = None;
        let mut offset_size = None;
        for (i, is_free) in self.bytes.iter().enumerate() {
            if *is_free {
                if first_free_ind.is_none() {
                    first_free_ind = Some(i)
                }
            } else {
                if let Some(offset) = first_free_ind {
                    let cur_block_size = i - offset;
                    let block_fits_better = offset_size
                        .map(|(_, best_block_size)| cur_block_size < best_block_size)
                        .unwrap_or(true);
                    if cur_block_size >= size && block_fits_better {
                        offset_size = Some((i, cur_block_size))
                    }
                }
                first_free_ind = None;
            }
        }

        if let Some((offset, size)) = offset_size {
            for i in offset..offset + size {
                self.bytes.set(i, false);
            }
            offset as i32
        } else {
            let offset = self.bytes.len();
            self.bytes.extend((0..size).map(|_| false));
            offset as i32
        }
    }

    pub fn free(&mut self, offset: usize, size: usize) {
        for i in offset..offset + size {
            self.bytes.set(i, true);
        }
    }

    fn size(&self) -> usize {
        self.bytes.len()
    }
}
