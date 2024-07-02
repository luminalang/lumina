use super::{BranchOf, DecTree, IsReachable, Merger, REACHABLE};
use ibig::IBig;
use lumina_typesystem::Bitsize;
use std::fmt;

#[derive(Clone, Debug)]
pub struct Range {
    // pub min: IBig,
    // pub max: IBig,
    pub start: IBig,
    pub end: IBig,
}

impl PartialEq for Range {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.end == other.end
    }
}

impl Range {
    pub fn full(min: &IBig, max: &IBig) -> Range {
        Range { start: min.clone(), end: max.clone() }
    }

    pub fn start_up_to(&self, end: &IBig) -> Range {
        Range { start: self.start.clone(), end: end.clone() }
    }

    pub fn end_down_to(&self, start: &IBig) -> Range {
        Range { start: start.clone(), end: self.end.clone() }
    }

    pub fn max(neg: bool, bitsize: Bitsize) -> Range {
        todo!();
    }
}

impl<'a> Merger<'a> {
    pub fn merge_int(
        &mut self,
        i: usize,
        ints: &mut BranchOf<Range>,
        mut orange: Range,
        onext: DecTree,
    ) -> IsReachable {
        let Some((range, _, next)) = ints.branches.get_mut(i) else {
            ints.branches.push((orange, 0, onext));
            return REACHABLE;
        };

        if *range == orange {
            // println!("merging exact match {range} == {erange}");
            return self.merge(next, onext);
        }

        let zero_overlap = orange.end < range.start || orange.start > range.end;
        if zero_overlap {
            // println!("{erange} has zero overlap with {range}");
            return self.merge_int(i + 1, ints, orange, onext);
        }

        // println!("{erange} has overlap with {range}");

        let early_start = orange.start < range.start;
        let early_end = orange.end < range.end;

        let late_start = orange.start > range.end;
        let late_end = orange.end > range.end;

        // we can perform the merge now because we know at least *some* of the existing range is overlapping
        let old_next = next.clone();
        let next_reachable = self.merge(next, onext.clone());

        let mut reachable = false;

        // when either side of the new range is further to the edge than the existing range
        // we split out the parts of the new range that aren't overlapping and merge them dependently
        {
            // 2..8 into 4..8
            if early_start {
                let mut early_portion = orange.start_up_to(&range.start);
                early_portion.end -= 1;
                orange.start = range.start.clone();
                // println!("early portion: {early_portion}");
                reachable |= self.merge_int(i + 1, ints, early_portion, onext.clone());
            }
            let (range, _, _) = &mut ints.branches[i];

            // 4..10 into 4..8
            if late_end {
                let mut late_portion = orange.end_down_to(&range.end);
                late_portion.start += 1;
                orange.end = range.end.clone();
                // println!("late portion: {late_portion}");
                reachable |= self.merge_int(i + 1, ints, late_portion, onext.clone());
            }
        }
        let (range, _, _) = &mut ints.branches[i];

        // when either side of the new range is inside of the bounds of the existing range
        // we split the existing range to match the bounds of the new range and then re-push the
        // portions that weren't overlapping with the old un-merged continuation
        {
            // 4..8 into 4..10
            if early_end {
                let mut split_right_side = range.end_down_to(&orange.end);
                split_right_side.start += 1;
                range.end = orange.end;
                // println!("split_right_side: {split_right_side}");
                let _ = self.merge_int(i + 1, ints, split_right_side, old_next.clone());
            }
            let (range, _, _) = &mut ints.branches[i];

            // 6..10 into 4..10
            if late_start {
                let mut split_left_side = range.start_up_to(&orange.start);
                split_left_side.end -= 1;
                range.start = orange.start;
                // println!("split_left_side: {split_left_side}");
                let _ = self.merge_int(i + 1, ints, split_left_side, old_next.clone());
            }
        }

        next_reachable || reachable
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.start == self.end {
            self.start.fmt(f)
        } else {
            self.start.fmt(f)?;
            "..".fmt(f)?;
            self.end.fmt(f)
        }
    }
}
