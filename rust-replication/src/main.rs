use std::alloc::{alloc, Layout};

fn main() {
    one();
}

fn one() {
    two();
}

fn two() {
    unsafe {
        let info = generate_unwind_info();
        dbg!(&info.name);
    }
}

extern "C" {
    fn unw_getcontext(cursor: *mut u8) -> i32;
    fn unw_init_local(cursor: *mut u8, context: *mut u8) -> i32;
    fn unw_init_remote(cursor: *mut u8, addr_space: *mut u8, void: *mut u8) -> i32;
    fn unw_step(cursor: *mut u8) -> i32;
    fn unw_get_proc_info(cursor: *mut u8, outinfo: *mut unw_proc_info) -> i32;
    fn unw_get_proc_name(cursor: *mut u8, charptr: *mut u8, size: usize, offp: *mut u32) -> i32;
}

#[repr(C)]
struct unw_proc_info {
    start_ip: u32,
    end_ip: u32,
    lsda: u32,

    handler: u32,
    _gp: u32,
    _flags: u32,
    format: u32,
    unwind_info_size: u32,
    unwind_info: u32,
    extra: u32,
}

struct FuncUnwindInfo {
    raw: *mut unw_proc_info,
    name: Vec<u8>,
    previous: Option<Box<FuncUnwindInfo>>,
}

const UNW_CONTEXT_SIZE: i32 = 21;
const CNW_CURSOR_SIZE: i32 = 33;

unsafe fn generate_unwind_info() -> FuncUnwindInfo {
    let context = alloc(Layout::from_size_align(UNW_CONTEXT_SIZE as usize * 8, 8).unwrap());
    dbg!(unw_getcontext(context));

    let cursor = alloc(Layout::from_size_align(CNW_CURSOR_SIZE as usize * 8, 8).unwrap());
    dbg!(unw_init_local(cursor, context));

    unwind_at(cursor)
}

unsafe fn unwind_at(cursor: *mut u8) -> FuncUnwindInfo {
    let raw = alloc(Layout::new::<unw_proc_info>()) as *mut unw_proc_info;
    dbg!(unw_get_proc_info(cursor, raw));

    let mut name: Vec<u8> = vec![0; 50];
    let offp: &mut u32 = &mut 0;
    dbg!(unw_get_proc_name(
        cursor,
        name.as_mut_ptr(),
        name.len(),
        offp as *mut u32,
    ));
    // dbg!(&offp);
    // println!("{name:?}");
    FuncUnwindInfo {
        raw,
        name,
        previous: unwind_next(cursor).map(Box::new),
    }
}

unsafe fn unwind_next(cursor: *mut u8) -> Option<FuncUnwindInfo> {
    if unw_step(cursor) == 0 {
        None
    } else {
        Some(unwind_at(cursor))
    }
}
