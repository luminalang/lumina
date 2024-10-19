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
        println!("{:?}", &info.name);
    }
}

struct unw_cursor {
    opaque: [u64; 127],
}

struct ucontext_t {
    uc_flags: u64,
    uc_link: *mut ucontext_t,
    uc_stack: stack_t,
    uc_mcontext: mcontext_t,
    uc_sigmask: sigset_t,
    __fpregs_mem: _libc_fpstate,
    __ssp: [u64; 4],
}

struct mcontext_t {
    gregs: gregset_t,
    fpregs: fpregset_t,
    __reserved1: [u64; 8],
}

type gregset_t = [greg_t; 23];
type fpregset_t = *mut _libc_fpstate;

struct _libc_fpstate {
    cwd: u16,
    swd: u16,
    ftw: u16,
    top: u16,
    rip: u16,
    rdp: u16,
    mxcsr: u32,
    mxcr_mask: u32,
    _st: [_libc_fpxreg; 8],
    _xmm: [_libc_xmmreg; 16],
    __glibc_reserved1: [u32; 24],
}

struct _libc_fpxreg {
    significand: [u16; 4],
    exponent: u16,
    __glibc_reserved1: [u16; 3],
}

struct _libc_xmmreg {
    element: [u32; 4],
}

type greg_t = i64;

struct stack_t {
    ss_sp: *mut (),
    ss_flags: i32,
    ss_size: usize,
}

struct sigset_t {
    __val: [u64; 16],
}

extern "C" {
    fn unw_getcontext(contexrt: *mut ucontext_t) -> i32;
    fn unw_init_local(cursor: *mut unw_cursor, context: *mut ucontext_t) -> i32;
    fn unw_init_remote(cursor: *mut unw_cursor, addr_space: *mut u8, void: *mut u8) -> i32;
    fn unw_step(cursor: *mut unw_cursor) -> i32;
    // fn unw_get_proc_info(cursor: *mut unw_cursor, outinfo: *mut unw_proc_info) -> i32;
    fn unw_get_proc_name(
        cursor: *mut unw_cursor,
        charptr: *mut i8,
        size: usize,
        offp: *mut u64,
    ) -> i32;
}

// #[repr(C)]
// struct unw_proc_info {
//     start_ip: u64,
//     end_ip: u64,
//     lsda: u64,
//
//     handler: u64,
//     _gp: u64,
//     _flags: u64,
//     format: i32,
//     unwind_info_size: i32,
//     unwind_info: *mut u8,
//     extra: unw_tdep_proc_info_t,
// }

// #[repr(C)]
// struct unw_tdep_proc_info_t {
//     unused: i8,
// }

struct FuncUnwindInfo {
    // raw: *mut unw_proc_info,
    name: Vec<u8>,
    previous: Option<Box<FuncUnwindInfo>>,
}

const UNW_CONTEXT_SIZE: i32 = 21;
const CNW_CURSOR_SIZE: i32 = 33;

unsafe fn generate_unwind_info() -> FuncUnwindInfo {
    let context = alloc(Layout::new::<ucontext_t>()) as *mut ucontext_t;
    // let context: ucontext_t = std::mem::MaybeUninit::uninit();
    dbg!(unw_getcontext(context));

    // let cursor = alloc(Layout::from_size_align(CNW_CURSOR_SIZE as usize * 8, 8).unwrap());
    let cursor = alloc(Layout::new::<unw_cursor>()) as *mut unw_cursor;
    dbg!(unw_init_local(cursor, context));

    unwind_at(cursor)
}

unsafe fn unwind_at(cursor: *mut unw_cursor) -> FuncUnwindInfo {
    // let raw = alloc(Layout::new::<unw_proc_info>()) as *mut unw_proc_info;
    // dbg!(unw_get_proc_info(cursor, raw));

    let mut name: Vec<u8> = vec![0; 4096];
    let mut offset: u64 = 0;
    dbg!(unw_get_proc_name(
        cursor,
        name.as_mut_ptr() as *mut i8,
        name.len(),
        (&mut offset) as *mut u64,
    ));
    // dbg!(&offp);
    // println!("{name:?}");
    FuncUnwindInfo { name, previous: unwind_next(cursor).map(Box::new) }
}

unsafe fn unwind_next(cursor: *mut unw_cursor) -> Option<FuncUnwindInfo> {
    if unw_step(cursor) == 0 {
        None
    } else {
        Some(unwind_at(cursor))
    }
}
