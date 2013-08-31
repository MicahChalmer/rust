fn xyzzy(
    a:int,
    b:char) { }

fn foo(a:int,
       b:char) { }

fn bar(a:int, b:char)
       -> int {
    3
}

fn this_is_a_really_long_name_so_we_want_the_args_on_the_next_line(
    a:int,
    b:~str) {
    let aaaaaa = [
        1,
        2,
        3];
    let bbbbbbb = [1, 2, 3,
                   4, 5, 6];
    let ccc = [   10, 9, 8,
                  7, 6, 5];
}

fn nexted_fns(a: fn(b:int,
                    c:char)
                    -> int,
              d: int)
              -> ~str
{
    ~"hi";
}

fn with_a_comment_after_arg_list(a:int, // and ending with a comment
                                 b:int) { }

fn with_a_comment_after_paren( // hello
    a:int,
    b:int) { }
