extern crate clap;
extern crate regex;

include!("../clap-test.rs");

use clap::{App, Arg};

macro_rules! assert_index_same_as_source {
    ($m:expr, $arg:expr, $expected:expr) => {
        let m = &$m;
        let arg = $arg;
        let expected = $expected;

        assert_eq!(m.index_of(arg), expected, "incorrect index for {:?}", arg);
        assert_eq!(
            m.source_index_of(arg),
            expected,
            "incorrect source index for {:?}",
            arg
        );
    };
}

macro_rules! assert_indices {
    ($e:expr, $expected:expr) => {
        assert_eq!(
            $e.unwrap().collect::<Vec<_>>(),
            $expected,
            "incorrect values for {:?}",
            stringify!($e)
        );
    };
}

macro_rules! assert_indices_same_as_source {
    ($m:expr, $arg:expr, $expected:expr) => {
        let m = &$m;
        let arg = $arg;
        let expected = $expected;

        assert_eq!(
            m.indices_of(arg).unwrap().collect::<Vec<_>>(),
            expected,
            "incorrect indices for {:?}",
            arg
        );
        assert_eq!(
            m.source_indices_of(arg).unwrap().collect::<Vec<_>>(),
            expected,
            "incorrect source indices for {:?}",
            arg
        );
    };
}

#[test]
fn indices_mult_opts() {
    let m = App::new("ind")
        .arg(
            Arg::with_name("exclude")
                .short("e")
                .takes_value(true)
                .multiple(true),
        )
        .arg(
            Arg::with_name("include")
                .short("i")
                .takes_value(true)
                .multiple(true),
        )
        .get_matches_from(vec!["ind", "-e", "A", "B", "-i", "B", "C", "-e", "C"]);

    assert_indices_same_as_source!(m, "exclude", &[2, 3, 8]);
    assert_indices_same_as_source!(m, "include", &[5, 6]);
}

#[test]
fn index_mult_opts() {
    let m = App::new("ind")
        .arg(
            Arg::with_name("exclude")
                .short("e")
                .takes_value(true)
                .multiple(true),
        )
        .arg(
            Arg::with_name("include")
                .short("i")
                .takes_value(true)
                .multiple(true),
        )
        .get_matches_from(vec!["ind", "-e", "A", "B", "-i", "B", "C", "-e", "C"]);

    assert_index_same_as_source!(m, "exclude", Some(2));
    assert_index_same_as_source!(m, "include", Some(5));
}

#[test]
fn index_flag() {
    let m = App::new("ind")
        .arg(Arg::with_name("exclude").short("e"))
        .arg(Arg::with_name("include").short("i"))
        .get_matches_from(vec!["ind", "-e", "-i"]);

    assert_index_same_as_source!(m, "exclude", Some(1));
    assert_index_same_as_source!(m, "include", Some(2));
}

#[test]
fn index_flags() {
    let m = App::new("ind")
        .arg(Arg::with_name("exclude").short("e").multiple(true))
        .arg(Arg::with_name("include").short("i").multiple(true))
        .get_matches_from(vec!["ind", "-e", "-i", "-e", "-e", "-i"]);

    assert_index_same_as_source!(m, "exclude", Some(1));
    assert_index_same_as_source!(m, "include", Some(2));
}

#[test]
fn index_repeated_parses() {
    let mut app = App::new("ind")
        .arg(Arg::with_name("exclude").short("e").multiple(true))
        .arg(Arg::with_name("include").short("i").multiple(true));

    let m = app
        .get_matches_from_safe_borrow(vec!["ind", "-e", "-i", "-e", "-e", "-i"])
        .unwrap();

    assert_index_same_as_source!(m, "exclude", Some(1));
    assert_index_same_as_source!(m, "include", Some(2));

    let m = app
        .get_matches_from_safe_borrow(vec!["ind", "-e", "-i", "-e", "-e", "-i"])
        .unwrap();

    assert_index_same_as_source!(m, "exclude", Some(1));
    assert_index_same_as_source!(m, "include", Some(2));
}

#[test]
fn indices_mult_flags() {
    let m = App::new("ind")
        .arg(Arg::with_name("exclude").short("e").multiple(true))
        .arg(Arg::with_name("include").short("i").multiple(true))
        .get_matches_from(vec!["ind", "-e", "-i", "-e", "-e", "-i"]);

    assert_indices_same_as_source!(m, "exclude", &[1, 3, 4]);
    assert_indices_same_as_source!(m, "include", &[2, 5]);
}

#[test]
fn indices_mult_flags_combined() {
    let m = App::new("ind")
        .arg(Arg::with_name("exclude").short("e").multiple(true))
        .arg(Arg::with_name("include").short("i").multiple(true))
        .get_matches_from(vec!["ind", "-eieei"]);

    assert_indices!(m.indices_of("exclude"), &[1, 3, 4]);
    assert_indices!(m.source_indices_of("exclude"), &[1, 1, 1]);

    assert_indices!(m.indices_of("include"), &[2, 5]);
    assert_indices!(m.source_indices_of("include"), &[1, 1]);
}

#[test]
fn indices_mult_flags_opt_combined() {
    let m = App::new("ind")
        .arg(Arg::with_name("exclude").short("e").multiple(true))
        .arg(Arg::with_name("include").short("i").multiple(true))
        .arg(Arg::with_name("option").short("o").takes_value(true))
        .get_matches_from(vec!["ind", "-eieeio", "val"]);

    assert_indices!(m.indices_of("exclude"), &[1, 3, 4]);
    assert_indices!(m.source_indices_of("exclude"), &[1, 1, 1]);

    assert_indices!(m.indices_of("include"), &[2, 5]);
    assert_indices!(m.source_indices_of("include"), &[1, 1]);

    assert_indices!(m.indices_of("option"), &[7]);
    assert_indices!(m.source_indices_of("option"), &[2]);
}

#[test]
fn indices_mult_flags_opt_combined_eq() {
    let m = App::new("ind")
        .arg(Arg::with_name("exclude").short("e").multiple(true))
        .arg(Arg::with_name("include").short("i").multiple(true))
        .arg(Arg::with_name("option").short("o").takes_value(true))
        .get_matches_from(vec!["ind", "-eieeio=val"]);

    assert_indices!(m.indices_of("exclude"), &[1, 3, 4]);
    assert_indices!(m.source_indices_of("exclude"), &[1, 1, 1]);

    assert_indices!(m.indices_of("include"), &[2, 5]);
    assert_indices!(m.source_indices_of("include"), &[1, 1]);

    assert_indices!(m.indices_of("option"), &[7]);
    assert_indices!(m.source_indices_of("option"), &[1]);
}

#[test]
fn indices_mult_opt_value_delim_eq() {
    let m = App::new("myapp")
        .arg(
            Arg::with_name("option")
                .short("o")
                .takes_value(true)
                .use_delimiter(true)
                .multiple(true),
        )
        .get_matches_from(vec!["myapp", "-o=val1,val2,val3"]);

    assert_indices!(m.indices_of("option"), &[2, 3, 4]);
    assert_indices!(m.source_indices_of("option"), &[1, 1, 1]);
}

#[test]
fn indices_mult_opt_value_no_delim_eq() {
    let m = App::new("myapp")
        .arg(
            Arg::with_name("option")
                .short("o")
                .takes_value(true)
                .multiple(true),
        )
        .get_matches_from(vec!["myapp", "-o=val1,val2,val3"]);

    assert_indices!(m.indices_of("option"), &[2]);
    assert_indices!(m.source_indices_of("option"), &[1]);
}

#[test]
fn indices_mult_opt_mult_flag() {
    let m = App::new("myapp")
        .arg(
            Arg::with_name("option")
                .short("o")
                .takes_value(true)
                .multiple(true),
        )
        .arg(Arg::with_name("flag").short("f").multiple(true))
        .get_matches_from(vec!["myapp", "-o", "val1", "-f", "-o", "val2", "-f"]);

    assert_indices_same_as_source!(m, "option", &[2, 5]);
    assert_indices_same_as_source!(m, "flag", &[3, 6]);
}
