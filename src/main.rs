

extern crate either;
use either::*;

type Bit = Either<(),()>;
const ZERO: Bit = Left (());
const ONE: Bit = Right (());

type Word2 = (Bit, Bit);
type Word4 = (Word2, Word2);
type Word8 = (Word4, Word4);

// combinators
fn iden<T>(a: T) -> T {
    a
}

fn unit<T>(_a: T) -> () {
}

fn comp<T1: 'static, T2: 'static, T3: 'static>(
    f1: impl Fn(T1) -> T2,
    f2: impl Fn(T2) -> T3
) -> impl Fn(T1) -> T3 {
    // TODO savil. Is this "move" correct?
    move |a: T1| f2(f1(a))
}

fn injl<T1, T2, T3>(f1: impl Fn(T1) -> T2, a: T1) -> Either<T2, T3> {
    Either::Left(f1(a))
}

fn injr<T1, T2, T3>(f1: impl Fn(T1) -> T3, a: T1) -> Either<T2, T3> {
    Either::Right(f1(a))
}

fn case<T1, T2, T3, T4>(
    f1: impl Fn((T1, T3)) -> T4,
    f2: impl Fn((T2, T3)) -> T4,
    (e, val): (Either<T1, T2>, T3)
) -> T4 {
    match e {
        Left(l) => f1((l, val)),
        Right(r) => f2((r, val))
    }
}

fn pair<T1: Copy, T2, T3>(
    f1: impl Fn(T1) -> T2,
    f2: impl Fn(T1) -> T3,
    input: T1
) -> (T2, T3) {
    (f1(input), f2(input))
}

fn take<T1, T2, T3>(func: impl Fn(T1) -> T3, (a, _b): (T1, T2)) -> T3 {
    func(a)
}

fn drop<T1, T2, T3>(func: impl Fn(T2) -> T3, (_a, b): (T1, T2)) -> T3 {
    func(b)
}

///////////// Programs ///////////////////////////////

fn not(input: Bit) -> Bit {
    let case1 = |a| case(|b| injr(unit, b), |b| injl(unit, b), a);
    comp(|a| pair(iden, unit, a), case1)(input)
}

fn half_adder((b1, b2): (Bit, Bit)) -> (Bit, Bit) {
    case(
        |c| drop( |a| pair(|b| injl(unit, b), iden, a), c),
        |c| drop( |a| pair(iden, not, a), c),
        (b1, b2)
    )
}

fn full_adder(((b1, b2), b3): ((Bit, Bit), Bit)) -> (Bit, Bit) {
    comp(
        |a| pair(|b| take(half_adder, b), |b| drop(iden, b), a),
        comp(
            |a| pair(
                |b| take(|c| take(iden, c), b),
                comp(
                    |b| pair(
                        |c| take(|d| drop(iden, d), c),
                        |c| drop(iden, c),
                        b
                    ),
                    half_adder
                ),
                a
            ),
            |a| pair(
                |b| case(
                    |c| drop(|d| take(iden, d), c),
                    |c| injr(unit, c),
                    b
                ),
                |b| drop(|c| drop(iden, c), b),
                a
            )
        )
    )( ((b1, b2), b3) )
}

fn main() {
    println!("Hello, world!");
    println!("not of 1 is: {:#?}", not(ONE));
    println!("not of 0 is: {:#?}", not(ZERO));

    println!("full adder ((one, one), zero): {:#?}", full_adder(((ONE, ONE), ZERO)));
    println!("full adder ((zero, zero), one): {:#?}", full_adder(((ZERO, ZERO), ONE)));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn not_one() {
        assert_eq!(not(ONE), ZERO);
    }

    #[test]
    fn not_zero() {
        assert_eq!(not(ZERO), ONE);
    }

    #[test]
    fn full_adder1() {
        assert_eq!(full_adder(((ONE, ONE), ZERO)), (ONE, ZERO));
    }

    #[test]
    fn full_adder2() {
        assert_eq!(full_adder(((ZERO, ZERO), ONE)), (ZERO, ONE));
    }
}
