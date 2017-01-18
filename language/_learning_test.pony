use "ponytest"

actor _LearningTest is TestList
  new make() =>
    None

  fun tag tests(test: PonyTest) =>
    learning_tests(test)

  fun tag learning_tests(test: PonyTest) =>
    test(object iso is UnitTest
      fun name(): String => "---- Describe: Learning ----"
      fun apply(h: TestHelper) => None
    end)
    test(_TestConsume)
    test(_TestEither)

class _TestConsume is UnitTest
  fun name(): String => "consume"
  fun apply(h: TestHelper) =>
    let s1: String iso = recover "hi".clone() end
    let string_iso: String iso = consume s1
    // h.env.out(s1) - gives error
    let s1_ref: String ref = consume string_iso
    let s1_ref': String ref = s1_ref

    let s2: String ref = String()
    let string_ref: String ref = consume s2

    let s3: String trn = recover String() end
    let string_trn: String trn = consume s3

class val Left
  let error_message: String
  new val create(err: String) =>
    error_message = err
  fun apply(): String =>
    error_message
  fun val map(fn: {(F64): F64} box): Either val =>
    this

class val Right
  let _value: F64
  new val create(value: F64) =>
    _value = value
  fun apply(): F64 =>
    _value
  fun map(fn: {(F64): F64} box): Either val =>
    Right(fn(_value))

type Either is (Left | Right)

class _TestEither is UnitTest
  fun name(): String => "either"
  fun apply(h: TestHelper) ? =>
    let r1 = success()
    h.assert_eq[F64](24, (r1 as Right)())
    let r2 = failure()
    h.assert_eq[String]("some error message", (r2 as Left)())

    // compose an operation
    h.assert_eq[F64](48,
      (r1.map({(v: F64): F64 => v + 24}) as Right)())
    h.assert_eq[String]("some error message",
      (r2.map({(v: F64): F64 => v + 24}) as Left)())

    // with match
    match r1
    | let r: Right => h.assert_eq[F64](24, r())
    | let l: Left => h.fail("oops")
    end
    match r2
    | let r: Right => h.fail("oops")
    | let l: Left => h.assert_eq[String]("some error message", l())
    end

    // with try
    try
      h.assert_eq[F64](24, (r1 as Right)())
    else
      h.fail("should not have matched")
    end

    try
      h.assert_eq[F64](24, (r2 as Right)())
      h.fail("should not have matched")
    else
      h.assert_eq[String]("some error message", (r2 as Left)())
    end

  fun success(): Either =>
    Right(24)

  fun failure(): Either =>
    Left("some error message")
