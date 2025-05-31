# FusionLang

FusionLang is a modern, expressive programming language designed for simplicity and flexibility. It combines familiar syntax with powerful features like type inference, traits, and compile-time evaluation.

## Features

### Semicolons and Commas

Semicolons are optional unless multiple expressions appear on the same line, where they separate statements. Commas are similarly optional for separating items in lists or function arguments unless written inline.

```fusion
a := 5; b := 6; println("Hello World")
c := 1
d := 2
fn add(x i32, y i32) i32 { return x + y }
fn sub(x i32 y: i32) i32 { return x - y } // Inline args without comma
```

### Variables

Variables are declared using `:=`. Names follow standard conventions (alphanumeric, underscores, no leading digits). Type inference is automatic, but types can be explicitly specified with `as`. Variables are immutable by default; prefix with `mut` for mutability. Assignment uses `=`. Global constants are declared with `const` and evaluated at compile time.

```fusion
a := 5              // Inferred as i32
const GLOBAL_VAR := "Hello World" // Compile-time constant
mut b := 78 as i64  // Explicit type
a = 6               // Error: a is immutable
b = 999             // OK: b is mutable
```

### Functions

Functions are declared with `fn`, followed by the name, parameters, and an optional return type. Return types are inferred if omitted. Use `return` to return a value. Functions marked `const` are evaluated at compile time and can only use compile-time-evaluable parameters.

```fusion
fn hello_world() {
    println("Hello World")
}

fn add(x: i32, y: i32) i32 {
    return x + y
}

const fn fib(n: i32) i32 {
    if n <= 1 { return n }
    return fib(n - 1) + fib(n - 2)
}
```

### Math Operations

FusionLang supports standard arithmetic operations: `+` (addition), `-` (subtraction), `*` (multiplication), `/` (division), `%` (modulus), and `**` (exponentiation). Comparison operators include `==`, `!=`, `<`, `>`, `<=`, `>=`. Logical operators are `&&`, `||`, and `!`. Operator precedence follows standard conventions (e.g., `*` before `+`).

```fusion
x := 10
y := 3
z := x + y * 2      // 16 (multiplication first)
w := x ** 2         // 100
is_equal := x == y  // false
is_valid := x > 0 && y < 5  // true
```

### Modules

Modules organize code into reusable units. Declare a module with `mod`, and import it with `use`. Modules can be nested, and items can be accessed with the `::` operator. Public items are marked with `pub`.

```fusion
mod math {
    pub fn square(x: i32) i32 {
        return x * x
    }
}

use math::square
result := square(4)  // 16
```

### Traits

Traits define interfaces that types can implement. Each trait function can have a default implementation, used if not overridden. Use `impl` to implement a trait for a type.

```fusion
trait ToString {
    fn toString(&self) string
    fn debug(&self) string {
        return "debug"
    }
}

struct Test = {
    value: string
}

impl ToString for Test {
    fn toString(&self) string {
        return self.value
    }
}

t := Test{ value: "example" }
println(t.toString())  // "example"
println(t.debug())     // "debug"
```

### Structs

Structs define custom data types without inheritance, using traits for composition. Fields can have default values. Instantiate structs with field initialization, omitting fields with defaults if desired.

```fusion
struct Test = {
    value: string,
    x: i32 = 5  // Default value
}

x := Test{
    value: "hello",
    x: 10
}

y := Test{
    value: "world"  // x defaults to 5
}
```

### Enums

Enums define types with a fixed set of variants. Variants can hold data. Pattern matching with `match` handles enum values.

```fusion
enum Option = {
    Some(i32),
    None
}

val := Option::Some(42)
result := match val {
    Some(x) => x * 2,
    None => 0
}  // result = 84
```

### Generics

Generics enable reusable code for functions, structs, enums, and traits. Use angle brackets `<T>` to define generic parameters, optionally constrained by traits.

#### Generic Functions

```fusion
fn swap<T>(x: T, y: T) (T, T) {
    return (y, x)
}

a := 5
b := 10
(a, b) = swap(a, b)  // a = 10, b = 5
```

#### Generic Structs

```fusion
struct Pair<T> = {
    first: T,
    second: T
}

p := Pair<i32>{ first: 1, second: 2 }
```

#### Generic Enums

```fusion
enum Result<T, E> = {
    Ok(T),
    Err(E)
}

r := Result<i32, string>::Ok(42)
```

#### Generic Traits

Traits can be generic or constrain generic types to implement specific traits.

```fusion
trait Container<T> {
    fn get(&self) T
}

struct Box<T> = {
    value: T
}

impl<T> Container<T> for Box<T> {
    fn get(&self) T {
        return self.value
    }
}

b := Box<i32>{ value: 100 }
println(b.get())  // 100
```

### Control Flow

FusionLang supports `if`, `else`, `while`, and `for` loops. `match` is used for pattern matching.

```fusion
x := 5
if x > 0 {
    println("Positive")
} else {
    println("Non-positive")
}

for i in 0..5 {
    println(i)
}

while x > 0 {
    x = x - 1
}
```

### Error Handling

Use the `Result` or `Option` enums for error handling, combined with `match` or the `?` operator to propagate errors.

```fusion
fn divide(x: i32, y: i32) Result<i32, string> {
    if y == 0 {
        return Result::Err("Division by zero")
    }
    return Result::Ok(x / y)
}

result := divide(10, 2)?  // Propagates error if Err
```

### C Bindings

To provide c compatability there are serveral compiler macros.

```fusion

_c_import "<stdio.h>" // Equates to "#include <stdio.h>"

```
