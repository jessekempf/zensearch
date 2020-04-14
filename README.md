# zensearch

Zensearch: Zendesk search app homework for Jesse Kempf

# Setup notes

- Install `stack`, Haskell's equivalent to Clojure's `lein` or Rust's `cargo`, from https://docs.haskellstack.org/en/stable/README/
- Clone this repo into some directory on your laptop.
- `cd` into the checkout dir and run `stack build` while plugged in. Go get a coffee or something. This isn't a joke -- initial build times are long and processor-intensive.
- Run `stack exec zensearch` for the usage docs.
- As an example, run `stack exec zensearch match Organization domain_names kage.com` to see real output.

I've included the sample data files for completeness.

# Remarks
## CLI
The CLI output is kind of quick-and-dirty. I used Haskell's pretty-printer for human-readable output. It'll produce adequate output, looking like:
```
[ Organization
    { orgId = OrganizationID 101
    , orgUrl =
        "http://initech.zendesk.com/api/v2/organizations/101.json"
    , orgExternalId = 9270ed79 - 35eb - 4a38 - a46f - 35725197ea8d
    , orgName = "Enthaze"
    , orgDomainNames =
        fromList
          [ "ecratic.com" , "endipin.com" , "kage.com" , "zentix.com" ]
    , orgCreatedAt = (2016 - 05 - 21) (21 : 10 : 28) UTC
    , orgDetails = Just "MegaCorp"
    , orgSharedTickets = False
    , orgTags = fromList [ "Farley" , "Fulton" , "Rodriguez" , "West" ]
    }
]
```

UUID pretty printing isn't the best, and neither is UTC time. Incidentally, all times are internally represented and reported in UTC, while the timestamp parser is ISO8601-compliant.

That `fromList` thingy is how the "pretty" printer renders `Set`s. It does something similar for `Map`s to print them as an association list.

## Haskell
If this is your first time reading Haskell, there are a few things to be aware of.

Haskell "objects" don't quite behave like OO objects -- especially when it comes to field names. Record field names live in the global namespace so it is customary to name things like "orgId" and "userId" when there are possible name collisions.

Like every functional language, Haskell's native `String` type is a linked list. This is obnoxiously inefficient so `Text` is used for a more efficient representation.

Our data types can contain functions and procedures. You'll run into that in `Zensearch.Record`, where a `Field` contains both a parsing procedure as well as a field value extraction function.

We have a zero-runtime-cost way of aliasing datatypes called `newtypes`. An `Int` and a `newtype MyInt = MyInt Int` have the same representation in memory, but the type checker will reject code that tries to use them interchangably. This technique makes it possible to eliminate large classes of errors by construction.

It's possible to define "type families", which are like functions that map from types to types. This shows up in `Zensearch.Record` and `Zensearch.Matchable`. `Comparand` maps a field type to a matching type, to make it possible for us to match sets based on membership, while matching primitive types based on equality.

You'll also notice that there are far fewer tests than one would expect in Ruby or Java. The tests that do exist are ones to cover where the type system can't help us -- when working with a record that has ten `Text` fields, as an example. Very broadly, one writes tests for logic in Haskell, and lets the type system ensure the correctness of plumbing.

Nullability is handled explicitly. `Word8` contains values between `0` and `255` inclusive, but can not be `NULL`. To do that we use `Maybe a`, which can be `Just val` or `Nothing`.

Conventionally, the `Either l r` type is used for operations that can fail. `l` is treated as the type of failure, while `r` the type of success.

C-style `enums`s and `union`s are unified as sum types. I can say `data Bool = False | True`. I can also say `data Contrived = Textual String | Numeric Int`.

Type classes are like Interfaces in Go, but more powerful. They're defined with the `class` keyword, and an implementation given for a type using the `instance` keyword. Haskell often can derive type class instances, and that's what's going on when you see things like `deriving (Eq, Show)` or so on. `Eq` is the class for types that support equality checks, and `Show` is the class for types that can be presented as `Strings`.

If I have two functions, `f :: a -> b` and `g :: b -> c`, I can write `g . f` to get a function `a -> c`. This style is known as "point-free".

## Code layout
The `main` function lives in `app/Main.hs`. In every Haskell project I've worked on, `app/Main.hs` has a way of containing ugly glue code so that the support library in `src` can stay relatively clean and focued on the problem domain. This one's no different.

The support library lives in `src/`.

Tests live in `test/`. The correspondence between the two should be clear enough.

- `Zensearch.Matchable` contains the matching code.
- `Zensearch.Parseable` contains the parsing code for user input. JSON parsing comes out-of-the-box.
- `Zensearch.Record` lets layer some relational-like operations atop data types.
- `Zensearch.Types.Common` contains definitions for `UserID` and `OrganizationID`.
- `Zensearch.Types.{Organization, Ticket, User}` contain the domain models.

I've taken what could be called the "Straightforward Haskell" or "Dumb Haskell" approach. Correct encapsulation of knowledge is maintained and good design principles are upheld. *However*, to take `Zensearch.Types.Organization` as an example, there's no fanciness I've engaged in to avoid repeating knowledge of the names of all of the fields in the record. There are techniques I could use to derive the JSON decoder and `Record` implementations entirely from the data definition and eliminate the risk of transcription errors. The result is very clean consuming code, but mind-bending producing code. I decided not to do that without knowing how the requirements grow.

## Data Assumptions
If there's a normative schema for any of the data files, I must've lost it. Several fields showed up as null when I tried running them through my parser, so I've marked those as nullable. This is easy enough to change on-the-fly.

A number of attributes, like `Organization.detail`, could rigorously be modeled as sum types, as `data OrgType = MegaCorp | NonProfit`. Without a normative schema saying which are user-defined and which are intrinsic to our problem domain, I've erred on the side of over-representing things as strings instead of sum types.

## Extensibility
Haskell has one very big constraint: All types must be known at compile-time. This means that one has to take some care to ensure type information is carried around in the right spots in the code so that, for example, we can choose the correct user input parser.

The constraint is also liberating, because it means that Haskell is very good for refactoring abstractions. This means that, with a little planning, doing more of what we've already done can become very cheap. On the other hand, doing something new is more expensive.

Writing new code in Haskell is tricky until the types line up, and then it's often not particularly difficult to follow the compiler errors until the project compiles and have the program work correctly the first time.

### What is easy
- Adding fields to existing records
- Adding richer types to existing fields
- Adding per-datatype rich matching
- Adding new record types

### What is intermediate
- Adding single-column indexes to speed up exact matches
- Implementing a predicate AST, so we can say `(_id = 101 OR external_id = 7cd6b8d4-2999-4ff2-8cfd-44d05aabb44e)`
- Writing a JSON decoder and `Record` implementation generic over any data type

### What is hard
- Adding multi-column indices
- Adding indexes to speed up range matches
- Implementing a predicate AST with different matching operators, so we could say `(_id >= 99 AND <= 123)`
