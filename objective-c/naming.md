# Haskell Objective-C FFI: Naming Conventions


- `C'NSObject` - static type of objects of that class
- `P'NSObject` - `NSObject` protocol (a type class – but we don't want to explicitly make every object an instance of that type class).
-  `o'NSObject` - class object of type Class(?)
- `s'alloc` - selector (what about the ones with arguments?) - or maybe `m'alloc` (for method)


I am thinking, we want to use some sort of prefix to avoid any name clashes with Haskell keywords or Prelude definitions right from the start.
