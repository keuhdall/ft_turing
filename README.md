# ft_turing - a [Turing Machine](https://en.wikipedia.org/wiki/Turing_machine) Simulator made in Haskell

###### Disclaimer : This is the 'v2' of my first Haskell project, but I kept the same foundations, the architecture (even the code itself actually) aren't that great. But I guess this is still a pretty good improvement, knowing that the finished code initially looked like [THIS](https://github.com/keuhdall/ft_turing/tree/d9ee22f9dbbcdbadc5db29726f2756fdde7500c6) (I'm so sorry for you if you actually clicked and browsed the files).

## Installing
#### Needed tools :
- [Haskell](https://www.haskell.org/platform/)
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Usage
Compile the program using `stack build`
Run the program using one of the Turing Machines plus an input. You can either use stack like so :
> Example : `stack run machines/02-palindrome.json 11100111`

Or directly execute the binary like so :
> Example : `./.stack_work/dist/(...)/build/ft-turing-exe/ft-turing-exe machines/02-palindrome.json 11100111`

## Understanding a simple Turing Machine : the unary addition
An unary addition is basically making sum of characters.
> Example : `111 + 11` would give `11111`

#### Setting up the rules of the machine
In this example, the alphabet of the input will be composed of 3 characters :
- `1` as character we want to add
- `+` as sign for the addition
- `=` as sign to give us the value of the operation
- `.` as a blank character (that can not be given in the input of the machine)

We now need to define the behavior of the machine for each character read, for this, we will define 2 states :
- We will call these states **Scan right** and **Add one**.
- The initial state of the machine will be *Scan right*.
- The machine will stop on the state **HALT**.

The rule of these states can be described as below :


#### Scan right :
| Read | Write | To state | Action |
| ---- | ----- | -------- | ------ |
| `.`  |  `.`  |Scan right| RIGHT  |
| `1`  |  `1`  |Scan right| RIGHT  |
| `+`  |  `.`  |Add one   | RIGHT  |
| `=`  |  `.`  |**HALT**  | LEFT   |

#### Add one :
| Read | Write | To state | Action |
| ---- | ----- | -------- | ------ |
| `1`  | `+`   |Add one   |  LEFT  |
| `.`  | `1`   |Scan right|  RIGHT |
| `=`  | `.`   |**HALT**  |  LEFT  |

#### Example with a simple input
Let's take the simplest input possible : `1+1=`
Which gives us a tape like this :
```
STATE : SCAN RIGHT
| 1 | + | 1 | = |
  ^
```
As the initial state is *Scan right*, the machine will read the `1`, replace it by `1` (doing nothing) and move the head to RIGHT, staying in state *Scan right*.
```
STATE : SCAN RIGHT
| 1 | + | 1 | = |
      ^
```
The machine now reads a `+`, in state *Scan right*, so it will replace it by a `.` and move the head to RIGHT, switch to state *Add one*
```
STATE : ADD ONE
| 1 | . | 1 | = |
          ^
```
The machine reads a `1` in state *Add one*, it is replaced by a `+` and the head of the machine moves to LEFT, staying in the same state.

```
STATE : ADD ONE
| 1 | . | + | = |
      ^
```
The machine reads a `.` in state *Add one*, it is replaced by a `1`, the head of the machine moves to RIGHT, switching back to state *Scan right*
```
STATE : SCAN RIGHT
| 1 | 1 | + | = |
          ^
```
The machine reads a `+` in state *Scan right*, it is replaced by a `.`, the head of the machine moves to RIGHT, still in state *Scan right*
```
STATE : SCAN RIGHT
| 1 | 1 | . | = |
              ^
```
The machine reads the character `=` in state *Scan right*, it is replaced by a '.', the head moves to LEFT, the machines switches to the state **HALT** and stops.

##### Final state :
```
STATE : HALT
| 1 | 1 | . | . |
          ^
```

## Stuff to improve

Well, basically everything. This was my first Haskell project ever and it kinda sucks. I tried to rework everything, but this project was made 2 years ago and it's quite hard to wrap my head around some parts. Also, I wasn't really paying attention to stuff like Monads, so the project architecture isn't great either. Nevertheless, I still can see some short-term improvements, like :

- Using the [Lens](https://github.com/ekmett/lens/) library to manipulate records and get rid of `NamedFieldPuns` and `RecordWildCards`
- Replace `Maybe` Monads by `Either` in Engine module, in order to get better error handling
- Probably rethink data structures
- `apply` function in the `Engine` module should have a different signature as `Maybe (Engine,Transition)` does not allow to recover and print the final state of the machine when doing the pattern matching line 35 and 36 (bad design here).
