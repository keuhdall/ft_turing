# ft_turing - a [Turing Machine](https://en.wikipedia.org/wiki/Turing_machine) Simulator made in Haskell

###### Disclamer : This project has been made as our first Haskell project, with no theorical nor practical knowledge of the language, we weren't aware of the best practices and the implementation has been made in a really 'naive' way. I might redo this project completely some day - if I'm not too lazy 💤

## Installing
#### Needed tools :
- ghc
- cabal

#### Needed dependencies :
- aeson
> To install aeson, simply run `cabal install aeson`

## Usage
Compile the program using `make`
Run the program using one of the Turing Machines plus an input.
> Example : `./ft_turing 02-palindrome.json 11100111`

## Understanding a simple Turing Machine : the unary addition
An unary addition is basically making sum of characters.
> Example : `111 + 11` would give `11111`

#### Setting up the rules of the machine
Im this example, the alphabet of the input will be composed of 3 characters :
- `1` as character we want to add
- `+` as sign for the addition
- `=` as sign to give us the value of the operation
- `.` as a blank character (that can not be given in the input of the machine)

We now need to define behavior of the machine for each character read, for this, we will define 2 states :
- We will call these states *Scan right* and *Add one*.
- The initial state of the machine will be *Scan right*.
- The machine will stop on the state **HALT**.

The rule of these states can be described as below :


##### Scan right :
| Read | Write | To state | Action |
| ---- | ----- | -------- | ------ |
| `.`  |  `.`  |Scan right| RIGHT  |
| `1`  |  `1`  |Scan right| RIGHT  |
| `+`  |  `.`  |Add one   | RIGHT  |
| `=`  |  `.`  |**HALT**  | LEFT   |

##### Add one :
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
The machine reads the character `=` in state *Scan right*, it is replaced by a '.', the head moves to LEFT, the machines switchs to the state **HALT** and stops.

###### Final state :
```
STATE : HALT
| 1 | 1 | . | . |
          ^
```
