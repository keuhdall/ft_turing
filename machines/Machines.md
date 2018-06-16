# The machines
### In this Markdown, we will describe the 5 Turing Machines we did and how they work.

## Machine 00 : Unary substraction
#### Description :
This machine does unary substractions (111 - 11 = 1).
#### How to use it ?
Run `./ft_turing machines/00-unary_sub.json` with an input of type : *initial number one 1's* **-** *number of 1's to substract* **=**
#### Example
`./ft_turing machines/00-unary_sub.json 11111-11=`  will output `111`.

---

## Machine 01 : Unary addition
#### Description :
This machine does unary additions (111 + 11 = 11111).
#### How to use it ?
Run `./ft_turing machines/01-unary_add.json` with an input of type : *initial number one 1's* **+** *number of 1's to add*
> No `=` sign is needed at the end of the input for this machine.
#### Example
`./ft_turing machines/01-unary_add.json 1+111`  will output `1111`.

---

## Machine 02 : Palindrome
#### Description :
This machine checks if the given input is a [palindrome](https://en.wikipedia.org/wiki/Palindrome) and appends `y` if the input is a palindrome, otherwise `n`.
#### How to use it ?
Run `./ft_turing machines/02-palindrome.json` with an input of type : *0's and 1's*
#### Examples
- `./ft_turing machines/02-palindrome.json 00100` will output `00100y`.
- `./ft_turing machines/02-palindrome.json 0111` will output `0111n`.

---

## Machine 03 : 0n1n
#### Description :
This machine checks if the given input has the same number of 0's and 1's.
> The input must always be of type `[0's][1's]`. Which means that 00011101 would be invalid.
#### How to use it ?
Run `./ft_turing machines/03-0n1n.json` with an input of type : *0's then 1's*
#### Examples
- `./ft_turing machines/03-0n1n.json 000111` will output `000111y`.
- `./ft_turing machines/03-0n1n.json 0001` will output `0001n`.
- `./ft_turing machines/03-0n1n.json 00011101` will output `00011101n`.

---

## Machine 04 : Even
#### Description :
This machine checks if the given has an even number of 0's
#### How to use it ?
Run `./ft_turing machines/04-even.json` with an input of type : *0's*
#### Examples
- `./ft_turing machines/04-even.json 0000` will output `0000y`.
- `./ft_turing machines/04-even.json 00000` will output `00000n`.

---

## Machine 05 : Turingception
#### Description :
This machines executes another Turing Machine as input
#### How to use it ?
Run `./ft_turing machines/05-turingception.json` with an input of type :
#### Example
- `./ft_turing machines/05-turingception.json (?_>) A:(1,+,.), S: {(.=.,x,>), (1=1,x,>), (+=.,xx,>), (V=.,H,<)] [ (1=+,xx,<), (.=1,x,>),(V=.,H,<) ] #1+1V..` will output `11`.