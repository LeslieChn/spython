# S-Python
## A statically-typed Python compiler   

S-Python is an educational tool that introduces the concepts of static typing and compiling to young learners within the Python programming language.

# Contents
* [Examples](#Examples)
* [Installation](#Installation)
* [Test and Run](#Test)

# Examples
```py
def gcd(int a, int b)->int:
    while (a != b):
        if (a > b):    
            a = a - b
        else:
            b = b - a
    return a
print(gcd (1234342213, 334232))
```
# Installation

Install Docker on whatever operating system 

# Test and run
```console
make test
```
```console
./testall.sh
```
