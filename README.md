# S-Python
## A statically-typed Python compiler   

S-Python is an educational tool that introduces the concepts of static typing and compiling to young learners within the Python programming language.

# Contents
* [Examples](#Examples)
* [Installation](#Installation)
* [Test and Run](#Test and run)

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

Install Docker on whatever operating system you're on
Under Ubuntu,
   apt install docker.io

   You'll likely need to add yourself to the "docker" group:
  
   sudo usermod -aG docker <username>

   Log out and back in for this to take effect.  Run "groups" to
   make sure you're part of the "docker" group.

* Test your installation

   docker run hello-world

  If this fails, you will need to correct your installation.

# Test and run
```console
make test
```
```console
./testall.sh
```
