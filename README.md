# S-Python
## A statically-typed Python compiler   

S-Python is an educational tool that introduces the concepts of static typing and compiling to young learners within the Python programming language.

## Contents
* [Examples](#ex)
* [Installation](#install)
* [Test and Run](#test)

## <a name="ex"></a> Examples
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
## <a name="install"></a> Installation

Install Docker on whatever operating system, see [Get Docker](https://docs.docker.com/get-docker/) for more information.

Build the docker image may take a minute or two:
```console
docker build -t spython .
```
and then run:
```console
docker run -v $PWD:/plt2024 spython
```

## <a name="test"></a> Test and Run
```console
make test
```
```console
./testall.sh
```
