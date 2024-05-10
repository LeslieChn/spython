# S-Python Final Project
## Introduction
S-Python, a project aimed at introducing the concept of static typing and compilation to coding beginners, leverages Python as the first programming language for kids today, akin to how Logo and Scratch once served as entry points. This initiative brings forth several notable benefits:

Firstly, by embracing static typing and compilation, S-Python enables compilers to identify potential issues arising from type mismatches early in the development cycle, thereby mitigating the risk of runtime errors. This preemptive approach enhances the reliability and robustness of codebases.

Moreover, the explicit type declarations and strict type rules inherent in S-Python enhance code readability. Developers can easily discern the expected types of variables and function parameters, fostering clarity and comprehension within the codebase.

Static typing also contributes to long-term code maintainability by reducing the likelihood of subtle bugs associated with type mismatches. This aspect simplifies code comprehension and modification, facilitating smoother software evolution over time.

Additionally, S-Python's adoption of a compiled language offers performance advantages compared to interpreted languages. Compilation optimizes code execution, leading to enhanced program efficiency and responsiveness.

In essence, S-Python's integration of static typing and compilation not only equips beginners with foundational programming concepts but also instills best practices for code reliability, readability, and performance optimization.

## Language Tutorial
1. Installation: 
* Install Docker on your operating system, see Get Docker for more information. 

* Obtain the latest version of S-  from Github:
    ```console
    git clone https://github.com/LeslieChn/spython.git
    ```
* Build the Docker image: 
    ```console
    docker build -t spython .
    ```
* Launch the Container:
    * for **Mac/Linux** users:
    ```console
    docker run -v $PWD:/plt2024 -it spython
    ```
    * for **Windows** users:
    ```console
    docker run -v %cd%:/plt2024 -it spython
    ```
* Run Makefile to install: 
    ```console
    make
    ```
* Compile the source file: 
    ```console
    ./spython source.spy [-o output]
    ```
* Run the executable: 
    ```console
    ./output
    ```
2. Basics:
S-Python uses indentation to define code blocks. Here's a simple "Hello, World!" program:
    ```py
    print("Hello, World!")
    ```
3. Variables and Data Types:
S-Python is static typed, meaning you must declare variable types explicitly. Common data types include boolean, characters, integers, floats, strings, and lists.
    ```py
    bool t = true
    char c = "b"
    int x = 10
    float y = 3.14
    str name = "Alice"
    int my_list = [1, 2, 3]
    ```
4. Control Structures:
S-Python supports common control structures like if statements, for loops and while loops.
    ```py
    int x = 10
    if x > 5:
        print("x is greater than 5")
    else:
        print("x is less than or equal to 5")
    int my_list[] = [1, 2, 3]
    for int num in my_list:
        print(num)
    while x > 0:
        print(x)
        x -= 1
    ```
5. Strings:
Strings are sequences of characters and support concatenation operation.
    ```py
    str message = "Hello, " + "Chris"
    ```
6. Lists:
Lists is used to store collections of items of the same type, and it's mutable.
    ```py
    int my_list[] = [1, 2, 3]
    my_list[1] = 4
    ```
7. Functions:
    ```py
    def add(int a, int b)->int:
        return a + b
    int c = add(1, 2)
    ```
## Architectural Design

![ad](img/ad.png)

1. Proprocessor:
The indentation of each line defines the scope of statement blocks. This unique feature of S-Python's syntax is known as the "off-side rule" or "indentation-based scoping." Code blocks are defined by their indentation level relative to the surrounding code. Each increase in indentation level signifies the beginning of a new block, and each decrease in indentation level signifies the end of a block. If the end of a block's indentation level can't find a previous beginning indentation level, an indentation error occurs. 

2. Lexical Analysis:
The process begins with lexical analysis, also known as scanning. This stage involves breaking down the source code into a stream of tokens. These tokens are the smallest meaningful units in the programming language, such as keywords, identifiers, constants, and operators.

3. Syntax Analysis:
After lexical analysis, the compiler proceeds to syntax analysis, also referred to as parsing. Here, the compiler verifies whether the sequence of tokens conforms to the rules defined by the language's grammar. This stage typically employs techniques like recursive descent parsing or LALR parsing to build a parse tree representing the syntactic structure of the program.

4. Semantic Analysis:
Semantic analysis follows syntax analysis and focuses on the meaning of the code. This stage checks for semantic correctness, such as type compatibility, undeclared variables, and adherence to language-specific rules. Semantic analysis often involves building and traversing abstract syntax trees (ASTs) to perform type checking and symbol resolution.

5. Intermediate Code Generation:
Once the source code has been validated syntactically and semantically, the compiler generates an LLVM intermediate representation (IR) of the program. This IR serves as an intermediate step between the high-level source code and the target machine code. Common intermediate representations include three-address code, abstract syntax trees, and control flow graphs.

7. Use Clang to assemble LLVM IR into into machine-readable code - executable file.


## Test Plan
* Positive test

    A positive test is designed to validate that a system behaves as expected when provided with valid inputs. It ensures that the software meets its functional requirements under normal operating conditions.

* Negative test

    Negative testing involves providing invalid inputs or unexpected conditions to the software to verify that it handles errors gracefully. It helps identify vulnerabilities, boundary conditions, and error handling capabilities of the software.

* Stress test

    Stress testing assesses the system's behavior under extreme conditions, such as high traffic, heavy loads, or resource exhaustion. It identifies performance bottlenecks, scalability issues, and the system's breaking point.

* Automation

    Bash script named testall.sh automates the execution of all tests and automatically reports any errors encountered during the process

## Summary
* State who did what
* Individual Team Member Contribution

    Leslie:Keep up the version control, break down the tasks into small pieces, and get the system ready.

    Richard: Aided in choosing the desired language to create from the few choices we outlined in the beginning of the project. Ran the test cases to ensure they passed. Looked into the test cases to make sure the language expected as intened under valid and invalid inputs. Aided in throubleshooting the docker image and ensuring the instructions worked as intended.

    Yongyi: Working on the language reference manual; designing several features and solving the problems; solving the environment problem for Windows users by using Docker.

    John: Contributed to the development of our programming language foundation, by assisting in identifying pre-existing programming languages suitable to our project motivations. Aided with debugging by reviewing source and test code. Ensured expected language functionality through testing in both Linux and Windows environments. Helped with multi-environment automation testing.

    Henry: Worked on developing the motivations for the project, helped define the goals for our language reference manual and helped develop the automation for testing in order to work on multiple environements. Managed the teams deliverables and meetings and helped where I could on debugging.

    Joseph: finding relevant information from past projects, drafting of the language reference manual, experimenting with development infrastructure.
  
* Each team member should explain his or her most important takeaways from working on this project
    Leslie: It's important to manage the schedule and keep everything documented.

    Richard: Testing is a great time saving tool to provide rapid code coverage under code changes. With good tests, confidence in the project is improved. On the other hand, one should not purely rely on test cases and the team should still stress test the codebase with different inputs throughout the development process.

    Yongyi: Designing a language is super complicated and time consuming, even if based on an existing language. We have to design carefully and consider every potential problem behind our design. Sometimes we meet unexpected problems but finding and fixing all the problems is the most valuable part of this project.

    John: Establishing a clear and coherent vision to strive for early on, as well as maintaining a regular stream of communication with your teammates, is crucial for effectively guiding project development. Additionally, acquiring a strong understanding of language architecture is extremely important not only for development, but for troubleshooting.

    Henry: Setting up a working environment is super important. Navigating different OS systems makes this slightly more challenging than expected. In addition, planning early and not biting off too much than what you can handle would be advice I give future teams. Programming languages are complicated, so not boiling the ocean is important.

    Joseph: Unit testing is important. Finishing a test-able prototypes such as a HelloWorld frontend is more helpful and easier to navigate than finishing large chunks of the code and only then beginning to test and debug.

* Include any advice the team has for future teams
    1. Understand the Problem Domain: Before implementing, ensure you thoroughly understand the problem domain. Familiarize yourself with compiler theory, parsing techniques, and language design principles.
    2. Break it Down: A compiler is a complex system composed of several stages (lexical analysis, parsing, semantic analysis, optimization, code generation, etc.). Break down the project into smaller, manageable tasks. This makes it easier to tackle and allows for incremental progress.
    3. Understand OCaml's Role: OCaml's concise syntax, robust pattern matching, strong typing, and functional paradigm simplify complex algorithms. In the context of compiler development, OCaml's ecosystem accelerates development. Its educational value for compiler concepts is profound, making it an ideal choice. It's important to note that OCaml is not just a programming language but a powerful tool that can significantly enhance your compiler development process.
    4. IDE configuration: The disparity between different versions of OCaml LLVM can be significant, potentially leading to compatibility issues with updated operating procedures. Leveraging Docker for the project's foundation provides a consistent and isolated environment, ensuring compatibility and simplifying setup across different systems.
    5. Test, Test, Test: Testing is crucial for ensuring the correctness and reliability of your compiler. Develop a regression testing suite that can ensure your incremental development process.
