# Todo

A to-do CLI application made with Haskell.

## Usage

As a CLI application, just open a terminal and type any valid command to run it.

``` cmd
usage: todo <command> [<args>]

These are the todo commands:
    new <todoListName>                  Creates a new to-do list
    view <todoListName>                 Show a to-do list's tasks
    add <todoListName> <task>           add a new task to the passed to-do list
    remove <todoListName> <taskIndex>   remove the to-do list's passed task number
    bump <todoListName> <taskIndex>     bumps the passed task to the top of the to-do list
    help                                Show this usage
```
