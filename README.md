# Todo

A to-do CLI application made with Haskell.

## Usage

As a CLI application, just open a terminal and type any valid command to run it.

``` cmd
usage: todo <command> [<args>]

These are the todo commands:
    ls                                  Shows user's to-do lists
    new <todoListName>                  Creates a new to-do list
    remove <todoListName>               removes a existing to-do list
    rename <todoListName> <newName>     renames a existing to-do list
    view <todoListName>                 Shows a to-do list's tasks
    add [-b] <todoListName> <task>      append a new task to the passed to-do list or prepend it if [-b] is setted.
    complete <todoListName> <taskIndex> complete the to-do list's passed task number
    bump <todoListName> <taskIndex>     bumps the passed task to the top of the to-do list
    help                                Show this usage
```
