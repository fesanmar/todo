# Todo

A to-do CLI application made with Haskell.

## Usage

As a CLI application, just open a terminal and type any valid command to run it.

``` cmd
usage: todo <command> [<args>]\n,
      These are the todo commands:
          config                                    Shows user's configuration
          ls                                        Shows user's to-do lists.
          new <todoListName>                        Creates a new to-do list.
          remove <todoListName>                     Removes a existing to-do list.
          rename <todoListName> <newName>           Renames a existing to-do list.
          dl <todoListName>                         Sets <todoListName> as the default list, so '--' can be 
                                                    used instead of the list name in the others commands.
          view <todoListName>                       Shows a to-do list's tasks.
          add [-b] <todoListName> <task>            Append a new task to the passed to-do list or prepend it
                                                    if [-b] is setted.
          complete <todoListName> <taskIndex>       Complete the to-do list's passed task number.
          bump <todoListName> <taskIndex> [steps]   Bumps the passed task to the top of the to-do list or
                                                    n steps up.
          drop <todoListName> <taskIndex> [steps]   Drops the passed task to the bottom of the to-do list 
                                                    or n steps down.
          mv <listFrom> <taskIndex> <listTo>        Moves a task from a list to another
          help                                      Show this usage.
```
