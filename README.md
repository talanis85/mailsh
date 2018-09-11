mailsh - A command line mail user agent
=======================================

Mailsh is a purely command-based MUA for the terminal, designed
to be used on local maildirs (probably in combination with something
like [isync](http://isync.sourceforge.net)).

It is made to embrace the powerful unix command line workflow, which
means you can pipe data in and out of it in various ways, use it in
scripts, etc.

Installation
============

1. Install [stack](https://haskellstack.org)
2. `git clone https://github.com/talanis85/mailsh.git`
3. `cd mailsh && stack install`

Quick start
===========

`cd` to the maildir you want to use.

To list all unread messages, simply type

    mailsh

Note: Mailsh needs to build a cache the first time it is run. So
this can take a while, depending on the size of your maildir.

You can manage as many maildirs as you want. Mailsh will always
operate on the current working directory.

To read a specific message, do

    mailsh read <message-number>

To read the oldest unread message, do

    mailsh next

To see the built-in help, do

    mailsh -h

To read more about a command, do

    mailsh <command> -h

Command reference
=================

* `mailsh read [MESSAGE] [-r|--render RENDERER]`

        Read a message.

      Available options:
        MESSAGE                  Either a message number (e.g. 123), a part reference
                                 (e.g. 2#123) or '-' for stdin. Default is the last
                                 accessed message.
        -r,--render RENDERER     Available renderers are: full, outline, preview,
                                 noquote
        -h,--help                Show this help text

* `mailsh cat [MESSAGE]`

        Output raw message data.

      Available options:
        MESSAGE                  Either a message number (e.g. 123), a part reference
                                 (e.g. 2#123) or '-' for stdin. Default is the last
                                 accessed message.
        -h,--help                Show this help text

* `mailsh view [MESSAGE]`

        View a message or part with mailcap.

      Available options:
        MESSAGE                  Either a message number (e.g. 123), a part reference
                                 (e.g. 2#123) or '-' for stdin. Default is the last
                                 accessed message.
        -h,--help                Show this help text

* `mailsh save [MESSAGE] -d DIR`

        Save a part to disk using its given filename.

      Available options:
        MESSAGE                  Either a message number (e.g. 123), a part reference
                                 (e.g. 2#123) or '-' for stdin. Default is the last
                                 accessed message.
        -d DIR                   Where to save the attachment.
        -h,--help                Show this help text

* `mailsh next [-r|--render RENDERER]`

        Read the next unread message.

      Available options:
        -r,--render RENDERER     Available renderers are: full, outline, preview,
                                 noquote
        -h,--help                Show this help text

* `mailsh compose [--dry] [-a|--attachment FILE] RECIPIENT`

        Compose a new message using your EDITOR

      Available options:
        --dry                    Dont actually send the message
        -a,--attachment FILE     Attach a file (can occur multiple times)
        RECIPIENT                The recipient's address
        -h,--help                Show this help text

* `mailsh reply [--dry] [-g|--group] [-a|--attachment FILE] [MESSAGE]`

        Reply to a message using your EDITOR.

      Available options:
        --dry                    Dont actually send the message
        -g,--group               Group reply
        -a,--attachment FILE     Attach a file (can occur multiple times)
        MESSAGE                  Either a message number (e.g. 123), a part reference
                                 (e.g. 2#123) or '-' for stdin. Default is the last
                                 accessed message.
        -h,--help                Show this help text

* `mailsh forward [--dry] RECIPIENT [MESSAGE]`

        Forward a message.

      Available options:
        --dry                    Dont actually send the message
        RECIPIENT                The recipient's address
        MESSAGE                  Either a message number (e.g. 123), a part reference
                                 (e.g. 2#123) or '-' for stdin. Default is the last
                                 accessed message.
        -h,--help                Show this help text

* `mailsh headers [-l|--limit LIMIT] [FILTER]`

        List all headers given a filter expression.

      Available options:
        -l,--limit LIMIT         How many headers to display
        FILTER                   A filter expression
        -h,--help                Show this help text

      Valid filter expressions are:

        /string/       'Subject' or 'From' contains 'string'
        a              Matches all messages
        d              Matches draft messages
        r              Matches replied messages
        s              Matches seen messages
        t              Matches trashed messages
        f              Matches flagged messages
        all            Equivalent to '~t' (all non-trashed messages)
        new            Equivalent to '~s' (all unseen messages)

      All of these expressions can be combined with the logical operators '&' (and), '|' (or) and '~' (not).

      Examples:
        f&~d                All flagged messages that are not a draft
        r&/hello/           All replied messages that contain the string 'hello'
        (t&/foo/)|(d&/bar/) All trashed messages containing 'foo' and all drafts containing 'bar'

* `mailsh trash [MESSAGE]`

        Trash a message.

      Available options:
        MESSAGE                  Message number. Default is the last accessed message.
        -h,--help                Show this help text

* `mailsh recover [MESSAGE]`

        Recover a trashed message.

      Available options:
        MESSAGE                  Message number. Default is the last accessed message.
        -h,--help                Show this help text

* `mailsh purge `

        Permanently delete trashed messages.

      Available options:
        -h,--help                Show this help text

* `mailsh unread [MESSAGE]`

        Mark a message as unread.

      Available options:
        MESSAGE                  Message number. Default is the last accessed message.
        -h,--help                Show this help text

* `mailsh flag [MESSAGE]`

        Mark a message as flagged.

      Available options:
        MESSAGE                  Message number. Default is the last accessed message.
        -h,--help                Show this help text

* `mailsh unflag [MESSAGE]`

        Mark a message as unflagged.

      Available options:
        MESSAGE                  Message number. Default is the last accessed message.
        -h,--help                Show this help text

* `mailsh filename [MESSAGE]`

        Get the filename of a message.

      Available options:
        MESSAGE                  Message number. Default is the last accessed message.
        -h,--help                Show this help text

* `mailsh outline [MESSAGE]`

        Display an outline of a message.

      Available options:
        MESSAGE                  Either a message number (e.g. 123), a part reference
                                 (e.g. 2#123) or '-' for stdin. Default is the last
                                 accessed message.
        -h,--help                Show this help text

* `mailsh tar [MESSAGE]`

        Output all attachments of a message to stdout as a tar archive.

      Available options:
        MESSAGE                  Either a message number (e.g. 123), a part reference
                                 (e.g. 2#123) or '-' for stdin. Default is the last
                                 accessed message.
        -h,--help                Show this help text
