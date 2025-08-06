mailsh - A command line mail user agent
=======================================

[![Build Status](https://travis-ci.org/talanis85/mailsh.svg?branch=master)](https://travis-ci.org/talanis85/mailsh)

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
