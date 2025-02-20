# nnhelplightning

A custom backend for emacs GNUS (Mail and News Reader) to support Help
Lightning's Help Threads.

(c) 2025 Marcus Dillavou <line72@line72net>

Released under the GPLv3 or later

## Installing

- Install request from melpa
   - M-x package-list-packages
   - Find `request`
   - Press `i`, then `x` to install
- Copy `nnhelplightning.el` somewhere (like
  `~/.emacs.d/nnhelplightning/`)
- Edit your `~/.emacs` file:
```lisp
;; Add the path to nnhelplightning.el 
(add-to-list 'load-path "~/.emacs.d/nnhelplightning/")

;; Load nnhelplightning custom backend
(require 'nnhelplightning)

;; Configure gnus to use this backend
(setq gnus-select-method '(nnhelplightning "Help Lightning"
                                           (environment . us)
                                           (pat . "change-this-to-your-pat")))
```

## Authentication (PAT)

This requires an authentication token. Log into Help Lightning, click
on your name in the top right, and choose `Settings`. Select the
`Security` tab and generate a `Personal Access Token (PAT)`. Save this
as it cannot be restored!

## Subscribing to Help Threads

Any time a new Help Thread is created, it will not be
auto-subscribed. To subscribe to a new Help Thread:

1. Press `F` to scan for new groups
1. Press `AA` to show all available groups
1. Find the new groups and press `U` to subscribe to them
1. Press `g` to fetch new messages in subscribed groups

## Running

It is recommended that you read the [GNUS
Manual](https://www.gnu.org/software/emacs/manual/html_node/gnus/index.html#SEC_Contents)
if you are not familiar with GNUs.

### Getting New Messages

GNUs does not auto-update. Press `g` to refresh all subscribed groups
for new messages. GNUs will show a count of messages next to each
group (Although, this count will initially be way off, as messages
don't have sequential IDs).

### Showing All Groups (Help Threads)

GNUs by default only shows groups (Help Threads) with unread
messages. If you'd like to all groups, press `L`. To switch back to
only unread groups, press `l`.

If you want GNUs to always show all groups, you can add the following
to your `~/.emacs` file:

```lisp
(setq gnus-permanently-visible-groups ".*")
```

### Posting

To post a message to a group, enter a group, then press `a`. This will
bring up emacs built-in email editor.

You MUST provide a subject, even though it is not used in Help
Lightning!

After composing your message, press `C-c C-c` to submit the
message. You _may_ get a warning from emacs if you have not configured
mail info, but that is ok, as it is not posted through email, but via
Help Lightning's RESTful API.

## Limitations

This custom backend has several limitations:

1. It is not possible to create new groups (Help Threads) yet.
1. Only Text type messages are handled. This means no images,
   documents, procedures, call info, etc...

