# followmon

A Telegram bot that monitors Twitter followers/followees, and notifies the user
on Telegram if there are changes.

This makes it easier to tell when other users have followed/unfollowed, and
although the user should know if they follow someone else, this will also let
them know if they are removed as a follower by their followee.

## Usage

A single command-line argument should be provided—the path to a Dhall
configuration file. The configuration file should have the following structure

```dhall
{ twitterBearerToken = "<Twitter Bearer Token (needs v2 access)>"
, intervalSeconds = 60 -- Number of seconds between each check
, username = "<Your Twitter username>"
, filename = "<Path to a file to store state between invocations>"
, telegramBotToken = "<Telegram bot token>"
, telegramChatID = "<Telegram chat ID to send messages to>"
, healthcheckUrl = Some "<healthchecks.io URL>" -- or None Text
, userCacheSeconds = 86400 -- Number of seconds to cache user information
}
```

`filename` should point to a path that the program can write to (e.g.
`./lists.bin`), where it will write the list of followers/followees upon each
update, so that it can keep track of changes between exiting and starting the
next time.

If the above configuration file is written to `/path/to/config.dhall`, the
program can be invoked in any of the following ways:

```
# From the project directory.
nix run . -- /path/to/config.dhall
cabal run followmon:exe:followmon -- /path/to/config.dhall

# Or, if compiled and installed to path.
followmon /path/to/config.dhall
```

## Note

This was written for my personal use, so there is not a lot of customization
available. Error messages will be sent to Telegram as well, as I am both the
intended user and the developer.

If you would like to extend this for your use, or have any ideas to make this
more generally usable, please feel free to open an issue.

### Paid Twitter API

It is no longer possible to access the endpoints required for anything less than
an exorbitant fee, so this project is effectively dead for now. A scraping-based
solution may be interesting...
