# Castmin Bot

Slack integration to help with general CAST admin

## Why?

Important new documents are often added to specific folders in the CAST google drive. Google Drive however does not let you filter notifications on a per folder basis (updates on everything that happens in the drive are sent).

As a remote organisation, slack is the primary way in which we communicate as a team, so notifications there are picked up immediately.

It would be great if we could combine finer grained notification settings for google drive and send them to slack.

## What?

Castmin Bot is a Haskell Snap server that communicates between the Google Drive API and Slack.

+ Castmin bot polls a google drive folder for new files
+ If it finds files created since it last checked it will notify slack with a message (and links to the files)
+ Notifications can be turned on and off from Slack via a [slash-command](https://api.slack.com/slash-commands) `/castmin-bot on` or `/castmin-bot off`

## Get up and running

The project is built using [`stack`](https://docs.haskellstack.org/en/stable/README/) (install guides [here](https://www.youtube.com/watch?v=sRonIB8ZStw) and [here](https://docs.haskellstack.org/en/stable/README/)).

install and build the project by running:

```sh
> stack build
```

### Google Drive and Slack

You'll need a google drive `client_id` and `client_secret` ([signup info here](https://developers.google.com/drive/v3/web/enable-sdk)).  
And to create a new slack app with a `webhook` and a `slash command` ([api reference here](https://api.slack.com/)).

### Testing Webhooks

To develop webooks locally (i.e. incoming messages from slack to the server), I've been using [ngrok](https://ngrok.com/) which creates a secure tunnel to localhost. When registering a slash command, start ngrok and paste in the temporary url.

## Environment Variables

These env vars (in a `.env` file) are required for the project to work:
```DotEnv
PORT=<server port>
SLACK_WEBHOOK_URL=<webhook url to post new messages to slack>
GDRIVE_CLIENT_ID=<client id for google drive>
GDRIVE_CLIENT_SECRET=<client secret for google drive>
POLLING_SECRET_KEY=<random string for internal http client>
REDIRECT_URI=<redirect uri for google drive authentication>
REDIS_URL=<url to redis database>
SLACK_VERIFICATION_TOKEN=<token sent from slack slash commands>
```

## Start the application

To start the snap server

```sh
> stack build
> stack exec castmin-bot
```

## Test Suite

To run the test suite

```sh
stack test
```
