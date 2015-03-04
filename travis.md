# Travis


Travis-CI is a free-for-open-source continuous integration service.

## What does it do?


It watches the [repository](repositories) for new commits (any branch) and validates them. The results are presented on

- [ the Travis results page](https://travis-ci.org/ghc/ghc/builds)


where you can select one of the recent build results.


To view a build log, you would

- click on the build number in the first column (e.g. 617)
- then scroll down and select one of the two jobs (one is built with debugging, one without). Click on the number (e.g. 617.1)
- the next page contains the build log, but presented using JavaScript, which may or may not be too much for your browser, and may be truncated. Therefore, while the page is loading, quickly click the “Download Log” above the build log. This will open the log as a plain text file.

## What is validated?


Because of time constraints, not a full validation run is done. Instead

- only static libraries are built,
- GHC is linked statically,
- the test suite is run in “fast” mode,
- all performance tests are skipped,
- and neither haddock nor documentation is built.


It does all this in two variants

- without `-DDEBUG`, to match what we release, and
- with `-DDEBUG`, to catch assertions.


These settings are made in [ghc/.travis.yml](/trac/ghc/browser/ghc/.travis.yml)[](/trac/ghc/export/HEAD/ghc/.travis.yml). You can conveniently experiment with different settings in a `wip/...` branch.

## Statuses

- **Success** (green checkmark):

>
> The validation run went through without problems. Great!

- **Failure** (Red cross):

>
> There was a validation error, such as a build failure or a failing test case. Go and fix it!

- **Error** (Grey exclamation mark): Travis could not finish the build. Most often, this is due to the build exceeding the time limit of 50 minutes. 

>
> In that case, you can probably ignore the problem. If you are a [ of the GitHub GHC team](https://github.com/orgs/ghc/members|member), you can restart the build, to keep the build history tidy.

## Mail status


Travis is not 100% reliable for us, as we occasionally hit the time limit. Therefore, mails about failing reports are sent to Joachim and the [ghc-builds](mailing-lists-and-irc) mailing lists. Joachim will report true positives to `ghc-dev` or the commiter directly.


If we can improve this, we could make Travis send mails directly to the commiter.


Again, this is configured in [ghc/.travis.yml](/trac/ghc/browser/ghc/.travis.yml)[](/trac/ghc/export/HEAD/ghc/.travis.yml). This also means that if you have a long-living feature branch you can put your own address in the file.
