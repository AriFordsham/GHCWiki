# Docker images

All of the Linux builders are run in Docker containers. The images for these builds can be found in the [ghc/ci-images](https://gitlab.haskell.org/ghc/ci-images) project. They are generated via GitLab Pipelines and pushed to the `gitlab.haskell.org/ghc/ci-images` Docker repository.

# GitLab runner configuration

A sample configuration:
```toml
concurrent = 1                    # <---- Set
check_interval = 0

[session_server]
  session_timeout = 1800

[[runners]]
  name = "centriq.haskell.org"    # <---- Set
  url = "https://gitlab.staging.haskell.org/"
  token = "TOKEN"                 # <---- Set
  executor = "docker"
  environment = ["CPUS=16"]       # <---- Set
  output_limit = 16000            # <---- Set
  [runners.docker]
    tls_verify = false
    image = "ghcci/aarch64-linux-deb9:0.1"
    privileged = false
    disable_entrypoint_overwrite = false
    oom_kill_disable = false
    disable_cache = false
    volumes = ["/cache"]
    shm_size = 0
  [runners.cache]
    [runners.cache.s3]
    [runners.cache.gcs]

```

## Job tags

The following job tags are currently defined:

 * `x86_64-linux`
 * `aarch64-linux`
 * `x86_64-darwin`
 * `x86_64-windows`
 * `docker`: supports Docker-in-Docker (used by the ghc/ci-images builds)

## Linux configuration

It is [necessary](https://gitlab.com/gitlab-org/gitlab-runner/issues/2980#note_131320536) to ensure that the Docker cache is periodically cleaned:
```bash
$ cat | sudo tee /etc/cron.daily/gitlab-clear-docker-cache <<EOF
#!/bin/sh -e

/usr/share/gitlab-runner/clear-docker-cache
EOF
$ sudo chmod ug+rx /etc/cron.daily/gitlab-clear-docker-cache
```

## Darwin configuration

Install Homebrew.
```
$ brew install autoconf automake python3 wget
```

Install `gitlab-runner` according to
<https://docs.gitlab.com/runner/install/osx.html>.


## Windows configuration

Note: In the case of Windows builders it is important that we run only one build per machine. Unfortunately concurrent builds are simply too fragile under Windows' file locking semantics.

Start with Windows Server GCE image.

Install [Git for Windows](https://git-scm.com/download/win). When prompted
select `Git from the command line and also from 3rd-party software`. Also,
ensure `core.autocrlf` is set to `false` (this can be set during installation or
in `C:\ProgramData\Git\gitconfig` thereafter).

Install msys2.

```
$ pacman -Syuu
$ pacman -S \
    git tar bsdtar unzip binutils autoconf make xz \
    curl libtool automake python python3 p7zip patch ca-certificates \
    mingw-w64-$(uname -m)-gcc mingw-w64-$(uname -m)-python3-sphinx \
    mingw-w64-$(uname -m)-tools-git
```

Create a `gitlab` user with a password.
[Grant](https://docs.gitlab.com/runner/faq/README.html#the-service-did-not-start-due-to-a-logon-failure-error-when-starting-service-on-windows)
this account the `SeServiceLogonRight` in the `Local Security Policy` tool.


[Download](https://docs.gitlab.com/runner/install/windows.html) 
`gitlab-runner` and place in `C:\GitLabRunner`. In an Administrator shell run,
```
cd C:\GitLabRunner
gitlab-runner install --user ".\gitlab" --password ...
```
Register the runner.

### Optimisation

* To improve IO performance disable creation of 8.3 filenames: `fsutil 8dot3name set 1`.
* To improve IO performance disable last-accessed timestaping: `fsutil behavior set disablelastaccess 1`.
* To allow the testsuite driver to use symbolic links grant `SeCreateSymbolicLinkPrivilege` to the `gitlab` user (using the `Local Security Policy` tool)

Reboot to ensure changes take effect.

## AArch64 configuration

On a Debian/Ubuntu machine as of Dec 2018:

```
$ sudo apt-get install ruby ruby-dev golang
$ git fetch https://gitlab.com/solidnerd/gitlab-runner.git 
$ git checkout feature/arm64-support
$ cd gitlab-runner
$ make deps
$ make build_simple
$ make out/helper-images/prebuilt-arm64.tar.xz
```

Also relevant: https://gitlab.com/gitlab-org/gitlab-runner/merge_requests/725


## Current Runners

See the [Google Doc](https://docs.google.com/spreadsheets/d/1_UncQmtD5PkinLgq4DSB4Y5dy7PhOPPjPp6qnhZNA9w/edit#gid=0)
