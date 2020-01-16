# Docker images

All of the Linux builders are run in Docker containers. The images for these builds can be found in the ghc/ci-images> project. They are generated via GitLab Pipelines and pushed to the `gitlab.haskell.org/ghc/ci-images` Docker repository.


# Job tags

The following job tags are currently defined:

 * `aarch64-linux`
 * `armv7-linux`
 * `ppc64le-linux`
 * `x86_64-linux`
 * `x86_64-darwin`
 * `x86_64-windows`
 * `head.hackage`: can run ghc/head.hackage> builds; this is limited to a subset of machines to increase cache reuse.
 * `trusted`: is run by a trusted entity and is therefore eligible to build release binary distributions
 * `docker`: supports [Docker-in-Docker](#configuring-docker-in-docker) (used by the ghc/ci-images> builds)
 * `lint`: a separate set of x86-64/Linux runner registrations to ensure that linters can always run with minimal latency

# Environment variables

The runners are configured to expose the following environment variables:

 * `CPUS`: The number of CPUs the job should use.

# GitLab runner configuration

## NixOS

See the `ci-worker.nix` expression in the ghc/ghc-servers> repository. This module can be dropped in `/etc/nixos/` and the following added to `/etc/nixos/configuration.nix`:
```nix
    import = [
      (import ./ghc-ci-worker.nix {name = "NAME"; cores = CORES; token = "TOKEN";})
    ];
```
where `NAME` is a descriptive name, `CORES` is the core count of the machine, and `TOKEN` is a runner token produced by `gitlab-runner register`.

## Non-NixOS
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

Also add a cron job to prune things:
```bash
echo "@daily root docker system prune --all --force --volumes" | sudo tee /etc/cron.d/docker-prune
```

## Configuring docker-in-docker

```
 sudo gitlab-runner register -n \
   --url https://gitlab.haskell.org/ \
   --registration-token REGISTRATION_TOKEN \
   --executor docker \
   --description "Docker-in-Docker runner" \
   --docker-image "docker:19.03.1" \
   --docker-privileged \
   --docker-volumes "/certs/client"
```
See <https://docs.gitlab.com/ee/ci/docker/using_docker_build.html#use-docker-in-docker-executor>.

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

Note that if a Darwin runner starts failing with mysterious crashes it may be that an operating system upgrade has occurred and XCode must be reinstalled (although we have no idea why)


## Windows configuration

Note: In the case of Windows builders it is important that we run only one build per machine. Unfortunately concurrent builds are simply too fragile under Windows' file locking semantics.

Start with Windows Server GCE image.

Enable long file path support by setting the registry key `HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\FileSystem\LongPathsEnabled` to `0x1`.

Install [Git for Windows](https://git-scm.com/download/win). When prompted
select `Git from the command line and also from 3rd-party software`. Also,
ensure `core.autocrlf` is set to `false` (this can be set during installation or
in `C:\ProgramData\Git\gitconfig` thereafter).

Install both 32- and 64-bit [msys2](https://www.msys2.org/) toolchains. Under a mingw64 shell (for the 64-bit toolchain) and mingw32 shell (for the 32-bit toolchain), run the following:

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

Install the cleanup scheduled task using [this script](https://gitlab.haskell.org/bgamari/ghc-utils/blob/master/cleanup-windows.py).

### Optimisation

* To improve IO performance disable creation of 8.3 filenames: `fsutil 8dot3name set 1`.
* To improve IO performance disable last-accessed timestaping: `fsutil behavior set disablelastaccess 1`.
* To allow the testsuite driver to use symbolic links grant `SeCreateSymbolicLinkPrivilege` to the `gitlab` user (using the `Local Security Policy` tool)
* Add Windows Defender exclusions for the following directories:
   * `C:\msys64`
   * `C:\GitLabRunner`

Reboot to ensure changes take effect.

## AArch64 configuration

On a Debian/Ubuntu machine with Docker installed as of Dec 2018:

```
$ sudo apt-get install make git ruby ruby-dev golang
$ git clone https://gitlab.com/solidnerd/gitlab-runner.git
$ cd gitlab-runner
$ git checkout feature/arm64-support
$ cd gitlab-runner
$ make deps
$ make build_simple
$ make out/helper-images/prebuilt-arm64.tar.xz
$ REVISION=$(git rev-parse --short=8 HEAD)
$ src=$(docker import out/helper-images/prebuilt-arm64.tar.xz)
$ docker tag $src gitlab/gitlab-runner-helper:arm-$REVISION
```
Currently we then just run `out/binaries/gitlab-runner run` in a `tmux` session.

Unfortunately, the `clear-docker-cache` script used [above](#linux-configuration) ends up
dropping `gitlab-runner`'s helper image. Consequently we amend the cron job accordingly,
```sh
#!/bin/sh -e

gitlab_runner=/root/gitlab-runner
$gitlab_runner/packaging/root/usr/share/gitlab-runner/clear-docker-cache

REVISION=$(git -C $gitlab_runner rev-parse --short=8 HEAD)
src=$(docker import $gitlab_runner/out/helper-images/prebuilt-arm64.tar.xz)
docker tag $src gitlab/gitlab-runner-helper:arm-$REVISION
```
Where `hash` needs to be updated whenever `gitlab-runner` is updated. Sigh. 

Also relevant: https://gitlab.com/gitlab-org/gitlab-runner/merge_requests/725

## ARMv7 on AArch64 configuration

The `ghc-arm-2` runner supports running ARMv7 containers.

## PowerPC configuration

The PowerPC box is hosted by the OSUOSL and runs Fedora 29.

### GitLab runner installation

```shell
$ yum install golang docker
$ git clone https://gitlab.com/bgamari/gitlab-runner.git
$ cd gitlab-runner
$ git checkout feature/ppc64le-support
$ cd gitlab-runner
$ make deps
$ make build_simple
$ make out/helper-images/prebuilt-ppc64le.tar.xz
$ REVISION=$(git rev-parse --short=8 HEAD)
$ src=$(docker import out/helper-images/prebuilt-ppc64le.tar.xz)
$ docker tag $src gitlab/gitlab-runner-helper:ppc64le-$REVISION
```

### Docker-in-docker image build

N.B. the latest static Docker build available from <https://download.docker.com/linux/static/stable/ppc64le/> is currently 18.06.
```shell
$ git clone https://github.com/docker-library/docker.git
$ cd docker/
$ git checkout 08e48bcb07e3edeff5399676924c42d18f894df6 # last commit with support for 18.06
$ docker build 18.06/
$ docker build 18.06/dind/
```

## FreeBSD

We use `gitlab-runner`'s VirtualBox executor to test on FreeBSD. The base VM is built with Vagrant and the following configurations:
```ruby
Vagrant.configure("2") do |config|
  config.vm.box = "freebsd/FreeBSD-11.3-RELEASE"
  #config.vm.box = "freebsd/FreeBSD-12.0-RELEASE"
  config.vm.guest = :freebsd
  config.ssh.shell = "sh"
  config.vm.synced_folder ".", "/vagrant", id: "vagrant-root", disabled: true
  config.vm.boot_timeout = 600
  config.vm.provision "shell", inline: $script
  # Requires vagrant-disksize, which is 
  # broken: https://github.com/sprotheroe/vagrant-disksize/issues/32
  #config.disksize.size = "15GB"

  config.vm.provider "virtualbox" do |v|
    v.cpus = 5
    v.memory = 8096 # megabytes
  end
end

$script = <<-SCRIPT
mkdir -p /usr/local/etc/pkg/repos/
echo 'FreeBSD: { url: "pkg+http://pkg.FreeBSD.org/${ABI}/latest" }' > /usr/local/etc/pkg/repos/FreeBSD.conf
pkg install -y git bash gitlab-runner gmake curl wget libiconv autoconf automake python3 py36-sphinx ghc

git clone https://github.com/haskell/cabal
cd cabal
git checkout cabal-install-v3.0.0.0
cd cabal-install
export NO_DOCUMENTATION=YES
./bootstrap.sh

$HOME/.cabal/bin/cabal update
$HOME/.cabal/bin/cabal install alex happy

echo "ghc::::::GHC builder::/bin/sh:" | adduser -f -
SCRIPT
```
Initial configuration looks something like this:
```
$ mkdir test
$ cat >Vagrantfile <<EOF
[Past Vagrantfile above]
EOF
$ vagrant up --provision
$ vm=$(VBoxManage list vms | cut -f 2 -d ' ')
$ VBoxManage controlvm $vm poweroff
$ VBoxManage clonevm $vm --name "ghc-ci-base"
```
And add an entry to `/etc/nixos/configuration.nix`:
```nix
  imports = [
    (import ./ghc-ci-vbox.nix {name = "ghc-ci-m1-vbox"; cores = 8; token = "tXBbomTiwnzJJR1q3-Lk"; vmName = "ghc-ci-base"; })
    ...
  ];
```
## Current Runners

See the [Google Doc](https://docs.google.com/spreadsheets/d/1_UncQmtD5PkinLgq4DSB4Y5dy7PhOPPjPp6qnhZNA9w/edit#gid=0)

## Updating bootstrap compiler

To update the GHC version used as the bootstrap compiler there are a few places that need to be updated:

 * for the Linux jobs update the `Dockerfile`s in the ghc/ci-images> project:
    * Update the binary distribution tarball URLs
    * Update the LLVM versions and tarball URLs (N.B. in the ARM and AArch64 case there are two relevant LLVM versions: the version needed by the bootstrap compiler and the version needed by GHC `master`).
 * for the Darwin and Windows jobs: the `GHC_TARBALL` and `CABAL_TARBALL` variables in `.gitlab-ci.yml` in the ghc/ghc> project need to be updated.

## Head.hackage builds

In addition to building GHC itself, we also provide CI jobs which build a subset of Hackage using the ghc/head.hackage> patchset. This builds can be triggered in one of three ways:

 * By manually running the CI job
 * By pushing to an MR bearing the ~user-facing label
 * Via the scheduled nightly builds

The `head.hackage` CI infrastructure is built on the Nix infrastructure in the `head.hackage` repository. The overall flow of control looks like this:

 1. The  ghc/ghc> pipeline's `hackage` job is triggered
 2. The `hackage` job triggers a pipeline of the ghc/head.hackage> project using a job trigger token owned by @head.hackage, passing its pipeline ID via the `GHC_PIPELINE_ID` variable
 3. The ghc/head.hackage> job uses the value of `GHC_PIPELINE_ID` to lookup the job ID of the pipeline's `x86_64-fedora27` job. This requires a personal access token which is provided by the @head.hackage user.
 4. The ghc/head.hackage> job fetches the binary distribution of the GHC `x86_64-fedora27` job
 5. The ghc/head.hackage> job uses [ghc-artefact-nix](https://github.com/mpickering/ghc-artefact-nix) to install the bindist
 6. The ghc/head.hackage> job uses `scripts/build-all.nix` to build a subset of Hackage (identified by the `testedPackages` attribute) using the bindist
 7. The ghc/head.hackage> job uses `scripts/summary.py` to produce a summary of the build results. This includes:
    1. a `dot` graph showing the package dependency graph and their failures
    2. a tarball containing the build log files
    3. log output showing abbreviated logs of failed builds
    
    (1) and (2) are then uploaded as artifacts.

Note that this currently requires a patched GitLab installation (see https://gitlab.com/gitlab-org/gitlab-ce/merge_requests/25712)