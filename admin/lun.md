# lun


lun is a vm, running on lambda. It runs the community services.

## VM


The config is in `lambda:/etc/libvirt/qemu/community.xml`, and the disk image
`/srv/community/vdisk.img`.


It's controlled by `virsh`, e.g.:

```wiki
lambda$ sudo virsh list --all
 Id Name                 State
----------------------------------
  - community            shut off

lambda$ sudo virsh start community
Domain community started

lambda$ sudo virsh list --all
 Id Name                 State
----------------------------------
  2 community            running
```


To start it with a console, use:

```wiki
lambda$ sudo virsh start community --console
```


or to connect to the console if it's already running:

```wiki
lambda$ sudo virsh console community
```


and `Ctrl+]` to exit the console.


See the `virsh` manpage for more information.

## exim


The config is in `/etc/exim4/exim4.conf.template` with local changes delimited by:

```wiki
# start lun local
...
# end lun local
```


After changing, run `/usr/sbin/update-exim4.conf` then `/etc/init.d/exim4 reload`.

## apache


The various sites are configured in `/etc/apache2/sites-available/*`. Symlinks in
`/etc/apache2/sites-enabled/` enable them.


Modules are similarly enabled by symlinks in `/etc/apache2/mods-enabled/`.


After changing anything, run `/etc/init.d/apache2 reload`.

## data


User data for service `foo` is generally in `/srv/foo`.

## mrtg


The mrtg config is in `/etc/mrtg.cfg`, some helpers are in `/srv/local/mrtg/`, and the output goes to `/var/www/mrtg`. The URL for it is [ http://lun.haskell.org/mrtg/](http://lun.haskell.org/mrtg/)