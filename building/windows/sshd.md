# Setting up the SSH daemon


As most GHC developers are used to work on Unix workstations, having to use a graphical remote desktop session to access the CygWin/MSYS2 environment is rather disruptive to typical workflows. By setting up a SSH daemon inside the MSYS2 environment, such a CygWin/MSYS2 environment can be treated almost as yet another remote Unix environment.


While on CygWin setting up `sshd` is taken care of by the provided `ssh-host-config` shell script which creates the required user accounts and installs `sshd` as a system service in Windows, with MSYS2 these steps need to be performed manually. To this end, here's the steps needed to setup `sshd` manually (which I had to find out the hard, time-consuming way, hence documenting them here):

- `pacman -S cygrunsrv openssh mingw-w64-$(uname -m)-editrights`
- `ssh-keygen -A`

- Create priviledged `cyg_server` user (required in most current Windows versions)

  ```

  # will be used as HOME
  dos_var_empty=$(/usr/bin/cygpath -w /var/empty)

  # create some random password; this is only needed internally by cygrunsrv
  _password=... 

  username=cyg_server
  unpriv_user=sshd
  admin_user=$(whoami)

  # Usually, 'admingroup=Administrators'
  admingroup=$(/usr/bin/mkgroup -l | /usr/bin/awk -F: '{if ( $2 == "S-1-5-32-544" ) print $1;}')


  # NB: From some reason, calling `net` doesn't work in MSYS's bash (seems that '/' isn't passed transparently)
  net user "${username}" "${_password}" /add /fullname:"Privileged server" /homedir:${dos_var_empty} /yes

  net localgroup "${admingroup}" "${username}" /add

  net user "${unpriv_user}" /add /fullname:"${unpriv_user} privsep" "/homedir:${dos_var_empty}" /active:no

  # set infinite passwd expiry
  passwd -e ${username}

  # set required priviledges; 
  # As of 2015/04/28 the `editrights.exe` program is available in MSYS2 from
  # either the mingw-w64-i686-editrights or mingw-w64-x86_64-editrights package.
  export PATH=/mingw64/bin/:/mingw32/bin:$PATH
  editrights -a SeAssignPrimaryTokenPrivilege -u ${username} && \
  editrights -a SeCreateTokenPrivilege -u ${username} && \
  editrights -a SeTcbPrivilege -u ${username} && \
  editrights -a SeDenyRemoteInteractiveLogonRight -u ${username} && \
  editrights -a SeServiceLogonRight -u ${username}

  # add passwd entry
  pwd_entry="$(/usr/bin/mkpasswd -l -u "${username}" | /usr/bin/sed -n -e '/^'${username}'/s?\(^[^:]*:[^:]*:[^:]*:[^:]*:[^:]*:\).*?\1'/var'/empty:/bin/false?p')"
  echo "${pwd_entry}" >> "/etc/passwd" 

  pwd_entry="$(/usr/bin/mkpasswd -l -u "${unpriv_user}" | /usr/bin/sed -n -e '/^'${unpriv_user}'/s?\(^[^:]*:[^:]*:[^:]*:[^:]*:[^:]*:\).*?\1'/var'/empty:/bin/false?p')"
  echo "${pwd_entry}" >> "/etc/passwd" 

  pwd_entry="$(/usr/bin/mkpasswd -l -u "${admin_user}")"
  echo "${pwd_entry}" >> "/etc/passwd"

  # finally, register service with cygrunsrv
  /usr/bin/cygrunsrv -I sshd -d "CYGWIN sshd" -p /usr/bin/sshd -a "-D" -y tcpip -u cyg_server -w "${_password}"

  # the SSH service should start up automatically when the Windows VM is rebooted. You can manually restart the service by running `net stop sshd` + `net start sshd`
  net start sshd

  # if something doesn't work, make sure  /etc/ssh*_* /var/empty /var/log/lastlog /var/log/sshd.log are accessible by cyg_server user.
  # NB: if you need to tweak env-vars such as PATH or MSYSTEM, use ~/.bashrc or ~/.bash_profile
  ```


TODO evaluate alternative script at [https://gist.github.com/samhocevar/00eec26d9e9988d080ac](https://gist.github.com/samhocevar/00eec26d9e9988d080ac)


