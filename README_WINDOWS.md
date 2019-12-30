<!--
   - SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
   -
   - SPDX-License-Identifier: LicenseRef-ReplaceMe
   -->

# Building StakerDAO project (and dependencies) on Windows.

You will be able to use StakerDAO project *only* under Windows 10 v1903 or later
(this version is tested to work, but perhaps v1803 or even earlier will do the thing).

This requirement is because you will need to use WSL (Windows Subsystem for Linux)
to run (and, if necessary, build) the project.

Technically it's absolutely not a problem to build native Windows StakerDAO executables,
which will run on all the Windows versions back to Vista, but the problem is that StakerDAO
executable offloads some critically important tasks to tezos client, and at the time of
this writing (Dec 30, 2019) there exist *NO* native Windows tezos client.

So *THE ONLY* option to run StakerDAO on Windows is currently limited to Windows 10 and
requires WSL to be installed.


## Install WSL
  
1. Go to "Control panel"/"Programs and Features"/"Turn Windows features on or off" and check
"Windows Subsystem for Linux" box if it's not checked yet. Follow the prompts Windows tells you.

2. After WSL is installed, go to Windows Store and install "Ubuntu" (at the time of this
writing, Dec 30, 2019, it's Ubuntu LTS 18.04, you may alternatively choose "Ubuntu-18.04",
this is exactly the same distro, named slightly differently).

## Use Linux binaries without building

If you have Linux binaries of StakerDAO executables and Tezos client built against
sufficiently recent 'glibc' (most non-ancient "mainstream" distros) you may well
stop here, 99% it will work under WSL.

You shall still install 'libhidapi-libusb0' required by Tezos client:
```
sudo apt install libhidapi-libusb0
```

If you don't have Linux binaries of these available, look further.


## Install nix

The supported way to build StakerDAO project is to use Nix.

1. To be able to use Nix on WSL you first *SHALL* create an /etc/nix/nix.conf file
containing these lines (may skip comments):

```
# Work around missing cgroups support https://github.com/Microsoft/WSL/issues/994
sandbox = false
# Work around incorrect file locking https://github.com/Microsoft/WSL/issues/2395
use-sqlite-wal = false
```

2. Then you *SHALL* enable Long Path support. To do so, go to Registry Editor and set
'HKLM\SYSTEM\CurrentControlSet\Control\FileSystem' 'LongPathsEnabled' key's value to 1.

3. Only then install "Nix":
```
curl https://nixos.org/nix/install | sh
```

4. After the installation finishes, either log out and then back in, or run
```
. /home/your_user_name/.nix-profile/etc/profile.d/nix.sh
```
(nix will remind you to do this).


## Build StakerDAO

1. Install 'make':
```
sudo apt install make
```

2. Clone 'staker-dao' repo and build the project

```
git clone https://github.com/serokell/staker-dao.git
cd staker-dao
make build
```

*IMPORTANT*: please do this in a VolFs, *not* DrvFs (i.e., "vanilla" Windows), directory,
e.g., the directory natively located (not symlinked from the DrvFs directory) under your WSL
home dir, otherwise you'll get a lot of directory access problems in the last build stage
(it's still possible to build things even in this case, if the build fails with
`setFileMode: permission denied (Operation not permitted)`, simply invoke `make` again, you might need
to do this a couple of dozens of times).


## Build Tezos client

1. Install 'opam', at the time of this writing the last released version
of opam is 2.0.5 (you may replace "2.0.5" everywhere below with something newer if any):

* Install prerequisites
```
sudo apt install -y rsync git m4 build-essential patch unzip wget libev-dev libhidapi-dev
```

* Download opam
```
wget https://github.com/ocaml/opam/releases/download/2.0.5/opam-2.0.5-x86_64-linux
```

* copy it to the standard location (may copy to any location in your PATH) and mark executable
```
sudo cp opam-2.0.5-x86_64-linux /usr/local/bin/opam
sudo chmod a+x /usr/local/bin/opam
```

2. Get tezos client

```
git clone https://gitlab.com/tezos/tezos.git`
cd tezos
# We use `babylonnet`
git checkout babylonnet
```

3. Init 'opam' and build the client (answer 'y' to all opam questions)

```
opam init --disable-sandboxing
eval $(opam env)
make build-deps
eval $(opam env)
make
```

*IMPORTANT*: You *SHALL* supply `--disable-sandboxing` option to `opam init`.


4. If all is ok (it should), you can find tezos client as '_build/default/src/bin_client/main_client.exe',
now you shall rename/copy it to the staker-dao directory:
```
cp _build/default/src/bin_client/main_client.exe the_dir_where_you_cloned_staker-dao/staker-dao/tezos-client
```
