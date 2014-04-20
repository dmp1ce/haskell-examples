#!/bin/bash

# Use an updated mirrorlist (use commands below to regenerate)
#pacman -S --needed --noconfirm reflector
#reflector --verbose -l 5 --sort rate --save /etc/pacman.d/mirrorlist
echo "Updating mirrorlist"
test -f /etc/pacman.d/mirrorlist.backup || (echo "Backing up original mirrorlist" && cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.backup)
cp /vagrant/mirrorlist /etc/pacman.d/mirrorlist

echo "Updating pacman.conf with [haskell-core] database"
test -f /etc/pacman.conf.backup || (echo "Backing up original pacman.conf" && cp /etc/pacman.conf /etc/pacman.conf.backup)
cp /vagrant/pacman.conf /etc
pacman-key -r 4209170B
pacman-key --lsign-key 4209170B
pacman -Syy

echo "Updating Arch"
pacman -Syu --noconfirm

echo "Trying to install Haskell"
pacman -S --needed --noconfirm ghc cabal-install

#echo "Trying to start MySQL"
#systemctl enable mysqld.service
#systemctl start mysqld.service
#sleep 2	# Give mySQL time to start up

#echo "Trying to create MySQL database"
#cat /vagrant/create_db.sql | mysql -u root
