#!/bin/bash

set -ex

#source /vagrant/.ssh_env
sudo echo -e "nameserver 8.8.8.8\nnameserver 1.1.1.1" | sudo tee /etc/resolv.conf

sudo apt update
sudo apt upgrade -y
sudo apt install -y \
  ocaml \
  opam \
  build-essential \
  m4 \
  make \
  autoconf \
  automake \
  libtool \
  pkg-config \
  libffi-dev \
  libncurses-dev \
  libx11-dev \
  libssl-dev \
  libgmp-dev \
  libsqlite3-dev \
  zlib1g-dev \
  libcairo2-dev


opam init --auto-setup
eval $(opam env)


opam install -y ocaml-lsp-server odoc ocamlformat utop dune
opam install -y ocsipersist-sqlite eliom ocsigen-ppx-rpc
opam install -y ocsigen-toolkit

#echo $SSH_KEY >> /home/vagrant/.ssh/authorized_keys
