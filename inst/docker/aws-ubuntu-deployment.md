# Overview

These are notes that outline how you can setup an Amacon EC2 instance to run
shinyproxy.io to deploy containerized shiny apps.

This very helpful blog post describes how to setup shinyrpoxy on Ubuntu 16.04
LTS, and even described how to include non-shiny applications:

https://lukesingham.com/shiny-containers-with-shinyproxy/

# Machine Setup

We start with an Ubuntu Server 16.04 LTS image (ami-79873901) on a t2.large
instance.

Some configs and pricing:

    instance      vCPUs     GB RAM   Price / hr
    t2.large      2         8        $0.0928 per Hour (2.23 /day)
    t2.xlarge     4         16       $0.1856 per Hour (4.45 / day)
    t2.2xlarge    8         32       $0.3712 per Hour (8.91 / day)

Step 4: And added 16GB worth of storage (/dev/xvda)

Step 6: Configure Security groups

We want to open a number of ports:

    Type              Protocol   Port Range     Source
    HTTP              TCP        80             0.0.0.0/0, ::/0
    SSH               TCP        22             0.0.0.0/0, ::/0
    Custom TCP Rule   TCP        8080           0.0.0.0/0, ::/0
    Custom TCP Rule   TCP        2375           0.0.0.0/0, ::/0
    Custom TCP Rule   TCP        443            0.0.0.0/0, ::/0

You may not need to open port 2375, but you can test that. Port 2375 is what
shinyproxy will use to talk to the docker daemon so it can launch apps.

Now launch your image!

# AMI machine configuration

Now it's time to `ssh` into the instance and start configuring the vanilla
ubuntu LTS image.

```bash
$ ssh ubuntu@XXX.XX.X.XXX
```

I like to use screen when working at the terminal, so I'll copy my config to the
server to make my life easier:

```bash
$ curl https://raw.githubusercontent.com/lianos/dotfiles/master/screenrc > ~/.screenrc
$ screen -RD
```

And let's update our package database so that we install the latest supported
versions of the software we need:

```bash
$ sudo apt-get update
```

## Java

Install Java8

```bash
sudo apt-get install openjdk-8-jre-headless
```

Confirm the update went as expected. You should have java version
`1.8.something`.

```bash
$ java -version
openjdk version "1.8.0_151"
OpenJDK Runtime Environment (build 1.8.0_151-8u151-b12-0ubuntu0.16.04.2-b12)
OpenJDK 64-Bit Server VM (build 25.151-b12, mixed mode)
```

## Docker

here are some tips to install docker on ubuntu:

https://docs.docker.com/v17.09/engine/installation/linux/docker-ce/ubuntu/#os-requirements


Uninstall old versions:

```bash 
sudo apt-get remove docker docker-engine docker.io
```

I guess docker serves installer packages from their own repositories, so we
need to configure our server to fetch from there:

```bash
sudo apt-get install \
    apt-transport-https \
    ca-certificates \
    curl \
    software-properties-common

## Add docker's official GPG key:
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

## Verify you have correct key by searching for last 8 chars of the fingerprint:
# sudo apt-key fingerprint 0EBFCD88
# pub   4096R/0EBFCD88 2017-02-22
#       Key fingerprint = 9DC8 5822 9FC7 DD38 854A  E2D8 8D81 803C 0EBF CD88
# uid                  Docker Release (CE deb) <docker@docker.com>
# sub   4096R/F273FCD8 2017-02-22

## Now add docker's stable repository:
$ sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"

## Now update package index to get docker's packages
$ sudo apt-get update

## And finally install docker-ce
$ sudo apt-get install docker-ce

## Verify docker is running:
$ sudo docker info
## should produce output
```

### Configure Docker

Create a `docker` group and add yourself to it so you can manage docker as
a non-root user:

```bash
$ sudo groupadd docker
$ sudo usermod -aG docker $USER
```

You now have to log out/in to pickup new docker group permissions. Verify
everythiing worked as expected by running a docker command w/o `sudo`:

```bash
$ docker info
```

Shinyproxy will try to talk to the docker daemon on `tcp://127.0.0.1:2375`. If
it can't, then launching applications won't work. To configure docker to listen
to that port you need to edit `/lib/systemd/system/docker.service` and set
`ExecStart` appropriately:

```bash
ExecStart=/usr/bin/dockerd -H fd:// -D -H tcp://127.0.0.1:2375
```

Restart docker after editing the config:

```bash
$ sudo systemctl daemon-reload
$ sudo systemctl restart docker
```

# Shinyproxy

Download latest shinyproxy

```bash
mkdir -p sw/shinyproxy
cd sw/shinyproxy
curl -O https://www.shinyproxy.io/downloads/shinyproxy-1.0.2.jar
```

Download `application.yml` as default template

```bash
curl -O https://raw.githubusercontent.com/lianos/multiGSEA.shiny/develop/inst/docker/application.yml
```

Launch shinyproxy

```bash
$ java -jar shinyproxy-1.0.2.jar
```
## Get application docker images 

```bash
docker pull lianos/multigsea-shinyproxy:latest
```

Navigate to your server's IP:8080 and witness the magic!
