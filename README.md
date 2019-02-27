HybridPDR
===

## Overview

Hybrid system verification using PDR/IC3.
This system is an Web application, working on localhost.

## Requirements

HybridPDR works on Docker.
You should install Docker.

#### Optional Requirements

You can also use Mathematica if you have it's license.

## Quick Start

Pull docker image from Docker Hub.
```
docker pull ksuenaga/hybridpdr
```

To start HybridPDR, run docker container.
```
cd HybridPDR
docker run --rm -it -v $PWD/data:/home/opam/data -p 5000:5000 hybridpdr
```

Access `localhost:5000` on a Web browser to use HybridPDR.

## Usage

When you open HybridPDR, it'll show a file explorer refering to `data`.
You can create, rename, and delete files and directories here.

Click file to open a project.

In a project page, you should input Definition, Initial Condition, Safety Condition, and Tactics.

Click Validate button to run HybridPDR.
Result will be shown on Result window.
You can export result to text file using Export button.

If HybridPDR returns error, it will show return code in Result window.

Return code:

| code | status |
:---:|:---
| 0 | normal termination
| 1 | known abnormal termination
| 2 | unknown abnormal termination

If you want to run HybridPDR in debug mode, you should check debug mode checkbox.
(To. 末永先生 デバッグモードによって出力がどう変わるかの記入をお願いします)

Also you can save your Definition with Save button below Definition editor.

## Build HybridPDR

If you need to build docker image of this system in your environment, follow this procedure.

Clone git repository.
```
git clone -b dgrid https://github.com/ksuenaga/HybridPDR.git
```
Then build docker images in two stages.
```
cd HybridPDR
docker build -t hybridpdr:core -f Dockerfile.core . # to build an executable
docker build -t hybridpdr . # to build an Web application
```
Run docker container to start HybridPDR.
```
docker run --rm -it -v $PWD/data:/home/opam/data -p 5000:5000 hybridpdr
```

## Install Mathematica

If you have Mathematica license, you can use Mathematica in HybridPDR.

To install Mathematica in HybridPDR, get installer from
-https://www.wolfram.com/index.en.html?source=footer-.
(To. 末永先生 MathematicaのインストーラのDLリンクに変更していただきたいです)
Put your installer in `HybridPDR`.

Execute
```
sh setup.sh
```
and you can install Mathematica automatically.

activate??
