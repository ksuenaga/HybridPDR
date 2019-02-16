HybridPDR
===

## Overview

Hybrid system verification using PDR/IC3.

This system is web application.
Access `localhost:5000` on your web browser to use this system.

## Requirements

HybridPDR runs on Docker.
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

Access `localhost:5000` on a web browser to use HybridPDR.

## Usage

HybridPDR first window is file explorer and it refers `data`.
You can create, rename, and delete files and directories here.

Click file to open project.

In project page, you should input Definition, Initial Condition, Safety Condition, and Tactics.

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
