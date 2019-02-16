HybridPDR
===

Hybrid system verification using PDR/IC3.

Docs: https://docs.google.com/document/d/1G0OhwpYI8uOxTetlJraxnlrS_JhVBSNzRiQR2elZQRg/edit#

## Description

## Requirement

HybridPDR is running on Docker.
You should install Docker.

## Quick Start

Pull docker image from Docker Hub in `/HybridPDR`.
```
docker pull ksuenaga/hybridpdr
```

To start HybridPDR, run docker container.
```
docker run --rm -it -v $PWD/data:/home/opam/data -p 5000:5000 hybridpdr
```

Access `localhost:5000` on web browser to use HybridPDR.

## Usage

HybridPDR first window is file explorer and it refers `/data`.
You can create, rename, and delete files and directories here.

Click file to open project.

In project page, you should input Definition, Initial Condition, Safety Condition, and Tactics.
Click Validate button to run HybridPDR.
Result will be shown on Result window.
You can export result to text file using Export button.

If HybridPDR returns error, it shows return code in result window.

Return code:

| code | status |
:---:|:---
| 0 | normal termination
| 1 | known abnormal termination
| 2 | unknown abnormal termination

Run HybridPDR in debug mode you should check debug mode checkbox.

Also you can save your Definition with Save button bellow Definition editor.

## Install Mathematica

If you have Mathematica license, you can use Mathematica in HybridPDR.

To install Mathematica in HybridPDR, get installer from https://www.wolfram.com/index.en.html?source=footer.
Put your installer in `/HybridPDR`.

Execute
```
sh setup.sh
```
and you can install Mathematica automatically.

activate??
