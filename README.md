# HybridPDR
Hybrid system verification using PDR/IC3

Docs: https://docs.google.com/document/d/1G0OhwpYI8uOxTetlJraxnlrS_JhVBSNzRiQR2elZQRg/edit#

TODO:

アルゴリズムの formalization
ハイブリッドオートマトンの定義
https://www.cs.colorado.edu/~xich8622/papers/rtss12.pdf の 3 節 A に沿ってやる．
GPDR の定義
性質の証明
GPDR の論文にそってやる
実装と実験
方針: 最初からすごいことをしようとしない
Conflict で非自明な interpolant を求めようとしない
すでに持っている述語をつかって interpolant を作る
Induction はすでに持っている述語を伝播させるだけ
Flow* に比べてよくなれるか
一つでも bounded でない invariant が求まればよい．
時間の bound がない invariant が求まるモデル検査器は他にあるか．
関連研究の調査
