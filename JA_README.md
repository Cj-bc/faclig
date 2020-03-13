[English README](README.md)

---

# faclig

![thumbnail](docs/img/289767.gif)

`faclig`は、顔のトラッキングに合わせてASCII Art(AA)モデルを動かします。
AA版の[live2D](https://www.live2d.com/)を目指しています。
これは[Cj-bc/FDS-protos](https://github.com/Cj-bc/FDS-protos)のフロントエンドとして設計されています。
現在開発途中です。


# 機能

- AAモデルのフォーマット
- トラッキングデータをAAに適用して動かす

# 使い方

まず、[Cj-bc/Face-Data-Server](https://github.com/Cj-bc/Face-data-server)
(もしくは、他の[Cj-bc/FDS-protos](https://github.com/Cj-bc/FDS-protos)のバックエンド実装)
を走らせる必要があります。
そのあと、`faclig`コマンドをfaclig model([faclig_format.md -- ja](docs/ja/faclig_format.md))のパスを引数に与えて走らせます。


```shell
# backendを起動
# これは上記リンクを参照
$ face-Data-Server &
# facligにモデルのパスを与えて実行
$ faclig resources/face.faclig.yaml
```

## 実行方法

```shell
# facligをインストールせずに実行
$ stack run -- <PATH>
# facligをインストール後に実行
$ faclig <PATH>
```

