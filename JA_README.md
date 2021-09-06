[English README](README.md)

---

# faclig

![thumbnail](docs/img/289767.gif)

`faclig`は、 [VMC Protocol](https://protocol.vmc.info/)に合わせてASCII Art(AA)モデルを動かします。
AA版の[live2D](https://www.live2d.com/)を目指しています。
現在開発途中です。


# 機能

- AAモデルのフォーマット
- トラッキングデータをAAに適用して動かす

# 使い方

まず、[Waidayo](https://booth.pm/ja/items/1779185) や [バーチャルモーションキャプチャー](https://vmc.info/)
などのVMCPでのデータ送信に対応したアプリケーションを起動します。
([vmc.infoページ](https://protocol.vmc.info/Reference)にてアプリケーションの例があります。)

そのあと、`faclig`コマンドをfaclig model([faclig_format.md -- ja](docs/ja/faclig_format.md))のパスを引数に与えて走らせます。


## 実行方法

```shell
# facligをインストールせずに実行
$ stack run -- <PATH>
# facligをインストール後に実行
$ faclig <PATH>
```
