# Faclig model仕様書

このドキュメントはfaclig用のASCII Artモデルの仕様を制定します。

# バージョン

現行のバージョンは 0.1.0

[semantics versioning 2.0.0](https://semver.org/spec/v2.0.0.html)を使用しています。

# 概要

モデルは二種類のファイルから構成されます:
  - `faclig format`ファイル: パーツからどのようにモデルを構成するかの情報とメタデータを格納しています。
  - `part`ファイル: パーツのファイルで、実際にはただの[Shgif](https://github.com/Cj-bc/brick-shgif)ファイルです。


## `faclig format` ファイル

### 概要

このファイルは以下のものを定義します:
  - モデルの名前
  - 製作者の名前
  - バージョン番号
  - `Part`情報

#### Yamlフォーマットと例

Yamlフォーマットは以下のとおりです:
```yaml
name: <モデルの名前, Text>
author: <製作者の名前, Text>
version: <バージョン番号, Text>
parts: # 次の章で説明します
  ...
```

例:
```yaml
name: haskell girl
author: Cj.BC_SD a.k.a Cj-bc
version: 0.1.0
parts:
  ...
```

### Part情報

`Part情報`は以下の情報を指します:
  - そのpartはどのファイルにあるのか？
  - そのパーツに必要なオフセット

#### オフセットとは

_オフセット_は、二つのIntの配列で構成されます。
デフォルト時、画面の左上からどのくらいずらした位置に表示するかを指定します。
最初の値がXのオフセット、二つ目がYのオフセットです。
配列の要素数が2より大きかった場合、単純に無視されます。
__画面左上__が`(0, 0)`です。

#### Yamlフォーマットと例

Yaml formatは:
```yaml
<part_name>:
  path: <Shgifへのパス, Text>
  offset: [] <partのオフセット, [Int, Int]>
```

例として, 'leftEye' part:
```yaml
leftEye:
  path: "leftEye.yaml"
  offset: [0, 0]
```

#### 定義ずみのPart

現在、7つのpartが定義されており、全て__必須__です。

| part name | description |
| :-:|:-:|
| contour | 顔の輪郭、土台 |
| leftEye | 左目  |
| rightEye | 右目 |
| nose     | 鼻 |
| mouth    | 口 |
| hair     | 前髪 |
| backHair | 後髪、contourよりも後ろにレンダリングされます。|


## `part`ファイル

Partファイルは、各Part用のShgifファイルです。
フォーマットについては[Cj-bc/brick-shgif -- docs/shgif-format.md](https://github.com/Cj-bc/brick-shgif/blob/master/docs/shgif-format.md)を参照してください。
facligはShgif 1.0.0を使用しています(2020/03/12現在)。
