# 気候変化シナリオデータを使ってみる
本リポジトリでは、[表題と同じタイトルのnote記事](https://note.com/calmtree/n/n486e634c99a5)を執筆する際に使用したコードを公開しています。

### ファイルの説明
- AMD_Climate_Scenario.ipynb
    - メッシュ農業気象データから気候変化シナリオデータを地点切り出しするipynbファイルです
    - 実行するためにはメッシュ農業気象データのユーザー登録と、専用Pythonモジュールが必要です
    - 詳しくは[公式サイト](https://amu.rd.naro.go.jp/wiki_open/doku.php?id=start)をご参照下さい
- AMD_Climate_Scenario.R
    - 上記ipynbファイルで取得した気候変化シナリオデータを分析・可視化するRスクリプトです
