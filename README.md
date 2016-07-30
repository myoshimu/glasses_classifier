# imgClassifier

Rで画像判別

 1) 作業用フォルダを準備して、load.Rとtrain.Rを保存
 
 2) 1）のフォルダの下に以下4つのディレクトリを作成し画像（JPEG形式）を保存
 
 pika：正解教師データ
 
 no_pika：非正解教師データ
 
 sample_pika：正解テストデータ
 
 sample_no_pika：非正解ストデータ
 
 3) Rでload.Rを実行し、画像をvectorに変換したtrain.csvとtest.csv作成
 
 4) train.Rで判別実施
