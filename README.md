# Bkroonga2

Groonga plugin Ver.2 for Becky! Ver.2  
https://github.com/shinonon301/Bkroonga2  
https://github.com/shinonon301/Bkroonga2/issues  


----------------------------------------------------------------------
## 概要

Bkroonga2 とは、Becky! Ver.2（POP3 アカウントのみ）で全文検索エンジン
Groonga を使ったメール高速全文検索ができるようにするプラグインです。  
一応 Apache-2.0 License としていますが、いわゆるフリーソフトとして公開します。
読み方は、「びーけーるんが２」でも「べきるんがつー」でも、好きに読んでください。


----------------------------------------------------------------------
## 特徴

* 自動的に全てのメールのインデクシングを行い、自由に高速全文検索が行えます
  (デフォルトではゴミ箱と一部の送信箱フォルダは除外)
* xdoc2txt を使用すれば添付ファイルの中身も全文検索できます（ただし、圧縮
  ファイルは ZIP中のファイル名のみで、ZIP以外、及び圧縮された中身のインデク
  シングはしません）
* 検索期間の絞り込みが簡単に視覚的に行えます
* 検索したい語句が全文引用された文に含まれていたとき、多くの場合はノイズとなり
  ますが、それを効率的に検索結果から除外できます。


----------------------------------------------------------------------
## インストール

* bkroonga2_???.zipファイルを解凍して、出てきたファイル一式(dllと付属テキス
  ト)を、メールボックスフォルダにある PlugInsフォルダ 
  (eg. C:\Becky!\PlugIns\ ) にコピーしてください。
* Groonga (64bitパッケージ推奨)をダウンロードしてインストールしてください。  
    http://groonga.org/ja/docs/install/windows.html  
  基本的には groonga-latest-x64-vs2019-with-vcruntime.zip をダウンロード
  して、C:\groonga の中にでも解凍すれば OK です。私は Grronga 12.0.3 
  (2022年5月現在)を使用しています。
* xdoc2txt をダウンロードしてください。(オプション)  
    http://ebstudio.info/home/xdoc2txt.html  
  zipファイルを解凍して出てきた xdoc2txt.exe を、bkroonga2.dll を置いた
  ところに入れてください。私が使っているのは Ver.2.20 64bit版(2022年5月現在)
  です。xdoc2txt を入れないと HTML,ZIP 以外の添付ファイルをインデクシング
  しなくなります。
    * バージョンアップするときは、Becky! を終了してから bkroonga2.dll を
      新バージョンで上書きするだけです。Groonga や Xdoc2txt をバージョアップ
      するときも、Becky! を終了してから上書きインストールしてください。
    * アンインストールする際は、bkroonga2.dll と xdoc2txt.exe を削除し、
      bkgroonga2db フォルダも削除し、Groonga をアンインストールしてください。


----------------------------------------------------------------------
## 初期設定方法

* 基本的には、特に何もしなくても使えるようにしています。
* 自動的に Groonga.exe を見つけられないときは、場所を指定してください。
* あとはできるだけ Becky! を使っていないタイミングを見計らって、バックグラ
  ウンドで自動的にインデクシングを行います。そのため、なるべく Becky! は
  終了しないで、使用していないときは最小化して放置するようにしてください。
* 検索ウィンドウを開くには、ツール→Bkroonga2検索 を実行してください。
  または、Ctrl+E を押しても同様です。(詳細は後述)
* 設定画面を開くには、ツール→プラグインの設定→Bkroonga2 を実行してください
  (詳細は後述)


----------------------------------------------------------------------
## 検索ウィンドウ

* ツール→「Bkroonga2検索」、または Ctrl+E で Bkroonga2 検索ウィンドウ
  が開きます。
* 検索文字列に検索したい語句を入力してください。スペースで区切って複数の
  語句を指定できます。
* 「検索開始」を押すか少し待つとすぐに検索が開始され、"HIT: n(m)" の
  ところにヒット数(n)と、下のリストビューに表示されているメール数(m)
  が表示されます。
* リストビューの上には「月絞り込み」リストが、左にはツリービューが、
  下にはスニペット(対象テキスト中から検索キーワード周辺のテキストを
  抜粋したもの)が表示されます。

#### 検索文字列

* 「検索対象」で、検索文字列の語句を検索する対象を指定できます。
    * 現在のメールボックスにチェックを入れると、Becky!で現在閲覧中の
      メールボックスのみを検索対象とします。
    * 本文はメールの最初の text パートの本文で、引用はそのパート内の
      引用文字(>等)が行先頭にある文となります。
    * ヘッダは、subject, from, to, cc, bcc が対象です。
    * 添付ファイルは、xdoc2txt を導入しているときに有用です。
      (未導入のときは添付された html のみ。ただし HTMLメールの HTML
      パート(というよりmultipart/alternativeの2番目以降のtextパート)
      はインデクシングされませんので検索対象となりません)
    * 引用のチェックを外すことで、ビジネスメールでよくある全文引用内
      の語句に引っかかったメールを、効率的に除外できます(textパート内
      で正しく引用されていれば)
* 「絞り込み」で Becky! で認識されたフラグ(Becky! のリストビューで
  のメールアイコンの違い)で絞り込みが行なえます。
* 「除外」で検索対象から、受信箱以下・送信箱以下・ゴミ箱以下を明示的
  に除外できます。
* 「差出人絞り込み」で、from での絞り込みができます。
  絞り込みを行うと、その結果で「ツリービュー」「月絞り込み」も更新
  されます。右クリックをすると、差出人絞り込みを解除できます。
* 「検索開始」ボタンを明示的に押すことにより、「ツリービュー」
  「月絞り込み」「差出人」の全ての絞り込みを解除することができます。


#### ツリービュー

* 検索したメールのフォルダごとの集計結果一覧が表示されます。
* 「フォルダ名 - n (m)」の意味は、n=そのフォルダ内の検索メール数、
  m=そのフォルダ下の検索メール数です
* フォルダをクリックすると、そのフォルダで絞り込みが行なえます。
  その際、右下のアイコンで「選択したフォルダのみ」「選択したフォルダ
  以下を含む」を変更できます。
* 絞り込みを行うと、その結果で「月絞り込み」「差出人絞り込み」も
  更新されます。
* 右クリックをすると絞り込みを解除できます。

#### 月絞り込み

* 検索したメールの date ヘッダから、月ごとの集計結果一覧が表示
  されます。その際、右下のアイコンで検索数0件の月を表示するか
  どうかを変更できます。
* 項目をクリックすると、選択した月のメールで絞り込みが行なえます。
* 絞り込みを行うと、その結果で「ツリービュー」「差出人絞り込み」も
  更新されます。
* 右クリックをすると絞り込みを解除できます。

#### リストビュー

* 検索したメール一覧が表示されます（最大1000件）
* カラムをクリックすると、それぞれの項目でソートが行なえます。
    * 一番左のカラムでソートを行うと、Becky!のスレッド表示と同様
      のソートが行なえます。さらにクリックするとスレッド全開・
      全閉が変化します。ここでスレッド全閉をすることで、ビジネス
      メールでよくある全文引用内の語句に引っかかったメールを、
      効率的に除外できます(きちんとスレッドがつながっていれば)
* クリックしてメールを選択すると、その下にスニペット(対象テキスト
  中から検索キーワード周辺のテキストを抜粋したもの)が表示されます。


----------------------------------------------------------------------
## 設定画面

* 「Groongaの場所」で、groonga.exe の場所を指定できます。
* 「除外するフォルダ」で、インデクシング対象から除外するフォルダを
  指定できます。デフォルトでゴミ箱以下は除外していますので、ゴミ箱
  をインデクシングしたい方はチェックを外してください。
    * 任意フォルダ名に入力すると、その文字列がフォルダ名に含まれる
      フォルダをインデクシングから除外します。複数指定するときは、
      / (スラッシュ記号)区切りで指定してください。
    * 送信箱以下のチェックを外しても、送信箱(次のタイミングで送信が
      行われる箱)とリマインダは除外されます。
* 「除外するメールボックス」でインデクシングから除外するメールボックス
  を指定できます。
* 「バックアップ作業フォルダを開く」で、Groonga DB のバックアップ・
  復元を行うことができます。(詳細は後述)
* [デバッグ用途] リストビューに動作ログ(INFO以上)が表示されます
* [デバッグ用途] 「query」で Groonga にコマンドが投げられます。
  また、「parse」で選択中メールの parse 結果が表示されます。


----------------------------------------------------------------------
## DBバックアップ

* 設定画面で「バックアップ作業フォルダを開く」を押すと、bkroonga2.dll
  のあるフォルダに bkroonga2bak というフォルダが作られ、その中に
  「DBバックアップ作成.bat」と
  「DBバックアップリストア(lz4ファイルをドロップ).bat」
  が入ったフォルダが開きます。
* Becky! を終了し「DBバックアップ作成.bat」を実行すると、その
  フォルダに dbbakYYYYMMDD.lz4 というファイルが作成され、それが
  DB バックアップファイルになります。PC 速度や Becky! のメール
  規模にもよりますが、数分～10数分程度かかると思います。
* Groonga に同梱されている lz4.exe を使用して圧縮・展開しています。
* リストアするときは、同じく Becky! を終了し、*.lz4ファイルを
  「DBバックアップリストア(lz4ファイルをドロップ).bat」にドロップ
  してください。こちらは、実行すると DB が初期化され、バックアップの
  数倍時間がかかりますので、覚悟して実行してください。
  （それでも全メールインデックスし直すことに比べれば遥かに早いです）


----------------------------------------------------------------------
## その他（おまけ）

* インデックスするテキスト長は、textパートは 128kB、添付ファイルは
  256kB までとなります。
* このプラグインはバックグラウンドで常にインデックス作業を行うので、
  モバイル環境で使うことは避けた方が無難でしょう。
* bkroonga2.dll と同じフォルダにできる bkroonga2.ini を開き、
  [Bkroonga2] セクションに以下の項目を手で追記すると、動作ログが
  出力されるようになります。
  ```
  bkroonga2.ini
    [Bkroonga2]
        :
    LogFile=bkroonga2.log
    LogLevel=1       
    LogSize=1000000
    LogHistory=7
  ```
  LogLevelは、0=verbose(指定しない方が無難), 1=debug, 2=info,
  3=warn, 4=error, 5=fatal となります。
  LogSize は書かないとデフォルト約1MBとなります。
  LogHistory は書かないとデフォルト7世代となります。
* 同じく bkroonga2.ini の [Bkroonga2] セクションに
  ```
    GroongaPort=10083
  ```
  を追記すると、Groongaを実行するときの HTTP ポート番号を指定
  できます。デフォルトは 10083 です。
* 同じく bkroonga2.ini の [Bkroonga2] セクションの DBDir で、
  Groonga DB の場所を変更できます。デフォルトは、bkroonga2.dll
  のあるフォルダに bkroonga2db というフォルダが作られます。
* bkroonga2.dll と同じフォルダに以下の名前の空ファイルを置くと、
  Groongaコマンドでエラーが起きたときの中身が出力されます。
  ```
    errorload.txt    loadコマンドでエラーが起きたとき
    errorselect.txt  selectコマンドでエラーが起きたとき
    errorjson.txt    その他のコマンドでエラーが起きたとき
  ```
* 設定画面の query で指定できるコマンドは、スペース区切りで
  ```
    コマンド名 パラメータ1 パラメータ2 …
  ```
  と記述します。パラメータは param=value と記述してください。  
  例）select table=MailIds limit=0  
      ⇒ "body"→"n_hits" からインデクシングされたメール数が
        得られます
* bkroonga2.dll と同じフォルダに snippethtml.txt というファイルを
  置くことで、スニペットの HTML ヘッダを変更することが可能です。
  デフォルトは以下の通りです。
  最後に自動的に ``` </body></html> ``` が追加されます。
  文字コードはUTF-8にしてください。
  ```
    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    <html lang="ja"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <style type="text/css">body{font-size:12px;margin:0px;padding:0px;}
    span.k1{background-color:rgb(255,255,128);color:black;font-weight:bold;}
    span.k2{background-color:rgb(131,139,253);color:white;font-weight:bold;}
    span.k3{background-color:rgb(220,146,235);color:black;font-weight:bold;}
    span.k4{background-color:rgb(90,133,90);color:white;font-weight:bold;}
    span.k5{background-color:rgb(136,110,136);color:white;font-weight:bold;}
    span.dot{color:gray;}</style></head><body>
  ```


----------------------------------------------------------------------
## ソースコードからbuild

* このソフトは embarcadero RAD Studio 10.4 Delphi Community Edition
  で開発しています。  
  https://www.embarcadero.com/jp/products/delphi/starter
* 特別なコンポーネントは使用していないので、embacardero のサイトで登録
  して無料版の開発環境をダウンロード・インストールし、bkroonga2.dproj を
  開いてビルドすれば、bkroonga2.dll が作成されるはずです。
* Becky! は 32bit版アプリなので、64bit版の DLL を作成しても意味ありません。
* どなたか、IMAPに対応してください！


----------------------------------------------------------------------
## 履歴

* Ver.2.0.0(2022/05/05)：初版
* Ver.2.0.1(2022/05/07)
    - リストビューで選択したメールに対するツリービューと月絞り込みを
      ハイライトするようにした
    - 検索数0件の月を表示したときの月絞り込みの件数が誤っていた


----------------------------------------------------------------------
## 著作権

* このプラグインは使用者の自己責任において使用してください。作者はこの
  プラグインによって生じるいかなる損害も補償しません。
* このプラグインの著作権は私 しののん が保有しています。
* Groonga の情報は http://groonga.org/ja/ を参照してください。
* xdoc2txt の情報は http://ebstudio.info/home/xdoc2txt.html を参照して
  ください。
* オリジナルアーカイブを改変しない限り、転載・再配布は自由です。特に私ま
  で確認の連絡をする必要はありません。

しののん  
https://github.com/shinonon301/Bkroonga2  
https://github.com/shinonon301/Bkroonga2/issues  
