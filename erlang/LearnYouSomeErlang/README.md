# [Learn you some Erlang for great good!](https://www.ymotongpoo.com/works/lyse-ja/)

主に, 17章以降をやっていく


## Chapter 17 クライアントとサーバ

- init
- handle_call
- handle_cast
  MassageとStateを引数に取り, 非同期呼び出しを処理. replyはしない.
- handle_info
  !演算子で直接送られてきたメッセージや, init/1のtimeoutのような特別なメッセージ, モニターの通知や'EXIT'シグナルのためだけに存在する
- terminate
  handle_xxxが{stop,Reason,NewState}または{stop,Reason,Reply,NewState}という形式のタプルを返した時に呼ばれる.
  タプル内のReasonとStateが引数となる.
  normal, shutdownまたは{shutdown, Term}以外でterminateが呼ばれると, OTPは失敗と見なして色々とログを残す.
- code_change
