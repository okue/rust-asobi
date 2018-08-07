# otp-example

http://erlang.shibu.jp/design_principles/applications.html

hello_app
 |- hello_sup
      |- hello


```
$ erl -make
$ erl -pa ebin/
```


## Use process_child:search/1

```
> process_child:search([hello_sup]).
```
