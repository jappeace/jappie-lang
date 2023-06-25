[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Jappiejappie](https://img.shields.io/badge/twitch.tv-jappiejappie-purple?logo=twitch)](https://www.twitch.tv/jappiejappie)
[![Jappiejappie](https://img.shields.io/badge/youtube-jappieklooster-red?logo=youtube)](https://www.youtube.com/channel/UCQxmXSQEYyCeBC6urMWRPVw)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/haskell-template-project/Test)](https://github.com/jappeace/haskell-template-project/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)
[![Hackage version](https://img.shields.io/hackage/v/template.svg?label=Hackage)](https://hackage.haskell.org/package/template) 

> xxxx

A basic language build around the lambda calcules.

# TODO
+ [x] Make a golden test of output
+ [x] Get scoping right, currently names are just overwritten even if they're rebound
+ [ ] Add code gen
+ [ ] Add simple type system
+ [ ] Add source mapping

# What are newlines

I'm taling about the newlines outside of parans.

So an issue I'm running into is figuring out what newlines mean in my langauge.
Currently I'm just returning a list of expressions, but that feels wrong.
Mainly because an AST from the parser should contain an entire file,
at least in my opinion.

So what does a new line do, I've 2 ideas:

1. It's a space that introduce room for new bindings, at least new lines not enclosed in parenthesis.

For example

```
xxx = ([x] whateverexresion... )
```

2. It'll be just another App.

In this case if I've:
```

; identity
([x] x)

; app
(([x] x) ([y] y))

```
it'll be the same as:

```
(([x] x) (([x] x) ([y] y)))
```

I think I prefer the second method as it's easier for me to implement.

