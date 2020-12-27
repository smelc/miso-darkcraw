File `card-bg.xcf` is used to generate `app/assets/card-bg-{human,undead}.png` (according
to the foreground layer being selected). These files are put locally by @smelc
in `app/assets`, then distributed thanks to the `git push` hook
`hooks/pre-push`, and downloaded by other devs using `./scripts/dl-large-assets.sh`.
