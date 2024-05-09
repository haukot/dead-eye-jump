# Dead Eye Jump

Emacs plugin like avy or ace-jump or EasyMotion, but with different mechanic - you select place in full frame based of quadrant of the screen.
So it'll move mechanic action without additional thinking - one place always will be the same keys.
And in 3 keys you could move(almost exactly) where you want on the screen.

# Usage

Add to config
```eslisp
(global-set-key (kbd "C-c j") 'highlight-refined-pixel-parts)
```

`C+c j <char> <char> <char>`

Examples (on Workman keyboard layout, that's why its not Querty):

`C+c j`
![1](./assets/1.png)

Press `a`
![2](./assets/2.png)

Press `h`
![3](./assets/3.png)

Press `u`
![4](./assets/4.png)

# TODO

* Work strangely on right side
