# Command

a command example for problem 19:

```
% stack exec diagonal 10 100 probs/19.png > solution/layer.10.100_19.isl
```

here, 1st parameter 10 is a box size for painting (1 to 400, must be divisor of 400)
and 2nd parameter 100 is a limit of acceptable distance (0 to 3*255^2).

you can check the result using eval comand:

```
% stack exec eval -- solution/diagonal.10.100_19.isl -t probs/19.png -o solution/png/diagonal.10.100_19.png
```
