### what


### why


### how 

- i would create an alias in your .bashrc like this:

    - if you want to use the image straight from the docker hub registry:
```
        - alias vv='docker run --rm -it -v `pwd`:/mnt justin2004/slimv_box'
```
    - or if you build the container yourself
```
        - alias vv='docker run --rm -it -v `pwd`:/mnt slimv_box'
```
        - replacing "slimv_box" with the tag name you used


### NOTES

- only files in the /mnt directory are saved when you leave vim!



