Docker setup for developing [Sytes](https://github.com/mishoo/sytes)
---

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [What is Sytes?](#what-is-sytes)
- [(Quickest way) Up and running with Docker Compose:](#quickest-way-up-and-running-with-docker-compose)
- [(Slower way) Up and running with Docker:](#slower-way-up-and-running-with-docker)
- [For emacs user](#for-emacs-user)
- [TODO](#todo)

<!-- markdown-toc end -->


## What is Sytes? ##

Quoting from Lisperator's website (Sytes' creator):

"Sytes is a small Common Lisp library for building simple websites. I wouldn't call
it a framework—it doesn't deal with any of those things you'd expect frameworks to do,
like handling authentication, database abstraction etc. Sytes just implements a
template engine with a Lispy syntax and a Hunchentoot dispatcher that maps URLs to
templates. Sytes is the PHP of Common Lisp (I hope this doesn't sound like an insult).
If you're a Lisper, you might enjoy it."

Here is an example:

```
;; a semicolon in the first column starts a comment
;; and it's discarded from the output
<h1>{page.title}</h1>

;; show links
<ul class="links">
  {(foreach link '(("http://foo.com" . "The Foo")
                   ("http://bar.com" . "The Bar")
                   ("http://baz.com" . "The Baz"))
     (let ((url (car link))
           (name (cdr link)))
       {<li><a href="{\url}">{\name}</a></li>}))}
</ul>
```

The full article/tutorial can be read [here](http://lisperator.net/sytes/). If you find
Sytes as interesting as I do, I hope you would also find this Docker setup a little handy.

This is pretty much a work in progress. I guess the only missing piece is getting
`/assets` to be served from the same docker image.

Built on top of daewok's [lisp-devel-docker](https://github.com/daewok/lisp-devel-docker)
Dockerfile for `quicklisp`. Comes bundled with:

- sbcl
- quicklisp
- sytes
- shared `./syte.blog` directory with docker's `/usr/local/share/common-lisp/source/sytes.blog`


## (Quickest way) Up and running with Docker Compose: ##

NOTE: If you're using Emacs, you may find the "For Emacs user" section useful :)

```
git clone https://github.com/nathandao/docker-sytes
docker-compose build
docker-compose up
```

Your blog should be accessible through http://localhost:7379

The blog's code is located in `./source/syte.blog`. Lisperator -
the creator of Sytes - wrote an amazing tutorial on how to use Sytes
in his blog, [lisperator.net](http://lisperator.net/sytes/tutorial/hello-world).
By using this docker setup, you can skip the installation part.

To connect to the current running docker, make sure you `docker-compose up`
session is still running. Open a new terminal session:

```
# get to the project's directory if you have not
cd docker-sytes

docker-compose run syte_web sbcl
```

## (Slower way) Up and running with Docker: ##

Build docker image

```
# clone the repository
git clone https://github.com/nathandao/docker-sytes

# build an image (replace syte/blog with your preferred name)
docker build -t syte/blog .
```

Confirm your new image is listed in `docker images`

```
docker images

# Output:
# REPOSITORY          TAG      .........
# syte/blog           latest   .........
```

After being built, you can now connect to sbcl inside docker.

```
docker run --rm -it -p 7379:7379 -v $(pwd)/syte.blog:/usr/local/share/common-lisp/source/syte.blog syte/blog sbcl

# replace 'syte/blog' with the one defined during docker build
# -v mounts the ./syte.blog folder to /usr/local/share/common-lisp/source/syte.blog in the docker image
# -p 7379:7379 mapping port 7379 to localhost, since this is the default port used by Sytes
```

Now it's time to start your blog, exSyting time! Start sbcl inside docker if you have not done so.

```
docker run --rm -it -p 7379:7379 -v $(pwd)/syte.blog:/usr/local/share/common-lisp/source/syte.blog syte/blog sbcl

* (ql:quickload "sytes")
* (ql:quickload "syte.blog")
* (sytes:start-server)
```

The blog is now available in http://localhost:7379

# For emacs user #

If you're using emacs, `slime` is most likely your rhyme.

Make sure you have [slime](https://common-lisp.net/project/slime/) and
[slime-docker](https://github.com/daewok/slime-docker) installed.

Add this `slime-docker` configuration to your `.emacs` file:

```
;; Do some standard SLIME configuration.
(slime-setup '(slime-fancy slime-tramp))
;; Configure slime-docker to use a specific image with shared volumes
;; and correct ports exposed
(setq slime-docker-implementations `((sbcl ("sbcl")
                                           :image-name "syte/blog"
                                           :mounts ((("/path/to/sytes-docker/syte.blog"
                                                      . "/home/lisp/quicklisp/local-projects/syte.blog")))
                                           :ports ((:host-port 7379 :container-port 7379 :ip "127.0.0.1")))))
```

Start Emacs, run `M-x slime-docker`. Quickload sytes, syte.blog and start the server:

```
CL-USER> (ql:quickload "sytes")
CL-USER> (ql:quickload "syte.blog")
CL-USER> (sytes:start-server)
```

The site should be available in http://localhost:7379

# TODO #

- [ ] Add support for `/assets`
- [ ] Deployment with Docker
- [ ] Publish to Docker Hub?
