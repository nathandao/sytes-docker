Docker setup for developing Sytes - a small Common Lisp library for building simple websites
---

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [What is Sytes?](#what-is-sytes)
- [(Quickest way) Up and running with Docker Compose:](#quickest-way-up-and-running-with-docker-compose)
- [(Slower way) Up and running with Docker:](#slower-way-up-and-running-with-docker)
- [For emacs user](#for-emacs-user)
<!-- markdown-toc end -->


## What is Sytes? ##

Quoting from [Lisperator's website](http://lisperator.net/sytes/) (Sytes' creator):

"Sytes is a small Common Lisp library for building simple websites. I wouldn't call
it a framework—it doesn't deal with any of those things you'd expect frameworks to do,
like handling authentication, database abstraction etc. Sytes just implements a
template engine with a Lispy syntax and a Hunchentoot dispatcher that maps URLs to
templates. Sytes is the PHP of Common Lisp (I hope this doesn't sound like an insult).
If you're a Lisper, you might enjoy it."

Here is an example:

```lisp
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

Built on top of daewok's [lisp-devel-docker](https://github.com/daewok/lisp-devel-docker)
Dockerfile for `quicklisp`. Comes bundled with:

- sbcl
- quicklisp
- Sytes
- shared `./syte.blog` directory with docker's `/home/lisp/quicklisp/local-projects/syte.blog/sytes.blog`
- Nginx proxy for Sytes and `/assets` folder


## (Quickest way) Up and running with Docker Compose: ##

NOTE: If you're using Emacs, you may find the "For Emacs user" section useful :)

```bash
git clone https://github.com/nathandao/docker-sytes
docker-compose build
docker-compose up
```

Your blog should be accessible through http://localhost:8080

The blog's code is located in `./source/syte.blog`. Lisperator -
the creator of Sytes - wrote an amazing tutorial on how to use Sytes
in his blog, [lisperator.net](http://lisperator.net/sytes/tutorial/hello-world).
By using this docker setup, you can skip the installation part.

To connect to the current running docker, make sure you `docker-compose up`
session is still running. Open a new terminal session:

```bash
# get to the project's directory if you have not
cd docker-sytes

docker-compose run syte_web sbcl
```

## (Slower way) Up and running with Docker: ##

Build docker image

```bash
# clone the repository
git clone https://github.com/nathandao/docker-sytes

# build an image (replace syte/blog with your preferred name)
docker build -t syte/blog .
```

Confirm your new image is listed in `docker images`

```bash
docker images

# Output:
# REPOSITORY          TAG      .........
# syte/blog           latest   .........
```

Now it's time to start your blog, exSyting time! Start sbcl inside docker if you have not done so.

```bash
docker run --rm -it -p 8080:8080 -v $(pwd)/syte.blog:/home/lisp/quicklisp/local-projects/syte.blog/syte.blog syte/blog sbcl

* (asdf:run-shell-command "service nginx restart")
* (ql:quickload "syte.blog")
* (sytes:start-server)
```

The blog is now available in http://localhost:8080

# For emacs user #

If you're using emacs, `slime` is most likely your rhyme.

Make sure you have [slime](https://common-lisp.net/project/slime/) and
[slime-docker](https://github.com/daewok/slime-docker) installed.

Add this `slime-docker` configuration to your `.emacs` file:

```elisp
;; Do some standard SLIME configuration.
(slime-setup '(slime-fancy slime-tramp))
;; Configure slime-docker to use a specific image with shared volumes
;; and correct ports exposed
(setq slime-docker-implementations `((sbcl ("sbcl")
                                           :image-name "syte/blog"
                                           :mounts ((("/path/to/sytes-docker/syte.blog"
                                                      . "/home/lisp/quicklisp/local-projects/syte.blog")))
                                           :ports ((:host-port 8080 :container-port 8080 :ip "127.0.0.1")))))
```

Start Emacs, run `M-x slime-docker`. Quickload sytes, syte.blog and start the server:

```lisp
(asdf:run-shell-command "service nginx restart")
(ql:quickload "syte.blog")
(sytes:start-server)
```

The site should be available in http://localhost:8080
